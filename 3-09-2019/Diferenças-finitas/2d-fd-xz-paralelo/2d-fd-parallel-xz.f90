	PROGRAM main
!	INCLUDE ’mpif.h’

	USE MPI 
	IMPLICIT REAL*8 (a-h,o-z)
	PARAMETER (m = 100, n = 100)
	DIMENSION a(m,n), b(m,n), c(m,n) 
	DIMENSION works1(n), workr1(n), works2(n), workr2(n) 
    INTEGER istatus(MPI_STATUS_SIZE)
 
	PARAMETER (iprocs = 3, jprocs = 3)
	INTEGER itable(-1:iprocs, -1:jprocs)
	
	CALL MPI_INIT(ierr)
	CALL MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr) 
	CALL MPI_COMM_RANK(MPI_COMM_WORLD, myrank, ierr) 
	
	IF (nprocs /= iprocs * jprocs) THEN
	PRINT *,'=== Error ==='
	STOP 
	ENDIF

	DO j = -1, jprocs 
	DO i = -1, iprocs
	itable(i,j) = MPI_PROC_NULL 
	ENDDO
	ENDDO

	irank = 0
	DO i = 0, iprocs - 1
	DO j = 0, jprocs - 1
	itable(i,j) = irank
	IF (myrank == irank ) THEN 
	myranki = i 
       myrankj = j
	ENDIF
       irank = irank + 1 
	ENDDO
     ENDDO

       CALL para_range(1, n, jprocs, myrankj, jsta, jend)
 
	jsta2 = jsta
	jend1 = jend
	IF (myrankj == 0)     jsta2 = 2
	IF (myrankj == jprocs - 1)  jend1= n-1
	
	CALL para_range(1, m, iprocs, myranki, ista,iend)

	ista2 = ista
	iend1 = iend

	IF (myranki == 0)  ista2= 2 
	IF (myranki == iprocs -1) iend1 = m-1

	ilen =iend-ista+1
	jlen =jend-jsta+1

	    jnext = itable(myranki, myrankj + 1) 
        jprev = itable(myranki, myrankj - 1) 

	    inext = itable(myranki + 1, myrankj) 
        iprev = itable(myranki - 1, myrankj) 

         DO j = jsta, jend
             DO i = ista, iend 
             a(i,j) = i + 10.0 * j 
             ENDDO 
         ENDDO


           IF (myranki /= iprocs -1) THEN
            DO j = jsta, jend
            works1(j) = a(iend, j) 
	    ENDDO	
	  ENDIF
          
           IF (myranki /= 0) THEN
	    DO j = jsta, jend 
	    works2(j) = a(ista,j)
    	ENDDO 
    	ENDIF

       CALL MPI_ISEND(a(ista,jend),ilen,MPI_REAL8,jnext,1,MPI_COMM_WORLD,isend1,ierr)
 
       CALL MPI_ISEND(a(ista,jsta),ilen,MPI_REAL8,jprev,1,MPI_COMM_WORLD,isend2,ierr) 

       CALL MPI_ISEND(works1(jsta),jlen,MPI_REAL8,inext,1,MPI_COMM_WORLD,jsend1,ierr)
 
       CALL MPI_ISEND(works2(jsta),jlen,MPI_REAL8,iprev,1,MPI_COMM_WORLD,jsend2,ierr)
 
       CALL MPI_IRECV(a(ista,jsta-1),ilen,MPI_REAL8,jprev,1,MPI_COMM_WORLD,irecv1,ierr) 

       CALL MPI_IRECV(a(ista,jend+1),ilen,MPI_REAL8,jnext,1,MPI_COMM_WORLD,irecv2,ierr) 

       CALL MPI_IRECV(workr1(jsta) ,jlen,MPI_REAL8,iprev,1,MPI_COMM_WORLD,jrecv1,ierr) 

       CALL MPI_IRECV(workr2(jsta) ,jlen,MPI_REAL8,inext,1,MPI_COMM_WORLD,jrecv2,ierr) 
       
	   CALL MPI_WAIT(isend1, istatus, ierr)       
       CALL MPI_WAIT(isend2, istatus, ierr) 
       CALL MPI_WAIT(jsend1, istatus, ierr) 
       CALL MPI_WAIT(jsend2, istatus, ierr) 
       CALL MPI_WAIT(irecv1, istatus, ierr) 
       CALL MPI_WAIT(irecv2, istatus, ierr) 
       CALL MPI_WAIT(jrecv1, istatus, ierr) 
       CALL MPI_WAIT(jrecv2, istatus, ierr) 
       
        IF (myranki /= 0) THEN
	    DO j = jsta, jend 
	    a(ista-1,j) = workr1(j)
	    ENDDO 
	    ENDIF
	
	    IF (myranki /= iprocs - 1) THEN
        DO j = jsta, jend 
	    a(iend+1,j) = workr2(j)
	    ENDDO 
	    ENDIF

	    DO j = jsta2, jend1 
	    DO i = ista2, iend1
    	b(i,j) = a(i-1,j) + a(i,j-1) + a(i,j+1) + a(i+1,j) 
	    ENDDO
    	ENDDO

      call MPI_ALLREDUCE ( b, c, m*n, MPI_REAL8, MPI_SUM, MPI_COMM_WORLD, ierr)
	
	    IF (myrank == 0)  then
	open (unit=9, file='result-parallel.dat', ACCESS='direct', & 
                    recl=4*n*m, status='UNKNOWN')
        write(9,rec=1)real(c,kind=4)
	endif


	CALL MPI_FINALIZE(ierr) 
	STOP
	END

	SUBROUTINE para_range(n1, n2, nprocs, irank, ista, iend) 
	iwork = (n2 - n1) / nprocs + 1
	ista = MIN(irank * iwork + n1, n2 + 1)
	iend = MIN(ista + iwork - 1, n2)
	END
