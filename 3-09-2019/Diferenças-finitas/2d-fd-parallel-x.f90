	PROGRAM main
	USE MPI
	IMPLICIT REAL*8 (a-h,o-z)
	PARAMETER (m = 100, n = 100)
	DIMENSION a(m,n), b(m,n), c(m,n)
	integer nprocs, myrank, ierr, jend1, jend, jsta, jsta2
	INTEGER istatus(MPI_STATUS_SIZE)

	CALL MPI_INIT(ierr)
	CALL MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr)
	CALL MPI_COMM_RANK(MPI_COMM_WORLD, myrank, ierr)

	CALL para_range(1, n, nprocs, myrank, jsta, jend)
	jsta2 = jsta
	jend1 = jend

	IF (myrank == 0) 	jsta2= 2
	IF (myrank == nprocs - 1)   jend1 = n - 1
	inext = myrank + 1
	iprev = myrank - 1
	IF (myrank == nprocs - 1)  inext = MPI_PROC_NULL
	IF (myrank == 0)           iprev=  MPI_PROC_NULL

	DO j = jsta, jend
	  DO i = 1, m
	  a(i,j) = i + 10.0 * j
  	  ENDDO
        ENDDO

	CALL MPI_ISEND(a(1,jend),m, MPI_REAL8,inext,1,MPI_COMM_WORLD, &
                             isend1,ierr)
	CALL MPI_ISEND(a(1,jsta),m, MPI_REAL8,iprev,1,MPI_COMM_WORLD, &
                           isend2,ierr)
	CALL MPI_IRECV(a(1,jsta-1),m,MPI_REAL8,iprev,1,MPI_COMM_WORLD, &
                     irecv1,ierr)
	CALL MPI_IRECV(a(1,jend+1),m,MPI_REAL8,inext,1,MPI_COMM_WORLD, &
                    irecv2,ierr)

	CALL MPI_WAIT(isend1, istatus, ierr)
	CALL MPI_WAIT(isend2, istatus, ierr)
	CALL MPI_WAIT(irecv1, istatus, ierr)
	CALL MPI_WAIT(irecv2, istatus, ierr)

	b =0.0

	DO j = jsta2, jend1
		DO i = 2, m - 1
		b(i,j) = a(i-1,j) + a(i,j-1) + a(i,j+1) + a(i+1,j)
		ENDDO
	ENDDO

	call MPI_ALLREDUCE ( b, c, m*n, MPI_REAL8, MPI_SUM, MPI_COMM_WORLD, ierr)

	IF (myrank == 0)  then
	open (unit=9, file='result-parallel-x.dat', ACCESS='direct', &
                    recl=4*n*m, status='UNKNOWN')
        write(9,rec=1)real(c,kind=4)
	endif


	CALL MPI_FINALIZE(ierr)
	END

 	SUBROUTINE para_range(n1, n2, nprocs, irank, ista, iend)
	iwork = (n2 - n1) / nprocs + 1
	ista = MIN(irank * iwork + n1, n2 + 1)
	iend = MIN(ista + iwork - 1, n2)
	END
