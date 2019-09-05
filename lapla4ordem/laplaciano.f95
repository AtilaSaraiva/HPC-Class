PROGRAM laplaciano

	USE MPI	
			
		IMPLICIT NONE
		INTEGER(kind=4) :: myid ! numero associado a cada processador, comecando do zero
		INTEGER(kind=4) :: nprocs ! numero de processadores que serao utilizados
		INTEGER(kind=4) :: ierr ! erro para ver se tá certo
		INTEGER :: n, m
		REAL :: dx, dz
		REAL*8, DIMENSION(:,:), ALLOCATABLE :: a, b, c, sx, sz
		INTEGER :: status(MPI_STATUS_SIZE)
		INTEGER :: jsta,jend,jsta2,jend2,inext,iprev, isend1, isend2, irecv1, irecv2
		INTEGER :: i, j

! Initializing MPI
				
		CALL MPI_init(ierr)
		CALL MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr)
		CALL MPI_Comm_rank(MPI_COMM_WORLD, myid, ierr)

		IF (myid.eq.0) THEN
			n=100
			m=100
		END IF

		CALL MPI_BCAST(n,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
		CALL MPI_BCAST(m,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

		ALLOCATE(a(m,n),b(m,n),c(m,n),sx(m,n),sz(m,n))
		a=0.
		b=0.
		c=0.
		sx=0.
		sz=0.
		dx=1.
		dz=1.

		CALL PARA_RANGE_1(1,n,nprocs,myid,jsta,jend)

		jsta2=jsta
		jend2=jend

		IF(myid.eq.0) THEN
			jsta2=2
		END IF

		IF(myid.eq.(nprocs-1)) THEN
			jend2= n-1
		END IF

		
		inext = myid+1
		iprev = myid-1
		
	
		IF(myid.eq.(nprocs-1)) THEN
			inext= MPI_PROC_NULL
		END IF	
	
		IF(myid.eq.0) THEN
			iprev= MPI_PROC_NULL
		END IF	
		
		DO i=1,m
			DO j=jsta,jend
			a(i,j)= (i + j)*1.
			END DO
		END DO 

		CALL MPI_ISEND(a(1,jend), m, MPI_REAL8, inext, 1, MPI_COMM_WORLD, isend1, ierr)
		CALL MPI_ISEND(a(1,jsta), m, MPI_REAL8, iprev, 1, MPI_COMM_WORLD, isend2, ierr)
		CALL MPI_IRECV(a(1,jsta-1), m, MPI_REAL8, iprev, 1, MPI_COMM_WORLD, irecv1, ierr)
		CALL MPI_IRECV(a(1,jend+1), m, MPI_REAL8, inext, 1, MPI_COMM_WORLD, irecv2, ierr)				
		
		CALL MPI_WAIT(isend1, status, ierr)
		CALL MPI_WAIT(isend2, status, ierr)
		CALL MPI_WAIT(irecv1, status, ierr)
		CALL MPI_WAIT(irecv2, status, ierr)
		


		DO j = jsta2,jend2
			DO i = 2, m-1
				sx(i,j) = a(i-1,j) + a(i+1,j) + 2*a(i,j)
				sz(i,j) = a(i,j-1) + a(i,j+1) + 2*a(i,j)
				c(i,j) = sx(i,j)/(dx**2) + sz(i,j)/(dz**2)
			END DO
		END DO

		CALL MPI_REDUCE(c, b, m*n, MPI_REAL8, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
		IF (myid.eq.0) THEN
			open (unit=10, file='laplaciano_paralelo.dat', ACCESS='direct', recl=4*n*m, status='UNKNOWN')
			write(10,rec=1)real(b,kind=4)
		END IF
		
		DEALLOCATE (a,b,c,sx,sz)
	CALL MPI_FINALIZE(ierr)
	
STOP
END PROGRAM laplaciano


! --------------------------------------------------------------------- ! 


SUBROUTINE PARA_RANGE_1(n1,n2,nprocs,myid,jsta,jend)
	INTEGER :: iwork1, iwork2, n1, n2, nprocs, myid, jsta, jend
	
	iwork1=(n2-n1+1)/nprocs
	iwork2=mod(n2-n1+1,nprocs)
	jsta=myid*iwork1+n1+min(myid,iwork2)
	jend=jsta+iwork1-1

	if (iwork2.gt.myid) then
	jend = jend + 1 
	end if
END SUBROUTINE PARA_RANGE_1

