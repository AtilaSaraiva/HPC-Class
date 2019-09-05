! Troca linhas de elementos entre matrizes.

PROGRAM troca_linhas_matriz

	USE MPI

		IMPLICIT NONE
		INTEGER(kind=4) :: myid ! numero associado a cada processador, comecando do zero
		INTEGER(kind=4) :: nprocs ! numero de processadores que serao utilizados
		INTEGER(kind=4) :: ierr ! erro para ver se tá certo
		INTEGER :: n, m
		REAL*8, DIMENSION(:,:), ALLOCATABLE :: a, b, c
		REAL*8, DIMENSION(:), ALLOCATABLE :: r1, r2, s1, s2
		INTEGER :: status(MPI_STATUS_SIZE)
		INTEGER :: ista,iend,ista2,iend2,inext,iprev, isend1, isend2, irecv1, irecv2
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

		ALLOCATE(a(m,n),b(m,n),c(m,n))
		ALLOCATE(r1(n),r2(n),s1(n),s2(n))
		r1=0.
		r2=0.
		a=0.
		b=0.
		c=0.
		s1=0.
		s2=0.

		CALL PARA_RANGE_1(1,m,nprocs,myid,ista,iend)

		ista2=ista
		iend2=iend

		IF(myid.eq.0) THEN
			ista2=2
		END IF

		IF(myid.eq.(nprocs-1)) THEN
			iend2= m-1
		END IF


		inext = myid+1
		iprev = myid-1


		IF(myid.eq.(nprocs-1)) THEN
			inext= MPI_PROC_NULL
		END IF

		IF(myid.eq.0) THEN
			iprev= MPI_PROC_NULL
		END IF

		DO j=1,n
			DO i=ista,iend
			a(i,j)= (i + j)*1.
			END DO
		END DO


		IF (myid.ne.(nprocs-1)) THEN
			DO j=1,n
				s1(j)=a(iend,j)
			END DO
		END IF


		IF (myid.ne.0) THEN
			DO j=1,n
				s2(j)=a(ista,j)
			END DO
		END IF

		CALL MPI_ISEND(s1, n, MPI_REAL8, inext, 1, MPI_COMM_WORLD, isend1, ierr)
		CALL MPI_ISEND(s2, n, MPI_REAL8, iprev, 1, MPI_COMM_WORLD, isend2, ierr)
		CALL MPI_IRECV(r1, n, MPI_REAL8, iprev, 1, MPI_COMM_WORLD, irecv1, ierr)
		CALL MPI_IRECV(r2, n, MPI_REAL8, inext, 1, MPI_COMM_WORLD, irecv2, ierr)

		CALL MPI_WAIT(isend1, status, ierr)
		CALL MPI_WAIT(isend2, status, ierr)
		CALL MPI_WAIT(irecv1, status, ierr)
		CALL MPI_WAIT(irecv2, status, ierr)


		IF (myid.ne.0) then
			DO j=1,n
                            a(ista-1,j)= r1(j)
			END DO
		END IF

		IF (myid.ne.(nprocs-1)) THEN
			DO j=1,n
                            a(iend+1,j) = r2(j)
			END DO
		END IF



		DO j = 2, n-1
			DO i = ista2,iend2
				c(i,j) = a(i-1,j) + a(i+1,j) + a(i,j-1) + a(i,j+1)
			END DO
		END DO

		CALL MPI_REDUCE(c, b, m*n, MPI_REAL8, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
		IF (myid.eq.0) THEN
			open (unit=10, file='result_par_linhas.dat', ACCESS='direct', recl=4*n*m, status='UNKNOWN')
			write(10,rec=1)real(b,kind=4)
		END IF

		DEALLOCATE (a,b,c)
	CALL MPI_FINALIZE(ierr)

STOP
END PROGRAM troca_linhas_matriz



! --------------------------------------------------------------------- !


SUBROUTINE PARA_RANGE_1(n1,n2,nprocs,myid,ista,iend)
	INTEGER :: iwork1, iwork2, n1, n2, nprocs, myid, ista, iend

	iwork1=(n2-n1+1)/nprocs
	iwork2=mod(n2-n1+1,nprocs)
	ista=myid*iwork1+n1+min(myid,iwork2)
	iend=ista+iwork1-1

	if (iwork2.gt.myid) then
	iend = iend + 1
	end if
END SUBROUTINE PARA_RANGE_1

