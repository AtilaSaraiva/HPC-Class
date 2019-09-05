PROGRAM troca_paralelo


! o vetor a é igual a soma de dois valores do vetor b.
	USE MPI	
			
		IMPLICIT NONE
		INTEGER(kind=4) :: myid ! numero associado a cada processador, comecando do zero
		INTEGER(kind=4) :: nprocs ! numero de processadores que serao utilizados
		INTEGER(kind=4) :: ierr ! erro para ver se tá certo
		INTEGER :: n
		INTEGER, DIMENSION(:), ALLOCATABLE :: a, b, a_aux
		INTEGER :: status(MPI_STATUS_SIZE)
		INTEGER :: ista,iend,ista2,iend2,inext,iprev, isend1, isend2, irecv1, irecv2
		INTEGER :: i, isend3, isend4, irecv3, irecv4

! Initializing MPI
				
		CALL MPI_init(ierr)
		CALL MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr)
		CALL MPI_Comm_rank(MPI_COMM_WORLD, myid, ierr)

		IF (myid.eq.0) THEN
			n=11
		END IF

		CALL MPI_BCAST(n,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

		ALLOCATE(a(n),b(n),a_aux(n))
		a=0
		b=0
		a_aux=0

		CALL PARA_RANGE_1(1,n,nprocs,myid,ista,iend)

		ista2=ista
		iend2=iend

		IF(myid.eq.0) THEN
			ista2=3
		END IF

		IF(myid.eq.(nprocs-1)) THEN
			iend2= n-2
		END IF

		
		inext = myid+1
		iprev = myid-1
		
	
		IF(myid.eq.(nprocs-1)) THEN
			inext= MPI_PROC_NULL
		END IF	
	
		IF(myid.eq.0) THEN
			iprev= MPI_PROC_NULL
		END IF	
		


		DO i=ista,iend
			b(i)=i
		END DO


		CALL MPI_ISEND(b(iend), 1, MPI_INTEGER, inext, 1, MPI_COMM_WORLD, isend1, ierr)
		CALL MPI_ISEND(b(ista), 1, MPI_INTEGER, iprev, 1, MPI_COMM_WORLD, isend2, ierr)
		CALL MPI_ISEND(b(ista+1),1, MPI_INTEGER, iprev, 1, MPI_COMM_WORLD, isend3, ierr)
		CALL MPI_ISEND(b(iend-1), 1, MPI_INTEGER, inext, 1, MPI_COMM_WORLD, isend4, ierr)



		CALL MPI_IRECV(b(ista-1), 1, MPI_INTEGER, iprev, 1, MPI_COMM_WORLD, irecv1, ierr)
		CALL MPI_IRECV(b(iend+1), 1, MPI_INTEGER, inext, 1, MPI_COMM_WORLD, irecv2, ierr)		
		CALL MPI_IRECV(b(ista-2), 1, MPI_INTEGER, iprev, 1, MPI_COMM_WORLD, irecv3, ierr)
		CALL MPI_IRECV(b(iend+2), 1, MPI_INTEGER, inext, 1, MPI_COMM_WORLD, irecv4, ierr)
		





		
		
		CALL MPI_WAIT(isend1, status, ierr)
		CALL MPI_WAIT(isend2, status, ierr)
		CALL MPI_WAIT(isend3, status, ierr)
		CALL MPI_WAIT(isend4, status, ierr)
		CALL MPI_WAIT(irecv1, status, ierr)
		CALL MPI_WAIT(irecv2, status, ierr)
		CALL MPI_WAIT(irecv3, status, ierr)
		CALL MPI_WAIT(irecv4, status, ierr)		


		DO i = ista2,iend2
			a_aux(i) = b(i-	2) + b(i-1) + b(i+1) + b(i+2)
		END DO

		CALL MPI_REDUCE(a_aux, a, n, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
		IF (myid.eq.0) THEN
			print*, a
		END IF
		
		DEALLOCATE (a,b,a_aux)
	CALL MPI_FINALIZE(ierr)
	
STOP
END PROGRAM troca_paralelo



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

