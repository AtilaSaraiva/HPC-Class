PROGRAM mult_mat_vec3



! Programa para calcular quando utilizando o reduce que fará a soma de m vetores de tamanho n (soma vetores colunas para dar o total)


	USE MPI	

		
		INTEGER(kind=4) :: myid ! numero associado a cada processador, comecando do zero
		INTEGER(kind=4) :: nprocs ! numero de processadores que serao utilizados
		INTEGER(kind=4) :: ierr ! erro para ver se tá certo
		INTEGER :: n, m, o
		INTEGER, DIMENSION (:,:), ALLOCATABLE :: A
		INTEGER, DIMENSION (:), ALLOCATABLE :: x, y, y_parc
		REAL(kind=8) :: start, finish ! para checar o tempo de processamento


! Initializing MPI
		

		n=8
		m=8		
		ALLOCATE (A(n,m),x(m),y(n))
		
		CALL MPI_init(ierr)
		CALL MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr)
		CALL MPI_Comm_rank(MPI_COMM_WORLD, myid, ierr)
		start = MPI_WTIME()

	
		A=0
		IF(myid.eq.0) then
			DO i=1,n
				A(i,i)=i
			END DO	

			DO i=1,n,2
			 x(i)=0
			 x(i+1)=1
			END DO
		
		END IF
		

		CALL MPI_BCast(A,n*m,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
		CALL MPI_BCast(x,m,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)


		CALL PARA_RANGE_1(1,m,nprocs,myid,ista,iend)
		ALLOCATE(y_parc(n))
		o=iend-ista+1
		y_parc=0
		DO j=ista,iend
			y_parc(1:n)=y_parc(1:n)+ A(:,j)*x(j)
		END DO
	

		CALL MPI_REDUCE(y_parc,y,n,MPI_INTEGER,MPI_SUM,0,MPI_COMM_WORLD,ierr)
		finish=MPI_WTIME()


		IF(myid.eq.0) then
		print*, 'y = ', y
		print*, 'Tempo de processamento =', finish-start
		end if
	
	DEALLOCATE(A,x,y,y_parc)
		
	CALL MPI_FINALIZE(ierr)
	
STOP
END PROGRAM mult_mat_vec3



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




