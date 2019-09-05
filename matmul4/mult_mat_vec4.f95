PROGRAM mult_mat_vec3



! Programa para calcular quando utilizando o reduce que fará a soma de m vetores de tamanho n (soma vetores colunas para dar o total)
! Sem precisar enviar o vetor e a matriz toda para todo mundo


	USE MPI	

		
		INTEGER(kind=4) :: myid ! numero associado a cada processador, comecando do zero
		INTEGER(kind=4) :: nprocs ! numero de processadores que serao utilizados
		INTEGER(kind=4) :: ierr ! erro para ver se tá certo
		INTEGER :: n, m, o
		INTEGER, DIMENSION (:,:), ALLOCATABLE :: A_aux, A
		INTEGER, DIMENSION (:), ALLOCATABLE :: x, y, y_parc, offset, tam_send
		REAL(kind=8) :: start, finish ! para checar o tempo de processamento


! Initializing MPI
		

		n=8
		m=8		
		ALLOCATE (x(m))
		
		CALL MPI_init(ierr)
		CALL MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr)
		CALL MPI_Comm_rank(MPI_COMM_WORLD, myid, ierr)
		start = MPI_WTIME()


		
		IF(myid.eq.0) then
		ALLOCATE (A(n,m),y(n))
		A=0	
		DO i=1,n
				A(i,i)=i
			END DO	

			DO i=1,n,2
			 x(i)=0
			 x(i+1)=1
			END DO
		
		END IF


		CALL MPI_BCast(x,m,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)		


		
		
		IF (myid.eq.0) then
		ALLOCATE(offset(0:nprocs-1),tam_send(0:nprocs-1))
		DO i=0,nprocs-1
		    	CALL PARA_RANGE_1(1,m,nprocs,i,ista,iend)
			tam_send(i)= (iend-ista+1)*n
			IF(i.eq.0) then
				offset(i)=0
			ELSE
				offset(i)=offset(i-1)+tam_send(i-1)
			END IF
		!	print*, 'Offset = ', offset(i), 'tam_send = ', tam_send(i)
		END DO
		END IF
		
		CALL PARA_RANGE_1(1,m,nprocs,myid,ista,iend)
		o=iend-ista+1
		ALLOCATE(A_aux(n,ista:iend))
		A_aux=0
		!print*, 'Rank = ', myid, 'ista = ', ista, 'iend = ', iend


		CALL sleep(1)
		!print*,  'Rank = ', myid, ' Colunas = ', o
		CALL MPI_ScatterV(A,tam_send,offset,MPI_INTEGER,A_aux,n*o,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)		

		!print*, 'Hello World'


		ALLOCATE(y_parc(n))
		y_parc=0
		DO j=ista,iend
			y_parc(1:n)=y_parc(1:n)+ A_aux(:,j)*x(j)
		END DO
		
		!print*, 'rank = ', 

		CALL MPI_REDUCE(y_parc,y,n,MPI_INTEGER,MPI_SUM,0,MPI_COMM_WORLD,ierr)
		finish=MPI_WTIME()


		IF(myid.eq.0) then
		print*, 'y = ', y
		print*, 'Tempo de processamento =', finish-start
		DEALLOCATE(A,y,offset,tam_send)
		end if
	
	DEALLOCATE(x,A_aux,y_parc)
		
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




