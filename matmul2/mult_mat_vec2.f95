PROGRAM mult_mat_vec2



! Programa para calcular quando a dimensão do vetor não for multiplo do numero de processadores utilizados
! Ou seja, não será usado o GATHER.


	USE MPI	

		
		INTEGER(kind=4) :: myid ! numero associado a cada processador, comecando do zero
		INTEGER(kind=4) :: nprocs ! numero de processadores que serao utilizados
		INTEGER(kind=4) :: ierr ! erro para ver se tá certo
		INTEGER :: n, m, o
		REAL :: pihome, sumpi
		INTEGER, DIMENSION (:,:), ALLOCATABLE :: A
		INTEGER, DIMENSION (:), ALLOCATABLE :: x, y, y_parc, offset, tam_send
		REAL(kind=8) :: start, finish ! para checar o tempo de processamento


! Initializing MPI
		

		n=8
		m=8		
		ALLOCATE (A(n,m),x(m),y(n))
		
		CALL MPI_init(ierr)
		CALL MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr)
		CALL MPI_Comm_rank(MPI_COMM_WORLD, myid, ierr)
		start = MPI_WTIME()

		ALLOCATE(offset(0:nprocs-1),tam_send(0:nprocs-1))
		
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

		IF (myid.eq.0) then
		DO i=0,nprocs-1
		    	CALL PARA_RANGE_1(1,n,nprocs,i,ista,iend)
			tam_send(i)= iend-ista+1
			IF(i.eq.0) then
				offset(i)=0
			ELSE
				offset(i)=offset(i-1)+tam_send(i-1)
			END IF
			print*, 'Offset = ', offset(i), 'tam_send = ', tam_send(i)
		END DO
		END IF
			

		CALL MPI_BCast(A,n*m,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
		CALL MPI_BCast(x,m,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)



		CALL PARA_RANGE_1(1,n,nprocs,myid,ista,iend)
		ALLOCATE(y_parc(ista:iend))
		
		DO i=ista,iend
			y_parc(i)=DOT_PRODUCT(A(i,:),x(:))
		END DO
	
		o=iend-ista+1
		!                o que se recebe, o tamanho de cada recebido, o tipo, onde irá juntar, o tamanho de cada recebido (vetor), o intervalo de cada posição, proc onde vai juntar...
		CALL MPI_GATHERV(y_parc,o,MPI_INTEGER,y,tam_send,offset,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
		finish=MPI_WTIME()


		IF(myid.eq.0) then
		print*, 'y = ', y
		print*, 'Tempo de processamento =', finish-start
		end if
	
	DEALLOCATE(A,x,y,y_parc,tam_send,offset)
		
	CALL MPI_FINALIZE(ierr)
	
STOP
END PROGRAM mult_mat_vec2



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




