
	PROGRAM env_rec_msg4

	USE MPI	
		
		INTEGER(kind=4) :: myid ! numero associado a cada processador, comecando do zero
		INTEGER(kind=4) :: nprocs ! numero de processadores que serao utilizados
		INTEGER(kind=4) :: ierr ! erro para ver se tá certo
		INTEGER :: valor, i, node !node = numero do processador
		INTEGER :: mpistatus(MPI_STATUS_SIZE)
		INTEGER :: root, aux, acm
		INTEGER :: vetor(10)



! Initializing MPI

		CALL MPI_init(ierr)
		CALL MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr)
		CALL MPI_Comm_rank(MPI_COMM_WORLD, myid, ierr)


		acm=0
		if(myid.eq.0) then
			do i=1,10
				vetor(i)=1
			end do
		
			do i=1,10
			 CALL MPI_RECV(node,1,MPI_INTEGER,MPI_ANY_SOURCE,MPI_ANY_TAG,MPI_COMM_WORLD,mpistatus,ierr)
		      	 CALL MPI_SEND(vetor(i),1,MPI_INTEGER,node,0,MPI_COMM_WORLD,ierr)
			end do

			valor= -1

			do i=1,nprocs-1
			 CALL MPI_RECV(node,1,MPI_INTEGER,MPI_ANY_SOURCE,MPI_ANY_TAG,MPI_COMM_WORLD,mpistatus,ierr)
		      	 CALL MPI_SEND(valor,1,MPI_INTEGER,node,0,MPI_COMM_WORLD,ierr)
			 CALL MPI_RECV(aux,1,MPI_INTEGER,node,MPI_ANY_TAG,MPI_COMM_WORLD,mpistatus,ierr)

   			 acm = acm+aux
		        end do
			
			print*, "Valor medio = ", acm/10.

		else

			do 
	         	 CALL MPI_SEND(myid,1,MPI_INTEGER,0,0,MPI_COMM_WORLD,ierr)
			 CALL MPI_RECV(valor,1,MPI_INTEGER,0,MPI_ANY_TAG,MPI_COMM_WORLD,mpistatus,ierr)			
			
			 if (valor.eq.-1) then
				exit
			 else
			 acm=acm+valor
			 end if

   			end do

	         	CALL MPI_SEND(acm,1,MPI_INTEGER,0,0,MPI_COMM_WORLD,ierr)
	
		end if

	CALL MPI_FINALIZE(ierr)
	
STOP
END PROGRAM env_rec_msg4
