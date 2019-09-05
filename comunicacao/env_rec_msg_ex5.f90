
	PROGRAM env_rec_msg5

	USE MPI	
		
		INTEGER(kind=4) :: myid ! numero associado a cada processador, comecando do zero
		INTEGER(kind=4) :: nprocs ! numero de processadores que serao utilizados
		INTEGER(kind=4) :: ierr ! erro para ver se tá certo
		INTEGER :: mpistatus(MPI_STATUS_SIZE)
		INTEGER :: valor


! Initializing MPI

		CALL MPI_init(ierr)
		CALL MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr)
		CALL MPI_Comm_rank(MPI_COMM_WORLD, myid, ierr)



		valor = 0
		if(myid.eq.0) then
			valor = 10
		end if

		print*, "T0 - Rank = ", myid, ", valor = ", valor

		!              variavel,tamanho,tipo,origem,grupo,erro
		call MPI_BCAST(valor,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
		
		print*, "T1- - Rank = ", myid, ", valor = ", valor

		call MPI_BARRIER(MPI_COMM_WORLD,ierr)
		
		
	CALL MPI_FINALIZE(ierr)
	
STOP
END PROGRAM env_rec_msg5
