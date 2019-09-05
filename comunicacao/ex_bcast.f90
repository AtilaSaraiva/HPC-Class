PROGRAM ex_bcast

	USE MPI	
		
		INTEGER(kind=4) :: myid ! numero associado a cada processador, comecando do zero
		INTEGER(kind=4) :: nprocs ! numero de processadores que serao utilizados
		INTEGER(kind=4) :: ierr ! erro para ver se tá certo
		INTEGER :: valor(5)


! Initializing MPI

		CALL MPI_init(ierr)
		CALL MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr)
		CALL MPI_Comm_rank(MPI_COMM_WORLD, myid, ierr)

		valor=0
		if (myid.eq.0) then
			valor = 10
		end if

		print*, "T0- Rank = ", myid, ", valor = ", valor


		!  		variavel, tamanho, tipo, origem, destino, erro
		CALL MPI_BCast(valor,5,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

		print*, "T1- Rank = ", myid, ", valor = ", valor
	
		! tem que esperar todo mundo receber o que foi mandado em cima, por isso a barreira
		CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)

		if (myid.eq.1) then
			valor =3 
		end if

		CALL MPI_BCast(valor,5,MPI_INTEGER,1,MPI_COMM_WORLD,ierr)

		print*, "T2- Rank = ", myid, ", valor = ", valor
		
	CALL MPI_FINALIZE(ierr)
	
STOP
END PROGRAM ex_bcast
