
	PROGRAM env_rec_msg2

	USE MPI

		INTEGER(kind=4) :: myid ! numero associado a cada processador, comecando do zero
		INTEGER(kind=4) :: nprocs ! numero de processadores que serao utilizados
		INTEGER(kind=4) :: ierr ! erro para ver se tá certo
		INTEGER(kind=4) :: status(MPI_STATUS_SIZE)
		INTEGER :: valor, mpistatus(3), i, mpirequest



! Initializing MPI

		CALL MPI_init(ierr)
		CALL MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr)
		CALL MPI_Comm_rank(MPI_COMM_WORLD, myid, ierr)

valor = 0

		IF (myid.eq.0) THEN
			valor=5
		END IF

		PRINT*, "T0 - rank", myid, ", valor =", valor


	IF (myid.eq.0) then
	DO i=1,nprocs-1
	CALL MPI_ISEND(valor,1,MPI_INTEGER,i,0,MPI_COMM_WORLD,mpirequest,ierr)
	! Na ordem: variavel, tamanho, tipo, destino, tag, grupo, erro
	END DO
	ELSE
	CALL MPI_IRECV(valor,1,MPI_INTEGER,MPI_ANY_SOURCE,MPI_ANY_TAG,MPI_COMM_WORLD,mpirequest,ierr)
	! Na ordem: variavel, tamanho, tipo, destino, tag, grupo, status, erro
	END IF

	CALL MPI_WAIT(mpirequest,status,ierr)


	PRINT*, "T1 - rank", myid, "valor =", valor


	CALL MPI_FINALIZE(ierr)

STOP
END PROGRAM env_rec_msg2
