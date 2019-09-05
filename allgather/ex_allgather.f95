PROGRAM ex_allgather

	USE MPI	
		
		INTEGER(kind=4) :: myid ! numero associado a cada processador, comecando do zero
		INTEGER(kind=4) :: nprocs ! numero de processadores que serao utilizados
		INTEGER(kind=4) :: ierr ! erro para ver se tá certo
		PARAMETER (nprocmax=12)
		REAL :: param(nprocmax), mine
		INTEGER :: sndcnt,rcvcnt

! Initializing MPI

		CALL MPI_init(ierr)
		CALL MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr)
		CALL MPI_Comm_rank(MPI_COMM_WORLD, myid, ierr)


		IF(nprocs.gt.nprocmax) then
			write(*,*) 'Increase array size(param): ', nprocmax,size
			STOP
		END IF

		sndcnt=1     	! data size to be sent
		mine=23.0+myid  ! some rank-dependent data
		

			rcvcnt = 1 	! data size to be receibed (from each)

		
		CALL MPI_ALLGATHER(mine,sndcnt,MPI_REAL,param,rcvcnt,MPI_REAL,MPI_COMM_WORLD,ierr)
		
			DO i=1,nprocs
				print *, 'PE: ', myid, 'param(',i,') is ', param(i)
			END DO

		
	CALL MPI_FINALIZE(ierr)
	
STOP
END PROGRAM ex_allgather
