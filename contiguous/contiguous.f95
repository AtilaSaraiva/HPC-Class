! programa que manda partes de uma matriz para cada processador
PROGRAM contiguous

	USE MPI
	IMPLICIT NONE

		INTEGER SIZE, SIZE1
		PARAMETER (SIZE=4, SIZE1=2)
		INTEGER(kind=4) :: myid ! numero associado a cada processador, comecando do zero
		INTEGER(kind=4) :: nprocs ! numero de processadores que serao utilizados
		INTEGER(kind=4) :: ierr ! erro para ver se tá certo
		INTEGER :: source, dest, tag, i
		INTEGER :: status(MPI_STATUS_SIZE)
		REAL*4 a(0:SIZE-1,0:SIZE-1), b(0:SIZE1-1)
		INTEGER columntype
		tag=1

		CALL MPI_init(ierr)
		CALL MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr)
		CALL MPI_Comm_rank(MPI_COMM_WORLD, myid, ierr)


		IF (myid.eq.0) THEN
			DATA a / 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0 /
		END IF




		! create contiguous derived data type
		CALL MPI_TYPE_CONTIGUOUS(SIZE1,MPI_REAL,columntype,ierr)
		CALL MPI_TYPE_COMMIT(columntype, ierr)

		IF (nprocs.eq.SIZE) then
		! task 0 sends one element of columntype to all tasks
			IF (myid.eq.0) THEN
                            DO i=1,nprocs-1
                                    CALL MPI_SEND(a(2:3,i),1, columntype, i, tag,MPI_COMM_WORLD,ierr)
                                    ! dois elementos estão sendo empacotados como se o tamanho fosse só 1, dentro do columntype
                            END DO

                        else
                            source = 0
                            CALL MPI_RECV(b,SIZE,columntype,source,tag, MPI_COMM_WORLD,status, ierr)
                            ! Size1 = 2, pois serão passados elementos de 2 a 2

                            print *, 'rank', myid, 'b = ', b
			END IF
		ELSE
			print *, 'Must specify ', SIZE, 'processors. Terminating.'

		END IF


		CALL MPI_TYPE_FREE (columntype,ierr)
		CALL MPI_FINALIZE(ierr)

END PROGRAM contiguous


