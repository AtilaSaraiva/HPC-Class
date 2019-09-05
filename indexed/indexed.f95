! permite criar blocos dentro de uma matriz e passar elementos ou intervalos especificos dessa matriz para os outros processadores

PROGRAM INDEXED

	USE MPI

	IMPLICIT NONE

		INTEGER :: NELEMENTS
		PARAMETER (NELEMENTS =6)
		INTEGER(kind=4) :: myid ! numero associado a cada processador, comecando do zero
		INTEGER(kind=4) :: nprocs ! numero de processadores que serao utilizados
		INTEGER(kind=4) :: ierr ! erro para ver se tá certo
		INTEGER :: source, dest, tag, i
		INTEGER :: status(MPI_STATUS_SIZE)
		REAL*4 a(0:15), b(0:NELEMENTS-1)
		INTEGER :: indextype
		INTEGER :: blocklenghts(0:1), displacements(0:1)

		tag=1

		CALL MPI_init(ierr)
		CALL MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr)
		CALL MPI_Comm_rank(MPI_COMM_WORLD, myid, ierr)


		IF (myid.eq.0) THEN
			DATA a / 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0 /
		END IF

		blocklenghts(0)=4
		blocklenghts(1)=2
		displacements(0)=5
		displacements(1)=12

		! o primeiro bloco terá 4 elementos que irão a partir de a(5), já o segundo bloco terá 2 elementos que começa em a(12)

		! create indexed derived data type
		CALL MPI_TYPE_INDEXED(2,blocklenghts, displacements, MPI_REAL,indextype,ierr)
		CALL MPI_TYPE_COMMIT(indextype, ierr)



		IF (myid.eq.0) THEN
			DO i=1,nprocs-1
				CALL MPI_SEND(a,1, indextype, i, tag,MPI_COMM_WORLD,ierr)
				! dois elementos estão sendo empacotados como se o tamanho fosse só 1, dentro do columntype
			END DO

                else
			source = 0
			CALL MPI_RECV(b,NELEMENTS,MPI_REAL,source,tag, MPI_COMM_WORLD,status, ierr)
			! Size1 = 2, pois serão passados elementos de 2 a 2

			print *, 'rank', myid, 'b = ', b


		END IF

		CALL MPI_TYPE_FREE (indextype,ierr)
		CALL MPI_FINALIZE(ierr)

END PROGRAM indexed
