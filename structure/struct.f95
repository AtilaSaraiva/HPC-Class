! STRUCT PERMITE PASSAR VARIÁVEIS DE TIPOS DIFERETES (INTEIRO, COMPLEXO, REAL, CARACTER, LOGICO), TODOS JUNTOS EM APENAS 1 ENVIO!

PROGRAM struct

USE MPI

	IMPLICIT NONE
		INTEGER :: NELEM
		PARAMETER(NELEM=25)
		INTEGER(kind=4) :: myid ! numero associado a cada processador, comecando do zero
		INTEGER(kind=4) :: nprocs ! numero de processadores que serao utilizados
		INTEGER(kind=4) :: ierr ! erro para ver se tá certo
		INTEGER :: source, dest, tag, i
		INTEGER :: status(MPI_STATUS_SIZE)

		TYPE Particle
                    sequence
                    real*4 x, y, z, velocity
                    integer n, tipo
		END TYPE Particle

		TYPE  (Particle) p(0:NELEM-1), particles(0:NELEM-1) ! é um vetor de 25 posicoes e cada tem 6 elementos (x,y,z,velocity,n,tipo)
		integer particletype, oldtypes(0:1) ! required variables
		integer blockcounts(0:1), offsets(0:1), extent
		tag=1

		CALL MPI_init(ierr)
		CALL MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr)
		CALL MPI_Comm_rank(MPI_COMM_WORLD, myid, ierr)

		! SETUP DESCRIPTION OF THE 4 Mpi_real FIELDS X, Y, Z, VELOCITY
		offsets(0)=0
		oldtypes(0) = MPI_REAL
		blockcounts(0)=4 ! 4 = x, y, z, velocity

		! SETUP DESCRIPTION OF THE 4 MPI_INTEGER FIELDS, n e tipo
		! NEED TO FIRST FIGURE OFFSET BY GETTING SIZE OF MPI_REAL
		call MPI_TYPE_EXTENT(MPI_REAL,extent,ierr)
		offsets(1)=4*extent
		oldtypes(1) = MPI_INTEGER
		blockcounts(1)=2

		! DEFINE STRUCTERED TYPE AND COMMIT IT
		CALL MPI_TYPE_STRUCT(2,blockcounts,offsets,oldtypes,particletype,ierr)
		CALL MPI_TYPE_COMMIT(particletype,ierr)

		! TASK 0 INITIALIZES THE PARTICLE ARRAY AND THEN SEND IT TO EACH TASK
		IF (myid.eq.0) THEN
			DO i=0,NELEM-1
				particles(i)=Particle(1.*i, -1.*i, 1.*i, 0.25, i, mod(i,2))
			END DO

			DO i=0, nprocs-1
				CALL MPI_SEND(particles,NELEM,particletype,i,tag,MPI_COMM_WORLD,ierr)
			END DO
		END IF


		! ALL TASKS RECEIVE PARTICLETYPE DATA
		SOURCE=0
		CALL MPI_RECV(p,NELEM,particletype,source,tag,MPI_COMM_WORLD,status,ierr)

		print *, 'rank = ', myid, ' p(3) = ', p(3)


		! FREE DATATYPE WEN DONE USING IT
		CALL MPI_TYPE_FREE(particletype,ierr)
		CALL MPI_FINALIZE(ierr)

END PROGRAM struct





