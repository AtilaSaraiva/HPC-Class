PROGRAM Avoidlock2
!-----------------------------------------------------------------
! MPI_SENDRECV, special primitive to avoid exchange deadlock
!-----------------------------------------------------------------
USE MPI

IMPLICIT NONE
INTEGER:: err, nproc, myid
INTEGER,DIMENSION(MPI_STATUS_SIZE):: status
INTEGER, PARAMETER:: N=10000000
REAL:: a(N), b(N)

CALL MPI_INIT (err)
CALL MPI_COMM_SIZE (MPI_COMM_WORLD, nproc, err)
CALL MPI_COMM_RANK (MPI_COMM_WORLD, myid, err)

IF (myid==0)  THEN
    a = 0
    CALL MPI_SENDRECV (a, N, MPI_REAL, 1, 10, & ! Sent data
                       b, N, MPI_REAL, 1, 11, & ! Received data
                       MPI_COMM_WORLD, status, err)
    WRITE(*,*) "Process", myid, "b =", b(1:3), "..."
ELSE
    a = 1
    CALL MPI_SENDRECV (a, N, MPI_REAL, 0, 11, & 
                       b, N, MPI_REAL, 0, 10, &
                       MPI_COMM_WORLD, status, err)
    WRITE(*,*) "Process", myid, "b =", b(1:3), "..."
END IF

CALL MPI_FINALIZE (err)

END PROGRAM Avoidlock2
