        program fifthmessage

        use mpi

        implicit none

    integer :: ierr, rank, comsize
    integer :: left, right
    integer :: tag
    integer :: status(MPI_STATUS_SIZE)
    double precision :: msgsent, msgrcvd


        call MPI_Init(ierr)
        call MPI_Comm_size(MPI_COMM_WORLD, comsize, ierr)
        call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

        left= rank-1
        if(left <0) left =comsize-1
        right= rank +1
        if(right >=comsize) right =0

        msgsent= rank*rank
        msgrcvd= -999.
        tag=1


        call MPI_Sendrecv(msgsent, 1, MPI_DOUBLE_PRECISION,right, tag,&
                             msgrcvd, 1, MPI_DOUBLE_PRECISION,left, tag, &
                            MPI_COMM_WORLD,status,ierr)

        print *, rank, 'Sent ', msgsent, 'and recvd ', msgrcvd
        call MPI_Finalize(ierr)
        end program fifthmessage
