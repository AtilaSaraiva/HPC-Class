        program taken_ring 

        use mpi
       
        implicit none

    integer :: ierr, rank, comsize
    integer :: para, from
    integer :: tags, tagr
    integer :: status(MPI_STATUS_SIZE)
    double precision :: msgsent, msgrcvd

    
        call MPI_Init(ierr)
        call MPI_Comm_size(MPI_COMM_WORLD, comsize, ierr)
        call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)


        para = mod( (rank+1), comsize)
        from =mod ((rank + comsize-1), comsize)

        print *, 'para from',para, from, comsize, rank

        msgsent= rank*rank
        msgrcvd= -999.
        tags=1
        tagr=10
        
        call MPI_Sendrecv(msgsent, 1, MPI_DOUBLE_PRECISION,para, tags,& 
                             msgrcvd, 1, MPI_DOUBLE_PRECISION,from, tagr, &
                            MPI_COMM_WORLD,status,ierr)
        
        print *, rank, 'Sent ', msgsent, 'and recvd ', msgrcvd
        call MPI_Finalize(ierr)
        end program taken_ring
