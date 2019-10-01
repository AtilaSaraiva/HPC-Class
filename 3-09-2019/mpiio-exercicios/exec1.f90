    Program  exerc1

    Use MPI

    integer, parameter :: BUFSIZE=100

    integer, parameter :: tag=1000

    integer ::  i,j, myrank, numprocs, ierr , irec
    integer :: buf(BUFSIZE)
    integer :: status(MPI_Status_size)

    call  MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, myrank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, numprocs,ierr)

    do j=1, BUFSIZE
         buf(j) = myrank * BUFSIZE + j
    enddo

    if (myrank .ne. 0) then
       write(6,*) 'sending node=',myrank
      call MPI_Send(buf, BUFSIZE, MPI_INT, 0, tag, MPI_COMM_WORLD,ierr)
    else
         write(6,*) 'node', myrank
         open(unit=10, file="testfile")
         write(10, *)(buf(i), i=1, BUFSIZE)
        do irec=1, numprocs-1
         write(6,*) 'receiving node=', irec
       call  MPI_Recv(buf, BUFSIZE, MPI_INT, irec, tag, MPI_COMM_WORLD, &
                status, ierr)
         write(10, *)(buf(i), i=1, BUFSIZE)
        enddo

        close(10)
        endif

         call MPI_Finalize(ierr)
    stop

    end Program exerc1
