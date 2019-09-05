    Program exec4
    USE MPI
    implicit none 

    integer, parameter :: BUFSIZE=100
!
    integer :: i, myrank, ierr
    integer :: buf(BUFSIZE)
    integer :: thefile, intsize
    INTEGER :: status(MPI_STATUS_SIZE)
    INTEGER(KIND=MPI_OFFSET_KIND) :: disp

    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, myrank, ierr)

    do i=1, BUFSIZE
        buf(i) = myrank * BUFSIZE + i
    enddo 

    call MPI_File_open(MPI_COMM_WORLD, 'testfile', &
                     MPI_MODE_CREATE+MPI_MODE_WRONLY,& 
                     MPI_INFO_NULL, thefile, ierr)
    
    call mpi_type_size ( mpi_integer, intsize, ierr)

    disp =  myrank*BUFSIZE*intsize

    print *, 'myrank=', myrank, 'disp=', disp 

    call MPI_File_set_view (thefile, disp, MPI_INT,& 
                 MPI_INT,'native',MPI_INFO_NULL, ierr)

    call MPI_File_write(thefile, buf, BUFSIZE, MPI_INT, & 
                          status, ierr)

    call MPI_File_close(thefile,ierr)

    call MPI_Finalize(ierr)

    end Program exec4 
