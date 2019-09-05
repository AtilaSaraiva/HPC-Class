    Program exec6
    USE MPI
    implicit none 

    integer, parameter :: BUFSIZE=100
!
    integer :: i, myrank, numprocs, ierr
    integer :: buf(BUFSIZE), buf_size, count
    integer :: thefile, intsize
    INTEGER :: status(MPI_STATUS_SIZE)
    INTEGER(KIND=MPI_OFFSET_KIND) :: disp, filesize
    integer, dimension (:), allocatable :: buf_read 

    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, myrank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, numprocs, ierr)

    do i=1, BUFSIZE
        buf(i) = myrank * BUFSIZE + i
    enddo 

! write file 

    call MPI_File_open(MPI_COMM_WORLD, 'testfile', &
                     MPI_MODE_CREATE+MPI_MODE_WRONLY,& 
                     MPI_INFO_NULL, thefile, ierr)

    call mpi_type_size ( mpi_integer, intsize, ierr) 

    disp =  myrank*BUFSIZE*intsize

    call MPI_File_set_view (thefile,disp,MPI_INT,& 
                 MPI_INT,'native',MPI_INFO_NULL, ierr)

    call MPI_File_write(thefile, buf, BUFSIZE, MPI_INT, & 
                          status, ierr)

    call MPI_File_close(thefile,ierr)

     if ( myrank == 0) then
     print *, 'writing file '
     endif

! read file 

    call MPI_File_open (MPI_COMM_WORLD, "testfile", MPI_MODE_RDONLY, & 
                 MPI_INFO_NULL, thefile, ierr)

    call MPI_File_get_size (thefile, filesize, ierr)

    filesize = filesize/intsize  ! in number of ints 
    buf_size = filesize/numprocs + 1 

    if ( myrank == 0) then
    print*,'filesize=', filesize, ' and buf_size=', buf_size 
    endif 

    ! local number to read 
    allocate (buf_read(buf_size)) 
        
    disp =  myrank * buf_size * intsize

    call MPI_File_set_view(thefile, disp, & 
              MPI_INT, MPI_INT, "native", MPI_INFO_NULL, ierr)

    call MPI_File_read (thefile, buf_read, buf_size, MPI_INT, status, ierr)

    call MPI_Get_count(status, MPI_INT, count, ierr)

    print *, "process=", myrank, "read=",count,"integers" 

    call MPI_File_close(thefile,ierr)

    call MPI_Finalize(ierr)
 
    end Program exec6
