    Program exec5
    USE MPI 
    implicit none 
    integer myrank, numprocs, bufsize, count, ierr
    integer thefile, intsize, i
    integer status(MPI_Status_SIZE)
    integer, dimension (:), allocatable :: buf 
    INTEGER(KIND=MPI_OFFSET_KIND) :: disp, filesize

    call MPI_Init(ierr)
    call MPI_Comm_rank (MPI_COMM_WORLD, myrank, ierr)
    call MPI_Comm_size (MPI_COMM_WORLD, numprocs, ierr)

    call MPI_File_open (MPI_COMM_WORLD, "testfile", MPI_MODE_RDONLY, & 
                 MPI_INFO_NULL, thefile, ierr)

    call MPI_File_get_size (thefile, filesize, ierr)

    call mpi_type_size ( mpi_integer, intsize, ierr )
        
    filesize = filesize/intsize  ! in number of ints 
    bufsize = filesize/numprocs + 1 

    if ( myrank == 0) then
    print*,'filesize=', filesize, ' and bufsize=', bufsize 
    endif 
 
    ! local number to read 
    allocate (buf(bufsize)) 

    call mpi_type_size ( mpi_integer, intsize, ierr )

    disp =  myrank * bufsize * intsize

    call MPI_File_set_view(thefile, disp, & 
              MPI_INT, MPI_INT, "native", MPI_INFO_NULL, ierr)

    call MPI_File_read (thefile, buf, bufsize, MPI_INT, status, ierr)
    call MPI_Get_count(status, MPI_INT, count, ierr)

     if (myrank == 9) then 
     open(unit=10, file='testfile-node9')
      do i=1, bufsize
     write(10,*) buf(i)
      enddo
     endif 

    print *, "process=", myrank, "read=",count,"ints" 
    call MPI_File_close(thefile,ierr)
    call MPI_Finalize(ierr)

    end Program exec5
