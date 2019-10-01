    Program exec3

    USE MPI

    implicit none 

    integer, parameter :: BUFSIZE=10
    integer i, myrank, ierr
    integer  buf(BUFSIZE)
    character*20 filename
    character*5  num
    integer  myfile

    call MPI_Init (ierr)
    call MPI_Comm_rank (MPI_COMM_WORLD, myrank, ierr)

    do i=1, BUFSIZE
        buf(i) = myrank * BUFSIZE + i
    enddo 
    
    write(num,'(i3.3)')myrank 
    filename='testfile'//'.'//trim(num)

    call MPI_File_open (MPI_COMM_SELF, filename,& 
                         MPI_MODE_WRONLY+MPI_MODE_CREATE, &
                          MPI_INFO_NULL, myfile, ierr)

    call MPI_File_write (myfile, buf, BUFSIZE, MPI_INT, & 
                           MPI_STATUS_IGNORE, ierr)

    call MPI_File_close (myfile, ierr)

    call MPI_Finalize (ierr)

    end Program exec3 
