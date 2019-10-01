 Program exec2
 
    USE MPI

    implicit none 

    integer, parameter :: BUFSIZE=10

    integer i, myrank, ierr 
    integer :: buf(BUFSIZE)
    character*20 filename
    character*5  num 

    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, myrank, ierr)

    do i=1, BUFSIZE
          buf(i) = myrank * BUFSIZE + i
    enddo 

    write(num,'(i3.3)')myrank 
    filename='testfile'//'.'//trim(num)

    write(6,*) 'filename e num ', filename, num 

    open(unit=10, file=filename)
    
    write(6,*) 'my rank ', myrank  

    write(10,*) buf
    
    close(10)
  
    call MPI_Finalize(ierr)
  
    end Program exec2
