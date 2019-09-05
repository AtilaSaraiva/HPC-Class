     PROGRAM main
     USE MPI 

      Implicit none 

     integer :: n, nprocs, myrank, ierr
     integer :: ista, iend, ista2, iend1, inext,iprev 

     real*4, DIMENSION (:), allocatable :: a, b, c
     integer status (MPI_status_size)
     integer i, isend1,isend2, irecv1, irecv2

     
     CALL MPI_INIT(ierr)
     CALL MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr)
     CALL MPI_COMM_RANK(MPI_COMM_WORLD, myrank, ierr)
     
     if ( myrank == 0) then 
     print*, ' forneca n'
     read (5,*) n 
     endif 

      call mpi_bcast ( n, 1, mpi_integer, 0, mpi_comm_world, ierr)

     allocate (a(n),b(n), c(n))
     a=0; b=0; c=0  

      CALL para_range(1, n, nprocs, myrank, ista, iend)
 
     ista2 = ista
     iend1 = iend
     if (myrank == 0) ista2 = 2
     If(myrank==nprocs-1) iend1=n-1

      inext = myrank + 1
      iprev = myrank - 1

     if (myrank == nprocs-1)  inext = MPI_PROC_NULL
     if (myrank == 0)     iprev = MPI_PROC_NULL

     DO i = ista, iend
     b(i) = i
     ENDDO

     CALL MPI_ISEND(b(iend), 1, MPI_REAL4, inext, 1, & 
                         MPI_COMM_WORLD, isend1, ierr)
 
     CALL MPI_ISEND(b(ista), 1, MPI_REAL4, iprev, 1, & 
                       MPI_COMM_WORLD, isend2, ierr) 

     CALL MPI_IRECV(b(ista-1),1, MPI_REAL4, iprev, 1, & 
                       MPI_COMM_WORLD, irecv1, ierr)
 
     CALL MPI_IRECV(b(iend+1),1, MPI_REAL4, inext,1, & 
                       MPI_COMM_WORLD, irecv2, ierr)


     CALL MPI_WAIT(isend1, status, ierr)
     CALL MPI_WAIT(isend2, status, ierr)
     CALL MPI_WAIT(irecv1, status, ierr)
     CALL MPI_WAIT(irecv2, status, ierr)

     DO i = ista2, iend1
     a(i) = b(i-1) + b(i+1)
     ENDDO

       call MPI_ALLREDUCE ( a, c, n, MPI_REAL4, MPI_SUM, MPI_COMM_WORLD, ierr)

      if (myrank == 0) then 
        do i=1, n
        print*, 'i=', i, 'valor de c=', c(i)
        enddo
     endif 

        deallocate (a,b,c) 

     CALL MPI_FINALIZE(ierr)    
     END program main 


      SUBROUTINE para_range(n1, n2, nprocs, irank, ista, iend) 
      
      integer n1,n2, nprocs, irank, ista, iend
      integer iwork
   
      iwork = (n2 - n1) / nprocs + 1
      ista = MIN(irank * iwork + n1, n2 + 1)
      iend = MIN(ista + iwork - 1, n2)

      END subroutine para_range 
