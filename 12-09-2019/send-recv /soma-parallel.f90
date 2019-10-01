	program  sum_parallel

	Use MPI 

	implicit none

       integer ::  istart, istop, uptotal, i
       integer ::  sum, sumtotal, sum_0

       integer ::  ierr, nproc, myrank  
       integer ::  istatus(MPI_STATUS_SIZE)
       
       call mpi_init (ierr)

       call mpi_comm_size (MPI_COMM_WORLD, nproc, ierr) 
       
       call mpi_comm_rank (MPI_COMM_WORLD, myrank, ierr) 

       print *, ' hello from myrank ', myrank 

       uptotal=10   

       istart = myrank*(uptotal/nproc) + 1
       if ( myrank == (nproc-1)) then
               istop = uptotal
        else 
             istop = istart + (uptotal/nproc) -1 
       endif 

       print *, 'myrank: istart istop', myrank, istart, istop  

        sum =0
        sumtotal=0

                do i= istart, istop
                sum = sum + i 
                enddo 

        call mpi_reduce (sum, sumtotal,1, MPI_INT, MPI_SUM, 0, MPI_COMM_WORLD, ierr)

        print *, 'myrank sum sumtotal', sum, sumtotal


        if (myrank == 0) then
        sum_0 =0.0      
        do i=1, uptotal
        sum_0 = sum_0 + i 
        enddo

        print*, 'node zero: value of sum_O ', sum_0, '( serial version)'

        endif 

       call mpi_finalize (ierr) 

       stop 

       end program sum_parallel


