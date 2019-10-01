	program  point_to_point_comm

	Use MPI 

	implicit none

       integer ::  dest, source, tag, outmsg, inmsg

       integer ::  ierr, nproc, myrank  
       integer ::  istatus(MPI_STATUS_SIZE)
       
       call mpi_init (ierr)

       call mpi_comm_size (MPI_COMM_WORLD, nproc, ierr) 
       
       call mpi_comm_rank (MPI_COMM_WORLD, myrank, ierr) 


       print *, ' hello from myrank ', myrank 


       if (myrank == 0) then
       dest =1
       source =1
       tag =1000 
       outmsg=myrank 

       call mpi_send (outmsg, 1, MPI_INT, dest, tag, MPI_COMM_WORLD, ierr)

       print *, 'myrank inmsg outmsg', myrank, inmsg, outmsg  

       call mpi_recv (inmsg, 1, MPI_INT, source, tag, MPI_COMM_WORLD, istatus,  ierr)

       endif 

       if (myrank == 1) then
       dest =0
       source =0
       tag =1000 
       outmsg=myrank 

       call mpi_recv (inmsg, 1, MPI_INT, source, tag, MPI_COMM_WORLD, istatus,  ierr)

       print *, 'myrank inmsg outmsg', myrank, inmsg, outmsg  

       call mpi_send (outmsg, 1, MPI_INT, dest, tag, MPI_COMM_WORLD, ierr)

       endif 

       call mpi_finalize (ierr) 

       stop 

       end program  point_to_point_comm


