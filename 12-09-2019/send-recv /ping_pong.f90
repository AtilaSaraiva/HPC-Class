	program  ping_pong

	Use MPI 

	implicit none

       integer ::  dest, source, tag, outmsg, inmsg

       integer ::  ierr, nprocs, myrank  
       integer ::  istatus(MPI_STATUS_SIZE)
       integer ::  left, centre, right, ping ,pong 
       
       call mpi_init (ierr)

       call mpi_comm_size (MPI_COMM_WORLD, nprocs, ierr) 
       
       call mpi_comm_rank (MPI_COMM_WORLD, myrank, ierr) 

       ! setting up the neighbouring processes 
       left = myrank -1
       centre = myrank 
       right =  myrank +1

       ! ping-pong game !! 

       if ( myrank == 0) then  

           ! set-up a ping value and proceed 

           ping = centre + 1

           call MPI_send (ping, 1, MPI_INT, right, centre, MPI_COMM_WORLD, ierr) 

           print *, 'ping from rank =',myrank, 'ping=', ping  


          else if ( myrank == nprocs -1 ) then

         call MPI_recv (pong, 1, MPI_INT, left, left, MPI_COMM_WORLD, istatus, ierr) 

          print *, 'pong from rank =',myrank, 'pong=', pong 


        else 

         call MPI_recv (pong, 1, MPI_INT, left, left, MPI_COMM_WORLD, istatus, ierr) 

           print *, 'pong from rank =',myrank, 'pong=', pong 

          ping = pong + 1 


         call MPI_send (ping, 1, MPI_INT, right, centre, MPI_COMM_WORLD, ierr) 
        
          print *, 'ping from rank =',myrank, 'ping=', ping  


        endif 

       call mpi_finalize (ierr) 

       stop 

       end program  ping_pong 


