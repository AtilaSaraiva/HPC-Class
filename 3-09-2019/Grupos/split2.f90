    program split_2 

! comm_split.c -- build a collection of q
!   communicators using MPI_Comm_split
! split odd and even processors into 2 communicators

!! Establishing an intercommunicator between two process groups

    Use MPI

  integer :: p, my_rank, ierr
  integer ::  color, zero_one
  integer ::   new_comm, new_nodes, new_rank
  integer ::  tag, intercomm, status(mpi_status_size)
  integer ::  valor  

         call MPI_Init(ierr)
         call MPI_Comm_size(MPI_COMM_WORLD, p,ierr)
         call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)

      color= mod(my_rank,2) ! color is either 0 or 1

     print *, ' my_rank color', my_rank, color 

     call  MPI_Comm_split(MPI_COMM_WORLD, color, my_rank, &
                 new_comm, ierr)

         call MPI_Comm_size(new_comm, new_nodes, ierr)
         call MPI_Comm_rank(new_comm, new_rank, ierr)
   
        if ( mod(my_rank,2) .eq. 0) then

!  Group 0: create intercommunicate and send message 
!  Note that remote leader has id 0 in the mpi_comm_world
        
        call mpi_intercomm_create (new_comm, 0, mpi_comm_world, 1, 99, & 
                        intercomm, ierr ) 
        valor=new_rank

        call mpi_send (valor, 1, mpi_integer, new_rank, 0, intercomm, ierr) 

        else 
!  Group 1: create intercommunicate and send message 
        call mpi_intercomm_create (new_comm, 0, mpi_comm_world, 0, 99, & 
                        intercomm, ierr )
  
          call mpi_recv (valor, 1, mpi_integer, new_rank, 0, intercomm, status, ierr) 

          print *, 'my_rank=', my_rank, 'new_rank=', new_rank, 'color=', color, 'valor=', valor

        endif 

        
     call MPI_comm_free(intercomm, ierr)
     call MPI_comm_free (new_comm,ierr )
     
     call MPI_Finalize(ierr)
    
    end Program split_2
