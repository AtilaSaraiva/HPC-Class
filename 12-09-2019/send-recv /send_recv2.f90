	program  send_recv2 

	Use MPI 

	implicit none

       integer, dimension (:), allocatable :: sendbuf, recvbuf

       integer ::  ierr, nprocs, myrank, i
       integer ::  istatus(MPI_STATUS_SIZE)
       integer :: next_rank, prev_rank

       integer, parameter ::  size = 3
       
       call mpi_init (ierr)

       call mpi_comm_size (MPI_COMM_WORLD, nprocs, ierr) 
       
       call mpi_comm_rank (MPI_COMM_WORLD, myrank, ierr) 
        
        allocate (sendbuf(size), recvbuf(size))    
            
        do i = 1, size
        sendbuf(i) = myrank
        recvbuf(i) = -100
        enddo 
    
        if (myrank < nprocs -1) then 
        next_rank = myrank + 1 
        else  
        next_rank = MPI_PROC_NULL;
        endif 

        if (myrank > 0 ) then
        prev_rank = myrank - 1 
        else 
        prev_rank = MPI_PROC_NULL;    
        endif 

        do  i = 1,  SIZE
        print *,  '[Before] Send Rank:', sendbuf(i), 'Recv Rank', recvbuf(i)
        
!
!       int MPI_Sendrecv(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
!            int dest, int sendtag, void *recvbuf, int recvcount,
!            MPI_Datatype recvtype, int source, int recvtag,
!            MPI_Comm comm, MPI_Status *status)
!    
          call MPI_Sendrecv( sendbuf, SIZE, MPI_INT, next_rank, 0,recvbuf, &  
               SIZE, MPI_INT, prev_rank, MPI_ANY_TAG, MPI_COMM_WORLD, istatus, ierr )
        enddo 

        do  i=1, SIZE
        print *, '[After] Send Rank:', sendbuf(i), 'Recv Rank:', recvbuf(i)
        enddo 
    
           deallocate (sendbuf, recvbuf)


     call  MPI_Finalize(ierr)
   
     stop 
       
     end program  send_recv2
