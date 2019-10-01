	program  sendrecv_replace

	Use MPI 

	implicit none

       REAL  :: a, b 

       integer ::  ierr, nproc, myrank  
       integer ::  istatus(MPI_STATUS_SIZE)
       
       call mpi_init (ierr)

       call mpi_comm_size (MPI_COMM_WORLD, nproc, ierr) 
       
       call mpi_comm_rank (MPI_COMM_WORLD, myrank, ierr) 
        
         if (myrank.eq.0) then
         a=10 + myrank; b=20+myrank
         else
         a=100+ myrank ; b=200 +myrank 
         endif

        !!  sendrecv ( sendbuf, sendcount, sendtype, dest, sendtag,
        !               recvbuf, recvcount, recvtype, source, recvtag,
        !               com, statuts, ierr) 

        if (myrank == 0) then

        print *, 'myrank: before sending and receiver (a,b) :', myrank, b, a 

        call MPI_SENDRECV_replace(b,1,MPI_REAL,1, 0,1,0, &
                          MPI_COMM_WORLD,istatus,ierr)
       
         print *, 'myrank: send b and get b ', myrank, b, a

         elseif (myrank == 1) then

         print *, 'myrank : before sending and receiver (a,b) :', myrank, a, b 

         call MPI_SENDRECV_replace(b,1,MPI_REAL,0,0,0,0, &
                         MPI_COMM_WORLD,istatus,ierr)

          print *, 'myrank: send b and get a ', myrank, b, a 
         end if

        if (myrank.eq.0) then
        write(*,*) a,b
        else
        write(*,*) a,b
        endif
       
        call mpi_finalize (ierr) 

       stop 

       end program  sendrecv_replace   
