    program split_1

! comm_split.c -- build a collection of q
!   communicators using MPI_Comm_split
! split odd and even processors into 2 communicators

    Use MPI

  integer :: p, my_rank, ierr
  integer ::  color, zero_one
  integer ::   new_comm, new_nodes, new_rank

   call MPI_Init(ierr)
   call MPI_Comm_size(MPI_COMM_WORLD, p,ierr)
   call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)

   color = mod(my_rank,2) ! color is either 0 or 1

   print *, ' my_rank color', my_rank, color

   call  MPI_Comm_split(MPI_COMM_WORLD, color, my_rank, &
                 new_comm, ierr)

         call MPI_Comm_size(new_comm, new_nodes, ierr)
         call MPI_Comm_rank(new_comm, new_rank, ierr)

        zero_one=-1

        if ( new_rank .eq. 0) zero_one=color

        call MPI_Bcast(zero_one, 1, MPI_INT, 0, new_comm, ierr)

        if (zero_one .eq. 0) then
        print *,'part of even processor communicator', &
        '   ', 'my_rank=', my_rank, 'color=', color, 'new_rank=', new_rank
        endif

        if (zero_one .eq. 1) then
        print *, 'part of odd processor communicator ', &
        '   ', 'my_rank=', my_rank, 'color=', color, 'new_rank=', new_rank
        endif


        print*, new_nodes
   call MPI_Finalize(ierr)

    end Program split_1
