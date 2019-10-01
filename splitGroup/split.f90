    program split

! comm_split.c -- build a collection of q
!   communicators using MPI_Comm_split
! Input: none
! Output:  Results of doing a broadcast across each of
!            the q communicators.
! Note:  Assumes the number of processes, p = q^2
!

    Use MPI

  integer :: p, my_rank, ierr
  integer ::  my_row_comm
  integer :: my_row, my_rank_in_row
  integer :: q, test

         call MPI_Init(ierr)
         call MPI_Comm_size(MPI_COMM_WORLD, p,ierr)
         call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)

     q = int(sqrt(real(p)))

    print *, 'p q my_rank', p, q, my_rank

!  my_rank is rank in MPI_COMM_WORLD.
!     q*q = p
      my_row = my_rank/q

        print *, 'my_row', my_row

     call  MPI_Comm_split(MPI_COMM_WORLD, my_row, my_rank, &
                 my_row_comm, ierr)

 !  Test the new communicators

    call  MPI_Comm_rank(my_row_comm, my_rank_in_row, ierr)

      if (my_rank_in_row .eq. 0) then
      test = my_row
    else
        test = 0
    endif

        call MPI_Bcast(test, 1, MPI_INT, 0, my_row_comm, ierr)

         print *, "Process", my_rank, "> my_row =", my_row, &
         "my_rank_in_row =", my_rank_in_row, "test =", test


     call MPI_Finalize(ierr)

    end Program split
