program mpi_group

!******************************************************************************
! FILE: mpi_group.c - fortran 90 ( reynam pestana)
! DESCRIPTION:
!  MPI tutorial example code: Groups/Communicators
! AUTHOR: Blaise Barney
! LAST REVISED: 04/13/05
!*****************************************************************************/

    Use MPI

integer, parameter ::  NPROCS=8

integer ::  rank, new_rank, sendbuf, recvbuf, numtasks, ierr
integer ranks1(4)/0,1,2,3/, ranks2(4)/4,5,6,7/

integer :: orig_group, new_group
integer :: new_comm

      call MPI_Init(ierr)
      call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
      call MPI_Comm_size( MPI_COMM_WORLD, numtasks, ierr)

    if (numtasks .ne. NPROCS) then
    print *, "Must specify tasks. Terminating.\n", NPROCS
    call MPI_Finalize(ierr)
    stop
    endif

    sendbuf = rank

! Extract the original group handle
    call MPI_Comm_group(MPI_COMM_WORLD, orig_group, ierr)

 ! Divide tasks into two distinct groups based upon rank

   if (rank .lt.  NPROCS/2) then
    call MPI_Group_incl(orig_group, NPROCS/2, ranks1, new_group, ierr)
else
   call  MPI_Group_incl(orig_group, NPROCS/2, ranks2, new_group, ierr)
  endif

! Create new new communicator and then perform collective communications */
    call MPI_Comm_create(MPI_COMM_WORLD, new_group, new_comm, ierr)

    call MPI_Allreduce(sendbuf, recvbuf, 1, MPI_INT, MPI_SUM, new_comm, ierr)

    call MPI_Group_rank (new_group, new_rank, ierr)

    print *, "rank=", rank,  "newrank=", new_rank,  "recvbuf=",recvbuf

    call MPI_Finalize(ierr)

    end program mpi_group
