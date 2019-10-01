program main

  use mpi
  implicit none
  integer                            :: irank, ista, iend
  integer                            :: m, n, i, j
  real, dimension(:,:), allocatable  :: A, B
  integer, dimension(:), allocatable :: itype, ireq

  integer :: myrank, nprocs, ierr, iireq
  integer :: istatus(MPI_STATUS_SIZE)

  call MPI_Init(ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, myrank, ierr)

  allocate(itype(0:nprocs-1), ireq(0:nprocs-1))

  m = 10;	n = 10

  if(myrank .eq. 0)then
     allocate(A(m,n))

     A = 0.
     do i = 1,n
     	do j = 1,m
     		A(j,i) = 10*j + i
  	enddo
     enddo
  endif

  do irank = 0,nprocs-1
  	call f_range(1, m, nprocs, irank, ista, iend)
  	call para_type_block2(1, m, 1, ista, iend, 1, n, MPI_REAL, itype(irank))
  enddo

  call para_range(1, m, nprocs, myrank, ista, iend)
  write(*,*) myrank, ista, iend
  allocate(B(m,n))
 
  if(myrank .ne. 0)then
      call MPI_IRECV(B, 1, itype(myrank), 0, 0, MPI_COMM_WORLD, iireq, ierr)
      call MPI_WAIT(iireq, istatus, ierr)

  else
     do irank = 1,nprocs-1
     	call MPI_ISEND(A, 1, itype(irank), irank, 0, MPI_COMM_WORLD, ireq(irank), ierr)
     enddo
     do irank = 1,nprocs-1
     	call MPI_WAIT(ireq(irank), istatus, ierr)
     enddo
  endif

  if(myrank .eq. 1)then
     do j = 1,m
     	write(10,*) (B(j,i), i=1,n)
     enddo
  endif

  call MPI_Finalize(ierr)

return
end program main

  subroutine para_type_block2(imin, imax, jmin, ista, iend, jsta, jend, ioldtype, inewtype)

  use mpi
  integer iblock(2), idisp(2), itype(2)

  call MPI_TYPE_EXTENT(ioldtype, isize, ierr)

  ilen = iend-ista+1
  jlen = jend-jsta+1

  call MPI_TYPE_VECTOR(jlen, ilen, imax-imin+1, ioldtype, itemp, ierr)

  iblock(1) = 1
  iblock(2) = 1
  idisp(1) = 0
  idisp(2) = ((imax-imin+1) * (jsta-jmin) + (ista-imin)) * isize
  itype(1) = MPI_LB
  itype(2) = itemp

  call MPI_TYPE_STRUCT(2, iblock, idisp, itype, inewtype, ierr)
  call MPI_TYPE_COMMIT(inewtype, ierr)

  return
  end subroutine para_type_block2

  subroutine f_range(n1, n2, nprocs, irank, ista, iend)

  implicit none
  integer :: n1, n2, nprocs, irank, ista, iend

  integer :: iwork1, iwork2

  iwork1 = (n2 - n1 + 1) / nprocs
  iwork2 = mod(n2 - n1 + 1, nprocs)

  ista = irank * iwork1 + n1 + min(irank, iwork2)
  iend = ista + iwork1 - 1
  if(iwork2 .gt. irank) iend = iend + 1

  return
  end subroutine f_range


  subroutine para_range(n1,n2,nprocs,irank,ista,iend)

  iwork=(n2-n1)/nprocs + 1
  ista=min(irank*iwork + n1,n2+1)
  iend=min(ista+iwork-1,n2)

  return
  end subroutine para_range
