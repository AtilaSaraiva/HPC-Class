program firstmpi
! inclusao da biblioteca MPI
  use mpi
  integer :: mpierror, mpisize, mpirank, valor
  integer :: world_group,new_group,ranks(2),new_comm
  real :: t0,t1

! inicializacao da MPI
  call MPI_Init(mpierror)
  call MPI_Comm_size(MPI_COMM_WORLD, mpisize, mpierror)
  call MPI_Comm_rank(MPI_COMM_WORLD, mpirank, mpierror)

  valor = -1
  if(mpirank==1) then
     valor = 1000
  endif

  ranks(1) = 1
  ranks(2) = 3
  call MPI_Comm_group(MPI_COMM_WORLD,world_group,mpierror)
  ! Criando novo grupo
  call MPI_Group_incl(world_group,2,ranks,new_group,mpierror)
  ! Criando comunicador new_comm
  call MPI_Comm_create(MPI_COMM_WORLD,new_group,new_comm,mpierror)

 print*,"T0 - Rank = ",mpirank,", valor = ",valor

 if(mpirank==1 .or. mpirank==3)then
!                variavel,tamanho,tipo,origem,grupo,erro
     call MPI_Bcast(valor,1,MPI_INTEGER,0,new_comm,mpierror)
 endif

 print*, "T1 - Rank = ", mpirank, "valor=", valor

 call MPI_Barrier(MPI_COMM_WORLD,mpierror)

! finalizacao da MPI
  call MPI_Finalize(mpierror)
end program firstmpi
