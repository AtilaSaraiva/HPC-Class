program firstmpi
        ! inclusão da bilbioteca MPI
        use mpi
        integer:: mpierror,mpisize,mpirank

        ! Inicialização da MPI
        call MPI_Init(mpierror)
        call MPI_Comm_size(MPI_COMM_WORLD,mpisize,mpierror)
        call MPI_Comm_rank(MPI_COMM_WORLD,mpirank,mpierror)

        ! processamento 
        print*,mpirank,mpisize
        
        ! finalização da MPI
        call MPI_Finalize (mpierror)
end program firstmpi
