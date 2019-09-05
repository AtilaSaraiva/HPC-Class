program firstmpi
        ! inclusão da bilbioteca MPI
        use mpi
        integer:: mpierror,mpisize,mpirank

        ! Inicialização da MPI
        call MPI_Init(mpierror)
        call MPI_Comm_size(MPI_COMM_WORLD,mpisize,mpierror)
        call MPI_Comm_rank(MPI_COMM_WORLD,mpirank,mpierror)

        ! processamento
        if(mpirank==0)then
                write(0,*) "EU SOU O MESTRE MUHAHAHA,  mpirank =",mpirank
        else
                write(0,*) "sou escravo!,  mpirank =",mpirank
        end if

        ! finalização da MPI
        call MPI_Finalize (mpierror)
end program firstmpi
