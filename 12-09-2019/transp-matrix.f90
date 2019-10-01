        program trans_matrix

        USE MPI        

        implicit none 

        REAL :: a(100,100), b(100,100)
        INTEGER ::  row, xpose, sizeofreal, myrank, ierr, nprocs
        INTEGER status(MPI_STATUS_SIZE)

        CALL MPI_INIT(ierr)

        CALL MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr)

        CALL MPI_COMM_RANK(MPI_COMM_WORLD, myrank, ierr)

 
! transpose matrix a into b.
       
        CALL MPI_TYPE_EXTENT(MPI_REAL, sizeofreal, ierr)
        CALL MPI_TYPE_VECTOR(100, 1, 100, MPI_REAL, row, ierr)
        CALL MPI_TYPE_HVECTOR(100, 1, sizeofreal, row, xpose, ierr)
        CALL MPI_TYPE_COMMIT(xpose, ierr)
        CALL MPI_SENDRECV(a, 1, xpose, myrank, 0, b, 100*100,  & 
                   MPI_REAL, myrank,0, MPI_COMM_WORLD, status, ierr)

        call MPI_Finalize (ierr) 

        stop 

        end  program trans_matrix

