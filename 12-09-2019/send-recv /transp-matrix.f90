        program trans_matrix

        USE MPI        

        implicit none 

        REAL :: a(5,5), b(5,5)
        INTEGER ::  row, xpose, sizeofreal, myrank, ierr, nprocs
        INTEGER status(MPI_STATUS_SIZE)
        integer :: i, j 

        CALL MPI_INIT(ierr)

        CALL MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr)

        CALL MPI_COMM_RANK(MPI_COMM_WORLD, myrank, ierr)

        !if ( myrank == 0)  then
        do i=1, 5 
        do  j=1, 5 
        a(j,i) = i 
        enddo 
        enddo         

       ! do i=1, 5
       !  print * , 'a', (a(i,j),j=1, 5)
       ! enddo

      !  endif

 
! transpose matrix a into b.
       
        CALL MPI_TYPE_EXTENT(MPI_REAL, sizeofreal, ierr)
        CALL MPI_TYPE_VECTOR(5, 1, 5, MPI_REAL, row, ierr)
        CALL MPI_TYPE_HVECTOR(5, 1, sizeofreal, row, xpose, ierr)
        CALL MPI_TYPE_COMMIT(xpose, ierr)
        CALL MPI_SENDRECV(a, 1, xpose, myrank, 0, a, 5*5,  & 
                   MPI_REAL, myrank,0, MPI_COMM_WORLD, status, ierr)

        if ( myrank == 5)  then 
        do i=1, 5 
         print * , 'a ', (a(i,j),j=1,5)
        enddo 
        endif 

        call MPI_Finalize (ierr) 

        stop 

        end  program trans_matrix

