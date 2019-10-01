        Program matrix_transpose

        USE MPI 

        Implicit none

        integer :: my_rank, ncpus, ierr 

        integer, parameter :: DIM=100  

        integer :: Nx, Ny,  i, j, iblock  
        real*4, dimension (:,:),allocatable :: A, B 
        
        real*4, dimension (:),allocatable :: Ctmp, Dtmp
        

        call MPI_Init(ierr)

        call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)

        call MPI_Comm_size(MPI_COMM_WORLD, ncpus, ierr)

        if (my_rank == 0) then

        if ( DIM/ncpus .ne. 0) then  !   make sure DIM can be divided by ncpus
              print *, "ERROR: DIM cannot be divided by ncpus!" 
        call MPI_Finalize(ierr)
        stop 
        endif 

        endif


         Nx = DIM/ncpus
         Ny = DIM

         allocate (A(0:Nx-1, 0:Ny-1))  !  allocate memory
         allocate (B(0:Nx-1, 0:Ny-1), Ctmp((Nx-1)*(Ny-1)), & 
                          Dtmp((Nx-1)*(Ny-1)))  !  work space

        do i =0,Nx-1
        do j =0, Ny-1
        A(i,j) = 2*(my_rank*Nx+i) + j
        enddo; enddo 

        B = 0 !  zero out B


     !  divide A into blocks --> Ctmp; A[i][iblock*Nx+j]  Ctmp[iblock][i][j]

      
      do i =0, Nx-1
        do iblock = 0, ncpus-1    
          do j=0, Nx-1
          Ctmp(iblock*Nx*Nx+i*Nx+j) = A(i,iblock*Nx+j)
      enddo; enddo; enddo 

        !local transpose of A --> Dtmp; Ctmp[iblock][i][j]  Dtmp[iblock][j][i]

       do iblock=0, ncpus-1
        do i=0, Nx-1
           do j=1, Nx-1
           Dtmp(iblock*Nx*Nx+i*Nx+j) = Ctmp(iblock*Nx*Nx+j*Nx+i)
       enddo; enddo; enddo


        ! All-to-all comm --> Ctmp

        call MPI_Alltoall(Dtmp, (Nx-1)*(Nx-1), MPI_REAL, Ctmp, & 
                 (Nx-1)*(Nx-1), MPI_REAL,MPI_COMM_WORLD, ierr)

     ! merge blocks --> B; Ctmp[iblock][i][j]  B[i][iblock*Nx+j]

  
       do i=0, Nx-1
         do iblock=0, ncpus-1
          do j=0,Nx-1
          B(i,iblock*Nx+j) = Ctmp(iblock*Nx*Nx+i*Nx+j)
          enddo; enddo; enddo 

           !  clean up

           Deallocate (A, B, Ctmp, Dtmp ) 

        call MPI_Finalize (ierr) 

       stop 

       end Program matrix_transpose
