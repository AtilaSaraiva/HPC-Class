	program matvec 

 ! Perform matrix vector product  --- Y = A X

	USE MPI 

	integer, parameter ::  dim1=80 , dim2=10, dim3=dim1*dim2 
	integer ierr, rank, size, root, i, j 

	real, dimension(dim1,dim1) :: a  
	real, dimension(dim1,dim2) :: apart
	real, dimension(dim1) ::  x,y, ypart 
	real, dimension(dim2) :: xpart
        
	root=0

	call MPI_INIT ( ierr ) 
	call MPI_COMM_RANK ( MPI_COMM_WORLD, rank, ierr) 
	call MPI_COMM_SIZE ( MPI_COMM_WORLD, size, ierr)

	print *, 'START process on processor ', rank 

	if (rank == root) then 

	do i=1, dim1
	 x = 1.0 
	       do j=1, dim1
	       a(j,i) =  1.0 
	       enddo
	enddo
	endif 

	call MPI_SCATTER (a, dim3, MPI_REAL, apart, dim3, MPI_REAL, root, & 
			MPI_COMM_WORLD, ierr) 


	call MPI_SCATTER (x, dim2, MPI_REAL, xpart, dim2, MPI_REAL, root, & 
			MPI_COMM_WORLD, ierr)

	do j=1, dim1
	     ypart(j) =0.0
	enddo
		do i=1,dim2 
		  do j=1, dim1 
		  ypart(j) = ypart(j) + xpart(i)*apart(j,i)
		  enddo
		enddo 
	

	call  MPI_REDUCE ( ypart, y, dim1,  MPI_REAL, MPI_SUM, root, &
                           MPI_COMM_WORLD, ierr) 

         print *, 'Finish processor ', rank 

	if ( rank == root ) then 

	print *,'Matrix vector productor,elements 10 and 60, are :', y(10), y(60)

	endif 

	call MPI_FINALIZE ( ierr) 

	stop 

	end program matvec 
