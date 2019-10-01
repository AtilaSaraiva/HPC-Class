 	program cartesian

        USE MPI	

  	 implicit none

        integer :: myrank, nprocs, ierr 
	
	integer ::  dim(2) 
        logical :: period(2), reorder 
	integer ::  coord(2)
	integer ::  left, right, up, down
        integer ::  new_comm, ndims 

       call MPI_Init(ierr)
       call MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr)
       call MPI_Comm_rank(MPI_COMM_WORLD, myrank, ierr)

	ndims = 2
	dim(1) = 4; dim(2)=3
	period(1) = .false.;  period(2)=.false.;  
	reorder = .false.

      call MPI_Cart_create (MPI_COMM_WORLD, ndims, dim,   &   
                         period, reorder, new_comm, ierr)


      if ( myrank .eq. 7 ) then 

	print *, ' 0  1  2 '
	print *, ' 3  4  5 '
	print *, ' 6  7  8 '
	print *, ' 7  8  9 '

	call MPI_Cart_coords (new_comm, myrank, 2, coord, ierr) 

	print *,  'My rank is ', myrank, 'My coordinates are', coord(1), coord(2) 
	

	call MPI_Cart_shift (new_comm, 0, 1, up, down, ierr) 
	call MPI_Cart_shift (new_comm, 1, 1, left, right, ierr) 
	
	
	print *, 'My neighbors are left', left, 'right', right, 'up', up, 'down', down 

	endif  


	end program cartesian 

