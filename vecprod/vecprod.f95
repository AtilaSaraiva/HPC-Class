
	program vecprod

! This program computes  z = x.y (x,y - vectors ) - on 8 processes

	USE MPI

	integer, parameter ::  dim1=80 , dim2=10
	integer ierr, rank, size, root

	real, dimension(dim1) :: x, y
	real, dimension(dim2) :: xpart, ypart
        real  z, zpart

	root=0

	call MPI_INIT ( ierr )
	call MPI_COMM_RANK ( MPI_COMM_WORLD, rank, ierr)
	call MPI_COMM_SIZE ( MPI_COMM_WORLD, size, ierr)

	print *, 'START process on processor ', rank

	if (rank == root) then

	x = 1.0
	y = 2.0

	endif

	call MPI_SCATTER (x, dim2, MPI_REAL, xpart, dim2, MPI_REAL, root, &
			MPI_COMM_WORLD, ierr)


	call MPI_SCATTER (y, dim2, MPI_REAL, ypart, dim2, MPI_REAL, root, &
			MPI_COMM_WORLD, ierr)

	zpart=0.0
	do i=1, dim2
	zpart = zpart + xpart(i)*ypart(i)
	enddo


	call MPI_REDUCE ( zpart, z, 1, MPI_REAL, MPI_SUM, root, &
			MPI_COMM_WORLD, ierr)

	print *, 'Finish processor ', rank

	if ( rank == root ) then

	print *,'vector product  is:', z

	endif

	call MPI_FINALIZE ( ierr)

	stop

	end program vecprod
