    program vecsum

! This program computes  z = a x + by  (x,y - vectors ) - on 8 processes

	USE MPI

	integer, parameter ::  dim1=80 , dim2=10
	integer ierr, rank, size, root

	real, dimension(dim1) :: x, y, z
	real, dimension(dim2) :: xpart, ypart, zpart
	real, dimension(2) :: coeff
        integer:: i

	root=0

	call MPI_INIT ( ierr )
	call MPI_COMM_RANK ( MPI_COMM_WORLD, rank, ierr)
	call MPI_COMM_SIZE ( MPI_COMM_WORLD, size, ierr)

	print *, 'START process on processor ', rank

	if (rank == root) then

	coeff = (/0.0, 1.0 /)

	x = 1.0
        do i=1,dim1
            y (i)= i
        end do

	endif

	call MPI_SCATTER (x, dim2, MPI_REAL, xpart, dim2, MPI_REAL, root, &
			MPI_COMM_WORLD, ierr)


	call MPI_SCATTER (y, dim2, MPI_REAL, ypart, dim2, MPI_REAL, root, &
			MPI_COMM_WORLD, ierr)


	call  MPI_BCAST ( coeff, 2, MPI_REAL, root, MPI_COMM_WORLD, ierr)

	do i=1, dim2
            zpart(i) = coeff(1)*xpart(i) + coeff(2)*ypart(i)
	enddo


	call MPI_GATHER ( zpart, dim2, MPI_REAL, z, dim2, MPI_REAL, root, &
			MPI_COMM_WORLD, ierr)

	print *, 'Finish processor ', rank

	if ( rank == root ) then

            print *,'vector sum, elements 10 and 60, are ', z(10) , z(60)
            do i=1,dim1
                write(0,*) z(i)
            end do

	endif

	call MPI_FINALIZE ( ierr)

	stop

    end program vecsum
