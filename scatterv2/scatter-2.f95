Program mpi_scatterv2
    USE MPI
    IMPLICIT NONE
    integer id, ierr, n, rank, ncols, np, L1, L2, root, i,j
    integer, allocatable :: sendcnt(:), offsets(:)
    logical convergent
    real (kind=8), allocatable :: A(:, :), alocal(:,:)
    real (kind=8), allocatable :: b(:), ab(:), c(:), c2(:)
    real (kind=8) tol, e_tm
    !--------------------------------------------------------------
    call MPI_INIT(ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, np, ierr)
    !
    root = 0
    !
    if ( rank == root ) then
        write(6,*) 'Enter dimension size - '
        read(5,*) n
        ALLOCATE( A(1:n,1:n), b(1:n), c(1:n), c2(1:n), sendcnt(0:np-1), &
        offsets(0:np-1) )
        !  	 call random_number(A); call random_number(b) ! Fill them with rands
        !
        do i=1, n
            b(i) = 1.0
            do j=1, n
                call random_number( a(j,i))
            enddo
        enddo

        do id=0, np-1
            call BLOCK_MAP(1, n, np, id, L1, L2) ! Block-Map n cols across np procs
            sendcnt(id) = ( L2 - L1 + 1 ) * n    ! Space for n-element cols for proc id
            if ( id == 0 ) then
                offsets(0) = 0
            else
                offsets(id) = offsets(id-1) + sendcnt(id-1)
            end if
        enddo

    endif

    call MPI_BCAST(n, 1, MPI_INTEGER, root, MPI_COMM_WORLD, ierr)
    If ( rank /= root ) ALLOCATE(  b(1:n) )
    call MPI_BCAST(b, n, MPI_REAL8, root, MPI_COMM_WORLD, ierr)

    call BLOCK_MAP(1, n, np, rank, L1, L2) ! Block-Map iters
    ncols = L2 - L1 + 1

    ALLOCATE( alocal(1:n, 1:ncols), ab(1:n) ); alocal = 0.0

    call mpi_scatterv(A, sendcnt, offsets, MPI_REAL8, alocal, &
    n*ncols, MPI_REAL8, root, MPI_COMM_WORLD, ierr)
    ab = 0.0
    do i=L1, L2
        ab = ab + b(i) * alocal(1:n, i-L1+1)
    end do

    call MPI_REDUCE(ab, c, n, MPI_REAL8, MPI_SUM, root, &
    MPI_COMM_WORLD, ierr)

    if ( rank == root ) then
        ! &
        c2 = matmul(A,b)             ! F90 intrinsic: c2=A*b
        !                               Confirm correctness
        write(6,fmt='(/"***", I4,"x",I4, &
        "Mat-vec multiplication ***",/) ') n,n
        write(6,fmt='("Matmul ---- Scatterv2" )')
        write(6,fmt='(4X,F9.4,6X,F9.4)') (c2(i), c(i), i=1, n)
    end if

    !
    call MPI_FINALIZE(ierr)
    stop
end Program mpi_scatterv2

SUBROUTINE BLOCK_MAP(N1, N2, NPROCS, MYID, L1, L2)
    ! Assigns L2-L1+1 consecutive iterations to MYID
    ! process. Assigned number of iterations vary by, at most, 1.
    INTEGER N1, N2, NPROCS, MYID, MYSIZ, L1, L2, RES
    !
    MYSIZ = (N2-N1+1)/NPROCS
    RES = MOD(N2-N1+1,NPROCS)
    IF ( MYID < RES ) THEN  ! First RES-1 proc’s get one extra
        MYSIZ = MYSIZ + 1
        L1 = N1 + MYSIZ*MYID
    ELSE
        L1 = N1 + RES + MYID*MYSIZ
    ENDIF
    L2 = MIN(L1 + MYSIZ - 1,N2)
    !
    RETURN
END
