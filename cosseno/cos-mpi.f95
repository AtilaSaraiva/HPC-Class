	PROGRAM MPI_COS_LOOP
	USE MPI
	IMPLICIT NONE
	INTEGER np, myid, ierr,l1,l2, i, mydim
	INTEGER, PARAMETER :: N=40 ! Total Array size (=NXN)
        REAL (KIND=8), ALLOCATABLE :: AA_LOCAL(:)
	REAL (KIND=8) ANG_INCR, PI
	CHARACTER (LEN=4) itoa
	CHARACTER (LEN=12) FN, fmtstr ! File Name & write format strg !

	call MPI_INIT(ierr)
	call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ierr)
	call MPI_COMM_SIZE(MPI_COMM_WORLD, np, ierr)
!
	FN = 'mpi_cos'//CHAR(48+myid)//'.dat' ! Proc-specific file name
        OPEN(UNIT=myid+10, FILE=FN, FORM='formatted', STATUS='unknown')
!
	PI = 4.0D1 * ATAN(1.0D1)
	ANG_INCR = (2.0D1 * PI)/N  ! Computation increment
! Map iterations L1 thru L2 to myid

	call BLOCK_MAP(1, N, np, myid, L1, L2)
!
	mydim = L2 - L1 + 1
	PRINT *, 'myid =',myid,' array_size per process =',mydim !
	ALLOCATE( AA_LOCAL(1:mydim) ) ! AA_LOCAL per process
!
	DO I=L1, L2
	AA_LOCAL(I-L1+1) = COS( REAL(I,KIND=8) * ANG_INCR )
        END DO
!
	fmtstr = '('//trim(ITOA(mydim))//'(F6.4,X))'

        WRITE(UNIT=myid+10, FMT=fmtstr) AA_LOCAL
        DEALLOCATE( AA_LOCAL )
!
	call MPI_FINALIZE(ierr)
	STOP
	END PROGRAM  MPI_COS_LOOP


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

	RECURSIVE FUNCTION itoa(n) RESULT(str)
       ! Converts int# to numeric strg
        CHARACTER (LEN=4) :: str
	INTEGER, INTENT(IN) :: n
	IF (n < 10) THEN
	   str = CHAR(n + 48)
	ELSE
	str = itoa(n/10)
	str = TRIM(str)//TRIM(CHAR(MOD(n, 10) + 48))
	END IF
	END FUNCTION itoa
