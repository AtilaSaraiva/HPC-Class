	Program test_block

	IMPLICIT NONE

	integer  n1, n2, nprocs, myid, l1, l2

       write(6,*) 'N1 e N2'
       read(5,*) N1, N2

	write(6,*) 'NPROCS'
	read(5,*)  NPROCS


	do myid =0, nprocs-1

        call BLOCK_MAP(N1, N2, NPROCS, MYID, L1, L2)

	print *, 'myid', myid, ' L1 e L2 ', L1, l2

	enddo

	stop

	end Program test_block



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

!wholeEach = n / totalProcs
 !    remainder = n - (wholeEach * totalProcs)
 !    if (remainder .eq. 0) then
 !     a_min = (wholeEach * a_rank) + 1
 !   a_max = a_min + wholeEach - 1
 ! else
 !   if (a_rank .lt. remainder) then
  !    a_min = (wholeEach * a_rank) + 1 + a_rank
   !   a_max = a_min + wholeEach
   ! else
   !   a_min = (wholeEach * a_rank) + 1 + remainder
   !   a_max = a_min + wholeEach - 1
   ! end if
 !  end if
 ! cnt = a_max-a_min+1
