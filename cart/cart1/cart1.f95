        PROGRAM Cartesian

! Run with 12 processes

        USE MPI

      INTEGER err, rank, size,i,j
      integer vu,dim(2),coord(2),id
      logical period(2),reorder

      CALL MPI_INIT(err)
      CALL MPI_COMM_RANK(MPI_COMM_WORLD,rank,err)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD,size,err)

      dim(1)=4
      dim(2)=3

      period(1)=.true.
      period(2)=.false.
      reorder=.true.

      call MPI_CART_CREATE(MPI_COMM_WORLD,2,dim,period,reorder,vu,err)

      if(rank.eq.5) then
        call MPI_CART_COORDS(vu,rank,2,coord,err)
        print*,'P:',rank,' my coordinates are',coord
      end if

      if(rank.eq.0) then
        coord(1)=3
        coord(2)=1
        call MPI_CART_RANK(vu,coord,id,err)
        print*,'P:',rank,' processor at position',coord,' is',id

        coord(1)=2
        coord(2)=1
        call MPI_CART_RANK(vu,coord,id,err)
        print*,'P:',rank,' processor at position',coord,' is',id

        coord(1)=2
        coord(2)=1
        call MPI_CART_RANK(vu,coord,id,err)
        print*,'P:',rank,' processor at position',coord,' is',id

       !do i=0,4
       !    do j=0,3
       !        coord(1) = i
       !        coord(2) = j
       !        call MPI_CART_RANK(vu,coord,id,err)
       !        write(*,'(i2)',advance='no')  id
       !    end do
       !    write(0,*)' '
       !end do
      end if


        CALL MPI_FINALIZE(err)

        END Program Cartesian

