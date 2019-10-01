PROGRAM CG_Serial

	USE MPI

	IMPLICIT NONE
	
	INTEGER(kind=4) :: myid ! numero associado a cada processador, comecando do zero
	INTEGER(kind=4) :: nprocs ! numero de processadores que serao utilizados
	INTEGER(kind=4) :: ierr ! erro para ver se tá certo
	INTEGER, PARAMETER :: n=7
	INTEGER, PARAMETER :: m=7
	REAL, ALLOCATABLE :: g(:), v(:), p(:), A(:,:), b(:), vtil(:)
	REAL, ALLOCATABLE :: gpart(:), ppart(:), bpart(:)
	REAL, ALLOCATABLE :: Gx(:,:), d(:), mo(:)
	INTEGER :: k, i, j
	INTEGER :: jsta, jend, ncols
	REAL :: lim=0.0001
	REAL :: gtv, gtv1, alf, bet, vp, vg, vtilp
	
	ALLOCATE(Gx(n,m),d(n),mo(m))

	CALL MPI_init(ierr)
	CALL MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr)
	CALL MPI_Comm_rank(MPI_COMM_WORLD, myid, ierr)

	! modelo inicial
	mo(1:m)=1.0 
	
	CALL PARA_RANGE_1(1,n,nprocs,myid,jsta,jend)	
	!ncols=jend-jsta+1

	IF (myid.eq.0) THEN
		DO j=1,n
			DO i=1,m
				Gx(i,j)=(i+j)*0.1
			END DO
			d(j)=0.25*(j+1)
		END DO	
	END IF

	! Faz um Bcast para todos terem G, sendo que define G só em 0, Mpi barrier
	CALL MPI_BCAST(Gx,n*m,MPI_REAL,0,MPI_COMM_WORLD,ierr)
	CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)

	! Faz um bcast dos dados
	CALL MPI_BCAST(d,n,MPI_REAL,0,MPI_COMM_WORLD,ierr)
	CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
	
	ALLOCATE(A(n,jsta:jend),b(m),bpart(m),v(m),g(m),gpart(m),p(m),ppart(m),vtil(m))
	!A=MATMUL(TRANSPOSE(Gx),Gx)
	A=0.0
	DO j=jsta,jend
		DO i=1,n
		A(i,j)=dot_product(Gx(1:n,i),Gx(1:n,j))
		END DO
	END DO

	! Calcular por MPI, aloca b e bpart, depois usa all reduce 	!b=MATMUL(TRANSPOSE(Gx),d)	
	bpart = 0.0
	b=0
	
	DO j=jsta,jend
		bpart(j)=dot_product(Gx(1:n,j),d(:))
	END DO

	CALL MPI_ALLREDUCE(bpart,b,m,MPI_REAL,MPI_SUM,MPI_COMM_WORLD,ierr)
	CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)

	! Calcular por MPI	!g(1:n)=-1*b(1:n) + MATMUL(A,mo)

	gpart=0.0
	g=0.0
	gpart=matmul(A(:,jsta:jend),mo(jsta:jend)) 	
	!DO j=jsta,jend
	!	gpart(1:m)= gpart(1:m)+A(1:m,j)*mo(j)
	!END DO
	CALL MPI_ALLREDUCE(gpart,g,m,MPI_REAL,MPI_SUM,MPI_COMM_WORLD,ierr)

	g = g-b
	CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)

	v=g

	DO k=1,10
		ppart=0.0
		p=0.0
		!DO j=jsta,jend
		!	ppart(1:m)= ppart(1:m)+A(1:m,j)*v(j)
		!END DO
			ppart=matmul(A(:,jsta:jend),v(jsta:jend)) 	
		
		CALL MPI_ALLREDUCE(ppart,p,m,MPI_REAL,MPI_SUM,MPI_COMM_WORLD,ierr)
		CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)

		vg=dot_product(v,g)
		vp=dot_product(v,p)
		alf=-1.*vg/vp	

		mo=mo+alf*v
		g=g+alf*p		
		vtil=g
	
		vtilp=dot_product(vtil,p)

		bet= - vtilp/vp
		v=vtil+bet*v

	END DO

	print *, 'mo = ', mo

	DEALLOCATE (g,v,p, A, Gx, b, d, mo, gpart, ppart, bpart, vtil)

		
	CALL MPI_FINALIZE(ierr)


END PROGRAM CG_Serial

! -----------------

SUBROUTINE PARA_RANGE_1(n1,n2,nprocs,myid,jsta,jend)
	INTEGER :: iwork1, iwork2, n1, n2, nprocs, myid, jsta, jend
	
	iwork1=(n2-n1+1)/nprocs
	iwork2=mod(n2-n1+1,nprocs)
	jsta=myid*iwork1+n1+min(myid,iwork2)
	jend=jsta+iwork1-1

	if (iwork2.gt.myid) then
	jend = jend + 1 
	end if
END SUBROUTINE PARA_RANGE_1
