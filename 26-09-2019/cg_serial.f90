PROGRAM CG_Serial

	IMPLICIT NONE
	INTEGER, PARAMETER :: n=4
	INTEGER, PARAMETER :: m=4
	REAL, DIMENSION(:) :: mo(n)
	REAL, ALLOCATABLE :: g(:), v(:), p(:), A(:,:),  Gx(:,:), b(:), d(:), vtil(:)
	INTEGER :: k, i, j
	REAL :: lim=0.0001
	REAL :: gtv, gtv1, alf, bet, vg, vp, vtilp
	
	ALLOCATE(g(n),v(n),p(n),Gx(m,n),A(n,n),d(m),b(n),vtil(n))

	DO j=1,m
		DO i=1,n
			Gx(j,i)=(i+j)*1.
		END DO
		d(j)=0.25*(j+1)
	END DO	

	! Faz um B cast para todos terem G, sendo que define G só em 0, Mpi barrier
	! Faz um b cast do Gx e dos dados

	! Calcular por MPI, aloca uma parte da matrix em cada processador usando jsta e jend	
	A=MATMUL(TRANSPOSE(Gx),Gx)

	! Calcular por MPI, aloca b e bpart, depois usa all reduce
	b=MATMUL(TRANSPOSE(Gx),d)	

	mo(1:n)=0.0 ! modelo inicial

	! Calcular por MPI
	g(1:n)=-1*b(1:n) + MATMUL(A,mo)

	v(1:n)=g(1:n)

	!k=1

	!gtv=0.0 ! residuos

	!gtv=dot_product(g,v)

	!DO WHILE(k.eq.1)


	vg=0.0
	vp=0.0
	vtilp=0.0
	DO k=1,10
		! calcula p em paralelo	aloca p e ppart, depois usa all reduce	e usa o mpi_barrier
		p=MATMUL(A,v)
		vg=dot_product(v,g)
		vp=dot_product(v,p)
		alf=-1.*vg/vp
	
		mo=mo+alf*v
		g=g+alf*p		
		vtil=g
		

		vtilp=dot_product(vtil,p)
		bet=-1.*vtilp/vp		
		v=vtil+bet*v
	END DO

	print *, mo

	DEALLOCATE (g,v,p, A, Gx, b, d, vtil)

		



END PROGRAM CG_Serial
