PROGRAM RTM_ZERO_OFFSET


	USE MPI

	IMPLICIT NONE
		INTEGER(kind=4) :: myid ! numero associado a cada processador, comecando do zero
		INTEGER(kind=4) :: nprocs ! numero de processadores que serao utilizados
		INTEGER(kind=4) :: ierr ! erro para ver se tá certo
		INTEGER :: nx,nz,nxnz,nt,tag
		INTEGER :: jsta, jend, jsta2, jend2
		INTEGER :: isend1, isend2, isend3, isend4, irecv1, irecv2, irecv3, irecv4
		INTEGER :: realsize, inext, iprev
		INTEGER :: is,itsnap,ix,iz,it
		INTEGER :: thefile, filesize, disp
		INTEGER :: w(3)
		REAL, DIMENSION(:,:), ALLOCATABLE :: vel,p,lap,pp,paux,vel2,timesec,aux
		REAL :: dz2inv,dx2inv,g,alfa,dt,dx,dz
		REAL :: dx2,dz2
		real*8 start,finish
		INTEGER :: status(MPI_STATUS_SIZE)
		character*20 filename


			CALL MPI_init(ierr)
			CALL MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr)
			CALL MPI_Comm_rank(MPI_COMM_WORLD, myid, ierr)

			! DECLARAÇÃO DE ALGUNS PARAMETROS

			nt=2504
			dt=0.002
			nz=300
			nx=1290
			dx=40.
			dz=40.
			dx2=dx*dx
			dz2=dz*dz
			nxnz= nx*nz
			alfa=0.2 ! é um parâmetro utilizado para razão de dispersão
			g=10.0   ! número de pontos por menor comprimento de onda
			dz2inv=1./dz2
			dx2inv=1./dx2


			CALL PARA_RANGE_1(1,nx,nprocs,myid,jsta,jend)
			
			ALLOCATE(vel(nz,nx),timesec(nt,nx))
		
			IF (myid.eq.0) THEN
				OPEN(10,file='vel-zero.bin',STATUS='UNKNOWN',ACCESS='DIRECT',FORM='UNFORMATTED',RECL=4*nxnz)
					READ(10,REC=1) vel
				CLOSE(10)
				CALL SYSTEM("rm -rf sec.bin")
			! Dividir o campo de velocidade por 2, por causa que é secao zero offset.
				vel=vel/2.
				OPEN(15,file='seg-zero2ms.bin',STATUS='UNKNOWN',ACCESS='DIRECT',FORM='UNFORMATTED',RECL=4*nx*nt)
					READ(15,REC=1) timesec
				CLOSE(15)
			END IF

			CALL MPI_BCAST(vel,nx*nz,MPI_REAL,0,MPI_COMM_WORLD,ierr)
			CALL MPI_BCAST(timesec,nx*nt,MPI_REAL,0,MPI_COMM_WORLD,ierr)
		
			
			ALLOCATE(lap(nz,nx),p(nz,nx),pp(nz,nx),paux(nz,nx),vel2(nz,nx),aux(nz,nx))
			
			vel2=vel*vel*(dt**2)
			
			IF (myid.eq.0) THEN
				OPEN(20,FILE='snaps-para-rtm.bin',STATUS='UNKNOWN',ACCESS='DIRECT',FORM='UNFORMATTED',RECL=4*nxnz)
			END IF		

			is=1
			itsnap=500

			jsta2=jsta
			jend2=jend
			

			IF (myid.eq.0) jsta2=3 ! PARA PASSAR A PARTIR DO PONTO 3, POIS IRÃO SER PASSADOS 2 ELEMENTOS ANTERIORES
			IF (myid.eq.0) jend2=nx-2

			inext=myid+1
			iprev=myid-1

			IF(myid.eq.(nprocs-1)) THEN
				inext= MPI_PROC_NULL
			END IF	
	
			IF(myid.eq.0) THEN
				iprev= MPI_PROC_NULL
			END IF	

			p=0.
			pp=0.
			paux=0.

			start=MPI_WTIME()

			DO it=nt,1,-1

				p(1,1:nx)=p(1,1:nx)+timesec(it,1:nx)

				CALL MPI_ISEND(p(1,jend), nz, MPI_REAL, inext, 1, MPI_COMM_WORLD, isend1, ierr)
				CALL MPI_ISEND(p(1,jsta), nz, MPI_REAL, iprev, 1, MPI_COMM_WORLD, isend2, ierr)
				CALL MPI_IRECV(p(1,jsta-1), nz, MPI_REAL, iprev, 1, MPI_COMM_WORLD, irecv1, ierr)
				CALL MPI_IRECV(p(1,jend+1), nz, MPI_REAL, inext, 1, MPI_COMM_WORLD, irecv2, ierr)


				CALL MPI_ISEND(p(1,jend-1), nz, MPI_REAL, inext, 1, MPI_COMM_WORLD, isend3, ierr)
				CALL MPI_ISEND(p(1,jsta+1), nz, MPI_REAL, iprev, 1, MPI_COMM_WORLD, isend4, ierr)
				CALL MPI_IRECV(p(1,jsta-2), nz, MPI_REAL, iprev, 1, MPI_COMM_WORLD, irecv3, ierr)
				CALL MPI_IRECV(p(1,jend+2), nz, MPI_REAL, inext, 1, MPI_COMM_WORLD, irecv4, ierr)	
		
		
				CALL MPI_WAIT(isend1, status, ierr)
				CALL MPI_WAIT(isend2, status, ierr)
				CALL MPI_WAIT(isend3, status, ierr)
				CALL MPI_WAIT(isend4, status, ierr)

				CALL MPI_WAIT(irecv1, status, ierr)
				CALL MPI_WAIT(irecv2, status, ierr)
				CALL MPI_WAIT(irecv3, status, ierr)
				CALL MPI_WAIT(irecv4, status, ierr)
			

				! CALCULO DO LAPLACIANO
				lap=0.0
	
				DO ix=jsta2,jend2
					DO iz=3,nz-2
	lap(iz,ix)=(-2.5*p(iz,ix)+4./3.*(p(iz+1,ix) + p(iz-1,ix))-1./12.*(p(iz+2,ix) + p(iz-2,ix)))*dz2inv
	lap(iz,ix)= lap(iz,ix)+(-2.5*p(iz,ix)+4./3.*(p(iz,ix+1) + p(iz,ix-1))-1./12.*(p(iz,ix+2) + p(iz,ix-2)))*dx2inv				
	
					END DO
				END DO

				
			!IF (myid.eq.0) THEN
			! print *, 'lap(max, min)= ', maxval(lap) ,minval(lap) 
				!OPEN(100,FILE='laplaciano.txt',STATUS='UNKNOWN',FORM='UNFORMATTED')
				!WRITE(100,*) lap
			!END IF
				
				DO ix=jsta,jend
					DO iz=1,nz
						pp(iz,ix)=2*p(iz,ix)-pp(iz,ix)+vel2(iz,ix)*lap(iz,ix)
					END DO
				END DO

				paux(1:nz,jsta:jend)=pp(1:nz,jsta:jend)
				pp(1:nz,jsta:jend)=p(1:nz,jsta:jend)
				p(1:nz,jsta:jend)=paux(1:nz,jsta:jend)

				IF (mod(it,itsnap).eq.0) THEN

					CALL MPI_ALLREDUCE(paux,aux,nx*nz,MPI_REAL,MPI_SUM,MPI_COMM_WORLD,ierr)

					IF (myid.eq.0) THEN
						WRITE(20,REC=is) aux
						is=is+1
					END IF
				END IF		

			END DO

			finish=MPI_WTIME()

			CALL MPI_ALLREDUCE(paux,aux,nx*nz,MPI_REAL,MPI_SUM,MPI_COMM_WORLD,ierr)

		
			IF (myid.eq.0) THEN


				! Filtragem de baixa frequencia para retirar o ruido	
				DO ix=3,nx-2
					DO iz=3,nz-2
lap(iz,ix)=(-2.5*aux(iz,ix)+4./3.*(aux(iz+1,ix) + aux(iz-1,ix))-1./12.*(aux(iz+2,ix) + aux(iz-2,ix)))*dz2inv
lap(iz,ix)= lap(iz,ix)+(-2.5*aux(iz,ix)+4./3.*(aux(iz,ix+1) + aux(iz,ix-1))-1./12.*(aux(iz,ix+2) + aux(iz,ix-2)))*dx2inv				
					END DO
				END DO			
	
				OPEN(55,FILE='sec-para-mig-lap.bin',STATUS='UNKNOWN',ACCESS='DIRECT',FORM='UNFORMATTED',RECL=4*nx*nz)	
				WRITE(55,REC=1) lap
				CLOSE(55)


				OPEN(50,FILE='sec-para-mig.bin',STATUS='UNKNOWN',ACCESS='DIRECT',FORM='UNFORMATTED',RECL=4*nx*nz)	
				WRITE(50,REC=1) aux
				CLOSE(50)

				!print *, 'nt = ', nt
			END IF


			PRINT *,'Tempo =', finish-start



			CALL MPI_FINALIZE(ierr)

END PROGRAM RTM_ZERO_OFFSET



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

