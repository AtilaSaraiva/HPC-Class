PROGRAM MODELAGEM


	USE MPI
	USE MODULO_FONTE

	IMPLICIT NONE
		INTEGER(kind=4) :: myid ! numero associado a cada processador, comecando do zero
		INTEGER(kind=4) :: nprocs ! numero de processadores que serao utilizados
		INTEGER(kind=4) :: ierr ! erro para ver se tá certo
		INTEGER :: nx,nz,nxnz,nt,tag,isx,isz
		INTEGER :: jsta, jend, jsta2, jend2
		INTEGER :: isend1, isend2, isend3, isend4, irecv1, irecv2, irecv3, irecv4
		INTEGER :: realsize, inext, iprev
		INTEGER :: is,itsnap,ix,iz,it
		INTEGER :: thefile, filesize, disp
		INTEGER :: w(3)
		REAL, DIMENSION(:,:), ALLOCATABLE :: vel,p,lap,pp,paux,vel2,timesec,timesec2,aux
		REAL, DIMENSION(:), ALLOCATABLE :: srt
		REAL :: dz2inv, dx2inv, g, alfa,tmax, vmax, vmin, dl, dt, fmax, fpeak, tdelay, dx,dz
		REAL :: dx2,dz2
		real*8 start,finish
		INTEGER :: status(MPI_STATUS_SIZE)
		character*20 filename


			CALL MPI_init(ierr)
			CALL MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr)
			CALL MPI_Comm_rank(MPI_COMM_WORLD, myid, ierr)

			! DECLARAÇÃO DE ALGUNS PARAMETROS

			nz=400
			nx=400
			dx=10.
			dz=10.
			dx2=dx*dx
			dz2=dz*dz
			isx=200 ! posição x da fonte
			isz=30 ! posição z da fonte
			nxnz= nx*nz
			alfa=0.2 ! é um parâmetro utilizado para razão de dispersão
			g=10.0   ! número de pontos por menor comprimento de onda
			tmax=2.0
			dz2inv=1./dz2
			dx2inv=1./dx2


			W(1)=-1/12.
			W(2)=0.75
			W(3)=-2.5

			CALL PARA_RANGE_1(1,nx,nprocs,myid,jsta,jend)
			
			ALLOCATE(vel(nz,nx))
		
			IF (myid.eq.0) THEN

				OPEN(10,file='model.ad',STATUS='UNKNOWN',ACCESS='DIRECT',FORM='UNFORMATTED',RECL=4*nxnz)
					READ(10,REC=1) vel
				CLOSE(10)

				CALL SYSTEM("rm -rf sec.bin")

			END IF

			CALL MPI_BCAST(vel,nx*nz,MPI_REAL,0,MPI_COMM_WORLD,ierr)

			vmin=minval(vel)
			vmax=maxval(vel)


			! DEFINIÇÃO DE ALGUMAS VARIÁVEIS, INCLUSIVE PARA NAO PERMITIR A DISPERSAO

			dl=min(dx,dz)
			dt=(alfa*dl)/vmax
			fmax=vmin/(g*dl)
			fpeak=fmax/2.
			nt=1+tmax/dt
		
			ALLOCATE(lap(nz,nx),srt(nt),p(nz,nx),pp(nz,nx),paux(nz,nx),vel2(nz,nx),timesec(nt,nx),timesec2(nt,nx),aux(nz,nx))
			
			vel2=vel*vel*(dt**2)
			
			CALL source_init(srt,dt,nt,fpeak,tdelay)	

			IF (myid.eq.0) THEN
				OPEN(20,FILE='snaps-para.bin',STATUS='UNKNOWN',ACCESS='DIRECT',FORM='UNFORMATTED',RECL=4*nxnz)
			END IF		

			is=1
			itsnap=50

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

			DO it=1,nt

				pp(isz,isx)=pp(isz,isx)+srt(it)

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
	!lap(iz,ix)=(w(1)*(p(iz-2,ix)+p(iz+2,ix))+ w(2)*(p(iz-1,ix)+p(iz+1,ix)) +  w(3)*p(iz,ix))*dz2inv
!lap(iz,ix)=lap(iz,ix)+(w(1)*(p(iz,ix-2)+p(iz,ix+2))+ w(2)*(p(iz,ix-1)+p(iz,ix+1)) +  w(3)*p(iz,ix))*dx2inv
	lap(iz,ix)=(-2.5*p(iz,ix)+4./3.*(p(iz+1,ix) + p(iz-1,ix))-1./12.*(p(iz+2,ix) + p(iz-2,ix)))*dz2inv
	lap(iz,ix)= lap(iz,ix)+(-2.5*p(iz,ix)+4./3.*(p(iz,ix+1) + p(iz,ix-1))-1./12.*(p(iz,ix+2) + p(iz,ix-2)))*dx2inv				
	
					END DO
				END DO

				
			IF (myid.eq.0) THEN
			 print *, 'lap(max, min)= ', maxval(lap) ,minval(lap) 
				!OPEN(100,FILE='laplaciano.txt',STATUS='UNKNOWN',FORM='UNFORMATTED')
				!WRITE(100,*) lap
			END IF
				
				DO ix=jsta,jend
					DO iz=1,nz
						pp(iz,ix)=2*p(iz,ix)-pp(iz,ix)+vel2(iz,ix)*lap(iz,ix)
					END DO
				END DO

				paux(1:nz,jsta:jend)=pp(1:nz,jsta:jend)
				pp(1:nz,jsta:jend)=p(1:nz,jsta:jend)
				p(1:nz,jsta:jend)=paux(1:nz,jsta:jend)

				IF (mod(it,itsnap).eq.0) THEN

					CALL MPI_ALLREDUCE(paux,aux,nz*nx,MPI_REAL,MPI_SUM,MPI_COMM_WORLD,ierr)

					IF (myid.eq.0) THEN
						WRITE(20,REC=is) aux
						is=is+1
					END IF
				END IF
				
			
				timesec(it,jsta:jend)=p(30,jsta:jend)

			END DO

			finish=MPI_WTIME()

			CALL MPI_ALLREDUCE(timesec,timesec2,nt*nx,MPI_REAL,MPI_SUM,MPI_COMM_WORLD,ierr)


			IF (myid.eq.0) THEN
				OPEN(50,FILE='sec-para.bin',STATUS='UNKNOWN',ACCESS='DIRECT',FORM='UNFORMATTED',RECL=4*nt*nx)	
				WRITE(50,REC=1) timesec2
				CLOSE(50)

				print *, 'nt = ', nt
			END IF


			PRINT *,'Tempo =', finish-start



			CALL MPI_FINALIZE(ierr)

END PROGRAM MODELAGEM



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

