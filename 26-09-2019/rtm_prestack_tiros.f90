PROGRAM MODELAGEM


	USE MPI
	USE MODULO_FONTE

	IMPLICIT NONE
		INTEGER(kind=4) :: myid ! numero associado a cada processador, comecando do zero
		INTEGER(kind=4) :: nprocs ! numero de processadores que serao utilizados
		INTEGER(kind=4) :: ierr ! erro para ver se tá certo
		INTEGER :: nx,nz,nxnz,nt,tag,isx,isz,nx2
		INTEGER :: isx1, isx2, ntiros, itiros
		INTEGER :: jsta, jend, jsta2, jend2
		INTEGER :: isend1, isend2, isend3, isend4, irecv1, irecv2, irecv3, irecv4
		INTEGER :: realsize, inext, iprev
		INTEGER :: is,itsnap,ix,iz,it
		INTEGER :: thefile, filesize, disp
		REAL, DIMENSION(:,:), ALLOCATABLE :: vel,p,lap,pp,paux,vel2,timesec,timesec2,aux,timesec2aux, imag
		REAL, DIMENSION(:), ALLOCATABLE :: srt
		REAL, DIMENSION(:,:,:), ALLOCATABLE :: aux3d
		REAL :: dz2inv, dx2inv, g, alfa,tmax, vmax, vmin, dl, dt, fmax, fpeak, tdelay, dx,dz
		REAL :: dx2,dz2
		real*8 start,finish
		INTEGER :: status(MPI_STATUS_SIZE)
		character*20 filename


			CALL MPI_init(ierr)
			CALL MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr)
			CALL MPI_Comm_rank(MPI_COMM_WORLD, myid, ierr)

			! DECLARAÇÃO DE ALGUNS PARAMETROS

			ntiros=7
			nz=300
			nx=1290
			nx2=201
			dx=40.
			dz=40.
			dx2=dx*dx
			dz2=dz*dz
			nxnz= nx*nz
			alfa=0.2 ! é um parâmetro utilizado para razão de dispersão
			g=10.0   ! número de pontos por menor comprimento de onda
			tmax=4.0
			dz2inv=1./dz2
			dx2inv=1./dx2
		
			CALL PARA_RANGE_1(1,nx,nprocs,myid,jsta,jend)
			
			ALLOCATE(vel(nz,nx))
		
			IF (myid.eq.0) THEN

				OPEN(10,file='vel-zero.bin',STATUS='UNKNOWN',ACCESS='DIRECT',FORM='UNFORMATTED',RECL=4*nxnz)
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

			IF (myid.eq.0) THEN
				print *, 'nt =', nt
			END IF	
		
			ALLOCATE(lap(nz,nx),srt(nt),p(nz,nx),pp(nz,nx),paux(nz,nx),vel2(nz,nx),timesec(nt,nx),timesec2(nt,nx),aux(nz,nx))

			ALLOCATE(aux3d(nz,nx,nt),timesec2aux(nt,nx))
	
			ALLOCATE(imag(nz,nx))
			
			vel2=vel*vel*(dt**2)

			CALL source_init(srt,dt,nt,fpeak,tdelay)
			

			isx=0 ! posição x da fonte
			isz=10 ! posição z da fonte
			timesec2aux=0.
			imag=0.


		 	IF (myid.eq.0) THEN
			OPEN(50,FILE='sec-para-mod-tiros.bin',STATUS='UNKNOWN',ACCESS='DIRECT',FORM='UNFORMATTED',RECL=4*nt*nx)	
			END IF

			DO itiros=1,ntiros

				IF (itiros.eq.1) THEN
					isx1=isx
					isx2=isx+200
				ELSE	
					isx1=isx-200
					isx2=isx+200 
				END IF


				IF (itiros.eq.ntiros) THEN
					isx1=isx-200
					isx2=1290
				ELSE
					isx1=isx-200
					isx2=isx+200
				END IF

				is=1

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
lap(iz,ix)=(-2.5*p(iz,ix)+4./3.*(p(iz+1,ix) + p(iz-1,ix))-1./12.*(p(iz+2,ix) + p(iz-2,ix)))*dz2inv
lap(iz,ix)= lap(iz,ix)+(-2.5*p(iz,ix)+4./3.*(p(iz,ix+1)+p(iz,ix-1))-1./12.*(p(iz,ix+2)+p(iz,ix-2)))*dx2inv				
	
						END DO
					END DO
				
					DO ix=jsta,jend
						DO iz=1,nz
							pp(iz,ix)=2*p(iz,ix)-pp(iz,ix)+vel2(iz,ix)*lap(iz,ix)
						END DO
					END DO

					paux(1:nz,jsta:jend)=pp(1:nz,jsta:jend)
					pp(1:nz,jsta:jend)=p(1:nz,jsta:jend)
					p(1:nz,jsta:jend)=paux(1:nz,jsta:jend)

				!	IF (mod(it,itsnap).eq.0) THEN
						CALL MPI_ALLREDUCE(paux,aux,nz*nx,MPI_REAL,MPI_SUM,MPI_COMM_WORLD,ierr)
			

						IF (myid.eq.0) THEN
							aux3d(1:nz,isx1:isx2,it)=aux(1:nz,isx1:isx2)
						END IF
				
			
					timesec(it,jsta:jend)=p(30,jsta:jend)

				END DO


				CALL MPI_ALLREDUCE(timesec,timesec2,nt*nx,MPI_REAL,MPI_SUM,MPI_COMM_WORLD,ierr)

				timesec2aux(1:nt,isx1:isx2)=timesec2(1:nt,isx1:isx2)

		


			IF (myid.eq.0) THEN
				WRITE(50,REC=itiros) timesec2aux

				!print *, 'nt = ', nt
			END IF


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
			aux=0.



			DO it=nt,1,-1

				p(1,isx1:isx2)=p(1,isx1:isx2)+timesec2(it,isx1:isx2)

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

				
				
				DO ix=jsta,jend
					DO iz=1,nz
						pp(iz,ix)=2*p(iz,ix)-pp(iz,ix)+vel2(iz,ix)*lap(iz,ix)
					END DO
				END DO

				paux(1:nz,jsta:jend)=pp(1:nz,jsta:jend)
				pp(1:nz,jsta:jend)=p(1:nz,jsta:jend)
				p(1:nz,jsta:jend)=paux(1:nz,jsta:jend)
				
				aux=0.
			

				CALL MPI_ALLREDUCE(paux,aux,nx*nz,MPI_REAL,MPI_SUM,MPI_COMM_WORLD,ierr)		

				IF (myid.eq.0) THEN
				! Condição de Imagem
					imag(1:nz,isx1:isx2)=imag(1:nz,isx1:isx2)+aux(1:nz,isx1:isx2)*aux3d(1:nz,isx1:isx2,it)
				END IF

			END DO

				isx=isx+200

		END DO

			
			!CALL MPI_ALLREDUCE(paux,aux,nx*nz,MPI_REAL,MPI_SUM,MPI_COMM_WORLD,ierr)

	IF (myid.eq.0) THEN

				CLOSE(50)
				! Filtragem de baixa frequencia para retirar o ruido	
				DO ix=3,nx-2
					DO iz=3,nz-2
lap(iz,ix)=(-2.5*imag(iz,ix)+4./3.*(imag(iz+1,ix) + imag(iz-1,ix))-1./12.*(imag(iz+2,ix) + imag(iz-2,ix)))*dz2inv
lap(iz,ix)= lap(iz,ix)+(-2.5*imag(iz,ix)+4./3.*(imag(iz,ix+1)+imag(iz,ix-1))-1./12.*(imag(iz,ix+2)+imag(iz,ix-2)))*dx2inv				
					END DO
				END DO			
	
				OPEN(55,FILE='sec-para-mig-lap-prestack.bin',STATUS='UNKNOWN',ACCESS='DIRECT',FORM='UNFORMATTED',RECL=4*nx*nz)	
				WRITE(55,REC=1) lap
				CLOSE(55)


				OPEN(51,FILE='sec-para-mig-prestack.bin',STATUS='UNKNOWN',ACCESS='DIRECT',FORM='UNFORMATTED',RECL=4*nx*nz)	
				WRITE(51,REC=1) imag
				CLOSE(51)

				!print *, 'nt = ', nt
			END IF


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

