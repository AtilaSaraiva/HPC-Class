module mpicontrol
!include 'mpif.h'
! modelu mpi control
USE MPI
integer*4 :: myrank, numprocs, mpierror, length, master
integer*4 :: status(MPI_STATUS_SIZE)
 character*80 :: hostname
end module mpicontrol

module PPdef
!! module antes de main program ou em outro arquivo
!! module   precicao usada
implicit none
Integer, parameter :: pp = selected_real_kind(8)
end module PPdef


PROGRAM main
!! Ax=y,  A(n,m); x(m), y(n)

! compilar :       mpif90 -O3 -o CGpal CGpal.f90
Use mpicontrol
USE PPdef
implicit none
integer :: n,m,i,j, niter,k,ncols
real (kind=pp), allocatable, dimension(:,:) :: A, Sl
real (kind=pp), allocatable, dimension(:) :: x,v1,xteo, dif,pk,r,qk,r1,q,u1,ynew, difdados
real (kind=pp), allocatable, dimension(:) :: v, p, ppart, g, gpart, b, bpart, y, ypart
real (kind=pp)  ::  eps, errD, errM, alf, beta,  to, vg, vp, errDrel, errMrel
REAL (kind=8)      :: start, ptime, finish, time, Stime
integer ::  jsta, jend
integer :: Npar
 character(len=100) :: arg1, arg2,arg3,arg4

 Call MPI_INIT(mpierror) !inicializar mpi
 call MPI_COMM_SIZE(MPI_comm_world,numprocs,mpierror) ! get a number or procesos = numprocs
 call MPI_COMM_RANK(MPI_comm_world,myrank,mpierror) !get the individual proceses ID = myrank


!!! gerando A

Npar=IARGC()
if (Npar < 4 .or. Npar > 4) then
  if ( myrank == 0 )then
        print *, "numero de argumentos ingresados =", Npar
  	print *, "Erro no uso"
  	write(*,'(a)')" Uso:  "
        write(*,'(a)')"     sh rodaPar.sh <np> CGpal <linhas> <colunas> <Max_Iter> <eps>"
        write(*,'(a)')"     /opt/openmpi/bin/mpirun -v -np <np> -report-bindings --hostfile hostfile CGpal <n> <m> <M_it> <eps>"
  	write(*,'(a)')" linhas   = Integer "
  	write(*,'(a)')" colunas  = Integer"
  	write(*,'(a)')" Max Iter = Integer"
  	write(*,'(a)')" eps      = Real"
        write(*,'(a)')" exemplo 1:  "
        write(*,'(a)')"      sh rodaPar.sh 64 CGpal 5000 5000 10000 1e-10"
        write(*,'(a)')" exemplo 2:  "
        write(*,'(a)')"      /opt/openmpi/bin/mpirun -v -np 3 -report-bindings --hostfile hostfile CGpal 12 15 1000 1e-10"
        write(*,'(a)')" exemplo 3:  "
        write(*,'(a)')"      qsub run.sh"
	write(*,'(a)')""
        write(*,'(a)')"      Onde  run.sh e uma shell "
	write(*,'(a)')""
        write(*,'(a)')" 	#!/bin/bash												"
 	write(*,'(a)')"		#$ -cwd													"
 	write(*,'(a)')"		#$ -j y													"
 	write(*,'(a)')"		#$ -S /bin/bash												"
	write(*,'(a)')"		#$ -pe orte 4												"
 	write(*,'(a)')"		#$ -N job												"
 	write(*,'(a)')"		#$ -q new.q												"
 	write(*,'(a)')"		/usr/bin/time -f ","%E"," /opt/openmpi/bin/mpirun -v -np $NSLOTS ./CGpal 5000 5000 10000 1e-10		"




  end if
  CALL MPI_FINALIZE(mpierror)
  stop
 ! exit

else
    call GETARG(1,arg1)
    call GETARG(2,arg2)
    call GETARG(3,arg3)
    call GETARG(4,arg4)
    read(arg1,*) n
    read(arg2,*) m
    read(arg3,*) niter
    read(arg4,*) eps
    if ( myrank == 0 )then
    	write(*,*) "n       = ", n
    	write(*,*) "m       = ", m
    	write(*,*) "Max Iter= ", niter
    	write(*,*) "eps     = ", eps
    end if
end if


allocate (A(n,m),x(m), xteo(m), y(n), ypart(n),ynew(n))
 CALL para_range1(1, m, myrank,numprocs, jsta, jend)
ncols=jend-jsta+1

if ( myrank == 0 ) then
  ! print *, "Gerando A"
   call random_number(A)  !! entre 0 e 1   
   !!A=A*2-1                !! entre -1 e 1
end if

 call MPI_Bcast(A,n*m,MPI_REAL8,0,MPI_comm_world, mpierror )
 Call print_SmallMat_ordered (n,m,A,'A de Ax=y')
 call MPI_Barrier (MPI_comm_world, mpierror)
!!! u teorico
 xteo=1.0


 call vec_output (m,xteo,"Uteo, modelo teorico")

!!! dados observados y=A x,   x=x teorico;  y(n)=A(n,m) x(m)
!y=matmul(A,xteo)       !!! Serial
    ypart=0.0 ; y=0.0
    !print *, "myrank = ", myrank, "  ppart antes= ", ppart
    do j=jsta,jend
        ypart(1:n)=ypart(1:n)+A(1:n,j)*xteo(j) 
    enddo
    !print *, "myrank = ", myrank, "  gpart depois= ", gpart
    Call MPI_AllReduce(ypart,y,n,MPI_REAL8,MPI_sum,MPI_COMM_WORLD,mpierror)
    call MPI_Barrier (MPI_comm_world, mpierror)
!!!!!!!!!!!!!!


 call vec_output (n,y,"y, dado observado")

!print *, "==================            CG             =====================  "
!! setup
allocate (Sl(m,jsta:jend))
allocate (b(m),bpart(m),v(m),g(m),gpart(m),v1(m),p(m),ppart(m),dif(m),difdados(n))

!!! novo sistema   A^T A x = A^T y
!!!                   S  x = b 

to=1.05   !! 5% de tol de aumento do erro, minimo locaal
start = MPI_Wtime()

!print *, "S = A^T A"
!! S em paralelo  S= A^T A  ! se puede paralelizar
Sl=0.0 !; S=0.0
!Sl=matmul(transpose(A),A(:,jsta:jend))  !em paralelo,falg -O3 aprox mesma eficiença que as 5 linhas seguentes, mas doble de mem allocada
do j = jsta, jend    !!! não precisa allocar memoria para A^T, melhor
    do i = 1, m
        Sl(i,j)=dot_product(A(1:n,i),A(1:n,j))
     end do
end do

 call print_SmallMat_ordered (m,ncols,Sl,"Sl = A^T x A")
 !call MPI_Barrier (MPI_comm_world, mpierror)
!print *, "gero Sl"


!!! calculo de "b = A^T y" em paralelo
!print *, "b = A^T y"
!b=matmul(transpose(A),y)  ; A(n,m), A^T(m,n), y(n), b(m)
    bpart=0.0 ; b=0.0
    !print *, "myrank = ", myrank, "  ppart antes= ", ppart
    !bpart=matmul(A(:,jsta:jend),y(jsta:jend))  
    do j=jsta,jend
        bpart(j)=dot_product(A(1:n,j),y(:)) 
    enddo
    !print *, "myrank = ", myrank, "  gpart depois= ", gpart
    Call MPI_AllReduce(bpart,b,m,MPI_REAL8,MPI_sum,MPI_COMM_WORLD,mpierror)
    call MPI_Barrier (MPI_comm_world, mpierror)
!!!!!!!!!!!!

 call vec_output (m,b,"A'b = transpose(A)*dado observado")

x=0.0  !! u inicial, modelo inicial
 

!!! calculo de "g = S x -b" em paralelo
!g=matmul(S,x)-b;  ! calculo de g, dif entre 
    gpart=0.0 ; g=0.0
    !print *, "myrank = ", myrank, "  ppart antes= ", ppart
    !gpart=matmul(Sl(:,jsta:jend),x(jsta:jend))  !! com flag -O3: mesma efieciencia, 
    do j=jsta,jend
       gpart(1:m)=gpart(1:m)+Sl(1:m,j)*x(j) 
    enddo
    !print *, "myrank = ", myrank, "  gpart depois= ", gpart
    Call MPI_AllReduce(gpart,g,m,MPI_REAL8,MPI_sum,MPI_COMM_WORLD,mpierror)
    g=g-b
    call MPI_Barrier (MPI_comm_world, mpierror)
!!!!

v=g;   !!calculo de v
 !call vec_output (m,v,"V=g inicial")

 
 call MPI_Barrier (MPI_comm_world, mpierror)
 !print *, "em interacoes "
 ptime = MPI_Wtime()
do k=1,niter
    !!! computing  p= Sv, em paralelo
    ppart=0.0 ; p=0.0
    !!!  p em paralelo p=Sl v
    ppart=matmul(Sl(:,jsta:jend),v(jsta:jend))   !! com flag -O3: mesma efieciencia, sem flag: mais eficiente == respeito as 3 linhas seguentes
    !do j=jsta,jend
    !    ppart(1:m)=ppart(1:m)+Sl(1:m,j)*v(j) 
    !enddo
    Call MPI_AllReduce(ppart,p,m,MPI_REAL8,MPI_sum,MPI_COMM_WORLD,mpierror)
    call MPI_Barrier (MPI_comm_world, mpierror)

   vg=dot_product(v,g) ! escarlar
   vp=dot_product(v,p)
   
   alf=-vg/vp   ! computing alf =-v'g/v'Sg
   x=x+alf*v  ! actualizando m, incognita
   g=g+alf*p  ! actualiza erro
   v1=g
   !beta=-(dot_product(v1,p))/(dot_product(v,p))
   beta=-(dot_product(v1,p))/vp
   v=v1+beta*v  !! actualiza v
   
   IF (vg <= eps) then
       !if ( myrank == 0 ) print *,"iter = ",k, " vg = ",vg
       !if ( myrank == 0 ) print '(I3,I5,ES13.4)',numprocs,k,vg
       !print *,"myrank EXIT = ", myrank
       exit
   endif  
   
   if ((k < 200 .and. mod(k,10) == 0) .or. (k < 10000 .and. mod(k,100) == 0) .or. mod(k,1000)==0 )then
       if ( myrank == 0 ) print *,"iter = ",k, "  vg = ",vg
       !if ( myrank == 0 ) print '(I3,I5,ES13.4)',numprocs,k,vg
   endif
   !if ( mod(k,10) == 0 )then
       !if ( myrank == 0 ) print *,"iter = ",k, "  vg = ",vg
   !    if ( myrank == 0 ) print '(I3,I5,ES13.4)',numprocs,k,vg
   !endif

enddo

call MPI_Barrier (MPI_comm_world, mpierror)
finish = MPI_Wtime()
if ( myrank == 0 ) then
   dif=xteo-x
   errM=dot_product(dif,dif)
   errMrel=errM/dot_product(xteo,xteo)

   ynew=matmul(A,x)
   difdados=y-ynew
   errD=dot_product(difdados,difdados)
   errDrel=errD/dot_product(y,y)

   finish = MPI_Wtime()
   stime=ptime-start
   time=finish-ptime

   print *, ""
   print *, "Informe        CG"
   print *, ""
   print *, "eps alvo  'vg <= eps'   = ",eps
   print *, "vg final                =",vg
   print *, "N iter max              = ",niter
   print *, "k iteracoes necesarias  = ",k
   print *, "Time  Seput       = ", stime
   print *, "Time  Iter        = ", time
   print *, "Time/iter         = ",time/k
   
   call vec_output (m,xteo,"u teorico, modelo")
   call vec_output (m,x,"u final")
   print *, "Energia erro do medelos      = ", errM
   print *, "Energia erro do medelos Rel  = ", errMrel

   call vec_output (n,y,"dados, com uteo")
   call vec_output (n,ynew,"dados com modelo calculado")

   print *, "Energia erro dos dados      = ", errD
   print *, "Energia erro dos dados rel  = ", errDrel
   print *, ""
   print *, ""
   print *, ""

   !print '(2I6,I3,I6,9ES13.4)',n,m,numprocs,k,stime,time,stime+time,time/k,vg,errM,errMrel,errD,errDrel
endif


deallocate (A,Sl,b,x,y,xteo,ynew)

 CALL MPI_FINALIZE(mpierror)

end PROGRAM main


!!==========================================================
subroutine para_range1(n1,n2,myrank,numprocs,ista,iend)
 IMPLICIT NONE
 integer :: n1,n2,myrank,numprocs,ista,iend
 integer :: iwork1, iwork2

 iwork1=(n2-n1+1)/numprocs
 iwork2=mod(n2-n1+1,numprocs)
 ista=myrank*iwork1+n1+min(myrank,iwork2)
 iend=ista+iwork1-1
 if (iwork2 > myrank) iend = iend+1
end subroutine para_range1
!!==========================================================

!!==========================================================
subroutine mat_output (n,l,C,nameMat)
!! cristian ariza
USE PPdef
  !implicit none
  integer*4 :: n,l,j
  real (kind=pp), dimension(n,l) ::c
  character(len=*) :: nameMat
  if (n<21 .and. l<21)then  !! print matriz ordered
  print *, ""
  print '(" matrix : " , a ," = ")',  trim(nameMat)
  do j = 1, n
      write (*,'(20f8.3)') c(j,1:l)
  end do
  print *, "\\\"
  endif 
end subroutine mat_output
!!==========================================================

subroutine vec_output (l,C,namevec)
!! cristian ariza
USE PPdef
  implicit none
  integer*4 :: n,l,j
  real (kind=pp), dimension(l) ::c
  character(len=*) :: namevec
  if ( l<21)then  !! print 
  print '(" vector: " , a ," = ")',  trim(namevec)

      write (*,'(20ES10.2)') c(1:l)

  print *, ""
  endif 
end subroutine vec_output
!!==========================================================


!!==========================================================

subroutine print_SmallMat_ordered (l,c,A,nameMat)
  use mpicontrol
  USE PPdef
  implicit none
  integer*4 :: c,l,j
  character(len=*) :: nameMat
  real (kind=pp), dimension(l,c) :: A
  !call sleep(1)
  call MPI_Barrier (MPI_comm_world, mpierror)
  if (l<21 .and. c<21)then  !! print matriz ordered
     call sleep(myrank)
     print '("myrank = " , I3 ," matrix : " , a ," = ")', myrank, trim(nameMat)
     call mat_output (l,c,A,trim(nameMat))
  endif
  call MPI_Barrier (MPI_comm_world, mpierror)
  ! call sleep(1)
end subroutine print_SmallMat_ordered
