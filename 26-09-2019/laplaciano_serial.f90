PROGRAM laplaciano_serial

			
		IMPLICIT NONE
		INTEGER :: n, m
		REAL :: dx, dz
		REAL*8, DIMENSION(:,:), ALLOCATABLE :: a, b, c, sx, sz
		INTEGER :: i, j

! Initializing MPI
				
			n=100
			m=100

		ALLOCATE(a(m,n),b(m,n),c(m,n),sx(m,n),sz(m,n))
		a=0.
		b=0.
		c=0.
		sx=0.
		sz=0.
		dx=1.
		dz=1.

	
		DO i=1,m
			DO j=1,n
			a(i,j)= (i + j)*1.
			END DO
		END DO 
	


		DO j = 2,n-1
			DO i = 2, m-1
				sx(i,j) = a(i-1,j) + a(i+1,j) + 2*a(i,j)
				sz(i,j) = a(i,j-1) + a(i,j+1) + 2*a(i,j)
				c(i,j) = sx(i,j)/(dx**2) + sz(i,j)/(dz**2)
			END DO
		END DO


			open (unit=10, file='laplaciano_serial.dat', ACCESS='direct', recl=4*n*m, status='UNKNOWN')
			write(10,rec=1)real(c,kind=4)
		
		DEALLOCATE (a,b,c,sx,sz)
	
STOP
END PROGRAM laplaciano_serial


