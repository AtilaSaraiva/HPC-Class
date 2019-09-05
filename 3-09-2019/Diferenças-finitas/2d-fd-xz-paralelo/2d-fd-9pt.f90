	PROGRAM main
	IMPLICIT REAL*8 (a-h,o-z) 
	PARAMETER (m = 100, n = 100) 
	DIMENSION a(m,n), b(m,n) 

	DO j = 1, n
	DO i = 1, m
	a(i,j) = i + 10.0 * j
	ENDDO 
	ENDDO

	DO j = 2, n - 1 
	  DO i = 2, m - 1
        	b(i,j) = a(i-1,j ) +  a(i, j-1) + a(i, j+1) + a(i+1,j ) + & 
		a(i-1,j-1) + a(i+1,j-1) + a(i-1,j+1) + a(i+1,j+1)
		ENDDO 
	ENDDO
        
        	open (unit=9, file='result-9pt.dat', ACCESS='direct', recl=4*n*m, &
                      status='UNKNOWN')
        write(9,rec=1)real(b,kind=4)

	STOP
	END
