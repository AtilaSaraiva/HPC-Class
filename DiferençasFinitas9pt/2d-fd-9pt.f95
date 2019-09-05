	PROGRAM main
	IMPLICIT REAL*4 (a-h,o-z) 
	PARAMETER (m = 100, n = 100) 
	DIMENSION a(m,n), b(m,n) 
	
	DO j = 1, n
		DO i = 1, m
		a(i,j) = i + 10.0 * j
		ENDDO 
	ENDDO

	b =0.0 
    w0=4.0
    w1=.75
    w2=.25

	DO j = 3, n - 2 
		DO i = 3, m - 2
		b(i,j) = w1*a(i-1,j) + w1*a(i,j-1) + w1*a(i,j+1) + w1*a(i+1,j) + & 
                 w0*a(i,j) + w2*a(i+2,j) + w2*a(i-2,j) + & 
                 w2*a(i,j-2) + w2*a(i,j+2) 
	ENDDO
	ENDDO 

	open (unit=9, file='result-9pt.dat', ACCESS='direct', recl=4*n*m, &
                      status='UNKNOWN')
        write(9,rec=1)real(b,kind=4)

	Stop
	END
