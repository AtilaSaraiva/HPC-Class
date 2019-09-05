! Troca colunas de elementos entre matrizes.

PROGRAM matriz_serial


		IMPLICIT NONE
		!INTEGER(kind=4) :: myid ! numero associado a cada processador, comecando do zero
		!INTEGER(kind=4) :: nprocs ! numero de processadores que serao utilizados
		!INTEGER(kind=4) :: ierr ! erro para ver se tá certo
		INTEGER :: n, m
		REAL, DIMENSION(:,:), ALLOCATABLE :: a, b
		INTEGER :: i, j

!
			n=100
			m=100


		ALLOCATE(a(m,n),b(m,n))
		a=0
		b=0


		DO i=1,m
			DO j=1,n
			a(i,j)= (i + j)*1.
			END DO
		END DO




		DO j = 3, n-2
			DO i = 3, m-2
				b(i,j) = a(i-1,j) + a(i+1,j) + a(i,j-1) + a(i,j+1) + a(i-2,j) + a(i+2,j) + a(i,j+2) + a(i,j-2)
			END DO
		END DO

			open (unit=8, file='a_serial.dat', ACCESS='direct', recl=4*n*m, status='UNKNOWN')
			write(8,rec=1)real(a,kind=4)

			open (unit=8, file='result_serial.dat', ACCESS='direct', recl=4*n*m, status='UNKNOWN')
			write(8,rec=1)real(b,kind=4)




		DEALLOCATE (a,b)

STOP
END PROGRAM matriz_serial



