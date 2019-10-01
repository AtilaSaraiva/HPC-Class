MODULE modulo_fonte


	CONTAINS
	SUBROUTINE source_init(srt,dt,nt,fpeak,tdelay)

	IMPLICIT NONE
		REAL, INTENT(INOUT), DIMENSION(nt) :: srt
		INTEGER, INTENT(IN) :: nt
		REAL, INTENT(IN) :: dt, fpeak
		REAL, INTENT(OUT) :: tdelay
		INTEGER :: i
		REAL :: t, pi=3.14159
		REAL :: wpeak, waux, tt

		wpeak= 2*pi*fpeak
		waux = 0.5*wpeak

		tdelay=6./(5.*fpeak)
	
		DO i=1,nt
			t=(i-1)*dt
			tt= t-tdelay
			! fórmula analítica da Ricker
			srt(i)=exp(-waux*waux*tt*tt/4.)*cos(wpeak*tt)
		
		END DO
	END SUBROUTINE source_init

END MODULE modulo_fonte
	

