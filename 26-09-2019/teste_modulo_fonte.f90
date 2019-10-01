PROGRAM teste_modulo_fonte

	
	USE modulo_fonte

! PROGRAMA PARA CRIAR A FONTE USANDO A WAVELET RICKER
	IMPLICIT NONE
		REAL, ALLOCATABLE :: trs(:)
		INTEGER :: ns
		REAL :: dt, fpeak, tdelay

		ns=400
		ALLOCATE(trs(ns))

		fpeak=25
		dt=0.004
			
		
		OPEN(10,FILE='source.bin',STATUS='UNKNOWN',ACCESS='DIRECT',FORM='UNFORMATTED',RECL=4*ns)
			CALL source_init(trs,dt,ns,fpeak,tdelay)
			WRITE(10,REC=1), trs
		CLOSE (UNIT=10)

		DEALLOCATE (trs)
		
END PROGRAM teste_modulo_fonte
