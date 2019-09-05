    Program main
    
    IMPLICIT None 
    
    integer :: i, n
    
    real*4,  DIMENSION (:), allocatable :: a, b

    print*, 'forneça n '
    read*, n 

    allocate (a(n),b(n))
    
    DO i = 1, n
    b(i) = real(i) 
    ENDDO

    a=0.0
    DO i = 2, n-1
    a(i) = b(i-1) + b(i+1)
    ENDDO 

    do i=1, n 
    print*, 'i=', i, 'valor de a=', a(i) 
    enddo 


    deallocate (a,b) 
    END
