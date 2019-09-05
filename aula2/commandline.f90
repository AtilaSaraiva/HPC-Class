	Program commandline
	implicit none 
	real :: num 
	integer :: option2, N 
	character(len=100) :: arg1,arg2,option1


       ! Example to read a character and an integer from the command line 
       !For example try, 
       ! >> ./a.exe inputfilename 21 


       ! Read input for the command line
       N = iargc()

       print *, 'Number of arguments', N 

       if ( N < 1 ) then
          write(*,*) "usage:  ./foo option1 option2 "
          write(*,*) "option1 = character "
          write(*,*) "option2 = integer "
       else

         call getarg(1, option1)    ! grab the first command line argument
                                    ! and store it in the variable
                                    ! 'option '

         call getarg(2, arg1 )   ! grab the 2nd command line argument
                                ! and store it in the temporary variable
                                ! 'arg1'

         call getarg(3, arg2 )   ! grab the 3th command line argument
                                ! and store it in the temporary variable
                                ! 'arg2'


         read (arg1, *) option2    ! now convert string to integer
         read (arg2, *) num 

         write(*,*) "Variable option1= ", trim(adjustl(option1))
         write(*,*) "Variable option2 - inteiro = ",  option2
         write(*,*) "Variable num - real= ",  num
      
     endif 

     end program commandline 
