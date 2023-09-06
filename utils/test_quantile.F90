program test_quantile
  use scaler, only: read_quantile_data
  implicit none 
    
    integer :: j
    integer, parameter :: nrows=500
    real(8), dimension(nrows) :: ext, scat, ssa, asy, qua, ppf  
    character(len=256), dimension(:), allocatable :: args
    
    allocate(args(1))
    
    call get_command_argument(1, args(1))
    call read_quantile_data(args(1), nrows, ext, scat, ssa, asy, qua, ppf)   
    
    print*, "	  Num 		ext 			scat 			ssa 				asy 			qua 			ppf"  
    do j=1,nrows
        write(*, *) j, ext(j), scat(j), ssa(j), asy(j), qua(j), ppf(j)
    end do
end program test_quantile
