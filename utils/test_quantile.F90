program test_quantile
  use scaler
  implicit none 
    
    integer :: j, k
    integer, parameter :: nrows=500, n=10
    real(8), dimension(n) :: examples, inv_ext, inv_sca, inv_ssa, inv_asy
    real(8), dimension(nrows) :: ext, sca, ssa, asy, qua, ppf 
    character(len=256), dimension(:), allocatable :: args
    
    allocate(args(1))
    
    call get_command_argument(1, args(1))
    call read_quantile_data(args(1), nrows, ext, sca, ssa, asy, qua, ppf)  
    
    print*, "	  Num 		ext 			scat 			ssa 				asy 			qua 			ppf"  
    
    do k=1,nrows
        write(*, *) k, ext(k), sca(k), ssa(k), asy(k), qua(k), ppf(k)
    end do
    
    print *, ""
    print *, ""
    
    examples = [-3.0, -2.0, -1.0, 0.0, 1.0, 2.0, 3.0, 0.33, 0.057, -0.01]
    
    print*, "	  Num 		ext 			sca 			ssa 				asy"  
    
    do j=1, n
        call inverse_transform(examples(j), ppf, ext, 500, inv_ext(j))    
        call inverse_transform(examples(j), ppf, sca, 500, inv_sca(j))    
        call inverse_transform(examples(j), ppf, ssa, 500, inv_ssa(j))    
        call inverse_transform(examples(j), ppf, asy, 500, inv_asy(j))    
        write(*, *) j, inv_ext(j), inv_sca(j), inv_ssa(j), inv_asy(j)
    end do
end program test_quantile
