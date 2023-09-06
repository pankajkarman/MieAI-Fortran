program test_scaler
  use scaler, only: read_min_max_data
  implicit none 
    
    integer :: i
    integer, parameter :: nrows=13
    real(8), dimension(nrows) :: min_vals, max_vals
    character(20), dimension(nrows) :: vname
    character(len=256), dimension(:), allocatable :: args
    
    allocate(args(1))
    
    call get_command_argument(1, args(1))
    call read_min_max_data(args(1), nrows, vname, max_vals, min_vals)   
    
    print*, "Variable 		  Minimum 		  Maximum"
    do i=1,13
        write(*, *) vname(i), min_vals(i), max_vals(i)
    end do

end program test_scaler
