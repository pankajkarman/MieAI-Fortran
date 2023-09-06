program MieAI
	use mod_kinds, only: ik, rk
	use mod_network, only: network_type
	use mod_scaler, only: read_min_max_data

	implicit none

	type(network_type) :: net
    
	integer, parameter :: nrows=13, n=7
	integer :: num = 10000, i, j, idx, indices1(n), indices2(3)  
	integer, allocatable :: exp_num(:)
    
	real, allocatable :: mie_input(:), mie_input1(:)
	real(rk), allocatable :: prediction(:), prediction1(:)    
	real(8), dimension(nrows) :: min_vals, max_vals

	character(len=256), dimension(:), allocatable :: args		    
	character(20), dimension(nrows) :: vname   
    
	allocate(exp_num(num), mie_input(7), mie_input1(7), prediction(3), prediction1(3))	
	allocate(args(3))

	!num_args = command_argument_count()

	call get_command_argument(1,args(1))
	call get_command_argument(2,args(2))
	call get_command_argument(3,args(3))
    
	call read_min_max_data(args(3), nrows, vname, max_vals, min_vals) 
    
	indices1 = [5, 13, 7, 8, 9, 10, 12]
	indices2 = [1, 3, 4]

	!print*, "    exp_num     coating, 	     x 		   n_core 	     k_core 	    n_shell 		k_shell 	lambda"  
	print*, "		Extinction 	  SSA 		  Asym"

	call net % load(args(1))
	open(10, file=args(2))
	read(10,*)

	do i = 1, num
		read(10,*) exp_num(i), mie_input
        
		do j = 1, n
			idx = indices1(j)
			mie_input1(j) = (mie_input(j) - min_vals(idx)) / (max_vals(idx) - min_vals(idx))
		end do
        
		prediction1 = net % output(mie_input1)
		do j = 1, 3
			idx = indices2(j)
			prediction(j) = prediction1(j) * (max_vals(idx) - min_vals(idx)) + min_vals(idx)
		end do
        
		!print*, exp_num(i),  mie_input1  
		!print*, exp_num(i), max_vals(1), min_vals(1), max_vals(2), min_vals(2), max_vals(3), min_vals(3)
		print*, exp_num(i), prediction
	end do
	close(10)
end program
