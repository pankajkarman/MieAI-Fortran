program MieAI
	use mod_kinds, only: ik, rk
	use mod_network, only: network_type

	implicit none

	type(network_type) :: net
	
	integer :: num = 20000, i 
	integer, allocatable :: exp_num(:)

	real, allocatable :: mie_input(:)
	real(rk), allocatable :: prediction(:)
	character(len=100) :: filename
	character(len=100), dimension(:), allocatable :: args		

	!print*, exp_num(i), coating(i), x(i), n_core(i), k_core(i), n_shell(i), k_shell(i), lambda(i)
	
	print*, "		Extinction 	  SSA 		  Asym"

	allocate(exp_num(num), mie_input(7))	
	allocate(args(2))

	!num_args = command_argument_count()

	call get_command_argument(1,args(1))
	call get_command_argument(2,args(2))

	call net % load(args(1))
	open(10, file=args(2))
	read(10,*)

	do i = 1, num
		read(10,*) exp_num(i), mie_input
		prediction = net % output(mie_input)
		print*, exp_num(i), prediction
	end do
end program
