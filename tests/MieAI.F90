program MieAI
    use mod_kinds, only: ik, rk
    use mod_network, only: network_type
    use mod_transformer

    implicit none

    type(network_type) :: net
    
    integer, parameter :: nrows=13, n=7, nrows1=1000
    integer :: i, j, jj, k, idx, indices1(n), indices2(3), num=20000
    integer, allocatable :: exp_num(:)
    
    real, allocatable :: mie_input(:), mie_input1(:), input(:,:)
    real(8), allocatable :: prediction(:, :) 
    real(8), dimension(nrows1) :: ext, sca, asy, qua, ppf
    real(8), dimension(nrows) :: min_vals, max_vals

    character(len=256), dimension(:), allocatable :: args    
    character(20), dimension(nrows) :: vname   
    character(len=256) :: min_max_file, quantile_transform_params, MieAI_file
    namelist /input_parameters/ min_max_file, quantile_transform_params, MieAI_file
    
    allocate(exp_num(num), mie_input(7), mie_input1(7), input(num, 7), prediction(num, 3))
    allocate(args(1))
    
    open(unit=20, file="/home/b/b382177/python/MieF/MieAI.nml", status="old")
    read(20, nml=input_parameters)
    close(20)
    
    ! Print the namelist values
    print *, "min_max_file =", min_max_file
    print *, "quantile_transform_params_file =", quantile_transform_params
    print *, "model_file =", MieAI_file
    
    ! load saved MieAI model
    call net % load(MieAI_file)
    
    ! load saved min-max values for training data
    call read_min_max_data(min_max_file, nrows, vname, max_vals, min_vals) 
    
    ! load parameters of quantile transformer
    call read_quantile_data(quantile_transform_params, nrows1, ext, sca, asy, qua, ppf)  
    
    indices1 = [5, 13, 7, 8, 9, 10, 12]
    !indices2 = [1, 3, 4]
    indices2 = [1, 2, 4]
    
    ! read input data for optical properties calculation using MieAI    
    !print*, "    exp_num     coating, 	     x 		   n_core 	     k_core 	    n_shell 		k_shell 	lambda"
    !num_args = command_argument_count()
    call get_command_argument(1,args(1))
    open(10, file=args(1))
    read(10, *)
    
    ! transform input data using min-max scaling and predict optical properties using MieAI
    do i = 1, num
        read(10, *) exp_num(i), mie_input        
        do j = 1, n
            idx = indices1(j)
            mie_input1(j) = (mie_input(j) - min_vals(idx)) / (max_vals(idx) - min_vals(idx)) 
        end do
        input(i, :) = mie_input
        prediction(i, :) = net % output(mie_input1)  
     end do 
    close(10)
        
    ! Inverse transform MieAI prediction using inverse quantile tranform    
    call inverse_quantile_transform(prediction, ext, sca, asy, ppf)
    
    ! denromalize optocal properties using training min-max values
    do k = 1, 3
        idx = indices2(k)
        prediction(:, k) = prediction(:, k) * (max_vals(idx) - min_vals(idx)) + min_vals(idx)
    end do        

    ! print MieAI prediction after post-processing
    !print*, "	Extinction  		  SCA 		 	  Asym"
    
    !print*, "	coating, 	x 		n_core 		k_core 		n_shell 	k_shell 	lambda			Extinction  		  SCA 		 	  Asym "
    
    print*, "	coating, 	x 		lambda		Extinction 			  SCA 		 	  Asym "
    
    do i = 1, num
        print*, input(i, [1,2,7]), prediction(i, :)
    end do    
end program
