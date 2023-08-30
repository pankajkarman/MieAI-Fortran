! test_keras.f90

! TO RUN
! ./test_keras $NF_PATH/ExampleModels/simple_model.txt

! this file is used in $NF_PATH/KerasWeightsProcessing/examples/test_network.py
! load a specified network from cmd line arg
! pass simple input through it
! print result

program test_keras
  use mod_kinds, only: ik, rk
  use mod_network, only: network_type

  implicit none

  type(network_type) :: net

  real(rk), allocatable :: result1(:), input(:)
  character(len=100), dimension(:), allocatable :: args

  allocate(args(1))
  call get_command_argument(1,args(1))

  ! load trained network from keras
  call net % load(args(1))

  ! out_col = ["Extinction", "SSA", "Asym"]
  ! in_col = ['coating', 'x', 'n_core', 'k_core', 'n_shell', 'k_shell', 'lambda']

  input = [0.225, 0.005454, 0.364270, 0.682194, 0.539978, 2.721096e-01, 0.143143]

  ! run test input through network
  result1 = net % output(input)
  ! result = [0.595836, 0.418503, 0.878600]
  print *, result1

end program test_keras
