program test_quantile
  use QuantileTransformer
  implicit none
  
  ! Declare program variables
  real(8), dimension(3) :: transformed_data, inverted_data
  character(255) :: quantile_json
  integer :: i

  ! Load saved parameters
  quantile_json = 'quantile_transformer_parameters.json'
  call load_parameters(quantile_json)

  ! Load transformed data
  transformed_data = [0.1d0, 0.5d0, 0.9d0]

  ! Perform inverse transformation
  do i = 1, size(transformed_data)
    inverted_data(i) = inverse_transform(transformed_data(i))
  end do

  ! Print results
  print *, 'Transformed Data:', transformed_data
  print *, 'Inverted Data:', inverted_data

end program test_quantile
