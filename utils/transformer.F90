module transformer

implicit none

private

public :: apply_minmax_scaling, InverseQuantileTransformer, read_json

contains

subroutine apply_minmax_scaling(data, n_samples, n_features, scaled_data)
    implicit none

    real(8), intent(in) :: data(:,:)
    integer, intent(in) :: n_samples, n_features
    real(8), intent(out) :: scaled_data(n_samples, n_features)
    
    real(8) :: min_vals(n_features), max_vals(n_features), range
    integer :: i, j

    ! Read minmax_scaler_params.csv to min_vals and max_vals

    open(1, file='minmax_scaler_params.csv', status='old', action='read')
    do j = 1, n_features
        read(1, *) min_vals(j), max_vals(j)
    end do
    close(1)

    ! Apply scaling
    do j = 1, n_features
        range = max_vals(j) - min_vals(j)
        do i = 1, n_samples
            scaled_data(i,j) = (data(i,j) - min_vals(j)) / range
        end do
    end do
end subroutine apply_minmax_scaling

subroutine read_json(json_file, n_quantiles, quantiles)
    implicit none
    character(*), intent(in) :: json_file
    integer, intent(out) :: n_quantiles
    real(real64), dimension(:), allocatable, intent(out) :: quantiles
    character(256) :: line
    integer :: i

    ! Open the JSON file for reading
    inquire(file=json_file, exist=i)
    if (i /= 1) then
        write(*, *) "JSON file not found:", json_file
        stop
    end if

    open(10, file=json_file, status='old')

    ! Read the number of quantiles
    read(10, '(a)', iostat=i) line
    if (i /= 0) then
        write(*, *) "Error reading JSON file:", json_file
        stop
    end if
    read(line, *) n_quantiles

    ! Allocate memory for quantiles array
    allocate(quantiles(n_quantiles))

    ! Read quantiles
    do i = 1, n_quantiles
        read(10, '(a)', iostat=i) line
        if (i /= 0) then
            write(*, *) "Error reading JSON file:", json_file
            stop
        end if
        read(line, *) quantiles(i)
    end do

    ! Close the file
    close(10)
    
end subroutine read_json

subroutine InverseQuantileTransformer(X_transformed, X_inverse, quantiles, n_samples, n_features)
    implicit none
    ! Input parameters
    real(kind=8), intent(in) :: X_transformed(:,:)
    real(kind=8), intent(in) :: quantiles(:)
    integer, intent(in) :: n_samples, n_features
    
    ! Output parameter
    real(kind=8), intent(out) :: X_inverse(:,:)
    
    ! Local variables
    integer :: i, j
    real(kind=8) :: x_min, x_max
    
    ! Calculate inverse transformed values using quantiles
    do j = 1, n_features
        x_min = quantiles(j)
        x_max = quantiles(n_features + j)
        do i = 1, n_samples
            X_inverse(i, j) = X_transformed(i, j) * (x_max - x_min) + x_min
        end do
    end do
end subroutine InverseQuantileTransformer
end module transformer