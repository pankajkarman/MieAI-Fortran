module scaler

implicit none

private

public :: read_min_max_data, apply_minmax_scaling

contains

subroutine read_min_max_data(filename, nrows, vname, min_vals, max_vals)
    implicit none    
    integer :: i, j
    
    character(256), intent(in) :: filename
    integer, intent(in) :: nrows
    character(20), dimension(nrows), intent(out) :: vname
    real(8), dimension(nrows), intent(out) :: min_vals, max_vals
    character(256) :: line    
    
    open(1, file=filename, status='old', action='read')
    read(1,*) 
    
    j = 1
    do
        read(1, '(A)', iostat=i) line
        ! Check if the end of file is reached
         if (i /= 0) then
            exit
         end if
         
        read(line, *) vname(j), min_vals(j), max_vals(j)        
        j = j+1
    end do
    close(1)
end subroutine read_min_max_data

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

end module scaler