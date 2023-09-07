module mod_transformer

implicit none

private

public :: read_min_max_data, read_quantile_data, interp_linear, inverse_quantile_transform

contains

subroutine read_min_max_data(filename, nrows, vname, max_vals, min_vals)
    implicit none    
    integer :: i, j
    
    character(256), intent(in) :: filename
    integer, intent(in) :: nrows
    character(20), dimension(nrows), intent(out) :: vname
    real(8), dimension(nrows), intent(out) :: min_vals, max_vals
    character(256) :: line    
    
    open(1, file=filename, status='old', action='read')
    read(1, *) 
    
    j = 1
    do
        read(1, '(A)', iostat=i) line
        ! Check if the end of file is reached
         if (i /= 0) then
            exit
         end if
         
        read(line, *) vname(j), max_vals(j), min_vals(j)        
        j = j+1
    end do
    close(1)
end subroutine read_min_max_data

subroutine read_quantile_data(filename, nrows, ext, sca, asy, qua, ppf)
    implicit none    
    
    character(256), intent(in) :: filename
    integer, intent(in) :: nrows
    real(8), dimension(nrows), intent(out) :: ext, sca, asy, qua, ppf
    integer :: i, j, num
    
    open(1, file=filename, status='old', action='read')
    read(1, *) 
    
    do j=1, nrows
        read(1, '(A)', iostat=i) num, ext(j), sca(j), asy(j), qua(j), ppf(j)         
    end do
    close(1)
end subroutine read_quantile_data

subroutine interp_linear(x, ppf, references, interp)
    real(8), intent(in) :: x
    real(8), intent(out) :: interp
    real(8), dimension(:), intent(in) :: ppf, references
    integer :: i

    do i = 2, size(ppf)
      if (x < ppf(i)) then
        interp = references(i-1) + (references(i) - references(i-1)) * &
                             (x - ppf(i-1)) / (ppf(i) - ppf(i-1))
        exit
      end if
    end do
end subroutine interp_linear

subroutine inverse_quantile_transform(prediction, ext, sca, asy, ppf)
    real(8), dimension(:), intent(out) :: ext, sca, asy, ppf
    real(8), dimension(:,:), intent(inout) :: prediction
    integer :: i
    
    do i = 1, size(prediction, dim=1)
        call interp_linear(prediction(i, 1), ppf, ext, prediction(i, 1))
        call interp_linear(prediction(i, 2), ppf, sca, prediction(i, 2))
        call interp_linear(prediction(i, 3), ppf, asy, prediction(i, 3))
    end do
end subroutine inverse_quantile_transform

end module mod_transformer