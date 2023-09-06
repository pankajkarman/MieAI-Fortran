module scaler

implicit none

private

public :: read_min_max_data, read_quantile_data

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
    read(1,*) 
    
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

subroutine read_quantile_data(filename, nrows, ext, scat, ssa, asy, qua, ppf)
    implicit none    
    integer :: i, j, num
    
    character(256), intent(in) :: filename
    integer, intent(in) :: nrows
    real(8), dimension(nrows), intent(out) :: ext, scat, ssa, asy, qua, ppf
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
         
        read(line, *) num, ext(j), scat(j), ssa(j), asy(j), qua(j), ppf(j)        
        j = j+1
    end do
    close(1)
end subroutine read_quantile_data
end module scaler