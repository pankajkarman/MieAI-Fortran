module QuantileTransformer
    implicit none

    ! Declare module variables
    real(8), dimension(:), allocatable :: quantiles, references

contains

    ! Function to perform inverse transformation
    real(8) function inverse_transform(value)
        real(8), intent(in) :: value
        integer :: i

        do i = 2, size(quantiles)
            if (value < quantiles(i)) then
                inverse_transform = references(i-1) + (references(i) - references(i-1)) * &
                                   (value - quantiles(i-1)) / (quantiles(i) - quantiles(i-1))
                exit
            end if
        end do
    end function inverse_transform

    ! Procedure to load saved parameters from file
    subroutine load_parameters(filename)
        character(*), intent(in) :: filename
        integer :: status, i
        open(unit=10, file=filename, status='old', action='read', iostat=status)

        if (status == 0) then
            allocate(quantiles(100))
            i = 1
            do
                read(10, *, iostat=status) quantiles(i)
                if (status /= 0) exit
                i = i + 1
            end do
            deallocate(quantiles)

            allocate(references(100))
            i = 1
            do
                read(10, *, iostat=status) references(i)
                if (status /= 0) exit
                i = i + 1
            end do
            deallocate(references)
        else
            print *, 'Error opening file:', filename
        end if

        close(10)
    end subroutine load_parameters

end module QuantileTransformer