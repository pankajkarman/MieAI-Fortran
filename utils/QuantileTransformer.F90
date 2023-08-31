module QuantileTransformer
  implicit none

  ! Declare module variables
  real(8), dimension(:), allocatable :: quantiles, references
  integer :: num_quantiles

contains

  ! Function to perform inverse transformation
  real(8) function inverse_transform(value)
    real(8), intent(in) :: value
    integer :: i

    do i = 2, num_quantiles
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
      read(10, *) num_quantiles
      allocate(quantiles(num_quantiles))
      allocate(references(num_quantiles))

      do i = 1, num_quantiles
        read(10, *) quantiles(i)
      end do

      do i = 1, num_quantiles
        read(10, *) references(i)
      end do

      close(10)
    else
      print *, 'Error opening file:', filename
    end if
  end subroutine load_parameters

  ! Procedure to save parameters to file
  subroutine save_parameters(filename)
    character(*), intent(in) :: filename
    integer :: i

    open(unit=20, file=filename, status='replace', action='write')

    write(20, *) num_quantiles
    do i = 1, num_quantiles
      write(20, *) quantiles(i)
    end do

    do i = 1, num_quantiles
      write(20, *) references(i)
    end do

    close(20)
  end subroutine save_parameters

end module QuantileTransformer