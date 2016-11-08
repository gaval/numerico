program prob6
  use runge_kutta
  implicit none
  integer, parameter  :: pr = kind(1.d0)
  integer :: nstep,ndim
  real(pr), allocatable, dimension(:) :: x
!  real(pr), dimension(1) :: x
  real(pr) :: a,b,t0
  !
  t0 = 0._pr ; a = 0._pr ; b = 1._pr
  ndim = 1
  nstep = 10
  !
  allocate (x(1:ndim))
  !
  call RK4 (a,b,nstep,ndim,t0,f,x)
  !
  deallocate (x)
  !
contains
  subroutine f(tt,xx,fi)
    implicit none
    integer :: i
    real(pr), intent(in) :: tt
    real(pr), intent(in) :: xx
    real(pr), intent(out) :: fi
    real(pr) :: pi
    !
    pi = real(3.14159265358979,pr)
    !
    fi = -xx + sin(2._pr*pi*tt)
  end subroutine f
end program prob6