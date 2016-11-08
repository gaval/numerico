program prob5
  use diferenciacion, only: forw_backw_centr
  implicit none
  integer, parameter :: pr = kind(1.d0)
  integer :: k
  real(pr) :: x,h,deri,err,deriex
  real(pr) :: bd,fd       ! las defino pero no las uso
  !
  x = real(2,pr)
  deriex = real(3,pr)*exp(real(2,pr))
  !
  open(3,file='forw.dat',action='write',status='replace')
  do k = 1, 15
    h = real(10,pr)**(-k)
    ! h = real(0.02,pr)
    call forw_backw_centr (x,h,f,fd,bd,deri)
    ! deri = (f(x+real(0.5,pr)*h) - f(x-real(0.5,pr)*h))/(h)
    err = abs(deriex - deri)/abs(deriex)
    ! write(*,*) deri,h,x,f(x+real(0.5,pr)*h),f(x-real(0.5,pr)*h)
    write(3,25) h,err
  end do
  25 format (E12.6,2X,E12.6)
  close(3)
  !
contains
  function f(y)
    implicit none
    real(pr), intent(in) :: y
    real(pr) :: f
    !
    f = x*exp(x)
    !
  end function f
end program prob5
