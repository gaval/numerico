!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! A este lo intente hacer con biseccion!!!!
!! y no anduvo, no se que onda. !!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program prob8
  use runge_kutta4
  implicit none
  integer, parameter :: pr = kind(1.d0) , ndim=2        ! por que la dimension es 2 si solo tengo x'' = f nada mas, o sea tengo una sola dimension
  integer :: npasos,i
  real(pr), parameter :: tol = real(1E-3,pr)
  real(pr), dimension(1:ndim) :: x , x_sol
  real(pr) :: a,b,t,h,t0,z,z1,z2,x11
  real(pr) :: u,v,w
  !
  a = 1._pr ; b = 2._pr ; h = 0.05 ; npasos = int((b-a)/h)
  t0 = a ; x11 = log(256._pr)
  z1 = -0.1_pr ; z2 = 1.4_pr
  !
  x(1) = 0._pr
  !
  t = t0 + h
  x(2) = z1
  do i = 1,npasos
      call rk4 (ndim,x,t,h,f,x_sol)
      x(:) = x_sol(:)
      t = t + h
  end do
  u = x_sol(1) - x11
  !
  x(1) = 0._pr ; x(2) = z2 ; t = t0 + h
  do i = 1,npasos
      call rk4 (ndim,x,t,h,f,x_sol)
      x(:) = x_sol(:)
      t = t + h
  end do
  v = x_sol(1) - x11
  write(*,*) u,v
  !
  if (sign(real(1,pr),u) == sign(real(1,pr),v)) then
  write(*,*) 'ERROR: la funcion en los extremos del intervalo tiene el mismo signo!'
  stop
  end if
  !
  x(2) = 0.5_pr*(z1 + z2) !; z = 1.5_pr*tol
  do while (abs(z2-z1) > tol)
    t = t0 + h ; x(1) = 0._pr
    do i = 1,npasos
        call rk4 (ndim,x,t,h,f,x_sol)
        x(:) = x_sol(:)
        t = t + h
    end do
    w = x_sol(1) - x11
!   Chequeo si c es raiz o sino achico el invervalo
    if (w == 0._pr) then
      write(*,*) 'La raiz esta en',x(2)
      stop
    elseif (u*w < 0) then
      z2 = x(2)
      v = w
    elseif (w*v<0) then
      z1 = x(2)
      u = w
    end if
    x(2) = 0.5_pr*(z1 + z2)
  end do
  !
contains
  subroutine f(tt,xx,fun)
    implicit none
    real(pr), intent(in), dimension(1:ndim) :: xx
    real(pr), intent(in) :: tt
    real(pr), intent(out), dimension(1:ndim) :: fun
    !
    ! fun(:) = -(xx(2)**2) !+ log(tt)			! esta funcion me hizo renegar mucho
    fun(1) = xx(2)
    fun(2) = (1._pr/tt**5)*((tt*xx(2))**2 - 9._pr*xx(1)**2 + 4._pr*tt**6)
    !
  end subroutine f
end program prob8
