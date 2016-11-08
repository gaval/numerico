program prob7
  use runge_kutta4
  implicit none
  integer, parameter :: pr = kind(1.d0) , ndim = 2
  integer :: nstep,i
  real(pr), parameter :: g = 10._pr,l = 1._pr
  real(pr), dimension(1:ndim) :: x , x_sol
  real(pr) :: a,b,h,m,t0,t,cinetica,potencial,mecanica
  !
  t0 = 0._pr ; a = 0._pr ; b = 10._pr ; m = 1._pr
  h = 0.5_pr
  x(1) = 0.5_pr ; x(2) = 0._pr
  nstep = (b-a)/h
  !
  open(3,file='pendulo.dat',action='write',status='replace')
  write(3,35) '#          t','theta(t)','u(t)','T','U','E'
  35 format(A12,12X,A8,14X,A4,3(16X,A1))
  t = t0 + h
  do i = 1,nstep
    call rk4 (ndim,x,t,h,f,x_sol)
    cinetica = 0.5_pr*m*l*x_sol(2)**2
    potencial = m*g*l*cos(x_sol(1))
    mecanica = cinetica + potencial
    x(:) = x_sol(:)
    write(3,25) t,x_sol(1),x_sol(2),cinetica,potencial,mecanica
    t = t + h
  end do
  25 format (6(2X,E16.7))
  !
  !
contains
  subroutine f(tt,xx,fun)
    implicit none
    real(pr), intent(in) :: tt
    real(pr), intent(in), dimension(1:ndim) :: xx			! x1 = theta, x2=u
    real(pr), intent(out), dimension(1:ndim) :: fun
    ! real(pr), parameter :: g = 10._pr,l = 1._pr
    !
    fun(1) = xx(2)				! x1'=x2
    fun(2) = -(g/l)*sin(xx(1))		! x2'=-(g/l)sin(x1)
    !
  end subroutine f
end program

! Me falta hacer la parte con la energia, la duda que tengo es como definir la energia en mi modulo de RK4
! porque T = 0.5*m*u^2 y U = m*g*l*cos(theta). En la notacion de x1,x2 seria T = 0.5*m*x2 y U = m*g*l*cos(x1)
! entonces E = T + U. Lo que no se es como hacer que en mi modulo pueda usar x(1) y x(2) en cada paso porque primero
! calculo para k=1 [x(1)] y luego para k=2 [x(2)], entonces los primeros nstep solo me da x(1) y x(2) vale cero.
! Quizas me convenga poner de otra forma los do?
!
! Para la parte c no entiendo bien el enunciado. Primero grafico theta(t) - exacta donde theta(t) es lo que calculo numericamente?
! Y le superpongo el ploteo de t vs sol.numerica y t vs sol.exacta?
