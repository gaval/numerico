! ========================================================================= !
! Este codigo resuelve ODE's con condiciones de borde usando RK4 y el       !
! metodo de la secante como está en el libro Kincaid Six Edition.           !
! ========================================================================= !
program doscuerpos_shooting
  use runge_kutta4
  implicit none
  integer, parameter :: pr = kind(1.d0)
  integer, parameter :: dimen = 4                                                  ! Dimension del sistema de ODE's
  integer :: i,j,npasos
  real(pr), parameter :: pi = real(3.14159265358979,pr) , tol = real(1E-10,pr)
  real(pr), parameter :: gg = 2.9600_pr*real(1E-4,pr)                              ! gg = G = constante de gravitacion universal
  real(pr), dimension(1:dimen) :: x, x_s           ! x = condic. inic. x_s = solucion
  real(pr) :: m_1,m_2,kntc,ptntl,mchncf
  real(pr) :: a,b,t,h,t0,beta                     ! beta = x_s(b)
  real(pr) :: phi1,phi2,phi3                      ! phi(z) = x_s|z (b) - beta
  real(pr) :: z1,z2,z3                            ! los z / x'(a) = z1,z2,z3
  real(pr) :: x01,x02,x03,x04     ! x01 = x(t0),x02 = vx(t0),x03 = y(t0),x04 = vy(t0)
  real(pr) :: theta,th1,th2,r,periodo,perihelio         ! angulo y radio
  !
  m_1 = 0.056_pr*(3.0016_pr*real(1E-6,pr)) ; m_2 = 1._pr
  periodo = 87.969_pr
  t0 = 0._pr ; h = real(1E-1,pr) ; npasos = int(periodo/h)
  beta = 2._pr*pi ; perihelio = 0.307499_pr
  z1 = sqrt((gg*m_1)/perihelio) ; z2 = 0.015998_pr
  !
  x01 = perihelio ; x02 = 0._pr
  x03 = 0._pr
  !
  ! Calculo phi(z1) que es resolver el problema de valores iniciales con x'(a) = z1 y phi(z1) = x_s(b) - beta
  x(1) = x01 ; x(2) = x02
  x(3) = x03 ; x(4) = z1
  t = t0 + h ; theta = 0._pr ; th1 = 0._pr ; th2 = 0._pr
  do i = 1,npasos
      call rk4 (dimen,x,t,h,f,x_s)
      r = sqrt(x_s(1)**2 + x_s(3)**2)
      if (x_s(3) > 0) th1 = acos(x_s(1)/r)
      if (x_s(3) < 0) th2 = 2._pr*pi - acos(x_s(1)/r)
      theta = th1 + th2
      x(:) = x_s(:)
      t = t + h
  end do
  phi1 = theta
  !
  write(*,*) 'Aguarde por favor'
  ! Ahora calculo phi(z2)
  x(1) = x01 ; x(2) = x02
  x(3) = x03 ; x(4) = z2
  t = t0 + h ; theta = 0._pr ; th1 = 0._pr ; th2 = 0._pr
  do i = 1,npasos
      call rk4 (dimen,x,t,h,f,x_s)
      r = sqrt(x_s(1)**2 + x_s(3)**2)
      if (x_s(3) > 0) th1 = acos(x_s(1)/r)
      if (x_s(3) < 0) th2 = 2._pr*pi - acos(x_s(1)/r)
      theta = th1 + th2
      x(:) = x_s(:)
      t = t + h
  end do
  phi2 = theta
  ! write(*,*) phi1,phi2
  ! stop
  !
  ! Ahora me hago un do while que vaya cambiando z3 en funcion de los anteriores y evaluando el resultado hasta que x_s(b) - beta >= tol
  write(*,*) 'Buscando la velocidad inicial optima'
  phi3 = 0._pr ; j = 0
  do while (abs(phi3-beta) > tol)
      j = j + 1
      z3 = z2 + (beta - phi2)*((z2 - z1)/(phi2 - phi1))
      x(1) = x01 ; x(2) = x02
      x(3) = x03 ; x(4) = z3
      t = t0 + h ; theta = 0._pr ; th1 = 0._pr ; th2 = 0._pr
      do i = 1,npasos
          call rk4 (dimen,x,t,h,f,x_s)
          r = sqrt(x_s(1)**2 + x_s(3)**2)
          if (x_s(3) > 0) th1 = acos(x_s(1)/r)
          if (x_s(3) < 0) th2 = 2._pr*pi - acos(x_s(1)/r)
          theta = th1 + th2
          x(:) = x_s(:)
          t = t + h
      end do
      phi3 = theta
      z1 = z2
      z2 = z3
      phi1 = phi2
      phi2 = phi3
  end do
  write(*,*) 'La velocidad inicial es',z3
  write(*,*) 'Se realizaron',j,'pasos.'
  write(*,*) 'A continuacion el programa resolvera el sistema con dicha velocidad inicial'
  write(*,*) '......'
  !
  ! Ahora con el z3 calculo la solucion que satisfaga mi condicion de borde
  open (3,file='shooting_sol.dat',action='write',status='replace')
  write(3,35) '#          t','x(t)','vx(t)','y(t)','vy(t)','T','U','E'
  35 format(A12,15X,A4,14X,A5,13X,A4,14X,A5,3(16X,A1))
  x(1) = x01 ; x(2) = x02
  x(3) = x03 ; x(4) = z3
  t = t0 + h ; theta = 0._pr ; th1 = 0._pr ; th2 = 0._pr
  do i = 1,npasos
      call rk4 (dimen,x,t,h,f,x_s)
      r = sqrt(x_s(1)**2 + x_s(3)**2)
      if (x_s(3) > 0) th1 = acos(x_s(1)/r)
      if (x_s(3) < 0) th2 = 2._pr*pi - acos(x_s(1)/r)
      kntc = 0.5_pr*m_1*(x_s(2)**2 + x_s(4)**2)
      ptntl = (gg*m_1*m_2)/(sqrt(x_s(1)**2 + x_s(3)**2))
      mchncf = ptntl + ptntl
      write(3,25) t,x_s(1),x_s(2),x_s(3),x_s(4),kntc,ptntl,mchncf
      theta = th1 + th2
      x(:) = x_s(:)
      t = t + h
  end do
  25 format (8(2X,E16.7))
  close(3)
  write(*,*) '|xf-xi| =',abs(x_s(1)-x01),'|yf-yi| =',abs(x_s(3)-x03)
  write(*,*) '|vxf-vxi| =',abs(x_s(2)-x02)
  write(*,*) 'El sistema fue resuelto, los datos estan en el archivo shooting_sol.dat'
  !
  !
contains
    !
    ! Subrutina en la que se definen las funciones F del sistema de ODE's acoplado
  subroutine f(tt,xx,fun)	! Cuidado en esta subrutina porque heredo variables, a saber, m_2, gg, dimen
    implicit none
    real(pr), intent(in) :: tt
    real(pr), intent(in), dimension(1:dimen) :: xx
    real(pr), intent(out), dimension(1:dimen) :: fun
    !
    fun(1) = xx(2)
    fun(2) = -(gg*m_2*xx(1))/((xx(1)**2 + xx(3)**2)*sqrt(xx(1)**2 + xx(3)**2))
    fun(3) = xx(4)
    fun(4) = -(gg*m_2*xx(3))/((xx(1)**2 + xx(3)**2)*sqrt(xx(1)**2 + xx(3)**2))
    !
  end subroutine f
end program doscuerpos_shooting
