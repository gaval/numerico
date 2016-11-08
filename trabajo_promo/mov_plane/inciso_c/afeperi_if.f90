! ============================================================================================= !
!   En este programa se resuelven las ecuaciones de movimiento. Se calculan el afelio y         !
!   perihelio para el inciso (c).                                                               !
! ============================================================================================= !
program afelio_perihelio
  use runge_kutta4
  implicit none
  integer, parameter :: pr = kind(1.d0)
  integer, parameter :: dimen = 4                                                  ! Dimension del sistema de ODE's
  integer :: i,npasos
  real(pr), parameter :: gg = 2.9600_pr*real(1E-4,pr)                              ! gg = G = constante de gravitacion universal
  real(pr), dimension(1:366) :: rr     ! modificar la dimension si se modifica la condicion de escritura sobre 7
  real(pr), dimension(1:dimen) :: x , x_s                                          ! x = x(t) , x_s = x(t+h) i.e. solucion
  real(pr) :: t_0,m_1,m_2,t,r_p,r_a,r,afelio,perihelio,h              ! t_0 = tiempo inicial , m_1 & m_2 = masas, t = tiempo , h = paso
  real(pr) :: periodo
  !
  ! Defino el tiempo inicial, periodo y las masas
  t_0 = 0._pr ; m_1 = 3.0016_pr*real(1E-6,pr) ; m_2 = 1._pr
  periodo = 365.256_pr
  !
  ! Defino las condiciones iniciales. Cabe aclarar que x(1) = x , x(2) = dx/dt = vx , x(3) = y , x(4) = dy/dt = vy
  x(1) = 0.98329134_pr ; x(2) = 0._pr
  x(3) = 0._pr ; x(4) = 0.01749578_pr
  !
  open (5,file='afelio.dat',action='write',status='replace')
  write(5,'(A9,2(12X,A1))') '#       r','x','y'
  open (7,file='perihelio.dat',action='write',status='replace')
  write(7,'(A9,2(12X,A1))') '#       r','x','y'
  !
  h = real(1E-1,pr) ; npasos = int(periodo/h)
  !
  ! Inicializo el tiempo y el contador
  t = t_0 + h ; r_p = 0.98329134_pr ; r_a = 0._pr   ! inicializo el afelio en r=0 y el perihelio en el dato inicial

  !
  do i = 1,npasos
      call rk4 (dimen,x,t,h,f,x_s)
      !
      r = sqrt(x_s(1)**2 + x_s(3)**2)
      if (r_a < r) r_a = r
      if (r_p > r) r_p = r
      x(:) = x_s(:)
      t = t + h
  end do
  write(5,'(3(2X,F11.8))') r_a,x_s(1),x_s(3)
  write(7,'(3(2X,F11.8))') r_p,x_s(1),x_s(3)
  close(7) ; close(5)
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
end program afelio_perihelio
