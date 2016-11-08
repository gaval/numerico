! ============================================================================================= !
!   En este programa se resuelven las ecuaciones de movimiento. El archivo de salida posee lo   !
!   solicitado en el inciso (a) del trabajo. Ademas se calculan el afelio y perihelio           !
!   para el inciso (c).                                                                         !
! ============================================================================================= !
program doscuerpos
  use runge_kutta4
  implicit none
  integer, parameter :: pr = kind(1.d0)
  integer, parameter :: dimen = 4                                                  ! Dimension del sistema de ODE's
  integer :: i,npasos
  real(pr), parameter :: gg = 2.9600_pr*real(1E-4,pr)      ! gg = G = constante de gravitacion universal
  real(pr), dimension(1:dimen) :: x , x_s            ! x = x(t) , x_s = x(t+h) i.e. solucion
  real(pr) :: t_0,m_1,m_2,periodo,t,r,h,kntc,ptntl,mchnci,mchncf    ! t_0 = tiempo inicial , m_1 & m_2 = masas, t = tiempo , h = paso
  !                                                       ! kntc = energia cinetica , ptntl = energia potencial
  !                                                       ! mchnci & mchncf = energ. inicial & final , r(t) = |r(t)|
  !
  ! Defino el tiempo inicial, el periodo de tabla y las masas
  t_0 = 0._pr ; m_1 = 3.0016_pr*real(1E-6,pr) ; m_2 = 1._pr
  periodo = 365.256_pr
  !
  ! Defino el paso h y la cantidad de iteraciones del programa
  h = real(1E-1,pr) ; npasos = int(periodo/h)
  ! Defino las condiciones iniciales. Cabe aclarar que x(1) = x , x(2) = dx/dt = vx , x(3) = y , x(4) = dy/dt = vy
  x(1) = 0.98329134_pr ; x(2) = 0._pr
  x(3) = 0._pr ; x(4) = 0.01749578_pr
  mchnci = 0.5_pr*m_1*(x(2)**2 + x(4)**2) + (gg*m_1*m_2)/(sqrt(x(1)**2 + x(3)**2))
  !
  open (3,file='2c_solu.dat',action='write',status='replace')
  write(3,35) '#          t','x(t)','vx(t)','y(t)','vy(t)','T','U','E'
  35 format(A12,15X,A4,14X,A5,13X,A4,14X,A5,3(16X,A1))
  !
  open (5,file='pot_vs_r.dat',action='write',status='replace')
  write(5,'(A12,15X,A1)') '#          r','U'
  !
  ! Inicializo el tiempo
  t = t_0 + h
  !
  ! Comienzo el proceso iterativo que llama a la subrutina rk4 encargada de calcular, para cada paso, la solucion del sistema de ODE's acopladas
  do i = 1,npasos
      !
      call rk4 (dimen,x,t,h,f,x_s)
      !
      kntc = 0.5_pr*m_1*(x_s(2)**2 + x_s(4)**2)
      ptntl = (gg*m_1*m_2)/(sqrt(x_s(1)**2 + x_s(3)**2))
      mchncf = ptntl + ptntl
      !
      r = sqrt(x_s(1)**2 + x_s(3)**2)
      write(3,25) t,x_s(1),x_s(2),x_s(3),x_s(4),kntc,ptntl,mchncf
      write(5,'(2(2X,E16.7))') r,ptntl
      x(:) = x_s(:)
      t = t + h
  end do
  25 format (8(2X,E16.7))
  close(3) ; close(5)
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
end program doscuerpos
