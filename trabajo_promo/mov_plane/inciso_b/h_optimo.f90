! ============================================================================================= !
!   Con el objetivo de determinar el paso h optimo utilizando la conservacion de la energia     !
!   se calcula la energia en t=0 y se la compara con la energia en t=T (T periodo)              !
!   correspondiente a la Tierra. Con tal motivo solo se desea un archivo de salida que          !
!   contenga el paso h y la diferencia Delta E = abs(E(T) - E(0)). Se toma el valor absoluto    !
!   para luego poder graficar en escala log-log.                                                !
! ============================================================================================= !
program hoptimo
  use runge_kutta4
  implicit none
  integer, parameter :: pr = kind(1.d0)
  integer, parameter :: dimen = 4                                            ! Dimension del sistema de ODE's
  integer :: i,j,npasos
  real(pr), parameter :: gg = 2.9600_pr*real(1E-4,pr)                        ! gg = G = constante de gravitacion universal
  real(pr), dimension(1:dimen) :: x , x_s                                    ! x = x(t) , x_s = x(t+h) i.e. solucion
  real(pr) :: t_0,m_1,m_2,t,h,kntc,ptntl,mchnci,mchncf        ! t_0 = tiempo inicial , m_1 & m_2 = masas, t = tiempo , h = paso
  real(pr) :: delta_mecanica,periodo                                      ! kntc = energia cinetica , ptntl = energia potencial , mchnci & mchncf = energ. inicial & final
  !                                                                          ! delta_mecanica = Delta E como se explica arriba
  ! Defino el tiempo inicial, periodo y las masas
  t_0 = 0._pr ; m_1 = 3.0016_pr*real(1E-6,pr) ; m_2 = 1._pr
  periodo = 365.256_pr
  ! Defino las condiciones iniciales. Cabe aclarar que x(1) = x , x(2) = dx/dt = vx , x(3) = y , x(4) = dy/dt = vy
  x(1) = 0.98329134_pr ; x(2) = 0._pr
  x(3) = 0._pr ; x(4) = 0.01749578_pr
  !
  mchnci = 0.5_pr*m_1*(x(2)**2 + x(4)**2) - (gg*m_1*m_2)/(sqrt(x(1)**2 + x(3)**2))
  !
  open (5,file='enervsh.dat',action='write',status='replace')
  write(5,'(A65)') '# Delta E es el cambio en la energia mecanica luego de un periodo'
  write(5,25) '#          h','Delta E'
  25 format (A12,13X,A7)
  !
  j = 2 ; h = 0._pr
  do while ( j > -5)
    !
    h = 10._pr**j
    npasos = int(periodo/h)
    x(1) = 0.98329134_pr ; x(2) = 0._pr
    x(3) = 0._pr ; x(4) = 0.01749578_pr
    t = t_0 + h
    do i = 1,npasos
      !
      call rk4 (dimen,x,t,h,f,x_s)
      !
      kntc = 0.5_pr*m_1*(x_s(2)**2 + x_s(4)**2)
      ptntl = -(gg*m_1*m_2)/(sqrt(x_s(1)**2 + x_s(3)**2))
      mchncf = kntc + ptntl
      x(:) = x_s(:)
      t = t + h
    end do
    j = j - 1
    delta_mecanica = abs(mchncf-mchnci)
    write(5,45) h,delta_mecanica
  end do
  45 format (2(2X,E16.7))
  close(5)
  !
  !
contains
    !
    ! Subrutina en la que se definen las funciones F del sistema de ODE's acoplado
  subroutine f(tt,xx,fun)		! Cuidado en esta subrutina porque heredo algunas variables, a saber, m_2, gg, dimen
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
end program hoptimo
