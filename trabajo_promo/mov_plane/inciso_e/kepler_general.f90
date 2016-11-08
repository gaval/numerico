! ============================================================================================= !
!   En este programa vamos a comprobar las tres leyes de Kepler segun se recomienda en la  guia !
! ============================================================================================= !
program kepler
  use runge_kutta4
  implicit none
  integer, parameter :: pr = kind(1.d0)
  integer, parameter :: dimen = 4                                ! Dimension del sistema de ODE's
  integer :: i,npasos
  real(pr), parameter :: pi = real(3.14159265358979,pr)
  real(pr), parameter :: gg = 2.9600_pr*real(1E-4,pr)            ! gg = G = constante de gravitacion universal
  real(pr), dimension(1:dimen) :: x , x_s                        ! x = x(t) , x_s = x(t+h) i.e. solucion
  real(pr) :: d_1,d_2,d_f                                        ! d_1 = distancia foco 1 a p_1, d_2 = foco 2 a p_1
  real(pr) :: ley_1,ley_2,ley_3                                  ! ley_1 = ley 1
  real(pr) :: afelio,perihelio
  real(pr) :: l,periodo                                            ! l = momento angular
  real(pr) :: t_0 , m_1, m_2 , t , r , r_0 , h
  !
  ! Defino el tiempo inicial,periodo y las masas
  t_0 = 0._pr ; m_1 = 3.0016_pr*real(1E-6,pr) ; m_2 = 1._pr
  periodo = 365.256_pr ; h = real(1E-1,pr)
  npasos = int(periodo/h)
  !
  ! Defino las condiciones iniciales. Cabe aclarar que x(1) = x , x(2) = dx/dt = vx , x(3) = y , x(4) = dy/dt = vy
  x(1) = 0.98329134_pr ; x(2) = 0._pr
  x(3) = 0._pr ; x(4) = 0.01749578_pr
  !
  ! Abro el archivo de salida, le escribo el nombre de cada columna, alguna anotacion, etc.
  open (3,file='1ley_kepler.dat',action='write',status='replace')
  open (5,file='2ley_kepler.dat',action='write',status='replace')
  open (7,file='3ley_kepler.dat',action='write',status='replace')
  write(3,'(A12,12X,A11)') '#          t','Primera Ley'
  write(5,'(A12,12X,A15)') '#          t','Momento angular'
  write(5,'(A12,12X,A11)') '#          t','Tercera Ley'
  !
  ! Mientras probaba los h optimos observe que el siguiente h es considerablemente bueno para hacer los calculos
  ! puesto que es bastante preciso (Delta E = 1.3629116426583334E-009) y el tiempo que tarda en hacer el computo es bueno

  !
  ! Inicializo el tiempo y el contador
  t = t_0 + h ; r_0 = x(1)  ! r_0 = vector posicion r(t=0)
  !
  ! Primera ley: defina el afelio y perihelio que estan en la tabla
  afelio = 1.01671388_pr ; perihelio = 0.98329134_pr
  ! a continuacion se definen los focos
  d_f = perihelio - afelio
  do i = 1,npasos
      call rk4 (dimen,x,t,h,f,x_s)
      !
      ! Distancias para 1ra ley y calculos + salida
      d_1 = sqrt(x_s(1)**2 + x_s(3)**2)
      d_2 = sqrt((x_s(1)+d_f)**2 + x_s(3)**2)
      ley_1 = d_f + d_1 + d_2
      write(3,25) t,ley_1
      !
      ! Segunda ley: Momento angular L = r x p
      l = m_1*(x_s(1)*x_s(4) - x_s(3)*x_s(2))
      write(5,25) t,l
      !
      x(:) = x_s(:)
      t = t + h
  end do
  !
  ! Tercera ley: defina el periodo del planeta
  ley_3 = (4._pr*(pi**2)*(perihelio**3))/(gg*m_2)
  write(7,25) (periodo**2),ley_3
  close(3) ; close(5) ; close(7)
  25 format (2(2X,E16.7))
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
end program kepler
