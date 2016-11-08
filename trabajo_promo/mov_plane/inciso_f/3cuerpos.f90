! ============================================================================================= !
!   En este programa se resuelven las ecuaciones de movimiento para 3 cuerpos.                  !
!   Tengo que resolver 8 ODE                                                                    !
! ============================================================================================= !
program trescuerpos
  use runge_kutta4
  implicit none
  integer, parameter :: pr = kind(1.d0)
  integer, parameter :: dimen = 8                ! Dimension del sistema de ODE's
  integer :: i,npasos,j
  real(pr), parameter :: gg = 2.9600_pr*real(1E-4,pr)      ! gg = G = constante de gravitacion universal
  real(pr), dimension(1:dimen) :: x , x_s            ! x = x(t) , x_s = x(t+h) i.e. solucion
  real(pr) :: t_0,m_1,m_2,m_3,periodo,t,r,h,kntc,ptntl,mchnci,mchncf    ! t_0 = tiempo inicial , m_1 & m_2 = masas, t = tiempo , h = paso
  !                                                       ! kntc = energia cinetica , ptntl = energia potencial
  !                                                       ! mchnci & mchncf = energ. inicial & final , r(t) = |r(t)|
  !
  ! Defino el tiempo inicial, el periodo de tabla y las masas
  t_0 = 0._pr ; m_1 = 3.0016_pr*real(1E-6,pr) ; m_2 = 1._pr
  m_3 = 317.9_pr*(3.0016_pr*real(1E-6,pr))
  periodo = 365.256_pr
  !
  ! Defino el paso h y la cantidad de iteraciones del programa
  h = real(1E-1,pr) ; npasos = int(periodo/h)
  ! Defino las condiciones iniciales. Cabe aclarar que x(1) = x , x(2) = dx/dt = vx , x(3) = y , x(4) = dy/dt = vy
  ! para la Tierra y de 5 a 8 identicamente pero para Jupiter
  ! Cond. Inic. Tierra.
  x(1) = 0.98329134_pr ; x(2) = 0._pr
  x(3) = 0._pr ; x(4) = 0.01749578_pr
  ! Cond. Inic. Jupiter
  x(5) = 4.950249_pr ; x(6) = 0._pr
  x(7) = 0._pr ; x(8) = 0.007924_pr
  ! energia mecanica Tierra
  mchnci = 0.5_pr*m_1*(x(2)**2+x(4)**2) - gg*m_1*m_2/(sqrt(x(1)**2+x(3)**2)) + gg*m_1*m_3/(sqrt((x(1)+x(5))**2+(x(3)+x(7))**2))
  !
  ! ===========================!
  ! Para la masa de Jupiter
  ! ===========================!
  open (3,file='3cT_mJ.dat',action='write',status='replace')
  write(3,35) '#          t','x(t)','vx(t)','y(t)','vy(t)','T','U','E'
  35 format(A12,15X,A4,14X,A5,13X,A4,14X,A5,3(16X,A1))
  ! Inicializo el tiempo
  t = t_0 + h
  do i = 1,npasos
      !
      call rk4 (dimen,x,t,h,f,x_s)
      !
      kntc = 0.5_pr*m_1*(x_s(2)**2 + x_s(4)**2)
      ptntl = -gg*m_1*m_2/(sqrt(x(1)**2+x(3)**2)) + gg*m_1*m_3/(sqrt((x(1)+x(5))**2+(x(3)+x(7))**2))
      mchncf = ptntl + ptntl
      !
      r = sqrt(x_s(1)**2 + x_s(3)**2)
      write(3,25) t,x_s(1),x_s(2),x_s(3),x_s(4),kntc,ptntl,mchncf
      x(:) = x_s(:)
      t = t + h
  end do
  25 format (8(2X,E16.7))
  close(3)
  !
  ! ==============================!
  ! 10 veces la masa de Jupiter
  ! ==============================!
  npasos = int(2._pr*periodo/h)
  m_3 = 10._pr*317.9_pr*(3.0016_pr*real(1E-6,pr))
  ! Cond. Inic. Tierra.
  x(1) = 0.98329134_pr ; x(2) = 0._pr
  x(3) = 0._pr ; x(4) = 0.01749578_pr
  ! Cond. Inic. Jupiter
  x(5) = 4.950249_pr ; x(6) = 0._pr
  x(7) = 0._pr ; x(8) = 0.007924_pr
  mchnci = 0.5_pr*m_1*(x(2)**2 + x(4)**2) + (gg*m_1*m_2)/(sqrt(x(1)**2 + x(3)**2))
  !
  open (5,file='3cT_10mJ.dat',action='write',status='replace')
  write(5,45) '#          t','x(t)','vx(t)','y(t)','vy(t)','T','U','E'
  45 format(A12,15X,A4,14X,A5,13X,A4,14X,A5,3(16X,A1))
  ! Inicializo el tiempo
  t = t_0 + h
  do i = 1,npasos
      !
      call rk4 (dimen,x,t,h,f,x_s)
      !
      kntc = 0.5_pr*m_1*(x_s(2)**2 + x_s(4)**2)
      ptntl = -gg*m_1*m_2/(sqrt(x(1)**2+x(3)**2)) + gg*m_1*m_3/(sqrt((x(1)+x(5))**2+(x(3)+x(7))**2))
      mchncf = ptntl + ptntl
      !
      r = sqrt(x_s(1)**2 + x_s(3)**2)
      write(5,55) t,x_s(1),x_s(2),x_s(3),x_s(4),kntc,ptntl,mchncf
      x(:) = x_s(:)
      t = t + h
  end do
  55 format (8(2X,E16.7))
  close(5)
  !
  !
  ! ==============================!
  ! 100 veces la masa de Jupiter
  ! ==============================!
  npasos = int(4._pr*periodo/h)
  m_3 = 100._pr*317.9_pr*(3.0016_pr*real(1E-6,pr))
  ! Cond. Inic. Tierra.
  x(1) = 0.98329134_pr ; x(2) = 0._pr
  x(3) = 0._pr ; x(4) = 0.01749578_pr
  ! Cond. Inic. Jupiter
  x(5) = 4.950249_pr ; x(6) = 0._pr
  x(7) = 0._pr ; x(8) = 0.007924_pr
  mchnci = 0.5_pr*m_1*(x(2)**2 + x(4)**2) + (gg*m_1*m_2)/(sqrt(x(1)**2 + x(3)**2))
  !
  open (7,file='3cT_100mJ.dat',action='write',status='replace')
  write(7,65) '#          t','x(t)','vx(t)','y(t)','vy(t)','T','U','E'
  65 format(A12,15X,A4,14X,A5,13X,A4,14X,A5,3(16X,A1))
  ! Inicializo el tiempo
  t = t_0 + h
  do i = 1,npasos
      !
      call rk4 (dimen,x,t,h,f,x_s)
      !
      kntc = 0.5_pr*m_1*(x_s(2)**2 + x_s(4)**2)
      ptntl = -gg*m_1*m_2/(sqrt(x(1)**2+x(3)**2)) + gg*m_1*m_3/(sqrt((x(1)+x(5))**2+(x(3)+x(7))**2))
      mchncf = ptntl + ptntl
      !
      r = sqrt(x_s(1)**2 + x_s(3)**2)
      write(7,75) t,x_s(1),x_s(2),x_s(3),x_s(4),kntc,ptntl,mchncf
      x(:) = x_s(:)
      t = t + h
  end do
  75 format (8(2X,E16.7))
  close(7)
  !
  ! ==============================!
  ! 1000 veces la masa de Jupiter
  ! ==============================!
  npasos = int(100._pr*periodo/h)
  m_3 = 10._pr*317.9_pr*(3.0016_pr*real(1E-6,pr))
  ! Cond. Inic. Tierra.
  x(1) = 0.98329134_pr ; x(2) = 0._pr
  x(3) = 0._pr ; x(4) = 0.01749578_pr
  ! Cond. Inic. Jupiter
  x(5) = 4.950249_pr ; x(6) = 0._pr
  x(7) = 0._pr ; x(8) = 0.007924_pr
  mchnci = 0.5_pr*m_1*(x(2)**2 + x(4)**2) + (gg*m_1*m_2)/(sqrt(x(1)**2 + x(3)**2))
  !
  open (9,file='3cT_1000mJ.dat',action='write',status='replace')
  write(9,85) '#          t','x(t)','vx(t)','y(t)','vy(t)','T','U','E'
  85 format(A12,15X,A4,14X,A5,13X,A4,14X,A5,3(16X,A1))
  ! Inicializo el tiempo
  t = t_0 + h
  j = 0
  do i = 1,npasos
      j = j + 1
      !
      call rk4 (dimen,x,t,h,f,x_s)
      !
      kntc = 0.5_pr*m_1*(x_s(2)**2 + x_s(4)**2)
      ptntl = gg*m_1*m_2/(sqrt(x(1)**2+x(3)**2)) + gg*m_1*m_3/(sqrt((x(1)+x(5))**2+(x(3)+x(7))**2))
      mchncf = ptntl + ptntl
      !
      r = sqrt(x_s(1)**2 + x_s(3)**2)
      if (mod(j,100) == 0) write(9,95) t,x_s(1),x_s(2),x_s(3),x_s(4),kntc,ptntl,mchncf
      x(:) = x_s(:)
      t = t + h
  end do
  95 format (8(2X,E16.7))
  close(9)
  !
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
    fun(2) = -(gg*m_2*xx(1))/((xx(1)**2+xx(3)**2)**1.5_pr) - (gg*m_3*(xx(1)+xx(5)))/(((xx(1)+xx(5))**2+(xx(3)+xx(7))**2)**1.5_pr)
    fun(3) = xx(4)
    fun(4) = -(gg*m_2*xx(3))/((xx(1)**2+xx(3)**2)**1.5_pr) - (gg*m_3*(xx(3)+xx(7)))/(((xx(1)+xx(5))**2+(xx(3)+xx(7))**2)**1.5_pr)
    fun(5) = xx(6)
    fun(6) = -(gg*m_2*xx(5))/((xx(5)**2+xx(7)**2)**1.5_pr) + (gg*m_1*(xx(1)+xx(5)))/(((xx(1)+xx(5))**2+(xx(3)+xx(7))**2)**1.5_pr)
    fun(7) = xx(8)
    fun(8) = -(gg*m_2*xx(7))/((xx(5)**2+xx(7)**2)**1.5_pr) + (gg*m_1*(xx(3)+xx(7)))/(((xx(1)+xx(5))**2+(xx(3)+xx(7))**2)**1.5_pr)
    !
  end subroutine f
end program trescuerpos
