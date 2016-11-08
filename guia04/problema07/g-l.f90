! ------------------------------------------------- !
!      Este programa utiliza el algoritmo de        !
!       cuadratura de Gauss-Legendre para           !
!      2 y 3 nodos (puntos). Los nodos y pesos      !
!      ya los calcule en la hoja.                   !
! ------------------------------------------------- !
program gauss_legendre
  implicit none
  integer, parameter :: pr = kind(1.d0)
  real(pr) :: a,b                         ! intervalo de integracion
  real(pr) :: w0,w1,w2                    ! pesos para G-L de 3 puntos
  real(pr) :: u0,u1                       ! pesos para G-L de 2 puntos
  real(pr) :: t0,t1,t2                    ! nodos mapeados para G-L 3ptos
  real(pr) :: r0,r1                       ! nodos mapeados para G-L 2ptos
  real(pr) :: gl3,gl2                     ! valores de la integral gl3 = 3ptos,  gl2 = 2ptos
  real(pr) :: p0,p1,p2,q0,q1              ! los uso para escribir los puntos donde debo evaluar f
  !
  ! Defino el intervalo de integracion
  a = real(1,pr)
  b = real(1.5,pr)
  !
  ! pesos y nodos para G-L de 3 puntos
  w0 = real(5,pr)/real(9,pr)
  w1 = real(8,pr)/real(9,pr)
  w2 = real(5,pr)/real(9,pr)
  !
  t0 = t(real(-sqrt(real(3,pr)/real(5,pr)),pr),a,b)
  t1 = t(real(0,pr),a,b)
  t2 = t(real(sqrt(real(3,pr)/real(5,pr)),pr),a,b)
  !
  ! Pesos y nodos para G-L de 2 puntos
  u0 = real(1,pr)
  u1 = real(1,pr)
  !
  r0 = t(real(-sqrt(real(1,pr)/real(3,pr)),pr),a,b)
  r1 = t(real(sqrt(real(1,pr)/real(3,pr)),pr),a,b)
  !
  ! Calculo integral de 3 puntos 
  gl3 = real(0.5,pr)*(b-a)*(w0*f(t0) + w1*f(t1) + w2*f(t2))
  write(*,25) 'El valor de f(x) = exp(-x*x) con Gauss-Legendre de 3 puntos es', gl3
  25 format (A62,X,F12.10)
  !
  ! Calculo integral de 2 puntos
  gl2 = real(0.5,pr)*(b-a)*(u0*f(r0) + u1*f(r1))
  write(*,35) 'El valor de f(x) = exp(-x*x) con Gauss-Legendre de 2 puntos es', gl2
  35 format (A62,X,F12.10)
  !
  !
contains
  function f(x)
    implicit none
    real(pr), intent(in) :: x
    real(pr) :: f
    !
    f = exp(-x*x)
    !
  end function
  !
  function t(y,a,b)
    implicit none
    real(pr), intent(in) :: y,a,b
    real(pr) :: t
    !
    t = real(0.5,pr)*((b-a)*y + (b+a))
    !
  end function
end program gauss_legendre
