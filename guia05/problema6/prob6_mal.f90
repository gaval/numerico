! =========================================== !
!    Calculo de integral de exp(-t) entre     !
!    0 y 1 usando Monte Carlo.                !
! =========================================== !
program prob6
  implicit none
  integer, parameter :: pr = kind(1.d0)
  integer :: i,j,rn
  real(pr) :: integ,xi !rn
  !
  ! do i = 100,30000,100
    ! integ = real(0,pr)
    ! rn = real(3,pr)
    ! do j = 1, 100
    !   xi = rn1(rn)
    !   integ = (real(1,pr)/real(i,pr))*f(xi) + integ                 ! los N puntos los tomo de una lista de a lo mas m puntos?
    !   write(*,*) xi                                                 ! que son los que vienen de generar numeros usando el algoritmo de congruencia?
    ! end do
    ! write(*,*) integ,xi
  ! end do
  do i = 100,30000,100
    rn = 3
    integ = real(0,pr)
    do j = 1,i
      xi = rn1(rn,i)
      integ = (real(1,pr)/real(i,pr))*f(xi) + integ
    end do
    write(*,*) integ
  end do
  !
contains
  !
  ! Funcion que genera los puntos randoms
  function rn1(rn,m)
    implicit none
    integer, intent(inout) :: rn
    integer, intent(in) :: m
    real(pr) :: rn1
    integer :: a,c
    a = 3 ; c = 2 ; !m = real(2**16,pr)
    !
    rn1 = real((mod(a*rn+c,m)),pr)/real(m,pr)
    rn = rn1
    !
  end function rn1
  !
  ! Funcion a integrar
  function f(x)
    implicit none
    real(pr), intent(in) :: x
    real(pr) :: f
    !
    f = exp(-x)
    !
  end function f
end program prob6
