! ======================================  !
!   Este modulo contiene los algoritmos   !
!   de diferenciacion de 2, 3 y 5 puntos  !
! ======================================  !
!
!
! ======================================================================= !
!     SUBRUTINAS:                                                         !
!           forw_backw_centr = derivada primera usando algoritmos hacia   !
!                              adelante, hacia atras y centrado.          !
!           twopointsf1 = formula de 2 puntos para f primera.             !
!           threepointsf1 = formula de 3 puntos para f primera.           !
!           fivepointsf1 = formula de 5 puntos para f primera.            !
!           threepointsf2 = formula de 3 puntos para f segunda.           !
!           fivepointsf2 = formula de 5 puntos para f segunda.            !
! ======================================================================= !
module diferenciacion
  !
contains
  !
                  ! ============================================= !
                  !   Algoritmos para la derivada primera de f    !
                  ! ============================================= !
  !
  ! =================================== !
  !     Formula hacia adelante, hacia   !
  !     atras y centrada.               !
  ! =================================== !
  subroutine forw_backw_centr (x,h,f,fd,bd,cd)
    implicit none
    integer, parameter :: pr = kind(1.d0)
    real(pr), external :: f
    real(pr), intent(in) :: x,h
    real(pr), intent(out) :: fd,bd,cd
    !
    fd = (f(x+h) - f(x))/(h)
    bd = (f(x-h) - f(x))/(h)
    cd = (f(x+real(0.5,pr)*h) - f(x-real(0.5,pr)*h))/(h)
    !
  end subroutine forw_backw_centr
  !
  ! =================================== !
  !      Formula de 2 puntos para la    !
  !      derivada primera de f.         !
  ! =================================== !
  subroutine twopointsf1 (x,h,f,g)
    implicit none
    integer, parameter :: pr = kind(1.d0)
    real(pr), external :: f
    real(pr), intent(in) :: x,h
    real(pr), intent(out) :: g
    !
    g = (f(x+h) - f(x))/h
    !
  end subroutine twopointsf1
  !
  ! =================================== !
  !      Formula de 3 puntos para la    !
  !      derivada primera de f.         !
  ! =================================== !
  subroutine threepointsf1 (x,h,f,g)
    implicit none
    integer, parameter :: pr = kind(1.d0)
    real(pr), external :: f
    real(pr), intent(in) :: x,h
    real(pr), intent(out) :: g
    !
    g = (f(x+h) - f(x-h))/(real(2,pr)*h)
    !
  end subroutine threepointsf1
  !
  ! =================================== !
  !      Formula de 5 puntos para la    !
  !      derivada primera de f.         !
  ! =================================== !
  subroutine fivepointsf1 (x,h,f,g)
    implicit none
    integer, parameter :: pr = kind(1.d0)
    real(pr), external :: f
    real(pr), intent(in) :: x,h
    real(pr), intent(out) :: g
    !
    g = (f(x+real(2,pr)*h) - real(8,pr)*f(x+h) + real(8,pr)*f(x-h) - f(x-real(2,pr)*h))/(real(12,pr)*h)
    !
  end subroutine fivepointsf1
  !
                  ! ============================================= !
                  !   Algoritmos para la derivada segunda de f    !
                  ! ============================================= !
  !
  ! =================================== !
  !      Formula de 3 puntos para la    !
  !      derivada segunda de f.         !
  ! =================================== !
  subroutine threepointsf2 (x,h,f,g)
    implicit none
    integer, parameter :: pr = kind(1.d0)
    real(pr), external :: f
    real(pr), intent(in) :: x,h
    real(pr), intent(out) :: g
    !
    g = (f(x+h) - real(2,pr)*f(x) + f(x-h))/(h**2)
    !
  end subroutine threepointsf2
  !
  ! =================================== !
  !      Formula de 5 puntos para la    !
  !      derivada segunda de f.         !
  ! =================================== !
  subroutine fivepointsf2 (x,h,f,g)
    implicit none
    integer, parameter :: pr = kind(1.d0)
    real(pr), external :: f
    real(pr), intent(in) :: x,h
    real(pr), intent(out) :: g
    !
    g = (-f(x-real(2,pr)*h) + real(16,pr)*f(x+h) - real(30,pr)*f(x) + real(16,pr)*f(x-h) - f(x+real(2,pr)*h))/(real(12,pr)*(h**2))
    !
  end subroutine fivepointsf2
  !
end module diferenciacion
