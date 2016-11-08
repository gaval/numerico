module runge_kutta4
!
contains
  subroutine rk4 (ndim,xx,tt,hh,f,xx_sol)
    implicit none
    external :: f							! Subrutina que tiene las funciones F = (f1,f2,f3,...,fn) dim f = ndim
    integer, parameter :: pr = kind(1.d0)
    integer, intent(in) :: ndim
    real(pr), intent(in), dimension(1:ndim) :: xx 		! vector X = (x1,x2,x3,...,xn) dim x = ndim
    real(pr), intent(in) :: tt,hh 				! tiempo y paso h
    real(pr), intent(out), dimension(1:ndim) :: xx_sol 		! vector solucion
    real(pr), dimension(1:ndim) :: k1,k2,k3,k4,fun
    !
    ! Genero k1
    call f(tt,xx,fun)
    k1(:) = fun(:)
    ! Genero k2
    call f(tt + 0.5_pr*hh,xx + 0.5_pr*hh*k1,fun)
    k2(:) = fun(:)
    ! Genero k3
    call f(tt + 0.5_pr*hh,xx + 0.5_pr*hh*k2,fun)
    k3(:) = fun(:)
    ! Genero k4
    call f(tt + hh,xx + hh*k3,fun)
    k4(:) = fun(:)
    !
    ! Escribo el vector xx_sol
    xx_sol(:) = xx(:) + (1._pr/6._pr)*hh*(k1(:) + 2._pr*k2(:) + 2._pr*k3(:) + k4(:))
    !
  end subroutine rk4
end module runge_kutta4
