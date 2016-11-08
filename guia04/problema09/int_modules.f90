module simp_trap
  !
contains
  ! Simpson compuesta
  subroutine int_simpcomp (a,b,n,f,t)
    implicit none
    integer, parameter :: pr=kind(1.d0)
    integer  :: i,j,k,l               ! i,j para las sumas. k,l para definir los limites de las sumatorias
    integer, intent(in) :: n
    real(pr) :: xi,xj
    real(pr), external :: f                     ! funcion definida arriba
    real(pr) :: h                     ! paso h
    real(pr) :: sum1,sum2             ! las dos sumatorias del algoritmo
    real(pr), intent(in) :: a,b       ! input del intervalo [a,b]
    real(pr), intent(out) :: t        ! valor de la integral
    !
    ! Calculo el paso h
    h = (b-a)/real(n,pr)
    !
    ! Calculo las dos sumatorias del algoritmo por separado
    k = n/2 ; sum1 = real(0,pr)
    do i = 1,k,1
      xi = a + real(2*i-1,pr)*h
      sum1 = real(4,pr)*f(xi) + sum1
    end do
    !
    l = (n-2)/2 ; sum2 = real(0,pr)
    do j = 1,l,1
      xj = a + real(2*j,pr)*h
      sum2 = real(2,pr)*f(xj) + sum2
    end do
    !
    ! La integral es entonces
    t = (h/real(3,pr))*(f(a) + f(b) + sum1 + sum2)
    !
  end subroutine int_simpcomp
  !
  ! Trapecio compuesta
  subroutine int_trapcomp (a,b,n,f,t)
    implicit none
    integer, parameter :: pr=kind(1.d0)
    integer :: i
    real(pr) :: h,xi,xii
    real(pr), external :: f
    integer, intent(in) :: n
    real(pr), intent(in) :: a,b
    real(pr), intent(out) :: t
    !
    ! Escribo el paso h
    h = (b-a)/real(n,pr)
    !
    ! Calculo la integral
    t = real(0,pr)
    do i = 0,n-1,1
      xi = a + real(i,pr)*h
      xii = a + real(i+1,pr)*h
      t = real(0.5,pr)*h*(f(xi) + f(xii)) + t
    end do
  end subroutine int_trapcomp
  !
end module simp_trap
