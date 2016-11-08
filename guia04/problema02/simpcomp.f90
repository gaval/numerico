! ----------------------------------------------------  !
!       Este programa calcula la integral de una        !
!       funcion definida por el usuario utilizando      !
!       el metodo de Simpson compuesto                  !
! ----------------------------------------------------  !
program simpcomp
  implicit none
  integer, parameter :: pr=kind(1.d0)
  integer :: n                ! Divide el intervalo
  real(pr) :: a,b,h           ! Intervalo [a,b] y paso h
  real(pr) :: t               ! Integral de la funcion definida en el contains
  real(pr) :: s               ! Integral exacta
  real(pr) :: e               ! error (en el practico aparece como epsilon de h)
  !
  ! Entro el intervalo
  write(*,*) 'ingrese su intervalo [a,b] con a<b'
  read(*,*) a,b
  if (a>b) then
    write(*,*) 'ERROR: a>b'
    stop
  end if
  !
  ! Entro el N para dividir el intervalo
  write(*,*) 'ingrese N entero para dividir el intervalo'
  read(*,*) n
  !
  ! Entro el valor exacto de la integral
  write(*,*) 'Ingrese el valor exacto de la integral de la funcion.'    ! Como puedo hacer para que no introduzca errores
  read(*,*) s                                                           ! si no se conoce el valor s?
  !
  ! Llamo a la subrutina para que calcule la integral con los datos ingresados
  call int_simpcomp (a,b,n,f,t)
  write(*,25) 'La integral de f es aproximadamente',t
  25 format (A35,X,F12.10)
  !
  ! Calculo el error
  h = (b-a)/real(n,pr)
  e = abs(t-s)
  write(*,35) 'El error e(h) = |S-I| con h =',h,'es e(h) =',e
  35 format (A29,X,F8.6,X,A9,X,F12.10)
  !
  ! ------------------------------------  !
  !     El cociente de precision Q        !
  !         es Q = 15.64285714            !
  ! ------------------------------------  !
  !
contains
  !
  ! Defino la funcion a integrar
  function f(x)
    implicit none
    real(pr), intent(in) :: x
    real(pr) :: f
    !
    f = exp(-x)           ! Aca hay que poner la funcion que se desea integrar
    !
  end function
  !
  ! Subrutina: algoritmo de Simpson
  subroutine int_simpcomp (a,b,n,f,t)
    implicit none
    integer  :: i,j,k,l               ! i,j para las sumas. k,l para definir los limites de las sumatorias
    integer, intent(in) :: n
    real(pr) :: xi,xj
    real(pr) :: f                     ! funcion definida arriba
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
end program simpcomp
