! ----------------------------------------------------  !
!       Este programa calcula la integral de una        !
!       funcion definida por el usuario utilizando      !
!       el metodo del trapecio compuesto                !
! ----------------------------------------------------  !
program trapecomp
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
! Llamo a la subrutina para que me calcule la integral con los datos ingresados previamente
call int_trapcomp (a,b,n,f,t)
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
!         es Q = 3.999875465            !
! ------------------------------------  !
!
contains
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
  ! Subrutina: integra con algoritmo del trapecio compuesto
  subroutine int_trapcomp (a,b,n,f,t)
    implicit none
    integer :: i
    real(pr) :: h,xi,xii
    real(pr) :: f
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
  !
end program trapecomp
