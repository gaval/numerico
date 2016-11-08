! ----------------------------------------------------  !
!       Este programa calcula la integral de una        !
!       funcion definida por el usuario utilizando      !
!       el metodo de Simpson compuesto y agregando      !
!       una condicion para el error, i.e. do while      !
! ----------------------------------------------------  !
program simp
  use simp_trap, only: int_simpcomp
  implicit none
  integer, parameter :: pr=kind(1.d0)
  integer :: i
  integer :: n                ! Divide el intervalo
  real(pr), parameter :: tol = real(1E-7,pr)    ! este es el error con el que quiero calcular la integral
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
  ! Entro el valor exacto de la integral
  write(*,*) 'Ingrese el valor exacto de la integral de la funcion.'    
  read(*,*) s                                                           
  !
  ! Calculo el valor de la integral con la condicion del error
  i = 0 ; n = 4 ; e = real(1.5,pr)*tol      ! Tengo que empezar con n=4 para que esten bien definidas las sumatorias de la subrutina
  do while (e>tol)
    call int_simpcomp (a,b,n,f,t)
    e = abs(t-s)
    n = n + 5
    i = i + 1
  end do
  write(*,25) 'La integral de f con un error relativo de 1E-7 es I =',t
  25 format (A53,X,F9.7)
  write(*,35) 'Se utilizaron',n,'puntos.'
  35 format (A13,X,I8,X,A7)
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
end program simp
