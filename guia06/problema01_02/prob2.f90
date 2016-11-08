program prob2
  use ode_euler
  implicit none
  integer, parameter :: pr = kind(1.d0)
  integer :: n,i
  real(pr) :: a,b,t0,x0,x,err
  real(pr) :: h,t
  !
  write(*,*) 'Ingrese el intervalo de tiempo [a,b]'
  read(*,*) a,b
  !ahora chequeo que a<b
  if (a > b) then
    ! write(*,*) 'ERROR: Ingreso mal el intervalo, a debe ser menor que b.'
    stop "ERROR: Ingreso mal el intervalo, a debe ser menor que b"
  end if
  !
  write(*,*) 'Ingrese la cantidad de pasos n'
  read(*,*) n
  !
  write(*,*) 'Ingrese la condicion inicial x0 = x(t=t0) y el valor del tiempo t0'
  read(*,*) x0,t0
  !
  ! Llamo la subrutina del modulo
  call euler(a,b,n,x0,t0,f,xe,x,err)
  !
contains
  ! defino la funcion x'=f(t,x)
  function f(t,y)
    implicit none
    real(pr), intent(in) :: t,y
    real(pr) :: f
    real(pr) :: pi
    !
    pi = real(3.14159265358979,pr)
    f = -y + sin(real(2,pr)*pi*t)
    !
  end function f
  !
  ! defino el valor exacto de x(t)
  function xe(tt)
    implicit none
    real(pr), intent(in) :: tt
    real(pr) :: xe,pi
    !
    pi = real(3.14159265358979,pr)
    xe = (1._pr + (2._pr*pi)/(1._pr + 4._pr*pi**2))*exp(-tt) + (sin(2._pr*pi*tt) - 2._pr*pi*cos(2._pr*pi*tt))/(1._pr + 4._pr*pi**2)
    !
  end function xe
end program prob2
