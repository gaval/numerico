program suma_trigonometrica
implicit none 						! Agrego el implicit none
integer, parameter :: dp=kind(1.d0)     		! Borre los espacios, defino la presicion para que sea portable
integer :: i,N						! Defino la variable N
real(kind=dp) :: suma,f        				! Cambio el real(8) para la portabilidad y defino las variables a usar
!
write(*,*) 'Ingrese N' 					! Mejoro el print
read(*,*) N 	       					! Agrego el read para que me lea del teclado
if (N < 1) then
  write(*,*) 'N debe ser  mayor o igual que 1'		! Verifico que N sea mayor o igual que 1
  stop
end if
!
suma = real(0,dp) 					! Tengo que iniciar la suma
do i = 1,N
  if (mod(i,3) == 0) then				! Acomodo el argumento del if
    suma = real(1,dp)/real(i,dp) + suma			! Convierto lo entero a real
  else							! Mejore la estructura del do y el if
    suma = f(i) + suma					! aca decia sums y es suma
  end if						! Antes cerraba el do y luego el if, esta mal, primero cierra el if
end do							! luego el do
write(*,*) 'La suma es',suma				! Puse mas bonito el print de salida
!
end program suma_trigonometrica 			! El programa cierra aca y no despues de la funcion
!
! Funcion
function f(x)
implicit none						! Agrego el implicit none
integer, parameter :: dp=kind(1.d0)			! Aca defino nuevamente la presicion para portabilidad
integer :: x						! La variable debe ser entera
real(kind=dp), parameter :: Pi=3.14159265358979324 	! Lo defino como parametro y como es doble presicion le quito cifras
real(kind=dp) :: f 					! Defino la variable f
!
f = cos(Pi*real(x,dp))/real(x,dp) 			! Dado que la variable es entera y pi es real debo convertir lo entero a
!							! real.
end function f
