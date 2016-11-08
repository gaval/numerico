program prob_10
implicit none
real(kind=8) :: a,b
!
! Queremos que el programa nos pida un input de dos reales y el output nos diga cual de ellos
! es mayor o si son iguales.
!
write(*,*) 'Ingrese el numero real a ='
read(*,*) a
!
write(*,*) 'Ingrese el numero real b ='
read(*,*) b
!
!
if (a==b) then
  write(*,*) 'a y b son iguales'
elseif (a > b) then
  write(*,*) 'a es mayor que b'
!elseif (a < b) then
!  write(*,*) 'a es menor que b'          ! Se puede hacer usando estas 3 lineas de comando
!else					  ! o simplemente por descarte como en las siguientes lineas
else 
  write(*,*) 'a es menor que b'		  ! Escribiendo estas lineas (supongo yo) se hace menos costoso
end if					  ! el programa (en términos del tiempo de procesamiento).
!
! el if/elseif lo que va a hacer es chequear si pasa que a=b imprime en pantalla el resultado
! sino chequeara si a>b y lo imprimira en pantalla, de lo contrario chequeara si a<b y lo imprimira
! en pantalla.
!
end program prob_10