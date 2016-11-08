program cambio_base_reloaded
implicit none
integer, parameter 		:: maxdig=50
integer(8) 			:: n,b,resto,digito
character(len=maxdig) 		:: resultado
character(len=1) 		:: d
!
write(*,*) 'Este programa cambiara de base un numero cuyo resultado tenga hasta',maxdig,'digitos'
!
write(*,*) 'Ingrese numero natural en base 10'
read(*,*), n
write(*,*) 'Ingrese una base 2<= b <= 16'
read(*,*), b
!
if (n < 0) then
  write(*,*) 'El numero debe ser positivo'
  stop
end if
!
if (b<2 .or. b>16 .or. b<0) then
  write(*,*) 'la base debe ser 2 <= b <= 16'
  stop
end if
!
digito = 0
resultado = ''
!
do while (n /= 0)		! La condición de que n sea distinto de cero es porque n = n/b, cuando el resto
  digito = digito + 1		! sea cero el paso recursivo haría 0/b lo cuál no tiene sentido.
  if (digito>maxdig) then
    write(*,*) 'El cambio de base de este numero excede los',maxdig,'digitos.'
    stop
  end if
  resto = mod(n,b)
  n = n/b
  select case (resto)
    case(10)
      d = 'A'
    case(11)
      d = 'B'
    case(12)
      d = 'C'
    case(13)
      d = 'D'
    case(14)
      d = 'E'
    case(15)
      d = 'F'
    case default		! El case default le aplica la misma sentencia ejecutable a todos los otros casos
      write(d,'(I1)'),resto 	! no especificados anteriormente, entonces en este wrtie lo que le digo es que me 
  end select			! grabe el valor del resto en el caracter d.
resultado = d//resultado	! Así cuando redefino el caracter resultado que lo concateno con d me escribe todos
write(d,'(I1)'),resto		! valores que fue tomando el resto y que se guardaron en el caracter d.
end do				
write(*,*),'El número en base',b,'es: ',resultado
!
end program cambio_base_reloaded