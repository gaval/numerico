program prob_11
implicit none
integer(kind=4) :: n,d
!
write(*,*) 'Ingrese un entero n ='
read(*,*) n
!
d = mod(n,10) ! la función mod hace: mod(x,y) = x - INT(x/y)*y donde INT(x) toma la parte entera de x
!
if (d == 0) then
  write(*,*) 'n es múltiplo de 2 y de 5'
else
  write(*,*) 'n no es múltiplo de 2 ni de 5'
end if
!
end program prob_11
!
!-------------------------------------------------------------------------------------------------------
! otra forma de hacerlo podría ser:
!integer(kind=4) :: n,r
!
!write(*,*) 'Ingrese un entero n ='
!read(*,*) n
!
!r = n/10
!if (r == 1 .or. r odd) then
!	write(*,*) 'n es multiplo de 2 y de 5'
!else
!	write(*,*) 'n no es multiplo de 2 ni de 5'
!end if
!
!me faltaría ver como hacer que el programa detecte si es par
!-------------------------------------------------------------------------------------------------------
