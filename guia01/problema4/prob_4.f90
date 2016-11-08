PROGRAM prob_4 ! Problema 4 - Guia 1
IMPLICIT NONE
REAL(4) :: a,b,h,d,e,f,g ! Trabaja con reales de 8 cifras significativas
!h es el inciso c pero cuando lo escribo c = ... me lo toma como si estuviera comentando la linea
!
a = 5/2 + 20/6
WRITE(*,*) 'Inciso a =',a
!
b = 4*(6/2) - 15/2
WRITE(*,*) 'Inciso b =',b
!
h = 5*(15/(2/(4-2)))
WRITE(*,*) 'Inciso c =',h
!
d = 1 + 1/4
WRITE(*,*) 'Inciso d =',d
!
e = 1.0 + 1/4
WRITE(*,*) 'Inciso e =',e
!
f = 1 + 1.0/4
WRITE(*,*) 'Inciso f =',f
!
g = 1.0 + 1.0/4.0
WRITE(*,*) 'Inciso g =',g
!
!
! Falta aprender como pedir que el WRITE tenga k cifras significativas de acuerdo a lo que necesite
!
!
END PROGRAM prob_4