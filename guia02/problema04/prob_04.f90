program prob_04
implicit none
real(kind(1.)) :: a,b,c,x1,x2,y1,y2
!
! Puesto que el polinomio a calcular las raices no tiene ninguna compleja, no hago la parte
! del programa que tenga en cuenta si el discriminante es menor que cero.
!
write(*,*) 'ingrese los coeficientes a,b,c de a*x**2 + b*x + c'
read(*,*) a,b,c
!
x1 = (-b+sqrt(b*b - 4.*a*c))/(2.*a)
x2 = (-b-sqrt(b*b - 4.*a*c))/(2.*a)
write(*,*) 'De la forma comun: x1 =',x1,'x2 =',x2
!
y1 = (-2.*c)/(b+sqrt(b*b - 4.*a*c))
y2 = (2.*c)/(-b+sqrt(b*b - 4.*a*c))
!
write(*,*) 'De la forma racionalizada: y1 =',y1,'y2 =',y2
!
end program prob_04