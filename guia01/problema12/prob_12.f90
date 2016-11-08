program prob_12
implicit none
real(kind=8) :: a,b,e,d,x,y !x e y son las raices
!
write(*,*) 'Ingrese el coeficiente a =' 
read(*,*) a
!
write(*,*) 'Ingrese el coeficiente b =' 
read(*,*) b
!
write(*,*) 'Ingrese el coeficiente c =' 
read(*,*) e
!
d = b**2 - 4.*a*e
if (d > 0.) then
  x = (1./2.*a)*(-b + sqrt(d))
  y = (1./2.*a)*(-b - sqrt(d))
  write(*,*) 'r1 = ',x
  write(*,*) 'r2 = ',y
elseif (d == 0.) then
  x = (1./2.*a)*(-b)
  write(*,*) 'r1 = r2 =',x
else
  write(*,*) 'r1 = ',(1./2.*a)*(-b) ,"+i", (1./2.*a)*sqrt(-d) !escribiendo ,".", hace un print como a+ib
  write(*,*) 'r2 = ',(1./2.*a)*(-b) ,"-i", (1./2.*a)*sqrt(-d)
end if
!
end program prob_12