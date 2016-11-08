program prob_13
implicit none
!real(4) :: 
!real, parameter ::
integer(4) :: n,b,k,d1,r1,rk,dk !d1 y dk son los cocientes 1 y k-esimo
!
write(*,*) 'ingrese un numero entero n'
read(*,*),n
!
write(*,*) 'ingrese una base b<=16'
read(*,*),b
!
if (n == 0 .or. b == 0 .or. b>16 .or. b<2) then
  write(*,*)'n debe ser distinto de cero, la base debe estar entre 2 y 16'
  stop
end if
!
d1 = INT(n/b)
r1 = n-d1*b
!
do while (dk >= b)
  dk = INT(d1/b)
  d1 = dk
  rk = ((n-r1)/b)-dk*b
  r1 = rk
  write(*,*), rk
end do
!
end program prob_13