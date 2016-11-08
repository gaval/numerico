program prob_03
implicit none
integer, parameter :: K = 1000000, N = 1000,pr=kind(1.)
integer :: i,j
real(pr) :: a,b,c,d
!
a = 1000000._pr*0.1_pr
write(*,*) 'forma a)=',a
!
b = 0._pr
do i = 1, K
  b = 0.1_pr + b
end do
write(*,*) 'forma b)=',b
!
 c = 0._pr
do i = 1,N
  c = 0.1_pr + c
end do
d = 0._pr
do i = 1,N
  d = c + d
end do
write(*,*) 'forma c)=',d
!
! Tomamos a como valor exacto
write(*,*) 'error relativo % en b',((abs(a-b)/abs(a))*real(100,pr)),'error relativo % en c',((abs(a-d)/abs(a))*real(100,pr)) 
!
! Las diferencias obtenidas es por como hace los calculos la pc transformando a binario, recordar que 0.1 en binario es periodico
! entonces la pc tiene que redondear ese binario introduciendo error en cada paso del do. En el caso (c) al hacer menos pasos el 
! error por conversion a binario es menor.
end program prob_03
