program prob_02
implicit none
integer, parameter :: K = 10000000,sp=kind(1.)
integer :: i
real(sp) :: s, o
!
s = 0._sp
do i = 1,K
  s = 1./real(i,sp) + s
end do
write(*,*) "en orden usual =",s
!
o = 0._sp
do i = K,1,-1
  o = 1./real(i,sp) + o
end do
write(*,*) "en orden opuesto =",o
!
end program prob_02