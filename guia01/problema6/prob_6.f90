program prob_6
implicit none
integer, parameter :: pr=kind(1.)
real(pr) :: a,b,c
character(len=2) :: nan='ho'
!
a = real(1,pr)/real(0,pr)
write(*,*) a
!
b = real(0,pr)/real(0,pr)
write(*,*) b
! !
  c = real(2,pr)*real(0,pr)
write(*,*) c
!
end program prob_6