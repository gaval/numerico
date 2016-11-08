program prob_15
implicit none
real(8) :: p1, pn
integer(4) :: i
!
p1 = 1.
do i=1,10**6
  pn = (((2.*i)**2)/(((2.*i)**2)-1.))*p1
  p1 = pn
end do
!
write(*,*) 'pi =',2.*pn
end program prob_15