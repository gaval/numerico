program test
implicit none
real(kind(1.)) :: x,y,s
integer :: i,j
!
i=6; j=3; x=4.; y=8.
call sum (i,x,s)
write(*,'(G8.2,"+",G8.2,"=",F5.1)') i,x,s
!
end program test
!
!
! Subrutina
!
!
subroutine sum (z,w,ss)
implicit none
real(kind(1.)), intent(in) :: z,w
real(kind(1.)), intent(out) :: ss
!
ss = z + w
!
end subroutine sum
