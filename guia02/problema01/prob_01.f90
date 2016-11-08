program test_igualdad
implicit none
integer, parameter :: dp=kind(1.d0)
real(kind=dp) :: k
!
!k = 19.08_dp + 2.01_dp !== 21.09d0
!write(*,*) k, 29.09_dp
!stop
!
if (19.08_dp + 2.01_dp == 21.09_dp) then
  write(*,*) '19.08 + 2.01 = 21.09'
else
  write(*,*) '19.08 + 2.01 /= 21.09'
end if
!
end program test_igualdad