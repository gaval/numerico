program prob14bis
implicit none
integer, parameter :: pr=kind(1.)
integer(8) :: i
real(pr) :: sumk,sumM
!
sumk = 0_pr
i = 1
do while ( sumM - sumk /= 0 )
  sumk = 1_pr/real(i,pr) + sumk
  sumM = sumk + 1_pr/real(i+1,pr)
  i = i + 1
end do
write(*,*) (i-1)
!
end program prob14bis