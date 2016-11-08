program prob_14
implicit none
real(4) :: sumk, sum0, sumn
integer(8) :: k,i
integer(8), parameter :: N=10**8
real(8), parameter :: tol=10E-06
!
i = 0
sum0 = 0.
sumn = sumk - (sumk + 1./real(N))
!
do while (sumn>tol)
  do k=1,N-1
  sumk = 1./real(k) + sum0
  sum0 = sumk
  end do
sumn = sumk - (sumk + 1./real(N))
!sum0 = sumk
i = i + 1
end do
!
write(*,*) 'Iteraciones =',i
!
end program