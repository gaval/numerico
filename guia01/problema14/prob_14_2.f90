program prob_14_dowhile
implicit none
real(4) :: sumk, sumn, sum0
real, parameter :: t=10E-6 !tolerancia
integer(8) :: i,k
integer, parameter :: N=10E8
!
!sum0 = 0.
!sumn = sumk - (sumk + 1./real(N))
!
do while ((sumk - (sumk + 1./real(N))) > t)
  do k=1,N-1
    sumk = 1./real(k) + sumk
    !sum0 = sumk
  end do
i = i + 1
end do
!
write(*,*) 'iteraciones=',i
!
end program