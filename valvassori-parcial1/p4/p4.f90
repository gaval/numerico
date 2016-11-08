program p4
implicit none
integer, parameter :: sp=kind(1.)
integer, parameter :: N = 1000000
integer :: k
real(sp) :: s1,s2,ea1,ea2,er1,er2,erp1,erp2
!
! Calculo el inciso a)
s1 = real(0,sp)
do k = 1,N
  s1 = real(1,sp)/sqrt(real(k,sp)) + s1
end do
write(*,*) 'S1 es',s1
!
! Calculo el inciso b)
s2 = real(0,sp)
do k = 1,N
  s2 = real(1,sp)/sqrt(real(N-k+1,sp)) + s2
end do
write(*,*) 'S2 es',s2
!
! Ahora conociendo el valor exacto calculemos el error absoluto, relativo y porcentual
! En S1
write(*,*) 'Errores en S1'
ea1 = abs(real(1998.54014,sp) - s1)
er1 = (abs(real(1998.54014,sp) - s1))/abs(real(1998.54014,sp))
erp1 = real(100,sp)*((abs(real(1998.54014,sp) - s1))/abs(real(1998.54014,sp)))
write(*,*) 'absoluto',ea1,'relativo',er1,'porcentual',erp1
!
! En s2
write(*,*) 'Errores en S2'
ea2 = abs(real(1998.54014,sp) - s2)
er2 = (abs(real(1998.54014,sp) - s2))/abs(real(1998.54014,sp))
erp2 = real(100,sp)*((abs(real(1998.54014,sp) - s2))/abs(real(1998.54014,sp)))
write(*,*) 'absoluto',ea2,'relativo',er2,'porcentual',erp2
!
end program p4  