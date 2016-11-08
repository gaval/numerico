program prob_20
implicit none
integer, parameter :: sp=kind(1.)
integer :: cfrs,xpnnt,i,j,k,n
real(sp) :: tol,cota,piN,piN1,piabs,pN,pN1
!
write(*,*) 'Ingrese la cantidad de cifras significativas con las que desea calcular el valor de PI (entre 1 y 5 cifras)'
read(*,*) cfrs
!
! Comprobamos que las cifras esten en el rango permitido
if (cfrs < 1 .or. cfrs > 5) then
  write(*,*) 'La cantidad de cifras debe estar entre 1 y 5'
  stop
else
end if
!
! Calculemos mi "epsilon de maquina" para dicha cantidad de cifras 
!
xpnnt = cfrs + 1
tol = 9_sp/real(10**xpnnt,sp)
write(*,*) tol
!
! Ahora con esta tolerancia veamos cuanto vale el valor absoluto del primer termino despreciado
!
k = 0
i = 1
do i = 1,1000000
  piN = 0_sp
    do j = 0,i,1
      piN = 4_sp*(real((-1)**j,sp)/real((2*j + 1),sp)) + piN
    end do
  piN1 = 0_sp
    do j = 0,i+1,1
      piN1 = 4_sp*(real((-1)**j,sp)/real((2*j + 1),sp)) + piN1
    end do
  pN = abs(piN)
  pN1 = abs(piN1)
  piabs = abs(pN1 - pN)
!   i = i + 1
  k = k + 1
  if (piabs<tol) exit
!    write(*,*) pN,pN1,piabs,k
end do
write(*,100) 'Se necesitan',k,'terminos para obtener pi con',cfrs,'cifras significativas.'
100 format(A12,X,I5,X,A28,X,I1,X,A22)
!
if (cfrs == 1) then
  write(*,200) 'Y el valor de pi es',pN
  200 format (A19,X,F3.1)
  stop
elseif (cfrs == 2) then
  write(*,300) 'Y el valor de pi es',pN
  300 format (A19,X,F4.2)
  stop
elseif (cfrs == 3) then
  write(*,400) 'Y el valor de pi es',pN
  400 format (A19,X,F5.3)
  stop
elseif (cfrs == 4) then
  write(*,500) 'Y el valor de pi es',pN
  500 format (A19,X,F6.4)
  stop
elseif (cfrs == 5) then
  write(*,600) 'Y el valor de pi es',pN
  600 format (A19,X,F7.5)
  stop
else
end if
!
end program