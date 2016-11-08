! ------------------------------------------------------------------------------ !
! A este programa se le puede mejorar el output haciendo que la salida para cada !
! parte sea un archivo con 3 columnas (xnex,xnpr,er) y que en cada fila indique  !
! que xn es para que quede mas ordenado y entendible. Ahora (27/04/15) no tengo  !
! ganas ni tiempo de hacer eso.							 !
! ------------------------------------------------------------------------------ !
program prob_06
implicit none
integer, parameter :: dp=kind(1.d0),sp=kind(1.)
integer :: i
real(sp) :: xnex,xnsp,x0,x1,er                  ! Estos dos conjuntos de variables son !
real(dp) :: xnex1,xndp,x00,x11,err              ! para el programa del inciso (b)      !
!
real(sp) :: ynex,ynsp,y0,y1,ery			! Esos dos conjuntos de variables son  !
real(dp) :: ynex1,yndp,y00,y11,erry		! para el programa del inciso (c)      !
!
! ------------------------------------------------------------------------------- !
! 			Inciso (b)						  !
!	xnex = xn = 1/3^n asumiendo que este es el valor exacto.		  !
! 	xndp = xnsp = (13/3)xn-1 - (4/3)xn-2					  !
! ------------------------------------------------------------------------------- !
!
! Calculo con simple presicion !
open (3,file='ersp.d',action='write',status='replace')
write(3,100) 'Error relativo porcentual'
100 format (A25)
x0 = real(1,sp)
x1 = real(1,sp)/real(3,sp)
do i = 0,15
  xnex = real(1,sp)/real(3**(i+2),sp)
  xnsp = (real(13,sp)/real(3,sp))*x1 - (real(4,sp)/real(3,sp))*x0
  x0 = x1
  x1 = xnsp
  er = (abs(xnex - xnsp)/abs(xnex))*real(100,sp)
  write(3,*) er
end do
close(3)
!
! Calculo con doble presicion !
open (4,file='erdp.d',action='write',status='replace')
write(4,200) 'Error relativo porcentual'
200 format (A25)
x00 = real(1,dp)
x11 = real(1,dp)/real(3,dp)
do i = 0,15
  xnex1 = real(1,dp)/real(3**(i+2),dp)
  xndp = (real(13,dp)/real(3,dp))*x11 - (real(4,dp)/real(3,dp))*x00
  x00 = x11
  x11 = xndp
  err = (abs(xnex1 - xndp)/abs(xnex1))*real(100,sp)
  write(4,*) err
end do
close(4)
!
! ------------------------------------------------------------------------------- !
!				Inciso (c)					  !
!	yn=1/xn => yn = 3^(n+1) lo asumo como valor exacto			  !
!	ynsp = 3/(13(1/yn-1) - 4(1/yn-2))					  !
! ------------------------------------------------------------------------------- !
!
! En simple presicion !
open (5,file='erspc.d',action='write',status='replace')
write(5,'(A25)') 'Error relativo porcentual'
y0 = real(1,sp)									  ! Este algoritmo es levemente mejor, comparando los errores	!
y1 = real(3,sp)									  ! relativos con los obtenidos arriba vemos que el error en el	!
do i = 0,15									  ! quinto valor es del ~80% mientras que en el inciso (b) era	!
  ynex = real(3**(i+2),sp)							  ! de ~101%							!
  xnex = real(1,sp)/ynex
  ynsp = real(3,sp)/(real(13,sp)*(real(1,sp)/y1) - real(4,sp)*(real(1,sp)/y0))
  xnsp = real(1,sp)/ynsp
  y0 = y1
  y1 = ynsp
  ery = (abs(xnex - xnsp)/abs(xnex))*real(100,sp)
  write(5,*) ery
end do
close(5)
!
! En doble presicion !
open (6,file='erdpc.d',action='write',status='replace')
write(6,'(A25)') 'Error relativo porcentual'
y00 = real(1,dp)								  ! Con esta presicion mejora mucho el algoritmo, comparando    !
y11 = real(3,dp)								  ! los errores relativos con los obtenidos en sp vemos que el  !
do i = 0,15									  ! error en el doceavo valor es bastante mejor que en sp	!
  ynex1 = real(3**(i+2),dp)
  xnex1 = real(1,sp)/ynex1
  yndp = real(3,dp)/(real(13,dp)*(real(1,dp)/y11) - real(4,dp)*(real(1,dp)/y00))
  xndp = real(1,dp)/yndp
  y00 = y11
  y11 = yndp
  erry = (abs(xnex1 - xndp)/abs(xnex1))*real(100,dp)
  write(6,*) erry
end do
close(6)
!
end program prob_06
!
!
! ------------------------------------------------------------------------------------- !
! 					CONCLUSIONES 					!
! El algoritmo del inciso (b) es totalmente inestable pero en doble presicion mejora un	!
! poquito (pero sigue siendo inestable).						!
! 											!
! El algoritmo del inciso (b) vuelve a ser inestable pero ahora la presicion mejora 	!
! donde se anoto más arriba. Y en doble presicion pasa lo mismo. 			!
!											!
! De todas maneras, ambos algoritmos son numericamente inestables que seguramente se	!
! debe a que la formula general es del tipo xn = A/3^n + B4^n y nosotros, dados los 	!
! dos primeros valores usamos otra.							!
! -------------------------------------------------------------------------------------	!