program p3
implicit none
integer, parameter :: pr=kind(1.) 			! Aca se cambia la presicion con la que se quiere realizar el calculo
integer :: i
real(pr), parameter :: pi = 3.14159265358979324, g = 9.8
real(pr) :: theta0,l,m					! Variables que ingreso
real(pr) :: w,p,t,dt					! Variables que calculo
real(pr) :: pang,vang 					! La salida de la subrutina
!
! Pido los valores de entrada
write(*,*) 'Ingrese el angulo inicial en radianes'
read(*,*) theta0
!
write(*,*) 'Ingrese la longitud del pendulo en metros'
read(*,*) l
!
write(*,*) 'Ingrese la masa del cuerpo en kilogramos'
read(*,*) m
!
! Calculo w y P
w = sqrt(g/l)
p = (real(2,pr)*pi)/w
! 
! El incremento temporal es dt
dt = p/real(500,pr)
! 
! Abramos el archivo s3.dat
open (unit=3,file='s3.dat',action='write',status='replace')
write(3,300) '# Los datos iniciales son: theta =',theta0,'rad l =',l,'m m =',m,'kg.'	! Escribo en archivo los datos iniciales
300 format (A34,X,F3.2,A7,X,F5.2,A5,X,F4.2,A3)
write(3,100) '# t','pos ang','vel ang'				! Doy nombre a las columnas
100 format (A3,9X,A7,9X,A7)
! 
! Calculemos las cosas que pide
t = real(0,pr)
i = 0
do while (t < p)
  call posvel (theta0,w,t,pang,vang)
  t = t + dt
  i = i + 1
  write(3,200) t,pang,vang
  200 format (E11.5,2X,E11.5,2X,E11.5)
end do
close(3) 							! Cierro el archivo
!
!
end program p3
!
! Subrutina
!
subroutine posvel (theta0,w,t,pang,vang)
implicit none
integer, parameter :: pr=kind(1.)
real(pr), parameter :: g = 9.8
real(pr), intent(in) :: theta0,w,t				! Las variables de entrada
real(pr), intent(out) :: pang,vang 				! pang = posicion angular, vang = velocidad agular
!
pang = theta0*cos(w*t)
vang = -theta0*w*sin(w*t)
!
end subroutine posvel