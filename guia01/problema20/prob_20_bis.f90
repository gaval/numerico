program prob_20
implicit none
integer :: i
integer, parameter :: dp=kind(1.d0),sp=kind(1.)
real(sp), parameter :: g = 9.81_sp
real(dp) :: theta,v0,vx,vy,rx,ry,t,tf,dt,vel_x,vel_y,pos_x,pos_y
!
write(*,*) 'Ingrese theta (grados), v0 (m/s)'
read(*,*) theta,v0
!
open(3,file="mp.dat",action="write",status='replace')
write(3,105)
105 format('#    t',10X,'x(t)',10X,'y(t)',10X,'vx(t)',10X,'vy(t)')
!
i = 0
t = 0._dp
tf = 2._dp*(v0*sin(theta)/g)
dt = tf/real(600,dp)
do while (t < tf)
  i = i+1
  vx = vel_x (v0,theta)
  vy = vel_y (v0,theta,t)
  rx = pos_x (v0,theta,t)
  ry = pos_y (v0,theta,t)
  t = t + dt
  write(3,100) t,rx,ry,vx,vy
end do
100 format(E12.6,2X,E12.6,2X,E12.6,2X,E12.6,2X,E12.6)
close(3)
!
end program prob_20
!
!
!Funciones
!
!
function vel_x (v0,theta)
integer, parameter :: dp=kind(1.d0)
real(dp):: v0, theta 
real(dp) :: vel_x
!
vel_x = v0*cos(theta)
!
end function vel_x
!
function vel_y (v0,theta,t)
integer, parameter :: dp=kind(1.d0),sp=kind(1.)
real(sp), parameter :: g = 9.81_sp
real(dp):: v0, theta, t 
real(dp) :: vel_y
!
vel_y = v0*sin(theta) - g*t
!
end function vel_y
!
function pos_x (v0,theta,t)
integer, parameter :: dp=kind(1.d0)
real(dp) :: v0, theta, t
real(dp) :: pos_x
!
pos_x = v0*t*cos(theta)
!
end function pos_x
!
function pos_y (v0,theta,t)
integer, parameter :: dp=kind(1.d0),sp=kind(1.)
real(sp), parameter :: g = 9.81_sp
real(dp) :: v0, theta, t
real(dp) :: pos_y
!
pos_y = (v0*sin(theta) - 0.5_dp*g*t)*t
!
end function pos_y