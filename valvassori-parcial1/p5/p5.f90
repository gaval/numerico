!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!! No llegue con el tiempo a terminar esto
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program p5
implicit none
integer, parameter :: dp=kind(1.d0)
integer :: n,i,j,k
real(dp), allocatable, dimension(:) :: v1,v2
real(dp), allocatable, dimension(:,:) :: m
real(dp), allocatable, dimension(:) :: w,prod,u
!
write(*,*) 'Ingrese la dimension N'
read(*,*) n
if (n <= 0) then
  write(*,*) 'La dimension debe ser N>0'
  stop
end if
!
open(3,file='resultado5.dat',action='write',status='replace')
allocate(v1(n),v2(n),m(n,n),w(n))
!
! Genero vectores
! v1
v1(1:n:1) = real(1,dp)
v1(2:n:1) = real(3,dp)
! v2
i = n/2
v2(1:n) = real(0,dp)
v2(1:i) = real(2,dp)
!
! Genero matriz
do k =1,n				!! no lleguue c
  
  m(1,k) = v1(1,k) - v2(1,k)

deallocate(v1,v2,m,w)
close(3)
!
end program p5
!
! Subrutina 
subroutine prod (n,v1,v2,m,w)
implicit none
integer, parameter :: dp=kind(1.d0)
integer :: n,i,j
real(dp), intent(in), dimension(n) :: v1,v2
real(dp), intent(in), dimension(n,n) :: m
real(dp), dimension(n) :: u,v
real(dp), intent(out), dimension(n) :: w
!
do j = 1,n
  v = real(0,dp)
    do i = 1,n
      v = v1(i)*m(i,j) + v
    end do
  u(j) = v
  w(j) = u(j)*v2(j)
end do
!
end subroutine prod