program prob_18
implicit none
integer(8) :: n, i
real(8), allocatable, dimension(:) :: v,u
real(8), allocatable, dimension(:,:) :: mat
real(8), allocatable, dimension(:,:) :: prod
character(len=5) :: nombre
!
write(*,*) 'ingrese la dimension del vector y la matriz'
read(*,*) n
!
nombre = 'mat.d'
!
open(3,file=nombre,status='old')

allocate(v(n),u(n))
allocate(mat(n,n))
!
v(1:n:2) = 3.d0 !este formato me dice que empieza en el primer lugar hasta n con un paso de 2 lugares
v(2:n:2) = 2.d0 !igual que arriba pero empieza en el 2do lugar
!
mat(:,:) = 0.d0
do i = 1,n
  mat(i,i) = 3.d0
  mat(i,i+1:n) = 1.d0
!  mat(i+1,1:) = 0.d0
end do
call mult (n,v,mat,u)
deallocate(v, mat)
!
write(3,*) u
close(3)

!
end program prob_18
!
!
!Subrutina
!
!  
subroutine mult (n,v,mat,u)
implicit none
integer(8) :: j,i
integer(8), intent(in) :: n
real(8), intent(in), dimension(n) :: v	
real(8), intent(in), dimension(n,n) :: mat
real(8), intent(out), dimension(n) :: u !este es mi producto u = v.mat
real(8) :: prod
!
do j = 1,n !do anidado
prod = 0.d0
   do i = 1,n
     prod = v(i)*mat(i,j) + prod
   end do  
u(j) = prod
end do
!
end subroutine mult