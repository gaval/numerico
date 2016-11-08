program prob_18
implicit none
integer(8) :: n, i, j
real(8), allocatable, dimension(:) :: v
real(8), allocatable, dimension(:,:) :: mat
real(8), allocatable, dimension(:,:) :: prod,prod0
!
write(*,*) 'ingrese la dimension del vector y la matriz'
read(*,*) n
!
allocate(v(n))
allocate(mat(n,n))
!
call subroutine generavector (n,v)
call subroutine generamatriz (n,mat)
!
prod0 = 0
!
do j = 1,n !do anidado
   do i = 1,n
     prod = v(i)*mat(i,j) + prod0
     prod0 = prod
   end do  
end do   
!
deallocate(v, mat)
!
!
!Subrutinas
!
!
  contains
!  
subroutine generavector (n,v)
integer(4), intent(in) :: n
real(4), dimension(n) :: v
!
v(1::2) = 3. !este formato me dice que empieza en el primer lugar hasta n con un paso de 2 lugares
v(:2:2) = 2. !igual que arriba pero empieza en el 2do lugar
!
return
!
end subroutine generavector
!
!
subroutine generamatriz (n,mat)
integer(e), intent(in) :: n
real(4), dimension(n) :: mat
!
mat(,)
return
!
end subroutine generamatriz
