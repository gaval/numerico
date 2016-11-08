program prob_2 		! Algoritmo de Lagrange general
implicit none
integer, parameter :: pr=kind(1.)
integer :: n,m
real(pr) :: x0,pol
real(pr), allocatable, dimension(:) :: x,y!,card
!
write(*,*) 'Ingrese el grado del polinomio interpolante que desea calcular'
read(*,*) n
!
write(*,*) 'Ingrese el punto x donde sea evaluar el polinomio'
read(*,*) x0
!
open(3,file='puntos_lagrange.dat',action='read',status='old')
allocate(x(0:n),y(0:n))!,card(0:n))
!
do m=0,n
  read(3,*) x(m),y(m)
end do
!
!call cardinal (n,x,x0,card)
call polinomio (n,x0,y,pol)!,card,pol)
!
write(*,*) 'Su polinomio de orden',n,'en',x0,'es =',pol
!
deallocate(x,y)!,card)
close(3)
!
!
  contains
! Subrutinas
!
subroutine cardinal (n,x,x0,l)			!card = l
implicit none
integer :: i,j
integer, intent(in) :: n
real(pr), intent(in) :: x0
real(pr), intent(in), dimension(0:n) :: x
real(pr), intent(out), dimension(0:n) :: l
!
l(:) = real(1,pr)
do i = 0,n
    do j = 0,n,1			! Como hago para que j /= i?
      if(j /= i)then
	l(i) = ((x0 - x(j))/(x(i) - x(j)))*l(i)
      end if
    end do
end do
!
end subroutine cardinal
!
!
subroutine polinomio (n,x0,y,pol)!,card,pol)
implicit none
integer :: k
integer, intent(in) :: n
real(pr), intent(in) :: x0		! esta de mas esta definicion?
real(pr), intent(in), dimension(0:n) :: y!,card
real(pr), dimension(0:n) :: card
real(pr), intent(out) :: pol
!
pol = real(0,pr)
call cardinal (n,x,x0,card)		!aca o adentro del do?
! do k = 1,n
!   pol = pol + card(k)*y(k)
! end do
pol = dot_product(card,y)
!
end subroutine polinomio
!
!
end program prob_2