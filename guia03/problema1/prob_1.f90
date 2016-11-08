program prob_1 		! Algoritmo de Newton general
implicit none		! Estaria bueno hacer un modulo del algoritmo de Horner y aplicarlo en la subrutina polinomio (o no?)
integer, parameter :: pr = kind(1.)
integer :: n,m
real(pr) :: x0,pn
real(pr), allocatable, dimension(:) :: x,y,c
!
write(*,*) 'Ingrese el grado del polinomio interpolante que desea obtener'
read(*,*) n
!
write(*,*) 'Ingrese el punto x donde desea evaluar el polinomio'
read(*,*) x0
!
open(3,file='puntos_newton.dat',action='read',status='old')
allocate(x(0:n),y(0:n),c(0:n))		! Puedo hacer una forma para chequear que la cantidad de puntos que haya en el archivo sea la misma cantidad que la
!					! dimension n?
do m = 0,n
read(3,*) x(m),y(m)
write(*,*) x(m),y(m)
end do
call coef (n,x,y,c)
write(*,*) c
call polinomio (c,n,x0,x,pn)
!
write(*,*) pn
!
deallocate (x,y,c)
close(3)
!
  contains
! Subrutinas
!
subroutine coef (n,x,y,c)		! Calculo los coeficientes cj
implicit none
integer :: i,j
integer, intent(in) :: n
real(pr) :: productoria
real(pr), intent(in), dimension(0:n) :: x,y
real(pr), intent(out), dimension(0:n) :: c
!
 c(0) = y(0)
do i = 1,n,1
    productoria = real(1,pr)
    do j = 0,i-1,1
      productoria = productoria*(x(i) - x(j))
    end do
  call polinomio (c,i-1,x(i),x,pn)
  c(i) = (y(i) - pn)/productoria
end do
!
end subroutine coef
!
!
subroutine polinomio (c,n,x0,x,pn)
implicit none
integer :: j,k
integer, intent(in) :: n
real(pr) :: p
real(pr), intent(in) :: x0
real(pr), intent(in), dimension(0:n) :: c,x
real(pr), intent(out) :: pn
!
if(n == 0)then
  pn = c(0)
else
  pn = real(0,pr)
  do j = 1,n,1
    p = real(1,pr)
      do k = 0,j-1,1
	       p = p*(x0 - x(k))
      end do
    pn = pn + c(j)*p
  end do
  pn = pn + c(0)
end if
!
end subroutine polinomio
!
!
end program prob_1
