! ----------------------------------------  !
!   Con este codigo vamos a calcular la     !
!   forma integral de Gauss-Hermite de      !
!   2 y 3 puntos para una funcion que       !
!   sea definida por el usuario             !
! ----------------------------------------  !
program g_h
  implicit none
  integer, parameter :: pr = kind(1.d0)
  integer :: n,i,j,k
  real(pr), allocatable, dimension(:) :: w,x
  real(pr) :: gh

  !
  write(*,*) 'Ingrese la cantidad de puntos N'
  read(*,*) n
  !
  allocate(w(n),x(n))
  !
  write(*,*) 'A continuacion ingrese los nodos en el orden indicado'
  do i = 1,n
    write(*,'(A9,I1)') 'Ingrese x',i
    read(*,*) x(i)
  end do
  !
  write(*,*) 'A continuacion ingrese los pesos en el orden indicado'
  do j = 1,n
    write(*,'(A9,I1)') 'Ingrese w',j
    read(*,*) w(j)
  end do
  !
  gh = real(0,pr)
  do k = 1,n
    gh = w(k)*f(x(k)) + gh
  end do
  !
  deallocate(w,x)
  !
  write(*,*) 'La integral de f(x) entre (-inf,inf) es',gh
  !
contains
  !
  function f(y)
    implicit none
    real(pr), intent(in) :: y
    real(pr) :: f
    !
    f = exp(y)
    !
  end function
  !
end program g_h

! con mis pesos y nodos  que calcule a mano me da 1.26... y wolfram 1.28....
! con los pesos y nodos que calcula la libreria que baje no da ni cerca.
