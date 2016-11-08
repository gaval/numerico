! ============================================  !
!   Este programa calcula integrales con        !
!   cuadratura de Gauss-Laguerre de 2 puntos.   !
!   Los nodos y pesos los calcule a mano.       !
! ============================================  !
program gauss_laguerre
  implicit none
  integer, parameter :: pr = kind(1.d0)
  integer :: n,i,j,k,o
  real(pr), allocatable, dimension(:) :: x,w
  real(pr) :: gl
  !
  write(*,*) 'Ingrese la cantidad de puntos n'
  read(*,*) n
  !
  allocate(x(n),w(n))
  !
  write(*,*) 'Ingrese en el orden indicado los nodos xi'
  do i = 1,n
    write(*,'(A9,I1)') 'Ingrese x',i
    read(*,*) x(i)
  end do
  !
  write(*,*) 'Ingrese en el orden indicado los pesos wi'
  do j = 1,n
    write(*,'(A9,I1)') 'Ingrese w',j
    read(*,*) w(j)
  end do
  !
  ! ========================================= !
  !     Caclculo la integral usando Gauss-    !
  !     Laguerre.                             !
  ! ========================================= !
  !
  gl=real(0,pr)
  do k=1,n
    gl = w(k)*f(x(k))+gl
  end do
  !
  deallocate(x,w)
  write(*,*) 'La integral de f(x) usando Gauss-Laguerre es',gl
  !
contains
  function f(y)
    implicit none
    real(pr), intent(in) :: y
    real(pr) :: f
    !
    f = (y**3)/(exp(y)-real(1,pr))
    !
  end function
  !
end program gauss_laguerre
