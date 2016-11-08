program gaulog  !! PROGRAMA QUE HIZO LA VIEJA GORDA NEGRA 
use gaussmode, only: gauss   !bajar el modulo gauss de la pagina
implicit none
!
real(kind(1.d0)) :: a,b,fx,exacto,gerror
real(kind(1.d0)) :: gcuad,h
real(kind(1.d0)),allocatable :: w(:),x(:)
integer :: i,j,npts,in,n_max
!
a=0.d0
b=1.d0
exacto=1.d0 - exp(-1.d0)
open(37,file='err_gauss.dat',status='replace')
!
!calculo para diferentes valores de x
n_max=5
allocate (x(0:n_max-1),w(0:n_max-1))

do i=3,n_max,2 !impar solo para simpson
  npts=in+1
  call gauss(nts,a,b,x(0:npts),w(0:npts)) !calculo nodos y pesos
    do j=0,npts
      write(*,*)in,x(j),w(j) !!el do lo usamos para que escriba uno abajo del otro,porq sino leerlo es un quilombo
    end do
  !calculo la integral
  gcuad=0.d0

  do j=0,npts
    call func(x(j),fx)
    gcuad=gcuad+fx*w(j)
!     if  (j==0) h=(b-a)/real(npts,pr)  !definir precision
!     call func
!     gerror=abs(gcuad-exacto)
  end do
  gerror=abs(gcuad-exacto)

  write(37,'(I5,2x,(e15.6,2x))')in,gerror

end do
close(37)
end program

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!ESTO COMVIENE HACERLO CON UN CONTAINS

subroutine func(x,fx)
implicit none
real(kind(1.d0)),intent(in) :: x
real(kind(1.d0)),intent(out) :: fx
fx=exp(-x)
return
end subroutine func
