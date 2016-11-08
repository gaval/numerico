module gaussmod
CONTAINS
!
subroutine gauss(npts, a, b, x, w)
!****************************************************
!     Calcula puntos y pesos para integracion de Gauss-Legendre
!     npts: nro de puntos - 1
!     a,b : intervalo
!     x,w : puntos y pesos
!
!****************************************************
implicit none
integer, parameter :: pr=kind(1.d0)
integer, intent(in)  :: npts
real (pr), intent(in)   :: a,b
real (pr), intent(out), dimension(0:npts)  :: x,w
real (pr)  :: t,t1,pp,p1,p2,p3,eps
real (pr)  :: pi
real (pr)  :: xi
integer :: m,i,j
!
pi = acos(real(-1,pr))
m = 0; i = 0; j = 0
t = real(0,pr);  t1 = real(0,pr); pp = real(0,pr);
p1 = real(0,pr); p2 = real(0,pr); p3 = real(0,pr)
eps = real(3E-14,pr)       ! Accuracy: ********ADJUST THIS*********!
m = (npts + 1)/2
!       do i=1,m + 1
do i=1,m + 1
!          t = cos(pi*(dble(i) - 0.25d0)/(dble(npts) + 0.5d0) )
   t = cos((pi*(real(i,pr) - real(0.25,pr)))/(real(npts+1,pr) + real(0.5,pr)))
   t1 = real(1,pr)
   do while(abs(t - t1) >= eps)
      p1 = real(1,pr) ; p2 = real(0,pr)
!
      do j=1,npts + 1
         p3 = p2; p2 = p1
!                p1 = ( (2.d0*dble(j) - 1.d0)*t*p2 - (dble(j) - 1.d0)*p3)/(dble(j) )
         p1 = ((real(2*j,pr) - real(1,pr))*t*p2 - real(j-1,pr)*p3)/real(j,pr)
      enddo
!
!             pp = dble(npts)*(t*p1 - p2)/(t*t - 1.d0)
      pp = real(npts+1,pr)*(t*p1 - p2)/(t*t - real(1,pr))
      t1 = t
      t = t1 - p1/pp
   enddo
   x(i - 1) = - t;
!          x(npts - i) = t
   x(npts + 1 - i) = t
   w(i - 1) = real(2,pr)/((real(1,pr) - t*t)*pp*pp)
!          w(npts - i) = w(i - 1)
   w(npts + 1 - i) = w(i - 1)
!              print *," x(i - 1)", x(i - 1) , " w " , w(npts - i),i
enddo
!
do i=0, npts
   x(i) = x(i)*(b - a)/real(2,pr) + (b + a)/real(2,pr)
   w(i) = w(i)*(b - a)/real(2,pr)
enddo
!
return
end subroutine gauss
!
end module gaussmod
