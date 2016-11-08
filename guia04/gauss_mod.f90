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
integer, intent(in)  :: npts
real (kind(0d0)), intent(in)   :: a,b
real (kind(0d0)), intent(out)  :: x(0:npts),w(0:npts)
real (kind(0d0))  :: t,t1,pp,p1,p2,p3,eps
real (kind(0d0))  :: pi
real (kind(0d0))  :: xi
integer :: m,i,j
!
pi = acos(-1.d0)
m = 0; i = 0; j = 0
t = 0.d0;  t1 = 0.d0; pp = 0.d0;
p1 = 0.d0; p2 = 0.d0; p3 = 0.d0
eps = 3.d-14       ! Accuracy: ********ADJUST THIS*********!
m = (npts + 1)/2
!       do i=1,m + 1
do i=1,m + 1
!          t = cos(pi*(dble(i) - 0.25d0)/(dble(npts) + 0.5d0) )
   t = cos(pi*(dble(i) - 0.25d0)/(dble(npts+1) + 0.5d0) )
   t1 = 1.d0
   do while( (abs(t - t1) ) >= eps)
      p1 = 1.d0 ; p2 = 0.d0
!
      do j=1,npts + 1
         p3 = p2; p2 = p1
!                p1 = ( (2.d0*dble(j) - 1.d0)*t*p2 - (dble(j) - 1.d0)*p3)/(dble(j) )
         p1 = ( (2.d0*dble(j) - 1.d0)*t*p2 - dble(j-1)*p3)/(dble(j) )
      enddo
!
!             pp = dble(npts)*(t*p1 - p2)/(t*t - 1.d0)
      pp = dble(npts+1)*(t*p1 - p2)/(t*t - 1.d0)
      t1 = t
      t = t1 - p1/pp
   enddo
   x(i - 1) = - t;
!          x(npts - i) = t
   x(npts + 1 - i) = t
   w(i - 1) = 2.d0/( (1.d0 - t*t)*pp*pp)
!          w(npts - i) = w(i - 1)
   w(npts + 1 - i) = w(i - 1)
!              print *," x(i - 1)", x(i - 1) , " w " , w(npts - i),i
enddo
!
do i=0, npts
   x(i) = x(i)*(b - a)/2.d0 + (b + a)/2.d0
   w(i) = w(i)*(b - a)/2.d0
enddo
!
return
end subroutine gauss
!
end module gaussmod
