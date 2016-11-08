program prob13_nr
implicit none
integer, parameter :: pr=kind(1.)
integer :: i
real(pr), parameter :: tol = real(0.001,pr) 
real(pr) :: t0,fn,gn,t1,tn,ln
! 
! 
write(*,*) 'Ingrese la aproximacion x0'
read(*,*) t0
! 
! Algoritmo N-R
i = 0
fn = f(t0)
gn = h(t0)
t1 = t0 - fn/gn
do while (abs(fn) > tol)		! .and. abs(xn-x0)/abs(xn) > tol relajo esa condicion
  ln = l(t1,t0)
!   Veo si es t0 es raiz, sino busco xn
    if (ln == real(0,pr)) then
      write(*,*) 'La raiz esta en',t1
      stop
    else
      fn = f(t1)
      gn = h(t1)
      tn = t1 - fn/gn
    end if
  t0 = t1
  t1 = tn
  i = i + 1
end do
! 
! Escribo el valor de las iteraciones, xn, y modulo de f(xn)
write(*,*) 'Se realizaron',i,'iteraciones'
write(*,*) 'La raiz se encuentra en',t1,'y la funcion en ese punto vale',f(t1)
write(*,*) 'El error relativo es'
! 
! Defino f y f'
  contains
! funcion
function f(t)
implicit none
integer, parameter :: pr=kind(1.)
real(pr), parameter :: h0 = real(10,pr), m = real(0.1,pr), k = real(0.149,pr), g = real(9.8,pr)
real(pr), intent(in) :: t
real(pr) :: f
!
f = h0 + (m*g/k)*(-t + (m/k)*(real(1,pr) - exp(-k*t/m)))
!
end function
! 
! derivada f' = g
function h(t)
integer, parameter :: pr=kind(1.)
real(pr), parameter :: h0 = real(10,pr), m = real(0.1,pr), k = real(0.149,pr), g = real(9.8,pr)
real(pr), intent(in) :: t
real(pr) :: h
!
h = -(m*g/k)*(real(1,pr) + exp(-k*t/m))
!
end function
!
! 
function l(t,t0)
integer, parameter :: pr=kind(1.)
real(pr), intent(in) :: t,t0
real(pr) :: l
! 
l = h(t0)*(t-t0) + f(t0)
! 
end function
! 
end program prob13_nr