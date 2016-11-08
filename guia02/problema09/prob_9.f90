program prob_9
implicit none
integer, parameter :: pr=kind(1.)
integer :: i, MAX_ITE
real(pr) :: x0,tol,fn,gn,x1,xn,ln
! 
! 
write(*,*) 'Ingrese la aproximacion x0'
read(*,*) x0
! 
write(*,*) 'Ingrese la tolerancia de hasta precision',pr
read(*,*) tol
! 
write(*,*) 'Ingrese la cantidad maxima de iteraciones'
read(*,*) MAX_ITE
! 
! Algoritmo N-R
i = 0
fn = f(x0)
gn = g(x0)
x1 = x0 - fn/gn
do while (abs(fn) > tol .and. i < MAX_ITE .and. abs(xn-x0)/abs(xn) > tol)
  ln = l(x1,x0)
!   Veo si es x0 es raiz, sino busco xn
    if (ln == real(0,pr)) then
      write(*,*) 'La raiz esta en',x1
      stop
    else
      fn = f(x1)
      gn = g(x1)
      xn = x1 - fn/gn
    end if
  x0 = x1
  x1 = xn
  i = i + 1
end do
! 
! Escribo el valor de las iteraciones, xn, y modulo de f(xn)
write(*,*) 'Se realizaron',i,'iteraciones'
write(*,*) 'La raiz se encuentra en',x1,'y la funcion en ese punto vale',f(x1)
write(*,*) 'El error relativo es'
! 
! Defino f y f'
  contains
! funcion
function f(x)
integer, parameter :: pr=kind(1.)
real(pr), intent(in) :: x
real(pr) :: f
!
f = real(2,pr)*x - tan(x) ! aca escriba su funcion
!
end function
! 
! derivada f' = g
function g(x)
integer, parameter :: pr=kind(1.)
real(pr), intent(in) :: x
real(pr) :: g
!
g = real(2,pr) - real(1,pr)/(cos(x)*cos(x)) ! aca escriba la derivada de su funcion
!
end function
!
! 
function l(x,x0)
integer, parameter :: pr=kind(1.)
real(pr), intent(in) :: x,x0
real(pr) :: l
! 
l = g(x0)*(x-x0) + f(x0)
! 
end function
! 
end program prob_9