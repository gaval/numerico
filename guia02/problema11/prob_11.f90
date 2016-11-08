program prob_11
implicit none
integer, parameter :: pr=kind(1.)
integer :: i 					! MAX_ITE: relajo esta condicion
real(pr) :: x0,tol,fn,gn,x1,xn,ln,r
! 
! 
write(*,*) 'Ingrese la aproximacion x0'
read(*,*) x0
! 
write(*,*) 'Ingrese la tolerancia de hasta precision',pr
read(*,*) tol
! 
! write(*,*) 'Ingrese la cantidad maxima de iteraciones'  
! read(*,*) MAX_ITE
! 
write(*,*) 'Ingrese el numero R>0 al que desea calcularle la raiz cubica'
read(*,*) r
! Chequeo que R sea positivo
if (r < 0) then
  write(*,*) 'ERROR: El numero que ingreso es negativo'
  stop
else
end if
! 
! Algoritmo N-R
i = 0
fn = f(x0,r)
gn = g(x0,r)
x1 = x0 - fn/gn
do while (abs(fn) > tol .and. abs(xn-x0)/abs(xn) > tol)
  ln = l(x1,x0)
!   Veo si es x0 es raiz, sino busco xn
    if (ln == real(0,pr)) then
      write(*,*) 'La raiz esta en',x1
      stop
    else
      fn = f(x1,r)
      gn = g(x1,r)
      xn = x1 - fn/gn
    end if
  x0 = x1
  x1 = xn
  i = i + 1
end do
! 
! Escribo el valor de las iteraciones, xn, y modulo de f(xn)
write(*,*) 'Se realizaron',i,'iteraciones'
write(*,*) 'La raiz se encuentra en',x1,'y la funcion en ese punto vale',f(x1,r)
write(*,*) 'El error relativo es'
! 
! Defino f y f'
  contains
! funcion
function f(x,r)
integer, parameter :: pr=kind(1.)
real(pr), intent(in) :: x,r
real(pr) :: f
!
f = x*x*x - r
!
end function
! 
! derivada f' = g
function g(x,r)
integer, parameter :: pr=kind(1.)
real(pr), intent(in) :: x,r
real(pr) :: g
!
g = real(3,pr)*x*x
!
end function
!
! 
function l(x,x0)
integer, parameter :: pr=kind(1.)
real(pr), intent(in) :: x,x0
real(pr) :: l
! 
l = g(x0,r)*(x-x0) + f(x0,r)
! 
end function
! 
end program prob_11