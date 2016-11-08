program prob13_bisec
implicit none
integer, parameter :: pr=kind(1.0)
integer :: i
real(pr), parameter :: tol = real(0.001,pr)
real(pr) :: a,b,c,u,v,w					! u=f(a), v=f(b), w=f(c)
!
write(*,*) 'Ingrese el intervalo [a,b] con a<b'
read(*,*) a,b
!
! Chequeo que a<b
if (a > b) then
  write(*,*) 'El intervalo es incorrecto, a debe ser menor que b'
  stop
else 
end if
!
u = f(a)
v = f(b)
! Ahora chequeo que uv<0, ie sign v /= sign u
if (sign(real(1,pr),u) == sign(real(1,pr),v)) then 					! aca puedo hacer if (u*v>0) then stop
  write(*,*) 'Error: la funcion en los extremos del intervalo tiene el mismo signo!'
  stop
else 
end if
!
! Hago el bucle para calcular la raiz
i = 0
 c = real(0.5,pr)*(a+b)
do while (abs(b-a) > tol)
  w = f(c)
!   Chequeo si c es raiz o sino achico el invervalo
    if (w == real(0,pr)) then
      write(*,*) 'La raiz esta en',c
      stop
    elseif (u*w < 0) then
      b = c
      v = w
    elseif (w*v<0) then
      a = c
      u = w
    else 
    end if
  c = real(0.5,pr)*(a+b)
  i = i + 1
end do
!
! Escribo el valor de c donde esta la raiz y el valor de f(c)
write(*,*) 'Se hicieron',i,'iteraciones'
write(*,*) 'La raiz esta en',c,'y el valor de f en c es',f(c) 			! me falta formatear la salida 
!
! Genero la funcion dentro del mismo programa
  contains
!
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
! Cierro el programa
end program prob13_bisec