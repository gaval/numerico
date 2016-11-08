program prob_8			! Esta es la estructura del problema 7
implicit none
integer, parameter :: pr=kind(1.0)
integer :: i
real(pr) :: a,b,c,tol,u,v,w					! u=f(a), v=f(b), w=f(c)
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
write(*,*) 'Ingrese la tolerancia en precision',pr
read(*,*) tol
! Evaluo la funcion en los extremos del intervalo
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
write(*,*) 'La raiz esta en',c,'y el valor de f en c es',f(c)
!
! Genero la funcion dentro del mismo programa
  contains
!
function f(x)
implicit none
integer, parameter :: pr=kind(1.0)
real(pr), intent(in) :: x
real(pr) :: f
!
f = real(2,pr)*x - tan(x) !real(2,pr)*x - tan(x) !x*x - real(3,pr) !escriba su funcion aqui
!
end function
!
! Cierro el programa
end program prob_8