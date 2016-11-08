program horner
implicit none
!
integer,parameter :: pr=kind(1.d0)
real(pr),allocatable,dimension(:) :: a
real(pr) :: pol,x
integer :: i,n,j
!
write(*,*)'queremos evaluar un polinomio de orden n'
write(*,*)'ingrese el orden del polinoimio,i.e el coeficiente que acompaña a la maxima potencia de x'
read(*,*)n
write(*,*)'ingrese el punto donde quiere evaluar el polinomio'
write(*,*)'x='
read(*,*)x
!
allocate (a(0:n))
!---------------------------------------!
!Inicializamos los elementos del array a!
!---------------------------------------!
do i=0,n
  write(*,101)'ingrese el coeficiente a(',i,')'		
    101 format (A25,1X,I1,1X,A1)
  read(*,*)a(i)
end do
!-------------------------------------------------------------------!
!Ahora utilizaremos el algoritmo de Horner para evaluar el polinomio!
!-------------------------------------------------------------------!
do j=1,n
   pol=a(n)*x+a(n-j)
   a(n)=pol 
!    write(*,*)j
end do
write(*,*)'el resultado es p(x)=',pol 
deallocate (a)
end program horner
