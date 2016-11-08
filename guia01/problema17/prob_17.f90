program prob_17
implicit none
real(4), dimension(50,20) :: a
integer(4) :: j
!
!este problema lo que quiere es que nosotros le setiemos los valore4s de lo que pide usando a(x:y,k:p)
!como input para cada entrada ij de la matriz.
!
! open(3,file="matrix.dat",status="replace")
!la primera fila de a
!do j=1,20
 ! a(1:50,j) = 1.
 ! write(3,*),a
!end do
!close(3)
a(1:50,1:1)=1.
write(*,*),a
!
end program