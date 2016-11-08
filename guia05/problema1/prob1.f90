! ======================================== !
!       Generador de numeros enteros       !
!       aleatorios usando el algoritmo     !
!       congruencial.                      !
! ======================================== !
program prob1
  implicit none
  integer(8) :: rn                          ! rn = seed
  integer(8) :: i
  !
  open(3,file='random.dat',action='write',status='replace')
  rn = 3
  !
  do i = 1,2**8
    write(3,*) rn1(rn)
  end do
  close(3)
  !
contains
  function rn1(rn)
    implicit none
    integer(8), intent(inout) :: rn
    integer(8) :: rn1
    integer(8) :: a,c,m
    a = 13 ; c = 2 ; m = 2**8                 ! si pones 2**31 - 1 te tira aritmethic overflow al compilar OJO!
    !
    rn1 = mod(a*rn+c,m)
    rn = rn1
    !
  end function rn1
end program prob1
