! ======================================== !
!   Este programa genera numeros random    !
!   en el intervalo [0,1] usando el        !
!   algoritmo congruencial                 !
! ======================================== !
program prob3
  implicit none
  integer, parameter :: pr = kind(1.d0)
  integer :: rn                          ! rn = seed
  integer :: i,m
  real(pr) :: j_1,j_2
  !
  open(3,file='random.dat',action='write',status='replace')
  rn = 3 ; m = 6075
  !
  do i = 1,3000
      j_1 = real(rn1(rn),pr)/real(m,pr)
      j_2 = real(rn1(rn),pr)/real(m,pr)
      write(3,'(2(2X,E16.7))') j_1,j_2
  end do
  close(3)
  !
contains
  function rn1(rn)
    implicit none
    integer, intent(inout) :: rn
    integer :: rn1
    integer :: a,c
    a = 106 ; c = 1283                ! si pones 2**32 - 1 te tira aritmethic overflow al compilar OJO!
    !
    rn1 = mod(a*rn+c,m)
    rn = rn1
    !
  end function rn1
end program prob3
