program prob2
  implicit none
  integer(8) :: rn                          ! rn = seed
  integer(8) :: i
  !
  open(3,file='random.dat',action='write',status='replace')
  rn = 3
  !
  do i = 1,15
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
    a = 3 ; c = 8 ; m = 12                 ! si pones 2**32 - 1 te tira aritmethic overflow al compilar OJO!
    !
    rn1 = mod(a*rn+c,m)
    rn = rn1
    !
  end function rn1
end program prob2
