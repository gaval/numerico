program prob9
  use diferenciacion, only: forw_backw_centr,fivepointsf1
  implicit none
  integer, parameter :: pr = kind(1.d0)
  integer :: k
  real(pr) :: x,h,fd,bd,cd,deri
  real(pr) :: erru1,erru2,erru3,errv1,errv2,errv3,fex,gex
  !
  write(*,*) 'Ingrese el valor de x donde desea evaluar la derivada'
  read(*,*) x
  !
  fex = -sin(x)
  gex = exp(x)
  write(*,*) fex,gex
  !
  open(3,file='cosfd.dat',action='write',status='replace')
  open(5,file='coscd.dat',action='write',status='replace')
  open(7,file='cos5p.dat',action='write',status='replace')
  write(3,'(A39,X,F5.1)') '# el punto a evaluar la derivada es x =',x
  write(5,'(A39,X,F5.1)') '# el punto a evaluar la derivada es x =',x
  write(7,'(A39,X,F5.1)') '# el punto a evaluar la derivada es x =',x
  do k = 1,14
    h = real(10,pr)**(-k)
    call forw_backw_centr (x,h,u,fd,bd,cd)
    call fivepointsf1 (x,h,u,deri)
    erru1 = abs(fex-fd)/abs(fex)
    erru2 = abs(fex-cd)/abs(fex)
    erru3 = abs(fex-deri)/abs(fex)
    write(3,'(E12.6,2X,E12.6)') h,erru1
    write(5,'(E12.6,2X,E12.6)') h,erru2
    write(7,'(E12.6,2X,E12.6)') h,erru3
  end do
  close(3) ; close(5) ; close(7)
  !
  open(9,file='expfd.dat',action='write',status='replace')
  open(11,file='expcd.dat',action='write',status='replace')
  open(13,file='exp5p.dat',action='write',status='replace')
  write(9,'(A39,X,F5.1)') '# el punto a evaluar la derivada es x =',x
  write(11,'(A39,X,F5.1)') '# el punto a evaluar la derivada es x =',x
  write(13,'(A39,X,F5.1)') '# el punto a evaluar la derivada es x =',x
  do k = 1,14
    h = real(10,pr)**(-k)
    call forw_backw_centr (x,h,v,fd,bd,cd)
    call fivepointsf1 (x,h,v,deri)
    errv1 = abs(gex-fd)/abs(fex)
    errv2 = abs(gex-cd)/abs(fex)
    errv3 = abs(gex-deri)/abs(fex)
    write(9,'(E12.6,2X,E12.6)') h,errv1
    write(11,'(E12.6,2X,E12.6)') h,errv2
    write(13,'(E12.6,2X,E12.6)') h,errv3
  end do
  close(9) ; close(11) ; close(13)
  !
contains
  function u(y)
    implicit none
    integer, parameter :: pr = kind(1.d0)
    real(pr), intent(in) :: y
    real(pr) :: u
    !
    u = cos(y)
    !
  end function u
  !
  function v(z)
    implicit none
    integer, parameter :: pr = kind(1.d0)
    real(pr), intent(in) :: z
    real(pr) :: v
    !
    v = exp(z)
    !
  end function v
end program prob9
