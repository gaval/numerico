! ======================================  !
!     Calculamos integrales con metodo    !
!     de Gauss-Legendre usando un modulo  !
!     que calcula nodos y pesos.          !
!                                         !
!     Luego calculamos la integral de     !
!     la misma funcion usando Simpson     !
!     y Trapecio compuestas.              !
!                                         !
!     Finalmente se comparan los errores  !
!     entre los 3 metodos.                !
! ======================================  !
program gauss_legendre
  use gaussmod
  use simp_trap
  implicit none
  integer, parameter :: pr=kind(1.d0)
  integer :: n                                  ! Numero de puntos
  integer :: i,j,k                              ! variable para el loop
  real(pr), allocatable, dimension(:) :: x,w    ! nodos y pesos
  real(pr) :: a,b                               ! intervalo [a,b]
  real(pr) :: gl,glex,errgl                     ! gl = numerica, glex = exacta, error relativo g-l
  real(pr) :: simp,errsimp                      ! simp = numerica, errsimp = error relativo simp
  real(pr) :: trap,errtrap                      ! trap = numerica, errtrap = error relativo trap
  !
  write(*,*) 'Ingrese el numero de puntos N'
  read(*,*) n
  write(*,*) 'Ingrese el paso'
  read(*,*) k
  !
  ! write(*,*) 'Ingrese el intervalo [a,b] con a<b'
  ! read(*,*) a,b
  ! if (a>b) then
  !   write(*,*) 'ERROR: a>b'
  !   stop
  ! end if
  a = real(0,pr)
  b = real(1,pr)
  !
  ! ====================================  !
  !           GAUSS-LEGENDRE              !
  ! ====================================  !
  !
  open(3,file='gl.dat',action='write',status='old',position='append')
  do j = 2,n,k
    allocate(x(0:j-1),w(0:j-1))
    call gauss(j-1,a,b,x,w)
    !
    glex = real(1,pr) - exp(real(-1,pr))
    gl=real(0,pr)
      do i=0,j-1
        gl = w(i)*f(x(i))+gl
      end do
    !
    errgl = abs(glex-gl)/abs(glex)
    write(3,25) (j-1),errgl
    25 format (I4,2X,E12.6)
    deallocate(x,w)
  end do
  close(3)
  !
  ! ====================================  !
  !               SIMPSON                 !
  ! ====================================  !
  !
  open(5,file='simp.dat',action='write',status='old',position='append')
  do j = 1,n,k
    call int_simpcomp (a,b,j,f,simp)
    !
    glex = real(1,pr) - exp(real(-1,pr))
    errsimp = abs(glex-simp)/abs(glex)
    write(5,35) j,errsimp
    35 format (I4,2X,E12.6)
  end do
  close(5)
  !
  ! ====================================  !
  !               TRAPECIO                !
  ! ====================================  !
  !
  open(7,file='trap.dat',action='write',status='old',position='append')
  do j = 1,n,k
    call int_trapcomp (a,b,j,f,trap)
    !
    glex = real(1,pr) - exp(real(-1,pr))
    errtrap = abs(glex-trap)/abs(glex)
    write(7,45) j,errtrap
    45 format (I4,2X,E12.6)
  end do
  close(7)
  !
contains
  function f(y)
    implicit none
    real(pr),intent(in) :: y
    real(pr) :: f
    !
    f = exp(-y)
    !
  end function f
  !
end program gauss_legendre
