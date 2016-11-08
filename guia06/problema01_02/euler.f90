module ode_euler
  ! ========================================================================= !
  !   Este modulo contiene el algoritmo de resolucion de ODE con el metodo de !
  !   Euler.                                                                  !
  !                                                                           !
  !   Las entradas de la subrutina son:                                       !
  !     * el intervalo [a,b]                                                  !
  !     * la condicion inicial x(t0)                                          !
  !     * el tiempo inicial t0                                                !
  !     * la cantidad de pasos n                                              !
  !     * la funcion tal que x'=f(t,x)                                        !
  !     * la solucion exacta xe (opcional)                                    !
  !   La salida de la subrutina es la funcion x(t) evaluada en los t=t+h      !
  !   en un archivo .dat (ahora le fijo el nombre del archivo pero se puede   !
  !   poner para que el usuario determine el nombre del mismo).               !
  !   Y el error e(t).                                                        !
  !                                                                           !
  !   La salida tambien da el archivo .dat que contiene err(t) vs t           !
  ! ========================================================================= !
contains
  subroutine euler (a,b,n,x0,t0,f,xe,x,err)
    implicit none
    integer, parameter :: pr = kind(1.d0)
    integer :: i
    integer, intent(in) :: n
    real(pr), intent(in) :: a,b,x0,t0
    real(pr), intent(out) :: x,err
    real(pr) :: h,t,xex
    real(pr), external :: f,xe
    !
    ! Defino el paso h
    h = (b - a)/real(n,pr)
    !
    ! Algoritmo de Euler
    open(3,file='xvst.dat',action='write',status='replace')
    open(5,file='errvst.dat',action='write',status='replace')
    open(7,file='xexvst.dat',action='write',status='replace')
    write(3,15) '#      t','x(t)'
    write(5,20) '#      t','err(t)'
    15 format (A8,13X,A4)
    20 format (A8,13X,A6)
    t = t0
    x = x0
    do i = 1,n,1
      x = x + h*f(t+h,x)
      xex = xe(t)
      err = abs(x-xex)
      t = t + h
      write(3,25) t,x
      write(5,25) t,err
      write(7,25) t,xex
    end do
    25 format (2(2X,E12.6))
    close(3) ; close(5)
    !
  end subroutine euler
end module ode_euler
