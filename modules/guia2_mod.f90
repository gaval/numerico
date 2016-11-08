module guia2_mod      ! este modulo contiene subrutinas utilizadas en la guia 2
  !
contains
                    ! ----------------------------------------  !
                    !     Subrutina metodo de biseccion         !
                    ! ----------------------------------------  !
  subroutine biseccion (a1,b1,tol,f,p,iter,err1)
    integer,parameter :: pr = kind(1.d0)
    integer, intent(out) :: iter              ! iter = numero de iteraciones del dowhile
    real(pr), intent(in) :: a1,b1,tol,err1    ! [a,b] ; tol = tolerancia ; err = error absoluto
    real(pr), intent(out) :: p                ! valor de la raiz
    real(pr), external :: f                   ! funcion a la que se le calculan las raices
    real(pr) :: u,v,w,a,b,err                 ! variables usadas para el algoritmo
    !
    ! Renombramos los valores de f en los extremos
    a = a1
    b = b1
    err = err1
    u=f(a)
    v=f(b)
    !-----------------------!
    !Ahora chequeamos si a<b!
    !-----------------------!
    if (a > b) then
      write(*,*)'a>b, a debe ser menor que b!!'
      stop
      else
    end if
    !
    !---------------------------------------------------!
    !Ahora chequeamos que efectivamente sign(u)=/sign(v)!
    !---------------------------------------------------!
    open(4,file = 'biseccion.dat',action = 'write',status = 'replace')
    write(4,101)
    101 format (10X,'#p',18X,'err abs',15X,'err rel',16X,'i')
    if (sign(real(1,pr),u)==sign(real(1,pr),v)) then
      write(*,*)'el sgn de la fc es igual en ambos extremos del intervalo'
      write(*,*)'debe elegirse un intervalo adecuado al problema!'
      stop
      else
    end if
    !-----------------------------------------!
    !Ahora haremos el do para calcular la raiz!
    !-----------------------------------------!
    iter = 0
    p = a + ((b - a)/real(2,pr))
    err = abs(b - a)
    do while (err > tol)
      w=f(p) !Chequeo que p no sea raiz!
      if (w == real(0,pr)) then
          write(*,*)'la raiz esta en x=',p
          stop
        else if (v*w < real(0,pr)) then
          a = p
        else if (u*w < real(0,pr)) then
          b = p
          else
      end if
      err = abs(b - a)
      iter = iter + 1
      p = a + ((b - a)/real(2,pr))
      write(4,102) p,err,err/p,iter
      102 format (F17.16,2X,E18.17,2X,E18.17,2X,I1)
    end do
    close(4)
    !
  end subroutine biseccion
  !
  !
  !
                        ! ------------------------------------  !
                        ! Subroutine de Newton-Raphson          !
                        ! ------------------------------------  !
  !
  !
  subroutine newton_raphson (x00,tol,max_ite,f,g,xn,err1,iter)
    implicit none
    integer, parameter :: pr=kind(1.d0)
    integer, intent(in) :: max_ite                  ! numero maximo de iteraciones
    integer, intent(out) :: iter                    ! iteraciones del do while
    real(pr), intent(in) :: x00,tol                 ! dato inicial y tolerancia
    real(pr), intent(out) :: err1,xn                ! error entre iteraciones y valor de la raiz
    real(pr), external :: f,g                       ! funcion a buscar la raiz y su derivada
    real(pr) :: x1,x0,err                           ! alguna variable interna
    !
    err = err1
    x0 = x00
    iter = 0
    x1 = x0 - f(x0)/g(x0)
    xn = x1
    open(3,file = 'newton_raphson.dat',action = 'write',status = 'replace')
    write(3,101)
      101 format (10X,'xn',22X,'f(xn)',23X,'err abs',23X,'err rel')
    do while (iter < max_ite .and. abs(f(xn)) > tol .and. abs(xn-x0)/abs(xn) > tol)
      err = f(xn)/g(xn)
      xn = x1 - err
      x0 = x1
      x1 = xn
      iter = iter + 1
      write(3,102)xn,f(xn),abs(err),abs(err)/xn, iter
       102 format (F20.16,5X,E25.18,5X,E25.18,5X,E25.18,2X,I3)
    end do
    close(3)
    !
  end subroutine newton_raphson
  !
  !
  !
                                  ! ------------------------------------  !
                                  !         Subroutine Horner             !
                                  ! ------------------------------------  !
  !
  ! subroutine horner
  !   integer,parameter :: pr=kind(1.d0)
  !   real(pr),allocatable,dimension(:) :: a
  !   real(pr) :: pol,x
  !   integer :: i,n,j
  !   !
  !   do j=1,n
  !      pol=a(n)*x+a(n-j)
  !      a(n)=pol
  !   !    write(*,*)j
  !   end do
  ! end subroutine horner

















end module guia2_mod
