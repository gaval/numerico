! ========================================================================= !
! Este codigo resuelve ODE's con condiciones de borde usando RK4 y el       !
! metodo de la secante como está en el libro Kincaid Six Edition.           !
! ========================================================================= !
program shooting
    use runge_kutta4
    implicit none
    integer, parameter :: pr = kind(1.d0) , ndim = 2
    integer :: i,npasos,j
    real(pr), parameter :: tol = real(1E-5,pr)
    real(pr), dimension(1:ndim) :: x, x_s           ! x = condic. inic. x_s = solucion
    real(pr) :: a,b,t,h,t0,beta                     ! beta = x_s(b)
    real(pr) :: phi1,phi2,phi3                      ! phi(z) = x_s|z (b) - beta
    real(pr) :: z1,z2,z3                            ! los z / x'(a) = z1,z2,z3
    real(pr) :: x0
    !
    a = 1._pr ; b = 2._pr ; t0 = 1._pr ; h = 0.05_pr ! defina el intervalo [a,b], el tiempo inicial y el paso h
    z1 = 2._pr ; z2 = 0.34_pr      ! inicie sus "guesses" z1 y z2
    npasos = int((b-a)/h) ; beta = log(2._pr)
    !
    ! CUIDADO!!!! La condicion inicial x(a) debe inicializarse cada vez que se llama a rk4
    ! por eso coloque la condicion incial en la variable x0 pues el codigo llama a x0 cada vez
    ! que sea necesario
    x0 = 0._pr
    !
    ! Calculo phi(z1) que es resolver el problema de valores iniciales con x'(a) = z1 y phi(z1) = x_s(b) - beta
    x(1) = x0 ; x(2) = z1 ; t = t0 + h
    do i = 1,npasos
        call rk4 (ndim,x,t,h,f,x_s)
        x(:) = x_s(:)
        t = t + h
    end do
    phi1 = x_s(1)
    !
    ! Ahora calculo phi(z2)
    x(1) = x0 ; x(2) = z2 ; t = t0 + h
    do i = 1,npasos
        call rk4 (ndim,x,t,h,f,x_s)
        x(:) = x_s(:)
        t = t + h
    end do
    phi2 = x_s(1)
    !
    ! Ahora me hago un do while que vaya cambiando z3 en funcion de los anteriores y evaluando el resultado hasta que x_s(b) - beta >= tol
    phi3 = 0._pr ; j = 0
    do while (abs(phi3-beta) > tol)
        j = j + 1
        z3 = z2 + (beta - phi2)*((z2 - z1)/(phi2 - phi1))
        x(1) = x0 ; x(2) = z3 ; t = t0 + h
        do i = 1,npasos
            call rk4 (ndim,x,t,h,f,x_s)
            x(:) = x_s(:)
            t = t + h
        end do
        phi3 = x_s(1)
        z1 = z2
        z2 = z3
        phi1 = phi2
        phi2 = phi3
    end do
    write(*,*) 'El z optimo es',z3
    write(*,*) 'Se realizaron',j,'pasos.'
    !
    ! Ahora con el z3 calculo la solucion que satisfaga mi condicion de borde
    open (3,file='shooting_sol.dat',action='write',status='replace')
    x(1) = x0 ; x(2) = z3 ; t = t0 + h
    do i = 1,npasos
        call rk4 (ndim,x,t,h,f,x_s)
        write(3,'(2(2X,E16.7))') t,x_s(1)
        x(:) = x_s(:)
        t = t + h
    end do
    close(3)
    !
contains
  subroutine f(tt,xx,fun)
    implicit none
    real(pr), intent(in), dimension(1:ndim) :: xx
    real(pr), intent(in) :: tt
    real(pr), intent(out), dimension(1:ndim) :: fun
    !
    ! fun(1) = xx(2)
    ! fun(2) = xx(1)
    fun(1) = xx(2)
    fun(2) = -(xx(2)**2) !+ log(tt)			! esta funcion me hizo renegar mucho
    ! fun(1) = xx(2)
    ! fun(2) = (1._pr/tt**5)*((tt*xx(2))**2 - 9._pr*xx(1)**2 + 4._pr*tt**6)
    !
  end subroutine f
end program shooting
