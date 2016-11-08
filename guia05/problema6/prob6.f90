program prob6
    use random_numbers
    implicit none
    integer, parameter :: pr = kind(1.d0)
    integer :: i,j,a,m,semilla
    real(pr) :: xi,integral,iest,exacta,err,ers
    !
    a = 16807 ; m = 2147483647 ; exacta = -exp(-1._pr) + 1
    !
    open (3,file='nvse.dat',action='write',status='replace')
    do i = 100,30000,100
        semilla = 1
        integral = 0._pr
        ! integral de MC
        do j = 1,i
            xi = real(sch(a,m,semilla),pr)/real(m,pr)
            integral = integral + (1._pr/real(i,pr))*f(xi)
        end do
        err = abs(integral-exacta)/abs(exacta)
        ! error estadistico
        iest = 0._pr
        do j = 1,i
            xi = real(sch(a,m,semilla),pr)/real(m,pr)
            iest = iest + (1._pr/real(i,pr))*f(2._pr*xi)
        end do
        ers = sqrt((iest - integral**2)/real(i,pr))
        write(3,'(I5,2(2X,E16.7))') i,err,ers
    end do
    close(3)
    !
    !
contains
    function f(xx)
        implicit none
        real(pr), intent(in) :: xx
        real(pr) :: f
        !
        f = exp(-xx)
    end function
end program prob6
