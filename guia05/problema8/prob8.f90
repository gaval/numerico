program prob8
    use random_numbers
    integer, parameter :: pr = kind(1.d0)
    integer :: i,n,j,k,a,m,semilla
    real(pr) :: integral,f
    real(pr), dimension(1:20) :: x
    !
    a = 16807 ; m = 2147483647
    !
    do i = 1,20
        semilla = 1 ; integral = 0._pr
        n = 2**i
        ! integral de MC
        do j = 1,n
            do k = 1,20
                x(k) = real(sch(a,m,semilla),pr)/real(m,pr)
            end do
            call func (x,f)
            integral = integral + (1._pr/real(n,pr))*f
        end do
        write(*,*) integral
    end do
    !
contains
    subroutine func (xx,ff)
        implicit none
        integer :: i
        real(pr), intent(in), dimension(1:20) :: xx
        real(pr), intent(out) :: ff
        real(pr) :: g
        !
        g = 0._pr
        do i = 1,20
            g = xx(i) + g
        end do
        ff = g**2
    end subroutine func
end program prob8
