! ============================================================= !
!   algoritmo de schrage a lo bruto y la salida es un entero    !
! ============================================================= !
program schrage
    implicit none
    integer :: a,seed,m,r,q,j_1             ! j_1 = ji+1
    integer :: i
    !
    a = 3 ; m = 17 ; seed = 1
    q = int(m/a) ; r = mod(m,a)
    !
    ! open (3,file='random_sch.dat',action='write',status='replace')
    do i = 1, 18
        call aleatorio (a,seed,m,r,q,j_1)
        write(*,*) j_1
        ! write(3,*) j_1                                            ! se descomenta el open file y este write, se le da el formato deseado y la salida
        seed = j_1                                                  ! queda en un .dat
    end do
    ! close(3)                                                      ! esto tmb se debe descomentar
    !
contains
    subroutine aleatorio (aa,semilla,mm,rr,qq,jj)
        implicit none
        integer :: random
        integer, intent(in) :: aa,semilla,mm,rr,qq
        integer, intent(out) :: jj
        !
        random = aa*mod(semilla,qq) - rr*int(semilla/qq)
        if (random > 0 .or. random == 0) then
            jj = random
        else
            jj = random + m
        end if
    end subroutine aleatorio
end program schrage
