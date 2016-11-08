! ============================================================= !
!   Este programa genera numeros random usando el metodo de     !
!   Schrage que permite tomar m = (2^31) - 1 sin dar overflow   !
!   y uso la funcion intrinseca modulo(x,y) que es lo que dio   !
!   el profe en el teorico, y se puede ver como funciona en el  !
!   programa schrage_bruto.f90                                  !
! ============================================================= !
!!!!!!!!!!!!!!!!!!!!
!! NUMEROS RANDOM !!
!! REALES ENTRE   !!
!! [0,1]          !!
!!!!!!!!!!!!!!!!!!!!
program sch
    implicit none
    integer, parameter :: pr = kind(1.d0)
    integer(8) :: a,seed,m
    integer(8) :: i
    real(pr) :: rn1,rn2,x,y
    !
    a = 14801 ; m = (2**30) - 1 ; seed = 1
    open (3,file='inter_req.dat',action='write',status='replace')
    open (5,file='inter_tot.dat',action='write',status='replace')
    do i = 1,100000
        rn1 = real(j_1(seed),pr)/real(m,pr)
        rn2 = real(j_1(seed),pr)/real(m,pr)
        if (0.1_pr < rn1 .and. rn1 < 0.13_pr) x = rn1
        if (0.1_pr < rn2 .and. rn2 < 0.13_pr) y = rn2
        write(3,'(2(2X,E16.7))') x,y
        ! write(3,*) rn1,rn2
        ! if (0.1_pr < rn1 .and. rn1 < 0.13_pr .and. 0.1_pr < rn2 .and. rn2 < 0.13_pr) then
            !  write(3,'(E16.7)') rn1,rn2
        ! elseif (rn1 == 0.1_pr .or. rn1 == 0.13_pr .or. rn2 == 0.1_pr .or. rn2 == 0.13_pr) then
            !   write(3,'(E16.7)') rn1,rn2
        ! end if
        ! if (0._pr < rn1 .or. rn1 < 0.1_pr .or. 0.13_pr < rn1 .or. rn1 < 1._pr) write(5,'(E16.7)') rn1,rn2
    end do
    close(3) ; close(5)
    !
contains
    function j_1(semilla)
        implicit none
        integer(8) :: alea,j_1
        integer(8), intent(inout) :: semilla
        !
        alea = modulo(a*semilla,m)
        if (alea > 0 .or. alea == 0) then
            j_1 = alea
        else
            j_1 = alea + m
        end if
        semilla = alea
    end function j_1
end program sch
