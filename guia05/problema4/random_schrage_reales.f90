! ============================================================= !
!   Este programa genera numeros random usando el metodo de     !
!   Schrage que permite tomar m = (2^31) - 1 sin dar overflow   !
!   y uso la funcion intrinseca modulo(x,y) que es lo que dio   !
!   el profe en el teorico, y se puede ver como funciona en el  !
!   programa schrage_bruto.f90                                  !
! ============================================================= !
!!!!!!!!!!!!!!!!!!!!
!! NUMEROS RANDOM !!
!! REALES ENTRE   !!                  con subrutina no pude generar pares de puntos
!! [0,1]          !!
!!!!!!!!!!!!!!!!!!!!
program sch
    implicit none
    integer, parameter :: pr = kind(1.d0)
    integer :: a,seed,m,j_1             ! j_1 = ji+1
    integer :: i
    real(pr) :: rn
    !
    a = 3 ; m = 7 ; seed = 1
    do i = 1,7
        call random (a,seed,m,j_1)
        rn = real(j_1,pr)/real(m,pr)
        write(*,*) rn
        seed = j_1
    end do
    !
contains
    subroutine random (aa,semilla,mmm,j_1)
        implicit none
        integer :: alea
        integer, intent(in) :: aa,semilla,mm
        integer, intent(out) :: jj
        !
        alea = modulo(aa*semilla,mm)
        if (alea > 0 .or. alea == 0) then
            jj = alea
        else
            jj = alea + mm
        end if
    end subroutine random
end program sch
