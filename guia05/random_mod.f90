module random_numbers
    ! ============================================================================= !
    !   Este modulo contiene el generador de numeros enteros aleatorios con el      !
    !   algoritmo de congruencia y el de Schrage.                                   !
    !   Entrada alg. congruencia: a,m,b,seed                                        !
    !   Entrada alg. Schrage :: a,m,seed                                            !
    !   Ambos son funciones asi que la salida es al evaluar dicha funcion           !
    ! ============================================================================= !
contains
    ! Congruencia
    function cng (aa,bb,mm,seed)
        implicit none
        integer, intent(in) :: aa,bb,mm
        integer, intent(inout) :: seed
        integer :: cng
        !
        cng = mod(aa*seed+bb,mm)
        seed = cng
    end function cng
    !
    ! Schrage
    function sch (aa,mm,seed)
        implicit none
        integer, intent(in) :: aa,mm
        integer, intent(inout) :: seed
        integer :: sch,alea
        !
        alea = modulo(aa*seed,mm)
        if (alea > 0 .or. alea == 0) then
            sch = alea
        else
            sch = alea + mm
        end if
        seed = alea
    end function sch
end module random_numbers
