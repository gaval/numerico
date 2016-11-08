PROGRAM prob_5
IMPLICIT NONE
!
INTEGER(2) :: a,b,h ! Integer(2) hace que me devuelvan numeros enteros de 2 bytes. Puse INTEGER(4) porque
!                     por alguna razon si pongo INTEGER(2) cuando compilo me dice
!		      Error: Arithmetic overflow converting INTEGER(4) to INTEGER(2)
!
!
a = 35767 + 1
WRITE(*,*) 'Inciso a =',a
!
!
b = 30000*2
WRITE(*,*) 'Inciso b =',b
!
!
h = -30000 - 10000
WRITE(*,*) 'Inciso c =',h
!
!
END PROGRAM prob_5