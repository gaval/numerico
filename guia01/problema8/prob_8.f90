PROGRAM test
IMPLICIT NONE
REAL(kind=4) :: sum0,sum1
INTEGER(kind=4) :: i
!
sum0 = 0. ;sum1 = 1.
DO i=1,10000
  sum0 = sum0 + 1.e-8
  sum1 = sum1 + 1.e-8
END DO
sum0 = sum0 + 1.
!-----------------------------------------------------------------------------
! El problema debe ser una cuestión con la presición usada.
!-----------------------------------------------------------------------------
WRITE(*,*) sum0,sum1
END PROGRAM test
