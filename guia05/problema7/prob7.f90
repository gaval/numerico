! ============================================================================ !
!   Calculo el volumen sin usar integrales. Genero n numeros aleatorios  y me  !
!   fijo cuantos caen dentro de los limites para x,y,z en el primer cuadrante  !
!   llamese nv la cantidad de numeros que caen ahi dentro. Luego el volumen    !
!   es vol = 8*(nv/n).                                                         !
!                                                                              !
!   Para hacerlo con integrales tendria que hacer la suma de montecarlo        !
!   evaluando x**2 + y**2 + z**2 en numeros aleatorios y multiplicando por     !
!   el largo de cada intervalo de integracion (b-a)(c-d)(e-f)/n                !
! ============================================================================ !
program prob7
    use random_numbers
    implicit none
    integer, parameter :: pr = kind(1.d0)
    integer :: a,m,seed,i,a1,a2,a3,nv,n
    real(pr) :: x,y,z,vol
    !
    a = 16807 ; m = 2147483647 ; seed = 1 ; n = 1000000
    !
    a1 = 0 ; a2 = 0 ; a3 = 0
    do i =1,n
        x = real(sch(a,m,seed),pr)/real(m,pr)
        y = real(sch(a,m,seed),pr)/real(m,pr)
        z = real(sch(a,m,seed),pr)/real(m,pr)
        if (0._pr < x .and. x < 0.5_pr) a1 = a1 + 1
        if (0._pr < y .and. y < sqrt(0.5_pr+x**2)) a2 = a2 + 1
        if (0._pr < z .and. z < 1._pr/sqrt(2._pr)) a3 = a3 + 1
        nv = a1 + a2 + a3
    end do
    !
    vol = 8._pr*real(nv,pr)/real(n,pr)
    write(*,*) vol
end program prob7
