program random_walk
    use random_numbers
    implicit none
    integer, parameter :: pr = kind(1.d0)
    integer :: a,m,semilla,c1,c2,c3,c4
    integer :: nstep,i,xi,yi,xf,yf,dx,dy,x_f,y_f,realizaciones        !xi,yi = posicion inicial, xf,yf = pos final. dx,dy = delta de cada paso, x_f,y_f = pos final desp de la caminata
    real(pr) :: rn,r
    !
    a = 16807 ; m = 2147483647
    !
    open (3,file='walking_dead.dat',action='write',status='replace')
    open (5,file='white_walker.dat',action='write',status='replace')
    !
    c1 = 0 ; c2 = 0 ; c3 = 0 ; c4 = 0
    do realizaciones = 1,10
        do nstep = 100,10000,100
            semilla = 1
            xi = 0 ; yi = 0 ; xf = 0 ; yf = 0
            r = 0._pr
            x_f = 0 ; y_f = 0
            do i = 1,nstep
                rn = real(sch(a,m,semilla),pr)/real(m,pr)
                ! random walk
                if (rn < 0.25_pr) then          ! norte
                    yf = yi + 1
                elseif (rn > 0.25_pr .and. rn < 0.5_pr) then  ! sur
                    yf = yi - 1
                elseif (rn > 0.5_pr .and. rn < 0.75_pr) then  ! este
                    xf = xi + 1
                else                                          ! oeste
                    xf = xi - 1
                end if
                dx = xf-xi ; dy = yf-yi
                r = real(dx**2,pr) + real(dy**2,pr) + r
                x_f = dx + x_f ; y_f = dy + y_f
                ! write(5,*) xf,yf,dx,dy,r,x_f,y_f
                xi = xf ; yi = yf
            end do
            write(5,*) nstep,r
        end do
        !cuadrantes                            ! c2|c1
        if (x_f > 0 .and. y_f > 0) then        ! -----
            c1 = c1 + 1                        ! c3|c4
        elseif (x_f < 0 .and. y_f > 0) then
            c2 = c2 + 1
        elseif (x_f < 0 .and. y_f < 0) then
            c3 = c3 + 1
        else
            c4 = c4 + 1
        end if
        !
        write(3,*) realizaciones,c1,c2,c3,c4
    end do
    close(3) ; close(5)
    !
end program random_walk
