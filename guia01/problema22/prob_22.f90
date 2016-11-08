program prob_22
implicit none
integer :: i
character(len=3) :: password,pw
!
pw = 'asd'
!
i = 0
do while (i < 3)
  Write(*,*) 'Ingrese la contraseña de 3 digitos'
  read(*,'(A3)') password
    if (password == pw) then
      write(*,*) 'Correcto'
      stop
    else 
      write(*,*) 'Incorrecto'
    end if
  i = i + 1
end do
!
end program prob_22