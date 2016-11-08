   program fnv
   implicit none
   integer, parameter :: pr=Kind(1.d0)
   integer, parameter :: n=2
   real(pr)           :: x(n)
   
  interface
   function f(xx)
    integer, parameter :: prs=Kind(1.d0)
    real(prs), intent(in)  :: xx(:)
    real(prs)              :: f(size(xx))
   end function f
 end interface
   x(1) = 2._pr
   x(2) = 1._pr

   write(*,*) x
   write(*,*) f(x)


  end program

  function f(xx)
  implicit none
  integer, parameter    :: pr=Kind(1.d0)
  real(pr), intent(in)  :: xx(:)
  real(pr)              :: f(size(xx))

  f(:) = xx(:)*2._pr
  end function

!  end program