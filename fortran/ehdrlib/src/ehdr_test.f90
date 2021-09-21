program ehdr_test
  use utilsmod, only: i4b, r4b, r8b, mxslen, tNum
  use ehdrModule, only: tEhdr
  !
  implicit none
  !
  type(tEhdr), pointer :: ehdr => null()
  character(len=mxslen) :: fp, s
  integer(i4b) :: nc, nr
  real(r4b) :: mv
  real(r4b), dimension(:,:), allocatable :: x
  real(r8b) :: xll, yll, cs
! ******************************************************************************  
  !
  fp = 's35e030_catch'
  write(*,*) (mod(1.d0,1.d0).eq.0.d0)
  !
  ! read and scale
  nc = 10; nr = 20; allocate(x(nc,nr))
  mv = 0.d0; xll = -2.d0; yll = 5000.d0; cs = 4.d0
  !
  allocate(ehdr)
  call ehdr%read_grid(fp, x, mv, xll, yll, cs)
  !

end
