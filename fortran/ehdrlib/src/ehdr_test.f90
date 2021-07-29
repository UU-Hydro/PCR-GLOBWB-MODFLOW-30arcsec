program ehdr_test
  use utilsmod, only: i4b, r4b, r8b, mxslen, tNum
  use ehdrModule, only: tEhdr
  !
  implicit none
  !
  type(tEhdr), pointer :: ehdr => null()
  character(len=mxslen) :: fp, s
  real(r4b) :: mv
  real(r4b), dimension(:,:), pointer :: x
  real(r8b) :: xll, yll, cs
! ******************************************************************************  
  !
  fp = 's35e030_catch'
  !
  allocate(ehdr)
  call ehdr%read_grid(fp, x, mv, xll, yll, cs)
  !

end
