program test
  ! modules
  use mf6_module, only: tMf6, i4b
  
  implicit none
  
  type(tMf6), pointer :: ggm => null()
  integer(i4b) :: isol
  character(len=1024) :: s, f, rootdir

  call getarg(1,s); read(s,*) isol
  call getarg(2,rootdir)
  call getarg(3,f)
  
  allocate(ggm)
  call ggm%init(isol, rootdir, f)
  call ggm%write()
  deallocate(ggm)
  
end program