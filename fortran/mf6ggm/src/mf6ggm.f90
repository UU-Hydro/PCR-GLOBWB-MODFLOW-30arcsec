program test
  ! modules
  use mf6_module, only: tMf6
  
  implicit none
  
  type(tMf6), pointer :: ggm => null()
  character(len=1024) :: f

  call getarg(1,f)
  
  allocate(ggm)
  call ggm%init(f, 49) !australia
  call ggm%exchange_init() !australia
!  call ggm%init(f, 9)
  deallocate(ggm)
  
end program