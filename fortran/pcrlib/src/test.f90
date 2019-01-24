program test
  ! modules
  use pcrModule
  
  implicit none
  
  type(tMap), pointer :: map
  logical :: ok
  character(len=1024) :: f

  call getarg(1,f)
  
  allocate(map)
  ok = map%read_header(trim(f)//'.map')
  call map%read_data()
  call map%idf_export(trim(f)//'.idf')
  call map%clean()
  deallocate(map)
  
end program