program test
  ! modules
  use pcrlib
  
  implicit none
  
  type(tMap), pointer :: map
  logical :: ok
  
  allocate(map)
  ok = map%read_header('g:\models\pcr-globwb-30arcsec\catchment_merged_ldd.scalar.map')
  
  
end program