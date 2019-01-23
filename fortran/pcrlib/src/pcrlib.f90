module pcrlib
  ! modules
  use utils
  
  implicit none
  
  private

  integer, parameter :: cr_int1  = 1 ! 4
  integer, parameter :: cr_int2  = 2 ! 21
  integer, parameter :: cr_int4  = 3 ! 38
  integer, parameter :: ncrint = cr_int4
  integer, parameter :: cr_uint1 = 4 ! 0
  integer, parameter :: cr_uint2 = 5 ! 17
  integer, parameter :: cr_uint4 = 6 ! 34
  integer, parameter :: cr_real4 = 7 ! 90
  integer, parameter :: cr_real8 = 8 ! 219
  integer, parameter :: cr_undef = 9 ! 100
  integer, parameter :: ncr = cr_undef
  
  integer, dimension(ncr) :: crval
  data crval/4,21,38,0,17,34,90,219,100/
  
  character(len=5), dimension(ncr) :: crstr
  data crstr/'int1 ','int2 ','int4 ','uint1','uint2','uint4','real4','real8','undef'/
  
  integer(kind=4), dimension(ncrint) :: crintmv
  data crintmv/-2147483648,-256,-32768/
  
  integer, parameter :: pt_xy     = 1
  integer, parameter :: pt_utm    = 2
  integer, parameter :: pt_latlon = 3
  integer, parameter :: pt_cart   = 4
  integer, parameter :: pt_rdm    = 5
  integer, parameter :: v1npt       = pt_rdm
  
  integer, dimension(v1npt) :: v1ptval
  data v1ptval/0,1,2,3,4/
  
  character(len=4), dimension(v1npt) :: v1ptstr
  data v1ptstr/'xy    ','utm   ','latlon','cart  ','rdm   '/
  
  integer, parameter :: pt_yinct2b = 1
  integer, parameter :: pt_ydect2b = 2
  integer, parameter :: v2npt      = pt_ydect2b
  
  integer, dimension(v2npt) :: v2ptval
  data v2ptval/0,1/
  
  integer, parameter :: vs_boolean   = 1
  integer, parameter :: vs_nominal   = 2
  integer, parameter :: vs_ordinal   = 3
  integer, parameter :: vs_scalar    = 4
  integer, parameter :: vs_direction = 5
  integer, parameter :: vs_ldd       = 6
  integer, parameter :: vs_vector    = 7
  integer, parameter :: nvs = vs_vector
  
  integer, dimension(nvs) :: vsval
  data vsval/224,226,242,235,251,240,236/
  
  character(len=13), dimension(v2npt) :: v2ptstr
  data v2ptstr/'y increasing ','y decreasing'/

  type tMapHdr
    ! main header
    character(len=27) :: signature
    integer(kind=2)   :: version
    integer(kind=4)   :: gisFileId
    integer(kind=2)   :: projection
    integer(kind=4)   :: attrTable
    integer(kind=2)   :: dataType
    integer(kind=4)   :: byteOrder
    ! raster header
    integer(kind=2)  :: valueScale
    integer(kind=2)  :: cellRepr
    real(kind=8)     :: xUL
    real(kind=8)     :: yUL
    integer(kind=4)  :: nrRows
    integer(kind=4)  :: nrCols
    real(kind=8)     :: cellSizeX
    real(kind=8)     :: cellSizeY
    real(kind=8)     :: angle    
    
    integer :: i4minVal, i4maxVal
    real :: r4minVal, r4maxVal
  end type tMapHdr
  
  type tMap
    integer, pointer :: iu => null()
    type(tMapHdr), pointer :: header => null()
  contains
    procedure :: read_header => map_read_header
  end type tMap
  public :: tMap
  
contains

  function is_map_file(fname) result(ok)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    character(len=*), intent(in) :: fname
    logical :: ok
    ! -- local
    integer :: ios, iu
    character(len=27) :: signature
! ------------------------------------------------------------------------------
    
    ok = .false.
    
     ! open file in stream mode
    iu=getlun()
    open(unit=iu,file=fname,form='unformatted',access='stream',status='old',iostat=ios)
    if (ios.ne.0) return
    
    ! READ HEADER
    
    ! main header
    read(iu,pos=  0+1, iostat=ios) signature
    if (ios /= 0) then
       close(iu)
       return
    end if    
    if (signature == 'RUU CROSS SYSTEM MAP FORMAT') ok = .true.
    close(iu)
    
  end function is_map_file
  
  function map_read_header(this, f) result(ok)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tMap) :: this
    character(len=*), intent(in) :: f
    logical :: ok
    ! -- local
    integer :: i, iinc
    real, parameter :: nodata = -9999. ! should be exactly the same as in rdrsmodule !
    type(tMapHdr), pointer :: hdr
! ------------------------------------------------------------------------------
    
    ! open file in stream mode
    allocate(this%iu)
    this%iu=getlun()
    open(unit=this%iu,file=f,form='unformatted',access='stream',status='old')
    
    ! READ HEADER
    allocate(this%header)
    hdr => this%header
    
    ! main header
    read(this%iu,pos=  0+1) hdr%signature
    if (hdr%signature.ne.'RUU CROSS SYSTEM MAP FORMAT')then
       call error('File not recognized as MAP-file: ')
    end if   
    read(this%iu,pos= 32+1) hdr%version
    read(this%iu,pos= 34+1) hdr%gisFileId
    read(this%iu,pos= 38+1) hdr%projection
    iinc = 1
    if (hdr%version.eq.1) then
      do i = 1, v1npt
         if(v1ptval(i).eq.hdr%projection)then
            !write(*,*) 'Version 1 projection: ',trim(v1ptstr(i))
         end if  
      end do   
    else
      do i = 1, v2npt
         if(v2ptval(i).eq.hdr%projection)then
            !write(*,*) 'Version 2 projection: ',trim(v2ptstr(i))
         end if  
      end do   
      if (hdr%projection.eq.v2ptval(pt_yinct2b)) iinc = -1
    end if    
    read(this%iu,pos= 40+1) hdr%attrTable
    read(this%iu,pos= 44+1) hdr%dataType
    read(this%iu,pos= 46+1) hdr%byteOrder
    ! raster header
    read(this%iu,pos= 64+1) hdr%valueScale
    read(this%iu,pos= 66+1) hdr%cellRepr
    do i = 1, ncr
       if(crval(i).eq.hdr%cellRepr)then
          !write(*,*) 'Cell representation: ',trim(crstr(i))
          hdr%cellRepr=i
          exit
       end if   
    end do
    select case(hdr%cellRepr)
    case(cr_int4)
       read(this%iu,pos= 68+1) hdr%i4minVal
       read(this%iu,pos= 76+1) hdr%i4maxVal
    case(cr_real4)
       read(this%iu,pos= 68+1) hdr%r4minVal
       read(this%iu,pos= 76+1) hdr%r4maxVal
    end select
    read(this%iu,pos= 84+1) hdr%xUL
    read(this%iu,pos= 92+1) hdr%yUL
    read(this%iu,pos=100+1) hdr%nrRows
    read(this%iu,pos=104+1) hdr%nrCols
    read(this%iu,pos=108+1) hdr%cellSizeX
    read(this%iu,pos=116+1) hdr%cellSizeY
    read(this%iu,pos=124 +1) hdr%angle
    
    ! checks
    select case(hdr%cellRepr)
    case(cr_uint1,cr_uint2,cr_uint4,cr_undef)
       call error('Unsupported cell representation for MAP-file: '//crstr(hdr%cellRepr))
    end select
    
    ! set return value
    ok = .true.
    
  end function map_read_header
  
  function getrval(this,irow,icol) result(rval)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
  ! -- modules
  use ieee_arithmetic 
  ! -- dummy
  class(tMap) :: this
  integer,intent(in) :: irow,icol
  ! locals
  integer :: p, n
  integer(kind=1) :: i1
  integer(kind=2) :: i2
  integer(kind=4) :: i4
  real :: r4
  double precision :: r8
! ------------------------------------------------------------------------------
  
  p = 256+1; n = (irow-1)*idf%ncol+icol-1
  
  ! stream mode reading byte by byte
  select case(idf%maphdr%cellrepr)
  case(cr_int1)
     read(unit=idf%iu,pos=p+n) i1
     mapgetval = real(i1)
  case(cr_int2)
     read(unit=idf%iu,pos=p+2*n) i2
     mapgetval = real(i2)
  case(cr_int4)
     read(unit=idf%iu,pos=p+4*n) i4
     mapgetval = real(i4)
  case(cr_real4)
     read(unit=idf%iu,pos=p+4*n) r4
     if (ieee_is_nan(r4)) then
        mapgetval = idf%nodata    
     else
        mapgetval = r4
     end if   
  case(cr_real8)
     read(unit=idf%iu,pos=p+8*n) r8
     if (ieee_is_nan(r8)) then
        mapgetval = idf%nodata    
     else
        mapgetval = real(r8)
     end if   
  end select 
  
  end function mapgetval
 
end module
