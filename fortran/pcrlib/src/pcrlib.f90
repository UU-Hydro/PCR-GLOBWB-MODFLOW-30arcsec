module pcrModule
  ! modules
  use, intrinsic :: iso_fortran_env , only: error_unit, output_unit, &
    i1b => int8, i2b => int16, i4b => int32, r4b => real32, r8b => real64
  use ieee_arithmetic 
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
    !
    integer :: i4minVal, i4maxVal
    real :: r4minVal, r4maxVal
  end type tMapHdr
  
  type tMap
    integer, pointer                      :: iu => null()
    type(tMapHdr), pointer                :: header => null()
    integer(i1b),                 pointer :: i1mv  => null()
    integer(i1b), dimension(:,:), pointer :: i1a   => null()
    integer(i2b),                 pointer :: i2mv  => null()
    integer(i2b), dimension(:,:), pointer :: i2a   => null()
    integer(i4b),                 pointer :: i4mv  => null()
    integer(i4b), dimension(:,:), pointer :: i4a   => null()
    real(r4b),                    pointer :: r4mv  => null()
    real(r4b),    dimension(:,:), pointer :: r4a   => null()
    real(r8b),                    pointer :: r8mv  => null()
    real(r8b),    dimension(:,:), pointer :: r8a   => null()
  contains
    procedure :: read_header => map_read_header
    procedure :: read_data   => map_read_data
    procedure :: set_nodata  => map_set_nodata
    procedure :: close       => map_close
    procedure :: clean       => map_clean
    procedure :: idf_export  => map_idf_export
    procedure :: get_r4ar    => map_get_r4ar
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
    call chkexist(f)
    
    ! open file in stream mode
    allocate(this%iu)
    this%iu=getlun()
    write(*,*) 'Opening '//trim(f)//'...'
    open(unit=this%iu,file=f,form='unformatted',access='stream',status='old')
    
    ! READ HEADER
    write(*,*) 'Reading header...'
    allocate(this%header)
    hdr => this%header
    
    ! main header
    read(this%iu,pos=  0+1) hdr%signature
    if (hdr%signature.ne.'RUU CROSS SYSTEM MAP FORMAT')then
       call errmsg('File not recognized as MAP-file: ')
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
    case(cr_uint2,cr_uint4,cr_undef)
       call errmsg('Unsupported cell representation for MAP-file: '//crstr(hdr%cellRepr))
    end select
    
    ! set return value
    ok = .true.
    
  end function map_read_header
  
  subroutine map_read_data(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tMap) :: this
    ! -- local
    integer(i4b) :: nc, nr, ic, ir, p
    character(len=1), dimension(:,:), allocatable :: wrk
! ------------------------------------------------------------------------------
    nc = this%header%nrCols; nr = this%header%nrRows
    p = 256+1
    
    write(*,*) 'Reading array...'
    select case(this%header%cellrepr)
      case(cr_uint1)
        allocate(this%i1a(nc,nr), wrk(nc,nr))
        read(unit=this%iu,pos=p)((wrk(ic,ir),ic=1,nc),ir=1,nr)
        do ir = 1, nr
          do ic = 1, nc
            this%i1a(ic,ir) = ichar(wrk(ic,ir))
          end do
        end do
        deallocate(wrk)
      case(cr_int1)
        allocate(this%i1a(nc,nr))
        read(unit=this%iu,pos=p)((this%i1a(ic,ir),ic=1,nc),ir=1,nr)
      case(cr_int2)
        allocate(this%i2a(nc,nr))
        read(unit=this%iu,pos=p)((this%i2a(ic,ir),ic=1,nc),ir=1,nr)
      case(cr_int4)
        allocate(this%i4a(nc,nr))
        read(unit=this%iu,pos=p)((this%i4a(ic,ir),ic=1,nc),ir=1,nr)
      case(cr_real4)
        allocate(this%r4a(nc,nr))
        read(unit=this%iu,pos=p)((this%r4a(ic,ir),ic=1,nc),ir=1,nr)
      case(cr_real8)
        allocate(this%r8a(nc,nr))
        read(unit=this%iu,pos=p)((this%r8a(ic,ir),ic=1,nc),ir=1,nr)
      case default
          call errmsg('Kind of MAP-file not supported.')
    end select 
    call this%set_nodata()
      
  end subroutine map_read_data
  
  subroutine map_set_nodata(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tMap) :: this
    ! -- local
    integer(i4b) :: nc, nr, ic, ir
! ------------------------------------------------------------------------------
    nc = this%header%nrCols; nr = this%header%nrRows
    
    select case(this%header%cellrepr)
    case(cr_uint1)
        allocate(this%i1mv)
        this%i1mv = 255
      case(cr_int1)
        allocate(this%i1mv)
        this%i1mv = -128
      case(cr_int2)
        allocate(this%i2mv)
        this%i2mv = 0
        this%i2mv = -huge(this%i2mv)-1
      case(cr_int4)
        allocate(this%i4mv)
        this%i4mv = 0
        this%i4mv = -huge(this%i4mv)-1
      case(cr_real4)
        allocate(this%r4mv)
        this%r4mv = 0
        this%r4mv = huge(this%r4mv)
        do ir = 1, nr
          do ic = 1, nc
            if (ieee_is_nan(this%r4a(ic,ir))) then
              this%r4a(ic,ir) = this%r4mv
            end if
          end do
        end do
      case(cr_real8)
        allocate(this%r8mv)
        this%r8mv = 0
        this%r8mv = huge(this%r8mv)
        do ir = 1, nr
          do ic = 1, nc
            if (ieee_is_nan(this%r8a(ic,ir))) then
              this%r8a(ic,ir) = this%r8mv
            end if
          end do
        end do
    end select 
   
  end subroutine map_set_nodata
  
  subroutine map_get_r4ar(this, r4a, r4mv, r4min, r4max)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tMap) :: this
    real(r4b), dimension(:,:), allocatable, intent(out) :: r4a
    real(r4b), intent(out) :: r4mv, r4min, r4max
    ! -- local
    integer(i4b) :: nc, nr, ic, ir
    !
    integer(i1b) :: vi1
    integer(i2b) :: vi2
    integer(i4b) :: vi4
    real(r4b)   :: vr4
    real(r8b)   :: vr8
! ------------------------------------------------------------------------------
  
    ! set nodata value
    r4min = 1e10
    r4max = -1e10
    
    ! allocate
    nc = this%header%nrCols; nr = this%header%nrRows
    if (allocated(r4a)) deallocate(r4a)
    allocate(r4a(nc,nr))
    
    ! set the array
    select case(this%header%cellrepr)
      case(cr_uint1,cr_int1)
        r4mv = int(this%i1mv,i1b)
        do ir = 1, nr
          do ic = 1, nc
            vi1 = this%i1a(ic,ir)
            r4a(ic,ir) = real(vi1,r4b)
            if (vi1 /= this%i1mv) then
              r4min = min(r4min,real(vi1,r4b))
              r4max = max(r4max,real(vi1,r4b))
            end if
          end do
        end do
      case(cr_int2)
        r4mv = int(this%i2mv,i2b)
        do ir = 1, nr
          do ic = 1, nc
            vi2 = this%i2a(ic,ir)
            r4a(ic,ir) = real(vi2,r4b)
            if (vi2 /= this%i2mv) then
              r4min = min(r4min,real(vi2,r4b))
              r4max = max(r4max,real(vi2,r4b))
            end if
          end do
        end do
      case(cr_int4)
        r4mv = int(this%i4mv,i4b)
        do ir = 1, nr
          do ic = 1, nc
            vi4 = this%i4a(ic,ir)
            r4a(ic,ir) = real(vi4,r4b)
            if (vi4 /= this%i4mv) then
              r4min = min(r4min,real(vi4,r4b))
              r4max = max(r4max,real(vi4,r4b))
            end if
          end do
        end do
      case(cr_real4)
        r4mv = real(this%r4mv,r4b)
        do ir = 1, nr
          do ic = 1, nc
            vr4 = this%r4a(ic,ir)
            r4a(ic,ir) = real(vr4,r4b)
            if (vr4 /= this%r4mv) then
              r4min = min(r4min,real(vr4,r4b))
              r4max = max(r4max,real(vr4,r4b))
            end if
          end do
        end do
      case(cr_real8)
        r4mv = real(this%r8mv,r8b)
        do ir = 1, nr
          do ic = 1, nc
            vr8 = this%r8a(ic,ir)
            r4a(ic,ir) = real(vr8,r4b)
            if (vr8 /= this%r8mv) then
              r4min = min(r4min,real(vr8,r4b))
              r4max = max(r4max,real(vr8,r4b))
            end if
          end do
        end do
      case default
          call errmsg('Kind of MAP-file not supported.')
    end select 
    
  end subroutine map_get_r4ar
  
  subroutine map_idf_export(this, f)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tMap) :: this
    character(len=*), intent(in) :: f
    ! -- local
    type(tMapHdr), pointer :: hdr
    integer(i4b) :: iu, ios, p, ic, ir
    real(r4b) :: nodata, dmin, dmax
    real(r4b), dimension(:,:), allocatable :: r4a
! ------------------------------------------------------------------------------

    ! check for equidistant grids
    hdr => this%header; p = 1
    if (hdr%cellSizeX /= hdr%cellSizeY) then
       call errmsg('Exporting IDF for non-equidistant grids is not yet supported.')
    end if
    
    ! get real data
    call this%get_r4ar(r4a, nodata, dmin, dmax)
    
    iu = getlun()
    write(*,*) 'Writing '//trim(f)//'...'
    open(unit=iu,file=f,form='unformatted',access='stream', &
      status='replace',iostat=ios)

    p = 1
    write(iu,pos=p) int(1271,i4b); p = p + 4 !1271
    write(iu,pos=p) int(hdr%nrCols,i4b); p = p + 4 !ncol
    write(iu,pos=p) int(hdr%nrRows,i4b); p = p + 4 !nrow
    write(iu,pos=p) real(hdr%xUL,r4b); p = p + 4 !xmin
    write(iu,pos=p) real(hdr%xUL+hdr%nrCols*hdr%cellSizeX,r4b); p = p + 4 !xmax
    write(iu,pos=p) real(hdr%yUL-hdr%nrRows*hdr%cellSizeY,r4b); p = p + 4 !ymin
    write(iu,pos=p) real(hdr%yUL,r4b); p = p + 4 !ymax
    write(iu,pos=p) real(dmin,r4b); p = p + 4 !dmin
    write(iu,pos=p) real(dmax,r4b); p = p + 4 !dmax
    write(iu,pos=p) real(nodata,r4b); p = p + 4 !nodata
    write(iu,pos=p) int(0,i1b); p = p + 1 !ieq
    write(iu,pos=p) int(0,i1b); p = p + 1 !itp
    write(iu,pos=p) int(0,i1b); p = p + 1 !i
    write(iu,pos=p) int(0,i1b); p = p + 1 !not used
    write(iu,pos=p) real(hdr%cellSizeX,r4b); p = p + 4 !dx
    write(iu,pos=p) real(hdr%cellSizeY,r4b); p = p + 4 !dy
    write(iu,pos=p)((r4a(ic,ir),ic=1,hdr%nrCols),ir=1,hdr%nrRows) !x
    close(iu)
    
    deallocate(r4a)
    
    close(iu)
  end subroutine map_idf_export
  
  subroutine map_close(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tMap) :: this
    ! -- local
    logical :: lop
! ------------------------------------------------------------------------------
    if (associated(this%iu)) then
      inquire(unit=this%iu,opened=lop)
      if (lop) then
        write(*,*) 'Closing map-file...'
        close(this%iu)
      end if
    end if
  end subroutine map_close

 subroutine map_clean(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tMap) :: this
    ! -- local
! ------------------------------------------------------------------------------
    ! close the file
    write(*,*) 'Cleaning map-file data structures...'
    call this%close()
    if (associated(this%iu))     deallocate(this%iu)
    if (associated(this%header)) deallocate(this%header)
    if (associated(this%i1a))    deallocate(this%i1a)
    if (associated(this%i1a))    deallocate(this%i1a)
    if (associated(this%i2a))    deallocate(this%i2a)
    if (associated(this%i4a))    deallocate(this%i4a)
    if (associated(this%r4a))    deallocate(this%r4a)
    if (associated(this%r8a))    deallocate(this%r8a)
    
  end subroutine map_clean
  
!  function getrval(this,irow,icol) result(rval)
!! ******************************************************************************
!! ******************************************************************************
!!
!!    SPECIFICATIONS:
!! ------------------------------------------------------------------------------
!  ! -- modules
!  use ieee_arithmetic 
!  ! -- dummy
!  class(tMap) :: this
!  integer,intent(in) :: irow,icol
!  ! locals
!  integer :: p, n
!  integer(kind=1) :: i1
!  integer(kind=2) :: i2
!  integer(kind=4) :: i4
!  real :: r4
!  double precision :: r8
!! ------------------------------------------------------------------------------
!  
!  p = 256+1; n = (irow-1)*idf%ncol+icol-1
!  
!  ! stream mode reading byte by byte
!  select case(idf%maphdr%cellrepr)
!  case(cr_int1)
!     read(unit=idf%iu,pos=p+n) i1
!     mapgetval = real(i1)
!  case(cr_int2)
!     read(unit=idf%iu,pos=p+2*n) i2
!     mapgetval = real(i2)
!  case(cr_int4)
!     read(unit=idf%iu,pos=p+4*n) i4
!     mapgetval = real(i4)
!  case(cr_real4)
!     read(unit=idf%iu,pos=p+4*n) r4
!     if (ieee_is_nan(r4)) then
!        mapgetval = idf%nodata    
!     else
!        mapgetval = r4
!     end if   
!  case(cr_real8)
!     read(unit=idf%iu,pos=p+8*n) r8
!     if (ieee_is_nan(r8)) then
!        mapgetval = idf%nodata    
!     else
!        mapgetval = real(r8)
!     end if   
!  end select 
!  
!  end function mapgetval
 
end module pcrModule