module ehdrModule
  ! modules
  use utilsmod, only: i1b, i2b, i4b, i8b, r4b, r8b, i_i1, i_i2, i_i4, i_r4, i_r8, &
    mxslen, change_case, logmsg, errmsg, tNum, open_file, parse_line, ta
  !
  implicit none
  !
  private
  !
  integer(i4b), parameter :: i_byteorder     =  1
  integer(i4b), parameter :: i_layout        =  2
  integer(i4b), parameter :: i_nrows         =  3 
  integer(i4b), parameter :: i_ncols         =  4
  integer(i4b), parameter :: i_nbands        =  5
  integer(i4b), parameter :: i_nbits         =  6
  integer(i4b), parameter :: i_bandrowbytes  =  7
  integer(i4b), parameter :: i_totalrowbytes =  8
  integer(i4b), parameter :: i_pixeltype     =  9
  integer(i4b), parameter :: i_ulxmap        = 10
  integer(i4b), parameter :: i_ulymap        = 11
  integer(i4b), parameter :: i_xllcorner     = 12
  integer(i4b), parameter :: i_yllcorner     = 13
  integer(i4b), parameter :: i_xdim          = 14
  integer(i4b), parameter :: i_ydim          = 15
  integer(i4b), parameter :: i_cellsize      = 16
  integer(i4b), parameter :: i_nodata        = 17
  integer(i4b), parameter :: i_nodata_value  = 18
  integer(i4b), parameter :: nkey = i_nodata_value
  
  character(len=13), dimension(nkey) :: keys
  !          1234567890123             
  data keys/'byteorder    ', & !01
            'layout       ', & !02
            'nrows        ', & !03
            'ncols        ', & !04
            'nbands       ', & !05
            'nbits        ', & !06
            'bandrowbytes ', & !07
            'totalrowbytes', & !08
            'pixeltype    ', & !09
            'lxmap        ', & !10
            'lymap        ', & !11
            'xllcorner    ', & !12
            'yllcorner    ', & !13
            'xdim         ', & !14
            'ydim         ', & !15
            'cellsize     ', & !16
            'nodata       ', & !17
            'nodata_value '/
  
  type tEhdrHdr
    integer(i4b) :: ncol = 0
    integer(i4b) :: nrow = 0
    integer(i4b) :: nbits = 0
    character(len=mxslen) :: pixeltype = ''
    !
    real(r4b), pointer :: xllr4 => null()
    real(r4b), pointer :: yllr4 => null()
    real(r4b), pointer :: csr4  => null()
    !
    real(r8b), pointer :: xllr8 => null()
    real(r8b), pointer :: yllr8 => null()
    real(r8b), pointer :: csr8  => null()
    !
    integer(i1b), pointer :: mvi1 => null()
    integer(i2b), pointer :: mvi2 => null()
    integer(i4b), pointer :: mvi4 => null()
    integer(r4b), pointer :: mvr4 => null()
    integer(r8b), pointer :: mvr8 => null()
  contains
    procedure :: init  => ehdrhdr_init
    procedure :: clean => ehdrhdr_clean
    procedure :: read  => ehdrhdr_read
    !procedure :: write => ehdrhdr_write
  end type tEhdrHdr
  
  type tEhdr
    type(tEhdrHdr), pointer :: hdr => null()
    integer(i4b) :: iu_flt = 0
    integer(i4b) :: i_type = 0
    integer(i1b), dimension(:,:), pointer :: xi1 => null()
    integer(i2b), dimension(:,:), pointer :: xi2 => null()
    integer(i4b), dimension(:,:), pointer :: xi4 => null()
    real(r4b),    dimension(:,:), pointer :: xr4 => null()
  contains
 !   generic   :: read_hdr => ehdr_read_grid_hdr
    procedure :: init     => ehdr_init
    procedure :: clean    => ehdr_clean
    procedure :: ehdr_set_i_type
    procedure :: ehdr_read_grid
    !
    generic   :: read_grid => ehdr_read_grid_i1_r4, ehdr_read_grid_i1_r8, &
                              ehdr_read_grid_i2_r4, ehdr_read_grid_i2_r8, &
                              ehdr_read_grid_i4_r4, ehdr_read_grid_i4_r8, &
                              ehdr_read_grid_r4_r4, ehdr_read_grid_r4_r8, &
                              ehdr_read_grid_r8_r4, ehdr_read_grid_r8_r8
    procedure :: ehdr_read_grid_i1_r4, ehdr_read_grid_i1_r8
    procedure :: ehdr_read_grid_i2_r4, ehdr_read_grid_i2_r8
    procedure :: ehdr_read_grid_i4_r4, ehdr_read_grid_i4_r8
    procedure :: ehdr_read_grid_r4_r4, ehdr_read_grid_r4_r8
    procedure :: ehdr_read_grid_r8_r4, ehdr_read_grid_r8_r8
    procedure :: ehdr_read_grid_i1, ehdr_read_grid_i2, ehdr_read_grid_i4
    procedure :: ehdr_read_grid_r4, ehdr_read_grid_r8
    !
    procedure :: ehdr_read_val_i1, ehdr_read_val_i2, ehdr_read_val_i4, ehdr_read_val_r4
    !
  end type tEhdr
  public tEhdr
  !
 interface writeflt
    module procedure :: writeflt_i1_r4
    module procedure :: writeflt_i4_r4
    module procedure :: writeflt_r4_r4
    module procedure :: writeflt_i1_r8
    module procedure :: writeflt_i4_r8
    module procedure :: writeflt_r4_r8
    module procedure :: writeflt_r8_r8
  end interface
  private :: writeflt_i1_r4, writeflt_i4_r4, writeflt_r4_r4
  private :: writeflt_i1_r8, writeflt_i4_r8, writeflt_r4_r8, writeflt_r8_r8
  !
  ! work arrays
  integer(i1b), dimension(:,:), pointer :: i1wk2d => null()
  integer(i2b), dimension(:,:), pointer :: i2wk2d => null()
  integer(i4b), dimension(:,:), pointer :: i4wk2d => null()
  integer(r4b), dimension(:,:), pointer :: r4wk2d => null()
  !
  contains
  
! ==============================================================================
! ==============================================================================
! tEhdrHdr
! ==============================================================================
! ==============================================================================
 subroutine ehdrhdr_init(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tEhdrHdr) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    this%ncol = 0
    this%nrow = 0
    this%nbits = 0
    this%pixeltype = ''
    !
    this%xllr4 => null()
    this%yllr4 => null()
    this%csr4  => null()
    !
    this%xllr8 => null()
    this%yllr8 => null()
    this%csr8  => null()
    !
    this%mvi1 => null()
    this%mvi4 => null()
    this%mvr4 => null()
    this%mvr8 => null()
    !
    return
 end subroutine ehdrhdr_init
  
subroutine ehdrhdr_clean(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tEhdrHdr) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    if (associated(this%xllr4)) deallocate(this%xllr4)
    if (associated(this%yllr4)) deallocate(this%yllr4)
    if (associated(this%csr4)) deallocate(this%csr4)
    !
    if (associated(this%xllr8)) deallocate(this%xllr8)
    if (associated(this%yllr8)) deallocate(this%yllr8)
    if (associated(this%csr8)) deallocate(this%csr8)
    !
    if (associated(this%mvi1)) deallocate(this%mvi1)
    if (associated(this%mvi4)) deallocate(this%mvi4)
    if (associated(this%mvr4)) deallocate(this%mvr4)
    if (associated(this%mvr8)) deallocate(this%mvr8)
    !
    call this%init()
    !
    return
  end subroutine ehdrhdr_clean
  
  subroutine ehdrhdr_read(this, fp)
! ******************************************************************************
    ! -- arguments
    class(tEhdrHdr) :: this
    character(len=*), intent(in) :: fp
    ! -- locals
    integer(i4b) :: iu
    character(len=mxslen) :: f, s, k, v
    character(len=mxslen), dimension(:), allocatable :: sa
    integer(i4b) :: ios, nrewind, nfound
    integer(i4b), dimension(nkey) :: flag
    !
    type(tNum) :: cs, nodata, xll, yll, xcul, ycul
! ------------------------------------------------------------------------------
    !
    f = trim(fp)//'.hdr'
    call open_file(f, iu, 'r')
    !
    flag = 0
    nrewind = 0
    do while(.true.)
      read(unit=iu,iostat=ios,fmt='(a)') s
      if (ios == 0) then
        call parse_line(s, sa)
        k = change_case(sa(1), 'l')
        v = sa(2)
        select case(k)
        case('ncols')
          read(v,*) this%ncol; flag(i_ncols) = 1
        case('nrows')
          read(v,*) this%nrow; flag(i_nrows) = 1
        case('xllcorner')
          call xll%read(v); flag(i_xllcorner) = 1; flag(i_ulxmap) = 1
        case('ulxmap') ! center
          call xcul%read(v); flag(i_xllcorner) = 1; flag(i_ulxmap) = 1
        case('yllcorner')
          call yll%read(v); flag(i_yllcorner) = 1; flag(i_ulymap) = 1
        case('ulymap') ! center
          call ycul%read(v); flag(i_yllcorner) = 1; flag(i_ulymap) = 1
        case('nodata','nodata_value')
          call nodata%read(v); flag(i_nodata) = 1; flag(i_nodata_value) = 1
        case('cellsize','xdim', 'ydim')
          call cs%read(v); flag(i_cellsize) = 1; flag(i_xdim) = 1; flag(i_ydim) = 1
        case('nbits')
          read(v,*) this%nbits; flag(i_nbits) = 1
        case('pixeltype')
          read(v,*) this%pixeltype; flag(i_pixeltype) = 1
          this%pixeltype = change_case(this%pixeltype, 'l')
        end select
      end if
      !
      nfound = sum(flag)
      if (nfound == nkey) exit
      if (ios /= 0) then
        if (nrewind < nfound) then
          nrewind = nrewind + 1
          rewind(iu)
        else
          if (nfound == nrewind) then
            exit
          end if
        end if
      end if
    end do
    close(iu)
    !
    ! r4 coordinates
    if (cs%flg(i_r4)) then
      allocate(this%csr4)
      this%csr4 = cs%r4v
    else
      call logmsg('Warning, could not set cs in r4 precision.')
    end if
    if (xll%flg(i_r4)) then
      allocate(this%xllr4)
      this%xllr4 = xll%r4v
    end if
    if (xcul%flg(i_r4).and.associated(this%csr4)) then
      allocate(this%xllr4)
!      xll = xcul - cs/2.d0
      this%xllr4 = xcul%r4v - this%csr4 / 2.
    end if
    if (yll%flg(i_r4)) then
      allocate(this%yllr4)
      this%yllr4 = yll%r4v
    end if
    if (ycul%flg(i_r4).and.associated(this%csr4)) then
      allocate(this%yllr4)
!      yll = ycul - cs*this%nrow + cs/2.d0
      this%yllr4 = ycul%r4v - this%csr4*this%nrow + this%csr4/2.
    end if
    !
    ! r8 coordinates
    if (cs%flg(i_r8)) then
      allocate(this%csr8)
      this%csr8 = cs%r8v
    else
      call logmsg('Warning, could not set cs in r8 precision.')
    end if
    if (xll%flg(i_r8)) then
      allocate(this%xllr8)
      this%xllr8 = xll%r8v
    end if
    if (xcul%flg(i_r8).and.associated(this%csr8)) then
      allocate(this%xllr8)
      this%xllr8 = xcul%r8v - this%csr8 / 2.d0
    end if
    if (yll%flg(i_r8)) then
      allocate(this%yllr8)
      this%yllr8 = yll%r8v
    end if
    if (ycul%flg(i_r8).and.associated(this%csr8)) then
      allocate(this%yllr8)
      this%yllr8 = ycul%r8v - this%csr8*this%nrow + this%csr8/2.d0
    end if
    !
    if (nodata%flg(i_i1)) then
      allocate(this%mvi1); this%mvi1 = nodata%i1v
    end if
    if (nodata%flg(i_i2)) then
      allocate(this%mvi2); this%mvi2 = nodata%i2v
    end if
    if (nodata%flg(i_i4)) then
      allocate(this%mvi4); this%mvi4 = nodata%i4v
    end if
    if (nodata%flg(i_r4)) then
      allocate(this%mvr4); this%mvr4 = nodata%r4v
    end if
    if (nodata%flg(i_r8)) then
      allocate(this%mvr8); this%mvr8 = nodata%r8v
    end if
    !
    return
  end subroutine ehdrhdr_read
  
  subroutine ehdrhdr_set(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tEhdrHdr) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    return
  end subroutine ehdrhdr_set
  
! ==============================================================================
! ==============================================================================
! tEhdr
! ==============================================================================
! ==============================================================================
 
  subroutine ehdr_init(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tEhdr) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    this%hdr => null()
    this%iu_flt = 0
    this%i_type = 0
    this%xi1 => null()
    this%xi2 => null()
    this%xi4 => null()
    this%xr4 => null()
    !
    return
  end subroutine ehdr_init

  subroutine ehdr_clean(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tEhdr) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    if (associated(this%hdr)) deallocate(this%hdr)
    if (associated(this%xi1)) deallocate(this%xi1)
    if (associated(this%xi2)) deallocate(this%xi2)
    if (associated(this%xi4)) deallocate(this%xi4)
    if (associated(this%xr4)) deallocate(this%xr4)
    !
    call this%init()
    !
    return
  end subroutine ehdr_clean

  subroutine ehdr_set_i_type(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tEhdr) :: this
    ! -- local
! ------------------------------------------------------------------------------
    select case(this%hdr%pixeltype)
    case('signedint')
       select case(this%hdr%nbits)
       case(8)
         this%i_type = i_i1
       case(16)
         this%i_type = i_i2
       case(32)
         this%i_type = i_i4
       case default
         call errmsg('Error, unsupported nbits for signedint.')
       end select
    case('float')
       select case(this%hdr%nbits)
       case(32)
         this%i_type = i_r4
       case default
         call errmsg('Error, unsupported nbits for float.')
       end select
    case default
      call errmsg('Error, not supported pixeltype.')
    end select
    !
    return
  end subroutine ehdr_set_i_type
  
  subroutine ehdr_read_grid(this, fp)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tEhdr) :: this
    character(len=*), intent(inout) :: fp
    ! -- local
    character(len=mxslen) :: f
    integer(i4b) :: ncol, nrow, iu, icol, irow
! ------------------------------------------------------------------------------
    !
    allocate(this%hdr)
    call this%hdr%init()
    call this%hdr%read(fp)
    !
    ncol = this%hdr%ncol
    nrow = this%hdr%nrow
    !
    f = trim(fp)//'.flt'
    call open_file(f, iu, 'r', .true.)
    !
    call this%ehdr_set_i_type()
    select case(this%i_type)
    case(i_i1)
      allocate(this%xi1(ncol,nrow))
      read(iu)((this%xi1(icol,irow),icol=1,ncol),irow=1,nrow); close(iu)
    case(i_i2)
      allocate(this%xi2(ncol,nrow))
      read(iu)((this%xi2(icol,irow),icol=1,ncol),irow=1,nrow); close(iu)
    case(i_i4)
      allocate(this%xi4(ncol,nrow))
      read(iu)((this%xi4(icol,irow),icol=1,ncol),irow=1,nrow); close(iu)
    case(i_r4)
      allocate(this%xr4(ncol,nrow))
      read(iu)((this%xr4(icol,irow),icol=1,ncol),irow=1,nrow); close(iu)
    end select
    !
    return
  end subroutine ehdr_read_grid

  subroutine ehdr_read_grid_i1_r4(this, fp, x, mv, xll, yll, cs)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tEhdr) :: this
    character(len=*), intent(inout) :: fp
    integer(i1b), dimension(:,:), intent(out), pointer :: x
    integer(i1b), intent(out) :: mv
    real(r4b), intent(out) :: xll, yll, cs
    ! -- local
    type(tEhdrHdr), pointer :: hdr
! ------------------------------------------------------------------------------
    !
    call this%ehdr_read_grid(fp)
    hdr => this%hdr
    !
    mv = hdr%mvi1; xll = hdr%xllr4; yll = hdr%yllr4; cs = hdr%csr4
    call this%ehdr_read_grid_i1(x, hdr)
    call this%clean()
    !
    return
  end subroutine ehdr_read_grid_i1_r4

  subroutine ehdr_read_grid_i1_r8(this, fp, x, mv, xll, yll, cs)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tEhdr) :: this
    character(len=*), intent(inout) :: fp
    integer(i1b), dimension(:,:), intent(out), pointer :: x
    integer(i1b), intent(out) :: mv
    real(r8b), intent(out) :: xll, yll, cs
    ! -- local
    type(tEhdrHdr), pointer :: hdr
! ------------------------------------------------------------------------------
    !
    call this%ehdr_read_grid(fp)
    hdr => this%hdr
    !
    mv = hdr%mvi1; xll = hdr%xllr8; yll = hdr%yllr8; cs = hdr%csr8
    call this%ehdr_read_grid_i1(x, hdr)
    call this%clean()
    !
    return
  end subroutine ehdr_read_grid_i1_r8
  
  subroutine ehdr_read_grid_i2_r4(this, fp, x, mv, xll, yll, cs)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tEhdr) :: this
    character(len=*), intent(inout) :: fp
    integer(i2b), dimension(:,:), intent(out), pointer :: x
    integer(i2b), intent(out) :: mv
    real(r4b), intent(out) :: xll, yll, cs
    ! -- local
    type(tEhdrHdr), pointer :: hdr
! ------------------------------------------------------------------------------
    !
    call this%ehdr_read_grid(fp)
    hdr => this%hdr
    !
    mv = hdr%mvi2; xll = hdr%xllr4; yll = hdr%yllr4; cs = hdr%csr4
    call this%ehdr_read_grid_i2(x, hdr)
    call this%clean()
    !
    return
  end subroutine ehdr_read_grid_i2_r4

  subroutine ehdr_read_grid_i2_r8(this, fp, x, mv, xll, yll, cs)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tEhdr) :: this
    character(len=*), intent(inout) :: fp
    integer(i2b), dimension(:,:), intent(out), pointer :: x
    integer(i2b), intent(out) :: mv
    real(r8b), intent(out) :: xll, yll, cs
    ! -- local
    type(tEhdrHdr), pointer :: hdr
! ------------------------------------------------------------------------------
    !
    call this%ehdr_read_grid(fp)
    hdr => this%hdr
    !
    mv = hdr%mvi2; xll = hdr%xllr8; yll = hdr%yllr8; cs = hdr%csr8
    call this%ehdr_read_grid_i2(x, hdr)
    call this%clean()
    !
    return
  end subroutine ehdr_read_grid_i2_r8
  
  subroutine ehdr_read_grid_i4_r4(this, fp, x, mv, xll, yll, cs)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tEhdr) :: this
    character(len=*), intent(inout) :: fp
    integer(i4b), dimension(:,:), intent(out), pointer :: x
    integer(i4b), intent(out) :: mv
    real(r4b), intent(out) :: xll, yll, cs
    ! -- local
    type(tEhdrHdr), pointer :: hdr
! ------------------------------------------------------------------------------
    !
    call this%ehdr_read_grid(fp)
    hdr => this%hdr
    !
    mv = hdr%mvi4; xll = hdr%xllr4; yll = hdr%yllr4; cs = hdr%csr4
    call this%ehdr_read_grid_i4(x, hdr)
    call this%clean()
    !
    return
  end subroutine ehdr_read_grid_i4_r4

  subroutine ehdr_read_grid_i4_r8(this, fp, x, mv, xll, yll, cs)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tEhdr) :: this
    character(len=*), intent(inout) :: fp
    integer(i4b), dimension(:,:), intent(out), pointer :: x
    integer(i4b), intent(out) :: mv
    real(r8b), intent(out) :: xll, yll, cs
    ! -- local
    type(tEhdrHdr), pointer :: hdr
! ------------------------------------------------------------------------------
    !
    call this%ehdr_read_grid(fp)
    hdr => this%hdr
    !
    mv = hdr%mvi4; xll = hdr%xllr8; yll = hdr%yllr8; cs = hdr%csr8
    call this%ehdr_read_grid_i4(x, hdr)
    call this%clean()
    !
    return
  end subroutine ehdr_read_grid_i4_r8
  
  subroutine ehdr_read_grid_r4_r4(this, fp, x, mv, xll, yll, cs)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tEhdr) :: this
    character(len=*), intent(inout) :: fp
    real(r4b), dimension(:,:), intent(out), pointer :: x
    real(r4b), intent(out) :: mv
    real(r4b), intent(out) :: xll, yll, cs
    ! -- local
    type(tEhdrHdr), pointer :: hdr
! ------------------------------------------------------------------------------
    !
    call this%ehdr_read_grid(fp)
    hdr => this%hdr
    !
    mv = hdr%mvr4; xll = hdr%xllr4; yll = hdr%yllr4; cs = hdr%csr4
    call this%ehdr_read_grid_r4(x, hdr)
    call this%clean()
    !
    return
  end subroutine ehdr_read_grid_r4_r4

  subroutine ehdr_read_grid_r4_r8(this, fp, x, mv, xll, yll, cs)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tEhdr) :: this
    character(len=*), intent(inout) :: fp
    real(r4b), dimension(:,:), intent(out), pointer :: x
    real(r4b), intent(out) :: mv
    real(r8b), intent(out) :: xll, yll, cs
    ! -- local
    type(tEhdrHdr), pointer :: hdr
! ------------------------------------------------------------------------------
    !
    call this%ehdr_read_grid(fp)
    hdr => this%hdr
    !
    mv = hdr%mvr4; xll = hdr%xllr8; yll = hdr%yllr8; cs = hdr%csr8
    call this%ehdr_read_grid_r4(x, hdr)
    call this%clean()
    !
    return
  end subroutine ehdr_read_grid_r4_r8
  
  subroutine ehdr_read_grid_r8_r4(this, fp, x, mv, xll, yll, cs)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tEhdr) :: this
    character(len=*), intent(inout) :: fp
    real(r8b), dimension(:,:), intent(out), pointer :: x
    real(r8b), intent(out) :: mv
    real(r4b), intent(out) :: xll, yll, cs
    ! -- local
    type(tEhdrHdr), pointer :: hdr
! ------------------------------------------------------------------------------
    !
    call this%ehdr_read_grid(fp)
    hdr => this%hdr
    !
    mv = hdr%mvr8; xll = hdr%xllr4; yll = hdr%yllr4; cs = hdr%csr4
    call this%ehdr_read_grid_r8(x, hdr)
    call this%clean()
    !
    return
  end subroutine ehdr_read_grid_r8_r4

  subroutine ehdr_read_grid_r8_r8(this, fp, x, mv, xll, yll, cs)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tEhdr) :: this
    character(len=*), intent(inout) :: fp
    real(r8b), dimension(:,:), intent(out), pointer :: x
    real(r8b), intent(out) :: mv
    real(r8b), intent(out) :: xll, yll, cs
    ! -- local
    type(tEhdrHdr), pointer :: hdr
! ------------------------------------------------------------------------------
    !
    call this%ehdr_read_grid(fp)
    hdr => this%hdr
    !
    mv = hdr%mvr8; xll = hdr%xllr8; yll = hdr%yllr8; cs = hdr%csr8
    call this%ehdr_read_grid_r8(x, hdr)
    call this%clean()
    !
    return
  end subroutine ehdr_read_grid_r8_r8
  
  subroutine ehdr_read_grid_i1(this, x, hdr)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tEhdr) :: this
    integer(i1b), dimension(:,:), intent(out), pointer :: x
    type(tEhdrHdr), pointer, intent(in) :: hdr
    ! -- local
    integer(i4b) :: ncol, nrow, icol, irow
    integer(i1b) :: mv
! ------------------------------------------------------------------------------
    !
    ncol = hdr%ncol; nrow = hdr%nrow
    allocate(x(ncol,nrow))
    if (this%i_type /= i_i1) then
      call logmsg('Warning, original EHDR file has different precision.')
    end if
    !
    mv = hdr%mvi1
    select case(this%i_type)
    case(i_i1)
      do irow = 1, nrow
        do icol = 1, ncol
          if (this%xi1(icol,irow) /= hdr%mvi1) then
            x(icol,irow) = int(this%xi1(icol,irow),i1b)
          else
            x(icol,irow) = mv
          end if
        end do
      end do
    case(i_i2)
      do irow = 1, nrow
        do icol = 1, ncol
          if (this%xi2(icol,irow) /= hdr%mvi2) then
            x(icol,irow) = int(this%xi2(icol,irow),i1b)
          else
            x(icol,irow) = mv
          end if
        end do
      end do
    case(i_i4)
      do irow = 1, nrow
        do icol = 1, ncol
          if (this%xi4(icol,irow) /= hdr%mvi4) then
            x(icol,irow) = int(this%xi4(icol,irow),i1b)
          else
            x(icol,irow) = mv
          end if
        end do
      end do
    case(i_r4)
      do irow = 1, nrow
        do icol = 1, ncol
          if (this%xr4(icol,irow) /= hdr%mvr4) then
            x(icol,irow) = int(this%xr4(icol,irow),i1b)
          else
            x(icol,irow) = mv
          end if
        end do
      end do
    end select
    !
    return
  end subroutine ehdr_read_grid_i1
  
  subroutine ehdr_read_grid_i2(this, x, hdr)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tEhdr) :: this
    integer(i2b), dimension(:,:), intent(out), pointer :: x
    type(tEhdrHdr), pointer, intent(in) :: hdr
    ! -- local
    integer(i4b) :: ncol, nrow, icol, irow
    integer(i2b) :: mv
! ------------------------------------------------------------------------------
    !
    ncol = hdr%ncol; nrow = hdr%nrow
    allocate(x(ncol,nrow))
    if (this%i_type /= i_i2) then
      call logmsg('Warning, original EHDR file has different precision.')
    end if
    !
    mv = hdr%mvi2
    select case(this%i_type)
    case(i_i1)
      do irow = 1, nrow
        do icol = 1, ncol
          if (this%xi1(icol,irow) /= hdr%mvi1) then
            x(icol,irow) = int(this%xi1(icol,irow),i2b)
          else
            x(icol,irow) = mv
          end if
        end do
      end do
    case(i_i2)
      do irow = 1, nrow
        do icol = 1, ncol
          if (this%xi2(icol,irow) /= hdr%mvi2) then
            x(icol,irow) = int(this%xi2(icol,irow),i2b)
          else
            x(icol,irow) = mv
          end if
        end do
      end do
    case(i_i4)
      do irow = 1, nrow
        do icol = 1, ncol
          if (this%xi4(icol,irow) /= hdr%mvi4) then
            x(icol,irow) = int(this%xi4(icol,irow),i2b)
          else
            x(icol,irow) = mv
          end if
        end do
      end do
    case(i_r4)
      do irow = 1, nrow
        do icol = 1, ncol
          if (this%xr4(icol,irow) /= hdr%mvr4) then
            x(icol,irow) = int(this%xr4(icol,irow),i2b)
          else
            x(icol,irow) = mv
          end if
        end do
      end do
    end select
    !
    return
  end subroutine ehdr_read_grid_i2
  
  subroutine ehdr_read_grid_i4(this, x, hdr)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tEhdr) :: this
    integer(i4b), dimension(:,:), intent(out), pointer :: x
    type(tEhdrHdr), pointer, intent(in) :: hdr
    ! -- local
    integer(i4b) :: ncol, nrow, icol, irow
    integer(i4b) :: mv
! ------------------------------------------------------------------------------
    !
    ncol = hdr%ncol; nrow = hdr%nrow
    allocate(x(ncol,nrow))
    if (this%i_type /= i_i4) then
      call logmsg('Warning, original EHDR file has different precision.')
    end if
    !
    mv = hdr%mvi4
    select case(this%i_type)
    case(i_i1)
      do irow = 1, nrow
        do icol = 1, ncol
          if (this%xi1(icol,irow) /= hdr%mvi1) then
            x(icol,irow) = int(this%xi1(icol,irow),i4b)
          else
            x(icol,irow) = mv
          end if
        end do
      end do
    case(i_i2)
      do irow = 1, nrow
        do icol = 1, ncol
          if (this%xi2(icol,irow) /= hdr%mvi2) then
            x(icol,irow) = int(this%xi2(icol,irow),i4b)
          else
            x(icol,irow) = mv
          end if
        end do
      end do
    case(i_i4)
      do irow = 1, nrow
        do icol = 1, ncol
          if (this%xi4(icol,irow) /= hdr%mvi4) then
            x(icol,irow) = int(this%xi4(icol,irow),i4b)
          else
            x(icol,irow) = mv
          end if
        end do
      end do
    case(i_r4)
      do irow = 1, nrow
        do icol = 1, ncol
          if (this%xr4(icol,irow) /= hdr%mvr4) then
            x(icol,irow) = int(this%xr4(icol,irow),i4b)
          else
            x(icol,irow) = mv
          end if
        end do
      end do
    end select
    !
    return
  end subroutine ehdr_read_grid_i4
  
  subroutine ehdr_read_grid_r4(this, x, hdr)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tEhdr) :: this
    real(r4b), dimension(:,:), intent(out), pointer :: x
    type(tEhdrHdr), pointer, intent(in) :: hdr
    ! -- local
    integer(i4b) :: ncol, nrow, icol, irow
    real(r4b) :: mv
! ------------------------------------------------------------------------------
    !
    ncol = hdr%ncol; nrow = hdr%nrow
    allocate(x(ncol,nrow))
    if (this%i_type /= i_r4) then
      call logmsg('Warning, original EHDR file has different precision.')
    end if
    !
    mv = hdr%mvr4
    select case(this%i_type)
    case(i_i1)
      do irow = 1, nrow
        do icol = 1, ncol
          if (this%xi1(icol,irow) /= hdr%mvi1) then
            x(icol,irow) = real(this%xi1(icol,irow),r4b)
          else
            x(icol,irow) = mv
          end if
        end do
      end do
    case(i_i2)
      do irow = 1, nrow
        do icol = 1, ncol
          if (this%xi2(icol,irow) /= hdr%mvi2) then
            x(icol,irow) = real(this%xi2(icol,irow),r4b)
          else
            x(icol,irow) = mv
          end if
        end do
      end do
    case(i_i4)
      do irow = 1, nrow
        do icol = 1, ncol
          if (this%xi4(icol,irow) /= hdr%mvi4) then
            x(icol,irow) = real(this%xi4(icol,irow),r4b)
          else
            x(icol,irow) = mv
          end if
        end do
      end do
    case(i_r4)
      do irow = 1, nrow
        do icol = 1, ncol
          if (this%xr4(icol,irow) /= hdr%mvr4) then
            x(icol,irow) = real(this%xr4(icol,irow),r4b)
          else
            x(icol,irow) = mv
          end if
        end do
      end do
    end select
    !
    return
  end subroutine ehdr_read_grid_r4

  subroutine ehdr_read_grid_r8(this, x, hdr)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tEhdr) :: this
    real(r8b), dimension(:,:), intent(out), pointer :: x
    type(tEhdrHdr), pointer, intent(in) :: hdr
    ! -- local
    integer(i4b) :: ncol, nrow, icol, irow
    real(r8b) :: mv !!!
! ------------------------------------------------------------------------------
    !
    ncol = hdr%ncol; nrow = hdr%nrow
    allocate(x(ncol,nrow))
    if (this%i_type /= i_r8) then
      call logmsg('Warning, original EHDR file has different precision.')
    end if
    !
    mv = hdr%mvr8 !!!
    select case(this%i_type)
    case(i_i1)
      do irow = 1, nrow
        do icol = 1, ncol
          if (this%xi1(icol,irow) /= hdr%mvi1) then
            x(icol,irow) = real(this%xi1(icol,irow),r8b) !!!
          else
            x(icol,irow) = mv
          end if
        end do
      end do
    case(i_i2)
      do irow = 1, nrow
        do icol = 1, ncol
          if (this%xi2(icol,irow) /= hdr%mvi2) then
            x(icol,irow) = real(this%xi2(icol,irow),r8b) !!!
          else
            x(icol,irow) = mv
          end if
        end do
      end do
    case(i_i4)
      do irow = 1, nrow
        do icol = 1, ncol
          if (this%xi4(icol,irow) /= hdr%mvi4) then
            x(icol,irow) = real(this%xi4(icol,irow),r8b) !!!
          else
            x(icol,irow) = mv
          end if
        end do
      end do
    case(i_r4)
      do irow = 1, nrow
        do icol = 1, ncol
          if (this%xr4(icol,irow) /= hdr%mvr4) then
            x(icol,irow) = real(this%xr4(icol,irow),r8b) !!!
          else
            x(icol,irow) = mv
          end if
        end do
      end do
    end select
    !
    return
  end subroutine ehdr_read_grid_r8
  
  function ehdr_get_val_i4(this, icol, irow) result(x)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tEhdr) :: this
    integer(i4b), intent(in) :: icol, irow
    integer(i4b) :: x
    ! -- local
    integer(i1b) :: i1v
    integer(i2b) :: i2v
    integer(i4b) :: i4v
    real(r4b) :: r4v
    real(r8b) :: r8v
! ------------------------------------------------------------------------------
    if (this%i_type == 0) then
      call this%ehdr_set_i_type()
    end if
    select case(this%i_type)
    case(i_i1)
      i1v = this%ehdr_read_val_i1(icol,irow); x = int(i1v,i4b)
    case(i_i2)
      i2v = this%ehdr_read_val_i2(icol,irow); x = int(i1v,i4b)
    case(i_i4)
      i4v = this%ehdr_read_val_i4(icol,irow); x = int(i1v,i4b)
    case(i_r4)
      r4v = this%ehdr_read_val_r4(icol,irow); x = int(i1v,i4b)
    end select
    !
    return
  end function ehdr_get_val_i4
  
  function ehdr_read_val_i1(this, icol, irow) result(x)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tEhdr) :: this
    integer(i4b), intent(in) :: icol, irow
    integer(i1b) :: x
    ! -- local
    integer(i8b) :: p
! ------------------------------------------------------------------------------
    !
    p = 1 + ((irow-1)*this%hdr%ncol + icol - 1)*i1b
    read(unit=this%iu_flt,pos=p) x
    !
    return
  end function ehdr_read_val_i1
  
  function ehdr_read_val_i2(this, icol, irow) result(x)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tEhdr) :: this
    integer(i4b), intent(in) :: icol, irow
    integer(i2b) :: x
    ! -- local
    integer(i8b) :: p
    type(tEhdrHdr), pointer :: hdr
! ------------------------------------------------------------------------------
    !
    p = 1 + ((irow-1)*this%hdr%ncol + icol - 1)*i2b
    read(unit=this%iu_flt,pos=p) x
    !
    return
  end function ehdr_read_val_i2
  
  function ehdr_read_val_i4(this, icol, irow) result(x)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tEhdr) :: this
    integer(i4b), intent(in) :: icol, irow
    integer(i4b) :: x
    ! -- local
    integer(i8b) :: p
    type(tEhdrHdr), pointer :: hdr
! ------------------------------------------------------------------------------
    !
    p = 1 + ((irow-1)*this%hdr%ncol + icol - 1)*i4b
    read(unit=this%iu_flt,pos=p) x
    !
    return
  end function ehdr_read_val_i4
  
  function ehdr_read_val_r4(this, icol, irow) result(x)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tEhdr) :: this
    integer(i4b), intent(in) :: icol, irow
    real(r4b) :: x
    ! -- local
    integer(i8b) :: p
    type(tEhdrHdr), pointer :: hdr
! ------------------------------------------------------------------------------
    !
    p = 1 + ((irow-1)*this%hdr%ncol + icol - 1)*r4b
    read(unit=this%iu_flt,pos=p) x
    !
    return
  end function ehdr_read_val_r4
  
  subroutine writeflt_header_r4(iu, ncol, nrow, xll, yll, cs, nodata, &
  nbits, pixeltype)
! ******************************************************************************
    ! -- arguments
    integer(i4b), intent(in) :: iu, ncol, nrow
    character(len=*), intent(in) :: nodata
    real(r4b), intent(in) :: xll, yll, cs
    integer(i4b), intent(in) :: nbits
    character(len=*), intent(in) :: pixeltype
    ! -- locals
! ------------------------------------------------------------------------------
    write(iu,'(a)') 'ncols '//ta((/ncol/))
    write(iu,'(a)') 'nrows '//ta((/nrow/))
    write(iu,'(a)') 'xllcorner '//ta((/xll/))
    write(iu,'(a)') 'yllcorner '//ta((/yll/))
    write(iu,'(a)') 'cellsize '//ta((/cs/))
    write(iu,'(a)') 'nodata_value '//trim(nodata)
    write(iu,'(a)') 'nbits '//ta((/nbits/))
    write(iu,'(a)') 'pixeltype '//trim(pixeltype)
    write(iu,'(a)') 'byteorder lsbfirst'
    close(iu)
    !
    return
  end subroutine writeflt_header_r4
  
  subroutine writeflt_header_r8(iu, ncol, nrow, xll, yll, cs, nodata, &
  nbits, pixeltype)
! ******************************************************************************
    ! -- arguments
    integer(i4b), intent(in) :: iu, ncol, nrow
    character(len=*), intent(in) :: nodata
    real(r8b), intent(in) :: xll, yll, cs
    integer(i4b), intent(in) :: nbits
    character(len=*), intent(in) :: pixeltype
    ! -- locals
! ------------------------------------------------------------------------------
    write(iu,'(a)') 'ncols '//ta((/ncol/))
    write(iu,'(a)') 'nrows '//ta((/nrow/))
    write(iu,'(a)') 'xllcorner '//ta((/xll/))
    write(iu,'(a)') 'yllcorner '//ta((/yll/))
    write(iu,'(a)') 'cellsize '//ta((/cs/))
    write(iu,'(a)') 'nodata_value '//trim(nodata)
    write(iu,'(a)') 'nbits '//ta((/nbits/))
    write(iu,'(a)') 'pixeltype '//trim(pixeltype)
    write(iu,'(a)') 'byteorder lsbfirst'
    close(iu)
    !
    return
  end subroutine writeflt_header_r8
!
  subroutine writeflt_i1_r4(fp, x, ncol, nrow, xll, yll, cs, nodata)
! ******************************************************************************
! see: https://gdal.org/drivers/raster/ehdr.html#raster-ehdr
    ! -- arguments
    character(len=*), intent(in) :: fp
    integer(i4b), intent(in) :: ncol, nrow
    integer(i1b), dimension(ncol,nrow), intent(in) :: x
    integer(i1b), intent(in) :: nodata
    real(r4b), intent(in) :: xll, yll, cs
    ! -- locals
    character(len=mxslen) :: f
    integer(i4b) :: iu, icol, irow
! ------------------------------------------------------------------------------
    f = trim(fp)//'.hdr'
    call open_file(f, iu, 'w')
    call writeflt_header_r4(iu, ncol, nrow, xll, yll, cs, ta((/nodata/)), 8, 'signedint')
    close(iu)
    !
    f = trim(fp)//'.flt'
    call open_file(f, iu, 'w', .true.)
    write(iu)((x(icol,irow),icol=1,ncol),irow=1,nrow)
    close(iu)
    !
    return
  end subroutine writeflt_i1_r4
  
 subroutine writeflt_i4_r4(fp, x, ncol, nrow, xll, yll, cs, nodata)
! ******************************************************************************
! see: https://gdal.org/drivers/raster/ehdr.html#raster-ehdr
    ! -- arguments
    character(len=*), intent(in) :: fp
    integer(i4b), intent(in) :: ncol, nrow
    integer(i4b), dimension(ncol,nrow), intent(in) :: x
    integer(i4b), intent(in) :: nodata
    real(r4b), intent(in) :: xll, yll, cs
    ! -- locals
    character(len=mxslen) :: f
    integer(i4b) :: iu, icol, irow
! ------------------------------------------------------------------------------
    f = trim(fp)//'.hdr'
    call open_file(f, iu, 'w')
    call writeflt_header_r4(iu, ncol, nrow, xll, yll, cs, ta((/nodata/)), 32, 'signedint')
    close(iu)
    !
    f = trim(fp)//'.flt'
    call open_file(f, iu, 'w', .true.)
    write(iu)((x(icol,irow),icol=1,ncol),irow=1,nrow)
    close(iu)
    !
    return
  end subroutine writeflt_i4_r4
 
  subroutine writeflt_r4_r4(fp, x, ncol, nrow, xll, yll, cs, nodata)
! ******************************************************************************
! see: https://gdal.org/drivers/raster/ehdr.html#raster-ehdr
    ! -- arguments
    character(len=*), intent(in) :: fp
    integer(i4b), intent(in) :: ncol, nrow
    real(r4b), dimension(ncol,nrow), intent(in) :: x
    real(r4b), intent(in) :: nodata
    real(r4b), intent(in) :: xll, yll, cs
    ! -- locals
    character(len=mxslen) :: f
    integer(i4b) :: iu, icol, irow
! ------------------------------------------------------------------------------
    f = trim(fp)//'.hdr'
    call open_file(f, iu, 'w')
    call writeflt_header_r4(iu, ncol, nrow, xll, yll, cs, ta((/nodata/)), 32, 'float')
    close(iu)
    !
    f = trim(fp)//'.flt'
    call open_file(f, iu, 'w', .true.)
    write(iu)((x(icol,irow),icol=1,ncol),irow=1,nrow)
    close(iu)
    !
    return
  end subroutine writeflt_r4_r4
  
  subroutine writeflt_i1_r8(fp, x, ncol, nrow, xll, yll, cs, nodata)
! ******************************************************************************
! see: https://gdal.org/drivers/raster/ehdr.html#raster-ehdr
    ! -- arguments
    character(len=*), intent(in) :: fp
    integer(i4b), intent(in) :: ncol, nrow
    integer(i1b), dimension(ncol,nrow), intent(in) :: x
    integer(i1b), intent(in) :: nodata
    real(r8b), intent(in) :: xll, yll, cs
    ! -- locals
    character(len=mxslen) :: f
    integer(i4b) :: iu, icol, irow
! ------------------------------------------------------------------------------
    f = trim(fp)//'.hdr'
    call open_file(f, iu, 'w')
    call writeflt_header_r8(iu, ncol, nrow, xll, yll, cs, ta((/nodata/)), 8, 'signedint')
    close(iu)
    !
    f = trim(fp)//'.flt'
    call open_file(f, iu, 'w', .true.)
    write(iu)((x(icol,irow),icol=1,ncol),irow=1,nrow)
    close(iu)
    !
    return
  end subroutine writeflt_i1_r8
  
 subroutine writeflt_i4_r8(fp, x, ncol, nrow, xll, yll, cs, nodata)
! ******************************************************************************
! see: https://gdal.org/drivers/raster/ehdr.html#raster-ehdr
    ! -- arguments
    character(len=*), intent(in) :: fp
    integer(i4b), intent(in) :: ncol, nrow
    integer(i4b), dimension(ncol,nrow), intent(in) :: x
    integer(i4b), intent(in) :: nodata
    real(r8b), intent(in) :: xll, yll, cs
    ! -- locals
    character(len=mxslen) :: f
    integer(i4b) :: iu, icol, irow
! ------------------------------------------------------------------------------
    f = trim(fp)//'.hdr'
    call open_file(f, iu, 'w')
    call writeflt_header_r8(iu, ncol, nrow, xll, yll, cs, ta((/nodata/)), 32, 'signedint')
    close(iu)
    !
    f = trim(fp)//'.flt'
    call open_file(f, iu, 'w', .true.)
    write(iu)((x(icol,irow),icol=1,ncol),irow=1,nrow)
    close(iu)
    !
    return
  end subroutine writeflt_i4_r8
 
  subroutine writeflt_r4_r8(fp, x, ncol, nrow, xll, yll, cs, nodata)
! ******************************************************************************
! see: https://gdal.org/drivers/raster/ehdr.html#raster-ehdr
    ! -- arguments
    character(len=*), intent(in) :: fp
    integer(i4b), intent(in) :: ncol, nrow
    real(r4b), dimension(ncol,nrow), intent(in) :: x
    real(r4b), intent(in) :: nodata
    real(r8b), intent(in) :: xll, yll, cs
    ! -- locals
    character(len=mxslen) :: f
    integer(i4b) :: iu, icol, irow
! ------------------------------------------------------------------------------
    f = trim(fp)//'.hdr'
    call open_file(f, iu, 'w')
    call writeflt_header_r8(iu, ncol, nrow, xll, yll, cs, ta((/nodata/)), 32, 'float')
    close(iu)
    !
    f = trim(fp)//'.flt'
    call open_file(f, iu, 'w', .true.)
    write(iu)((x(icol,irow),icol=1,ncol),irow=1,nrow)
    close(iu)
    !
    return
  end subroutine writeflt_r4_r8
!
  subroutine writeflt_r8_r8(fp, x, ncol, nrow, xll, yll, cs, nodata)
! ******************************************************************************
! see: https://gdal.org/drivers/raster/ehdr.html#raster-ehdr
    ! -- arguments
    character(len=*), intent(in) :: fp
    integer(i4b), intent(in) :: ncol, nrow
    real(r8b), dimension(ncol,nrow), intent(in) :: x
    real(r8b), intent(in) :: nodata
    real(r8b), intent(in) :: xll, yll, cs
    ! -- locals
    character(len=mxslen) :: f
    integer(i4b) :: iu, icol, irow
    real(r4b), dimension(:,:), allocatable :: r4x
    real(r4b) :: r4nodata
! ------------------------------------------------------------------------------
    f = trim(fp)//'.hdr'
    call open_file(f, iu, 'w')
    r4nodata = real(nodata,r4b)
    call writeflt_header_r8(iu, ncol, nrow, xll, yll, cs, ta((/r4nodata/)), 32, 'float')
    close(iu)
    !
    f = trim(fp)//'.flt'
    call open_file(f, iu, 'w', .true.)
    allocate(r4x(ncol,nrow))
    do irow = 1, nrow
      do icol = 1, ncol
        if (x(icol,irow) /= nodata) then
          r4x(icol,irow) = real(x(icol,irow),r4b)
        else
          r4x(icol,irow) = r4nodata
        end if
      end do
    end do
    write(iu)((r4x(icol,irow),icol=1,ncol),irow=1,nrow)
    close(iu)
    deallocate(r4x)
    !
    return
  end subroutine writeflt_r8_r8  

end module ehdrModule
