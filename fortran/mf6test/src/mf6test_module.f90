module mf6test_module
   ! -- modules
  use, intrinsic :: iso_fortran_env , only: error_unit, output_unit, &
     i1b => int8, i2b => int16, i4b => int32, i8b => int64, r4b => real32, r8b => real64
  use utilsmod, only: getlun, chkexist, readline, change_case, errmsg, logmsg, &
    checkdim, addboundary, tUnp, &
    calc_unique, sa, open_file, create_dir, swap_slash, tas, ta, get_rel_up, &
    writebin, get_jd, jd_next_month, get_month_days, getwords, getminmax, &
    replacetoken, IZERO, RZERO, DZERO, DONE, tBB, calc_unique, tI4grid, count_i1a, &
    get_slash, get_abs_path, get_month_days, get_month_days_s, get_ymd_from_jd
  use ehdrModule
  !
  implicit none
  !
  integer(i4b),          parameter :: mxslen = 1024
  character(len=mxslen), parameter :: resultsbindir = '..\..\models\run_output_bin'
  character(len=mxslen), parameter :: resultslstdir = '..\..\models\run_output_lst'
  logical                          :: ltransient = .false.
  character(len=2)                 :: ctim = 'ss'
  real(r8b), parameter :: r8nodata = -12345.d0
  
  integer(i4b), parameter :: inam  =  1
  integer(i4b), parameter :: itdis =  2
  integer(i4b), parameter :: idisu =  3
  integer(i4b), parameter :: iic   =  4
  integer(i4b), parameter :: ioc   =  5
  integer(i4b), parameter :: inpf  =  6
  integer(i4b), parameter :: isto  =  7
  integer(i4b), parameter :: ichd  =  8
  integer(i4b), parameter :: idrn  =  9
  integer(i4b), parameter :: iriv  = 10
  integer(i4b), parameter :: irch  = 11
  integer(i4b), parameter :: iwel  = 12
  integer(i4b), parameter :: ighb  = 13
  integer(i4b), parameter :: npck = ighb
  character(len=4), dimension(npck) :: pck
  data pck/'nam ', 'tdis', 'disu', 'ic  ', 'oc  ', 'npf ', 'sto ', 'chd ',&
           'drn ', 'riv ', 'rch ', 'wel ', 'ghb '/
  integer(i4b), dimension(npck) :: pckact
  
  ! stencil
  integer(i4b), parameter :: jp   = 1
  integer(i4b), parameter :: jt   = 2
  integer(i4b), parameter :: jn   = 3
  integer(i4b), parameter :: jw   = 4 
  integer(i4b), parameter :: je   = 5
  integer(i4b), parameter :: js   = 6
  integer(i4b), parameter :: jb   = 7
  !
  integer(i4b), parameter :: ns   = jb
  
  private
  
  integer(i4b), parameter :: i_undefined = 0
  integer(i4b), parameter :: i_ehdr       = 1
  integer(i4b), parameter :: maxnraw = 1000

  type tData
    character(mxslen)     :: s = ''
    integer(i4b)          :: file_type = i_undefined
    logical               :: readall = .false.
    type(tEhdr),  pointer :: ehdr     => null()
    real(r8b)             :: r8mult = DONE
    real(r8b)             :: r8add = DZERO
  end type tData
  
  type tRaw
    character(mxslen) :: key = ''
    type(tData), pointer :: dat => null()
    integer(I4B)      :: ilay_min = 0
    integer(I4B)      :: ilay_max = 0
    integer(I4B)      :: iper_min = 0
    integer(I4B)      :: iper_max = 0
  end type tRaw
  type tRawDat
    type(tRaw), dimension(maxnraw) :: raw
    integer(I4B)      :: nraw = 0
    integer(I4B)      :: nper = 0
    character(len=mxslen):: sdate = ''
    character(len=mxslen), dimension(:), allocatable :: perdate
  contains
    procedure :: init => mf6_raw_init
    procedure :: init_file => mf6_raw_init_file
    procedure :: mf6_raw_get_index
    procedure :: mf6_raw_key_exists
    procedure :: mf6_raw_get_name_char, mf6_raw_get_name_i4b, mf6_raw_get_name_r8b
    generic   :: exists => mf6_raw_key_exists
    generic   :: getc  => mf6_raw_get_name_char
    generic   :: geti => mf6_raw_get_name_i4b
    generic   :: getr8 => mf6_raw_get_name_r8b
  end type tRawDat
  type(tRawDat) :: raw
  
  integer(i4b) :: gncol      = 0
  integer(i4b) :: gnrow      = 0
  integer(i4b) :: gnlay      = 0
  real(r8b)    :: gxmin      = DZERO
  real(r8b)    :: gymin      = DZERO
  real(r8b)    :: gcs        = DZERO
  integer(i4b), dimension(:), allocatable :: rlev
  !
  ! work arrays
  character(len=mxslen), dimension(:),     pointer :: cwrk1d   => null()
  character(len=mxslen), dimension(:,:),   pointer :: cwrk2d   => null()
  integer(i1b),          dimension(:),     pointer :: i1wrk    => null()
  integer(i1b),          dimension(:),     pointer :: i1wrk2   => null()
  integer(i1b),          dimension(:,:),   pointer :: i1wrk2d  => null()
  integer(i4b),          dimension(:),     pointer :: i4wrk1d  => null()
  integer(i4b),          dimension(:),     pointer :: i4wrk1d2 => null()
  integer(i4b),          dimension(:,:),   pointer :: i4wrk2d  => null()
  integer(i4b),          dimension(:,:,:), pointer :: i4wrk3d  => null()
  real(r8b),             dimension(:),     pointer :: r8wrk    => null()
  real(r8b),             dimension(:),     pointer :: r8wrk2   => null()
  real(r8b),             dimension(:),     pointer :: r8wrk3   => null()
  real(r8b),             dimension(:,:),   pointer :: r8wrk2d  => null()
  real(r8b),             dimension(:,:),   pointer :: r8wrk2d2 => null()
  
  type tDisu
    integer(i4b),               pointer :: nodes => null()
    integer(i4b),               pointer :: nja   => null()
    integer(i4b), dimension(:), pointer :: iac   => null() !length nodes
    integer(i4b), dimension(:), pointer :: ja    => null() !length nja
    integer(i4b), dimension(:), pointer :: ihc   => null() !length nja
    real(r8b),    dimension(:), pointer :: cl12  => null() !length nja
    real(r8b),    dimension(:), pointer :: hwva  => null() !length nja
    real(r8b),    dimension(:), pointer :: top   => null()
    real(r8b),    dimension(:), pointer :: bot   => null()
  end type tDisu
  !
  type tRegmap
    type(tBb),                    pointer :: bb => null()
    integer(i4b), dimension(:,:), pointer :: nodmap => null()
    integer(i4b), dimension(:,:), pointer :: bndmap => null()
  end type tRegmap
  !
  type tReg
    type(tBb),                      pointer :: bb          => null()
    integer(i2b), dimension(:,:),   pointer :: itile       => null()
    integer(i4b), dimension(:),     pointer :: layer_nodes => null()
    type(tregMap), dimension(:),    pointer :: regmap      => null()
  end type tReg
  
  type tExchange
    type(tBb),                    pointer :: bb          => null()
    integer(i4b), dimension(:),   pointer :: m1reg       => null() !region ID M1
    integer(i4b), dimension(:),   pointer :: m2reg       => null() !region ID M1
    integer(i4b),                 pointer :: m1mod       => null() !local model index for solution
    integer(i4b),                 pointer :: m2mod       => null()
    character(len=mxslen),        pointer :: m2modelname => null()
    integer(i4b),                 pointer :: nexg        => null()
    integer(i4b), dimension(:),   pointer :: cellidm1    => null()
    integer(i4b), dimension(:),   pointer :: cellidm2    => null()
    integer(i4b), dimension(:),   pointer :: ihc         => null()
    real(r8b),    dimension(:),   pointer :: cl1         => null()
    real(r8b),    dimension(:),   pointer :: cl2         => null()
    real(r8b),    dimension(:),   pointer :: hwva        => null()
    integer(i4b), dimension(:,:), pointer :: gicirilm1   => null() !help
    integer(i4b), dimension(:,:), pointer :: gicirilm2   => null() !help
  end type tExchange
  
  type tMf6_mod
    integer(i4b)                           :: imod        = -1      !model ID
    integer(i4b),                  pointer :: isol        => null() !solution ID
    type(tBb),                     pointer :: bb          => null()
    character(len=mxslen),         pointer :: modelname   => null()
    character(len=mxslen),         pointer :: rootdir     => null()
    character(len=mxslen),         pointer :: bindir      => null()
    integer(i4b),                  pointer :: nreg        => null()
    type(tReg), dimension(:),      pointer :: reg         => null()
    type(tDisu),                   pointer :: disu        => null()
    integer(i4b),                  pointer :: nxch        => null()
    type(tExchange), dimension(:), pointer :: xch         => null()
    integer(i4b), dimension(:),    pointer :: layer_nodes => null()
    integer(i4b),                  pointer :: ncol        => null()
    integer(i4b),                  pointer :: nrow        => null()
    character(len=mxslen),         pointer :: fbin        => null()
    integer(i4b),                  pointer :: iubin       => null()
  contains
    procedure :: get_model_name => mf6_mod_get_model_name
    procedure :: set_disu       => mf6_mod_set_disu
    procedure :: get_val        => mf6_mod_get_val_r8
    procedure :: get_array      => mf6_mod_get_array_r8
    generic   :: write_array    => mf6_mod_write_array_i4, &
                                   mf6_mod_write_array_r8
    procedure :: mf6_mod_write_array_i4
    procedure :: mf6_mod_write_array_r8
    generic   :: write_list    => mf6_mod_write_list_1, &
                                  mf6_mod_write_list_2, &
                                  mf6_mod_write_list_3
    procedure :: mf6_mod_write_list_1
    procedure :: mf6_mod_write_list_2
    procedure :: mf6_mod_write_list_3
    !
    procedure :: write       => mf6_mod_write
    procedure :: write_nam   => mf6_mod_write_nam
    procedure :: write_disu  => mf6_mod_write_disu
    procedure :: write_ic    => mf6_mod_write_ic
    procedure :: write_oc    => mf6_mod_write_oc
    procedure :: write_npf   => mf6_mod_write_npf
    procedure :: write_sto   => mf6_mod_write_sto
    procedure :: write_chd   => mf6_mod_write_chd
    procedure :: write_riv   => mf6_mod_write_riv
    procedure :: write_drn   => mf6_mod_write_drn
    procedure :: write_rch   => mf6_mod_write_rch
    procedure :: write_wel   => mf6_mod_write_wel
    procedure :: write_ghb   => mf6_mod_write_ghb
    !
    procedure :: write_exchanges => mf6_mod_write_exchanges
    !
    procedure :: write_post_map  => mf6_mod_write_post_map
    !
    procedure :: clean_regions  => mf6_mod_clean_regions
    !
    procedure :: count_i1a => mf6_mod_count_i1a
    !
  end type tMf6_mod
  
  type tMf6_sol
    integer(i4b)                          :: isol = -1
    character(len=mxslen),        pointer :: solname => null()
    integer(i4b),                 pointer :: nmod    => null() !number of models/partitions
    integer(i4b), dimension(:),   pointer :: mod_id  => null() !model ids
    integer(i4b),                 pointer :: npart    => null() !number of partitions
    integer(i4b), dimension(:),   pointer :: mod_part => null() !model partition numbers
  contains
    procedure :: write           => mf6_sol_write
    procedure :: write_tdis      => mf6_sol_write_tdis
    procedure :: write_ims       => mf6_sol_write_ims
    procedure :: write_wrap      => mf6_sol_write_wrap
    procedure :: write_mfsim     => mf6_sol_write_mfsim
    procedure :: write_post_map  => mf6_sol_write_post_map
    procedure :: clean           => mf6_sol_clean
  end type tMf6_sol
  
  ! public variables
  public :: tMf6_mod
  public :: tRegmap
  public :: tReg
  public :: tMf6_sol
  public :: tExchange
  public :: i1b, i4b, i8b, r4b, r8b
  public :: raw
  public :: gncol, gnrow, gnlay, gxmin, gymin, gcs, rlev
  
  save
  
  contains
  
  subroutine mf6_raw_init(this, f_in)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tRawDat) :: this
    character(len=*), intent(inout) :: f_in
    ! -- local
    character(len=mxslen) :: s
    integer(I4B) :: iu, ios, nw, n, i, y, m ,d, ymd
    real(R8B) :: jd
    character(len=mxslen), dimension(:), allocatable :: words
    type(tData), pointer :: dat
! ------------------------------------------------------------------------------
    call logmsg('Reading '//trim(f_in)//'...')
    call open_file(f_in, iu)
    ios = 0
    n = 0
    do while (ios == 0)
      ios = readline(iu, s)
      !write(*,*) 'Reading ',trim(s)
      if (ios == 0) then
        words = getwords(s)
        nw = size(words)
        if (nw > 1) then
          n = n + 1; this%nraw = n
          this%raw(n)%key = change_case(words(1), 'l')
          call getminmax(words(1), '_', 'L', this%raw(n)%ilay_min, this%raw(n)%ilay_max)
          call getminmax(words(1), '_', 'P', this%raw(n)%iper_min, this%raw(n)%iper_max)
          !
          ! set data
          allocate(this%raw(n)%dat)
          dat => this%raw(n)%dat
          dat%s = words(2)
          dat%file_type = i_undefined
          dat%readall = .false.
          !
          !set absolute paths
          call get_abs_path(dat%s)
          !
          ! check for supported file types
          if (nw >= 3) then
            s = change_case(words(3), 'l')
            if (s(1:4) == 'ehdr') then
              dat%file_type = i_ehdr
              if (s == 'ehdr_readall') dat%readall = .true.
            end if
          end if
          !
          if (nw > 3) then
            s = change_case(words(4), 'u')
            if (trim(s) == '*') then
              call logmsg('**** Multiplication factor found **** ')
              read(words(5),*) dat%r8mult
            end if
            if (trim(s) == '+') then
              call logmsg('**** Addition factor found **** ')
              read(words(5),*) dat%r8add
            end if
            if (nw > 5) then
              s = change_case(words(6), 'u')
              if (trim(s) == '*') then
                call logmsg('**** Multiplication factor found **** ')
                read(words(7),*) dat%r8mult
              end if
              if (trim(s) == '+') then
                call logmsg('**** Addition factor found **** ')
                read(words(7),*) dat%r8add
              end if
            end if
          end if
          !
        end if
      end if
    end do
    !
    this%nper  = this%geti('nper')
    if (this%nper > 1) then
      ltransient = .true.; ctim = 'tr'
    end if
    this%sdate = this%getc('startdate')
    allocate(this%perdate(this%nper))
    !
    read(this%sdate(1:4),*) y
    read(this%sdate(5:6),*) m
    read(this%sdate(7:8),*) d
    !
    jd = get_jd(y, m, d)
    do i = 1, this%nper
      call  get_ymd_from_jd(jd, ymd, y, m, d)
      write(this%perdate(i),'(i4,i2.2)') y, m
      call jd_next_month(jd)
    end do
    !
    return
  end subroutine mf6_raw_init

  subroutine mf6_raw_init_file(this, gdim)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tRawDat) :: this
    real(r8b), intent(in) :: gdim
    ! -- local
    type(tEhdrHdr), pointer :: hdr => null()
    type(tData), pointer :: dat1, dat2
    integer(I4B) :: i, j
    logical :: lfound
! ------------------------------------------------------------------------------  
    do i = 1, this%nraw
      dat1 => this%raw(i)%dat
      if (dat1%file_type == i_ehdr) then
        allocate(dat1%ehdr)
        lfound = .false.
        do j = 1, this%nraw
          if (i == j) cycle
          dat2 => this%raw(j)%dat
          if (dat2%file_type == i_ehdr) then
            if (associated(dat2%ehdr)) then
              if (dat1%s == dat2%s) then
                lfound = .true.
                dat1%ehdr => dat2%ehdr
              end if
            end if
          end if
        end do
        if (.not.lfound) then
          call dat1%ehdr%ehdr_get_val_init(dat1%s)
          hdr => dat1%ehdr%hdr
          !
          ! check if the data is square
          if (hdr%ncol /= hdr%nrow) then
            call errmsg('Error: invalid EHDR data (ncol /= nrow).')
          end if
          if (hdr%ncol*hdr%csr8 /= gdim) then
            call errmsg('Error: invalid EHDR data (mismatch in dimension).')
          end if
        end if
      end if
    end do
    !
    return
  end subroutine mf6_raw_init_file
  !
  function mf6_raw_get_index(this, key, ilay, iper) result(ind)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tRawDat) :: this
    character(len=*), intent(in) :: key
    integer(I4B), intent(in) :: ilay
    integer(I4B), intent(in) :: iper
    integer(I4B) :: ind
    ! -- local
    integer :: i, j, il_min, il_max, ip_min, ip_max
    character(len=mxslen) :: s
    logical :: lilay, liper
! ------------------------------------------------------------------------------
    ind = 0
    do i = 1, this%nraw
      s = this%raw(i)%key
      j = max(index(s,'_P'),index(s,'_L'))
      if (j > 0) s = s(1:j-1)
      if (index(s, trim(key)) > 0) then
        il_min = this%raw(i)%ilay_min; il_max = this%raw(i)%ilay_max
        ip_min = this%raw(i)%iper_min; ip_max = this%raw(i)%iper_max
        lilay = .false.; liper = .false.
        if ((il_min == 0).and.(il_max == 0)) then
          lilay = .true.
        else
          if ((il_min <= ilay).and.(ilay <= il_max)) then
            lilay = .true.
          end if
        end if
        if ((ip_min == 0).and.(ip_max == 0)) then
          liper = .true.
        else
          if ((ip_min <= iper).and.(iper <= ip_max)) then
            liper = .true.
          end if
        end if
        if (lilay.and.liper) then
          if (ind == 0) then
            ind = i
          else
            call errmsg('Error, '//trim(key)//' found twice')
          end if
          exit
        end if
      end if
    end do
    
    !if (ind == 0) then
    !  call errmsg('Program error mf6_get_index')
    !end if
    !
    return
  end function mf6_raw_get_index

  function mf6_raw_key_exists(this, key, ilay, iper) result(lex)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tRawDat) :: this
    character(len=*), intent(in) :: key
    integer(I4B), optional, intent(in) :: ilay
    integer(I4B), optional, intent(in) :: iper
    logical :: lex
    ! -- local
    integer(I4B) :: i
    integer(I4B) :: jlay, jper
! ------------------------------------------------------------------------------
    jlay = 0
    jper = 0
    if (present(ilay)) jlay = ilay
    if (present(iper)) jper = iper
    !
    i = this%mf6_raw_get_index(key, jlay, jper)
    if (i > 0) then
      lex = .true.
    else
      lex = .false.
    endif
    !
    return
  end function mf6_raw_key_exists

  function mf6_raw_get_name_char(this, key, ilay, iper, cdef) result(cval)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tRawDat) :: this
    character(len=*), intent(in) :: key
    integer(I4B), optional, intent(in) :: ilay
    integer(I4B), optional, intent(in) :: iper
    character(len=*), optional, intent(in) :: cdef
    character(len=:), allocatable :: cval
    ! -- local
    integer(I4B) :: i
    integer(I4B) :: jlay, jper
! ------------------------------------------------------------------------------
    jlay = 0
    jper = 0
    if (present(ilay)) jlay = ilay
    if (present(iper)) jper = iper
    !
    if (.not.this%exists(key, jlay, jper)) then
      if (present(cdef)) then
        cval = trim(cdef)
        return
      else
        call errmsg('Error: key '//trim(key)// ' not found.')
      end if
    end if
    i = this%mf6_raw_get_index(key, jlay, jper)
    cval = trim(this%raw(i)%dat%s)
    !
    return
  end function mf6_raw_get_name_char
 
! ==============================================================================
  function mf6_raw_get_name_i4b(this, key, ilay, iper, idef) result(ival)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tRawDat) :: this
    character(len=*), intent(in) :: key
    integer(I4B), optional, intent(in) :: ilay
    integer(I4B), optional, intent(in) :: iper
    integer(I4B) :: ival
    integer(I4B), optional, intent(in) :: idef
    ! -- local
    character(len=mxslen) :: s
    integer(I4B) :: ios
    integer(I4B) :: jlay, jper
! ------------------------------------------------------------------------------
    jlay = 0
    jper = 0
    if (present(ilay)) jlay = ilay
    if (present(iper)) jper = iper
    
    if (.not.this%exists(key, jlay, jper)) then
      if (present(idef)) then
        ival = idef
        return
      else
        call errmsg('Error: key '//trim(key)// ' not found.')
      end if
    end if
    s = this%mf6_raw_get_name_char(key, ilay, iper)
    read(s,*,iostat=ios) ival
    if (ios /= 0) then
      call errmsg('Could not read target for '//trim(key)//' as integer 4 bytes')
    end if
    !
    return
  end function mf6_raw_get_name_i4b

! ==============================================================================
  function mf6_raw_get_name_r8b(this, key, ilay, iper, idef) result(rval)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tRawDat) :: this
    character(len=*), intent(in) :: key
    integer(I4B), optional, intent(in) :: ilay
    integer(I4B), optional, intent(in) :: iper
    real(R8B) :: rval
    integer(I4B), optional, intent(in) :: idef
    ! -- local
    character(len=mxslen) :: s
    integer(I4B) :: ios
    integer(I4B) :: jlay, jper, ival
! ------------------------------------------------------------------------------
    jlay = 0
    jper = 0
    if (present(ilay)) jlay = ilay
    if (present(iper)) jper = iper
    
    if (.not.this%exists(key, jlay, jper)) then
      if (present(idef)) then
        ival = idef
        return
      else
        call errmsg('Error: key '//trim(key)// ' not found.')
      end if
    end if
    s = this%mf6_raw_get_name_char(key, ilay, iper)
    read(s,*,iostat=ios) rval
    if (ios /= 0) then
      call errmsg('Could not read target for '//trim(key)//' as real 8 bytes')
    end if
    !
    return
  end function mf6_raw_get_name_r8b
  
  subroutine mf6_mod_clean_regions(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    ! -- local
    type(tReg), pointer :: reg
    integer(i4b) :: ireg, ilay
! ------------------------------------------------------------------------------
    !
    if (.not.associated(this%nreg)) return
    !
    do ireg = 1, this%nreg
      reg => this%reg(ireg)
      if (associated(reg%bb))          deallocate(reg%bb)
      if (associated(reg%itile))       deallocate(reg%itile)
      if (associated(reg%layer_nodes)) deallocate(reg%layer_nodes)
      if (associated(reg%regmap)) then
        do ilay = 1, gnlay
          if (associated(reg%regmap(ilay)%nodmap)) deallocate(reg%regmap(ilay)%nodmap)
          if (associated(reg%regmap(ilay)%bndmap)) deallocate(reg%regmap(ilay)%bndmap)
        end do
        deallocate(reg%regmap)
      end if
    end do
    !
    deallocate(this%nreg); this%nreg => null()
    deallocate(this%reg);  this%reg  => null()
    !
    return
  end subroutine mf6_mod_clean_regions
  
  function mf6_mod_count_i1a(this, i1a) result(cnt)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    integer(i1b), dimension(:), intent(in) :: i1a
    integer(i4b), dimension(gnlay) :: cnt
    ! -- local
    type(tReg), pointer :: reg => null()
    type(tRegmap), pointer :: regmap => null()
    type(tBb), pointer :: bb => null()
    integer(i4b) :: ireg, il, ir, ic, n
! ------------------------------------------------------------------------------
    !
    cnt = 0
    !
    if (.not.associated(this%nreg)) return
    !
    do ireg = 1, this%nreg
      reg => this%reg(ireg)
      do il = 1, gnlay
        regmap => reg%regmap(il)
        bb => regmap%bb
        do ir = 1, bb%nrow
          do ic = 1, bb%nrow
            n = abs(regmap%nodmap(ic,ir))
            if (n > 0) then
              if (i1a(n) == 1) then
                cnt(il) = cnt(il) + 1
              end if
            end if
          end do
        end do
      end do
    end do
    !
    call logmsg('#cells assigned to layers: '//ta(cnt))
    !
    return
  end function mf6_mod_count_i1a
  
  subroutine mf6_mod_get_model_name(this, i, modelname)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    integer(i4b), intent(in) :: i
    character(len=*), intent(out) :: modelname
    ! -- local
! ------------------------------------------------------------------------------
    write(modelname,'(a,i5.5)') 'm', i
    !
    return
  end subroutine mf6_mod_get_model_name
  
  subroutine mf6_mod_set_disu(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    ! -- local
    type tCellDat
      integer(i4b) :: ic = 0
      integer(i4b) :: ir = 0
      integer(i4b) :: nrl = 0
    end type tCellDat
    type(tCellDat), dimension(:), allocatable :: celldat
    !
    character(len=mxslen) :: f
    real(r8b), parameter :: thkmin = 0.1d0
    type(tDisu), pointer :: disu => null()
    
    integer(i4b), dimension(:,:,:), allocatable :: nod3d
    integer(i4b) :: iact, il, ir, ic, jl, jr, jc, kl, kr, kc
    integer(i4b) :: i, j, n, m, nja, nr, nc, nrl, mrl
    integer(i4b) :: gir, gic, gjr, gjc, nnbr, nlnbr, maxnrl, np, mp, kp, iside
    integer(i4b) :: jr0, jr1, jc0, jc1, ic0, ir0
    integer(i4b), dimension(gnlay) :: top_i_raw, bot_i_raw, dz_i_raw
    real(r8b) :: topval, botval, dzval, r8val, r8sum
    !
    integer(i4b), parameter :: maxnbr = 10
    integer(i4b), dimension(1+maxnbr) :: ihc
    integer(i4b), dimension(1+maxnbr) :: st
    real(r8b), dimension(1+maxnbr) :: hwva, cl12
    !
    integer(i4b) :: ihc_val
    real(r8b) :: hwva_val, cl12_val
    !
    logical :: ldz
! ------------------------------------------------------------------------------
    !
    allocate(this%disu)
    disu => this%disu
    !
    call clear_wrk()
    !
    maxnrl = 0
    do il = 1, gnlay
      maxnrl = max(maxnrl,2**rlev(il))
    end do
    allocate(i4wrk1d(maxnrl**2)); allocate(i4wrk1d2(10))
    !
    ! count the number of nodes
    allocate(disu%nodes); disu%nodes = 0
    do il = 1, gnlay
      nrl = 2**rlev(il); nr = this%nrow/nrl; nc = this%ncol/nrl
      disu%nodes = disu%nodes + nr*nc
    end do
    !
    ldz = .false.
    do il = 1, gnlay
      ldz = raw%exists('dz', il, 0)
      if (.not.ldz) exit
    end do
    !
    if (.not.ldz) then
      do il = 1, gnlay
        top_i_raw(il) = raw%mf6_raw_get_index('top', il, 0)
        bot_i_raw(il) = raw%mf6_raw_get_index('bot', il, 0)
      end do
    else
      do il = 1, gnlay
        top_i_raw(il) = raw%mf6_raw_get_index('top', 1, 0)
        dz_i_raw(il)  = raw%mf6_raw_get_index('dz', il, 0)
      end do
    end if
    !
    allocate(celldat(disu%nodes), i1wrk2d(2,disu%nodes))
    if (allocated(nod3d)) deallocate(nod3d)
    allocate(nod3d(this%ncol,this%nrow,gnlay))
    do il = 1, gnlay
      do ir = 1, this%nrow
        do ic = 1, this%ncol
          nod3d(ic,ir,il) = 0
        end do
      end do
    end do
    !
    allocate(disu%top(disu%nodes),disu%bot(disu%nodes))
    allocate(this%layer_nodes(gnlay))
    this%layer_nodes = 0
    disu%nodes = 0; this%layer_nodes = 0
    do il = 1, gnlay
      nrl = 2**rlev(il)
      nr = this%nrow/nrl; nc = this%ncol/nrl
      do ir = 1, nr
        do ic = 1, nc
          this%layer_nodes(il) = this%layer_nodes(il) + 1
          disu%nodes = disu%nodes + 1
          jc = (ic-1)*nrl + 1; jr = (ir-1)*nrl + 1
          celldat(disu%nodes)%ic  = jc
          celldat(disu%nodes)%ir  = jr
          celldat(disu%nodes)%nrl = nrl
          disu%top(disu%nodes) = r8nodata
          disu%bot(disu%nodes) = r8nodata
          do i = 1, nrl
            do j = 1, nrl
              jc = (ic-1)*nrl + j; jr = (ir-1)*nrl + i
              nod3d(jc,jr,il) = disu%nodes
            end do
          end do
        end do
      end do
      if (.false.) then
        f = 'i4wrk3d_l'//ta((/il/))
        call writeflt(f, nod3d(:,:,il), this%ncol, this%nrow, DZERO, DZERO, gcs, 0)
      end if
    end do
    !
    ! fill the top and bot values
    if (ldz) then
      ic0 = this%bb%ic0; ir0 = this%bb%ir0
      ! top first
      call logmsg('Determining top for layer 1...')
      il = 1
      do ir = 1, this%nrow
        do ic = 1, this%ncol
          np = nod3d(ic,ir,il)
          if (np == 0) cycle
          if (disu%top(np) == r8nodata) then
            gic = celldat(np)%ic; gir = celldat(np)%ir; nrl = celldat(np)%nrl
            topval = this%get_val(top_i_raw(il), il, gic+ic0-1, gir+ir0-1, nrl)
            if (topval == r8nodata) then
              call errmsg('Error: nodata top value.')
            end if
            disu%top(np) = topval
          end if
        end do
      end do
      !
      do il = 2, gnlay
        call logmsg('Determining top and bot for layer '//ta((/il/))//'...')
        do ir = 1, this%nrow
          do ic = 1, this%nrow
            np = nod3d(ic,ir,il)
            !if (np == 28673) then
            !  write(*,*) 'Break'
            !end if
            if (np == 0) cycle
            topval = disu%top(np); botval = disu%bot(np)
            if ((topval /= r8nodata).and.(botval /= r8nodata)) cycle
            !
            gic = celldat(np)%ic; gir = celldat(np)%ir; nrl = celldat(np)%nrl
            ! first check upper neighbor
            if (il > 1) then
              mp = nod3d(ic,ir,il-1) ! top node
              mrl = celldat(mp)%nrl !top
              !
              ! determine the scaled bot
              n = 0; botval = DZERO
              do jr = gir, gir+nrl-1
                do jc = gic, gic+nrl-1 ! loop over the top node cells
                  kp = nod3d(jc,jr,il-1)
                  topval = disu%top(kp)
                  dzval = this%get_val(dz_i_raw(il-1), il-1, jc+ic0-1, jr+ir0-1, 1)! mrl)
                  if ((topval /= r8nodata).and.(dzval /= r8nodata)) then
                    n = n + 1
                    botval = botval + (topval-dzval)
                  end if
                end do
              end do
              botval = botval/n
              !
              ! correct is neccary
              dzval = huge(dzval)
              do jr = gir, gir+nrl-1
                do jc = gic, gic+nrl-1 ! loop over the top node cells
                  kp = nod3d(jc,jr,il-1)
                  topval = disu%top(kp)
                  if (topval == r8nodata) then
                    call errmsg('Program error: Top is no data.')
                  end if
                  dzval = min(dzval,topval-botval)
                end do
              end do
              if (dzval < DZERO) then
                botval = botval - abs(dzval) - thkmin
              end if
              do jr = gir, gir+nrl-1
                do jc = gic, gic+nrl-1 ! loop over the top node cells
                  kp = nod3d(jc,jr,il-1)
                  if (disu%bot(kp) == r8nodata) disu%bot(kp) = botval
                end do
              end do
              if (disu%top(np) == r8nodata) disu%top(np) = botval
              if (il == gnlay) then
                dzval = this%get_val(dz_i_raw(il), il, gic+ic0-1, gir+ir0-1, 1) ! nrl)
                if (disu%bot(np) == r8nodata) disu%bot(np) = disu%top(np) - dzval
              end if
            end if
          end do
        end do
      end do
      !
    else
      call errmsg('Not yet supported.')
    end if
    !
    ! check consistency
    do n = 1, disu%nodes
      topval = disu%top(n); botval = disu%bot(n)
      if ((topval == r8nodata).or.(botval == r8nodata)) then
        call errmsg('Program error: inconsistent top/bot.')
      end if
      dzval = topval - botval
      if (dzval < DZERO) then
        call errmsg('Program error: negative layer thickness.')
      end if
    end do
    !
    ! debug
    if (.false.) then
      allocate(r8wrk2d(this%nrow,this%ncol),r8wrk2d2(this%nrow,this%ncol))
      do il = 1, gnlay
        do ir = 1, this%nrow
          do ic = 1, this%nrow
            r8wrk2d(ic,ir) = r8nodata
            np = nod3d(ic,ir,il)
            if (np == 0) cycle
            r8wrk2d(ic,ir)  = disu%top(np)
            r8wrk2d2(ic,ir) = disu%bot(np)
          end do
        end do
        f = 'top_'//ta((/il/))
        call writeflt(f, r8wrk2d, this%ncol, this%nrow, DZERO, DZERO, gcs, r8nodata)
        f = 'bot_'//ta((/il/))
        call writeflt(f, r8wrk2d2, this%ncol, this%nrow, DZERO, DZERO, gcs, r8nodata)
      end do
      stop
    end if
    !
    ! iac
    allocate(disu%iac(disu%nodes))
    allocate(disu%nja)
    !
    do iact = 1, 2
      do n = 1, disu%nodes
        disu%iac(n) = 0
      end do
      disu%nja = 0
      do i = 1, disu%nodes
        i1wrk2d(:,i) = 0
      end do
      !
      do il = 1, gnlay
        nrl = 2**rlev(il)
        do ir = 1, this%nrow
          do ic = 1, this%ncol
            !
            np = nod3d(ic,ir,il) ! get the node number
            !
            if (i1wrk2d(1,np) == 1) cycle ! node is already processed
            i1wrk2d(1,np) = 1
            !
            gic = celldat(np)%ic; gir = celldat(np)%ir; nrl = celldat(np)%nrl
            topval = disu%top(np); botval = disu%bot(np)
            !
            ! set for the center node
            st(1)   = np
            ihc(1)  = 0
            cl12(1) = DZERO
            hwva(1) = DZERO
            ! 
            nnbr = 0 ! number of neighboring nodes
            do iside = 2, ns
              select case(iside)
              case(jn) !N
                jr0 = gir-1; jr1 = jr0
                jc0 = gic; jc1 = gic + nrl - 1
                kl = il
              case(js) !S
                jr0 = gir + nrl; jr1 = jr0
                jc0 = gic; jc1 = gic + nrl - 1
                kl = il
              case(jw) !W
                jr0 = gir; jr1 = gir + nrl - 1
                jc0 = gic - 1; jc1 = jc0
                kl = il
              case(je) !E
                jr0 = gir; jr1 = gir + nrl - 1
                jc0 = gic + nrl; jc1 = jc0
                kl = il
              case(jt) !T
                jr0 = gir; jr1 = gir + nrl - 1
                jc0 = gic; jc1 = gic + nrl - 1
                kl = il - 1
              case(jb) !B
                jr0 = gir; jr1 = gir + nrl - 1
                jc0 = gic; jc1 = gic + nrl - 1
                kl = il + 1
              end select
              !
              if ((jr0 < 1).or.(jr0 > this%nrow)) cycle
              if ((jr1 < 1).or.(jr1 > this%nrow)) cycle
              if ((jc0 < 1).or.(jc0 > this%ncol)) cycle
              if ((jc1 < 1).or.(jc1 > this%ncol)) cycle
              if ((kl < 1).or.(kl > gnlay)) cycle
              !
              select case(iside)
              case(jn,js,jw,je)
                ihc_val  = 1
                cl12_val = gcs*nrl/2.d0
                hwva_val = gcs*nrl
              case(jt,jb)
                ihc_val  = 0
                cl12_val = max(thkmin, topval-botval)/2.d0
                mrl = min(nrl,2**rlev(kl))
                hwva_val = (gcs*mrl)**2
              end select
              !
              ! get non-unique neighboring node numbers
              n = 0
              do jr = jr0, jr1
                do jc = jc0, jc1
                  m = nod3d(jc,jr,kl) ! neighboring node number
                  n = n + 1
                  i4wrk1d(n) = m
                end do
              end do
              !
              ! get unique neighboring node numbers
              nlnbr = 0
              do i = 1, n
                m = i4wrk1d(i)
                if (i1wrk2d(2,m) == 0) then
                  i1wrk2d(2,m) = 1
                  nlnbr = nlnbr + 1
                  i4wrk1d2(nlnbr) = m
                end if
              end do
              !
              ! set the neighboring data and reset
              do i = 1, nlnbr
                m = i4wrk1d2(i)
                nnbr = nnbr + 1
                st(1+nnbr)   = m
                ihc(1+nnbr)  = ihc_val
                cl12(1+nnbr) = cl12_val
                hwva(1+nnbr) = hwva_val
                !
                i1wrk2d(2,m) = 0
              end do
            end do
            !
            do i = 1, 1+nnbr
              disu%iac(np) = disu%iac(np) + 1
              disu%nja = disu%nja + 1
              if (iact == 2) then
                if ((disu%nja < 1).or.(disu%nja > nja)) then
                 call errmsg('mf6_mod_set_disu: program error')
                end if
                disu%ja(disu%nja)   = st(i)
                disu%ihc(disu%nja)  = ihc(i)
                disu%cl12(disu%nja) = cl12(i)
                disu%hwva(disu%nja) = hwva(i)
              end if
            end do
          end do
        end do
      end do
      if (iact == 1) then
        nja = disu%nja
        allocate(disu%ja(disu%nja))
        allocate(disu%ihc(disu%nja))
        allocate(disu%cl12(disu%nja))
        allocate(disu%hwva(disu%nja))
        do i = 1, disu%nja
          disu%ihc(i) = 0
        end do
      end if
    end do ! iact
    !
    deallocate(nod3d)
    !
    return
  end subroutine mf6_mod_set_disu

  subroutine mf6_mod_write_array_i4(this, iu, nx, f, arr, lbin, lbinpos_in)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    integer(i4b), intent(in) :: iu 
    integer(i4b), intent(in) :: nx
    character(len=*), intent(inout) :: f
    integer(i4b), dimension(:), intent(in) :: arr
    logical, intent(in) :: lbin
    logical, intent(in), optional :: lbinpos_in
    ! -- local
    integer(i4b), parameter :: i4dum = 1
    real(r8b), parameter :: r8dum = DZERO
    character(len=16) :: c16dum = ''
    integer(i4b) :: ju, i, narr
    integer(i8b) :: p0, p1
    character(len=mxslen) :: fmt
    logical :: lbinpos
! ------------------------------------------------------------------------------
    if (present(lbinpos_in)) then
      lbinpos = lbinpos_in
    else
      lbinpos = .false.
    end if
    !    
    write(fmt,'(a,i,a)') '(',nx,'x,a)'
    if (lbin) then
      if (.not.lbinpos) then
        f = trim(f)//'.bin'
        call open_file(f, ju, 'w', .true.)
      else
        f = this%fbin
        ju = this%iubin
      end if
      narr = size(arr)
      if (lbinpos) inquire(ju,pos=p0)
      write(ju) i4dum, i4dum, r8dum, r8dum, c16dum, narr, i4dum, i4dum
      write(ju) arr
      if (lbinpos) then
        inquire(ju,pos=p1)
      else
        close(ju)
      end if
      !call get_rel_up(f, 2)
      if (.not.lbinpos) then
        write(iu,fmt) 'OPEN/CLOSE '//trim(f)//' (BINARY)'
      else
        write(iu,fmt) 'OPEN/CLOSE '//trim(f)//' (BINARY) '//ta((/p0,p1/))
      end if
    else
      f = trim(f)//'.asc'
      call open_file(f, ju, 'w')
      do i = 1, size(arr)
        write(ju,'(a)') ta((/arr(i)/))
      end do
      close(ju)
      !call get_rel_up(f, 2)
      write(iu,fmt) 'OPEN/CLOSE '//trim(f)
    end if
    !
    return
  end subroutine mf6_mod_write_array_i4
  
  subroutine mf6_mod_write_array_r8(this, iu, nx, f, arr, lbin, lbinpos_in)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    integer(i4b), intent(in) :: iu 
    integer(i4b), intent(in) :: nx
    character(len=*), intent(inout) :: f
    real(r8b), dimension(:), intent(in) :: arr
    logical, intent(in) :: lbin
    logical, intent(in), optional :: lbinpos_in
    ! -- local
    integer(i4b), parameter :: i4dum = 1
    real(r8b), parameter :: r8dum = DZERO
    character(len=16) :: c16dum = ''
    integer(i4b) :: ju, i, narr
    integer(i8b) :: p0, p1
    character(len=mxslen) :: fmt
    logical :: lbinpos
! ------------------------------------------------------------------------------
    if (present(lbinpos_in)) then
      lbinpos = lbinpos_in
    else
      lbinpos = .false.
    end if
    !
    write(fmt,'(a,i,a)') '(',nx,'x,a)'
    if (lbin) then
      if (.not.lbinpos) then
        f = trim(f)//'.bin'
        call open_file(f, ju, 'w', .true.)
      else
        f = this%fbin
        ju = this%iubin
      end if
      if (lbinpos) inquire(ju,pos=p0)
      narr = size(arr)
      write(ju) i4dum, i4dum, r8dum, r8dum, c16dum, narr, i4dum, i4dum
      write(ju) arr
      if (lbinpos) then
        inquire(ju,pos=p1)
      else
        close(ju)
      end if
      !call get_rel_up(f, 2)
      if (.not.lbinpos) then
        write(iu,fmt) 'OPEN/CLOSE '//trim(f)//' (BINARY)'
      else
        write(iu,fmt) 'OPEN/CLOSE '//trim(f)//' (BINARY) '//ta((/p0,p1/))
      end if
    else
      f = trim(f)//'.asc'
      call open_file(f, ju, 'w')
      do i = 1, size(arr)
        write(ju,'(a)') ta((/arr(i)/))
      end do
      close(ju)
      !call get_rel_up(f, 2)
      write(iu,fmt) 'OPEN/CLOSE '//trim(f)
    end if
    !
    return
  end subroutine mf6_mod_write_array_r8
  
  subroutine mf6_mod_write_list_1(this, iu, nx, f, arrflg, arr, lbin, lbinpos, s)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    integer(i4b), intent(in) :: iu 
    integer(i4b), intent(in) :: nx
    character(len=*), intent(inout) :: f
    integer(i1b), dimension(:), intent(in) :: arrflg
    real(r8b), dimension(:), intent(in) :: arr
    logical, intent(in) :: lbin
    logical, intent(in) :: lbinpos
    character(len=*), intent(out), optional :: s
    ! -- local
    integer(i4b), dimension(:), allocatable :: i4w
    real(r8b), dimension(:), allocatable :: r8w1
    integer(i4b) :: ju, i, n
    integer(i8b) :: p0, p1
    character(len=mxslen) :: fmt
    logical :: liu
! ------------------------------------------------------------------------------
    !
    if (present(s)) then
      liu = .false.
    else
      liu = .true.
    end if
    !
    write(fmt,'(a,i,a)') '(',nx,'x,a)'
    if (lbin) then
      if (.not.lbinpos) then
        f = trim(f)//'.bin'
        call open_file(f, ju, 'w', .true.)
      else
        f = this%fbin
        ju = this%iubin
      end if
      n = 0
      do i = 1, size(arrflg)
        if (arrflg(i) == 1) then
          n = n + 1
        end if
      end do
      if (lbinpos) then
        inquire(ju,pos=p0)
      end if
      if (n > 0) then
        allocate(i4w(n),r8w1(n))
        n = 0
        do i = 1, size(arrflg)
          if (arrflg(i) == 1) then
            n = n + 1
            i4w(n) = i; r8w1(n) = arr(i)
          end if
        end do
        write(ju)((i4w(i),r8w1(i)),i=1,n)
        deallocate(i4w,r8w1)
      end if
      if (lbinpos) then
        inquire(ju,pos=p1)
      else
        close(ju)
      end if
      !call get_rel_up(f, 2)
      if (liu) then
        if (lbinpos) then
          write(iu,fmt) 'OPEN/CLOSE '//trim(f)// ' (BINARY) '//ta((/p0,p1/))
        else
          write(iu,fmt) 'OPEN/CLOSE '//trim(f)// ' (BINARY)'
        end if
      else
        if (lbinpos) then
          write(s,fmt) 'OPEN/CLOSE '//trim(f)// ' (BINARY) '//ta((/p0,p1/))
        else
          write(s,fmt) 'OPEN/CLOSE '//trim(f)// ' (BINARY)'
        end if
      end if
    else
      f = trim(f)//'.asc'
      call open_file(f, ju, 'w')
      do i = 1, size(arrflg)
        if (arrflg(i) == 1) then
          write(ju,'(a)') ta((/i/))//' '//ta((/arr(i)/))
        end if
      end do
      close(ju)
      !call get_rel_up(f, 2)
      if (liu) then
        write(iu,fmt) 'OPEN/CLOSE '//trim(f)
      else
        write(s,fmt) 'OPEN/CLOSE '//trim(f)
      end if
    end if
    !
    return
  end subroutine mf6_mod_write_list_1
  
  subroutine mf6_mod_write_list_2(this, iu, nx, f, arrflg, arr, arr2, lbin, lbinpos, s)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    integer(i4b), intent(in) :: iu 
    integer(i4b), intent(in) :: nx
    character(len=*), intent(inout) :: f
    integer(i1b), dimension(:), intent(in) :: arrflg
    real(r8b), dimension(:), intent(in) :: arr
    real(r8b), dimension(:), intent(in) :: arr2
    logical, intent(in) :: lbin
    logical, intent(in) :: lbinpos
    character(len=*), intent(out), optional :: s
    ! -- local
    integer(i4b), dimension(:), allocatable :: i4w
    real(r8b), dimension(:), allocatable :: r8w1, r8w2
    integer(i4b) :: ju, i, n
    integer(i8b) :: p0, p1
    character(len=mxslen) :: fmt
    logical :: liu
! ------------------------------------------------------------------------------
    !
    if (present(s)) then
      liu = .false.
    else
      liu = .true.
    end if
    !
    write(fmt,'(a,i,a)') '(',nx,'x,a)'
    if (lbin) then
      if (.not.lbinpos) then
        f = trim(f)//'.bin'
        call open_file(f, ju, 'w', .true.)
      else
        f = this%fbin
        ju = this%iubin
      end if
      n = 0
      do i = 1, size(arrflg)
        if (arrflg(i) == 1) then
          n = n + 1
        end if
      end do
      if (lbinpos) then
        inquire(ju,pos=p0)
      end if
      if (n > 0) then
        allocate(i4w(n),r8w1(n),r8w2(n))
        n = 0
        do i = 1, size(arrflg)
          if (arrflg(i) == 1) then
            n = n + 1
            i4w(n) = i; r8w1(n) = arr(i); r8w2(n) = arr2(i)
          end if
        end do
        write(ju)((i4w(i),r8w1(i),r8w2(i)),i=1,n)
        deallocate(i4w,r8w1,r8w2)
      end if
      if (lbinpos) then
        inquire(ju,pos=p1)
      else
        close(ju)
      end if
      !call get_rel_up(f, 2)
      if (liu) then
        if (lbinpos) then
          write(iu,fmt) 'OPEN/CLOSE '//trim(f)// ' (BINARY) '//ta((/p0,p1/))
        else
          write(iu,fmt) 'OPEN/CLOSE '//trim(f)// ' (BINARY)'
        end if
      else
        if (lbinpos) then
          write(s,fmt) 'OPEN/CLOSE '//trim(f)// ' (BINARY) '//ta((/p0,p1/))
        else
          write(s,fmt) 'OPEN/CLOSE '//trim(f)// ' (BINARY)'
        end if
      end if
    else
      f = trim(f)//'.asc'
      call open_file(f, ju, 'w')
      do i = 1, size(arrflg)
        if (arrflg(i) == 1) then
          write(ju,'(a)') ta((/i/))//' '//ta((/arr(i)/))//' '//ta((/arr2(i)/))
        end if
      end do
      close(ju)
      !call get_rel_up(f, 2)
      if (liu) then
        write(iu,fmt) 'OPEN/CLOSE '//trim(f)
      else
        write(s,fmt) 'OPEN/CLOSE '//trim(f)
      end if
    end if
    !
    return
  end subroutine mf6_mod_write_list_2
  
  subroutine mf6_mod_write_list_3(this, iu, nx, f, arrflg, arr, arr2, arr3, &
    lbin, lbinpos, s)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    integer(i4b), intent(in) :: iu 
    integer(i4b), intent(in) :: nx
    character(len=*), intent(inout) :: f
    integer(i1b), dimension(:), intent(in) :: arrflg
    real(r8b), dimension(:), intent(in) :: arr
    real(r8b), dimension(:), intent(in) :: arr2
    real(r8b), dimension(:), intent(in) :: arr3
    logical, intent(in) :: lbin
    logical, intent(in) :: lbinpos
    character(len=*), intent(out), optional :: s
    ! -- local
    integer(i4b), dimension(:), allocatable :: i4w
    real(r8b), dimension(:), allocatable :: r8w1, r8w2, r8w3
    integer(i4b) :: ju, i, n
    integer(i8b) :: p0, p1
    character(len=mxslen) :: fmt
    logical :: liu
! ------------------------------------------------------------------------------
    if (present(s)) then
      liu = .false.
    else
      liu = .true.
    end if
    !
    write(fmt,'(a,i,a)') '(',nx,'x,a)'
    if (lbin) then
      if (.not.lbinpos) then
        f = trim(f)//'.bin'
        call open_file(f, ju, 'w', .true.)
      else
        f = this%fbin
        ju = this%iubin
      end if
      n = 0
      do i = 1, size(arrflg)
        if (arrflg(i) == 1) then
          n = n + 1
        end if
      end do
      if (lbinpos) then
        inquire(ju,pos=p0)
      end if
      if (n > 0) then
        allocate(i4w(n),r8w1(n),r8w2(n),r8w3(n))
        n = 0
        do i = 1, size(arrflg)
          if (arrflg(i) == 1) then
            n = n + 1
            i4w(n) = i; r8w1(n) = arr(i); r8w2(n) = arr2(i); r8w3(n) = arr3(i)
          end if
        end do
        write(ju)((i4w(i),r8w1(i),r8w2(i),r8w3(i)),i=1,n)
        deallocate(i4w,r8w1,r8w2,r8w3)
      end if
      if (lbinpos) then
        inquire(ju,pos=p1)
      else
        close(ju)
      end if
      !call get_rel_up(f, 2)
      if (liu) then
        if (lbinpos) then
          write(iu,fmt) 'OPEN/CLOSE '//trim(f)// ' (BINARY) '//ta((/p0,p1/))
        else
          write(iu,fmt) 'OPEN/CLOSE '//trim(f)// ' (BINARY)'
        end if
      else
        if (lbinpos) then
          write(s,fmt) 'OPEN/CLOSE '//trim(f)// ' (BINARY) '//ta((/p0,p1/))
        else
          write(s,fmt) 'OPEN/CLOSE '//trim(f)// ' (BINARY)'
        end if
      end if
    else
      f = trim(f)//'.asc'
      call open_file(f, ju, 'w')
      do i = 1, size(arrflg)
        if (arrflg(i) == 1) then
          write(ju,'(a)') ta((/i/))//' '// &
            ta((/arr(i)/))//' '//ta((/arr2(i)/))//' '//ta((/arr3(i)/))
        end if
      end do
      close(ju)
      !call get_rel_up(f, 2)
      if (liu) then
        write(iu,fmt) 'OPEN/CLOSE '//trim(f)
      else
        write(s,fmt) 'OPEN/CLOSE '//trim(f)
      end if
    end if
    !
    return
  end subroutine mf6_mod_write_list_3

  subroutine mf6_mod_write(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    !
    ! -- local
    logical, parameter :: lbin    = .true.
    logical, parameter :: lbinpos = .true.
    character(len=mxslen) :: f, pack
    integer(i4b) :: iu
! ------------------------------------------------------------------------------
    call logmsg('**************************************************************')
    if (raw%nper == 1) then 
      call logmsg('***** Writing for steady-state model '//trim(this%modelname)//'...')
    else
      call logmsg('***** Writing for transient model '//trim(this%modelname)//'...')
    end if
    call logmsg('**************************************************************')
    !
    ! open the binary file
    if (lbinpos) then
      allocate(this%fbin, this%iubin)
      this%fbin = trim(this%bindir)//trim(this%modelname)//'.bin'
      call open_file(this%fbin, this%iubin, 'w', .true.)
    end if
    !
    ! set the defaults
    pckact = 1
    pckact(ioc)  = raw%geti('act_oc',idef=0)
    pckact(isto) = raw%geti('act_sto',idef=0)
    pckact(ichd) = raw%geti('act_chd',idef=0)
    pckact(iriv) = raw%geti('act_riv',idef=0)
    pckact(idrn) = raw%geti('act_drn',idef=0)
    pckact(ighb) = raw%geti('act_ghb',idef=0)
    pckact(irch) = raw%geti('act_rch',idef=0)
    pckact(iwel) = raw%geti('act_wel',idef=0)
    
    !
    call this%write_disu(lbin, lbinpos)
    call this%write_ic(lbin, lbinpos)
    call this%write_oc()
    call this%write_npf(lbin, lbinpos)
    call this%write_sto(lbin, lbinpos)
    call this%write_chd(lbin, lbinpos)
    call this%write_riv(lbin, lbinpos)
    call this%write_drn(lbin, lbinpos)
    call this%write_ghb(lbin, lbinpos)
    call this%write_rch(lbin, lbinpos)
    call this%write_wel(lbin, lbinpos)
    call this%write_nam()
    !
    ! close the binary file
    if (lbinpos) then
      close(this%iubin)
    end if
    !
    f = '..\..\log\done_'//trim(this%modelname)
    call open_file(f, iu, 'w')
    close(iu)
    !
    return
  end subroutine mf6_mod_write
  
 subroutine mf6_mod_write_nam(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    ! -- local
    character(len=mxslen) :: f, mn
    integer(i4b) :: iu, ipck, jpck
! ------------------------------------------------------------------------------
    !
    mn = this%modelname
    f = trim(this%rootdir)//trim(mn)//'.nam'
    call open_file(f, iu, 'w')
    write(iu,'(   a)') 'BEGIN OPTIONS'
    f = trim(resultslstdir)//'\'//trim(this%modelname)//'.lst'
    call swap_slash(f)
    write(iu,'(2x,a)') 'LIST '//trim(f)
    if (raw%geti('newton',idef=0) == 1) then
      write(iu,'(2x,a)') 'NEWTON UNDER_RELAXATION'
    end if
    write(iu,'(   a)') 'END OPTIONS'
    write(iu,'(a)')
    write(iu,'(   a)') 'BEGIN PACKAGES'
    do ipck = 3, npck
      if (pckact(ipck) == 0) cycle
      f = trim(this%rootdir)//trim(mn)//'.'//trim(pck(ipck))
      call swap_slash(f)
      write(iu,'(2x,a)') trim(change_case(pck(ipck),'u'))//'6 '//trim(f)
    end do
    write(iu,'(   a)') 'END PACKAGES'
    close(iu)
    !
    return
  end subroutine mf6_mod_write_nam
  
  subroutine mf6_mod_write_disu(this, lbin, lbinpos)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    logical, intent(in) :: lbin
    logical, intent(in) :: lbinpos
    ! -- local
    integer(i4b), dimension(:,:,:), allocatable :: nod3d
    type(tDisu), pointer :: disu
    character(len=mxslen) :: p, pb, f
    integer(i4b) :: il, ir, ic, n, iu, nrl
    real(r8b) :: topval, botval
! ------------------------------------------------------------------------------
    !
    if (pckact(idisu) == 0) return
    call logmsg('Processing for '//trim(pck(idisu))//'...')
    !
    call clear_wrk()
    !
    ! set disu
    call this%set_disu()
    if (allocated(nod3d)) deallocate(nod3d)
    disu => this%disu
    !
    p = trim(this%rootdir)//trim(this%modelname)
    if (lbin) then
      pb =  trim(this%bindir)//trim(this%modelname)
    else
      pb = p
    end if
    !
    f = trim(p)//'.disu'
    call open_file(f, iu, 'w')
    !
    write(iu,'(   a)') 'BEGIN OPTIONS'
    write(iu,'(2x,a)') 'LENGTH_UNITS METERS'
    write(iu,'(2x,a)') 'NOGRB'
    write(iu,'(   a)') 'END OPTIONS'
    write(iu,'(a)')
    write(iu,'(   a)') 'BEGIN DIMENSIONS'
    write(iu,'(2x,a)') 'NODES '//ta((/disu%nodes/))
    write(iu,'(2x,a)') 'NJA '//ta((/disu%nja/))
    write(iu,'(   a)') 'END DIMENSIONS'
    write(iu,'(a)')
    write(iu,'(   a)') 'BEGIN GRIDDATA'
    !
    write(iu,'(2x,a)') 'TOP'
    f = trim(pb)//'.disu.top'; call this%write_array(iu, 4, f, disu%top, lbin, lbinpos)
    write(iu,'(2x,a)') 'BOT'
    f = trim(pb)//'.disu.bot'; call this%write_array(iu, 4, f, disu%bot, lbin, lbinpos)
    call clear_wrk()
    write(iu,'(2x,a)') 'AREA'
    write(iu,'(4x,a)') 'CONSTANT '//ta((/gcs*gcs/))
    write(iu,'(   a)') 'END GRIDDATA'
    write(iu,'(a)')
    write(iu,'(   a)') 'BEGIN CONNECTIONDATA'
    write(iu,'(2x,a)') 'IAC'
    f = trim(pb)//'.disu.iac'; call this%write_array(iu, 4, f, disu%iac, lbin, lbinpos)
    write(iu,'(2x,a)') 'JA'
    f = trim(pb)//'.disu.ja'; call this%write_array(iu, 4, f, disu%ja, lbin, lbinpos)
    write(iu,'(2x,a)') 'IHC'
    f = trim(pb)//'.disu.ihc'; call this%write_array(iu, 4, f, disu%ihc, lbin, lbinpos)
    write(iu,'(2x,a)') 'CL12'
    f = trim(pb)//'.disu.cl12'; call this%write_array(iu, 4, f, disu%cl12, lbin, lbinpos)
    write(iu,'(2x,a)') 'HWVA'
    f = trim(pb)//'.disu.hwva'; call this%write_array(iu, 4, f, disu%hwva, lbin, lbinpos)
    write(iu,'(   a)') 'END CONNECTIONDATA'
    close(iu)
    !
    ! clean up memory
    deallocate(disu%iac);  disu%iac  => null()
    deallocate(disu%ja);   disu%ja   => null()
    deallocate(disu%ihc);  disu%ihc  => null()
    deallocate(disu%cl12); disu%cl12 => null()
    deallocate(disu%hwva); disu%hwva => null()
    deallocate(disu%top);  disu%top  => null()
    deallocate(disu%bot);  disu%bot   => null()
    !
    return
  end subroutine mf6_mod_write_disu

  subroutine mf6_mod_write_ic(this, lbin, lbinpos)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    logical, intent(in) :: lbin
    logical, intent(in) :: lbinpos
    ! -- local
    character(len=mxslen) :: p, pb, f
    integer(i4b) :: iu
! ------------------------------------------------------------------------------
    !
    if (pckact(iic) == 0) return
    call logmsg('Processing for '//trim(pck(iic))//'...')
    !
    p = trim(this%rootdir)//trim(this%modelname)
    if (lbin) then
      pb =  trim(this%bindir)//trim(this%modelname)
    else
      pb = p
    end if
    
    f = trim(p)//'.ic'
    call open_file(f, iu, 'w')
    write(iu,'(   a)') 'BEGIN OPTIONS'
    write(iu,'(   a)') 'END OPTIONS'
    write(iu,'(a)')
    write(iu,'(   a)') 'BEGIN GRIDDATA'
    write(iu,'(2x,a)') 'STRT'
    write(iu,'(4x,a)') 'CONSTANT 0'
    write(iu,'(   a)') 'END GRIDDATA'
    close(iu)
    !
    return
  end subroutine mf6_mod_write_ic
  
  subroutine mf6_mod_write_oc(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    ! -- local
    character(len=mxslen) :: p, f
    integer(i4b) :: iu, iper
! ------------------------------------------------------------------------------
    if (pckact(ioc) == 0) return
    call logmsg('Processing for '//trim(pck(ioc))//'...')
    !
    p = trim(this%rootdir)//trim(this%modelname)
    !
    f = trim(p)//'.oc'
    call open_file(f, iu, 'w')
    write(iu,'(   a)') 'BEGIN OPTIONS'
    f = trim(resultsbindir)//'\'//trim(this%modelname)//'.'//trim(ctim)//'.hds'
    call swap_slash(f)
    write(iu,'(2x,a)') 'HEAD FILEOUT '//trim(f)
    write(iu,'(   a)') 'END OPTIONS'
    write(iu,'(a)')
    do iper = 1, raw%nper
      write(iu,'(   a)') 'BEGIN PERIOD '//ta((/iper/))
      write(iu,'(2x,a)') 'SAVE HEAD LAST'
      write(iu,'(   a)') 'END PERIOD'
    end do
    close(iu)
    !
    return
  end subroutine mf6_mod_write_oc
  
  subroutine mf6_mod_write_npf(this, lbin, lbinpos)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    logical, intent(in) :: lbin
    logical, intent(in) :: lbinpos
    ! -- local
    logical :: lconst
    character(len=mxslen) :: p, pb, f
    integer(i4b) :: ilay, iu, icelltype
! ------------------------------------------------------------------------------
    if (pckact(inpf) == 0) return
    call logmsg('Processing for '//trim(pck(inpf))//'...')
    !
    call clear_wrk()
    !
    p = trim(this%rootdir)//trim(this%modelname)
    if (lbin) then
      pb =  trim(this%bindir)//trim(this%modelname)
    else
      pb = p
    end if
    !
    f = trim(p)//'.npf'
    call open_file(f, iu, 'w')
    !
    write(iu,'(   a)') 'BEGIN OPTIONS'
    write(iu,'(   a)') 'END OPTIONS'
    write(iu,'(a)')
    write(iu,'(   a)') 'BEGIN GRIDDATA'
    write(iu,'(2x,a)') 'ICELLTYPE'
    icelltype = raw%geti('icelltype',idef=0)
    write(iu,'(4x,a)') 'CONSTANT '//ta((/icelltype/))
    !
    write(iu,'(2x,a)') 'K'
    do ilay = 1, gnlay
      call logmsg('k layer '//ta((/ilay/))//'...')
      call this%get_array('k', ilay, 0, ilay, i1wrk, r8wrk)
    end do
    f = trim(pb)//'.npf.k'; call this%write_array(iu, 4, f, r8wrk, lbin, lbinpos)
    call clear_wrk()
    !
    write(iu,'(2x,a)') 'K33'
    if (gnlay > 1) then
      do ilay = 1, gnlay
        call logmsg('k33 layer '//ta((/ilay/))//'...')
        call this%get_array('k33', ilay, 0, ilay, i1wrk, r8wrk)
      end do
      f = trim(pb)//'.npf.k33'; call this%write_array(iu, 4, f, r8wrk, lbin, lbinpos)
      call clear_wrk()
    else
      write(iu,'(4x,a)') 'CONSTANT 0.1'
    end if
    !
    write(iu,'(   a)') 'END GRIDDATA'
    close(iu)
    !
    return
  end subroutine mf6_mod_write_npf
  
  subroutine mf6_mod_write_sto(this, lbin, lbinpos)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    logical, intent(in) :: lbin
    logical, intent(in) :: lbinpos
    ! -- local
! ------------------------------------------------------------------------------
    !
    if (pckact(isto) == 0) return
    call logmsg('Processing for '//trim(pck(isto))//'...')
    !
    call errmsg('STO package not yet supported.')
    !
    return
  end subroutine mf6_mod_write_sto
  
 subroutine mf6_mod_write_chd(this, lbin, lbinpos)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    logical, intent(in) :: lbin
    logical, intent(in) :: lbinpos
    ! -- local
    character(len=mxslen), dimension(:), allocatable :: cwk
    character(len=mxslen) :: p, pb, f
    integer(i4b) :: i, n, iu, nbound, maxbound, iper, jper, nper, nperspu, ilay
    integer(i4b), dimension(gnlay) :: nbound_lay
    logical, dimension(:), allocatable :: lact
! ------------------------------------------------------------------------------
    !
    if (pckact(ichd) == 0) return
    call logmsg('Processing for '//trim(pck(ichd))//'...')
    !
    call clear_wrk()
    !
    p = trim(this%rootdir)//trim(this%modelname)
    if (lbin) then
      pb =  trim(this%bindir)//trim(this%modelname)
    else
      pb = p
    end if
    !
    ! write all binary files and store the file strings
    nper = raw%nper
    allocate(cwk(nper), lact(nper))
    maxbound = 0
    do iper = 1, nper
      do ilay = 1, gnlay
        call this%get_array('chd_head', ilay, iper, ilay, i1wrk, r8wrk)
      end do
      !
      nbound_lay = this%count_i1a(i1wrk); nbound = sum(nbound_lay)
      maxbound = max(nbound,maxbound)
      if (nbound == 0) then
        lact(iper) = .false.
        call logmsg('No drains found.')
      else
        lact(iper) = .true.
        f = trim(pb)//'.chd.sp'//ta((/iper/),'(i3.3)')
        call this%write_list(iu, 2, f, i1wrk, r8wrk, lbin, lbinpos, cwk(iper))
      end if
      call clear_wrk()
    end do
    !
    if (maxbound == 0) then
      call logmsg('No constant heads found.')
      pckact(idrn) = 0
      return
    end if
    
    f = trim(p)//'.chd'
    call open_file(f, iu, 'w')
    write(iu,'(   a)') 'BEGIN OPTIONS'
    write(iu,'(   a)') 'END OPTIONS'
    write(iu,'(a)')
    write(iu,'(   a)') 'BEGIN DIMENSIONS'
    write(iu,'(2x,a)') 'MAXBOUND '//ta((/maxbound/))
    write(iu,'(   a)') 'END DIMENSIONS'
    write(iu,'(a)')
    do iper = 1, nper
      write(iu,'(   a)') 'BEGIN PERIOD '//ta((/iper/))
      if (lact(iper)) write(iu,'(a)') trim(cwk(iper))
      write(iu,'(   a)') 'END PERIOD'
    end do
    close(iu)
    !
    if (ltransient) then
      f = trim(p)//'.chd'
      call open_file(f, iu, 'w')
      write(iu,'(   a)') 'BEGIN OPTIONS'
      write(iu,'(   a)') 'END OPTIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN DIMENSIONS'
      write(iu,'(2x,a)') 'MAXBOUND '//ta((/maxbound/))
      write(iu,'(   a)') 'END DIMENSIONS'
      write(iu,'(a)')
      nperspu = raw%geti('nyear_spinup')*12
      do iper = 1, nper
        write(iu,'(   a)') 'BEGIN PERIOD '//ta((/iper/))
        jper = mod(iper, nperspu)
        if (jper == 0) jper = nperspu
        if (lact(jper)) write(iu,'(a)') trim(cwk(jper))
        write(iu,'(   a)') 'END PERIOD'
      end do
      close(iu)
    end if
    !
    deallocate(cwk, lact)
    call clear_wrk()
    !
    return
  end subroutine mf6_mod_write_chd
  
  subroutine mf6_mod_write_riv(this, lbin, lbinpos)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    logical, intent(in) :: lbin
    logical, intent(in) :: lbinpos
    ! -- local
! ------------------------------------------------------------------------------
    !
    if (pckact(iriv) == 0) return
    call logmsg('Processing for '//trim(pck(iriv))//'...')
    !
    call errmsg('RIV package not yet supported.')
    !
    return
  end subroutine mf6_mod_write_riv
 
  subroutine mf6_mod_write_drn(this, lbin, lbinpos)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    logical, intent(in) :: lbin
    logical, intent(in) :: lbinpos
    ! -- local
    character(len=mxslen), dimension(:), allocatable :: cwk
    character(len=mxslen) :: p, pb, f
    integer(i4b) :: i, n, iu, nbound, maxbound, iper, jper, nper, nperspu, ilay
    integer(i4b), dimension(gnlay) :: nbound_lay
    logical, dimension(:), allocatable :: lact
! ------------------------------------------------------------------------------
    !
    if (pckact(idrn) == 0) return
    call logmsg('Processing for '//trim(pck(idrn))//'...')
    !
    call clear_wrk()
    !
    p = trim(this%rootdir)//trim(this%modelname)
    if (lbin) then
      pb =  trim(this%bindir)//trim(this%modelname)
    else
      pb = p
    end if
    !
    ! write all binary files and store the file strings
    nper = raw%nper
    allocate(cwk(nper), lact(nper))
    maxbound = 0
    do iper = 1, nper
      do ilay = 1, gnlay
        call this%get_array('drn_stage', ilay, iper, ilay, i1wrk, r8wrk, ib_in=2) !i_drn_elev_l1
        call this%get_array('drn_cond', ilay, iper, ilay, i1wrk, r8wrk2, ib_in=2)
      end do
      !
      ! check
      do i = 1, size(i1wrk)
        if (r8wrk(i) < -1000D0) then
          call errmsg("Invalid range for drain.")
        end if
      end do
      !
      ! filter for zero conductance
      n = 0
      do i = 1, size(i1wrk)
        if ((r8wrk2(i) == DZERO) .or. (r8wrk2(i) == r8nodata)) then
          if (i1wrk(i) == 1) n = n + 1
          i1wrk(i) = 0
        end if
        if ((i1wrk(i) == 1).and.(r8wrk2(i) < DZERO)) then
          call errmsg("Negative drain conductance.")
        end if
      end do
      if (n > 0) then
        call logmsg('Removed '//ta((/n/))//' drains with zero conductance.')
      end if
      !
      nbound_lay = this%count_i1a(i1wrk); nbound = sum(nbound_lay)
      maxbound = max(nbound,maxbound)
      if (nbound == 0) then
        lact(iper) = .false.
        call logmsg('No drains found.')
      else
        lact(iper) = .true.
        f = trim(pb)//'.drn.sp'//ta((/iper/),'(i3.3)')
        call this%write_list(iu, 4, f, i1wrk, r8wrk, r8wrk2, lbin, lbinpos, cwk(iper))
      end if
      call clear_wrk()
    end do
    !
    if (maxbound == 0) then
      call logmsg('No drains found.')
      pckact(idrn) = 0
      return
    end if
    
    f = trim(p)//'.drn'
    call open_file(f, iu, 'w')
    write(iu,'(   a)') 'BEGIN OPTIONS'
    write(iu,'(   a)') 'END OPTIONS'
    write(iu,'(a)')
    write(iu,'(   a)') 'BEGIN DIMENSIONS'
    write(iu,'(2x,a)') 'MAXBOUND '//ta((/maxbound/))
    write(iu,'(   a)') 'END DIMENSIONS'
    write(iu,'(a)')
    do iper = 1, nper
      write(iu,'(   a)') 'BEGIN PERIOD '//ta((/iper/))
      if (lact(iper)) write(iu,'(a)') trim(cwk(iper))
      write(iu,'(   a)') 'END PERIOD'
    end do
    close(iu)
    !
    if (ltransient) then
      f = trim(p)//'.drn'
      call open_file(f, iu, 'w')
      write(iu,'(   a)') 'BEGIN OPTIONS'
      write(iu,'(   a)') 'END OPTIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN DIMENSIONS'
      write(iu,'(2x,a)') 'MAXBOUND '//ta((/maxbound/))
      write(iu,'(   a)') 'END DIMENSIONS'
      write(iu,'(a)')
      nperspu = raw%geti('nyear_spinup')*12
      do iper = 1, nper
        write(iu,'(   a)') 'BEGIN PERIOD '//ta((/iper/))
        jper = mod(iper, nperspu)
        if (jper == 0) jper = nperspu
        if (lact(jper)) write(iu,'(a)') trim(cwk(jper))
        write(iu,'(   a)') 'END PERIOD'
      end do
      close(iu)
    end if
    !
    deallocate(cwk, lact)
    call clear_wrk()
    !
    return
  end subroutine mf6_mod_write_drn
  
  subroutine mf6_mod_write_ghb(this, lbin, lbinpos)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    logical, intent(in) :: lbin
    logical, intent(in) :: lbinpos
    ! -- local
! ------------------------------------------------------------------------------
    !
    if (pckact(ighb) == 0) return
    call logmsg('Processing for '//trim(pck(ighb))//'...')
    !
    call errmsg('GHB package not yet supported.')
    !
    return
  end subroutine mf6_mod_write_ghb
  
  subroutine mf6_mod_write_rch(this, lbin, lbinpos)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    logical, intent(in) :: lbin
    logical, intent(in) :: lbinpos
    ! -- local
    character(len=mxslen), dimension(:), allocatable :: cwk
    character(len=mxslen) :: p, pb, f
    integer(i4b) :: iu, i, n, nbound, maxbound, iper, jper, nper, nperspu
    integer(i4b), dimension(gnlay) :: nbound_lay
    logical, dimension(:), allocatable :: lact
! ------------------------------------------------------------------------------
    !
    if (pckact(irch) == 0) return
    call logmsg('Processing for '//trim(pck(irch))//'...')
    !
    call clear_wrk()
    !
    p = trim(this%rootdir)//trim(this%modelname)
    if (lbin) then
      pb =  trim(this%bindir)//trim(this%modelname)
    else
      pb = p
    end if
    !
    ! write all binary files and store the file strings
    nper = raw%nper
    allocate(cwk(nper), lact(nper))
    maxbound = 0
    !
    do iper = 1, nper
      call this%get_array('recharge', 0, iper, 1, i1wrk, r8wrk, ib_in=2, toponly_in=.false.)
      !
      ! check and filter for zero recharge
      n = 0
      do i = 1, size(i1wrk)
        if (r8wrk(i) == DZERO) then
          if (i1wrk(i) == 1) n = n + 1
          i1wrk(i) = 0
        end if
        if (r8wrk(i) < DZERO) then
          call logmsg('Negative recharge found.')
        end if
      end do
      if (n > 0) then
        call logmsg('Removed '//ta((/n/))//' cells with zero recharge.')
      end if
      !
      nbound_lay = this%count_i1a(i1wrk); nbound = sum(nbound_lay)
      maxbound = max(nbound,maxbound)
      if (nbound == 0) then
        lact(iper) = .false.
        call logmsg('No recharge found.')
      else
        lact(iper) = .true.
        f = trim(pb)//'.rch.sp'//ta((/iper/),'(i3.3)')
        call this%write_list(iu, 2, f, i1wrk, r8wrk, lbin, lbinpos, cwk(iper))
      end if
      call clear_wrk()
    end do
    !
    if (maxbound == 0) then
      call logmsg('No recharge found.')
      pckact(irch) = 0
      return
    end if
    !
    f = trim(p)//'.rch'
    call open_file(f, iu, 'w')
    write(iu,'(   a)') 'BEGIN OPTIONS'
    write(iu,'(   a)') 'END OPTIONS'
    write(iu,'(a)')
    write(iu,'(   a)') 'BEGIN DIMENSIONS'
    write(iu,'(2x,a)') 'MAXBOUND '//ta((/maxbound/))
    write(iu,'(   a)') 'END DIMENSIONS'
    write(iu,'(a)')
    do iper = 1, nper
      write(iu,'(   a)') 'BEGIN PERIOD '//ta((/iper/))
      if (lact(iper)) write(iu,'(a)') trim(cwk(iper))
      write(iu,'(   a)') 'END PERIOD'
    end do
    close(iu)
    !
    if (ltransient) then
      f = trim(p)//'.rch'
      call open_file(f, iu, 'w')
      write(iu,'(   a)') 'BEGIN OPTIONS'
      write(iu,'(   a)') 'END OPTIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN DIMENSIONS'
      write(iu,'(2x,a)') 'MAXBOUND '//ta((/maxbound/))
      write(iu,'(   a)') 'END DIMENSIONS'
      write(iu,'(a)')
      nperspu = raw%geti('nyear_spinup')*12
      do iper = 1, nper
        write(iu,'(   a)') 'BEGIN PERIOD '//ta((/iper/))
        !jper = mod(iper,nperspu)
        !if (jper == 0) jper = nperspu
        jper = mod(iper,12)
        if (jper == 0) jper = 1
        if (lact(jper)) write(iu,'(a)') trim(cwk(jper))
        write(iu,'(   a)') 'END PERIOD'
      end do
      close(iu)
    end if
    !
    deallocate(cwk, lact)
    call clear_wrk()
    !
    return
  end subroutine mf6_mod_write_rch
  
  subroutine mf6_mod_write_wel(this, lbin, lbinpos)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    logical, intent(in) :: lbin
    logical, intent(in) :: lbinpos
    ! -- local
    character(len=mxslen), dimension(:), allocatable :: cwk
    character(len=mxslen) :: p, pb, f
    integer(i4b) :: i, n, iu, nbound, maxbound, iper, jper, nper, nperspu, ilay
    integer(i4b), dimension(gnlay) :: nbound_lay
    logical, dimension(:), allocatable :: lact
! ------------------------------------------------------------------------------
    !
    if (pckact(iwel) == 0) return
    call logmsg('Processing for '//trim(pck(iwel))//'...')
    !
    call clear_wrk()
    !
    p = trim(this%rootdir)//trim(this%modelname)
    if (lbin) then
      pb =  trim(this%bindir)//trim(this%modelname)
    else
      pb = p
    end if
    !
    ! write all binary files and store the file strings
    nper = raw%nper
    allocate(cwk(nper), lact(nper))
    maxbound = 0
    do iper = 1, nper
      do ilay = 1, gnlay
        call this%get_array('wel_q', ilay, iper, ilay, i1wrk, r8wrk)
      end do
      !
      nbound_lay = this%count_i1a(i1wrk); nbound = sum(nbound_lay)
      maxbound = max(nbound,maxbound)
      if (nbound == 0) then
        lact(iper) = .false.
        call logmsg('No wells found.')
      else
        lact(iper) = .true.
        f = trim(pb)//'.wel.sp'//ta((/iper/),'(i3.3)')
        call this%write_list(iu, 2, f, i1wrk, r8wrk, lbin, lbinpos, cwk(iper))
      end if
      call clear_wrk()
    end do
    !
    if (maxbound == 0) then
      call logmsg('No wells found.')
      pckact(iwel) = 0
      return
    end if
    
    f = trim(p)//'.wel'
    call open_file(f, iu, 'w')
    write(iu,'(   a)') 'BEGIN OPTIONS'
    write(iu,'(   a)') 'END OPTIONS'
    write(iu,'(a)')
    write(iu,'(   a)') 'BEGIN DIMENSIONS'
    write(iu,'(2x,a)') 'MAXBOUND '//ta((/maxbound/))
    write(iu,'(   a)') 'END DIMENSIONS'
    write(iu,'(a)')
    do iper = 1, nper
      write(iu,'(   a)') 'BEGIN PERIOD '//ta((/iper/))
      if (lact(iper)) write(iu,'(a)') trim(cwk(iper))
      write(iu,'(   a)') 'END PERIOD'
    end do
    close(iu)
    !
    if (ltransient) then
      f = trim(p)//'.wel'
      call open_file(f, iu, 'w')
      write(iu,'(   a)') 'BEGIN OPTIONS'
      write(iu,'(   a)') 'END OPTIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN DIMENSIONS'
      write(iu,'(2x,a)') 'MAXBOUND '//ta((/maxbound/))
      write(iu,'(   a)') 'END DIMENSIONS'
      write(iu,'(a)')
      nperspu = raw%geti('nyear_spinup')*12
      do iper = 1, nper
        write(iu,'(   a)') 'BEGIN PERIOD '//ta((/iper/))
        jper = mod(iper, nperspu)
        if (jper == 0) jper = nperspu
        if (lact(jper)) write(iu,'(a)') trim(cwk(jper))
        write(iu,'(   a)') 'END PERIOD'
      end do
      close(iu)
    end if
    !
    deallocate(cwk, lact)
    call clear_wrk()
    !
    return
  end subroutine mf6_mod_write_wel
  
  subroutine mf6_mod_write_exchanges(this, ju)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    integer(i4b), intent(in) :: ju
    ! -- local
    character(len=mxslen) :: s, d, f, fexg, m1s, m2s
    integer(i4b) :: iu, ixch, iexg
    type(tExchange), pointer :: xch => null()
! ------------------------------------------------------------------------------
    d = '..\..\models\run_input\exchanges\'
    call create_dir(d)
    
    do ixch = 1, this%nxch
      xch => this%xch(ixch)
      !
      m1s = this%modelname
      m2s = xch%m2modelname
      !
      fexg = trim(m1s)//'-'//trim(m2s)//'.exg'
      !
      ! .exg file
      f = trim(d)//trim(fexg); call swap_slash(f)
      call open_file(f, iu, 'w')
      write(iu,'(   a)') 'BEGIN OPTIONS'
      if (raw%geti('exchange_newton',idef=0) == 1) then
        write(iu,'(2x,a)') 'NEWTON'
      end if
      write(iu,'(   a)') 'END OPTIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN DIMENSIONS'
      write(iu,'(2x,a)') 'NEXG '//ta((/xch%nexg/))
      write(iu,'(   a)') 'END DIMENSIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN EXCHANGEDATA'
      f = trim(d)//trim(fexg)//'.asc'; call swap_slash(f)
      write(iu,'(2x,a)') 'OPEN/CLOSE '//trim(f)
      write(iu,'(   a)') 'END EXCHANGEDATA'
      close(iu)
      !
      ! .asc file
      f = trim(d)//trim(fexg)//'.asc'; call swap_slash(f)
      call open_file(f, iu, 'w')
      do iexg = 1, xch%nexg
        write(iu,'(a)') ta((/xch%cellidm1(iexg), xch%cellidm2(iexg)/))//' '// &
                        ta((/xch%ihc(iexg)/))//' '// &
                        ta((/xch%cl1(iexg), xch%cl2(iexg), xch%hwva(iexg)/))
      end do
      close(iu)
      !
      !GWF6-GWF6 .\exchanges\m000039-m000040.exg m000039 m000040
      write(s,'(a,3(1x,a))') 'GWF6-GWF6',  trim(d)//trim(fexg), trim(m1s), trim(m2s)
      write(ju,'(a)') trim(s)
    end do
! ------------------------------------------------------------------------------
    !
    return
  end subroutine mf6_mod_write_exchanges
  
  subroutine clear_wrk()
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
! ------------------------------------------------------------------------------
    if (associated(cwrk1d)) then
      deallocate(cwrk1d)
      cwrk1d => null()
    end if
    if (associated(cwrk2d)) then
      deallocate(cwrk2d)
      cwrk2d => null()
    end if
    if (associated(i1wrk)) then
      deallocate(i1wrk)
      i1wrk => null()
    end if
    if (associated(i1wrk2)) then
      deallocate(i1wrk2)
      i1wrk2 => null()
    end if
    if (associated(i1wrk2d)) then
      deallocate(i1wrk2d)
      i1wrk2d => null()
    end if
    if (associated(i4wrk1d)) then
      deallocate(i4wrk1d)
      i4wrk1d => null()
    end if
    if (associated(i4wrk1d2)) then
      deallocate(i4wrk1d2)
      i4wrk1d2 => null()
    end if
    if (associated(i4wrk2d)) then
      deallocate(i4wrk2d)
      i4wrk2d => null()
    end if
    if (associated(i4wrk3d)) then
      deallocate(i4wrk3d)
      i4wrk3d => null()
    end if
    if (associated(r8wrk)) then
      deallocate(r8wrk)
      r8wrk => null()
    end if
    if (associated(r8wrk2)) then
      deallocate(r8wrk2)
      r8wrk2 => null()
    end if
    if (associated(r8wrk3)) then
      deallocate(r8wrk3)
      r8wrk3 => null()
    end if
    if (associated(r8wrk2d)) then
      deallocate(r8wrk2d)
      r8wrk2d => null()
    end if
    if (associated(r8wrk2d2)) then
      deallocate(r8wrk2d2)
      r8wrk2d2 => null()
    end if
    !
    return
  end subroutine clear_wrk
  
  subroutine mf6_sol_write(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_sol) :: this
    ! -- local
! ------------------------------------------------------------------------------
    call this%write_wrap()
    call this%write_tdis()
    call this%write_ims()
    call this%write_mfsim()
    call this%write_post_map()
    !
    return
  end subroutine mf6_sol_write
!    
  subroutine mf6_sol_clean(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_sol) :: this
    ! -- local
! ------------------------------------------------------------------------------
    !
    if (associated(this%solname))  deallocate(this%solname)
    if (associated(this%nmod))     deallocate(this%nmod)
    if (associated(this%mod_id))   deallocate(this%mod_id)
    if (associated(this%npart))    deallocate(this%npart)
    if (associated(this%mod_part)) deallocate(this%mod_part)
    !
    this%solname  => null()
    this%nmod     => null()
    this%mod_id   => null()
    this%npart    => null()
    this%mod_part => null()
    !
    return
  end subroutine mf6_sol_clean
  !
  subroutine mf6_sol_write_wrap(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_sol) :: this
    ! -- local
    integer(i4b), parameter :: iser = 1
    integer(i4b), parameter :: ipar = 2
    logical, dimension(2) :: lcpu
    character(len=4), dimension(2) :: pcpu
    data pcpu/'.ser','.par'/
    !
    character(len=mxslen) :: f, ms, s
    integer(i4b) :: iu, i, icpu
! ------------------------------------------------------------------------------
    !
    lcpu = .true.
    if (this%nmod == 1) then
      lcpu(ipar) = .false.
    end if
    
    ! models.asc
    do icpu = 1, 2
      if (.not.lcpu(icpu)) cycle
 
      f = trim(this%solname)//pcpu(icpu)//'.models.asc'
      call open_file(f, iu, 'w')
      do i = 1, this%nmod
        ms =  'm'//ta((/this%mod_id(i)/),'(i5.5)')
        s = 'GWF6 ..\..\models\run_input\'//trim(ms)// &
          '\'//trim(ms)//'.nam '//trim(ms)
        call swap_slash(s)
        if (icpu == ipar) then
          write(iu,'(a)') trim(s)//' '//ta((/this%mod_part(i)/))
        else
          write(iu,'(a)') trim(s)
        end if
      end do
      close(iu)
    end do
    !
    ! solmodels.asc
    f = trim(this%solname)//'.solmodels.asc'
    call open_file(f, iu, 'w')
    do i = 1, this%nmod
      ms = 'm'//ta((/this%mod_id(i)/),'(i5.5)'); write(iu,'(a)') trim(ms)
    end do
    close(iu)
    
    ! cgc.solmodels.asc
    f = trim(this%solname)//'.cgc.solmodels.asc'
    call open_file(f, iu, 'w')
    do i = 1, this%nmod
      ms = 'm'//ta((/this%mod_id(i)/),'(i5.5)')
      write(iu,'(a)') trim(ms)//' '//ta((/i/))
    end do
    close(iu)
    !
    ! solmodels.asc.wrp
    f = trim(this%solname)//'.solmodels.wrp.asc'
    call open_file(f, iu, 'w')
    write(iu,'(   a)') 'BEGIN MODELS'
    s = '..\run_input\'//trim(this%solname)//'.solmodels.asc'
    call swap_slash(s)
    write(iu,'(2x,a)') 'OPEN/CLOSE '//trim(s)
    write(iu,'(   a)') 'END MODELS'
    close(iu)
    !
    ! cgc.solmodels.asc.wrp
    f = trim(this%solname)//'.cgc.solmodels.wrp.asc'
    call open_file(f, iu, 'w')
    write(iu,'(   a)') 'BEGIN MODELS'
    s = '..\run_input\'//trim(this%solname)//'.cgc.solmodels.asc'
    call swap_slash(s)
    write(iu,'(2x,a)') 'OPEN/CLOSE '//trim(s)
    write(iu,'(   a)') 'END MODELS'
    close(iu)
    !
    return
  end subroutine mf6_sol_write_wrap
  
  subroutine mf6_sol_write_mfsim(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_sol) :: this
    ! -- local
#ifdef LINUX
    logical, parameter :: lwritebat = .false.
#else
    logical, parameter :: lwritebat = .true.
#endif 
    integer(i4b), parameter :: iser = 1
    integer(i4b), parameter :: ipar = 2
    logical, dimension(2) :: lcpu
    character(len=4), dimension(2) :: pcpu
    data pcpu/'.ser','.par'/
    !
    character(len=mxslen) :: d, nam, ms, f, s, mpi_exe, mf6_exe
    integer(i4b) :: iu, ju, i, icpu, irun, irun0, irun1
! ------------------------------------------------------------------------------
    !
    d = '..\run_input\'; call swap_slash(d)
    !
    if (lwritebat) then
      mpi_exe = raw%getc('mpi_exe',cdef='mpiexec.exe')
      mf6_exe = raw%getc('mf6_exe',cdef='mf6.exe')
    end if
    !
    lcpu = .true.
    if (this%nmod == 1) then
      lcpu(ipar) = .false.
    end if
    !
    if (.not.ltransient) then
      !if (lwritebat) then
      !  f = '..\run_output\run.'//trim(this%solname)//pcpu(iser)//'.bat'
      !  call swap_slash(f)
      !  call open_file(f, ju, 'w')
      !  write(ju,'(a)') 'set exe="'//trim(mf6_exe)//'"'
      !  write(ju,'(a)')
      !  write(ju,'(a)') '@echo off'
      !  write(ju,'(a)')
      !end if
      !do i = 1, this%nmod
      !  ms =  'm'//ta((/this%mod_id(i)/),'(i5.5)')
      !  nam = trim(this%solname)//pcpu(iser)//'.mfsim.'//trim(ms)//'.nam'
      !  call open_file(nam, iu, 'w')
      !  write(iu,'(   a)') 'BEGIN OPTIONS'
      !  write(iu,'(2x,a)') 'MEMORY_PRINT_OPTION SUMMARY'
      !  write(iu,'(   a)') 'END OPTIONS'
      !  write(iu,'(a)')
      !  write(iu,'(   a)') 'BEGIN TIMING'
      !  write(iu,'(2x,a)') 'TDIS6 '//trim(d)//trim(this%solname)//'.tdis'
      !  write(iu,'(   a)') 'END TIMING'
      !  write(iu,'(a)')
      !  write(iu,'(   a)') 'BEGIN MODELS'
      !  s = 'GWF6 ..\..\models\run_input\'//trim(ms)// &
      !    '\'//trim(ms)//'.nam '//trim(ms)
      !  call swap_slash(s)
      !  write(iu,'(2x,a)') trim(s)
      !  write(iu,'(   a)') 'END MODELS'
      !  write(iu,'(a)')
      !  write(iu,'(   a)') 'BEGIN EXCHANGES'
      !  write(iu,'(   a)') 'END EXCHANGES'
      !  write(iu,'(a)')
      !  write(iu,'(   a)') 'BEGIN SOLUTIONGROUP 1'
      !  write(iu,'(2x,a)') 'IMS6 '//trim(d)//trim(this%solname)//'.ims '//trim(ms)
      !  write(iu,'(   a)') 'END SOLUTIONGROUP'
      !  close(iu)
      !  !
      !  if (lwritebat) then
      !    write(ju,'(a)') '%exe% -s ..\run_input\'//trim(nam)
      !  end if
      !end do
      !if (lwritebat) then
      !  write(ju,'(a)')
      !  write(ju,'(a)') 'echo Serial runs complete. Press any key to continue.'
      !  write(ju,'(a)') 'pause>nul'
      !  close(ju)
      !end if
    end if
    !
    ! if parallel: also write the serial mfsim.nam
    do icpu = 1, 2
      if (.not.lcpu(icpu)) cycle
      do irun = irun0, irun1
        nam = trim(this%solname)//pcpu(icpu)//'.mfsim.nam'
        call open_file(nam, iu, 'w')
        write(iu,'(   a)') 'BEGIN OPTIONS'
        write(iu,'(2x,a)') 'MEMORY_PRINT_OPTION SUMMARY'
        if (icpu == ipar) write(iu,'(2x,a)') 'DOMAIN_DECOMPOSITION '//ta((/this%npart/))
        write(iu,'(   a)') 'END OPTIONS'
        write(iu,'(a)')
        write(iu,'(   a)') 'BEGIN TIMING'
        write(iu,'(2x,a)') 'TDIS6 '//trim(d)//trim(this%solname)//'.tdis'
        write(iu,'(   a)') 'END TIMING'
        write(iu,'(a)')
        write(iu,'(   a)') 'BEGIN MODELS'
        write(iu,'(2x,a)') 'OPEN/CLOSE '//trim(d)//trim(this%solname)//pcpu(icpu)//'.models.asc'
        write(iu,'(   a)') 'END MODELS'
        write(iu,'(a)')
        write(iu,'(   a)') 'BEGIN EXCHANGES'
        write(iu,'(2x,a)') 'OPEN/CLOSE '//trim(d)//trim(this%solname)//'.exchanges.asc'
        write(iu,'(   a)') 'END EXCHANGES'
        write(iu,'(a)')
        write(iu,'(   a)') 'BEGIN SOLUTIONGROUP 1'
        write(iu,'(2x,a)') '# IMS6_CGC '//trim(d)//trim(this%solname)//'.ims FILEIN '//trim(d)// &
          trim(this%solname)//'.cgc.solmodels.wrp.asc'
        write(iu,'(2x,a)') 'IMS6 '//trim(d)//trim(this%solname)//'.ims FILEIN '//trim(d)// &
          trim(this%solname)//'.solmodels.wrp.asc'
        write(iu,'(   a)') 'END SOLUTIONGROUP'
        close(iu)
        !
        ! write the DOS batch file
        if (lwritebat) then
          f = '..\run_output\run.'//trim(this%solname)//pcpu(icpu)//'.bat'
          call swap_slash(f)
          call open_file(f, iu, 'w')
          write(iu,'(a)') 'set exe="'//trim(mf6_exe)//'"'
          write(iu,'(a)') 'set nam=..\run_input\'//trim(nam)
          if (icpu == ipar) then
            write(iu,'(a)') 'set mpi="'//trim(mpi_exe)//'"'
            write(iu,'(a)') 'set np='//ta((/this%npart/))
          end if
          write(iu,'(a)')
          write(iu,'(a)') '@echo off'
          write(iu,'(a)')
          if (icpu == ipar) then
             write(iu,'(a)') '%mpi% -np %np% %exe% -s %nam%'
          else
             write(iu,'(a)') '%exe% -s %nam%'
          end if
          write(iu,'(a)') 'echo.'
          if (icpu == ipar) then
            write(iu,'(a)') 'echo Parallel run complete. Press any key to continue.'
          else
            write(iu,'(a)') 'echo Serial run complete. Press any key to continue.'
          end if
          write(iu,'(a)') 'pause>nul'
          close(iu)
        end if
      end do
    end do
    !
    return
  end subroutine mf6_sol_write_mfsim
! 
  subroutine mf6_sol_write_post_map(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_sol) :: this 
    ! -- local
    character(len=mxslen) :: d, f
    integer(i4b) :: i, iu
! ------------------------------------------------------------------------------
    !
    d = '..\post_mappings\'; call create_dir(d, .true.)
    f = trim(d)//trim(this%solname)//'.modmap.bin'; call swap_slash(f)
    !
    call open_file(f, iu, 'w', .true.)
    write(iu) this%nmod
    write(iu)(this%mod_id(i),i=1, this%nmod)
    close(iu)
    !
    return
  end subroutine mf6_sol_write_post_map
!
  subroutine mf6_sol_write_tdis(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_sol) :: this
    ! -- local
    character(len=mxslen) :: f
    integer(i4b) :: iu, iper, jper, nstp_fac, nd, nperspu
! ------------------------------------------------------------------------------
    !
    if (.not.ltransient) then !SS
      f = trim(this%solname)//'.tdis'
      call open_file(f, iu, 'w')
      write(iu,'(   a)') 'BEGIN OPTIONS'
      write(iu,'(2x,a)') 'TIME_UNITS DAYS'
      write(iu,'(   a)') 'END OPTIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN DIMENSIONS'
      write(iu,'(2x,a)') 'NPER 1'
      write(iu,'(   a)') 'END DIMENSIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN PERIODDATA'
      write(iu,'(2x,a)') '1 1 1'
      write(iu,'(   a)') 'END PERIODDATA'
      close(iu)
    !
    else !TR
      nstp_fac = raw%geti('nstp_fac',idef=1)
      !
      nperspu = raw%geti('nyear_spinup')*12
      f = trim(this%solname)//'.tdis'
      call open_file(f, iu, 'w')
      write(iu,'(   a)') 'BEGIN OPTIONS'
      write(iu,'(2x,a)') 'TIME_UNITS DAYS'
      write(iu,'(   a)') 'END OPTIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN DIMENSIONS'
      write(iu,'(2x,a)') 'NPER '//ta((/nperspu/))
      write(iu,'(   a)') 'END DIMENSIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN PERIODDATA'
      do iper = 1, nperspu
        !jper = mod(iper,nperspu)
        !if (jper == 0) jper = nperspu
        jper = mod(iper,12)
        if (jper == 0) jper = 1
        nd = get_month_days_s(raw%perdate(jper))
        write(iu,'(2x,a)') ta((/nd/))//' '//ta((/nd*nstp_fac/))//' 1'
      end do
      write(iu,'(   a)') 'END PERIODDATA'
      close(iu)
      !
      f = trim(this%solname)//'.tdis'
      call open_file(f, iu, 'w')
      write(iu,'(   a)') 'BEGIN OPTIONS'
      write(iu,'(2x,a)') 'TIME_UNITS DAYS'
      write(iu,'(   a)') 'END OPTIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN DIMENSIONS'
      write(iu,'(2x,a)') 'NPER '//ta((/raw%nper/))
      write(iu,'(   a)') 'END DIMENSIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN PERIODDATA'
      do iper = 1, raw%nper
        nd = get_month_days_s(raw%perdate(iper))
        write(iu,'(2x,a)') ta((/nd/))//' '//ta((/nd*nstp_fac/))//' 1'
      end do
      write(iu,'(   a)') 'END PERIODDATA'
      close(iu)
    end if
    !
    return
  end subroutine mf6_sol_write_tdis
  !
  subroutine mf6_sol_write_ims(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_sol) :: this
    ! -- local
    character(len=mxslen) :: f
    integer(i4b) :: iu
    logical :: lcomplex
    !
    integer(i4b), parameter :: nnlvar = 11
    integer(i4b), parameter :: nlvar  = 10
    !
    type tCompl
      character(len=mxslen) :: name = ''
      character(mxslen), dimension(2,nnlvar) :: nlvar
      character(mxslen), dimension(2,nlvar)  :: lvar
    end type tCompl
    !
    type(tCompl), pointer :: c => null()
    type(tCompl), dimension(:), pointer :: compl => null()
    integer(i4b) :: ncompl, i, j
    character(mxslen) :: cname, rclose_option
! ------------------------------------------------------------------------------
    !
    ! define the complexities
    ncompl = 3
    allocate(compl(ncompl))
    !
    ! SIMPLE
    i = 1; c => compl(i); c%name = 'SIMPLE'
    j =     1; c%nlvar(:,j) = (/'OUTER_HCLOSE'                  , '0.001'/)
    j = j + 1; c%nlvar(:,j) = (/'OUTER_MAXIMUM '                , '25'/)
    j = j + 1; c%nlvar(:,j) = (/'UNDER_RELAXATION '             , 'NONE'/)
    j = j + 1; c%nlvar(:,j) = (/'UNDER_RELAXATION_THETA '       , '0.0'/)
    j = j + 1; c%nlvar(:,j) = (/'UNDER_RELAXATION_KAPPA '       , '0.0'/)
    j = j + 1; c%nlvar(:,j) = (/'UNDER_RELAXATION_GAMMA '       , '0.0'/)
    j = j + 1; c%nlvar(:,j) = (/'UNDER_RELAXATION_MOMENTUM '    , '0.0'/)
    j = j + 1; c%nlvar(:,j) = (/'BACKTRACKING_NUMBER '          , '0'/)
    j = j + 1; c%nlvar(:,j) = (/'BACKTRACKING_TOLERANCE '       , '0.0'/)
    j = j + 1; c%nlvar(:,j) = (/'BACKTRACKING_REDUCTION_FACTOR ', '0.0'/)
    j = j + 1; c%nlvar(:,j) = (/'BACKTRACKING_RESIDUAL_LIMIT '  , '0.0'/)
    !
    j =     1; c%lvar(:,j) = (/'INNER_MAXIMUM'                 , '50'/)
    j = j + 1; c%lvar(:,j) = (/'INNER_HCLOSE'                  , '0.001'/)
    j = j + 1; c%lvar(:,j) = (/'INNER_RCLOSE'                  , '0.1'/)
    j = j + 1; c%lvar(:,j) = (/'LINEAR_ACCELERATION'           , 'CG'/)
    j = j + 1; c%lvar(:,j) = (/'RELAXATION_FACTOR'             , '0.0'/)
    j = j + 1; c%lvar(:,j) = (/'PRECONDITIONER_LEVELS'         , '0'/)
    j = j + 1; c%lvar(:,j) = (/'PRECONDITIONER_DROP_TOLERANCE' , '0.0'/)
    j = j + 1; c%lvar(:,j) = (/'NUMBER_ORTHOGONALIZATIONS'     , '0'/)
    j = j + 1; c%lvar(:,j) = (/'SCALING_METHOD'                , 'NONE'/)
    j = j + 1; c%lvar(:,j) = (/'REORDERING_METHOD'             , 'NONE'/)
    !
    ! MODERATE
    i = 2; c => compl(i); c%name = 'MODERATE'
    j =     1; c%nlvar(:,j) = (/'OUTER_HCLOSE'                  , '0.01'/)
    j = j + 1; c%nlvar(:,j) = (/'OUTER_MAXIMUM '                , '50'/)
    j = j + 1; c%nlvar(:,j) = (/'UNDER_RELAXATION '             , 'DBD'/)
    j = j + 1; c%nlvar(:,j) = (/'UNDER_RELAXATION_THETA '       , '0.9'/)
    j = j + 1; c%nlvar(:,j) = (/'UNDER_RELAXATION_KAPPA '       , '0.0001'/)
    j = j + 1; c%nlvar(:,j) = (/'UNDER_RELAXATION_GAMMA '       , '0.0'/)
    j = j + 1; c%nlvar(:,j) = (/'UNDER_RELAXATION_MOMENTUM '    , '0.0'/)
    j = j + 1; c%nlvar(:,j) = (/'BACKTRACKING_NUMBER '          , '0'/)
    j = j + 1; c%nlvar(:,j) = (/'BACKTRACKING_TOLERANCE '       , '0.0'/)
    j = j + 1; c%nlvar(:,j) = (/'BACKTRACKING_REDUCTION_FACTOR ', '0.0'/)
    j = j + 1; c%nlvar(:,j) = (/'BACKTRACKING_RESIDUAL_LIMIT '  , '0.0'/)
    !
    j =     1; c%lvar(:,j) = (/'INNER_MAXIMUM'                 , '100'/)
    j = j + 1; c%lvar(:,j) = (/'INNER_HCLOSE'                  , '0.01'/)
    j = j + 1; c%lvar(:,j) = (/'INNER_RCLOSE'                  , '0.1'/)
    j = j + 1; c%lvar(:,j) = (/'LINEAR_ACCELERATION'           , 'BICGSTAB'/)
    j = j + 1; c%lvar(:,j) = (/'RELAXATION_FACTOR'             , '0.97'/)
    j = j + 1; c%lvar(:,j) = (/'PRECONDITIONER_LEVELS'         , '0'/)
    j = j + 1; c%lvar(:,j) = (/'PRECONDITIONER_DROP_TOLERANCE' , '0.0'/)
    j = j + 1; c%lvar(:,j) = (/'NUMBER_ORTHOGONALIZATIONS'     , '0'/)
    j = j + 1; c%lvar(:,j) = (/'SCALING_METHOD'                , 'NONE'/)
    j = j + 1; c%lvar(:,j) = (/'REORDERING_METHOD'             , 'NONE'/)
    !
    ! COMPLEX
    i = 3; c => compl(i); c%name = 'COMPLEX'
    j =     1; c%nlvar(:,j) = (/'OUTER_HCLOSE'                  , '0.1'/)
    j = j + 1; c%nlvar(:,j) = (/'OUTER_MAXIMUM '                , '100'/)
    j = j + 1; c%nlvar(:,j) = (/'UNDER_RELAXATION '             , 'DBD'/)
    j = j + 1; c%nlvar(:,j) = (/'UNDER_RELAXATION_THETA '       , '0.8'/)
    j = j + 1; c%nlvar(:,j) = (/'UNDER_RELAXATION_KAPPA '       , '0.0001'/)
    j = j + 1; c%nlvar(:,j) = (/'UNDER_RELAXATION_GAMMA '       , '0.0'/)
    j = j + 1; c%nlvar(:,j) = (/'UNDER_RELAXATION_MOMENTUM '    , '0.0'/)
    j = j + 1; c%nlvar(:,j) = (/'BACKTRACKING_NUMBER '          , '20'/)
    j = j + 1; c%nlvar(:,j) = (/'BACKTRACKING_TOLERANCE '       , '1.05'/)
    j = j + 1; c%nlvar(:,j) = (/'BACKTRACKING_REDUCTION_FACTOR ', '0.1'/)
    j = j + 1; c%nlvar(:,j) = (/'BACKTRACKING_RESIDUAL_LIMIT '  , '0.002'/)
    !
    j =     1; c%lvar(:,j) = (/'INNER_MAXIMUM'                 , '500'/)
    j = j + 1; c%lvar(:,j) = (/'INNER_HCLOSE'                  , '0.1'/)
    j = j + 1; c%lvar(:,j) = (/'INNER_RCLOSE'                  , '0.1'/)
    j = j + 1; c%lvar(:,j) = (/'LINEAR_ACCELERATION'           , 'BICGSTAB'/)
    j = j + 1; c%lvar(:,j) = (/'RELAXATION_FACTOR'             , '0.0'/)
    j = j + 1; c%lvar(:,j) = (/'PRECONDITIONER_LEVELS'         , '5'/)
    j = j + 1; c%lvar(:,j) = (/'PRECONDITIONER_DROP_TOLERANCE' , '0.0001'/)
    j = j + 1; c%lvar(:,j) = (/'NUMBER_ORTHOGONALIZATIONS'     , '2'/)
    j = j + 1; c%lvar(:,j) = (/'SCALING_METHOD'                , 'NONE'/)
    j = j + 1; c%lvar(:,j) = (/'REORDERING_METHOD'             , 'NONE'/)
    !
    cname = raw%getc('complexity',cdef='SIMPLE')
    select case (cname)
    case('SIMPLE')
      c => compl(1)
    case('MODERATE')
      c => compl(2)
    case('COMPLEX')
      c => compl(3)
    case default
      call errmsg("Invalid complexity: "//trim(cname))
    end select
    !
    ! overwrite the defaults
    do j = 1, nnlvar
      c%nlvar(2,j) = raw%getc(trim(change_case(c%nlvar(1,j),'l')),cdef=trim(c%nlvar(2,j)))
    end do
    do j = 1, nlvar
      c%lvar(2,j) = raw%getc(trim(change_case(c%lvar(1,j),'l')),cdef=trim(c%lvar(2,j)))
    end do
    rclose_option = raw%getc('rclose_option',cdef='')
    if (len_trim(rclose_option) > 0) then
      c%lvar(2,3) = trim(c%lvar(2,3))//' '//trim(rclose_option)
    end if
    !
    f = trim(this%solname)//'.ims'
    call open_file(f, iu, 'w')
    !
    write(iu,'(   a)') 'BEGIN OPTIONS'
    write(iu,'(2x,a)') 'PRINT_OPTION '//raw%getc('print_option',cdef='ALLITER')
    write(iu,'(   a)') 'END OPTIONS'
    write(iu,'(a)')
    write(iu,'(   a)') 'BEGIN NONLINEAR'
    do j = 1, nnlvar
      write(iu,'(2x,a)') trim(c%nlvar(1,j))//' '//trim(trim(c%nlvar(2,j)))
    end do
    write(iu,'(   a)') 'END NONLINEAR'
    write(iu,'(a)')
    write(iu,'(   a)') 'BEGIN LINEAR'
    do j = 1, nlvar
      write(iu,'(2x,a)') trim(c%lvar(1,j))//' '//trim(trim(c%lvar(2,j)))
    end do
    write(iu,'(   a)') 'END LINEAR'
    close(iu)
    !
    deallocate(compl)
    !
    return
  end subroutine mf6_sol_write_ims

  function mf6_mod_get_val_r8(this, i_raw, ilay_dat, gic0, gir0, gnrl) &
    result(r8val)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(tMf6_mod) :: this
    integer(i4b), intent(in) :: i_raw
    integer(i4b), intent(in) :: ilay_dat
    integer(i4b), intent(in) :: gic0
    integer(i4b), intent(in) :: gir0
    integer(i4b), intent(in) :: gnrl
    real(r8b) :: r8val
    ! -- local
    type(tEhdr), pointer :: ehdr
    type(tData), pointer :: dat => null()
    integer(i4b) :: bs, ir0, ir1, ic0, ic1, ir, ic
    logical :: lmv
! ------------------------------------------------------------------------------
    dat => raw%raw(i_raw)%dat
    !
    select case(dat%file_type)
    case(i_ehdr)
      ehdr => dat%ehdr
      if (dat%readall) then
        call ehdr%ehdr_read_full_grid(ehdr%fp)
      end if
      bs = int(gnrl*gcs/ehdr%hdr%csr8)
      ir = (gir0 - 1)/bs + 1 ! ir0 = (irow-1)*bs + 1
      ic = (gic0 - 1)/bs + 1 ! ic0 = (icol-1)*bs + 1
      r8val = ehdr%ehdr_get_val_r8(ic, ir, gnrl*gcs, lmv)
    case(i_undefined)
      read(dat%s,*) r8val
    end select
    !
    if (lmv) then
      r8val= r8nodata
    else 
      r8val = r8val * dat%r8mult + dat%r8add
    end if
    !
    return
  end function mf6_mod_get_val_r8
    
  subroutine mf6_mod_get_array_r8(this, key, ilay_dat, iper_dat, ilay_tgt, &
    arrflg, arr, ib_in, toponly_in)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    character(len=*), intent(in) :: key
    integer(i4b), intent(in) :: ilay_dat
    integer(i4b), intent(in) :: iper_dat
    integer(i4b), intent(in) :: ilay_tgt
    integer(i1b), dimension(:), pointer, intent(inout) :: arrflg
    real(r8b), dimension(:), pointer, intent(inout) :: arr
    integer(i4b), intent(in), optional :: ib_in
    logical, intent(in), optional :: toponly_in
    ! -- local
    type(tEhdr), pointer :: ehdr
    type(tReg), pointer :: reg
    type(tRegmap), pointer :: regmap => null()
    type(tData), pointer :: dat => null()
    integer(i4b) :: n, nt, i, ireg, ir, ic, jr, jc, ib, itile, ilay, nlay
    integer(i4b) :: ir0, ir1, ic0, ic1, nrl
    integer(i4b) :: arrsiz
    real(r4b) :: r4val
    real(r8b) :: r8val
    logical :: lfirst, ltile, toponly, found, ldefault, lmv
! ------------------------------------------------------------------------------
    lfirst = .true.
    ltile = .false.
    ldefault = .false.
    !
    if (present(ib_in)) then
      ib = ib_in
    else
      ib = 0
    end if
    !
    if (present(toponly_in)) then
      toponly = toponly_in
    else
      toponly = .false.
    end if
    !
    i = raw%mf6_raw_get_index(key, ilay_dat, iper_dat)
    if (i <= 0) then
      call logmsg('Warning: array not read for '//trim(key)// &
        ' layer '//ta((/ilay_dat/))//' kper '//ta((/iper_dat/)))
      return
    end if
    dat => raw%raw(i)%dat
    !
    n = sum(this%layer_nodes)
    if (.not.associated(arr)) then
      allocate(arr(n))
      do i = 1, size(arr)
        arr(i) = DZERO
      end do
    end if
    if (.not.associated(arrflg)) then
      allocate(arrflg(n))
      do i = 1, size(arrflg)
        arrflg(i) = 0
      end do
    end if
    !
    if (size(arrflg) /= size(arr)) then
      call errmsg('mf6_mod_get_array_r8: program error 1')
    end if
    arrsiz = size(arrflg)
    
    select case(dat%file_type)
      case(i_ehdr)
        ehdr => dat%ehdr
        if (dat%readall) then
          call ehdr%ehdr_read_full_grid(ehdr%fp)
        end if
    end select
    
    do ireg = 1, this%nreg
      reg => this%reg(ireg)
      regmap => reg%regmap(ilay_tgt)
      nrl = 2**rlev(ilay_tgt)
      ir0 = regmap%bb%ir0; ir1 = regmap%bb%ir1
      ic0 = regmap%bb%ic0; ic1 = regmap%bb%ic1
      do ir = ir0, ir1
        do ic = ic0, ic1
          jr = ir - ir0 + 1; jc = ic - ic0 + 1
          n = regmap%nodmap(jc,jr)
          ! only select active cells seen from top
          if (toponly.and.(n /= 0)) then
            call errmsg('mf6_mod_get_array_r8: toponly not yet supported')
          end if
          !
          if (n < 0) then 
            call errmsg('mf6_mod_get_array_r8: program error 2')
          end if
          if (n /= 0) then
            select case(dat%file_type)
            case(i_ehdr)
              r8val = ehdr%ehdr_get_val_r8(ic, ir, nrl*gcs, lmv)
            case(i_undefined)
              read(dat%s,*) r8val
            end select
            if (n /= 0 .and. .not.lmv) then
              if ((n < 1) .or. (n > arrsiz)) then
                call errmsg('mf6_mod_get_array_r8: program error 3 '//ta((/ib/))//' '//&
                  ta((/n/))//' '//ta((/arrsiz/)))
              end if
              arr(n) = r8val * dat%r8mult + dat%r8add
              arrflg(n) = 1
            end if
            if (lmv) then
              arr(n) = r8nodata
              arrflg(n) = 0
            end if
          end if
        end do
      end do
    end do
    !
    select case(dat%file_type)
      case(i_ehdr)
        ehdr => dat%ehdr
        !call ehdr%clean_x()
    end select
    
    !if (.not.ltile) then
    !  call logmsg('WARNING, for some points the tile was not found!')
    !end if
    !if (ldefault) then
    !  call logmsg('WARNING, for some points default values were taken!')
    !end if
    !
    return
  end subroutine mf6_mod_get_array_r8
!
  subroutine mf6_mod_write_post_map(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    ! -- local
    character(len=mxslen) :: d
    type(tReg), pointer :: reg => null()
    type(tRegmap), pointer :: regmap => null()
    type(tBb), pointer :: bb => null()
    character(len=mxslen) :: f
    integer(i4b) :: ireg, nodes, n, iu, il, ir, ic, gic, gir, gil, i, j 
! ------------------------------------------------------------------------------
    !
    nodes = 0
    do ireg = 1, this%nreg
      reg => this%reg(ireg)
      do il = 1, size(reg%layer_nodes)
        nodes = nodes + reg%layer_nodes(il)
      end do
    end do
    !
    call clear_wrk()
    allocate(i4wrk2d(4,nodes))
    do i = 1, nodes
      do j = 1, 4
        i4wrk2d(j,i) = 0
      end do
    end do
    !
    do ireg = 1, this%nreg
      reg => this%reg(ireg)
      do il = 1, gnlay
        regmap => reg%regmap(il)
        bb => regmap%bb
        do ir = 1, bb%nrow
          do ic = 1, bb%ncol
            n = regmap%nodmap(ic,ir)
            gil = il; gic = ic + bb%ic0 - 1; gir = ir + bb%ir0 - 1
            if (n < 0) then
              gil = -gil
            end if
            if (n /= 0) then
              n = abs(n)
              if (i4wrk2d(1,n) /= 0) then
                call errmsg('mf6_mod_write_post_map: program error')
              end if
              i4wrk2d(1,n) = gil
              i4wrk2d(2,n) = rlev(il)
              i4wrk2d(3,n) = gir
              i4wrk2d(4,n) = gic
            end if
          end do
        end do
      end do
    end do
    !
    ! check
    do i = 1, nodes
      if (i4wrk2d(1,i) == 0) then
        call errmsg('mf6_mod_write_post_map: program error')
      end if
    end do
    !
    d = '..\..\models\post_mappings\'
    call create_dir(d, .true.)
    f = trim(d)//trim(this%modelname)//'.nodmap.bin'
    call swap_slash(f)
    call open_file(f, iu, 'w', .true.)
    write(iu) this%bb%ic0, this%bb%ic1, this%bb%ir0, this%bb%ir1
    write(iu) nodes ! number of nodes
    write(iu)((i4wrk2d(j,i),j=1,4),i=1,nodes)
    close(iu)
    !
    return
  end subroutine mf6_mod_write_post_map
!
end module