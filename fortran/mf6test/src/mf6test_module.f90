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
  use imod_idf
  
  integer(i4b),          parameter :: mxslen = 1024
  character(len=mxslen), parameter :: resultsbindir = '..\..\models\run_output_bin'
  character(len=mxslen), parameter :: resultslstdir = '..\..\models\run_output_lst'
  logical                          :: ltransient = .false.
  character(len=2)                 :: ctim = 'ss'
  
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
  integer(i4b), parameter :: jp = 1
  integer(i4b), parameter :: jt = 2
  integer(i4b), parameter :: jn = 3
  integer(i4b), parameter :: jw = 4
  integer(i4b), parameter :: je = 5
  integer(i4b), parameter :: js = 6
  integer(i4b), parameter :: jb = 7
  integer(i4b), parameter :: ns = jb
  
  private
  
  integer(i4b), parameter :: i_undefined = 0
  integer(i4b), parameter :: i_idf       = 1
  integer(i4b), parameter ::  maxnraw = 1000

  type tData
    character(mxslen)       :: s = ''
    integer(i4b)            :: file_type = i_undefined
    type(idfobj),   pointer :: idf     => null()
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
  
  ! work arrays
  character(len=mxslen), dimension(:),     pointer :: cwrk1d => null()
  character(len=mxslen), dimension(:,:),   pointer :: cwrk2d => null()
  integer(i1b),          dimension(:),     pointer :: i1wrk   => null()
  integer(i1b),          dimension(:),     pointer :: i1wrk2  => null()
  integer(i4b),          dimension(:),     pointer :: i4wrk1d => null()
  integer(i4b),          dimension(:,:),   pointer :: i4wrk2d => null()
  integer(i4b),          dimension(:,:,:), pointer :: i4wrk3d => null()
  real(r8b),             dimension(:),     pointer :: r8wrk   => null()
  real(r8b),             dimension(:),     pointer :: r8wrk2  => null()
  real(r8b),             dimension(:),     pointer :: r8wrk3  => null()
  
  type tDisu
    integer(i4b),               pointer :: nodes => null()
    integer(i4b),               pointer :: nja   => null()
    integer(i4b), dimension(:), pointer :: iac   => null() !length nodes
    integer(i4b), dimension(:), pointer :: ja    => null() !length nja
    integer(i4b), dimension(:), pointer :: ihc   => null() !length nja
    real(r8b),    dimension(:), pointer :: cl12  => null() !length nja
    real(r8b),    dimension(:), pointer :: hwva  => null() !length nja
  end type tDisu
  
  type tExchange
    type(tBb),                    pointer :: bb          => null()
    integer(i4b),                 pointer :: m1mod       => null() !local model index for solution
    integer(i4b),                 pointer :: m2mod       => null()
    character(len=mxslen),        pointer :: m2modelname => null()
    integer(i4b),                 pointer :: nexg        => null()
    integer(i4b), dimension(:),   pointer :: cellidm1    => null()
    integer(i4b), dimension(:),   pointer :: cellidm2    => null()
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
    procedure :: clean           => mf6_sol_clean
  end type tMf6_sol
  
  ! public variables
  public :: tMf6_mod
  public :: tMf6_sol
  public :: tExchange
  public :: i1b, i4b, i8b, r4b, r8b
  public :: raw
  public :: gncol, gnrow, gnlay, gxmin, gymin, gcs
  
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
          !
          !set absolute paths
          call get_abs_path(dat%s)
          !
          ! check for supported file types
          s = change_case(words(2), 'l')
          i = index(s, '.idf', back=.true.)
          if (i > 0) then
            dat%file_type = i_idf
            allocate(dat%idf)
          end if
          !
          ! allocate and read header
          select case(dat%file_type)
          case(i_idf)
            allocate(dat%idf)
            if (.not.idfread(dat%idf, dat%s, 0)) then
              call errmsg('Could not read '//trim(dat%s))
            end if 
          end select
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
        call errmsg('Error: key '//trim(key)// 'not found.')
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
        call errmsg('Error: key '//trim(key)// 'not found.')
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
        call errmsg('Error: key '//trim(key)// 'not found.')
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
    real(r8b), parameter :: thkmin = 0.1d0
    type(tDisu), pointer :: disu => null()
    integer(i4b) :: iact, il, ir, ic
    integer(i4b), dimension(ns) :: ihc
    integer(i4b), dimension(ns) :: st
    !        p t n w e s b
    data ihc/0,0,1,1,1,1,0/
    real(r8b) :: topval, botval
    real(r8b), dimension(ns) :: hwva, cl12
! ------------------------------------------------------------------------------
    !
    hwva(jp) = DZERO
    hwva(jn) = gcs
    hwva(js) = gcs
    hwva(jw) = gcs
    hwva(je) = gcs
    hwva(jt) = gcs*gcs
    hwva(jb) = gcs*gcs
    !
    allocate(this%disu)
    disu => this%disu
    !
    call clear_wrk()
    allocate(i4wrk3d(this%ncol,this%nrow,gnlay))
    allocate(this%layer_nodes(gnlay))
    allocate(disu%nodes)
    disu%nodes = 0; this%layer_nodes = 0
    do il = 1, gnlay
      do ir = 1, this%nrow
        do ic = 1, this%ncol
          disu%nodes = disu%nodes + 1
          i4wrk3d(ic,ir,il) = disu%nodes
        end do
      end do
      this%layer_nodes(il) = this%nrow*this%ncol
    end do
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
      s = 0; cl12 = DZERO
      do il = 1, gnlay
        topval = raw%getr8('top',il)
        botval = raw%getr8('bot',il)
        do ir = 1, this%nrow
          do ic = 1, this%ncol
            n = i4wrk3d(ic,ir,il)
            st(jp) = n !CENTER
            if (ir > 1) then !NORTH
              st(jn) = i4wrk3d(ic,ir-1,il)
              if (st(jn) /= 0) then
                cl12(jn) = gcs/2
              end if
            end if
            if (ir < this%nrow) then !SOUTH
              st(js) = i4wrk3d(ic,ir+1,il)
              if (st(js) /= 0) then
                cl12(js) = gcs/2
              end if
            end if
            if (ic > 1) then !WEST
              st(jw) = i4wrk3d(ic-1,ir,il)
              if (st(jw) /= 0) then
                cl12(jw) = gcs/2
              end if
            end if
            if (ic < this%ncol) then !EAST
              st(je) = i4wrk3d(ic+1,ir,il)
              if (st(je) /= 0) then
                cl12(je) = gcs/2
              end if
            end if
            if (il > 1) then !TOP
              st(jt) = i4wrk3d(ic,ir,il-1)
              cl12(jt) = max(thkmin, topval-botval)
              cl12(jt) = cl12(jt)/2
            end if
            if (il < gnlay) then !BOT
              st(jb) = i4wrk3d(ic,ir,il+1)
              cl12(jb) = max(thkmin, topval-botval)
              cl12(jb) = cl12(jb)/2
            end if
            do i = 1, ns
              if (st(i) /= 0) then
                if (st(i) < 0) then
                  call errmsg('mf6_mod_set_disu: program error')
                end if
                disu%iac(n) = disu%iac(n) + 1
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
              end if
              st(i) = 0
              cl12(i) = DZERO
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

  subroutine mf6_mod_write(this, pir, pic, pnr, pnc)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    integer(i4b), intent(in) :: pir
    integer(i4b), intent(in) :: pic
    integer(i4b), intent(in) :: pnr
    integer(i4b), intent(in) :: pnc
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
    
    pckact(isto) = raw%geti('act_sto',idef=0)
    pckact(ichd) = raw%geti('act_chd',idef=0)
    pckact(iriv) = raw%geti('act_riv',idef=0)
    pckact(idrn) = raw%geti('act_drn',idef=0)
    pckact(ighb) = raw%geti('act_ghb',idef=0)
    pckact(ichd) = raw%geti('act_rch',idef=0)
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
! ------------------------------------------------------------------------------
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
    type(tDisu), pointer :: disu
    character(len=mxslen) :: p, pb, f
    integer(i4b) :: il, ir, ic, n
    real(r8b) :: topval, botval
! ------------------------------------------------------------------------------
    !
    if (pckact(idisu) == 0) return
    !
    call clear_wrk()
    !
    ! set disu
    call this%set_disu()
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
    allocate(r8wrk(disu%nodes),r8wrk2(disu%nodes))
    n = 0
    do il = 1, gnlay
      topval = raw%getr8('top',il)
      botval = raw%getr8('bot',il)
      do ir = 1, this%nrow
        do ic = 1, this%ncol
          n = n + 1
          r8wrk(n) = topval
          r8wrk2(n) = botval
        end do
      end do
    end do
    
    write(iu,'(2x,a)') 'TOP'
    f = trim(pb)//'.disu.top'; call this%write_array(iu, 4, f, r8wrk, lbin, lbinpos)
    write(iu,'(2x,a)') 'BOT'
    f = trim(pb)//'.disu.bot'; call this%write_array(iu, 4, f, r8wrk2, lbin, lbinpos)
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
! ------------------------------------------------------------------------------
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
    character(len=mxslen) :: p, pb, f
! ------------------------------------------------------------------------------
    if (pckact(inpf) == 0) return
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
    write(iu,'(2x,a)') 'K'
    
    
    call this%get_array(i_k, 1, 0, 1, i1wrk, r8wrk) !i_k_l1
    if (gnlay == 2) call this%get_array(i_k, 2, 0, 2, i1wrk, r8wrk) !i_k_l2
    
    
    
    f = trim(pb)//'.npf.k'; call this%write_array(iu, 4, f, r8wrk, lbin, lbinpos)
    call clear_wrk()
    write(iu,'(2x,a)') 'K33'
    
    
    if (gnlay > 1) then
      
      call this%get_array(i_k33, 1, 0, 1, i1wrk, r8wrk) !i_k33_l1
      call this%get_array(i_k33, 2, 0, 2, i1wrk, r8wrk) !i_k33_l2
      
      
      
      f = trim(pb)//'.npf.k33'; call this%write_array(iu, 4, f, r8wrk, lbin, lbinpos)
    else
      write(iu,'(4x,a)') 'CONSTANT 0.1'
    end if
    call clear_wrk()
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
! ------------------------------------------------------------------------------
    !
    if (pckact(idrn) == 0) return
    !
    call errmsg('DRN package not yet supported.')
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
! ------------------------------------------------------------------------------
    !
    if (pckact(idrn) == 0) return
    !
    call errmsg('DRN package not yet supported.')
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
! ------------------------------------------------------------------------------
    !
    if (pckact(irch) == 0) return
    !
    call errmsg('RCH package not yet supported.')
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
! ------------------------------------------------------------------------------
    !
    if (pckact(iwel) == 0) return
    !
    call errmsg('WEL package not yet supported.')
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
        write(iu,'(a)') ta((/xch%cellidm1(iexg), xch%cellidm2(iexg)/))//' 1 '//&
                        ta((/gcs/2, gcs/2, gcs/))
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
    if (associated(i4wrk1d)) then
      deallocate(i4wrk1d)
      i4wrk1d => null()
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
          write(iu,'(a)') '::pause>nul'
          close(iu)
        end if
      end do
    end do
    !
    return
  end subroutine mf6_sol_write_mfsim
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
      c%nlvar(2,j) = raw%getc(trim(c%nlvar(1,j)),cdef=trim(c%nlvar(2,j)))
    end do
    do j = 1, nlvar
      c%lvar(2,j) = raw%getc(trim(c%lvar(1,j)),cdef=trim(c%lvar(2,j)))
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

  
  
end module