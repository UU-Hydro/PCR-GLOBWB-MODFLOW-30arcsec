module mf6_post_module
  ! -- modules
  use, intrinsic :: iso_fortran_env , only: error_unit, output_unit, &
     i1b => int8, i2b => int16, i4b => int32, i8b => int64, r4b => real32, r8b => real64
  use utilsmod, only: IZERO, DZERO, DONE, tBB, errmsg, logmsg, swap_slash, open_file, chkexist, &
    get_jd, get_ymd_from_jd, ta, writeasc, writeidf, replacetoken
  use pcrModule, only: tmap
  use imod_idf
  !
  implicit none
  !
  private
  !
  ! -- parameters
  character(len=1), parameter :: comment = '#'
  real(r8b), parameter :: r8nodata = -9999.D0
  integer(i4b), parameter :: mxslen = 1024
  !
  integer(i4b), parameter :: i_out_idf = 1
  integer(i4b), parameter :: i_out_asc = 2
  integer(i4b), parameter :: i_out_ipf = 3
  !
  ! -- global variables
  integer(i4b) :: gncol = IZERO
  integer(i4b) :: gnrow = IZERO
  integer(i4b) :: gnlay = IZERO
  real(r8b)    :: gxmin = DZERO
  real(r8b)    :: gymin = DZERO
  real(r8b)    :: gcs   = DZERO
  character(len=mxslen) :: sdate = ''
  character(len=mxslen) :: tilebb = ''
  character(len=mxslen) :: top = ''
  !
  integer(i4b), parameter :: i_idf = 1
  integer(i4b), parameter :: i_map = 2
  
  type tTop
    integer(i4b), pointer               :: i_type => null()
    integer(i4b), pointer               :: ntile => null()
    type(tBb), dimension(:), pointer    :: tilebb => null()
    type(tMap), dimension(:), pointer   :: maps => null()
    type(idfobj), dimension(:), pointer :: idfs => null()
  end type tTop
  type(tTop), pointer :: topmap => null()
  !
  ! types
  type tGen
    character(len=mxslen), pointer        :: in_dir   => null()
    character(len=mxslen), pointer        :: in_postf => null()
    character(len=mxslen), pointer        :: out_dir  => null()
    character(len=mxslen), pointer        :: out_pref => null()
    integer(i4b), pointer                 :: il_min   => null()
    integer(i4b), pointer                 :: il_max   => null()
    logical, pointer                      :: ltop     => null()
    integer(i4b), pointer                 :: i_out    => null()
  end type tGen
  !
  type tPostMod
    integer(i4b), pointer                 :: modid      => null()
    character(len=mxslen), pointer        :: modname    => null()
    integer(i4b), pointer                 :: iu         => null()
    type(tBB), pointer                    :: bb         => null()
    integer(i4b), pointer                 :: nodes      => null()
    integer(i4b), dimension(:,:), pointer :: giliric    => null()
    real(r8b), pointer                    :: totim_read => null()
    real(r8b), dimension(:), pointer      :: top        => null()
    real(r8b), dimension(:), pointer      :: r8buff     => null()
    !
    type(tGen), pointer :: gen => null()
  contains
    procedure :: init        => mf6_post_mod_init
    procedure :: read_nodbin => mf6_post_mod_read_nodbin
    procedure :: read_top    => mf6_post_mod_read_top
    procedure :: read        => mf6_post_mod_read
    procedure :: get_data    => mf6_post_mod_get_data
    procedure :: write       => mf6_post_mod_write
    procedure :: clean       => mf6_post_mod_clean
  end type tPostMod
  !
  type tPostSol
    logical, pointer                      :: lwritesol   => null()
    logical, pointer                      :: lwritemodid => null()
    integer(i4b), pointer                 :: solid     => null()
    character(len=mxslen), pointer        :: solname     => null()
    type(tBB), pointer                    :: bb         => null()
    integer(i4b), pointer                 :: nmod => null()
    type(tPostMod), dimension(:), pointer :: mod  => null()
    !
    type(tGen), pointer :: gen => null()
  contains
    procedure :: init        => mf6_post_sol_init
    procedure :: read_modbin => mf6_post_sol_read_modbin
    procedure :: write       => mf6_post_sol_write
    procedure :: clean       => mf6_post_sol_clean
  end type tPostSol
  !
  save
  !
  public :: tPostMod, tPostSol
  public :: gncol, gnrow, gnlay, gxmin, gymin, gcs, sdate, tilebb, top
  
  contains
  !
! ==============================================================================
! subroutines/functions type tPostMod
! ==============================================================================
  
  subroutine mf6_post_mod_init(this)
! ******************************************************************************
    ! -- arguments
    class(tPostMod) :: this
    ! --- local
    character(len=mxslen) :: f
! ------------------------------------------------------------------------------
    allocate(this%modname)
    write(this%modname,'(a,i5.5)') 'm', this%modid
    !
    call this%read_nodbin()
    !
    if (this%gen%ltop) then
      call this%read_top()
    end if
    !
    allocate(this%iu); this%iu = -1
    allocate(this%totim_read); this%totim_read = DZERO
    !
    ! open the hds file
    f = trim(this%gen%in_dir)//'models\run_output_bin\'// &
      trim(this%modname)//trim(this%gen%in_postf)
    call swap_slash(f)
    !
    call open_file(f, this%iu, 'r', .true.)
    !
    return
  end subroutine mf6_post_mod_init
  
  subroutine mf6_post_mod_read_nodbin(this)
! ******************************************************************************
    ! -- arguments
    class(tPostMod) :: this
    ! --- local
    type(tBb), pointer :: bb => null()
    integer(i4b) :: iu, i, j
    character(len=mxslen) :: f
! ------------------------------------------------------------------------------
    f = trim(this%gen%in_dir)//'models\post_mappings\'//trim(this%modname)// &
      '.nodmap.bin'
    call swap_slash(f)
    !
    allocate(this%bb); bb => this%bb
    allocate(this%nodes)
    call open_file(f, iu, 'r', .true.)
    read(iu) bb%ic0, bb%ic1, bb%ir0, bb%ir1 
    bb%ncol = bb%ic1 - bb%ic0 + 1; bb%nrow = bb%ir1 - bb%ir0 + 1
    read(iu) this%nodes
    allocate(this%giliric(3,this%nodes))
    read(iu)((this%giliric(j,i),j=1,3),i=1,this%nodes)
    close(iu)
    !
    return
  end subroutine mf6_post_mod_read_nodbin
  
  subroutine mf6_post_mod_read_top(this)
! ******************************************************************************
    ! -- arguments
    class(tPostMod) :: this
    ! --- local
    type(tBb), pointer :: bb => null()
    type(tMap), pointer :: map => null()
    type(idfobj), pointer :: idf => null()
    logical :: linbb, lfound
    integer(i4b) :: i, j, gic, gir, ic, ir 
    real(r4b) :: r4val
    real(r4b) :: r4mv
    real(r8b) :: r8val
! ------------------------------------------------------------------------------
    r4mv = -9999.
    if (.not.associated(this%top)) then
      allocate(this%top(this%nodes))
    end if
    !
    do i = 1, this%nodes
      gir = this%giliric(2,i); gic = this%giliric(3,i)
      linbb = .false.; lfound = .false.
      do j = 1, topmap%ntile
        bb => topmap%tilebb(j)
        if ((bb%ir0 <= gir).and.(gir <= bb%ir1).and. &
            (bb%ic0 <= gic).and.(gic <= bb%ic1)) then
          linbb = .true.
        end if
        if (linbb) then
          ic = gic - bb%ic0 + 1; ir = gir - bb%ir0 + 1
          if (topmap%i_type == i_map) then
            map => topmap%maps(j)
            if (.not.associated(map%r4mv)) then !!WORKAROUND!!
              allocate(map%r4mv)
              map%r4mv = r4mv
            end if
            call map%get_val(ic, ir, r4val, r4mv)
            if (r4val /= r4mv) then
              lfound = .true.
              this%top(i) = real(r4val,r8b)
            else
              linbb = .false.
            end if
          else !idf
            idf => topmap%idfs(j)
            r8val = idfgetval(idf,ir,ic)
            if (r8val /= idf%nodata) then
              lfound = .true.
              !if (r8val /= DZERO) then !workaround
                this%top(i) = r8val
              !end if
            else
              linbb = .false.
            end if
          end if
        end if
        if (lfound) exit
      end do
      if (.not.lfound) then
        call errmsg('mf6_post_mod_read_top')
      end if
    end do
    !
    return
  end subroutine mf6_post_mod_read_top
  
  subroutine mf6_post_mod_read(this)
! ******************************************************************************
    ! -- arguments
    class(tPostMod) :: this
    ! --- local
    ! mf6 header:
    character(len=16) :: text_in ! ulasav
    integer(i4b) :: kstp_in, kper_in, ncol_in, nrow_in, ilay_in
    real(r8b) :: pertim_in, totim_in
    !
    logical :: lop
    integer(i4b) :: ios, i
! ------------------------------------------------------------------------------
    !
    inquire(unit=this%iu, opened=lop)
    if (.not.lop) return
    !
    read(unit=this%iu,iostat=ios) kstp_in, kper_in, pertim_in, totim_in, &
      text_in, ncol_in, nrow_in, ilay_in
    !
    if (ios /= 0) then
      this%totim_read = -abs(this%totim_read)
      return
    end if
    !
    ! check
    if (ncol_in /= this%nodes) then
      call errmsg('Invalid number of nodes reading for model '// &
        trim(this%modname)//'.')
    end if
    !
    this%totim_read = totim_in
    !
    if (.not.associated(this%r8buff)) then
      allocate(this%r8buff(this%nodes))
    end if
    read(unit=this%iu,iostat=ios)(this%r8buff(i),i=1,this%nodes)
    !
    if (ios /= 0) then
      call errmsg('Could not read data.')
    end if
    !
    return
  end subroutine mf6_post_mod_read
  
  function mf6_post_get_out_pref(name, totim, il, gen) result(f)
! ******************************************************************************
    ! -- arguments
    character(len=*), intent(in) :: name
    real(r8b), intent(in) :: totim
    integer(i4b), intent(in) :: il
    type(tGen), pointer, intent(in) :: gen
    character(len=:), allocatable :: f
    ! --- local
    character(len=mxslen) :: ts
    integer(i4b) :: y, m, d, idum
    real(r8b) :: jd
! ------------------------------------------------------------------------------
    read(sdate(1:4),*) y; read(sdate(5:6),*) m; read(sdate(7:8),*) d
    jd = get_jd(y, m, d) + totim - DONE
    call get_ymd_from_jd(jd, idum, y, m, d)
    ts = ta((/y/))//ta((/m/),2)//ta((/d/),2)
    
    f = trim(gen%out_dir)//trim(name)//'_'//trim(gen%out_pref)// &
      trim(ts)//'_l'//ta((/il/))
    !
    call swap_slash(f)
    return
  end function mf6_post_get_out_pref
  !
  function mf6_post_mod_get_data(this, il) result(r8x)
! ******************************************************************************
    ! -- arguments
    class(tPostMod) :: this
    integer(i4b), intent(in) :: il
    real(r8b), dimension(:,:), allocatable :: r8x
    ! --- local
    type(tGen), pointer :: gen => null()
    type(tBb), pointer :: bb => null()
    integer(i4b) :: gir, gic, ir, ic, i
! ------------------------------------------------------------------------------
    bb => this%bb; gen => this%gen
    !
    if (allocated(r8x)) deallocate(r8x)
    allocate(r8x(bb%ncol,bb%nrow))
    !
    do ir = 1, bb%nrow
      do ic = 1, bb%ncol
        r8x(ic,ir) = r8nodata
      end do
    end do
    !
    do ir = 1, bb%nrow
      do ic = 1, bb%ncol
        r8x(ic,ir) = r8nodata
      end do
    end do
    !
    do i = 1, this%nodes
     if (abs(this%giliric(1,i)) == il) then
       gir = this%giliric(2,i); gic = this%giliric(3,i) ! global
       ir = gir - bb%ir0 + 1; ic = gic - bb%ic0 + 1 ! local
       !
       !check
       if ((ir < 1).or.(ir > bb%nrow).or.(ic < 1).or.(ic > bb%ncol)) then
         call errmsg('mf6_post_mod_write')
       end if
       if (gen%ltop) then
         !r8x(ic,ir) = this%r8buff(i) - this%top(i)
         r8x(ic,ir) = this%top(i) - this%r8buff(i) !Water table depth > 0!
       else
         r8x(ic,ir) = this%r8buff(i)
       end if
     end if
    end do
    !
    return
  end function mf6_post_mod_get_data
  
  subroutine mf6_post_mod_write(this)
! ******************************************************************************
    ! -- arguments
    class(tPostMod) :: this
    ! --- local
    type(tGen), pointer :: gen => null()
    type(tBb), pointer :: bb => null()
    character(len=mxslen) :: f
    integer(i4b) :: il
    real(r8b) :: xmin, ymin
    real(r8b), dimension(:,:), allocatable :: r8wk
! ------------------------------------------------------------------------------
    !
    bb => this%bb; gen => this%gen
    !
    do il = gen%il_min, gen%il_max
      r8wk = this%get_data(il)
      !
      ! write the file
      f = mf6_post_get_out_pref(this%modname, this%totim_read, il, this%gen)
      !
      xmin = gxmin + (bb%ic0-1)*gcs
      ymin = gymin + (gnrow-bb%ir1)*gcs
      !
      select case(gen%i_out)
      case(i_out_asc)
          call writeasc(trim(f)//'.asc',r8wk, bb%ncol, bb%nrow, &
            xmin, ymin, gcs, r8nodata)
        case(i_out_idf)
          call writeidf(trim(f)//'.idf', r8wk, bb%ncol, bb%nrow, &
            xmin, ymin, gcs, r8nodata)
      end select
    end do
    !
    deallocate(r8wk, this%r8buff)
    !
    return
  end subroutine mf6_post_mod_write
  
  subroutine mf6_post_mod_clean(this)
! ******************************************************************************
    ! -- arguments
    class(tPostMod) :: this
    ! --- local
    logical :: lop
! ------------------------------------------------------------------------------
    if (associated(this%modid))     deallocate(this%modid)
    if (associated(this%modname))   deallocate(this%modname)
    if (associated(this%iu)) then
      inquire(unit=this%iu, opened=lop)
      if (lop) close(this%iu)
      deallocate(this%iu)
    end if
    if (associated(this%bb))         deallocate(this%bb)
    if (associated(this%nodes))      deallocate(this%nodes)
    if (associated(this%giliric))    deallocate(this%giliric)
    if (associated(this%totim_read)) deallocate(this%totim_read)
    if (associated(this%r8buff))     deallocate(this%r8buff)
    if (associated(this%top))        deallocate(this%top)
    !
    this%modid      => null()
    this%modname    => null()
    this%iu         => null()
    this%bb         => null()
    this%nodes      => null()
    this%giliric    => null()
    this%totim_read => null()
    this%r8buff     => null()
    this%top        => null()
    this%gen        => null()
    !
    return
  end subroutine mf6_post_mod_clean
  
! ==============================================================================
! subroutines/functions type tPostSol
! ==============================================================================
  
  subroutine mf6_post_sol_init(this, s)
! ******************************************************************************
    ! -- arguments
    class(tPostSol) :: this
    character(len=*), intent(in) :: s
    ! --- local
    type(tGen), pointer :: gen => null()
    type(tPostMod), pointer :: m => null()
    type(tBb), pointer :: bb, sbb, mbb
    logical :: lok, lex
    integer(i4b) :: i, j, iu
    integer(i4b), dimension(:), allocatable :: i4wk
    character(len=mxslen) :: f
    character(len=mxslen), dimension(10) :: sa
! ------------------------------------------------------------------------------
    !
    !return in case of comment
    if (s(1:1) == comment) then
      call logmsg('Skipping...')
      return
    end if
    !
    ! 1          2 3 4       5 6 7 8   9                                    10
    ! .\mada_ss\ m 3 .ss.hds 1 2 1 idf .\mada_ss\models\run_output_bin_idf\ head_ss_
    i = 1; f = s
    do i = 1, 9
      f = adjustl(f)
      j = index(f,' ')
      sa(i) = f(1:j-1)
      f = adjustl(f(j:))
    end do
    sa(10) = f(1:len_trim(f))
    !read(s,*)(sa(i),i=1,10)
    !
    allocate(this%gen); gen => this%gen
    allocate(gen%in_dir);   gen%in_dir = sa(1)
    allocate(gen%in_postf); gen%in_postf = sa(4)
    allocate(gen%out_dir); gen%out_dir = sa(9)
    allocate(gen%out_pref); gen%out_pref = sa(10)
    allocate(gen%il_min); read(sa(5),*) gen%il_min
    allocate(gen%il_max); read(sa(6),*) gen%il_max
    allocate(gen%ltop)
    read(sa(7),*) i
    if (i == 1) then
      gen%ltop = .true.
      allocate(topmap)
      call open_file(tilebb, iu, 'r')
      allocate(topmap%ntile)
      read(iu,*) topmap%ntile
      allocate(topmap%tilebb(topmap%ntile))
      do i = 1, topmap%ntile
        bb => topmap%tilebb(i)
        read(iu,*) bb%ic0, bb%ic1, bb%ir0, bb%ir1
        bb%ncol = bb%ic1 - bb%ic0 + 1
        bb%nrow = bb%ir1 - bb%ir0 + 1
      end do
      close(iu)
      allocate(topmap%i_type)
      if (index(top,'.map',back=.true.) > 0) then
        topmap%i_type = i_map
      else if (index(top,'.idf',back=.true.) > 0) then
        topmap%i_type = i_idf
      else
        call errmsg("Unrecognize top file format.")
      end if
      if (topmap%i_type == i_map) then
        allocate(topmap%maps(topmap%ntile))
      else
        allocate(topmap%idfs(topmap%ntile))
      end if
      do i = 1, topmap%ntile
        f = top
        call replacetoken(f, '?', i)
        call swap_slash(f)
        inquire(file=f,exist=lex)
        if (.not.lex) then
          call logmsg("Could not find "//trim(f))
        else
          if (topmap%i_type == i_map) then
            lok = topmap%maps(i)%init(f, 1)
          else
            lok = idfread(topmap%idfs(i), f, 0)
          end if
          if (.not.lok) then
            call errmsg('Initializing top.')
          end if
        end if
      end do
    else
      gen%ltop = .false.
    end if
    allocate(gen%i_out)
    select case(trim(sa(8)))
      case('asc')
        gen%i_out = i_out_asc
      case('idf')
        gen%i_out = i_out_idf
      case default
        call errmsg('mf6_post_sol_init')
    end select
    !
    allocate(this%lwritesol, this%lwritemodid, this%nmod)
    this%lwritemodid = .false.
    select case(trim(sa(2)))
      case('m')
        this%lwritesol = .false.
        this%nmod = 1
        allocate(this%mod(1))
        m => this%mod(1)
        allocate(m%modid); read(sa(3),*) m%modid
      case('s','smodid')
        this%lwritesol = .true.
        if (trim(sa(2)) == 'smodid') then !solution + tile
          this%lwritemodid = .true.
        end if
        allocate(this%solid, this%solname)
        read(sa(3),*) this%solid
        write(this%solname,'(a,i2.2)') 's',this%solid
        f = trim(gen%in_dir)//'solutions\post_mappings\'//trim(this%solname)//'.modmap.bin'
        call swap_slash(f)
        call chkexist(f)
        !
        call open_file(f, iu, 'r', .true.)
        read(iu) this%nmod
        allocate(this%mod(this%nmod))
        allocate(i4wk(this%nmod))
        read(iu)(i4wk(i),i=1,this%nmod)
        do i = 1, this%nmod
          m => this%mod(i)
          allocate(m%modid); m%modid = i4wk(i)
        end do
        deallocate(i4wk)
        close(iu)
      case default
        call errmsg('mf6_post_sol_init')
      end select
    !
    ! initialize the models
    do i = 1, this%nmod
      m => this%mod(i)
      m%gen => this%gen
      call m%init()
    end do
    !
    ! determine the bounding box
    allocate(this%bb); sbb => this%bb
    do i = 1, this%nmod
      mbb => this%mod(i)%bb
      sbb%ic0 = min(sbb%ic0, mbb%ic0)
      sbb%ic1 = max(sbb%ic1, mbb%ic1)
      sbb%ir0 = min(sbb%ir0, mbb%ir0)
      sbb%ir1 = max(sbb%ir1, mbb%ir1)
    end do
    sbb%ncol = sbb%ic1 - sbb%ic0 + 1
    sbb%nrow = sbb%ir1 - sbb%ir0 + 1
    !
    return
  end subroutine mf6_post_sol_init
!  
  subroutine mf6_post_sol_read_modbin(this)
! ******************************************************************************
    ! -- arguments
    class(tPostSol) :: this
    ! --- local
! ------------------------------------------------------------------------------
    return
  end subroutine mf6_post_sol_read_modbin
!  
  subroutine mf6_post_sol_write(this)
! ******************************************************************************
    ! -- arguments
    class(tPostSol) :: this
    ! --- local
    type(tBb), pointer :: sbb => null()
    type(tBb), pointer :: mbb => null()
    type(tPostMod), pointer :: m => null()
    character(len=mxslen) :: f
    integer(i4b) :: i, il, ic, ir, jc, jr, gic, gir
    integer(i4b), dimension(:,:), allocatable :: si4wk
    real(r8b) :: t, xmin, ymin
    real(r8b), dimension(:), allocatable :: totimmod
    real(r8b), dimension(:,:), allocatable :: sr8wk, mr8wk
! ------------------------------------------------------------------------------
    !
    if (.not.associated(this%nmod)) return
    !
    allocate(totimmod(this%nmod))
    !
    do while(.true.)
      do i = 1, this%nmod
        m => this%mod(i)
        call m%read()
        totimmod(i) = m%totim_read
        if ((.not.this%lwritesol).and.(m%totim_read > DZERO)) then
          call m%write()
        end if
      end do
      !
      ! check
      t = minval(totimmod)
      if (t /= maxval(totimmod)) then
        call errmsg('Inconsistent kper.')
      end if
      !
      if (t < DZERO) exit
      !
      if (this%lwritesol) then
        sbb => this%bb
        !
        if (this%lwritemodid) then
          allocate(si4wk(sbb%ncol,sbb%nrow))
          do ir = 1, sbb%nrow
            do ic = 1, sbb%ncol
              si4wk(ic,ir) = 0
            end do
          end do
        end if
        !
        allocate(sr8wk(sbb%ncol,sbb%nrow))
        do il = this%gen%il_min, this%gen%il_max
          do ir = 1, sbb%nrow
            do ic = 1, sbb%ncol
              sr8wk(ic,ir) = r8nodata
            end do
          end do
          do i = 1, this%nmod
            m => this%mod(i); mbb => m%bb 
            mr8wk = m%get_data(il)
            do ir = 1, mbb%nrow
              do ic = 1, mbb%ncol
                if (mr8wk(ic,ir) /= r8nodata) then
                  gir = ir + mbb%ir0 - 1; gic = ic + mbb%ic0 - 1
                  jr = gir - sbb%ir0 + 1; jc = gic - sbb%ic0 + 1
                  if ((jr < 1).or.(jr > sbb%nrow).or.(jc < 1).or.(jc > sbb%ncol)) then
                    call errmsg('mf6_post_sol_write')
                  end if
                  sr8wk(jc,jr) = mr8wk(ic,ir)
                  if (this%lwritemodid) then
                    si4wk(jc,jr) = m%modid
                  end if
                end if
              end do
            end do
          end do
          f = mf6_post_get_out_pref(this%solname, t, il, this%gen)
          xmin = gxmin + (sbb%ic0-1)*gcs
          ymin = gymin + (gnrow-sbb%ir1)*gcs
          !
          select case(this%gen%i_out)
            case(i_out_asc)
              call writeasc(trim(f)//'.asc',sr8wk, sbb%ncol, sbb%nrow, &
                xmin, ymin, gcs, r8nodata)
              if (this%lwritemodid) then
                call writeasc(trim(f)//'_modid.asc',si4wk, sbb%ncol, sbb%nrow, &
                  xmin, ymin, gcs, DZERO)
              end if
            case(i_out_idf)
              call writeidf(trim(f)//'.idf', sr8wk, sbb%ncol, sbb%nrow, &
                xmin, ymin, gcs, r8nodata)
              if (this%lwritemodid) then
                call writeidf(trim(f)//'_modid.idf', si4wk, sbb%ncol, sbb%nrow, &
                  xmin, ymin, gcs, DZERO)
              end if
          end select
        end do
        deallocate(sr8wk, mr8wk)
        if (allocated(si4wk)) deallocate(si4wk)
      end if
    end do
    !
    return
  end subroutine mf6_post_sol_write
  
  subroutine mf6_post_clean_gen(gen)
! ******************************************************************************
    ! -- arguments
    type(tGen), intent(inout), pointer :: gen
    ! --- local
! ------------------------------------------------------------------------------
    !
    if (.not.associated(gen)) return
    !
    if (associated(gen%in_dir))   deallocate(gen%in_dir)
    if (associated(gen%in_postf)) deallocate(gen%in_postf)
    if (associated(gen%out_dir))  deallocate(gen%out_dir)
    if (associated(gen%out_pref)) deallocate(gen%out_pref)
    if (associated(gen%il_min))   deallocate(gen%il_min)
    if (associated(gen%il_max))   deallocate(gen%il_max)
    if (associated(gen%ltop))     deallocate(gen%ltop)
    if (associated(gen%i_out))    deallocate(gen%i_out)
    !
    gen%in_dir   => null()
    gen%in_postf => null()
    gen%out_dir  => null()
    gen%out_pref => null()
    gen%il_min   => null()
    gen%il_max   => null()
    gen%ltop     => null()
    gen%i_out    => null()
    !
    return
  end subroutine mf6_post_clean_gen
  
  subroutine mf6_post_sol_clean(this)
! ******************************************************************************
    ! -- arguments
    class(tPostSol) :: this
    ! --- local
    type(tMap), pointer :: map => null()
    integer(i4b) :: i
! ------------------------------------------------------------------------------
    !
    if (associated(this%lwritesol)) deallocate(this%lwritesol)
    if (associated(this%solid))     deallocate(this%solid)
    if (associated(this%solname))   deallocate(this%solname)
    if (associated(this%bb))        deallocate(this%bb)
    !
    if (associated(topmap)) then
      if (associated(topmap%maps)) then
        do i = 1, topmap%ntile
          map => topmap%maps(i)
          call map%clean()
        end do
        deallocate(topmap%maps)
      end if
      if (associated(topmap%idfs)) then
        call idfdeallocate(topmap%idfs, topmap%ntile) 
      end if
      deallocate(topmap%ntile)
      deallocate(topmap%tilebb)
      deallocate(topmap); topmap => null()
    end if
    !
    if (associated(this%gen)) then
      call mf6_post_clean_gen(this%gen)
      deallocate(this%gen)
      this%gen => null()
    end if
    !
    if (associated(this%nmod)) then
      do i = 1, this%nmod
        call this%mod(i)%clean()
      end do
      deallocate(this%nmod)
      deallocate(this%mod)
    end if
    !
    this%lwritesol => null()
    this%solid     => null()
    this%solname   => null()
    this%bb        => null()
    this%nmod      => null()
    this%mod       => null()
    !
    return
  end subroutine mf6_post_sol_clean
  
end module mf6_post_module