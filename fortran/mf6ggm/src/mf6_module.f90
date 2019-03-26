! ==============================================================================
module mf6_module
   ! -- modules
  use, intrinsic :: iso_fortran_env , only: error_unit, output_unit, &
     i1b => int8, i4b => int32, i8b => int64, r4b => real32, r8b => real64
  use utilsmod, only: getlun, chkexist, readline, change_case, errmsg, logmsg, &
    readidf_block, readidf_val, writeidf, checkdim, addboundary, tUnp, &
    calc_unique, sa, open_file, create_dir, swap_slash, tas, ta, get_rel_up
  use imod_idf
  
  implicit none 
  
  ! parameters
  integer(i4b),          parameter :: mxslen = 1024
  character(len=1),      parameter :: slash = '\' !DOS/LINUX slash
  character(len=mxslen), parameter :: resultsdir = 'results'
  integer(i4b),          parameter :: nlay = 2
  real(r8b),             parameter :: delrc = 0.008333333333333D0
  real(r8b),             parameter :: DZERO = 0.D0
  real(r8b),             parameter :: DONE  = 1.D0
  integer(i4b),          parameter :: dcell = 5 ! boundary
  !
  ! stencil
  integer(i4b), parameter :: jp = 1
  integer(i4b), parameter :: jt = 2
  integer(i4b), parameter :: jn = 3
  integer(i4b), parameter :: jw = 4
  integer(i4b), parameter :: je = 5
  integer(i4b), parameter :: js = 6
  integer(i4b), parameter :: jb = 7
  integer(i4b), parameter :: ns = jb
  integer(i4b), dimension(ns) :: s
  integer(i4b), dimension(2,ns) :: sicir
  
  private
  
  integer(i4b), parameter :: i_top          = 1
  integer(i4b), parameter :: i_bot_l1       = 2
  integer(i4b), parameter :: i_bot_l2       = 3
  integer(i4b), parameter :: i_k_l1         = 4
  integer(i4b), parameter :: i_k_l2         = 5
  integer(i4b), parameter :: i_k33_l1       = 6
  integer(i4b), parameter :: i_k33_l2       = 7
  integer(i4b), parameter :: i_strt_l1      = 8
  integer(i4b), parameter :: i_strt_l2      = 9
  integer(i4b), parameter :: i_drn_elev_l1  = 10
  integer(i4b), parameter :: i_drn_elev_l2  = 11
  integer(i4b), parameter :: i_drn_cond     = 12
  integer(i4b), parameter :: i_riv_stage_l1 = 13
  integer(i4b), parameter :: i_riv_cond     = 14
  integer(i4b), parameter :: i_riv_rbot_l1  = 15
  integer(i4b), parameter :: i_recharge     = 16
  integer(i4b), parameter :: i_part         = 17
  integer(i4b), parameter :: i_sol          = 18
  integer(i4b), parameter :: ngdat          = i_sol
  character(len=12), dimension(ngdat) :: gdat_label
                  !123456789012
  data gdat_label/'top         ', 'bot_l1      ', 'bot_l2      ', &
                  'k_l1        ', 'k_l2        ', 'k33_l1      ', &
                  'k33_l2      ', 'strt_l1     ', 'strt_l2     ', &
                  'drn_elev_l1 ', 'drn_elev_l2 ', 'drn_cond    ', &
                  'riv_stage_l1', 'riv_cond    ', 'riv_rbot_l1 ', &
                  'recharge    ', 'partitions  ', 'solutions   ' /
  type tGdat
    character(len=mxslen), pointer :: f =>  null()
    type(idfobj)         , pointer :: idf => null() 
  end type tGdat
  type(tGdat), dimension(:), pointer :: gdat => null()
  !
  ! solver
  integer(i4b), parameter :: i_print_option      = 1
  integer(i4b), parameter :: i_complexity        = 2
  integer(i4b), parameter :: i_outer_hclose      = 3
  integer(i4b), parameter :: i_outer_maximum     = 4
  integer(i4b), parameter :: i_inner_maximum     = 5
  integer(i4b), parameter :: i_inner_hclose      = 6
  integer(i4b), parameter :: i_inner_rclose      = 7
  integer(i4b), parameter :: i_relaxation_factor = 8
  integer(i4b), parameter :: nsolver = i_relaxation_factor
  character(len=17), dimension(nsolver) :: solver_label
  data solver_label/'print_option     ', 'complexity       ', &
                    'outer_hclose     ', 'outer_maximum    ', &
                    'inner_maximum    ', 'inner_hclose     ', &
                    'inner_rclose     ', 'relaxation_factor'/
  character(len=mxslen), dimension(nsolver) :: solverdat
  
  integer(i4b) :: nparts     = 0
  integer(i4b) :: gncol      = 0
  integer(i4b) :: gnrow      = 0
  integer(i4b) :: nsol       = 0
  integer(i4b) :: nsol_indep = 0
  integer(i4b) :: nsol_dep   = 0
    
  real(r4b), dimension(:,:), pointer :: r4a => null()
  !
  ! work arrays
  integer(i4b), dimension(:), pointer   :: iwrk1d  => null()
  integer(i4b), dimension(:,:), pointer :: iwrk2d  => null()
  integer(i1b), dimension(:), pointer   :: i1wrk   => null()
  real(r8b), dimension(:), pointer      :: r8wrk   => null()
  real(r8b), dimension(:), pointer      :: r8wrk2  => null()
  real(r8b), dimension(:), pointer      :: r8wrk3  => null()
  
  type tDisu
    integer(i4b),               pointer :: nodes => null()
    integer(i4b),               pointer :: nja   => null()
    integer(i4b), dimension(:), pointer :: iac   => null() !length nodes
    integer(i4b), dimension(:), pointer :: ja    => null() !length nja
    integer(i4b), dimension(:), pointer :: ihc   => null() !length nja
    real(r8b),    dimension(:), pointer :: cl12  => null() !length nja
    real(r8b),    dimension(:), pointer :: hwva  => null() !length nja
  end type tDisu
  
  type tReg
    integer(i4b),                 pointer :: ir0             => null()
    integer(i4b),                 pointer :: ir1             => null()
    integer(i4b),                 pointer :: ic0             => null()
    integer(i4b),                 pointer :: ic1             => null()
    integer(i4b),                 pointer :: ncol            => null()
    integer(i4b),                 pointer :: nrow            => null()
    integer(i4b),                 pointer :: layer_nodes     => null()
    integer(i4b),                 pointer :: layer_chd_next  => null()
    integer(i4b),                 pointer :: layer_chd_nint  => null()
    integer(i4b), dimension(:,:), pointer :: nodmap          => null()
    integer(i4b), dimension(:,:), pointer :: bndmap          => null()
  end type tReg
  
  type tExchange
    integer(i4b),                 pointer :: m1mod    => null()
    integer(i4b),                 pointer :: m1part   => null()
    integer(i4b),                 pointer :: m2mod    => null()
    integer(i4b),                 pointer :: m2part   => null()
    integer(i4b),                 pointer :: nexg     => null()
    integer(i4b), dimension(:),   pointer :: cellidm1 => null()
    integer(i4b), dimension(:),   pointer :: cellidm2 => null()
    integer(i4b), dimension(:,:), pointer :: gicirm1  => null()
    integer(i4b), dimension(:,:), pointer :: gicirm2  => null()
  end type tExchange
  
  type tMf6_mod
    character(len=mxslen),         pointer :: modelname   => null()
    character(len=mxslen),         pointer :: rootdir     => null()
    integer(i4b),                  pointer :: layer_nodes => null()
    integer(i4b),                  pointer :: nreg        => null()
    type(tReg), dimension(:),      pointer :: reg         => null()
    type(tDisu),                   pointer :: disu        => null()
    integer(i4b),                  pointer :: nxch        => null()
    type(tExchange), dimension(:), pointer :: xch         => null()
    logical,                       pointer :: lchd_int    => null()
    logical,                       pointer :: lchd_ext    => null()
    integer(i4b),                  pointer :: ir0         => null()
    integer(i4b),                  pointer :: ir1         => null()
    integer(i4b),                  pointer :: ic0         => null()
    integer(i4b),                  pointer :: ic1         => null()
    integer(i4b),                  pointer :: ncol        => null()
    integer(i4b),                  pointer :: nrow        => null()
  contains
    procedure :: get_model_name => mf_mod_get_model_name
    procedure :: set_disu       => mf6_mod_set_disu
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
    procedure :: write_nam   => mf6_mod_write_nam
    procedure :: write_disu  => mf6_mod_write_disu
    procedure :: write_ic    => mf6_mod_write_ic
    procedure :: write_oc    => mf6_mod_write_oc
    procedure :: write_npf   => mf6_mod_write_npf
    procedure :: write_chd   => mf6_mod_write_chd
    procedure :: write_drn   => mf6_mod_write_drn
    procedure :: write_riv   => mf6_mod_write_riv
    procedure :: write_rch   => mf6_mod_write_rch
  end type tMf6_mod
  
  type tMf6
    integer(i4b),                 pointer :: isol       => null()
    integer(i4b),                 pointer :: ir0        => null()
    integer(i4b),                 pointer :: ir1        => null()
    integer(i4b),                 pointer :: ic0        => null()
    integer(i4b),                 pointer :: ic1        => null()
    integer(i4b),                 pointer :: ncol       => null()
    integer(i4b),                 pointer :: nrow       => null()
    !
    integer(i4b),                 pointer :: maxpart    => null()
    integer(i4b), dimension(:,:), pointer :: part       => null()
    integer(i4b), dimension(:),   pointer :: partmap    => null() 
    integer(i4b), dimension(:),   pointer :: partmapinv => null() 
    integer(i4b), dimension(:,:), pointer :: partbb     => null()
    !
    character(len=mxslen),        pointer :: rootdir => null()
    character(len=mxslen),        pointer :: solname => null()
    integer(i4b),                 pointer :: nmod    => null() !number of partitions
    type(tMf6_mod), dimension(:), pointer :: mod     => null()
  contains
    procedure :: init            => mf6_init
    procedure :: exchange_init   => mf6_exchange_init
    procedure :: write           => mf6_write
    procedure :: write_mfsim     => mf6_write_mfsim
    procedure :: write_tdis      => mf6_write_tdis
    procedure :: write_ims       => mf6_write_ims
    procedure :: write_exchanges => mf6_write_exchanges
  end type tMf6
  
  public :: tMf6
  
  save
  
  contains
  
  ! ==============================================================================
  
  subroutine clear_wrk()
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
! ------------------------------------------------------------------------------
    if (associated(iwrk1d)) then
      deallocate(iwrk1d)
      iwrk1d => null()
    end if
    if (associated(iwrk2d)) then
      deallocate(iwrk2d)
      iwrk2d => null()
    end if
    if (associated(i1wrk)) then
      deallocate(i1wrk)
      i1wrk => null()
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
  end subroutine clear_wrk
  
! ==============================================================================
  
  subroutine mf6_init(this, f_in)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6) :: this
    character(len=*), intent(inout) :: f_in
    ! -- local
    character(len=mxslen) :: s, key, f, val
    integer(i4b) :: iu, ju, n, i, j, ifound, idum, jsol, imod, ireg
    integer(i4b) :: ir0, ir1, ic0, ic1, ir, ic, jr, jc, kr, kc, nr, nc, nreg
    integer(i4b), dimension(:,:), allocatable :: regun
    real(r4b) :: xll, yll, cs, nodata !DEBUG
    type(tUnp), dimension(:), allocatable :: regbb
    type(tMf6_mod), pointer :: mod => null()
    type(tReg), pointer     :: reg => null()
! ------------------------------------------------------------------------------
    !
    ! solver initialization
    solverdat(i_print_option)      = 'ALLITER'
    solverdat(i_complexity)        = 'SIMPLE'
    solverdat(i_outer_hclose)      = '0.001'
    solverdat(i_outer_maximum)     = '50'
    solverdat(i_inner_maximum)     = '30'
    solverdat(i_inner_hclose)      = '0.001'
    solverdat(i_inner_rclose)      = '1000'
    solverdat(i_relaxation_factor) = '0.98'
    !
    call logmsg('Reading '//trim(f_in)//'...')
    call open_file(f_in, iu)
    !
    ! read the solution ID
    call readline(iu, s)
    allocate(this%isol)
    read(s,*) this%isol
    !
    ! set solution name
    allocate(this%solname)
    write(this%solname,'(a,i2.2)') 's', this%isol
    !
    ! root directory
    allocate(this%rootdir)
    call readline(iu, this%rootdir)
    call create_dir(this%rootdir)
    !
    ! read the partition file
    call readline(iu, f)
    call open_file(f, ju)
    call readline(ju, s); read(s,*) nparts
    call readline(ju, s); read(s,*) gncol, gnrow
    call readline(ju, s)
    call readline(ju, s)
    call readline(ju, s)
    call readline(ju, s); read(s,*) nsol_indep, nsol_dep, nsol
    call readline(ju, s)
    allocate(this%ir0, this%ir1, this%ic0, this%ic1, this%nrow, this%ncol)
    jsol = 0
    do i = 1, nsol_indep
      call readline(ju, s); read(s,*) jsol
      if (this%isol == jsol) then
        read(s,*) jsol, idum, this%ir0, this%ir1, this%ic0, this%ic1
      end if
    end do
    do i = 1, nsol_dep
      call readline(ju, s); read(s,*) jsol
      if (this%isol == jsol) then
        read(s,*) jsol, idum, idum, this%ir0, this%ir1, this%ic0, this%ic1
      end if
    end do
    close(ju)
    !
    ! create output directory
    f= trim(this%rootdir)//trim(resultsdir)
    call create_dir(f)
    !
    ! check
    if (jsol == 0) then
      call errmsg('Reading solution bounding box.')
    end if
    !
    ! read input data and open
    allocate(gdat(ngdat))
    call readline(iu, s)
    read(s,*) n
    do i = 1, n
      call readline(iu, s)
      read(s,*) key, f
      call chkexist(f)
      key = change_case(key, 'l')
      ifound = -1
      do j = 1, ngdat
        if (key == gdat_label(j)) then
          ifound = j
          exit
        end if
      end do
      if (ifound == -1) then
        call errmsg('Key '//trim(key)//' not found.')
      end if
      allocate(gdat(ifound)%f)
      call swap_slash(f); gdat(ifound)%f = f
      allocate(gdat(ifound)%idf) ! read header
      if (.not.idfread(gdat(ifound)%idf, f, 0)) then
        call errmsg('Could not read '//trim(f))
      end if 
    end do
      
    if (.false.) then !DEBUG
      ir0 = 4233; ir1 = 5000; ic0 = 21899; ic1 = 22635
      nodata = gdat(i_strt_l1)%idf%nodata
      call readidf_block(gdat(i_strt_l1)%idf, ir0, ir1, ic0, ic1, r4a, nodata)
      xll = gdat(i_strt_l1)%idf%xmin; yll = gdat(i_strt_l1)%idf%ymin
      cs = gdat(i_strt_l1)%idf%dx
      call writeidf('test.idf', r4a, ic1-ic0+1, ir1-ir0+1, &
        xll+(ic0-1)*cs, yll+(gdat(i_strt_l1)%idf%nrow-ir1)*cs, cs, nodata); stop
    end if
    !
    ! solver options
    call readline(iu, s)
    read(s,*) n
    do i = 1, n
      call readline(iu, s)
      read(s,*) key, val
      key = change_case(key, 'l')
      ifound = -1
      do j = 1, nsolver
        if (key == solver_label(j)) then
          ifound = j
          exit
        end if
      end do
      if (ifound == -1) then
        call errmsg('Key '//trim(key)//' not found.')
      end if
      solverdat(ifound) = val
    end do
    !
    close(iu)
    !
    ! make the bounding box a little larger to include the boundary
    this%ir0 = max(this%ir0-1,1+dcell); this%ir1 = min(this%ir1+1,gnrow-dcell-1)
    this%ic0 = max(this%ic0-1,1+dcell); this%ic1 = min(this%ic1+1,gncol-dcell-1)
    this%nrow = this%ir1 - this%ir0 + 1; this%ncol = this%ic1 - this%ic0 + 1
    !
    ! read the partition and solution
    call readidf_block(gdat(i_part)%idf, this%ir0, this%ir1, this%ic0, this%ic1, &
      this%part, 0)
    call readidf_block(gdat(i_sol)%idf, this%ir0, this%ir1, this%ic0, this%ic1, &
      iwrk2d, 0)
    call checkdim(size(this%part,1), size(this%part,2), &
      size(iwrk2d,1), size(iwrk2d,2))
    !
    do ir = 1, size(this%part,2)
      do ic = 1, size(this%part,1)
        if (iwrk2d(ic,ir) /= this%isol) then
          this%part(ic,ir) = 0
        end if
      end do 
    end do
    deallocate(iwrk2d); iwrk2d => null() 
    !
    ! add the constant head boundary
    call addboundary(this%part, size(this%part,1), size(this%part,2))
    !
    if (.false.) then !DEBUG
      xll = gdat(i_part)%idf%xmin; yll = gdat(i_part)%idf%ymin
      cs = gdat(i_part)%idf%dx
      call writeidf('part.idf', this%part, this%ic1-this%ic0+1, this%ir1-this%ir0+1, &
        xll+(this%ic0-1)*cs, yll+(gdat(i_part)%idf%nrow-this%ir1)*cs, cs, 0.); stop
    end if
    
    ! create some mappings
    allocate(this%maxpart)
    this%maxpart = maxval(this%part)
    allocate(this%partmap(this%maxpart))
    do i = 1, this%maxpart
      this%partmap(i) = 0
    end do
    do ir = 1, size(this%part,2)
      do ic = 1, size(this%part,1)
        j = abs(this%part(ic,ir))
        if (j /= 0) then
          this%partmap(j) = 1
        end if
      end do
    end do
    !
    allocate(this%nmod)
    this%nmod = sum(this%partmap)
    allocate(this%partmapinv(this%nmod))
    this%nmod = 0
    do i = 1, this%maxpart
      j = this%partmap(i)
      if (j == 1) then
        this%nmod = this%nmod + 1
        this%partmap(i) = this%nmod
        this%partmapinv(this%nmod) = i
      end if
    end do
    !
    ! bounding box
    allocate(this%partbb(4,this%nmod))
    do i = 1, this%nmod
      this%partbb(1,i) = gnrow
      this%partbb(2,i) = 0
      this%partbb(3,i) = gncol
      this%partbb(4,i) = 0
    end do
    do ir = this%ir0, this%ir1
      do ic = this%ic0, this%ic1
        jr = ir-this%ir0+1; jc = ic-this%ic0+1
        i = abs(this%part(jc,jr))
        if (i /= 0) then
          j = this%partmap(i)
          this%partbb(1,j) = min(this%partbb(1,j), ir)
          this%partbb(2,j) = max(this%partbb(2,j), ir)
          this%partbb(3,j) = min(this%partbb(3,j), ic)
          this%partbb(4,j) = max(this%partbb(4,j), ic)
        end if
      end do
    end do
    
    allocate(this%mod(this%nmod))
    do imod = 1, this%nmod !loop over number of partitions
      mod => this%mod(imod)
      allocate(mod%modelname, mod%rootdir)
      call mod%get_model_name(this%partmapinv(imod), mod%modelname)
      mod%rootdir = trim(this%rootdir)//trim(mod%modelname)//'\'
      call create_dir(mod%rootdir)
      ir0 = this%partbb(1,imod); ir1 = this%partbb(2,imod)
      ic0 = this%partbb(3,imod); ic1 = this%partbb(4,imod)
      nr = ir1 - ir0 + 1; nc = ic1 - ic0 + 1
      allocate(iwrk2d(nc,nr))
      do ir = 1, nr
        do ic = 1, nc
          iwrk2d(ic,ir) = 0
        end do
      end do
      do ir = ir0, ir1
        do ic = ic0, ic1
          kr = ir - this%ir0 + 1; kc = ic - this%ic0 + 1
          jr = ir -      ir0 + 1; jc = ic -      ic0 + 1
          i = this%part(kc,kr)
          if (abs(i) == this%partmapinv(imod)) then
            if (i > 0) then
              iwrk2d(jc,jr) = 1
            else
              iwrk2d(jc,jr) = -1
            end if
          end if
        end do
      end do
      !
      ! determine number of independent regions
      call calc_unique(iwrk2d, 9, regun, regbb, nreg, idum, 0., 0., 0.)
      deallocate(iwrk2d); iwrk2d => null()
      !
      if (.false. .and. (nreg >1)) then !DEBUG
        xll = gdat(i_part)%idf%xmin; yll = gdat(i_part)%idf%ymin; cs = gdat(i_part)%idf%dx
        call writeidf('regun.idf', regun, size(regun,1), size(regun,2), &
          xll+(ic0-1)*cs, yll+(gdat(i_part)%idf%nrow-ir1)*cs, cs, 0.); stop
      end if
      
      allocate(mod%nreg, mod%layer_nodes)
      allocate(mod%ir0, mod%ir1, mod%ic0, mod%ic1, mod%ncol, mod%nrow)
      mod%ir0 = gnrow; mod%ir1 = 0; mod%ic0 = gncol; mod%ic1 = 0
      mod%nreg = nreg
      allocate(mod%reg(nreg))
      mod%layer_nodes = 0
      n = 0
      do ireg = 1, nreg
        reg => mod%reg(ireg)
        allocate(reg%ir0, reg%ir1, reg%ic0, reg%ic1, reg%nrow, reg%ncol)
        allocate(reg%layer_nodes, reg%layer_chd_next, reg%layer_chd_nint)
        reg%layer_nodes    = 0
        reg%layer_chd_next = 0
        reg%layer_chd_nint = 0
        !
        reg%ir0 = regbb(ireg)%ir0 + ir0 - 1; reg%ir1 = regbb(ireg)%ir1 + ir0 - 1 !index bb
        reg%ic0 = regbb(ireg)%ic0 + ic0 - 1; reg%ic1 = regbb(ireg)%ic1 + ic0 - 1 !index bb
        mod%ir0 = min(mod%ir0, reg%ir0); mod%ir1 = max(mod%ir1, reg%ir1)
        mod%ic0 = min(mod%ic0, reg%ic0); mod%ic1 = max(mod%ic1, reg%ic1)
        reg%nrow = reg%ir1 - reg%ir0 + 1
        reg%ncol = reg%ic1 - reg%ic0 + 1
        !
        allocate(reg%nodmap(reg%ncol,reg%nrow))
        allocate(reg%bndmap(reg%ncol,reg%nrow))
        do ir = 1, reg%nrow
          do ic = 1, reg%ncol
            reg%nodmap(ic,ir) = 0
            reg%bndmap(ic,ir) = 0
          end do
        end do
        !
        do ir = reg%ir0, reg%ir1
          do ic = reg%ic0, reg%ic1
            kr = ir -     ir0 + 1; kc = ic -     ic0 + 1
            jr = ir - reg%ir0 + 1; jc = ic - reg%ic0 + 1
            i = regun(kc,kr)
            if (abs(i) == ireg) then
              n = n + 1
              reg%layer_nodes = reg%layer_nodes + 1
              reg%nodmap(jc,jr) = n
              if (i < 0) then
                reg%layer_chd_next = reg%layer_chd_next + 1
                reg%bndmap(jc,jr) = -n
              end if
            else
              reg%nodmap(jc,jr) = 0
              reg%bndmap(jc,jr) = 0
            end if
            !if (i < 0) then
            !  reg%bndmap(jc,jr) = -n
            !else
            !  reg%bndmap(jc,jr) = 0
            !end if
          end do
        end do
        n = n + (nlay-1)*reg%layer_nodes
        mod%layer_nodes = mod%layer_nodes + reg%layer_nodes
      end do
      !
      mod%nrow = mod%ir1 - mod%ir0 + 1
      mod%ncol = mod%ic1 - mod%ic0 + 1
      ! determine DISU data structures
    end do !model loop
    !
    ! initialize exchanges
    call this%exchange_init()

  end subroutine mf6_init
  
  subroutine mf6_exchange_init(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6) :: this
    ! -- local
    character(len=mxslen) :: f
    real(r4b) :: xll, yll, cs, nodata !DEBUG
    integer(i4b) :: ic, ir, jc, jr, kc, kr, i, j, imod, jmod, ireg, ixch, nexgf
    integer(i4b) :: iact, n, imod1, imod2, ilay, iu
    type(tMf6_mod), pointer  :: mod  => null()
    type(tMf6_mod), pointer  :: mod1 => null()
    type(tMf6_mod), pointer  :: mod2 => null()
    type(tReg), pointer      :: reg => null()
    type(tExchange), pointer :: xch => null()
! ------------------------------------------------------------------------------
    call logmsg('Initializing exchanges')
    !
    if (.false.) then !DEBUG
      xll = gdat(i_part)%idf%xmin; yll = gdat(i_part)%idf%ymin
      cs = gdat(i_part)%idf%dx
      call writeidf('part.idf', this%part, this%ic1-this%ic0+1, this%ir1-this%ir0+1, &
        xll+(this%ic0-1)*cs, yll+(gdat(i_part)%idf%nrow-this%ir1)*cs, cs, 0.); stop
    end if
    
    allocate(iwrk2d(this%nmod, this%nmod))
    do i = 1, this%nmod
      do j = 1, this%nmod
        iwrk2d(j,i) = 0
      end do
    end do
    !
    do iact = 1, 3
      s = 0
      do imod = 1, this%nmod
        mod => this%mod(imod)
        do ireg = 1, mod%nreg
          reg => mod%reg(ireg)
          do ir = reg%ir0, reg%ir1
            do ic = reg%ic0, reg%ic1
              jr = ir - this%ir0 + 1; jc = ic - this%ic0 + 1
              if (this%part(jc,jr) ==  this%partmapinv(imod)) then
                s(jp) = this%part(jc,jr)
                sicir(1,jp) = jc; sicir(2,jp) = jr
                if (jr > 1) then !NORTH
                  s(jn) = abs(this%part(jc,jr-1))
                  sicir(1,jn) = jc; sicir(2,jn) = jr-1
                end if
                if (jr < this%nrow) then !SOUTH
                  s(js) = abs(this%part(jc,jr+1))
                  sicir(1,js) = jc; sicir(2,js) = jr+1
                end if
                if (jc > 1) then !WEST
                  s(jw) = abs(this%part(jc-1,jr))
                  sicir(1,jw) = jc-1; sicir(2,jw) = jr
                end if
                if (jc < this%ncol) then !EAST
                  s(je) = abs(this%part(jc+1,jr))
                  sicir(1,je) = jc+1; sicir(2,je) = jr
                end if
                do i = 1, ns
                  if ((s(i) /= 0).and.(s(i) /= s(jp))) then
                    jmod = this%partmap(s(i))
                    if (iact == 1) then
                      iwrk2d(jmod,imod) = 1
                    else
                      j = iwrk2d(jmod,imod)
                      if (j > 0) then
                        xch => mod%xch(j)
                        xch%nexg = xch%nexg + 1
                        if (iact == 3) then
                          xch%gicirm1(1,xch%nexg) = sicir(1,jp)
                          xch%gicirm1(2,xch%nexg) = sicir(2,jp)
                          xch%gicirm2(1,xch%nexg) = sicir(1,i)
                          xch%gicirm2(2,xch%nexg) = sicir(2,i)
                        end if
                      end if
                    end if
                  end if
                  s(i) = 0
                end do
              end if
            end do
          end do
        end do
      end do
      if (iact == 1) then
        ! set symmetric part
        nexgf = 0
        do imod = 1, this%nmod
          do jmod = 1, imod
            iwrk2d(jmod,imod) = 0
          end do
        end do
        do imod = 1, this%nmod
          mod => this%mod(imod)
          allocate(mod%nxch)
          mod%nxch = 0
          do jmod = 1, this%nmod
            if (iwrk2d(jmod,imod) == 1) then
              mod%nxch = mod%nxch + 1
              iwrk2d(jmod,imod) = mod%nxch
              nexgf = nexgf + 1
            end if
          end do
          if (mod%nxch > 0) then
            allocate(mod%xch(mod%nxch))
            do i = 1, mod%nxch
              xch => mod%xch(i)
              allocate(xch%nexg, xch%m1mod, xch%m1part, xch%m2mod, xch%m2part)
            end do
            do jmod = 1, this%nmod
              i = iwrk2d(jmod,imod)
              if (i > 0) then
                xch => mod%xch(i)
                xch%m1mod  = imod
                xch%m1part = this%partmapinv(imod)
                xch%m2mod  = jmod
                xch%m2part = this%partmapinv(jmod)
              end if
            end do
          end if
        end do
      end if
      !
      if (iact == 2) then
        do imod = 1, this%nmod
          mod => this%mod(imod)
          do ixch = 1, mod%nxch
            xch => mod%xch(ixch)
            if (xch%nexg > 0) then
              allocate(xch%cellidm1(xch%nexg))
              allocate(xch%cellidm2(xch%nexg))
              allocate(xch%gicirm1(2,xch%nexg))
              allocate(xch%gicirm2(2,xch%nexg))
            end if
          end do
        end do
      end if
      if (iact < 3) then
        ! init
        do imod = 1, this%nmod
          mod => this%mod(imod)
          do ixch = 1, mod%nxch
            xch => mod%xch(ixch)
            xch%nexg = 0
          end do
        end do
      end if
    !
    end do
    !
    deallocate(iwrk2d); iwrk2d => null()
    allocate(iwrk2d(this%ncol,this%nrow))
    do imod1 = 1, this%nmod
      write(sa(1),*) imod1; write(sa(2),*) this%nmod
      write(*,*) 'Processing exchange for model '//tas(sa(1))//'/'//tas(sa(2))//'...'
      mod1 => this%mod(imod1)
      ! "outer" interface nodes (M2)
      do ixch = 1, mod1%nxch
        xch => mod1%xch(ixch)
        imod2 = xch%m2mod
        mod2 => this%mod(imod2)
!        do ir = 1, this%nrow
!          do ic = 1, this%ncol
        do ir = reg%ir0, reg%ir1
          do ic = reg%ic0, reg%ic1
            kr = ir - this%ir0 + 1; kc = ic - this%ic0 + 1
            iwrk2d(kc,kr) = 0
          end do
        end do
        do i = 1, xch%nexg
          ic = xch%gicirm2(1,i); ir = xch%gicirm2(2,i)
          iwrk2d(ic,ir) = -1
        end do
        !if ((this%partmapinv(imod1) == 977).and.(this%partmapinv(imod2)==982)) then
        !  xll = gdat(i_part)%idf%xmin; yll = gdat(i_part)%idf%ymin; cs = gdat(i_part)%idf%dx
        !  call writeidf('iwrk2d.idf', iwrk2d, size(iwrk2d,1), size(iwrk2d,2), &
        !  xll+(this%ic0-1)*cs, yll+(gdat(i_part)%idf%nrow-this%ir1)*cs, cs, 0.)
        !end if
        do ireg = 1, mod2%nreg
          reg => mod2%reg(ireg)
          do ir = reg%ir0, reg%ir1
            do ic = reg%ic0, reg%ic1
              kr = ir - this%ir0 + 1; kc = ic - this%ic0 + 1
              jr = ir - reg%ir0  + 1; jc = ic - reg%ic0  + 1
              n = reg%nodmap(jc,jr); j = abs(iwrk2d(kc,kr))
              if ((j /= 0) .and. (n /= 0)) then
                iwrk2d(kc,kr) = n
                if (reg%bndmap(jc,jr) == 0) then
                  reg%bndmap(jc,jr) = n
                end if
              end if
            end do
          end do
        end do
        do i = 1, xch%nexg
          ic = xch%gicirm2(1,i); ir = xch%gicirm2(2,i)
          n = iwrk2d(ic,ir)
          if (n <= 0) then
            call errmsg('Program error 1')
          end if
          xch%cellidm2(i) = n
        end do
      end do
      !
      ! "inner" interface nodes (M1)
      do ir = 1, this%nrow
        do ic = 1, this%ncol
          iwrk2d(ic,ir) = 0
        end do
      end do
      do ixch = 1, mod1%nxch
        xch => mod1%xch(ixch)
        do i = 1, xch%nexg
          ic = xch%gicirm1(1,i); ir = xch%gicirm1(2,i)
          iwrk2d(ic,ir) = -1
        end do
      end do
      do ireg = 1, mod1%nreg
        reg => mod1%reg(ireg)
        do ir = reg%ir0, reg%ir1
          do ic = reg%ic0, reg%ic1
            kr = ir - this%ir0 + 1; kc = ic - this%ic0 + 1
            jr = ir - reg%ir0  + 1; jc = ic - reg%ic0  + 1
            n = reg%nodmap(jc,jr); j = abs(iwrk2d(kc,kr))
            if ((j /= 0) .and. (n /= 0)) then
              iwrk2d(kc,kr) = n
              if (reg%bndmap(jc,jr) == 0) then
                reg%bndmap(jc,jr) = n
              end if
            end if
          end do
        end do
      end do
      do ixch = 1, mod1%nxch
        xch => mod1%xch(ixch)
        do i = 1, xch%nexg
          ic = xch%gicirm1(1,i); ir = xch%gicirm1(2,i)
          n = iwrk2d(ic,ir)
          if (n <= 0) then
            call errmsg('Program error 2')
          end if
          xch%cellidm1(i) = n
        end do
      end do
      !
    end do
    !
    if (.false.) then
      xll = gdat(i_part)%idf%xmin; yll = gdat(i_part)%idf%ymin; cs = gdat(i_part)%idf%dx
      do imod = 1, this%nmod
        mod => this%mod(imod)
        do ireg = 1, mod%nreg
          reg => mod%reg(ireg)
          write(sa(1),'(i4.4)') this%partmapinv(imod); write(sa(2),'(i4.4)') ireg
          f = tas(sa(1))//'_nodmap_reg'//tas(sa(2))//'.idf'
          call writeidf(f, reg%nodmap, size(reg%nodmap,1), size(reg%nodmap,2), &
            xll+(reg%ic0-1)*cs, yll+(gdat(i_part)%idf%nrow-reg%ir1)*cs, cs, 0.)
          f = tas(sa(1))//'_bndmap_reg'//tas(sa(2))//'.idf'
          call writeidf(f, reg%bndmap, size(reg%nodmap,1), size(reg%nodmap,2), &
            xll+(reg%ic0-1)*cs, yll+(gdat(i_part)%idf%nrow-reg%ir1)*cs, cs, 0.)
        end do
      end do
      stop 1
    end if
    !
    ! write nodmap and bndmap
    f = trim(this%rootdir)//'mappings'
    call create_dir(f)
    f = trim(this%rootdir)//'mappings\'//trim(this%solname)//'.asc'
    call open_file(f, iu, 'w')
    xll = gdat(i_part)%idf%xmin; yll = gdat(i_part)%idf%ymin; cs = gdat(i_part)%idf%dx
    write(iu,'(a)') ta((/gncol, gnrow, this%ncol, this%nrow, nlay, this%ir0, this%ir1, this%ic0, this%ic1/))
    write(iu,'(a)') ta((/this%nmod/))
    do imod = 1, this%nmod
      mod => this%mod(imod)
      !if ((mod%modelname /= 'm000167').and.(mod%modelname /= 'm000168')) cycle
      write(iu,'(a)') trim(mod%modelname)//' '//ta((/mod%ncol, mod%nrow, mod%ir0, mod%ir1, mod%ic0, mod%ic1/))
      call clear_wrk()
      allocate(iwrk2d(mod%ncol,mod%nrow))
      do ilay = 1, nlay
        do ir = 1, mod%nrow
          do ic = 1, mod%ncol
            iwrk2d(ic,ir) = 0
          end do
        end do
        do ireg = 1, mod%nreg
          reg => mod%reg(ireg)
          do ir = reg%ir0, reg%ir1
            do ic = reg%ic0, reg%ic1
              kr = ir - mod%ir0 + 1;  kc = ic - mod%ic0 + 1
              jr = ir - reg%ir0  + 1; jc = ic - reg%ic0 + 1
              n = reg%nodmap(jc,jr)
              if (n /= 0) then
                iwrk2d(kc,kr) = n + (ilay-1)*reg%layer_nodes
              end if
            end do
          end do
        end do
        f = trim(this%rootdir)//'mappings\nodmap_'//trim(mod%modelname)//'_l'//ta((/ilay/))//'.idf'
        call swap_slash(f)
        call writeidf(f, iwrk2d, size(iwrk2d,1), size(iwrk2d,2), &
          xll+(mod%ic0-1)*cs, yll+(gdat(i_part)%idf%nrow-mod%ir1)*cs, cs, 0.)
      end do
      do ir = 1, mod%nrow
        do ic = 1, mod%ncol
          iwrk2d(ic,ir) = 0
        end do
      end do
      do ireg = 1, mod2%nreg
        reg => mod%reg(ireg)
        do ir = reg%ir0, reg%ir1
          do ic = reg%ic0, reg%ic1
            kr = ir - mod%ir0 + 1;  kc = ic - mod%ic0 + 1
            jr = ir - reg%ir0  + 1; jc = ic - reg%ic0  + 1
            n = reg%bndmap(jc,jr)
            if (n > 0) then
              iwrk2d(kc,kr) = 1
            else if (n < 0) then
              iwrk2d(kc,kr) = -1
            end if
          end do
        end do
      end do
      f = trim(this%rootdir)//'mappings\bndmap_'//trim(mod%modelname)//'.idf'
      call swap_slash(f)
      call writeidf(f, iwrk2d, size(iwrk2d,1), size(iwrk2d,2), &
        xll+(mod%ic0-1)*cs, yll+(gdat(i_part)%idf%nrow-mod%ir1)*cs, cs, 0.)
    end do
    close(iu)
    !
    if (.false.) then !DEBUG
      xll = gdat(i_part)%idf%xmin; yll = gdat(i_part)%idf%ymin; cs = gdat(i_part)%idf%dx
      call writeidf('regnodmap.idf', reg%nodmap, size(reg%nodmap,1), size(reg%nodmap,2), &
        xll+(reg%ic0-1)*cs, yll+(gdat(i_part)%idf%nrow-reg%ir1)*cs, cs, 0.)
      call writeidf('iwrk2d.idf', iwrk2d, size(iwrk2d,1), size(iwrk2d,2), &
        xll+(this%ic0-1)*cs, yll+(gdat(i_part)%idf%nrow-this%ir1)*cs, cs, 0.); stop
    end if
    
    write(*,*) 'Number of exchanges:',nexgf
    
  end subroutine mf6_exchange_init
  
  subroutine mf6_write(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6) :: this
    ! -- local
    logical, parameter :: lbin = .true.
    integer(i4b) :: imod
    type(tMf6_mod), pointer :: mod => null()
! ------------------------------------------------------------------------------
    call this%write_tdis()
    call this%write_ims()
    call this%write_exchanges()
    call this%write_mfsim()
    !
    do imod = 1, this%nmod
      mod => this%mod(imod)
      !if ((mod%modelname /= 'm000167').and.(mod%modelname /= 'm000168')) cycle
      call logmsg('***** Processing for model '//trim(mod%modelname)//'...')
      call mod%write_disu(lbin)
      call mod%write_ic(lbin)
      call mod%write_oc()
      call mod%write_npf(lbin)
      call mod%write_chd(lbin)
      call mod%write_drn(lbin)
      call mod%write_riv(lbin)
      call mod%write_rch(lbin)
      call mod%write_nam()
    end do
      
  end subroutine mf6_write
  
! ==============================================================================
  
  subroutine mf6_write_mfsim(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6) :: this
    ! -- local
    character(len=mxslen) :: f
    integer(i4b) :: iu, imod
    type(tMf6_mod), pointer :: mod => null()
! ------------------------------------------------------------------------------
    !
    ! all connected models
    f = trim(this%rootdir)//trim(this%solname)//'.mfsim.nam'
    call open_file(f, iu, 'w')
    write(iu,'(   a)') 'BEGIN OPTIONS'
    write(iu,'(2x,a)') 'MEMORY_PRINT_OPTION SUMMARY'
    write(iu,'(2x,a)') 'DOMAIN_DECOMPOSITION '//ta((/this%nmod/))
    write(iu,'(   a)') 'END OPTIONS'
    write(iu,'(a)')
    write(iu,'(   a)') 'BEGIN TIMING'
    write(iu,'(2x,a)') 'TDIS6 '//trim(this%solname)//'.tdis'
    write(iu,'(   a)') 'END TIMING'
    write(iu,'(a)')
    write(iu,'(   a)') 'BEGIN MODELS'
    write(iu,'(2x,a)') 'OPEN/CLOSE '//trim(this%solname)//'.models.asc'
    write(iu,'(   a)') 'END MODELS'
    write(iu,'(a)')
    write(iu,'(   a)') 'BEGIN EXCHANGES'
    write(iu,'(2x,a)') 'OPEN/CLOSE '//trim(this%solname)//'.exchanges.asc'
    write(iu,'(   a)') 'END EXCHANGES'
    write(iu,'(a)')
    write(iu,'(   a)') 'BEGIN SOLUTIONGROUP 1'
    write(iu,'(2x,a)') 'IMS6 '//trim(this%solname)//'.ims FILEIN '// &
      trim(this%solname)//'.solmodels.wrp.asc'
    write(iu,'(   a)') 'END SOLUTIONGROUP'
    close(iu)
    !
    f = trim(this%rootdir)//trim(this%solname)//'.models.asc'
    call open_file(f, iu, 'w')
    do imod = 1, this%nmod
      mod => this%mod(imod)
      f = 'GWF6 .\'//trim(mod%modelname)//'\'//trim(mod%modelname)//'.ext.nam '// &
        trim(mod%modelname)//' '//ta((/imod/))
      call swap_slash(f)
      write(iu,'(a)') trim(f)
    end do
    close(iu)
    !
    f = trim(this%rootdir)//trim(this%solname)//'.solmodels.wrp.asc'
    call open_file(f, iu, 'w')
    write(iu,'(   a)') 'BEGIN MODELS'
    write(iu,'(2x,a)') 'OPEN/CLOSE '//trim(this%solname)//'.solmodels.asc'
    write(iu,'(   a)') 'END MODELS'
    close(iu)
    !
    f = trim(this%rootdir)//trim(this%solname)//'.solmodels.asc'
    call open_file(f, iu, 'w')
    do imod = 1, this%nmod
      mod => this%mod(imod)
      write(iu,'(a)') trim(mod%modelname)
      !write(iu,'(a)') trim(mod%modelname)//' '//ta((/imod/)) !CGC
    end do
    close(iu)
    !
    ! separate models
    do imod = 1, this%nmod
      mod => this%mod(imod)
      f = trim(this%rootdir)//trim(this%solname)//'.'// &
        trim(mod%modelname)//'.mfsim.nam'
      call open_file(f, iu, 'w')
      write(iu,'(   a)') 'BEGIN OPTIONS'
      write(iu,'(2x,a)') 'MEMORY_PRINT_OPTION SUMMARY'
      write(iu,'(   a)') 'END OPTIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN TIMING'
      write(iu,'(2x,a)') 'TDIS6 '//trim(this%solname)//'.tdis'
      write(iu,'(   a)') 'END TIMING'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN MODELS'
      f = '.\'//trim(mod%modelname)//'\'//trim(mod%modelname)//&
        '.int.ext.nam '//trim(mod%modelname)
      call swap_slash(f)
      write(iu,'(2x,a)') 'GWF6 '//trim(f)
      write(iu,'(   a)') 'END MODELS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN EXCHANGES'
      write(iu,'(   a)') 'END EXCHANGES'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN SOLUTIONGROUP 1'
      write(iu,'(2x,a)') 'IMS6 '//trim(this%solname)//'.ims '//trim(mod%modelname)
      write(iu,'(   a)') 'END SOLUTIONGROUP'
      close(iu)
    end do
  end subroutine mf6_write_mfsim

  subroutine mf6_write_tdis(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6) :: this
    ! -- local
    character(len=mxslen) :: f
    integer(i4b) :: iu
! ------------------------------------------------------------------------------
    f = trim(this%rootdir)//trim(this%solname)//'.tdis'
    call open_file(f, iu, 'w')
    !
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
    
  end subroutine mf6_write_tdis
    
  subroutine mf6_write_ims(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6) :: this
    ! -- local
    character(len=mxslen) :: f
    integer(i4b) :: iu
! ------------------------------------------------------------------------------
    f = trim(this%rootdir)//trim(this%solname)//'.ims'
    call open_file(f, iu, 'w')
    
    write(iu,'(   a)') 'BEGIN OPTIONS'
    write(iu,'(2x,a)') 'PRINT_OPTION '//trim(solverdat(i_print_option))
    write(iu,'(2x,a)') 'COMPLEXITY '//trim(solverdat(i_complexity))
    write(iu,'(   a)') 'END OPTIONS'
    write(iu,'(a)')
    write(iu,'(   a)') 'BEGIN NONLINEAR'
    write(iu,'(2x,a)') 'OUTER_HCLOSE '//trim(solverdat(i_outer_hclose))
    write(iu,'(2x,a)') 'OUTER_MAXIMUM '//trim(solverdat(i_outer_maximum))
    write(iu,'(   a)') 'END NONLINEAR'
    write(iu,'(a)')
    write(iu,'(   a)') 'BEGIN LINEAR'
    write(iu,'(2x,a)') 'INNER_MAXIMUM '//trim(solverdat(i_inner_maximum))
    write(iu,'(2x,a)') 'INNER_HCLOSE '//trim(solverdat(i_inner_hclose))
    write(iu,'(2x,a)') 'INNER_RCLOSE '//trim(solverdat(i_inner_rclose))
    write(iu,'(2x,a)') 'RELAXATION_FACTOR '//trim(solverdat(i_relaxation_factor))
    write(iu,'(   a)') 'END LINEAR'
    close(iu)
  end subroutine mf6_write_ims
  
  subroutine mf6_write_exchanges(this)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6) :: this
    ! -- local
    character(len=mxslen) :: f, fexg, m1s, m2s
    integer(i4b) :: iu, ju, imod, ixch, iexg
    type(tMf6_mod), pointer :: mod => null()
    type(tExchange), pointer :: xch => null()
! ------------------------------------------------------------------------------
    f = trim(this%rootdir)//'exchanges'
    call create_dir(f)
    
    f = trim(this%rootdir)//trim(this%solname)//'.exchanges.asc'
    call open_file(f, iu, 'w')
    do imod = 1, this%nmod
      mod => this%mod(imod)
      do ixch = 1, mod%nxch
        xch => mod%xch(ixch)
        !
        call mod%get_model_name(xch%m1part, m1s)
        call mod%get_model_name(xch%m2part, m2s)
        !
        fexg = trim(m1s)//'-'//trim(m2s)//'.exg'
        f =  'GWF6-GWF6 .\exchanges\'//trim(fexg)//' '//trim(m1s)//' '//trim(m2s)
        call swap_slash(f)
        write(iu,'(a)') trim(f)
        !
        ! .exg file
        f = trim(this%rootdir)//'exchanges\'//trim(fexg)
        call open_file(f, ju, 'w')
        write(ju,'(   a)') 'BEGIN OPTIONS'
        write(ju,'(   a)') 'END OPTIONS'
        write(ju,'(a)')
        write(ju,'(   a)') 'BEGIN DIMENSIONS'
        write(ju,'(2x,a)') 'NEXG '//ta((/xch%nexg/))
        write(ju,'(   a)') 'END DIMENSIONS'
        write(ju,'(a)')
        write(ju,'(   a)') 'BEGIN EXCHANGEDATA'
        f = '.\exchanges\'//trim(fexg)//'.asc'; call swap_slash(f)
        write(ju,'(2x,a)') 'OPEN/CLOSE '//trim(f)
        write(ju,'(   a)') 'END EXCHANGEDATA'
        close(ju)
        !
        ! .asc file
        f = trim(this%rootdir)//'exchanges\'//trim(fexg)//'.asc'
        call open_file(f, ju, 'w')
        do iexg = 1, xch%nexg
          write(ju,'(a)') ta((/xch%cellidm1(iexg), xch%cellidm2(iexg)/))//' 1 '//&
                          ta((/delrc/2, delrc/2, delrc/))
        end do
        close(ju)
      end do
    end do
    close(iu)
    
  end subroutine mf6_write_exchanges

! ==============================================================================
! ==============================================================================
! ==============================================================================
! ==============================================================================
! ==============================================================================

  subroutine mf6_mod_get_array_r8(this, i_dat, ilay, arrflg, arr, nflg, ib_in)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    integer(i4b), intent(in) :: i_dat
    integer(i4b), intent(in) :: ilay
    integer(i1b), dimension(:), pointer, intent(inout) :: arrflg
    real(r8b), dimension(:), pointer, intent(inout) :: arr
    integer(i4b), intent(out) :: nflg
    integer(i4b), intent(in), optional :: ib_in
    ! -- local
    type(tReg), pointer :: reg
    integer(i4b) :: n, i, ireg, ir, ic, jr, jc, ib
    integer(i4b) :: arrsiz
    real(r4b) :: r4val
    logical :: lfirst = .true.
! ------------------------------------------------------------------------------
    if (present(ib_in)) then
      ib = ib_in
    else
      ib = 0
    end if
    
    if (.not.associated(arr)) then
      allocate(arr(nlay*this%layer_nodes))
      do i = 1, size(arr)
        arr(i) = DZERO
      end do
    end if
    if (.not.associated(arrflg)) then
      allocate(arrflg(nlay*this%layer_nodes))
      do i = 1, size(arrflg)
        arrflg(i) = 0
      end do
    end if
    !
    if (size(arrflg) /= size(arr)) then
      call errmsg('mf6_mod_get_array_r8: program error 1')
    end if
    arrsiz = size(arrflg)
    
    do ireg = 1, this%nreg
      reg => this%reg(ireg)
      do ir = reg%ir0, reg%ir1
        do ic = reg%ic0, reg%ic1
          jr = ir - reg%ir0 + 1; jc = ic - reg%ic0 + 1
          select case(ib)
            case(0)
              n = reg%nodmap(jc,jr)
            case(1)
              n = reg%bndmap(jc,jr)
              if (n < 0) then
                n = abs(n)
              else
                n = 0
              end if
            case(2)
              n = reg%bndmap(jc,jr)
              if (n < 0) then
                n = 0
              end if
            case default
              call errmsg('mf6_mod_get_array_r8: program error 1')
          end select
          if (n < 0) then 
            call errmsg('mf6_mod_get_array_r8: program error 2')
          end if
          if (n /= 0) then
            n = n + (ilay-1)*reg%layer_nodes
            r4val = readidf_val(gdat(i_dat)%idf, ic, ir)
            if (r4val == gdat(i_dat)%idf%nodata) then
              if (lfirst) then
                call logmsg('WARNING, using nodata from '//trim(gdat(i_dat)%idf%fname)//'!')
                lfirst = .false.
              end if
            end if
            if ((n < 1) .or. (n > arrsiz)) then
              call errmsg('mf6_mod_get_array_r8: program error 3 '//ta((/ib/))//' '//&
                ta((/n/))//' '//ta((/arrsiz/)))
            end if
            arr(n) = dble(r4val)
            arrflg(n) = 1
          end if
        end do
      end do
    end do
    
    nflg = 0
    do i = 1, arrsiz
      if (arrflg(i) == 1) then
        nflg = nflg + 1
      end if
    end do
    
  end subroutine mf6_mod_get_array_r8
  
  subroutine mf_mod_get_model_name(this, i, modelname)
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
    write(modelname,'(a,i6.6)') 'm', i
    !
  end subroutine mf_mod_get_model_name
  
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
    real(r4b), parameter :: nodata = -9999.0
    real(r8b), parameter :: thkmin = 0.1D0
    !
    integer(i4b), dimension(ns) :: ihc
    !        p t n w e s b
    data ihc/0,0,1,1,1,1,0/
    real(r8b), dimension(ns) :: hwva, cl12
    type(tDisu), pointer :: disu => null()
    type(tReg), pointer :: reg => null()
    integer(i4b) :: ireg, n, ilay, iact, nja
    integer(i4b), dimension(ns) :: s
    integer(i4b) :: ic, ir, i
    real(r4b), dimension(:,:), pointer :: top => null()
    real(r4b), dimension(:,:), pointer :: bot => null()
    real(r8b) :: thk
! ------------------------------------------------------------------------------
    hwva(jp) = DZERO
    hwva(jn) = delrc
    hwva(js) = delrc
    hwva(jw) = delrc
    hwva(je) = delrc
    hwva(jt) = delrc*delrc
    hwva(jb) = delrc*delrc
    !
    allocate(this%disu)
    disu => this%disu
    !
    allocate(disu%nodes)
    disu%nodes = 0
    do ireg = 1, this%nreg
      reg => this%reg(ireg)
      disu%nodes = disu%nodes + nlay*reg%layer_nodes
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
      do ireg = 1, this%nreg
        reg => this%reg(ireg)
        do ilay = 1, nlay
          if (ilay == 1) then
            call readidf_block(gdat(i_bot_l1)%idf, reg%ir0, reg%ir1, reg%ic0, reg%ic1, top, nodata)
            call readidf_block(gdat(i_bot_l2)%idf, reg%ir0, reg%ir1, reg%ic0, reg%ic1, bot, nodata)
          else if (ilay == 2) then
            call readidf_block(gdat(i_top)%idf,    reg%ir0, reg%ir1, reg%ic0, reg%ic1, top, nodata)
            call readidf_block(gdat(i_bot_l1)%idf, reg%ir0, reg%ir1, reg%ic0, reg%ic1, bot, nodata)
          else
            call errmsg('mf6_mod_set_disu: Nlay > 2 not yet supported')
          end if
          !
          do ir = 1, reg%nrow
            do ic = 1, reg%ncol
              n = reg%nodmap(ic,ir)
              if (n /= 0) then
                n = n + (ilay-1)*reg%layer_nodes
                s(jp) = n !CENTER
                if (ir > 1) then !NORTH
                  s(jn) = reg%nodmap(ic,ir-1)
                  if (s(jn) /= 0) then
                    s(jn) = s(jn) + (ilay-1)*reg%layer_nodes
                    cl12(jn) = delrc/2
                  end if
                end if
                if (ir < reg%nrow) then !SOUTH
                  s(js) = reg%nodmap(ic,ir+1)
                  if (s(js) /= 0) then
                    s(js) = s(js) + (ilay-1)*reg%layer_nodes
                    cl12(js) = delrc/2
                  end if
                end if
                if (ic > 1) then !WEST
                  s(jw) = reg%nodmap(ic-1,ir)
                  if (s(jw) /= 0) then
                    s(jw) = s(jw) + (ilay-1)*reg%layer_nodes
                    cl12(jw) = delrc/2
                  end if
                end if
                if (ic < reg%ncol) then !EAST
                  s(je) = reg%nodmap(ic+1,ir)
                  if (s(je) /= 0) then
                    s(je) = s(je) + (ilay-1)*reg%layer_nodes
                    cl12(je) = delrc/2
                  end if
                end if
                if (ilay > 1) then !TOP
                  s(jt) = n - reg%layer_nodes
                  if ((top(ic,ir) == nodata).or.(bot(ic,ir) == nodata)) then
                    call errmsg('mf6_mod_set_disu: program error (top/bot).')
                  end if
                  cl12(jt) = max(thkmin, top(ic,ir)-bot(ic,ir))
                  cl12(jt) = cl12(jt)/2
                end if
                if (ilay < nlay) then !BOT
                  s(jb) = n + reg%layer_nodes
                  if ((top(ic,ir) == nodata).or.(bot(ic,ir) == nodata)) then
                    call errmsg('mf6_mod_set_disu: program error (top/bot).')
                  end if
                  cl12(jb) = max(thkmin, top(ic,ir)-bot(ic,ir))
                  cl12(jb) = cl12(jb)/2
                end if
                do i = 1, ns
                  if (s(i) /= 0) then
                    if (s(i) < 0) then
                      call errmsg('mf6_mod_set_disu: program error')
                    end if
                    if ((n < 1).or.(n > disu%nodes)) then
                     call errmsg('mf6_mod_set_disu: program error')
                    end if
                    disu%iac(n) = disu%iac(n) + 1
                    disu%nja = disu%nja + 1
                    if (iact == 2) then
                      if ((disu%nja < 1).or.(disu%nja > nja)) then
                       call errmsg('mf6_mod_set_disu: program error')
                      end if
                      disu%ja(disu%nja)   = s(i)
                      disu%ihc(disu%nja)  = ihc(i)
                      disu%cl12(disu%nja) = cl12(i)
                      disu%hwva(disu%nja) = hwva(i)
                    end if
                  end if
                  s(i) = 0
                  cl12(i) = DZERO
                end do
                !
              end if
            end do
          end do
          deallocate(top, bot); top => null(); bot => null()
        end do !ilay
      end do !rreg
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
    end do !iact
    
  end subroutine mf6_mod_set_disu
  
  subroutine mf6_mod_write_array_i4(this, iu, nx, f, arr, lbin)
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
    ! -- local
    integer(i4b), parameter :: i4dum = 1
    real(r8b), parameter :: r8dum = DZERO
    character(len=16) :: c16dum = ''
    integer(i4b) :: ju, i, narr
    character(len=mxslen) :: fmt
! ------------------------------------------------------------------------------
    write(fmt,'(a,i,a)') '(',nx,'x,a)'
    if (lbin) then
      f = trim(f)//'.bin'
      call open_file(f, ju, 'w', .true.)
      narr = size(arr)
      write(ju) i4dum, i4dum, r8dum, r8dum, c16dum, narr, i4dum, i4dum
      write(ju) arr
      close(ju)
      call get_rel_up(f, 2); write(iu,fmt) 'OPEN/CLOSE '//trim(f)//' (BINARY)'
    else
      f = trim(f)//'.asc'
      call open_file(f, ju, 'w')
      do i = 1, size(arr)
        write(ju,'(a)') ta((/arr(i)/))
      end do
      close(ju)
      call get_rel_up(f, 2); write(iu,fmt) 'OPEN/CLOSE '//trim(f)
    end if
  end subroutine mf6_mod_write_array_i4
  
  subroutine mf6_mod_write_array_r8(this, iu, nx, f, arr, lbin)
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
    ! -- local
    integer(i4b), parameter :: i4dum = 1
    real(r8b), parameter :: r8dum = DZERO
    character(len=16) :: c16dum = ''
    integer(i4b) :: ju, i, narr
    character(len=mxslen) :: fmt
! ------------------------------------------------------------------------------
    write(fmt,'(a,i,a)') '(',nx,'x,a)'
    if (lbin) then
      f = trim(f)//'.bin'
      call open_file(f, ju, 'w', .true.)
      narr = size(arr)
      write(ju) i4dum, i4dum, r8dum, r8dum, c16dum, narr, i4dum, i4dum
      write(ju) arr
      close(ju)
      call get_rel_up(f, 2); write(iu,fmt) 'OPEN/CLOSE '//trim(f)//' (BINARY)'
    else
      f = trim(f)//'.asc'
      call open_file(f, ju, 'w')
      do i = 1, size(arr)
        write(ju,'(a)') ta((/arr(i)/))
      end do
      close(ju)
      call get_rel_up(f, 2); write(iu,fmt) 'OPEN/CLOSE '//trim(f)
    end if
  end subroutine mf6_mod_write_array_r8
  
  subroutine mf6_mod_write_list_1(this, iu, nx, f, arrflg, arr, lbin)
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
    ! -- local
    integer(i4b) :: ju, i
    character(len=mxslen) :: fmt
! ------------------------------------------------------------------------------
    write(fmt,'(a,i,a)') '(',nx,'x,a)'
    if (lbin) then
      f = trim(f)//'.bin'
      call open_file(f, ju, 'w', .true.)
      do i = 1, size(arrflg)
        if (arrflg(i) == 1) then
          write(ju) i, arr(i)
        end if
      end do
      close(ju)
      call get_rel_up(f, 2); write(iu,fmt) 'OPEN/CLOSE '//trim(f)// ' (BINARY)'
    else
      f = trim(f)//'.asc'
      call open_file(f, ju, 'w')
      do i = 1, size(arrflg)
        if (arrflg(i) == 1) then
          write(ju,'(a)') ta((/i/))//' '//ta((/arr(i)/))
        end if
      end do
      close(ju)
      call get_rel_up(f, 2); write(iu,fmt) 'OPEN/CLOSE '//trim(f)
    end if
  end subroutine mf6_mod_write_list_1
  
  subroutine mf6_mod_write_list_2(this, iu, nx, f, arrflg, arr, arr2, lbin)
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
    ! -- local
    integer(i4b) :: ju, i
    character(len=mxslen) :: fmt
! ------------------------------------------------------------------------------
    write(fmt,'(a,i,a)') '(',nx,'x,a)'
    if (lbin) then
      f = trim(f)//'.bin'
      call open_file(f, ju, 'w', .true.)
      do i = 1, size(arrflg)
        if (arrflg(i) == 1) then
          write(ju) i, arr(i), arr2(i)
        end if
      end do
      close(ju)
      call get_rel_up(f, 2); write(iu,fmt) 'OPEN/CLOSE '//trim(f)// ' (BINARY)'
    else
      f = trim(f)//'.asc'
      call open_file(f, ju, 'w')
      do i = 1, size(arrflg)
        if (arrflg(i) == 1) then
          write(ju,'(a)') ta((/i/))//' '//ta((/arr(i)/))//' '//ta((/arr2(i)/))
        end if
      end do
      close(ju)
      call get_rel_up(f, 2); write(iu,fmt) 'OPEN/CLOSE '//trim(f)
    end if
  end subroutine mf6_mod_write_list_2
  
  subroutine mf6_mod_write_list_3(this, iu, nx, f, arrflg, arr, arr2, arr3, lbin)
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
    ! -- local
    integer(i4b) :: ju, i
    character(len=mxslen) :: fmt
! ------------------------------------------------------------------------------
    write(fmt,'(a,i,a)') '(',nx,'x,a)'
    if (lbin) then
      f = trim(f)//'.bin'
      call open_file(f, ju, 'w', .true.)
      do i = 1, size(arrflg)
        if (arrflg(i) == 1) then
          write(ju) i, arr(i), arr2(i), arr3(i)
        end if
      end do
      close(ju)
      call get_rel_up(f, 2); write(iu,fmt) 'OPEN/CLOSE '//trim(f)// ' (BINARY)'
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
      call get_rel_up(f, 2); write(iu,fmt) 'OPEN/CLOSE '//trim(f)
    end if
  end subroutine mf6_mod_write_list_3
  
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
    character(len=mxslen) :: p, f, mn
    integer(i4b) :: iu, i
    integer(i4b), parameter :: npck = 8
    character(len=4), dimension(npck) :: pck
    data pck/'disu', 'ic  ', 'oc  ', 'npf ', 'chd ', &
             'drn ', 'riv ', 'rch '/
! ------------------------------------------------------------------------------
    mn = this%modelname
    p = '.\'//trim(mn)//'\'
    f = trim(this%rootdir)//trim(mn)//'.int.ext.nam'
    call open_file(f, iu, 'w')
    write(iu,'(   a)') 'BEGIN OPTIONS'
    write(iu,'(   a)') 'END OPTIONS'
    write(iu,'(a)')
    write(iu,'(   a)') 'BEGIN PACKAGES'
    do i = 1, npck
      if (pck(i) == 'chd') then
        if (this%lchd_int) then
          f = trim(p)//trim(mn)//'.int.'//trim(pck(i)); call swap_slash(f)
          write(iu,'(2x,a)') trim(change_case(pck(i),'u'))//'6 '//trim(f)
        end if
        if (this%lchd_ext) then
          f = trim(p)//trim(mn)//'.ext.'//trim(pck(i)); call swap_slash(f)
          write(iu,'(2x,a)') trim(change_case(pck(i),'u'))//'6 '//trim(f)
        end if
      else if ((pck(i) == 'ic').or.(pck(i) == 'oc')) then
        f = trim(p)//trim(mn)//'.int.ext.'//trim(pck(i)); call swap_slash(f)
        write(iu,'(2x,a)') trim(change_case(pck(i),'u'))//'6 '//trim(f)
      else  
        f = trim(p)//trim(mn)//'.'//trim(pck(i)); call swap_slash(f)
        write(iu,'(2x,a)') trim(change_case(pck(i),'u'))//'6 '//trim(f)
      end if  
    end do
    write(iu,'(   a)') 'END PACKAGES'
    close(iu)
    !
    f = trim(this%rootdir)//trim(mn)//'.ext.nam'
    call open_file(f, iu, 'w')
    write(iu,'(   a)') 'BEGIN OPTIONS'
    write(iu,'(   a)') 'END OPTIONS'
    write(iu,'(a)')
    write(iu,'(   a)') 'BEGIN PACKAGES'
    do i = 1, npck
      if (pck(i) == 'chd') then
        if (this%lchd_ext) then
          f = trim(p)//trim(mn)//'.ext.'//trim(pck(i)); call swap_slash(f)
          write(iu,'(2x,a)') trim(change_case(pck(i),'u'))//'6 '//trim(f)
        end if
      else if ((pck(i) == 'ic').or.(pck(i) == 'oc')) then
        f = trim(p)//trim(mn)//'.ext.'//trim(pck(i)); call swap_slash(f)
        write(iu,'(2x,a)') trim(change_case(pck(i),'u'))//'6 '//trim(f)
      else
        f = trim(p)//trim(mn)//'.'//trim(pck(i)); call swap_slash(f)
        write(iu,'(2x,a)') trim(change_case(pck(i),'u'))//'6 '//trim(f)
      end if  
    end do
    write(iu,'(   a)') 'END PACKAGES'
    close(iu)
    !
  end subroutine mf6_mod_write_nam
  
  subroutine mf6_mod_write_disu(this, lbin)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    logical, intent(in) :: lbin
    ! -- local
    character(len=mxslen) :: p, f
    integer(i4b) :: iu, n
    type(tDisu), pointer :: disu
! ------------------------------------------------------------------------------
    call clear_wrk()
    !
    ! set disu
    call this%set_disu()
    disu => this%disu
    !
    p = trim(this%rootdir)//trim(this%modelname)
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
    write(iu,'(2x,a)') 'TOP'
    call this%get_array(i_top,    1, i1wrk, r8wrk, n)
    call this%get_array(i_bot_l1, 2, i1wrk, r8wrk, n)
    f = trim(p)//'.disu.top'; call this%write_array(iu, 4, f, r8wrk, lbin)
    call clear_wrk()
    write(iu,'(2x,a)') 'BOT'
    call this%get_array(i_bot_l1, 1, i1wrk, r8wrk, n)
    call this%get_array(i_bot_l2, 2, i1wrk, r8wrk, n)
    f = trim(p)//'.disu.bot'; call this%write_array(iu, 4, f, r8wrk, lbin)
    call clear_wrk()
    write(iu,'(2x,a)') 'AREA'
    write(iu,'(4x,a)') 'CONSTANT '//ta((/delrc*delrc/))
    write(iu,'(   a)') 'END GRIDDATA'
    write(iu,'(a)')
    write(iu,'(   a)') 'BEGIN CONNECTIONDATA'
    write(iu,'(2x,a)') 'IAC'
    f = trim(p)//'.disu.iac'; call this%write_array(iu, 4, f, disu%iac, lbin)
    write(iu,'(2x,a)') 'JA'
    f = trim(p)//'.disu.ja'; call this%write_array(iu, 4, f, disu%ja, lbin)
    write(iu,'(2x,a)') 'IHC'
    f = trim(p)//'.disu.ihc'; call this%write_array(iu, 4, f, disu%ihc, lbin)
    write(iu,'(2x,a)') 'CL12'
    f = trim(p)//'.disu.cl12'; call this%write_array(iu, 4, f, disu%cl12, lbin)
    write(iu,'(2x,a)') 'HWVA'
    f = trim(p)//'.disu.hwva'; call this%write_array(iu, 4, f, disu%hwva, lbin)
    write(iu,'(   a)') 'END CONNECTIONDATA'
    close(iu)
    !
    ! clean up memory
    deallocate(disu%iac);  disu%iac  => null()
    deallocate(disu%ja);   disu%ja   => null()
    deallocate(disu%ihc);  disu%ihc  => null()
    deallocate(disu%cl12); disu%cl12 => null()
    deallocate(disu%hwva); disu%hwva => null()
    
  end subroutine mf6_mod_write_disu

  subroutine mf6_mod_write_ic(this, lbin)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    logical, intent(in) :: lbin
    ! -- local
    character(len=mxslen) :: p, f
    integer(i4b) :: iu, n
! ------------------------------------------------------------------------------
    call clear_wrk()
    !
    p = trim(this%rootdir)//trim(this%modelname)
    !
    f = trim(p)//'.int.ext.ic'
    call open_file(f, iu, 'w')
    write(iu,'(   a)') 'BEGIN OPTIONS'
    write(iu,'(   a)') 'END OPTIONS'
    write(iu,'(a)')
    write(iu,'(   a)') 'BEGIN GRIDDATA'
    write(iu,'(2x,a)') 'STRT'
    call this%get_array(i_strt_l1, 1, i1wrk, r8wrk, n)
    call this%get_array(i_strt_l2, 2, i1wrk, r8wrk, n)
    f = trim(p)//'.ext.ic'; call this%write_array(iu, 4, f, r8wrk, lbin)
    write(iu,'(   a)') 'END GRIDDATA'
    close(iu)

    f = trim(p)//'.ext.ic'
    call open_file(f, iu, 'w')
    write(iu,'(   a)') 'BEGIN OPTIONS'
    write(iu,'(   a)') 'END OPTIONS'
    write(iu,'(a)')
    write(iu,'(   a)') 'BEGIN GRIDDATA'
    write(iu,'(2x,a)') 'STRT'
    f = '.\'//trim(resultsdir)//'\'//trim(this%modelname)//'.int.ext.hds'
    call swap_slash(f)
    write(iu,'(2x,a)') 'OPEN/CLOSE '//trim(f)//' (BINARY)'
    write(iu,'(   a)') 'END GRIDDATA'
    close(iu)
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
    integer(i4b) :: iu
! ------------------------------------------------------------------------------
    !
    p = trim(this%rootdir)//trim(this%modelname)
    !
    f = trim(p)//'.int.ext.oc'
    call open_file(f, iu, 'w')
    
    write(iu,'(   a)') 'BEGIN OPTIONS'
    f = '.\'//trim(resultsdir)//'\'//trim(this%modelname)//'.int.ext.hds'
    call swap_slash(f)
    write(iu,'(2x,a)') 'HEAD FILEOUT '//trim(f)
    write(iu,'(   a)') 'END OPTIONS'
    write(iu,'(a)')
    write(iu,'(   a)') 'BEGIN PERIOD 1'
    write(iu,'(2x,a)') 'SAVE HEAD ALL'
    write(iu,'(   a)') 'END PERIOD'
    close(iu)
    
    f = trim(p)//'.ext.oc'
    call open_file(f, iu, 'w')
    write(iu,'(   a)') 'BEGIN OPTIONS'
    f = '.\'//trim(resultsdir)//'\'//trim(this%modelname)//'.ext.hds'
    call swap_slash(f)
    write(iu,'(2x,a)') 'HEAD FILEOUT '//trim(f)
    write(iu,'(   a)') 'END OPTIONS'
    write(iu,'(a)')
    write(iu,'(   a)') 'BEGIN PERIOD 1'
    write(iu,'(2x,a)') 'SAVE HEAD ALL'
    write(iu,'(   a)') 'END PERIOD'
    close(iu)
  end subroutine mf6_mod_write_oc
  
  subroutine mf6_mod_write_npf(this, lbin)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    logical, intent(in) :: lbin
    ! -- local
    character(len=mxslen) :: p, f
    integer(i4b) :: iu, n
! ------------------------------------------------------------------------------
    call clear_wrk()
    !
    p = trim(this%rootdir)//trim(this%modelname)
    f = trim(p)//'.npf'
    call open_file(f, iu, 'w')
    !
    write(iu,'(   a)') 'BEGIN OPTIONS'
    write(iu,'(   a)') 'END OPTIONS'
    write(iu,'(a)')
    write(iu,'(   a)') 'BEGIN GRIDDATA'
    write(iu,'(2x,a)') 'ICELLTYPE'
    write(iu,'(4x,a)') 'CONSTANT 0'
    write(iu,'(2x,a)') 'K'
    call this%get_array(i_k_l1, 1, i1wrk, r8wrk, n)
    call this%get_array(i_k_l2, 2, i1wrk, r8wrk, n)
    f = trim(p)//'.npf.k'; call this%write_array(iu, 4, f, r8wrk, lbin)
    call clear_wrk()
    write(iu,'(2x,a)') 'K33'
    call this%get_array(i_k33_l1, 1, i1wrk, r8wrk, n)
    call this%get_array(i_k33_l2, 2, i1wrk, r8wrk, n)
    f = trim(p)//'.npf.k33'; call this%write_array(iu, 4, f, r8wrk, lbin)
    call clear_wrk()
    write(iu,'(   a)') 'END GRIDDATA'
    close(iu)
    !
  end subroutine mf6_mod_write_npf
  
  subroutine mf6_mod_write_chd(this, lbin)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    logical, intent(in) :: lbin
    ! -- local
    character(len=mxslen) :: p, f
    integer(i4b) :: iu, maxbound, i
! ------------------------------------------------------------------------------
    allocate(this%lchd_ext, this%lchd_int)
    this%lchd_ext = .false.; this%lchd_int = .false.
    
    call clear_wrk()
    !
    p = trim(this%rootdir)//trim(this%modelname)
    !
    ! external boundaries (sea)
    call this%get_array(i_strt_l1, 1, i1wrk, r8wrk, maxbound, 1)
    call this%get_array(i_strt_l2, 2, i1wrk, r8wrk, maxbound, 1)
    if (maxbound > 0) then
      this%lchd_ext = .true.
      f = trim(p)//'.ext.chd'
      call open_file(f, iu, 'w')
      !
      write(iu,'(   a)') 'BEGIN OPTIONS'
      write(iu,'(   a)') 'END OPTIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN DIMENSIONS'
      write(iu,'(2x,a)') 'MAXBOUND '//ta((/maxbound/))
      write(iu,'(   a)') 'END DIMENSIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN PERIOD 1'
      ! set the external boundary to zero (sea-level) ***WORKAROUND***
      do i = 1, size(r8wrk)
        r8wrk(i) = DZERO
      end do
      f = trim(p)//'.ext.chd'; call this%write_list(iu, 2, f, i1wrk, r8wrk, lbin)
      write(iu,'(   a)') 'END PERIOD'
      close(iu)
    end if
    call clear_wrk()
    
    ! internal boundaries (partitions)
    call this%get_array(i_strt_l1, 1, i1wrk, r8wrk, maxbound, 2)
    call this%get_array(i_strt_l2, 2, i1wrk, r8wrk, maxbound, 2)
    if (maxbound > 0) then
      this%lchd_int = .true.
      f = trim(p)//'.int.chd'
      call open_file(f, iu, 'w')
      !
      write(iu,'(   a)') 'BEGIN OPTIONS'
      write(iu,'(   a)') 'END OPTIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN DIMENSIONS'
      write(iu,'(2x,a)') 'MAXBOUND '//ta((/maxbound/))
      write(iu,'(   a)') 'END DIMENSIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN PERIOD 1'
      f = trim(p)//'.int.chd'; call this%write_list(iu, 4, f, i1wrk, r8wrk, lbin)
      write(iu,'(   a)') 'END PERIOD'
      close(iu)
    end if
    call clear_wrk()
    
  end subroutine mf6_mod_write_chd
  
  subroutine mf6_mod_write_drn(this, lbin)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    logical, intent(in) :: lbin
    ! -- local
    character(len=mxslen) :: p, f
    integer(i4b) :: iu, maxbound
! ------------------------------------------------------------------------------
    call clear_wrk()
    !
    p = trim(this%rootdir)//trim(this%modelname)
    !
    call this%get_array(i_drn_elev_l1, 1, i1wrk, r8wrk,  maxbound)
    call this%get_array(i_drn_elev_l1, 2, i1wrk, r8wrk,  maxbound)
    call this%get_array(i_drn_cond,    1, i1wrk, r8wrk2, maxbound)
    call this%get_array(i_drn_cond,    2, i1wrk, r8wrk2, maxbound)
    
    if (maxbound > 0) then 
      f = trim(p)//'.drn'
      call open_file(f, iu, 'w')
      write(iu,'(   a)') 'BEGIN OPTIONS'
      write(iu,'(   a)') 'END OPTIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN DIMENSIONS'
      write(iu,'(2x,a)') 'MAXBOUND '//ta((/maxbound/))
      write(iu,'(   a)') 'END DIMENSIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN PERIOD 1'
      f = trim(p)//'.drn'; call this%write_list(iu, 4, f, &
        i1wrk, r8wrk, r8wrk2, lbin)
      call clear_wrk()
      write(iu,'(   a)') 'END PERIOD'
      close(iu)
    else
      call errmsg('No drains found')
    end if
    
  end subroutine mf6_mod_write_drn
  
  subroutine mf6_mod_write_riv(this, lbin)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    logical, intent(in) :: lbin
    ! -- local
    character(len=mxslen) :: p, f
    integer(i4b) :: iu, maxbound, i
    real(r8b) :: stage, rbot, cond
! ------------------------------------------------------------------------------
    call clear_wrk()
    !
    p = trim(this%rootdir)//trim(this%modelname)
    !
    call this%get_array(i_riv_stage_l1, 1, i1wrk, r8wrk,  maxbound)
    call this%get_array(i_riv_cond,     1, i1wrk, r8wrk2, maxbound)
    call this%get_array(i_riv_rbot_l1,  1, i1wrk, r8wrk3, maxbound)
    !
    !checks
    do i = 1, size(i1wrk)
      if (i1wrk(i) == 1) then
        stage = r8wrk(i); cond = r8wrk2(i); rbot = r8wrk3(i)
        if (stage < rbot) then
          call errmsg('Inconsistent river stage/rbot.')
        end if
        if (cond < 0) then
          call errmsg('Negative river conductance.')
        end if
      end if
    end do
    
    if (maxbound > 0) then 
      f = trim(p)//'.riv'
      call open_file(f, iu, 'w')
      write(iu,'(   a)') 'BEGIN OPTIONS'
      write(iu,'(   a)') 'END OPTIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN DIMENSIONS'
      write(iu,'(2x,a)') 'MAXBOUND '//ta((/maxbound/))
      write(iu,'(   a)') 'END DIMENSIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN PERIOD 1'
      f = trim(p)//'.riv'; call this%write_list(iu, 2, f, i1wrk, &
        r8wrk, r8wrk2, r8wrk3, lbin)
      call clear_wrk()
      write(iu,'(   a)') 'END PERIOD'
      close(iu)
    else
      call errmsg('No rivers found')
    end if
    !
    call clear_wrk()
  end subroutine mf6_mod_write_riv
  
  subroutine mf6_mod_write_rch(this, lbin)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6_mod) :: this
    logical, intent(in) :: lbin
    ! -- local
    character(len=mxslen) :: p, f
    integer(i4b) :: iu, maxbound
! ------------------------------------------------------------------------------
    call clear_wrk()
    !
    p = trim(this%rootdir)//trim(this%modelname)
    !
    call this%get_array(i_recharge, 1, i1wrk, r8wrk, maxbound)
    !
    if (maxbound > 0) then 
      f = trim(p)//'.rch'
      call open_file(f, iu, 'w')
      write(iu,'(   a)') 'BEGIN OPTIONS'
      write(iu,'(   a)') 'END OPTIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN DIMENSIONS'
      write(iu,'(2x,a)') 'MAXBOUND '//ta((/maxbound/))
      write(iu,'(   a)') 'END DIMENSIONS'
      write(iu,'(a)')
      write(iu,'(   a)') 'BEGIN PERIOD 1'
      f = trim(p)//'.rch'; call this%write_list(iu, 2, f, i1wrk, r8wrk, lbin)
      call clear_wrk()
      write(iu,'(   a)') 'END PERIOD'
      close(iu)
    else
      call errmsg('No recharge found')
    end if
    
  end subroutine mf6_mod_write_rch
  
end module