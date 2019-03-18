! ==============================================================================
module mf6_module
   ! -- modules
  use, intrinsic :: iso_fortran_env , only: error_unit, output_unit, &
     i1b => int8, i4b => int32, i8b => int64, r4b => real32, r8b => real64
  use utilsmod, only: getlun, chkexist, readline, change_case, errmsg, logmsg, &
    readidf_block, writeidf, checkdim, addboundary, tUnp, calc_unique
  use imod_idf
  
  implicit none 
  
  ! parameters
  integer(i4b), parameter     :: mxslen = 1024
  character(len=1), parameter :: slash = '\' !DOS/LINUX slash
  !
  integer(i4b), parameter :: nlay = 2
  real(r8b), parameter    :: delrc = 0.008333333333333D0
  real(r8b), parameter    :: DZERO = 0.D0
  real(r8b), parameter    :: DONE  = 1.D0
  !
  ! stencil
  integer(i4b), parameter :: jp = 1
  integer(i4b), parameter :: jn = 2
  integer(i4b), parameter :: js = 3
  integer(i4b), parameter :: jw = 4
  integer(i4b), parameter :: je = 5
  integer(i4b), parameter :: jt = 6
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
  integer(i4b), parameter :: i_riv_rbot_l1  = 14
  integer(i4b), parameter :: i_riv_cond     = 15
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
                  'riv_stage_l1', 'riv_rbot_l1 ', 'riv_cond    ', &
                  'recharge    ', 'partitions  ', 'solutions   ' /
  type tGdat
    character(len=mxslen), pointer :: f =>  null()
    type(idfobj)         , pointer :: idf => null() 
  end type tGdat
  type(tGdat), dimension(:), pointer :: gdat => null()
  integer(i4b) :: nparts     = 0
  integer(i4b) :: gncol      = 0
  integer(i4b) :: gnrow      = 0
  integer(i4b) :: nsol       = 0
  integer(i4b) :: nsol_indep = 0
  integer(i4b) :: nsol_dep   = 0
    
  real(r4b), dimension(:,:), pointer :: r4a => null()
  
  integer(i4b), dimension(:), pointer   :: iwrk1d => null()
  integer(i4b), dimension(:,:), pointer :: iwrk2d => null()
  
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
    character(len=mxslen),    pointer :: root => null()
   ! integer(i4b),             pointer :: ir0  => null()
   ! integer(i4b),             pointer :: ir1  => null()
   ! integer(i4b),             pointer :: ic0  => null()
   ! integer(i4b),             pointer :: ic1  => null()
    integer(i4b),                  pointer :: nreg => null()
    type(tReg), dimension(:),      pointer :: reg  => null()
    type(tDisu),                   pointer :: disu => null()
    integer(i4b),                  pointer :: nxch => null()
    type(tExchange), dimension(:), pointer :: xch  => null()
  contains
    procedure :: set_disu   => mf6_mod_set_disu
    procedure :: write_nam  => mf6_mod_write_nam
    procedure :: write_disu => mf6_mod_write_disu
    procedure :: write_npf  => mf6_mod_write_npf
    procedure :: write_chd  => mf6_mod_write_chd
    procedure :: write_drn  => mf6_mod_write_drn
    procedure :: write_riv  => mf6_mod_write_riv
    procedure :: write_oc   => mf6_mod_write_oc
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
    character(len=mxslen),        pointer :: root => null()
    integer(i4b),                 pointer :: nmod => null() !number of partitions
    type(tMf6_mod), dimension(:), pointer :: mod  => null()
  contains
    procedure :: init            => mf6_init
    procedure :: exchange_init   => mf6_exchange_init
    !
    procedure :: write_mfsim     => mf6_write_mfsim
    procedure :: write_tdis      => mf6_write_tdis
    procedure :: write_ims       => mf6_write_ims
    procedure :: write_exchanges => mf6_write_exchanges
  end type tMf6
  
  public :: tMf6
  
  save
  
  contains

! ==============================================================================
  
  subroutine mf6_init(this, f_in, isol)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
!
    ! -- dummy
    class(tMf6) :: this
    character(len=*), intent(in) :: f_in
    integer(i4b), intent(in) :: isol
    ! -- local
    character(len=mxslen) :: s, key, f
    integer(i4b) :: iu, ju, n, i, j, ifound, idum, jsol, ip, imod, ireg
    integer(i4b) :: ir0, ir1, ic0, ic1, ir, ic, jr, jc, kr, kc, maxid, nr, nc, nreg
    integer(i4b), dimension(:,:), allocatable :: regun
    real(r4b) :: xll, yll, cs, nodata !DEBUG
    type(tUnp), dimension(:), allocatable :: regbb
    type(tMf6_mod), pointer :: mod => null()
    type(tReg), pointer     :: reg => null()
! ------------------------------------------------------------------------------
    allocate(this%isol)
    this%isol = isol
    !
    call chkexist(f_in)
    call logmsg('Reading '//trim(f_in)//'...')
    !
    iu = getlun(); open(unit=iu, file=f_in, action='read')
    call readline(iu, f)
    !
    ! read the partition file
    call chkexist(f)
    ju = getlun(); open(unit=ju, file=f, action='read')
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
      if (isol == jsol) then
        read(s,*) jsol, idum, this%ir0, this%ir1, this%ic0, this%ic1
      end if
    end do
    do i = 1, nsol_dep
      call readline(ju, s); read(s,*) jsol
      if (isol == jsol) then
        read(s,*) jsol, idum, idum, this%ir0, this%ir1, this%ic0, this%ic1
      end if
    end do
    close(ju)
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
      allocate(gdat(ifound)%f); gdat(ifound)%f = f
      allocate(gdat(ifound)%idf) ! read header
      if (.not.idfread(gdat(ifound)%idf, f, 0)) then
        call errmsg('Could not read '//trim(f))
      end if 
    end do
      
    if (.false.) then !DEBUG
      ir0 = 4233; ir1 = 5000; ic0 = 21899; ic1 = 22635
      nodata = gdat(i_strt_l1)%idf%nodata
      call readidf_block(gdat(i_strt_l1)%idf, ir0, ir1, ic0, ic1, nodata, r4a)
      xll = gdat(i_strt_l1)%idf%xmin; yll = gdat(i_strt_l1)%idf%ymin
      cs = gdat(i_strt_l1)%idf%dx
      call writeidf('test.idf', r4a, ic1-ic0+1, ir1-ir0+1, &
        xll+(ic0-1)*cs, yll+(gdat(i_strt_l1)%idf%nrow-ir1)*cs, cs, nodata); stop
    end if
    
    close(iu)
    !
    ! make the bounding box a little larger to include the boundary
    this%ir0 = max(this%ir0-1,1); this%ir1 = min(this%ir1+1,gnrow)
    this%ic0 = max(this%ic0-1,1); this%ic1 = min(this%ic1+1,gncol)
    this%nrow = this%ir1 - this%ir0 + 1; this%ncol = this%ic1 - this%ic0 + 1
    !
    ! read the partition and solution
    call readidf_block(gdat(i_part)%idf, this%ir0, this%ir1, this%ic0, this%ic1, &
      0, this%part)
    call readidf_block(gdat(i_sol)%idf, this%ir0, this%ir1, this%ic0, this%ic1, &
      0, iwrk2d)
    call checkdim(size(this%part,1), size(this%part,2), &
      size(iwrk2d,1), size(iwrk2d,2))
    !
    do ir = 1, size(this%part,2)
      do ic = 1, size(this%part,1)
        if (iwrk2d(ic,ir) /= isol) then
          this%part(ic,ir) = 0
        end if
      end do 
    end do
    deallocate(iwrk2d)
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
      ir0 = this%partbb(1,imod); ir1 = this%partbb(2,imod)
      ic0 = this%partbb(3,imod); ic1 = this%partbb(4,imod)
      nr = ir1 - ir0 + 1; nc = ic1 - ic0 + 1
      allocate(iwrk2d(nc,nr))
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
      call calc_unique(iwrk2d, regun, regbb, nreg, idum, 0., 0., 0.)
      deallocate(iwrk2d)
      !
      if (.false. .and. (nreg >1)) then !DEBUG
        xll = gdat(i_part)%idf%xmin; yll = gdat(i_part)%idf%ymin; cs = gdat(i_part)%idf%dx
        call writeidf('regun.idf', regun, size(regun,1), size(regun,2), &
          xll+(ic0-1)*cs, yll+(gdat(i_part)%idf%nrow-ir1)*cs, cs, 0.); stop
      end if
      
      allocate(mod%nreg)
      mod%nreg = nreg
      allocate(mod%reg(nreg))
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
        reg%nrow = reg%ir1 - reg%ir0 + 1
        reg%ncol = reg%ic1 - reg%ic0 + 1
        !
        allocate(reg%nodmap(reg%ncol,reg%nrow))
        allocate(reg%bndmap(reg%ncol,reg%nrow))
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
            if (i < 0) then
              reg%bndmap(jc,jr) = -n
            else
              reg%bndmap(jc,jr) = 0
            end if
          end do
        end do
        n = n + (nlay-1)*reg%layer_nodes
      end do
      !
      ! determine DISU data structures
    end do !model loop
    
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
    real(r4b) :: xll, yll, cs, nodata !DEBUG
    integer(i4b) :: ic, ir, jc, jr, kc, kr, i, j, imod, jmod, ireg, ixch, nexgf
    integer(i4b) :: iact
    type(tMf6_mod), pointer  :: mod => null()
    type(tReg), pointer      :: reg => null()
    type(tExchange), pointer :: xch => null()
! ------------------------------------------------------------------------------
    call logmsg('Initializing exchanges')
    !
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
              if (abs(this%part(jc,jr)) ==  this%partmapinv(imod)) then
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
                  if (i == jp) cycle
                  if (s(i) /= 0) then
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
    deallocate(iwrk2d)
    allocate(iwrk2d(this%ncol,this%nrow))
    do ir = 1, this%nrow
      do ic = 1, this%nrow
        iwrk2d(ic,ir) = 0
      end do
    end do
    do imod = 1, this%nmod
      mod => this%mod(imod)
      do ixch = 1, mod%nxch
        xch => mod%xch(ixch)
        do i = 1, xch%nexg
          ic = xch%gicirm2(1,i)
          ir = xch%gicirm2(2,i)
          iwrk2d(ic,ir) = xch%m2part
        end do
      end do
    end do
    do imod = 1, this%nmod
      mod => this%mod(imod)
      do ireg = 1, mod%nreg
        reg => mod%reg(ireg)
        do ir = reg%ir0, reg%ir1
          do ic = reg%ic0, reg%ic1
            kr = ir - this%ir0 + 1; kc = ic - this%ic0 + 1
            jr = ir - reg%ir0  + 1; jc = ic - reg%ic0  + 1
            if ((iwrk2d(kc,kr) /= 0) .and. (reg%nodmap(jc,jr) /= 0)) then
              iwrk2d(kc,kr) = ireg
            end if
          end do
        end do
      end do
    end do
    
    if (.true.) then !DEBUG
      xll = gdat(i_part)%idf%xmin; yll = gdat(i_part)%idf%ymin; cs = gdat(i_part)%idf%dx
      call writeidf('xch.idf', iwrk2d, this%ic1-this%ic0+1, this%ir1-this%ir0+1, &
        xll+(this%ic0-1)*cs, yll+(gdat(i_part)%idf%nrow-this%ir1)*cs, cs, 0.); stop
    end if
    
    write(*,*) 'Number of exchanges:',nexgf
    
  end subroutine mf6_exchange_init
  
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
! ------------------------------------------------------------------------------
  
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
! ------------------------------------------------------------------------------
  
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
! ------------------------------------------------------------------------------
  
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
! ------------------------------------------------------------------------------
  
  end subroutine mf6_write_exchanges

! ==============================================================================

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
    data ihc/0,1,1,1,1,0,0/
    real(r8b), dimension(ns) :: hwva, cl12
    type(tDisu), pointer :: disu => null()
    type(tReg), pointer :: reg => null()
    integer(i4b) :: ireg, n, ilay, iact
    integer(i4b), dimension(ns) :: s
    integer(i4b) :: ir0, ir1, ic0, ic1, ic, ir, jc, jr, i
    real(r4b), dimension(:,:), pointer :: top, bot
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
    disu => this%disu
    allocate(disu)
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
            call readidf_block(gdat(i_bot_l1)%idf, reg%ir0, reg%ir1, reg%ic0, reg%ic1, nodata, top)
            call readidf_block(gdat(i_bot_l2)%idf, reg%ir0, reg%ir1, reg%ic0, reg%ic1, nodata, bot)
          else if (ilay == 2) then
            call readidf_block(gdat(i_top)%idf,    reg%ir0, reg%ir1, reg%ic0, reg%ic1, nodata, top)
            call readidf_block(gdat(i_bot_l1)%idf, reg%ir0, reg%ir1, reg%ic0, reg%ic1, nodata, bot)
          else
            call errmsg('Nlay > 2 not yet supported')
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
                    call errmsg('Program error: top/bot.')
                  end if
                  cl12(jt) = max(thkmin, top(ic,ir)-bot(ic,ir))
                  cl12(jt) = cl12(jt)/2
                end if
                if (ilay < nlay) then !BOT
                  s(jb) = n + reg%layer_nodes
                  if ((top(ic,ir) == nodata).or.(bot(ic,ir) == nodata)) then
                    call errmsg('Program error: top/bot.')
                  end if
                  cl12(jb) = max(thkmin, top(ic,ir)-bot(ic,ir))
                  cl12(jb) = cl12(jb)/2
                end if
                do i = 1, ns
                  if (s(i) /= 0) then
                    if (s(i) < 0) then
                      call errmsg('Program error')
                    end if
                    disu%iac(n) = disu%iac(n) + 1
                    disu%nja = disu%nja + 1
                    if (iact == 2) then
                      disu%ja(disu%nja)   = s(i)
                      disu%ihc(disu%nja)  = ihc(i)
                      disu%hwva(disu%nja) = hwva(i)
                      disu%cl12(disu%nja) = cl12(i)
                    end if
                  end if
                  s(i) = 0
                  cl12(i) = DZERO
                end do
                !
              end if
            end do
          end do
          deallocate(top, bot)
        end do !ilay
      end do !rreg
      if (iact == 1) then
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
  
  end subroutine mf6_mod_write_nam
  
  subroutine mf6_mod_write_disu(this)
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
    ! set disu
    call this%set_disu()

  end subroutine mf6_mod_write_disu
  
  subroutine mf6_mod_write_npf(this)
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
  
  end subroutine mf6_mod_write_npf
  
  subroutine mf6_mod_write_chd(this)
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
  
  end subroutine mf6_mod_write_chd
  
  subroutine mf6_mod_write_drn(this)
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
  
  end subroutine mf6_mod_write_drn
  
  subroutine mf6_mod_write_riv(this)
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
  
  end subroutine mf6_mod_write_riv
  
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
  
  end subroutine mf6_mod_write_oc
  
end module