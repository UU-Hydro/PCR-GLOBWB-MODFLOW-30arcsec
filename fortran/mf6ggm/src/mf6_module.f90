! ==============================================================================
module mf6_module
   ! -- modules
  use, intrinsic :: iso_fortran_env , only: error_unit, output_unit, &
    i4b => int32, i8b => int64, r4b => real32, r8b => real64
  use utilsmod, only: getlun, chkexist, readline, change_case, errmsg, logmsg, &
    readidf_block, writeidf, checkdim, addboundary
  use imod_idf
  
  implicit none 
  
  ! parameters
  integer(i4b), parameter     :: mxslen = 1024
  character(len=1), parameter :: slash = '\' !DOS/LINUX slash
  !
  real(r8b), parameter :: delrc = 0.008333333333333D0
  
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
  
  type tReg
    integer(i4b),                 pointer :: ir0   => null()
    integer(i4b),                 pointer :: ir1   => null()
    integer(i4b),                 pointer :: ic0   => null()
    integer(i4b),                 pointer :: ic1   => null()
    integer(i4b),                 pointer :: ncol  => null()
    integer(i4b),                 pointer :: nrow  => null()
    integer(i4b),                 pointer :: nodes => null()
    integer(i4b), dimension(:,:), pointer :: nod   => null()
  end type tReg
  
  type tMf6_mod
    character(len=mxslen),    pointer :: root => null()
    integer(i4b),             pointer :: ir0  => null()
    integer(i4b),             pointer :: ir1  => null()
    integer(i4b),             pointer :: ic0  => null()
    integer(i4b),             pointer :: ic1  => null()
    type(tReg), dimension(:), pointer :: reg  => null()
  contains
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
    integer(i4b) :: iu, ju, n, i, j, ifound, idum, jsol, ip
    integer(i4b) :: ir0, ir1, ic0, ic1, ir, ic, jr, jc, maxid, nr, nc
    real(r4b) :: xll, yll, cs, nodata !DEBUG
    character(len=mxslen) :: s, key, f
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
    call readline(ju, s); read(s,*) nsol, nsol_indep, nsol_dep
    call readline(ju, s)
    allocate(this%ir0, this%ir1, this%ic0, this%ic1)
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
        j = this%part(ic,ir)
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
        i = this%part(jc,jr)
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
    do i = 1, this%nmod
      ir0 = this%partbb(1,j); ir1 = this%partbb(2,j)
      ic0 = this%partbb(3,j); ic1 = this%partbb(4,j)
      nr = ir1-ir0+1; nc = ic1-ic0+1
      allocate(iwrk2d(nc,nr))
      
      
      deallocate(iwrk2d)
    end do
    
    
    
  end subroutine mf6_init
  
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