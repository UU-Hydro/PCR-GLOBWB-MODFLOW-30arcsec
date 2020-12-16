module readMf6
   use utilsmod, only: open_file, mxslen, i4b, r8b, errmsg
  
  implicit none
  
  contains
  
  subroutine readasc(f, x, nc, nr)
    ! -- arguments
    character(len=*), intent(inout) :: f
    real(r8b), dimension(:,:), allocatable, intent(inout) :: x
    integer(i4b), intent(in) :: nc
    integer(i4b), intent(in) :: nr
    ! -- locals
    integer(i4b) :: iu, ic, ir
! ------------------------------------------------------------------------------
    call open_file(f, iu, 'r')
    if (allocated(x)) deallocate(x)
    allocate(x(nc,nr))
    read(iu,*)((x(ic,ir),ic=1,nc),ir=1,nr)
    close(iu)
    !
    return
  end subroutine readasc
  
  subroutine readlist2d(f, x, nc, nr, nd, nodata)
    ! -- arguments
    character(len=*), intent(inout) :: f
    real(r8b), dimension(:,:,:), allocatable, intent(inout) :: x
    integer(i4b), intent(in) :: nc
    integer(i4b), intent(in) :: nr
    integer(i4b), intent(in) :: nd
    real(r8b), intent(in) :: nodata
    ! -- locals
    character(len=1) :: cdum
    character(len=mxslen) :: s
    integer(i4b) :: i, j, iu, id2, ic, ir, n, ios
    real(r8b), dimension(:,:), allocatable :: r8wk2d
    logical :: lfound
! ------------------------------------------------------------------------------
    call open_file(f, iu, 'r')
    if (allocated(x)) deallocate(x)
    allocate(x(nc,nr,nd))
    !
    do i = 1, nd
      do ir = 1, nr
        do ic = 1, nc
          x(ic,ir,i) = nodata
        end do
      end do
    end do
    !
    lfound = .false.
    do while(.true.)
      read(unit=iu,fmt='(a)',iostat=ios) s
      if (ios /= 0) exit
      if (index(s,'MAXBOUND') > 0) then
        read(s,*) cdum, n
        lfound = .true.
        exit
      end if
    end do
    if ((ios /= 0).or.(.not.lfound)) call errmsg("Could not read "//trim(f))
    !
    lfound = .false.
    do while(.true.)
      read(unit=iu,fmt='(a)',iostat=ios) s
      if (ios /= 0) exit
      if (index(s,'BEGIN PERIOD 1') > 0) then
        lfound = .true.
        exit
      end if
    end do
    if ((ios /= 0).or.(.not.lfound)) call errmsg("Could not read "//trim(f))
    !
    ! read block
    allocate(r8wk2d(nd+3,n))
    read(iu,*)((r8wk2d(ic,ir),ic=1,nd+3),ir=1,n)
    close(iu)
    !
    do i = 1, n
      ir = int(r8wk2d(2,i),i4b); ic = int(r8wk2d(3,i),i4b)
      do j = 4, nd+3
        x(ic,ir,j-3) = r8wk2d(j,i)
      end do
    end do
    !
    deallocate(r8wk2d)
    !
    return
  end subroutine readlist2d
end module readMf6
  
program conustile
! ******************************************************************************
  ! --- local
  use imod_idf_par, only: idfobj
  use imod_idf
  use utilsmod, only: mxslen, i1b, i2b, i4b, r4b, r8b, DZERO, DONE, &
    writeasc, writeidf, tBB, errmsg, open_file, &
    chkexist, logmsg, ta, create_dir, swap_slash, DZERO
  use readMf6, only: readasc, readlist2d
  !
  implicit none
  !
  ! --- local
  type tSub
    type(tBB)    :: bb
    integer(i4b) :: itile = 0
    character(len=mxslen) :: id = ''
    character(len=mxslen) :: huc = ''
  end type tSub
  
  type(idfobj) :: idf
  type(tBB), dimension(:), allocatable :: tilbb
  type(tSub), dimension(:), allocatable :: sub
  !
  integer(i4b), parameter :: maxvar = 10
  real(r8b), parameter :: defnodata = -99999.d0
  
  character(len=1) :: cdum
  character(len=mxslen), dimension(:), allocatable :: tilhuc
  character(len=mxslen) :: ts, d, sd, f, s, d_sub, d_tile_out, f_tile, f_tile_huc, fi, fo
  character(len=mxslen), dimension(maxvar) :: rvar, lvar
  logical :: lfound, lex
  logical, dimension(maxvar) :: rep_mv
  integer(i4b), dimension(:,:), allocatable :: i4wk2d
  integer(i4b) :: iu, i, j, gncol, gnrow, nsub, ntile, nc, nr, ic0, ir1
  integer(i4b) :: ic, ir, jc, jr, ios, n, nvar
  integer(i1b), dimension(:,:), allocatable :: pghb
  real(r8b), dimension(maxvar) :: src_mv, tgt_mv 
  real(r8b) :: gxll, gyll, gcs, xll, yll
  real(r8b), dimension(:,:), allocatable :: x2d, idomain
  real(r8b), dimension(:,:,:), allocatable :: x3d
! ------------------------------------------------------------------------------
  !
  !
  !gir0 = 1; gir1 = 9000; gic0 = 1; gic1 = 16000
  ! count the arguments
  
  call getarg(1,f); call chkexist(f)
  call open_file(f, iu, 'r')
  read(iu,*) gxll, gyll, gcs
  read(iu,'(a)') d_tile_out
  read(iu,'(a)') f_tile; call chkexist(f)
  read(iu,'(a)') f_tile_huc; call chkexist(f)
  read(iu,*) nsub
  allocate(sub(nsub))
  read(iu,'(a)') d_sub
  do i = 1, nsub
    read(iu,'(a)') s
    j = index(s,'_MF6')
    sub(i)%id = s
    sub(i)%huc = s(1:j-1)
  end do
  close(iu)
  !
  ! read the tile toplology
  call open_file(f_tile, iu, 'r')
  read(iu,*) ntile, gncol, gnrow
  allocate(tilbb(ntile))
  do i = 1, ntile
    read(iu,*) tilbb(i)%ic0, tilbb(i)%ic1, tilbb(i)%ir0, tilbb(i)%ir1
    tilbb(i)%ncol = tilbb(i)%ic1 - tilbb(i)%ic0 + 1
    tilbb(i)%nrow = tilbb(i)%ir1 - tilbb(i)%ir0 + 1
  end do
  close(iu)
  !
  allocate(tilhuc(ntile))
  call open_file(f_tile_huc, iu, 'r')
  do i = 1, ntile
    read(iu,'(a)') tilhuc(i)
  end do
  close(iu)
  !
  ! determine itile for the chd
  do i = 1, nsub
    s = trim(sub(i)%huc)
    lfound = .false.
    do j = 1, ntile
      if (trim(s) == trim(tilhuc(j))) then
        lfound = .true.
        sub(i)%itile = j
        exit
      end if
    end do
    if (.not.lfound) then
      call errmsg("Tile not found "//trim(s))
    end if
  end do
  !
  ! set the bounding box
  do i = 1, nsub
    j = sub(i)%itile
    sub(i)%bb%ic0 = tilbb(j)%ic0; sub(i)%bb%ic1 = tilbb(j)%ic1
    sub(i)%bb%ir0 = tilbb(j)%ir0; sub(i)%bb%ir1 = tilbb(j)%ir1
    sub(i)%bb%ncol = tilbb(j)%ncol; sub(i)%bb%nrow = tilbb(j)%nrow
  end do
  !
  nvar = 0
  nvar = nvar + 1; rvar(nvar) = 'idomain';    rep_mv(nvar) = .false.; src_mv(nvar) =          DZERO; tgt_mv(nvar) = DZERO
  nvar = nvar + 1; rvar(nvar) = 'top';        rep_mv(nvar) = .true.;  src_mv(nvar) = -2147483650.d0; tgt_mv(nvar) = defnodata
  nvar = nvar + 1; rvar(nvar) = 'bottoms';    rep_mv(nvar) = .true.;  src_mv(nvar) = -2147483650.d0; tgt_mv(nvar) = defnodata
  nvar = nvar + 1; rvar(nvar) = 'startheads'; rep_mv(nvar) = .true.;  src_mv(nvar) =        -999.d0; tgt_mv(nvar) = defnodata
  nvar = nvar + 1; rvar(nvar) = 'hk' ;        rep_mv(nvar) = .false.; src_mv(nvar) =          DZERO; tgt_mv(nvar) = defnodata
  nvar = nvar + 1; rvar(nvar) = 'rch';        rep_mv(nvar) = .false.; src_mv(nvar) =          DZERO; tgt_mv(nvar) = defnodata
  nvar = 0 !debug
  
  do i = 1, nsub
    ts = 'tile_'//ta((/sub(i)%itile/),3)//'-'//ta((/ntile/),3)
    write(*,'(a)') '================================================================='
    write(*,'(a)') 'Processing for tile '//trim(ts)//' (huc '//trim(sub(i)%huc)//')...'
    write(*,'(a)') '================================================================='
    !
    d = trim(d_tile_out)//trim(ts)//'\'; call swap_slash(d)
    call create_dir(d)
    !
    nc = sub(i)%bb%ncol; nr = sub(i)%bb%nrow
    ic0 = sub(i)%bb%ic0; ir1 = sub(i)%bb%ir1
    xll = gxll + (sub(i)%bb%ic0-1)*gcs; yll = gyll + (gnrow-sub(i)%bb%ir1)*gcs
    !
    !
!    do j = 1, 2
    do j = 1, nvar
      if (j > 4) then
        s = '_1'
      else
        s = ''
      end if
      !
      fi = trim(d_sub)//trim(sub(i)%id)//'\'//trim(sub(i)%id)//trim(s)//'.'//trim(rvar(j))
      fo = trim(d)//trim(rvar(j))
      call readasc(fi, x2d, nc, nr)
      if (rep_mv(j)) then
        do ir = 1, nr
          do ic = 1, nc
            if (x2d(ic,ir) == src_mv(j)) then
              x2d(ic,ir) = tgt_mv(j)
            end if
          end do
        end do
      end if
      !
      if (j == 1) then
        if (allocated(idomain)) deallocate(idomain)
        allocate(idomain(nc,nr))
        do ir = 1, nr
          do ic = 1, nc
            idomain(ic,ir) = x2d(ic,ir)
          end do
        end do
      else
        do ir = 1, nr
          do ic = 1, nc
            if (idomain(ic,ir) == DZERO) then
              x2d(ic,ir) = tgt_mv(j)
            end if
          end do
        end do
      end if
      call writeidf(trim(fo)//'.idf', x2d, nc, nr, xll, yll, gcs, tgt_mv(j))
      call writeasc(trim(fo)//'.asc', x2d, nc, nr, xll, yll, gcs, tgt_mv(j))
    end do
    !
    ! ***** CHD
    !
    if (.false.) then !debug
    fi = trim(d_sub)//trim(sub(i)%id)//'\'//trim(sub(i)%id)//'.chd'
    inquire(file=fi, exist=lex)
    if (lex) then
      call readlist2d(fi, x3d, nc, nr, 1, defnodata)
    else
      call logmsg("***** Writing dummy raster for CHD *****")
      if (allocated(x3d)) deallocate(x3d)
      allocate(x3d(nc,nr,1))
      do ir = 1, nr
        do ic = 1, nc
          x3d(ic,ir,1) = defnodata
        end do
      end do
    end if
    lvar(1) = trim(d)//'chd.head'
    call writeidf(trim(lvar(1))//'.idf', x3d(:,:,1), nc, nr, xll, yll, gcs, defnodata)
    call writeasc(trim(lvar(1))//'.asc', x3d(:,:,1), nc, nr, xll, yll, gcs, defnodata)
    end if
    !
    ! ***** GHB: open water cells in the National Land Cover Datase
    if (.false.) then !debug
    fi = trim(d_sub)//trim(sub(i)%id)//'\'//trim(sub(i)%id)//'.ghb.ghb-nlcd'
    call readlist2d(fi, x3d, nc, nr, 2, defnodata)
    lvar(1) = trim(d)//'ghb-nlcd.bhead'
    lvar(2) = trim(d)//'ghb-nlcd.cond'
    do j = 1, 2
      call writeidf(trim(lvar(j))//'.idf', x3d(:,:,j), nc, nr, xll, yll, gcs, defnodata)
      call writeasc(trim(lvar(j))//'.asc', x3d(:,:,j), nc, nr, xll, yll, gcs, defnodata)
    end do
    end if
    !
    ! ***** GHB: Great Lakes shorelines and long-term average lake stages)
    if (.true.) then !debug
    fi = trim(d_sub)//trim(sub(i)%id)//'\'//trim(sub(i)%id)//'.ghb.ghb-largelakes'
    inquire(file=fi, exist=lex)
    if (lex) then
      call readlist2d(fi, x3d, nc, nr, 2, defnodata)
    else
      call logmsg("***** Writing dummy raster for GHB large lakes *****")
      if (allocated(x3d)) deallocate(x3d)
      allocate(x3d(nc,nr,2))
      do j = 1, 2
        do ir = 1, nr
          do ic = 1, nc
            x3d(ic,ir,1) = defnodata
          end do
        end do
      end do
    end if 
    lvar(1) = trim(d)//'ghb-largelakes.bhead'
    lvar(2) = trim(d)//'ghb-largelakes.cond'
    do j = 1, 2
      call writeidf(trim(lvar(j))//'.idf', x3d(:,:,j), nc, nr, xll, yll, gcs, defnodata)
      call writeasc(trim(lvar(j))//'.asc', x3d(:,:,j), nc, nr, xll, yll, gcs, defnodata)
    end do
    end if
    !
    ! ***** DRN
    !
    if (.false.) then !debug
    fi = trim(d_sub)//trim(sub(i)%id)//'\'//trim(sub(i)%id)//'.drn'
    call readlist2d(fi, x3d, nc, nr, 2, defnodata)
    lvar(1) = trim(d)//'drn.elev'
    lvar(2) = trim(d)//'drn.cond'
    do j = 1, 2
      call writeidf(trim(lvar(j))//'.idf', x3d(:,:,j), nc, nr, xll, yll, gcs, defnodata)
      call writeasc(trim(lvar(j))//'.asc', x3d(:,:,j), nc, nr, xll, yll, gcs, defnodata)
    end do
    end if !debug
    
  end do
  !
end program
  