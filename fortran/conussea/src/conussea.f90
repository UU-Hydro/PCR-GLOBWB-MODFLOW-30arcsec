program conussea
! ******************************************************************************
  ! --- local
  use imod_idf_par, only: idfobj
  use imod_idf
  use utilsmod, only: mxslen, i1b, i2b, i4b, r4b, r8b, DZERO, DONE, &
    readidf_block, writeasc, writeidf, tBB, errmsg, quicksort_d, addboundary, open_file, &
    chkexist, readgen, tPol, logmsg, ta
  !
  implicit none
  !
  ! --- local
  type tChd
    type(tBB)    :: bb
    integer(i4b) :: itile = 0
    character(len=mxslen) :: f = ''
    character(len=mxslen) :: huc = ''
  end type tChd
  
  type(idfobj) :: idf
  type(tBB), dimension(:), allocatable :: tilbb
  type(tChd), dimension(:), allocatable :: chd
  !
  character(len=1) :: cdum
  character(len=mxslen), dimension(:), allocatable :: tilhuc
  character(len=mxslen) :: d, f, s, f_idf_in, f_idf_out, f_tile, f_tile_huc
  logical :: lfound
  integer(i4b), dimension(:,:), allocatable :: i4wk2d
  integer(i4b) :: iu, i, j, gncol, gnrow, nchd, ntile
  integer(i4b) :: ic, ir, jc, jr, ios, n
  integer(i1b), dimension(:,:), allocatable :: pghb
! ------------------------------------------------------------------------------
  !
  !
  !gir0 = 1; gir1 = 9000; gic0 = 1; gic1 = 16000
  ! count the arguments
  
  call getarg(1,f); call chkexist(f)
  call open_file(f, iu, 'r')
  read(iu,'(a)') f_idf_in; call chkexist(f)
  read(iu,'(a)') f_idf_out
  read(iu,'(a)') f_tile; call chkexist(f)
  read(iu,'(a)') f_tile_huc; call chkexist(f)
  read(iu,*) nchd
  allocate(chd(nchd))
  read(iu,'(a)') d
  do i = 1, nchd
    read(iu,'(a)') s
    f = trim(d)//trim(s); call chkexist(f)
    j = index(s,'_MF6')
    chd(i)%f = f
    chd(i)%huc = s(1:j-1)
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
  do i = 1, nchd
    s = trim(chd(i)%huc)
    lfound = .false.
    do j = 1, ntile
      if (trim(s) == trim(tilhuc(j))) then
        lfound = .true.
        chd(i)%itile = j
        exit
      end if
    end do
    if (.not.lfound) then
      call errmsg("Tile not found "//trim(s))
    end if
  end do
  !
  ! set the bounding box
  do i = 1, nchd
    j = chd(i)%itile
    chd(i)%bb%ic0 = tilbb(j)%ic0; chd(i)%bb%ic1 = tilbb(j)%ic1
    chd(i)%bb%ir0 = tilbb(j)%ir0; chd(i)%bb%ir1 = tilbb(j)%ir1
    chd(i)%bb%ncol = tilbb(j)%ncol; chd(i)%bb%nrow = tilbb(j)%nrow
  end do
  !
  ! read the ghb files
  allocate(pghb(gncol,gnrow))
  do ir = 1, gnrow
    do ic = 1, gncol
      pghb(ic,ir) = 0
    end do
  end do
  !
  do i = 1, nchd
    call open_file(chd(i)%f, iu, 'r')
    do while(.true.)
      read(unit=iu,fmt='(a)',iostat=ios) s
      if (ios /= 0) exit
      if (index(s,'MAXBOUND') > 0) then
        read(s,*) cdum, n
        exit
      end if
    end do
    if (ios /= 0) call errmsg("Could not read "//trim(chd(i)%f))
    !
    do while(.true.)
      read(unit=iu,fmt='(a)',iostat=ios) s
      if (ios /= 0) exit
      if (index(s,'BEGIN PERIOD 1') > 0) then
        exit
      end if
    end do
    !
    ! read block
    if (allocated(i4wk2d)) deallocate(i4wk2d)
    allocate(i4wk2d(4,n))
    read(iu,*)((i4wk2d(ic,ir),ic=1,4),ir=1,n)
    close(iu)
    !
    ! set pointer
    do j = 1, n
      ir = i4wk2d(2,j); ic = i4wk2d(3,j)
      jc = chd(i)%bb%ic0 + ic - 1; jr = chd(i)%bb%ir0 + ir - 1
      pghb(jc,jr) = 1
    end do
  end do
  
  if (.not.idfread(idf, f_idf_in, 1)) then
    call errmsg('Could not read '//trim(f))
  end if
  !
  ! check
  if ((idf%ncol /= gncol).or.(idf%nrow /= gnrow)) then
    call errmsg("Inconsistent ncol/nrow for "//trim(f_idf_in))
  end if
  !
  do ir = 1, gnrow
    do ic = 1, gncol
      if (idf%x(ic,ir) /= idf%nodata) then
        if (pghb(ic,ir) == 1) then
          idf%x(ic,ir) = -idf%x(ic,ir)
        end if
      end if
    end do
  end do
  !
  call writeidf(f_idf_out, idf%x, idf%ncol, idf%nrow, idf%xmin, idf%ymin, idf%dx, idf%nodata)
  !
end program
  