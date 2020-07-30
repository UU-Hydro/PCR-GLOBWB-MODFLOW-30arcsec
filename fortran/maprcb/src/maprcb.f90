program maprcb
  ! modules
  use pcrModule
  use utilsmod, only: getdigits, writeasc, writeidf
  use rcbmod, only: primefactors, rcb

  implicit none
  
  type(tMap), pointer :: map
  logical :: ok
  character(len=1024) :: f, s, fmt
  integer :: np, iverb, maxlev, ilev, iproc, ncol, nrow, icol, irow, n, nr, nc, maxnodes
  integer :: ic0 ,ic1, ir0, ir1, jc0 , jc1, jr0, jr1, lun, i
  integer(kind=8) :: m
  integer, dimension(:), allocatable :: ncut, proc_icolmin, proc_icolmax, proc_irowmin, proc_irowmax
  integer, dimension(:), allocatable :: proc_dicolmin, proc_dicolmax, proc_dirowmin, proc_dirowmax
  real :: nodata, dmin, dmax
  real, dimension(:,:), allocatable :: loadptr, wrk
  double precision :: xul, yul, xmin, xmax, ymin, ymax, cs, dxmin, dxmax, dymin, dymax
  logical, parameter :: lround = .true.
  character(len=1024), dimension(10) :: sa
  integer, parameter :: maxnp = 1000
  
  call getarg(1,f)
  call getarg(2,s); read(s,*) np
  call getarg(3,s); read(s,*) iverb
  
  ! read the map file
  allocate(map)
  ok = map%read_header(f)
  cs  = map%header%cellsizex
  xul = map%header%xul
  yul = map%header%yul
  ncol = map%header%nrcols
  nrow = map%header%nrrows
  call map%read_data()
  call map%get_r4ar(loadptr, nodata, dmin, dmax)
  call map%close()
  call map%clean()
  
  ! change nodata to zero
  do irow = 1, nrow
    do icol = 1, ncol
      if (loadptr(icol,irow) == nodata) then
        loadptr(icol,irow) = 0.0
      else
        loadptr(icol,irow) = 1.0
      end if
    end do
  end do
  nodata = 0.0
  
  if (.false.) then
    allocate(ncut(np/2))
    call primefactors(np, ncut, maxlev)
    maxnodes = 0
  else
    maxlev = 50
    allocate(ncut(maxlev))
    ncut = 2
    maxnodes = nrow*ncol / np
    np = maxnp
  end if
  
  ilev   = 0
  iproc  = 0
  allocate(proc_icolmin(np), proc_icolmax(np), proc_irowmin(np), proc_irowmax(np))
  allocate(proc_dicolmin(np), proc_dicolmax(np), proc_dirowmin(np), proc_dirowmax(np))
  
  write(*,*) 'Calling RCB...'
  call rcb(loadptr, nodata, ncol, nrow, 1, ncol, 1, nrow, ilev, maxlev, &
           iproc, np, proc_icolmin, proc_icolmax, proc_irowmin, proc_irowmax, &
           ncut, maxnodes)
  np = iproc
  write(*,*) 'Done RCB...'
  
  ! round coordinates to whole numbers
  if (lround) then
    do iproc = 1, np
      ic0 = proc_icolmin(iproc); ic1 = proc_icolmax(iproc)
      ir0 = proc_irowmin(iproc); ir1 = proc_irowmax(iproc)
      ic0 = max(1, ic0 - 1); ic1 = min(ncol, ic1 + 1) ! additional box for BC
      ir0 = max(1, ir0 - 1); ir1 = min(nrow, ir1 + 1) ! additional box for BC
      nc = ic1-ic0+1; nr = ir1-ir0+1
      xmin = xul+(ic0-1)*cs; ymin = yul-ir1*cs
      xmax = xmin + nc*cs; ymax = ymin + nr*cs
      jc0 = (floor(xmin)-xul)/cs   + 1; jc1 = (ceiling(xmax)-xul)/cs
      jr0 = (yul-ceiling(ymax))/cs + 1; jr1 = (yul-floor(ymin))/cs
      jc0 = min(jc0, ic0); jc1 = max(jc1, ic1)
      jr0 = min(jr0, ir0); jr1 = max(jr1, ir1)
      jc0 = max(1, jc0); jc1 = min(ncol, jc1)
      jr0 = max(1, jr0); jr1 = min(nrow, jr1)
      ! deltas to non-overlapping
      proc_dicolmin(iproc) = proc_icolmin(iproc)-jc0
      proc_dicolmax(iproc) = jc1-proc_icolmax(iproc)
      proc_dirowmin(iproc) = proc_irowmin(iproc)-jr0
      proc_dirowmax(iproc) = jr1-proc_irowmax(iproc)
      ! set new bb
      proc_icolmin(iproc) = jc0; proc_icolmax(iproc) = jc1
      proc_irowmin(iproc) = jr0; proc_irowmax(iproc) = jr1
    end do
  end if
  
  m = np; n = getdigits(m)

  ! write information
  !write(fmt,'(2(a,i),a)') '(i',n,'.',n,')'
  !write(f,fmt) np
  !write(*,*) 'Writing '//trim(f)//'.txt...'
  !open(file=trim(f)//'.txt', newunit=lun, status='replace')
  open(file='part.txt', newunit=lun, status='replace')
  write(sa(1),*) np
  write(sa(2),*) ncol
  write(sa(3),*) nrow
  write(lun,'(2(a,1x),a)')(trim(adjustl(sa(i))) ,i=1,3)
  do iproc = 1, np
    write(sa(1),*) proc_icolmin(iproc)
    write(sa(2),*) proc_icolmax(iproc)
    write(sa(3),*) proc_irowmin(iproc)
    write(sa(4),*) proc_irowmax(iproc)
    write(sa(5),*) proc_dicolmin(iproc)
    write(sa(6),*) proc_dicolmax(iproc)
    write(sa(7),*) proc_dirowmin(iproc)
    write(sa(8),*) proc_dirowmax(iproc)
    write(lun,'(7(a,1x),a)')(trim(adjustl(sa(i))),i=1,8)
  end do
  close(lun)
  if (iverb == 1) stop  
  
  write(fmt,'(4(a,i),a)') '(i',n,'.',n,',a,i',n,'.',n,')'
  do iproc = 1, np
    write(f,fmt) iproc,'-',np
    ic0 = proc_icolmin(iproc); ic1 = proc_icolmax(iproc)
    ir0 = proc_irowmin(iproc); ir1 = proc_irowmax(iproc)
    nc = ic1-ic0+1; nr = ir1-ir0+1
    !
    allocate(wrk(nc,nr))
    do irow = ir0, ir1
      do icol = ic0, ic1
        wrk(icol-ic0+1,irow-ir0+1) = loadptr(icol,irow)
      end do
    end do
    xmin = xul+(ic0-1)*cs; ymin = yul-ir1*cs
    xmax = xmin+nc*cs;     ymax = ymin+nr*cs
    dxmin = abs(real(nint(xmin))-xmin)
    dymin = abs(real(nint(ymin))-ymin)
    dxmax = abs(real(nint(xmax))-xmax)
    dymax = abs(real(nint(ymax))-ymax)
    write(*,*) 'Abs. rounding error =', dxmin + dymin + dxmax + dymax
    call writeasc('rcb_'//trim(f)//'.asc', wrk, nc, nr, &
      xmin, ymin, cs, 0.D0)
    call writeidf('rcb_'//trim(f)//'.idf', wrk, nc, nr, &
      xmin, ymin, cs, 0.D0)
    deallocate(wrk)
  end do

end program
