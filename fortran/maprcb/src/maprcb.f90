program maprcb
  ! modules
  use pcrModule
  use utilsmod, only: primefactors, rcb, getdigits, writeasc, writeidf

  implicit none
  
  type(tMap), pointer :: map
  logical :: ok
  character(len=1024) :: f, s, fmt
  integer :: np, maxlev, ilev, iproc, ncol, nrow, icol, irow, n, nr, nc
  integer :: ic0 ,ic1, ir0, ir1, jc0 , jc1, jr0, jr1
  integer(kind=8) :: m
  integer, dimension(:), allocatable :: ncut, proc_icolmin, proc_icolmax, proc_irowmin, proc_irowmax
  real :: nodata, dmin, dmax
  real, dimension(:,:), allocatable :: loadptr, wrk
  double precision :: xul, yul, xmin, xmax, ymin, ymax, cs, dxmin, dxmax, dymin, dymax
  logical, parameter :: lround = .true.
  
  
  call getarg(1,f)
  call getarg(2,s); read(s,*) np
  
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
  
  allocate(ncut(np/2))
  call primefactors(np, ncut, maxlev)
  
  ilev   = 0
  iproc  = 0
  allocate(proc_icolmin(np))
  allocate(proc_icolmax(np))
  allocate(proc_irowmin(np))
  allocate(proc_irowmax(np))
  
  write(*,*) 'Calling RCB...'
  call rcb(loadptr, nodata, ncol, nrow, 1, ncol, 1, nrow, ilev, maxlev, &
           iproc, np, proc_icolmin, proc_icolmax, proc_irowmin, proc_irowmax, &
           ncut)
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
      proc_icolmin(iproc) = jc0; proc_icolmax(iproc) = jc1;
      proc_irowmin(iproc) = jr0; proc_irowmax(iproc) = jr1;
    end do
  end if
  
  m = np; n = getdigits(m)
  
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