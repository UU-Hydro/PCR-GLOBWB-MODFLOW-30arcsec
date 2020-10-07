program maprcb
  ! modules
  use pcrModule
  use utilsmod, only: getdigits, writeasc, writeidf, addboundary

  implicit none
  
  type(tMap), pointer :: map
  logical :: ok
  character(len=1024) :: f, s, fmt
  integer :: res, nx_map, ny_map, nc_map, nr_map
  integer :: np, iverb, itile, iproc, ncol, nrow, icol, irow, ic, ir, jc, jr, n, nr, nc
  integer :: ic0 ,ic1, ir0, ir1, jc0 ,jc1, jr0, jr1, lun, i
  integer(kind=8) :: m
  integer, dimension(:), allocatable :: proc_icolmin, proc_icolmax, proc_irowmin, proc_irowmax
  integer, dimension(:), allocatable :: proc_dicolmin, proc_dicolmax, proc_dirowmin, proc_dirowmax
  real :: nodata, dmin, dmax
  integer, dimension(:,:), allocatable :: iwrk
  real, dimension(:,:), allocatable :: loadptr, wrk
  double precision :: xul, yul, xmin, xmax, ymin, ymax, cs, dxmin, dxmax, dymin, dymax
  logical, parameter :: lround = .true.
  character(len=1024), dimension(100) :: sa
  integer, parameter :: maxnp = 1000
  integer, dimension(:), allocatable :: iadd
  
  call getarg(1,f)
  call getarg(2,s); read(s,*) res
  call getarg(3,s); read(s,*) iverb
  
  ! read the map file
  allocate(map)
  ok = map%init(f)
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
  ir0 = nrow+1; ir1 = 0
  do irow = 1, nrow
    do icol = 1, ncol
      if (loadptr(icol,irow) == nodata) then
        loadptr(icol,irow) = 0.0
      else
        loadptr(icol,irow) = 1.0
        ir0 = min(ir0, irow); ir1 = max(ir1, irow)
      end if
    end do
  end do
  !nodata = 0.0
  !call addboundary(loadptr, nodata)
  !
  nx_map = 360/res; ny_map = 180/res
  nc_map = ncol/nx_map; nr_map = nc_map
  np = nx_map*ny_map
  !
  write(*,*) 'Min/max row:',yul-(ir0-1)*cs, yul-(ir1-1)*cs
  write(*,*) 'Nx, ny =', nx_map, ny_map
  !
  iproc  = 0
  allocate(proc_icolmin(np), proc_icolmax(np), proc_irowmin(np), proc_irowmax(np))
  allocate(proc_dicolmin(np), proc_dicolmax(np), proc_dirowmin(np), proc_dirowmax(np))
  allocate(iadd(np))
  !
  iproc = 0
  do irow = 1, ny_map
    do icol = 1, nx_map
      iproc = iproc + 1
      proc_icolmin(iproc) = (icol-1)*nc_map + 1
      proc_icolmax(iproc) = proc_icolmin(iproc) + nc_map - 1
      proc_irowmin(iproc) = (irow-1)*nr_map + 1
      proc_irowmax(iproc) = proc_irowmin(iproc) + nr_map - 1
    end do
  end do
  !
  ! round coordinates to whole numbers
  if (lround) then
    do iproc = 1, np
      ic0 = proc_icolmin(iproc); ic1 = proc_icolmax(iproc)
      ir0 = proc_irowmin(iproc); ir1 = proc_irowmax(iproc)
      !ic0 = max(1, ic0 - 1); ic1 = min(ncol, ic1 + 1) ! additional box for BC
      !ir0 = max(1, ir0 - 1); ir1 = min(nrow, ir1 + 1) ! additional box for BC
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
      !write(*,*) 'nc,nr=',nc,nr, jc0, jc1
      proc_icolmin(iproc) = jc0; proc_icolmax(iproc) = jc1
      proc_irowmin(iproc) = jr0; proc_irowmax(iproc) = jr1
    end do
  end if
  !
  ! check which tile have load
  iadd = 0
  do iproc = 1, np
    do irow = proc_irowmin(iproc), proc_irowmax(iproc)
      do icol = proc_icolmin(iproc), proc_icolmax(iproc)
        if (abs(loadptr(icol,irow)) > 0.) iadd(iproc) = 1
      end do
    end do
  end do
  !
  m = sum(iadd)
  write(*,*) 'npart =', m
  !
  ! write information
  !write(fmt,'(2(a,i),a)') '(i',n,'.',n,')'
  !write(f,fmt) np
  !write(*,*) 'Writing '//trim(f)//'.txt...'
  !open(file=trim(f)//'.txt', newunit=lun, status='replace')
  fmt = '(2(a,i2.2),a)'
  open(file='tile.txt', newunit=lun, status='replace')
  !write(sa(1),*) nx_map
  !write(sa(2),*) ny_map
  !write(sa(3),*) m
  !write(sa(4),*) ncol
  !write(sa(5),*) nrow
  !write(lun,'(4(a,1x),a)')(trim(adjustl(sa(i))) ,i=1,5)
  write(sa(1),*) m
  write(sa(2),*) ncol
  write(sa(3),*) nrow
  write(lun,'(2(a,1x),a)')(trim(adjustl(sa(i))) ,i=1,3)
  iproc = 0
  do irow = 1, ny_map
    do icol = 1, nx_map
      iproc = iproc + 1
      if (iadd(iproc) == 0) cycle
      !write(sa(1),*) icol
      !write(sa(2),*) irow
      ic0 = proc_icolmin(iproc); ic1 = proc_icolmax(iproc)
      ir0 = proc_irowmin(iproc); ir1 = proc_irowmax(iproc)
      write(sa(1),*) ic0
      write(sa(2),*) ic1
      write(sa(3),*) ir0
      write(sa(4),*) ir1
      !
      ! local bounding box
      n = 0
      jr0 = nrow+1; jr1 = 0; jc0 = ncol+1; jc1 = 0
      do ir = ir0, ir1
        do ic = ic0, ic1
          jr = ir-ir0+1; jc = ic-ic0+1
          if (abs(loadptr(ic,ir)) > 0) then
            n = n + 1
            jr0 = min(jr0, jr); jr1 = max(jr1, jr)
            jc0 = min(jc0, jc); jc1 = max(jc1, jc)
          end if
        end do
      end do
      !write(sa(7),*) n
      !write(sa(8),*) jc0
      !write(sa(9),*) jc1
      !write(sa(10),*) jr0
      !write(sa(11),*) jr1
      !write(lun,'(10(a,1x),a)')(trim(adjustl(sa(i))),i=1,11)
      write(sa(5),*) proc_dicolmin(iproc)
      write(sa(6),*) proc_dicolmax(iproc)
      write(sa(7),*) proc_dirowmin(iproc)
      write(sa(8),*) proc_dirowmax(iproc)
      write(lun,'(7(a,1x),a)')(trim(adjustl(sa(i))),i=1,8)
    end do
  end do
  close(lun)
  if (iverb == 1) stop  
  
  allocate(iwrk(ncol,nrow))
  do irow = 1, nrow
    do icol = 1, ncol
      iwrk(icol,irow) = 0
    end do
  end do
  !
  iproc = 0
  itile = 0
  do irow = 1, ny_map
    do icol = 1, nx_map
      iproc = iproc + 1
      if (iadd(iproc) == 0) cycle
      itile = itile + 1
      write(f,'(a,i3.3,a,i3.3)') 'tile_', itile, '-', m
      !
      ic0 = proc_icolmin(iproc); ic1 = proc_icolmax(iproc)
      ir0 = proc_irowmin(iproc); ir1 = proc_irowmax(iproc)
      nc = ic1-ic0+1; nr = ir1-ir0+1
      !
      allocate(wrk(nc,nr))
      do ir = ir0, ir1
        do ic = ic0, ic1
          wrk(ic-ic0+1,ir-ir0+1) = loadptr(ic,ir)
          iwrk(ic,ir) = iproc
        end do
      end do
      xmin = xul+(ic0-1)*cs; ymin = yul-ir1*cs
      xmax = xmin+nc*cs;     ymax = ymin+nr*cs
      dxmin = abs(real(nint(xmin))-xmin)
      dymin = abs(real(nint(ymin))-ymin)
      dxmax = abs(real(nint(xmax))-xmax)
      dymax = abs(real(nint(ymax))-ymax)
      write(*,*) 'Abs. rounding error =', dxmin + dymin + dxmax + dymax
      call writeasc(trim(f)//'.asc', wrk, nc, nr, &
        xmin, ymin, cs, 0.D0)
      call writeidf(trim(f)//'.idf', wrk, nc, nr, &
        xmin, ymin, cs, 0.D0)
      deallocate(wrk)
    end do
  end do

  f = 'tiles_glob'
  xmin = xul; ymin = yul-nrow*cs
  call writeasc(trim(f)//'.asc', iwrk, ncol, nrow, &
    xmin, ymin, cs, 0.D0)
  call writeidf(trim(f)//'.idf', iwrk, ncol, nrow, &
    xmin, ymin, cs, 0.D0)
end program
