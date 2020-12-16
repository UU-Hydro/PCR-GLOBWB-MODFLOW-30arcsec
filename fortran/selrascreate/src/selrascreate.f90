program selrascreate
  ! -- modules
  use utilsmod, only: i4b, r4b, r8b, get_args, mxslen, open_file, &
    errmsg, logmsg, ta, parse_line, DZERO, tBB, insert_tab, &
    addboundary, writeasc
  use pcrModule, only: tMap
  !
  implicit none
  !
  ! -- locals
  type(tBB), pointer :: bb => null()
  type(tBB), dimension(:), pointer :: gbb => null()
  type(tMap), pointer :: selmap => null(), maskmap => null()
  !
  character(len=mxslen) :: in_selmap_file, in_csv_file, maskmap_file
  character(len=mxslen) :: out_ascfile, out_csv_file, csv_hdr, s
  character(len=mxslen), dimension(:), allocatable :: csv_dat
  character(len=mxslen), dimension(:), allocatable :: args, sa
  logical :: ok
  integer(i4b) :: gnc, gnr, na, gic0, gic1, gir0, gir1, nc, nr, iter, nodes
  integer(i4b) :: ic, ir, jc, jr, n, iu, ios, iact, iobj, nobj, mxobj, i
  integer(i4b), dimension(:), allocatable :: objact
  integer(i4b), dimension(:,:), allocatable :: xs, xm, wrk
  real(r8b) :: gxmin, gxmax, gymin, gymax, gcs, xmin, ymin
! ------------------------------------------------------------------------------
  !
  ! read the command line arguments
  args = get_args(); na = len(args)
  read(args(1:11),*) in_selmap_file, in_csv_file, maskmap_file, &
    out_ascfile, out_csv_file, gnc, gnr, gxmin, gxmax, gymin, gymax
  
  gcs = (gxmax-gxmin)/gnc
  
  gic0 = 1; gic1 = gnc; gir0 = 1; gir1 = gnr
  !gic0 = 21000; gic1 = 24000; gir0 = 3800; gir1 = 6600 !DEBUG
  !
  nc = gic1 - gic0 + 1; nr = gir1 - gir0 + 1
  !
  ! read the csv file
  call open_file(in_csv_file, iu, 'r')
  do iact = 1, 2
    nobj = 0; mxobj = 0
    rewind(iu)
    read(iu,'(a)') csv_hdr
    do while(.true.)
      read(unit=iu,fmt='(a)',iostat=ios) s
      if (ios /= 0) exit
      if (len_trim(s) == 0) exit
      read(s,*) iobj
      mxobj = max(iobj, mxobj)
      nobj = nobj + 1
      if (iact == 2) then
        csv_dat(nobj) = s
      end if
    end do
    if ((iact == 1).and.(nobj > 0)) then
      allocate(csv_dat(nobj))
    end if
  end do
  close(iu)
  !
  allocate(objact(mxobj), gbb(mxobj))
  do i = 1, mxobj
    objact(i) = 0
  end do
  !
  ! read selection map file
  allocate(selmap)
  ok = selmap%init(in_selmap_file)
  call selmap%read_block(xs, gir0, gir1, gic0, gic1, 0)
  !
  ! read mask file
  allocate(maskmap)
  ok = selmap%init(maskmap_file)
  call selmap%read_block(xm, gir0, gir1, gic0, gic1, 0)
  !
  nodes = nc*nr
  allocate(wrk(2,nodes))
  i = 0
  do ir = 1, nr
    do ic = 1, nc
      i = i + 1
      wrk(1,i) = ic
      wrk(2,i) = ir
    end do
  end do
  !
  iter = 0
  do while(.true.)
    n = 0
    do ir = 1, nr
      do ic = 1, nc
        if ((xm(ic,ir) > 0).and.(xs(ic,ir) == 0)) then
          n = n + 1
        end if
      end do
    end do
    if (n == 0) then
      exit
    end if
    iter = iter + 1
    call logmsg('Iteration '//ta((/iter/))//' ('//ta((/n/))//' found)...')
    
    call logmsg('# Adding boundary for '//ta((/nodes/))//' nodes...')
    call addboundary(xs, nc, nr, wrk, nodes)
    !
    nodes = 0
    do ir = 1, nr
      do ic = 1, nc
        if (xs(ic,ir) < 0) then
          nodes = nodes + 1
          wrk(1,nodes) = ic; wrk(2,nodes) = ir
          xs(ic,ir) = abs(xs(ic,ir))
        end if
      end do
    end do
    !
    call logmsg('# boundary cells added: '//ta((/n/)))
  end do
  !
  ! clip with the mask
  do ir = 1, nr
    do ic = 1, nc
      if (xm(ic,ir) == 0) then
        xs(ic,ir) = 0
      end if
    end do
  end do
  !
  ! determine bounding box
  do ir = 1, nr
    do ic = 1, nc
      iobj = xs(ic,ir)
      if (iobj > 0) then
        objact(iobj) = 1
        jc = gic0 + ic - 1; jr = gir0 + ir - 1
        bb => gbb(iobj)
        bb%ic0 = min(bb%ic0, jc); bb%ic1 = max(bb%ic1, jc)
        bb%ir0 = min(bb%ir0, jr); bb%ir1 = max(bb%ir1, jr)
      end if
    end do
  end do
  !
  ! write the selection csv file
  call open_file(out_csv_file, iu, 'w')
  write(iu,'(a)') trim(csv_hdr)//',gic0,gic1,gir0,gir1'
  do i = 1, nobj
    read(csv_dat(i),*) iobj
    if (objact(iobj) == 0) cycle
    bb => gbb(iobj)
    write(iu,'(a)') trim(csv_dat(i))//','//&
      ta((/bb%ic0/))//','//ta((/bb%ic1/))//','// &
      ta((/bb%ir0/))//','//ta((/bb%ir1/))
  end do
  close(iu)
  !
  ! write the selection asc file
  xmin = gxmin + (gic0-1)*gcs; ymin = gymin + (gnr-gir1)*gcs
  call writeasc(out_ascfile, xs, nc, nr, xmin, ymin, gcs, DZERO)
  !
end program