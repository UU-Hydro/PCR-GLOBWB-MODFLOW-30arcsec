program filtertsstat
  ! -- modules
  use utilsmod, only: i4b, r8b, get_args, mxslen, open_file, &
    errmsg, logmsg, ta, parse_line, DZERO, tBB
  !
  implicit none
  !
  type tStat
    logical               :: act = .true.
    logical               :: write = .false.
    integer(i4b)          :: ic = 0
    integer(i4b)          :: ir = 0
    character(len=mxslen) :: raw = ''
    real(r8b)             :: rho = DZERO
    real(r8b)             :: qre = DZERO
  end type tStat
  
  ! -- locals
  type(tBB), pointer :: bb => null()
  type(tStat), pointer :: sd => null()
  type(tStat), dimension(:), pointer :: sdat => null()
  character(len=mxslen) :: f_in, f_out, hdr, s
  character(len=mxslen), dimension(:), allocatable :: args, sa
  integer(i4b) :: nsf, ns, na, iu, i, gnc, gnr, ic, ir, nc, nr
  integer(i4b) :: ylat_ic, xlon_ic, rho_ic, qre_ic, iact, ios
  real(r8b) :: gxmin, gxmax, gymin, gymax, gcs, x, y, rho, qre
  integer(i4b), dimension(:,:), allocatable :: si2d
  real(r8b), dimension(:,:), allocatable :: rho2d, qre2d
! ------------------------------------------------------------------------------
  !
  ! read the command line arguments
  args = get_args(); na = len(args)
  read(args(1:6),*) gnc, gnr, gxmin, gxmax, gymin, gymax
  gcs = (gxmax-gxmin)/gnc
  f_in = args(7) 
  read(args(8:11),*) ylat_ic, xlon_ic, rho_ic, qre_ic
  f_out = args(12)
  !
  ! read the summary file
  call open_file(f_in, iu, 'r')
  do iact = 1, 2
    read(unit=iu,fmt='(a)',iostat=ios) hdr
    ns = 0
    do while(ios == 0)
      read(unit=iu,fmt='(a)',iostat=ios) s
      if (len_trim(s) == 0) exit
      ns = ns + 1
      if (iact == 2) then
        sd => sdat(ns)
        call parse_line(s, sa)
        read(sa(xlon_ic),*) x; read(sa(ylat_ic),*) y
        ic = int((x - gxmin)/gcs, i4b) + 1 ! global
        ir = gnr - int((y - gymin)/gcs, i4b) !global
        if ((ic < 1).or.(ic > gnc).or. (ir < 1).or.(ir > gnr)) then
          sd%act = .false.
        else
          bb%ic0 = min(bb%ic0,ic); bb%ic1 = max(bb%ic1,ic)
          bb%ir0 = min(bb%ir0,ir); bb%ir1 = max(bb%ir1,ir)
          sd%ic = ic; sd%ir = ir
          read(sa(rho_ic),*) sd%rho; read(sa(qre_ic),*) sd%qre
          sd%raw = trim(s)
        end if
      end if
    end do
    if (iact == 1) then
      allocate(bb)
      if (ns > 0) then
        allocate(sdat(ns))
      end if
    end if
    rewind(iu)
  end do
  close(iu)
  bb%ncol = bb%ic1 - bb%ic0 + 1; nc = bb%ncol
  bb%nrow = bb%ir1 - bb%ir0 + 1; nr = bb%nrow
  !
  ! get the maximum rho and minumum qre
  allocate(si2d(nc,nr), rho2d(nc,nr), qre2d(nc,nr))
  do ir = 1, nr
    do ic = 1, nc
      si2d(ic,ir) = 0
      rho2d(ic,ir) = -huge(DZERO)
      qre2d(ic,ir) =  huge(DZERO)
    end do
  end do
  !
  do i = 1, ns
    sd => sdat(i)
    ic = sd%ic - bb%ic0 + 1; ir = sd%ir - bb%ir0 + 1 
    rho = sd%rho; qre = abs(sd%qre)
    if ((rho > rho2d(ic,ir)).and.(qre < qre2d(ic,ir))) then
      si2d(ic,ir)  = i 
      rho2d(ic,ir) = rho 
      qre2d(ic,ir) = qre
    end if
  end do
  !
  ! label for writing
  nsf = 0
  do ir = 1, nr
    do ic = 1, nc
      i = si2d(ic,ir)
      if (i > 0) then
        nsf = nsf + 1
        sdat(i)%write = .true.
      end if
    end do
  end do
  !
  ! write the new stations
  call logmsg('Writing '//trim(f_out)//' for '//ta((/nsf/))//'/'//ta((/ns/))//' stations...')
  call open_file(f_out, iu, 'w')
  write(iu,'(a)') trim(hdr)
  do i = 1, ns
    sd => sdat(i)
    if (sd%write) then
      write(iu,'(a)') trim(sd%raw)
    end if
  end do
  close(iu)
  !
end program