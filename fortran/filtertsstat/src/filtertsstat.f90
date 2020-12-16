program filtertsstat
  ! -- modules
  use utilsmod, only: i4b, r4b, r8b, get_args, mxslen, open_file, &
    errmsg, logmsg, ta, parse_line, DZERO, tBB, insert_tab
  !
  implicit none
  !
  type tStat
    logical               :: act = .true.
    logical               :: write = .false.
    integer(i4b)          :: ic = 0
    integer(i4b)          :: ir = 0
    real(r8b)             :: x = DZERO
    real(r8b)             :: y = DZERO
    character(len=mxslen) :: raw = ''
    real(r8b)             :: rho = DZERO
    real(r8b)             :: qre = DZERO
    integer(i4b)          :: iperf = 0
  end type tStat
  
  ! -- locals
  integer(i4b), parameter :: i_best = 1
  integer(i4b), parameter :: i_mean = 2
  type(tBB), pointer :: bb => null()
  type(tStat), pointer :: sd => null()
  type(tStat), dimension(:), pointer :: sdat => null()
  character(len=mxslen) :: f_in, f_out, hdr, s
  character(len=mxslen), dimension(:), allocatable :: args, sa
  logical :: qre_crit, rho_crit
  integer(i4b) :: filt_method
  integer(i4b) :: nsf, ns, na, iu, i, gnc, gnr, ic, ir, nc, nr, n, nmax, np2cand, np3cand
  integer(i4b) :: ylat_ic, xlon_ic, rho_ic, qre_ic, iact, ios, np
  integer(i4b), dimension(4) :: nperf
  integer(i4b), dimension(:,:), allocatable :: si2d
  real(r4b), dimension(4) :: pperf
  real(r8b) :: gxmin, gxmax, gymin, gymax, gcs, x, y, rho, qre
  real(r8b), dimension(:,:), allocatable :: rho2d, qre2d
! ------------------------------------------------------------------------------
  !
  ! read the command line arguments
  args = get_args(); na = len(args)
  read(args(1:7),*) filt_method, gnc, gnr, gxmin, gxmax, gymin, gymax
  select case(filt_method)
    case(i_best)
      call logmsg('Aplying best-cell statistics.')
    case(i_mean)
      call logmsg('Aplying mean-cell statistics.')
    case default
      call errmsg('Invalid filter method.')
  end select
  
  gcs = (gxmax-gxmin)/gnc
  f_in = args(8) 
  read(args(9:12),*) ylat_ic, xlon_ic, rho_ic, qre_ic
  f_out = args(13)
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
          if ((trim(sa(rho_ic)) == 'NA').or.(trim(sa(rho_ic)) == 'Inf')) then
            sd%act = .false.
            cycle
          end if
          if ((trim(sa(qre_ic)) == 'NA').or.(trim(sa(qre_ic)) == 'Inf')) then
            sd%act = .false.
            cycle
          end if
          read(sa(rho_ic),*) sd%rho; read(sa(qre_ic),*) sd%qre
          if ((abs(sd%qre) <= 0.5d0).and.((sd%rho) >= 0.5d0)) then
            sd%iperf = 1
          end if
          if ((abs(sd%qre)  > 0.5d0).and.((sd%rho) >= 0.5d0)) then
            sd%iperf = 2
          end if
          if ((abs(sd%qre) <= 0.5d0).and.((sd%rho)  < 0.5d0)) then
            sd%iperf = 3
          end if
          if ((abs(sd%qre)  > 0.5d0).and.((sd%rho)  < 0.5d0)) then
            sd%iperf = 4
          end if
          if (sd%iperf == 0) then
            call errmsg('Error classifying.')
          end if
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
  !
  if (filt_method == i_best) then !best
    do ir = 1, nr
      do ic = 1, nc
        si2d(ic,ir) = 0
        rho2d(ic,ir) = -huge(DZERO)
        qre2d(ic,ir) =  huge(DZERO)
      end do
    end do
    do i = 1, ns
      sd => sdat(i)
      if (.not.sd%act) cycle
      ic = sd%ic - bb%ic0 + 1; ir = sd%ir - bb%ir0 + 1 
      rho = sd%rho; qre = abs(sd%qre)
      if ((rho > rho2d(ic,ir)).and.(qre < qre2d(ic,ir))) then
        si2d(ic,ir)  = i 
        rho2d(ic,ir) = rho 
        qre2d(ic,ir) = qre
      end if
    end do
    ! label for writing
    nperf = 0
    nsf = 0
    do ir = 1, nr
      do ic = 1, nc
        i = si2d(ic,ir)
        if (i > 0) then
          sd => sdat(i)
          nsf = nsf + 1
          sd%write = .true.
          nperf(sd%iperf) = nperf(sd%iperf) + 1
        end if
      end do
    end do
  else if (filt_method == i_mean) then
    do ir = 1, nr
      do ic = 1, nc
        si2d(ic,ir) = 0
        rho2d(ic,ir) = DZERO
        qre2d(ic,ir) = DZERO
      end do
    end do
    do i = 1, ns
      sd => sdat(i)
      if (.not.sd%act) cycle
      ic = sd%ic - bb%ic0 + 1; ir = sd%ir - bb%ir0 + 1 
      rho = sd%rho; qre = abs(sd%qre)
      si2d(ic,ir)  = si2d(ic,ir) + 1
      rho2d(ic,ir) = rho2d(ic,ir) + rho 
      qre2d(ic,ir) = qre2d(ic,ir) + qre
    end do
    deallocate(sdat)
    !
    ! count
    ns = 0
    do ir = 1, nr
      do ic = 1, nc
        n = si2d(ic,ir)
        if (n > 0) then
          ns = ns + 1
          rho2d(ic,ir) = rho2d(ic,ir)/n
          qre2d(ic,ir) = qre2d(ic,ir)/n
        end if
      end do
    end do
    if (ns <= 0) then
      call errmsg('No cells found.')
    end if
    allocate(sdat(ns))
    if (allocated(sa)) deallocate(sa)
    allocate(sa(6))
    ns = 0; nperf = 0; nmax = 0; np2cand = 0; np3cand = 0
    do ir = 1, nr
      do ic = 1, nc
        n = si2d(ic,ir)
        if (n > 0) then
          nmax = max(n,nmax)
          ns = ns + 1
          sd => sdat(ns)
          sd%write = .true.
          sd%ic = bb%ic0 + ic - 1 !global
          sd%ir = bb%ir0 + ir - 1 !global;
          sd%x = gxmin + sd%ic*gcs -gcs/2.d0
          sd%y = gymax + sd%ir*gcs -gcs/2.d0
          sd%rho = rho2d(ic,ir)
          sd%qre = qre2d(ic,ir)
          if ((abs(sd%qre) <= 0.5d0).and.(sd%rho >= 0.5d0)) then
            sd%iperf = 1
          end if
          if ((abs(sd%qre)  > 0.5d0).and.(sd%rho >= 0.5d0)) then
            sd%iperf = 2
          end if
          if ((abs(sd%qre) <= 0.5d0).and.(sd%rho  < 0.5d0)) then
            sd%iperf = 3
          end if
          qre_crit = (abs(sd%qre) > 0.5d0)
          rho_crit = (sd%rho  < 0.5d0)
          if (qre_crit .and. rho_crit) then
            sd%iperf = 4
          end if
          nperf(sd%iperf) = nperf(sd%iperf) + 1
          write(sa(1),*) (sd%ir - 1)*gnc + sd%ic
          write(sa(2),*) sd%x
          write(sa(3),*) sd%y
          write(sa(4),*) sd%rho
          write(sa(5),*) sd%qre
          write(sa(6),*) achar(9)
          sd%raw = ta(sa(1:6))
          call insert_tab(sd%raw)
        end if
      end do
    end do
  end if
  !
  ! write the new stations
  call logmsg('Writing '//trim(f_out)//'...')
  call open_file(f_out, iu, 'w')
  if (filt_method == i_mean) then !best
    s = 'cell_id latitude longitude avg_rho avg_aqre perf_class'
    call insert_tab(s)
  else  
    s = trim(hdr)//'perf_class'
  end if
  write(iu,'(a)') trim(s)
  do i = 1, ns
    sd => sdat(i)
    if (sd%write) then
      if (len_trim(sd%raw) == 0) then
        call errmsg('Program error')
      end if
      write(iu,'(a)') trim(sd%raw)//ta((/sd%iperf/))
    end if
  end do
  close(iu)
  !
  np = sum(nperf)
  write(s,'(f10.2)') 100.*np/ns
  do i = 1, 4
    pperf(i) = 100.*nperf(i)/np
  end do
  call logmsg('Total # classified: '//ta((/np/))//'/'//ta((/ns/))//' ('//trim(adjustl(s))//' %)')
  call logmsg('class            I         II        III         IV ')
  call logmsg('%     : '//ta(pperf,'(f10.1)'))
  call logmsg('Count : '//ta(nperf,'(i10)'))
  if (filt_method == i_mean) then
    call logmsg('Max count per cell : '//ta((/nmax/),'(i10)'))
  end if
  !
end program