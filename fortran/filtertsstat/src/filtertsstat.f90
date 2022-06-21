program filtertsstat
  ! -- modules
  use utilsmod, only: i4b, r4b, r8b, get_args, mxslen, open_file, &
    errmsg, logmsg, ta, parse_line, RZERO, DZERO, tBB, insert_tab, writeasc, writeflt
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
    real(r8b)             :: measslp = DZERO
    real(r8b)             :: modslp = DZERO
    integer(i4b)          :: iperf = 0
  end type tStat
  
  ! -- locals
  integer(i4b), parameter :: jc1    =  1
  integer(i4b), parameter :: jc2    =  2
  integer(i4b), parameter :: jc3    =  3
  integer(i4b), parameter :: jc4    =  4
  integer(i4b), parameter :: jc1a   =  5
  integer(i4b), parameter :: jc1b   =  6
  integer(i4b), parameter :: jc1c   =  7
  integer(i4b), parameter :: jc1d   =  8
  integer(i4b), parameter :: jc1e   =  9
  integer(i4b), parameter :: jc1f   = 10
  integer(i4b), parameter :: jc1g   = 11
  integer(i4b), parameter :: jc1h   = 12
  integer(i4b), parameter :: jc1i   = 13
  integer(i4b), parameter :: jc2a   = 14
  integer(i4b), parameter :: jc2b   = 15
  integer(i4b), parameter :: jc2c   = 16
  integer(i4b), parameter :: jc2d   = 17
  integer(i4b), parameter :: jc2e   = 18
  integer(i4b), parameter :: jc2f   = 19
  integer(i4b), parameter :: jc2g   = 20
  integer(i4b), parameter :: jc2h   = 21
  integer(i4b), parameter :: jc2i   = 22
  integer(i4b), parameter :: jc3a   = 23
  integer(i4b), parameter :: jc3b   = 24
  integer(i4b), parameter :: jc3c   = 25
  integer(i4b), parameter :: jc3d   = 26
  integer(i4b), parameter :: jc3e   = 27
  integer(i4b), parameter :: jc3f   = 28
  integer(i4b), parameter :: jc3g   = 29
  integer(i4b), parameter :: jc3h   = 30
  integer(i4b), parameter :: jc3i   = 31
  integer(i4b), parameter :: jc4a   = 32
  integer(i4b), parameter :: jc4b   = 33
  integer(i4b), parameter :: jc4c   = 34
  integer(i4b), parameter :: jc4d   = 35
  integer(i4b), parameter :: jc4e   = 36
  integer(i4b), parameter :: jc4f   = 37
  integer(i4b), parameter :: jc4g   = 38
  integer(i4b), parameter :: jc4h   = 39
  integer(i4b), parameter :: jc4i   = 40
  integer(i4b), parameter :: nclass = jc4i
  !
  character(len=4), dimension(nclass) :: class_str
  !               1234   1234   1234   1234   1234   1234   1234   1234   1234   1234 
  data class_str /'I  ', 'II  ','III ', 'IV ', &
                  'Ia  ','Ib  ','Ic  ','Id  ','Ie  ','If  ','Ig  ','Ih  ','Ii  ', &
                  'IIa ','IIb ','IIc ','IId ','IIe ','IIf ','IIg ','IIh ','IIi ', &
                  'IIIa','IIIb','IIIc','IIId','IIIe','IIIf','IIIg','IIIh','IIIi', &
                  'IVa ','IVb ','IVc ','IVd ','IVe ','IVf ','IVg ','IVh ','IVi '/
  
  integer(i4b), parameter :: i_best = 1
  integer(i4b), parameter :: i_mean = 2
  real(r8b), parameter :: rho_thres  = 0.5D0
  real(r8b), parameter :: aqre_thres = 0.5D0
  real(r8b), parameter :: trend_thres = 0.05D0
  
!  real(r8b), parameter :: aqre_thres = 50.0D0
  type(tBB), pointer :: bb => null()
  type(tStat), pointer :: sd => null()
  type(tStat), dimension(:), pointer :: sdat => null()
  character(len=mxslen) :: f_in, f_out, f_out_pref, hdr, s
  character(len=mxslen), dimension(:), allocatable :: args, sa
  logical :: l_amp, l_tim
  integer(i4b) :: mod_tr_dir, meas_tr_dir
  logical :: qre_crit, rho_crit, write_perf, trend_perf
  integer(i4b) :: filt_method
  integer(i4b) :: nsf, nsr, ns, na, iu, i, i0, i1, gnc, gnr, ic, ir, nc, nr, n, nmax, np2cand, np3cand
  integer(i4b) :: ylat_ic, xlon_ic, rho_ic, qre_ic, measslp_ic, modslp_ic, iact, ios, np
  integer(i4b), dimension(:), allocatable :: nperf
  integer(i4b), dimension(:,:), allocatable :: si2d, perf2d
  real(r4b), dimension(:), allocatable :: pperf
  real(r8b) :: gxmin, gxmax, gymin, gymax, gcs, x, y, rho, qre, slp
  real(r8b) :: rhotot, aqretot
  real(r8b), dimension(:,:), allocatable :: rho2d, qre2d, modslp2d, measslp2d
! ------------------------------------------------------------------------------
  !
  ! read the command line arguments
  args = get_args(); na = size(args)
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
  read(args(9:14),*) ylat_ic, xlon_ic, rho_ic, qre_ic, measslp_ic, modslp_ic
  if ((measslp_ic <= 0).or.(modslp_ic <= 0)) then
    trend_perf = .false.
  else
    trend_perf = .true.
  end if
  allocate(nperf(nclass), pperf(nclass))
  do i = 1, nclass
    nperf(i) = 0
    pperf(i) = RZERO
  end do
  !
  f_out = args(15)
  !
  write_perf = .false.
  if (na > 15) then
    write_perf = .true.
    f_out_pref = args(16)
  end if
  !
  ! read the summary file
  call open_file(f_in, iu, 'r')
  do iact = 1, 2
    rhotot = DZERO; aqretot = DZERO
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
          rhotot = rhotot + sd%rho
          aqretot = aqretot + abs(sd%qre)
          sd%iperf = 0
          !
          if (abs(sd%qre) < aqre_thres) then ! amplitude
            l_amp = .true.
          else
            l_amp = .false.
          end if
          if (sd%rho > rho_thres) then ! timing
            l_tim = .true.
          else
            l_tim = .false.
          end if
          !
          if (trend_perf) then
            read(sa(measslp_ic),*) sd%measslp; read(sa(modslp_ic),*) sd%modslp
            !
            meas_tr_dir = 0
            if (abs(sd%measslp) > trend_thres) then ! measurement has trend
              if (sd%measslp < RZERO) then
                meas_tr_dir = -1
              else
                meas_tr_dir = 1
              end if
            end if
            mod_tr_dir = 0
            if (abs(sd%modslp) > trend_thres) then ! model has trend
              if (sd%modslp < RZERO) then
                mod_tr_dir = -1
              else
                mod_tr_dir = 1
              end if
            end if
            !
            if (l_amp .and.l_tim) then
              if ((mod_tr_dir ==  0).and.(meas_tr_dir ==  0)) sd%iperf = jc1a
              if ((mod_tr_dir ==  1).and.(meas_tr_dir ==  1)) sd%iperf = jc1b
              if ((mod_tr_dir == -1).and.(meas_tr_dir == -1)) sd%iperf = jc1c
              if ((mod_tr_dir ==  0).and.(meas_tr_dir ==  1)) sd%iperf = jc1d
              if ((mod_tr_dir ==  0).and.(meas_tr_dir == -1)) sd%iperf = jc1e
              if ((mod_tr_dir ==  1).and.(meas_tr_dir ==  0)) sd%iperf = jc1f
              if ((mod_tr_dir ==  1).and.(meas_tr_dir == -1)) sd%iperf = jc1g
              if ((mod_tr_dir == -1).and.(meas_tr_dir ==  0)) sd%iperf = jc1h
              if ((mod_tr_dir == -1).and.(meas_tr_dir ==  1)) sd%iperf = jc1i
            end if
            if ((.not.l_amp).and.l_tim) then
              if ((mod_tr_dir ==  0).and.(meas_tr_dir ==  0)) sd%iperf = jc2a
              if ((mod_tr_dir ==  1).and.(meas_tr_dir ==  1)) sd%iperf = jc2b
              if ((mod_tr_dir == -1).and.(meas_tr_dir == -1)) sd%iperf = jc2c
              if ((mod_tr_dir ==  0).and.(meas_tr_dir ==  1)) sd%iperf = jc2d
              if ((mod_tr_dir ==  0).and.(meas_tr_dir == -1)) sd%iperf = jc2e
              if ((mod_tr_dir ==  1).and.(meas_tr_dir ==  0)) sd%iperf = jc2f
              if ((mod_tr_dir ==  1).and.(meas_tr_dir == -1)) sd%iperf = jc2g
              if ((mod_tr_dir == -1).and.(meas_tr_dir ==  0)) sd%iperf = jc2h
              if ((mod_tr_dir == -1).and.(meas_tr_dir ==  1)) sd%iperf = jc2i
            end if
            if (l_amp.and.(.not.l_tim)) then
              if ((mod_tr_dir ==  0).and.(meas_tr_dir ==  0)) sd%iperf = jc3a
              if ((mod_tr_dir ==  1).and.(meas_tr_dir ==  1)) sd%iperf = jc3b
              if ((mod_tr_dir == -1).and.(meas_tr_dir == -1)) sd%iperf = jc3c
              if ((mod_tr_dir ==  0).and.(meas_tr_dir ==  1)) sd%iperf = jc3d
              if ((mod_tr_dir ==  0).and.(meas_tr_dir == -1)) sd%iperf = jc3e
              if ((mod_tr_dir ==  1).and.(meas_tr_dir ==  0)) sd%iperf = jc3f
              if ((mod_tr_dir ==  1).and.(meas_tr_dir == -1)) sd%iperf = jc3g
              if ((mod_tr_dir == -1).and.(meas_tr_dir ==  0)) sd%iperf = jc3h
              if ((mod_tr_dir == -1).and.(meas_tr_dir ==  1)) sd%iperf = jc3i
            end if
            if ((.not.l_amp).and.(.not.l_tim)) then
              if ((mod_tr_dir ==  0).and.(meas_tr_dir ==  0)) sd%iperf = jc4a
              if ((mod_tr_dir ==  1).and.(meas_tr_dir ==  1)) sd%iperf = jc4b
              if ((mod_tr_dir == -1).and.(meas_tr_dir == -1)) sd%iperf = jc4c
              if ((mod_tr_dir ==  0).and.(meas_tr_dir ==  1)) sd%iperf = jc4d
              if ((mod_tr_dir ==  0).and.(meas_tr_dir == -1)) sd%iperf = jc4e
              if ((mod_tr_dir ==  1).and.(meas_tr_dir ==  0)) sd%iperf = jc4f
              if ((mod_tr_dir ==  1).and.(meas_tr_dir == -1)) sd%iperf = jc4g
              if ((mod_tr_dir == -1).and.(meas_tr_dir ==  0)) sd%iperf = jc4h
              if ((mod_tr_dir == -1).and.(meas_tr_dir ==  1)) sd%iperf = jc4i
            end if
            !
            if (sd%iperf == 0) call errmsg('Error classifying.')
          end if
          if (sd%iperf == 0) then
            if (l_amp.and.l_tim) then
              sd%iperf = jc1
            end if
            if ((.not.l_amp).and.l_tim) then
              sd%iperf = jc2
            end if
            if (l_amp.and.(.not.l_tim)) then
              sd%iperf = jc3
            end if
            if ((.not.l_amp).and.(.not.l_tim)) then
              sd%iperf = jc4
            end if
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
  nsr = ns
  bb%ncol = bb%ic1 - bb%ic0 + 1; nc = bb%ncol
  bb%nrow = bb%ir1 - bb%ir0 + 1; nr = bb%nrow
  !
  ! get the maximum rho and minumum qre
  allocate(si2d(nc,nr), rho2d(nc,nr), qre2d(nc,nr), modslp2d(nc,nr), measslp2d(nc,nr))
  if (write_perf) then
    allocate(perf2d(nc,nr))
    do ir = 1, nr
      do ic = 1, nc
        perf2d(ic,ir) = 0
      end do
    end do
  end if
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
!      if (rho > rho2d(ic,ir)) then
!        si2d(ic,ir)  = i 
!        rho2d(ic,ir) = rho 
!      end if
!      if (qre < qre2d(ic,ir)) then
!        qre2d(ic,ir) = qre
!      end if
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
    if (trend_perf) then
      do i = 1, ns
        sd => sdat(i)
        if (.not.sd%act) cycle
        ic = sd%ic - bb%ic0 + 1; ir = sd%ir - bb%ir0 + 1 
        modslp2d(ic,ir) = sd%modslp
        slp = sd%measslp
        measslp2d(ic,ir) = measslp2d(ic,ir) + slp
      end do
    end if
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
          if (trend_perf) then
            measslp2d(ic,ir) = measslp2d(ic,ir)/n
          end if
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
          sd%x = gxmin + sd%ic*gcs - gcs/2.d0
          sd%y = gymax - sd%ir*gcs + gcs/2.d0
          sd%rho = rho2d(ic,ir)
          sd%qre = qre2d(ic,ir)
          sd%measslp = measslp2d(ic,ir)
          sd%modslp = modslp2d(ic,ir)
          sd%iperf = 0
          
          if (abs(sd%qre) < aqre_thres) then ! amplitude
            l_amp = .true.
          else
            l_amp = .false.
          end if
          if (sd%rho > rho_thres) then ! timing
            l_tim = .true.
          else
            l_tim = .false.
          end if
          
          if (trend_perf) then
            meas_tr_dir = 0
            if (abs(sd%measslp) > trend_thres) then ! measurement has trend
              if (sd%measslp < RZERO) then
                meas_tr_dir = -1
              else
                meas_tr_dir = 1
              end if
            end if
            mod_tr_dir = 0
            if (abs(sd%modslp) > trend_thres) then ! model has trend
              if (sd%modslp < RZERO) then
                mod_tr_dir = -1
              else
                mod_tr_dir = 1
              end if
            end if
            !
            if (l_amp .and.l_tim) then
              if ((mod_tr_dir ==  0).and.(meas_tr_dir ==  0)) sd%iperf = jc1a
              if ((mod_tr_dir ==  1).and.(meas_tr_dir ==  1)) sd%iperf = jc1b
              if ((mod_tr_dir == -1).and.(meas_tr_dir == -1)) sd%iperf = jc1c
              if ((mod_tr_dir ==  0).and.(meas_tr_dir ==  1)) sd%iperf = jc1d
              if ((mod_tr_dir ==  0).and.(meas_tr_dir == -1)) sd%iperf = jc1e
              if ((mod_tr_dir ==  1).and.(meas_tr_dir ==  0)) sd%iperf = jc1f
              if ((mod_tr_dir ==  1).and.(meas_tr_dir == -1)) sd%iperf = jc1g
              if ((mod_tr_dir == -1).and.(meas_tr_dir ==  0)) sd%iperf = jc1h
              if ((mod_tr_dir == -1).and.(meas_tr_dir ==  1)) sd%iperf = jc1i
            end if
            if ((.not.l_amp).and.l_tim) then
              if ((mod_tr_dir ==  0).and.(meas_tr_dir ==  0)) sd%iperf = jc2a
              if ((mod_tr_dir ==  1).and.(meas_tr_dir ==  1)) sd%iperf = jc2b
              if ((mod_tr_dir == -1).and.(meas_tr_dir == -1)) sd%iperf = jc2c
              if ((mod_tr_dir ==  0).and.(meas_tr_dir ==  1)) sd%iperf = jc2d
              if ((mod_tr_dir ==  0).and.(meas_tr_dir == -1)) sd%iperf = jc2e
              if ((mod_tr_dir ==  1).and.(meas_tr_dir ==  0)) sd%iperf = jc2f
              if ((mod_tr_dir ==  1).and.(meas_tr_dir == -1)) sd%iperf = jc2g
              if ((mod_tr_dir == -1).and.(meas_tr_dir ==  0)) sd%iperf = jc2h
              if ((mod_tr_dir == -1).and.(meas_tr_dir ==  1)) sd%iperf = jc2i
            end if
            if (l_amp.and.(.not.l_tim)) then
              if ((mod_tr_dir ==  0).and.(meas_tr_dir ==  0)) sd%iperf = jc3a
              if ((mod_tr_dir ==  1).and.(meas_tr_dir ==  1)) sd%iperf = jc3b
              if ((mod_tr_dir == -1).and.(meas_tr_dir == -1)) sd%iperf = jc3c
              if ((mod_tr_dir ==  0).and.(meas_tr_dir ==  1)) sd%iperf = jc3d
              if ((mod_tr_dir ==  0).and.(meas_tr_dir == -1)) sd%iperf = jc3e
              if ((mod_tr_dir ==  1).and.(meas_tr_dir ==  0)) sd%iperf = jc3f
              if ((mod_tr_dir ==  1).and.(meas_tr_dir == -1)) sd%iperf = jc3g
              if ((mod_tr_dir == -1).and.(meas_tr_dir ==  0)) sd%iperf = jc3h
              if ((mod_tr_dir == -1).and.(meas_tr_dir ==  1)) sd%iperf = jc3i
            end if
            if ((.not.l_amp).and.(.not.l_tim)) then
              if ((mod_tr_dir ==  0).and.(meas_tr_dir ==  0)) sd%iperf = jc4a
              if ((mod_tr_dir ==  1).and.(meas_tr_dir ==  1)) sd%iperf = jc4b
              if ((mod_tr_dir == -1).and.(meas_tr_dir == -1)) sd%iperf = jc4c
              if ((mod_tr_dir ==  0).and.(meas_tr_dir ==  1)) sd%iperf = jc4d
              if ((mod_tr_dir ==  0).and.(meas_tr_dir == -1)) sd%iperf = jc4e
              if ((mod_tr_dir ==  1).and.(meas_tr_dir ==  0)) sd%iperf = jc4f
              if ((mod_tr_dir ==  1).and.(meas_tr_dir == -1)) sd%iperf = jc4g
              if ((mod_tr_dir == -1).and.(meas_tr_dir ==  0)) sd%iperf = jc4h
              if ((mod_tr_dir == -1).and.(meas_tr_dir ==  1)) sd%iperf = jc4i
            end if
            !
            if (sd%iperf == 0) call errmsg('Error classifying.')
          end if
          if (sd%iperf == 0) then
            if (l_amp.and.l_tim) then
              sd%iperf = jc1
            end if
            if ((.not.l_amp).and.l_tim) then
              sd%iperf = jc2
            end if
            if (l_amp.and.(.not.l_tim)) then
              sd%iperf = jc3
            end if
            if ((.not.l_amp).and.(.not.l_tim)) then
              sd%iperf = jc4
            end if
          end if
          nperf(sd%iperf) = nperf(sd%iperf) + 1
          write(sa(1),*) (sd%ir - 1)*gnc + sd%ic
          write(sa(2),*) sd%y
          write(sa(3),*) sd%x
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
      if (write_perf) then
        ic = sd%ic - bb%ic0 + 1 !local
        ir = sd%ir - bb%ir0 + 1 !local
        perf2d(ic,ir) = sd%iperf
      end if
      if (len_trim(sd%raw) == 0) then
        call errmsg('Program error')
      end if
      write(iu,'(a)') trim(sd%raw)//ta((/sd%iperf/))
    end if
  end do
  close(iu)
  !
  if (write_perf) then
!    call writeasc(f_out_pref, perf2d, bb%ncol, bb%nrow, &
!      gxmin + (bb%ic0-1)*gcs, gymin + (gnr-bb%ir1)*gcs, gcs, 0.D0)
    call writeflt(f_out_pref, perf2d, bb%ncol, bb%nrow, &
      gxmin + (bb%ic0-1)*gcs, gymin + (gnr-bb%ir1)*gcs, gcs, 0)
  end if
  !
  np = sum(nperf)
  write(s,'(f10.2)') 100.*np/ns
  do i = 1, size(nperf)
    pperf(i) = 100.*nperf(i)/np
  end do
  call logmsg('Total # classified: '//ta((/np/))//'/'//ta((/ns/))//' ('//trim(adjustl(s))//' %)')
  if (trend_perf) then
    i0 = 5; i1 = nclass
  else
    i0 = 1; i1 = 4
  end if
  do i = i0, i1
    call logmsg(class_str(i)//': '//ta((/pperf(i)/),'(f10.1)')//'% '//ta((/nperf(i)/),'(i10)'))
  end do
  if (filt_method == i_mean) then
    call logmsg('Max count per cell : '//ta((/nmax/),'(i10)'))
  end if
  call logmsg('Avg. rho: '//ta((/rhotot/nsr/)))
  call logmsg('Avg. absolute qre: '//ta((/aqretot/nsr/)))
  
  !
end program