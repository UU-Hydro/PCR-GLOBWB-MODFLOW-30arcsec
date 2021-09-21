program mf6test
  ! -- modules
  use utilsmod, only: mxslen, i1b, i2b, i4b, i8b, r4b, r8b, logmsg, errmsg, open_file, create_dir, &
    change_work_dir, swap_slash, RZERO, DZERO, ta, tBb
  use ehdrModule, only: writeflt
  use mf6test_module, only: raw, tMf6_sol, tMf6_mod, tExchange, tReg, gcs, gnlay
  !
  implicit none
  !
  ! -- local
  integer(i4b), parameter :: p_jn = 1
  integer(i4b), parameter :: p_js = 2
  integer(i4b), parameter :: p_jw = 3
  integer(i4b), parameter :: p_je = 4
  integer(i4b), parameter :: p_ns = p_je
  !
  integer(i4b), dimension(2,p_ns) :: pst
  data pst/0,-1,0,1,-1,0,1,0/
  !
  type(tMf6_sol), pointer :: sol => null()
  type(tMf6_mod), pointer :: mod => null()
  type(tExchange), pointer :: xch => null()
  type(tBb), pointer :: bb => null()
  type(tReg), pointer :: reg => null()
  !
  ! work arrays
  real(r4b), dimension(:), allocatable :: r4wk1d
  real(r4b), dimension(:,:), allocatable :: r4wk2d, r4wk2d2
  real(r8b), dimension(:), allocatable :: r8wk1d
  !
  character(len=mxslen) :: out_dir, f, fp, d, s
  integer(i4b) :: p_rd, n_rd, nbs_rd, csexp_rd, n, nl, gnr, gnc, lnr, lnc, lnodes, nmod, im, jm, ixch
  integer(i4b) :: iu, pir, pic, pjr, pjc, i, n1, n2, ist, iact, iopt, ios
  integer(i4b) :: il, jl, ir, ic, jr, jc, bnexp, bcsexp, bs, step
  integer(i4b) :: ic0, ic1, ic2, ir0, ir1, ir2
  real(r4b) :: r4xll, r4yll, r4cs, r4nodata
  real(r8b) :: r8xll, r8yll
  !
  real(r4b) :: drn_width, drn_cond
  real(r4b) :: kmin, kmax, kmean, k2sigma, kmin_rnd, kmax_rnd
  real(r4b) :: chd_head_w, chd_head_e, chd_head_n, chd_head_s
  integer(i4b) :: nwel
  real(r4b) :: qmin, qmax, qmean, q2sigma
  !
  ! mf6 header:
  character(len=16) :: text_in ! ulasav
  integer(i4b) :: kstp_in, kper_in, ncol_in, nrow_in, ilay_in
  real(r8b) :: pertim_in, totim_in
  !
  ! ------------------------------------------------------------------------------
  n = nargs()-1
  if (n < 5) call errmsg('Invalid program arguments.')
  call getarg(1, f)
  call getarg(2, s); read(s,*) p_rd
  call getarg(3, s); read(s,*) n_rd
  call getarg(4, s); read(s,*) csexp_rd
  call getarg(5, out_dir)
  if (n >= 6) then
    call getarg(6, s); read(s,*) iopt
  else
    iopt = 0
  end if
  !
  ! read the model parameter keywords
  call raw%init(f)
  !
  nl = raw%geti('nlay')
  gnc = p_rd*2**(n_rd-1); gnr = gnc; lnr = gnr/p_rd; lnc = lnr
  gcs = 2**csexp_rd; gnlay = nl
  lnodes = lnr*lnc*gnlay
  !
  ! base grid
  bnexp = raw%geti('base_n_exp')
  bcsexp = raw%geti('base_cs_exp')
  !
  select case(iopt)
  case(1) ! raster input
    if (n == 7) then
      call getarg(7, s); read(s,*) nbs_rd
      bs = 2**(nbs_rd-1) ! block = bs * bs
    else
      bs = 2**(n_rd-1) ! block = bs * bs
    end if
    !gnc = 2**(bnexp-1); gnr = gnc
    allocate(r4wk2d(gnc,gnr))
    r4xll = RZERO; r4yll = RZERO; r4nodata = RZERO; r4cs = real(2**bcsexp,r4b)
    do ir = 1, gnr
      do ic = 1, gnr
        r4wk2d(ic,ir) = r4nodata
      end do
    end do
    !----- DRN -----
    drn_width = 8**0
    do ir = bs/2, gnr-bs/2, bs
      do ic = 1, gnc
        r4wk2d(ic,ir)   = r4wk2d(ic,ir)   + r4cs*drn_width/2.
        r4wk2d(ic,ir+1) = r4wk2d(ic,ir+1) + r4cs*drn_width/2.
      end do
    end do
    do ir = 1, gnr
      do ic = bs/2, gnr-bs/2, bs
        r4wk2d(ic,ir)   = r4wk2d(ic,ir)   + r4cs*drn_width/2.
        r4wk2d(ic+1,ir) = r4wk2d(ic+1,ir) + r4cs*drn_width/2.
      end do
    end do
    ! correct for corners
    do ir = bs/2, gnr-bs/2, bs
      do ic = bs/2, gnr-bs/2, bs
        r4wk2d(ic,ir)     = r4wk2d(ic,ir)     - (drn_width/2.)**2
        r4wk2d(ic+1,ir)   = r4wk2d(ic+1,ir)   - (drn_width/2.)**2
        r4wk2d(ic,ir+1)   = r4wk2d(ic,ir+1)   - (drn_width/2.)**2
        r4wk2d(ic+1,ir+1) = r4wk2d(ic+1,ir+1) - (drn_width/2.)**2
      end do
    end do
    f = 'drn_cond'
    call writeflt(f, r4wk2d, gnc, gnr, r4xll, r4yll, r4cs, r4nodata, &
      hdrKeys=(/'DLT_USCLTYPE SUMCDR','DLT_DSCLTYPE NOINTP'/))
    !
    !----- K -----
    kmin = 5.  !  1 = fijn zand
    kmax = 30. ! 30 = grof zand
    k2sigma = (kmax-kmin)/2.; kmean = kmin + k2sigma
    !call random_seed()
    call random_number(r4wk2d)
    kmin_rnd = huge(kmin_rnd); kmax_rnd = -huge(kmax_rnd)
    do ir = 1, gnr
      do ic = 1, gnc
       r4wk2d(ic,ir) = kmean + k2sigma*2.*(r4wk2d(ic,ir)-0.5) ! m + s * 2*(r - 0.5)
       kmin_rnd = min(kmin_rnd,r4wk2d(ic,ir))
       kmax_rnd = max(kmax_rnd,r4wk2d(ic,ir))
      end do
    end do
    call logmsg('Min/max k-values: '//ta((/kmin_rnd, kmax_rnd/)))
    f = 'k'
    call writeflt(f, r4wk2d, gnc, gnr, r4xll, r4yll, r4cs, r4nodata, &
      hdrKeys=(/'DLT_USCLTYPE GEOM','DLT_DSCLTYPE NOINTP'/))
    !
    !----- CHD -----
    r4nodata = -9999.
    chd_head_w = 10.; chd_head_e = 0.; chd_head_n = r4nodata;  chd_head_s = r4nodata
    do ir = 1, gnr
      do ic = 1, gnc
        r4wk2d(ic,ir) = r4nodata
      end do
    end do
    do ir = 1, gnr
      if (r4wk2d(  1,ir) == r4nodata) r4wk2d(  1,ir) = chd_head_w
      if (r4wk2d(gnc,ir) == r4nodata) r4wk2d(gnc,ir) = chd_head_e
    end do
    do ic = 1, gnc
      if (r4wk2d(ic,  1) == r4nodata) r4wk2d(ic,  1) = chd_head_n
      if (r4wk2d(ic,gnr) == r4nodata) r4wk2d(ic,gnr) = chd_head_s
    end do
    f = 'chd_head'
    call writeflt(f, r4wk2d, gnc, gnr, r4xll, r4yll, r4cs, r4nodata, &
      hdrKeys=(/'DLT_USCLTYPE ARITH','DLT_DSCLTYPE NOINTP'/))
    !
    !----- WEL -----
    nwel = 60000; qmin = 5000.; qmax = 5000.
    q2sigma = (qmax-qmin)/2.; qmean = qmin + q2sigma
    do ir = 1, gnr
      do ic = 1, gnc
        r4wk2d(ic,ir) = r4nodata
      end do
    end do
    !
    allocate(r4wk2d2(3,nwel)) ! x, y, q
    !call random_seed()
    call random_number(r4wk2d2)
    do i = 1, nwel
      ic = int(ceiling(gnc*r4wk2d2(1,i)))
      ir = int(ceiling(gnr*r4wk2d2(2,i)))
      r4wk2d(ic,ir) = -(qmean +q2sigma*2.*(r4wk2d2(3,i)-0.5))
    end do
    f = 'wel_q'
    call writeflt(f, r4wk2d, gnc, gnr, r4xll, r4yll, r4cs, r4nodata, &
      hdrKeys=(/'DLT_USCLTYPE SUMCDR','DLT_DSCLTYPE NOINTP'/))
    stop
  case(2)
    if (n == 7) then
      call getarg(7, fp)
    else
      fp = 'head'
    end if
    d = trim(out_dir)//'models\run_output_bin'
    allocate(r8wk1d(lnodes), r4wk2d(gnc,gnr))
    do il = 1, gnlay
      im = 0
      do pir = 1, p_rd
        do pic = 1, p_rd
          im = im + 1
          ic0 = (pic-1)*lnc + 1; ic1 = ic0 + lnc - 1
          ir0 = (pir-1)*lnr + 1; ir1 = ir0 + lnr - 1
          write(f,'(2a,i5.5,a)') trim(d), '\m', im, '.ss.hds'
          call open_file(f, iu, 'r', .true.)
          read(unit=iu) kstp_in, kper_in, pertim_in, totim_in, &
            text_in, ncol_in, nrow_in, ilay_in
          read(unit=iu,iostat=ios)(r8wk1d(i),i=1,lnodes)
          n = 0
          do jl = 1, gnlay
            if (jl == il) then
              do ir = 1, lnr
                do ic = 1, lnc
                  jc = ic0 + ic - 1; jr = ir0 + ir - 1
                  n = n + 1
                  r4wk2d(jc,jr) = real(r8wk1d(n),r4b)
                end do
              end do
            else
              n = n + lnr*lnc
            end if
          end do
          close(iu)
        end do
      end do 
      write(f,'(a,i2.2)') trim(fp)//'_l', il
      r8xll = DZERO; r8yll = DZERO
      call writeflt(f, r4wk2d, gnc, gnr, r8xll, r8yll, gcs, RZERO)
    end do
    deallocate(r8wk1d, r4wk2d)
    stop
  end select
  !
  call raw%init_file()
  !
  ! create output directories
  call create_dir(out_dir, .true.)
  d = trim(out_dir)//'log'; call create_dir(d,.true.)
  d = trim(out_dir)//'models'; call create_dir(d,.true.)
  d = trim(out_dir)//'models\run_output_lst'; call create_dir(d,.true.)
  d = trim(out_dir)//'models\run_output_bin'; call create_dir(d,.true.)
  d = trim(out_dir)//'solutions\run_output'; call create_dir(d,.true.)
  d = trim(out_dir)//'solutions\post_mappings'; call create_dir(d,.true.)
  !
  ! change the cwd to directory with mfsim nam files.
  d = trim(out_dir)//'solutions\run_input'
  call create_dir(d,.true.); call change_work_dir(d)
  !  
  nmod = p_rd**2
  allocate(sol); sol%isol = 1
  allocate(sol%solname); write(sol%solname,'(a,i2.2)') 's', sol%isol
  allocate(sol%nmod); sol%nmod = nmod
  allocate(sol%npart); sol%npart = nmod
  allocate(sol%mod_id(nmod), sol%mod_part(nmod))
  do im = 1, nmod
    sol%mod_id(im) = im
    sol%mod_part(im) = im
  end do
  !
  ! fill for one submodel
  allocate(mod)
  allocate(mod%ncol, mod%nrow, mod%nxch, mod%modelname, mod%rootdir, mod%bindir, mod%nreg, mod%bb)
  mod%ncol = lnc; mod%nrow = lnr; mod%nxch = 0; mod%nreg = 1
  allocate(mod%reg(1)); allocate(mod%reg(1)%bb)
  !
  ! set the DISU
  call mod%set_disu()
  !
  ! write the exchanges
  write(f,'(a,i2.2,a)') 's', sol%isol, '.exchanges.asc'
  call open_file(f, iu, 'w')
  im = 0
  do pir = 1, p_rd
    do pic = 1, p_rd
      im = im + 1
      mod%imod = im
      write(mod%modelname,'(a,i5.5)') 'm', im
      !
      do iact = 1, 2
        mod%nxch = 0
        do ist = 1, p_ns
          pjc = pic+pst(1,ist); pjr = pir+pst(2,ist)
          if ((pjc >= 1).and.(pjc <= p_rd).and.&
              (pjr >= 1).and.(pjr <= p_rd)) then
            jm = pjc + (pjr-1)*p_rd
            if (im /= jm) then
              mod%nxch = mod%nxch + 1
              if (iact == 2) then
                xch => mod%xch(mod%nxch)
                write(xch%m2modelname,'(a,i5.5)') 'm', jm
                !
                select case(ist)
                case(p_jn)
                  ir1 = 1; ir2 = lnr
                case(p_js)
                  ir1 = lnr; ir2 = 1
                case(p_jw)
                  ic1 = 1; ic2 = lnc
                case(p_je)
                  ic1 = lnc; ic2 = 1
                end select
                !
                select case(ist)
                case(p_jn, p_js)
                  do il = 1, gnlay
                    do ic = 1, lnc
                      n1 = ic + (ir1-1)*lnc + (il-1)*lnc*lnr
                      n2 = ic + (ir2-1)*lnc + (il-1)*lnc*lnr
                      xch%nexg = xch%nexg + 1
                      xch%cellidm1(xch%nexg) = n1
                      xch%cellidm2(xch%nexg) = n2
                    end do
                  end do
                case(p_jw, p_je)
                  do il = 1, gnlay
                    do ir = 1, lnr
                      n1 = ic1 + (ir-1)*lnc + (il-1)*lnc*lnr
                      n2 = ic2 + (ir-1)*lnc + (il-1)*lnc*lnr
                      xch%nexg = xch%nexg + 1
                      xch%cellidm1(xch%nexg) = n1
                      xch%cellidm2(xch%nexg) = n2
                    end do
                  end do
                end select
              end if
            end if
          end if
        end do
        if (iact == 1) then
          if (associated(mod%xch)) then
            do ixch = 1, size(mod%xch)
              xch => mod%xch(ixch)
              if (associated(xch%nexg)) deallocate(xch%nexg)
              if (associated(xch%m2modelname)) deallocate(xch%m2modelname)
              if (associated(xch%cellidm1)) deallocate(xch%cellidm1)
              if (associated(xch%cellidm2)) deallocate(xch%cellidm2)
            end do
            deallocate(mod%xch)
          end if
          allocate(mod%xch(mod%nxch))
          do ixch = 1, mod%nxch
            xch => mod%xch(ixch)
            allocate(xch%nexg); xch%nexg = 0
            allocate(xch%m2modelname)
            allocate(xch%cellidm1(gnlay*mod%ncol))
            allocate(xch%cellidm2(gnlay*mod%ncol))
          end do
        end if
      end do
      !
      ! write the exchanges
      call mod%write_exchanges(iu)
    end do
  end do
  close(iu)
  !
  ! write the solution
  call sol%write()
  call sol%clean()
  !
  ! write the models
  im = 0
  do pir = 1, p_rd
    do pic = 1, p_rd
      im = im + 1
      mod%imod = im
      write(mod%modelname,'(a,i5.5)') 'm', im
      !
      reg => mod%reg(1); bb => reg%bb
      bb%ic0 = (pic-1)*mod%ncol + 1; bb%ic1 = bb%ic0 + mod%ncol - 1
      bb%ir0 = (pir-1)*mod%nrow + 1; bb%ir1 = bb%ir0 + mod%nrow - 1
      bb%ncol = mod%ncol; bb%nrow = mod%nrow
      !
      mod%bb%ic0 =  bb%ic0; mod%bb%ic1 =  bb%ic1
      mod%bb%ir0 =  bb%ir0; mod%bb%ir1 =  bb%ir1
      mod%bb%ncol = bb%ncol; mod%bb%nrow = bb%nrow
      if (im == 1) then
        allocate(reg%nodmap(mod%ncol, mod%nrow, gnlay), reg%layer_nodes(gnlay))
        reg%layer_nodes = 0
        n = 0
        do il = 1, gnlay
          reg%layer_nodes(il) = mod%nrow*mod%ncol
          do ir = 1, mod%nrow
            do ic = 1, mod%ncol
              n = n + 1
              reg%nodmap(ic,ir,il) = n
            end do
          end do
        end do
      end if
      !
      ! create directories
      mod%rootdir = '..\..\models\run_input\'//trim(mod%modelname)//'\'
      call swap_slash(mod%rootdir)
      call create_dir(mod%rootdir,.true.)
      !
      mod%bindir = trim(mod%rootdir)
      call swap_slash(mod%bindir)
      !
      call mod%write()
      call mod%write_post_map()
      !
    end do
   end do
  
end program

