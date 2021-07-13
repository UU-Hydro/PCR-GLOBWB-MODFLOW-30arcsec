program mf6test
  ! -- modules
  use utilsmod, only: mxslen, i4b, r4b, logmsg, errmsg, open_file, create_dir, &
    change_work_dir, swap_slash
  use mf6test_module, only: raw, tMf6_sol, tMf6_mod, tExchange, gcs, gnlay
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
  !
  character(len=mxslen) :: out_dir, f, d, s
  integer(i4b) :: p_rd, n_rd, csexp_rd, n, nl, nr, nc, il, nmod, im, jm, ixch
  integer(i4b) :: iu, pnr, pnc, pir, pic, pjr, pjc, i, n1, n2, ist, iact
  integer(i4b) :: ir, ic, ir1, ir2, ic1, ic2
  ! ------------------------------------------------------------------------------
  n = nargs()-1
  if (n < 5) call errmsg('Invalid program arguments.')
  call getarg(1, f)
  call getarg(2, s); read(s,*) p_rd
  call getarg(3, s); read(s,*) n_rd
  call getarg(4, s); read(s,*) csexp_rd
  call getarg(5, out_dir)
  !
  ! read the model parameter keywords
  call raw%init(f)
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
  nl = raw%geti('nlay')
  nc = 2**(n_rd-1); nr = nc
  allocate(mod%ncol, mod%nrow, mod%nxch, mod%modelname, mod%rootdir, mod%bindir)
  mod%ncol = nc; mod%nrow = nr; mod%nxch = 0
  gcs = 2**csexp_rd; gnlay = nl
  pnr = p_rd; pnc = p_rd
  !
  ! set the DISU
  call mod%set_disu()
  !
  ! write the exchanges
  write(f,'(a,i2.2,a)') 's', sol%isol, '.exchanges.asc'
  call open_file(f, iu, 'w')
  im = 0
  do pir = 1, pnr
    do pic = 1, pnc
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
            jm = pjc + (pjr-1)*pnc
            if (im /= jm) then
              mod%nxch = mod%nxch + 1
              if (iact == 2) then
                xch => mod%xch(mod%nxch)
                write(xch%m2modelname,'(a,i5.5)') 'm', jm
                !
                select case(ist)
                case(p_jn)
                  ir1 = 1; ir2 = nr
                case(p_js)
                  ir1 = nr; ir2 = 1
                case(p_jw)
                  ic1 = 1; ic2 = nc
                case(p_je)
                  ic1 = nc; ic2 = 1
                end select
                !
                select case(ist)
                case(p_jn, p_js)
                  do il = 1, gnlay
                    do ic = 1, nc
                      n1 = ic + (ir1-1)*nc + (il-1)*nc*nr
                      n2 = ic + (ir2-1)*nc + (il-1)*nc*nr
                      xch%nexg = xch%nexg + 1
                      xch%cellidm1(xch%nexg) = n1
                      xch%cellidm2(xch%nexg) = n2
                    end do
                  end do
                case(p_jw, p_je)
                  do il = 1, gnlay
                    do ir = 1, nr
                      n1 = ic1 + (ir-1)*nc + (il-1)*nc*nr
                      n2 = ic2 + (ir-1)*nc + (il-1)*nc*nr
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
            allocate(xch%cellidm1(gnlay*nr))
            allocate(xch%cellidm2(gnlay*nr))
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
  do pir = 1, pnr
    do pic = 1, pnc
      im = im + 1
      mod%imod = im
      write(mod%modelname,'(a,i5.5)') 'm', im
      !
      ! create directories
      mod%rootdir = '..\..\models\run_input\'//trim(mod%modelname)//'\'
      call swap_slash(mod%rootdir)
      call create_dir(mod%rootdir,.true.)
      !
      mod%bindir = trim(mod%rootdir)
      call swap_slash(mod%bindir)
      !
      call mod%write(pir, pic, pnr, pnc)
      !
    end do
   end do
  
end program

