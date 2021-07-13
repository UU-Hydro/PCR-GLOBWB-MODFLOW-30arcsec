program  catchcreatemetis
! ******************************************************************************
   ! -- modules
  use imod_idf_par, only: idfobj
  use imod_idf
  use metis_module, only: tMetis
  use utilsmod, only: i1b, i4b, i8b, r4b, r8b, open_file, errmsg, getdigits, &
    writeasc, writeidf, chkexist, DZERO, mxslen, readidf_block, &
    addboundary, calc_unique, tUnp, quicksort_d, errmsg, logmsg, ta

  implicit none

  !--  general parameters
  logical, parameter :: ldebug = .false.

  ! -- some METIS parameters
  logical, parameter :: pgrec    = .true.   ! multilevel recursive bisection

  ! -- locals
  type(idfobj) :: idf
  type(tMetis), pointer :: met => null()
  type(tUnp), dimension(:), allocatable :: regbb
  logical :: lrb
  character(len=mxslen) :: str, vwgtfile, partfile
  integer(i4b) :: iaddbnd,  gnparts, nparts, n, ip, gir0, gir1, gic0, gic1, gnc, gnr, nc, nr, ic, ir
  integer(i4b) :: ireg, jreg, kreg, jc, jr
  integer(i4b) :: idum, nreg, nreg_np, i, j, ir0, ir1, ic0, ic1
  integer(i4b), dimension(:), allocatable :: reg_np
  integer(i1b), dimension(:,:), pointer :: bnd
  integer(i4b), dimension(:,:), pointer :: i4wk2d
  integer(i4b), dimension(:,:), allocatable :: regun
  real(r8b), dimension(:), allocatable :: regarea
  integer(i4b), dimension(:), allocatable :: regind
  real(r8b) :: xll, yll, cs, tgtload, load
! ------------------------------------------------------------------------------
  !
  lrb = .false.
  !
  !gir0 = 10000; gir1 = 16300; gic0 = 33000; gic1 = 42000; lrb = .true. !AUS
  
  ! read command line arguments
  n = nargs()-1
  if (n < 4) then
    call errmsg('invalid number of arguments')
  end if

  ! check if the partitioning is is for a dummy block
  call getarg(1,str); read(str,*) iaddbnd
  call getarg(2,vwgtfile)
  call getarg(3,partfile)
  call getarg(4,str); read(str,*) tgtload ! target load in number of cells
  if (n > 4) then
    call getarg(5,str); read(str,*) nreg_np
    if (nreg_np > 0) then
      allocate(reg_np(nreg_np))
      do i = 1, nreg_np
        call getarg(5+i,str); read(str,*) reg_np(i)
      end do
    end if
  else
    nreg_np = 0
  end if
  !
  ! read IDF header
  if (.not.idfread(idf, vwgtfile, 0)) then
   call errmsg('Could not read '//trim(vwgtfile))
  end if
  if (.not.lrb) then
    gir0 = 1; gir1 = idf%nrow; gic0 = 1; gic1 = idf%ncol
  end if
  call readidf_block(idf, gir0, gir1, gic0, gic1, i4wk2d)
  gnc = size(i4wk2d,1); gnr = size(i4wk2d,2)
  cs = idf%dx; xll = idf%xmin + (gic0 - 1)*cs; yll = idf%ymin + (gir0 - 1)*cs
  !
  ! set uniform weights; init boundary flag
  do ir = 1, gnr
    do ic = 1, gnc
      if (i4wk2d(ic,ir) /= 0) i4wk2d(ic,ir) = 1
    end do
  end do
  !
  ! add the bounday
  if (iaddbnd == 1) then
    call logmsg('Adding boundary')
    call addboundary(i4wk2d, gnc, gnr)
  end if
  !
  ! store boundary position
  allocate(bnd(gnc,gnr))
  do ir = 1, gnr
    do ic = 1, gnc
      if (i4wk2d(ic,ir) < 0) then
        bnd(ic,ir) = 1
      else
        bnd(ic,ir) = 0
      end if
    end do
  end do
  !
  call calc_unique(i4wk2d, 9, regun, regbb, nreg, idum, 0., 0., 0.)
  !
  allocate(regarea(nreg), regind(nreg))
  do i = 1, nreg
    regind(i) = i
    regarea(i) = -real(regbb(i)%n,r8b)
  end do
  call quicksort_d(regarea, regind, nreg)
  do i = 1, nreg
    regarea(i) = abs(regarea(i))
  end do
  !
  kreg = 0
  gnparts = 1
  do ireg = 1, nreg
    jreg = regind(ireg)
    ir0 = regbb(jreg)%ir0; ir1 = regbb(jreg)%ir1
    ic0 = regbb(jreg)%ic0; ic1 = regbb(jreg)%ic1
    nr = ir1 - ir0 + 1; nc = ic1 - ic0 + 1
    !
    kreg = kreg + 1
    if (kreg <= nreg_np) then
      nparts = reg_np(kreg)
      if (nparts < 0) then
        nparts = -nparts
        nreg_np = 0
        tgtload = regbb(jreg)%n/nparts
        call logmsg('**** Resetting target load to '//ta((/tgtload/))// &
          ' for this and next regions! ****')
      end if
    else
      nparts = nint(real(regbb(jreg)%n,r8b) / tgtload )
      nparts = max(1,nparts)
    end if
    load = regbb(jreg)%n/nparts
    !
    if (nparts == 1) then
      do ir = ir0, ir1
        do ic = ic0, ic1
          jr = ir - ir0 + 1; jc = ic - ic0 + 1
          if (regun(ic,ir) == jreg) then
            regun(ic,ir) = -gnparts
          end if
        end do
      end do
    elseif (nparts > 1) then ! METIS
      call logmsg('Region '//ta((/ireg/))//' (size '//ta((/regbb(jreg)%n/))//'): realized load '// &
        ta((/int(load)/))//' for target load '//ta((/int(tgtload)/)))
      if (associated(i4wk2d)) deallocate(i4wk2d); i4wk2d => null()
      allocate(i4wk2d(nc,nr))
      do ir = ir0, ir1
        do ic = ic0, ic1
          jr = ir - ir0 + 1; jc = ic - ic0 + 1
          if (regun(ic,ir) == jreg) then
            i4wk2d(jc,jr) = 1 ! uniform weight
          else
            i4wk2d(jc,jr) = 0
          end if
        end do
      end do
      !
      allocate(met)
      call met%init(i4wk2d, nparts)
      call met%set_opts()
      if (met%nparts > 1) then
        if (pgrec) then
          call met%recur()
        else
          call met%kway()
       end if
      end if
      !
      ! set the partitions
      n = 0
      do ir = ir0, ir1
        do ic = ic0, ic1
          if (regun(ic,ir) == jreg) then
            n = n + 1
            ip = met%part(n) + 1 + gnparts
            regun(ic,ir) = -ip
          end if
        end do
      end do
      !
      ! clean-up
      call met%clean()
      deallocate(met)
      !
    else
      call errmsg(""
    end if
    gnparts = gnparts + nparts
    !
  end do
  !
  write(*,*) 'Total # partitions: '//ta((/gnparts/))
  !
  ! filter for the bnd
  do ir = 1, gnr
    do ic = 1, gnc
      if (bnd(ic,ir) == 1) then
        regun(ic,ir) = 0
      else
        regun(ic,ir) = -regun(ic,ir)
      end if
    enddo
  end do
  !
  if (index(partfile,'.asc',back=.true.) > 0) then
    call writeasc(trim(partfile), regun, gnc, gnr, xll, yll, cs, DZERO)
  else
    call writeidf(trim(partfile), regun, gnc, gnr, xll, yll, cs, DZERO)
  end if
  !

end program