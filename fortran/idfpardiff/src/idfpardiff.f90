program idfpardiff
  use imod_idf
  use utilsmod, only: writeidf
  use utils, only: chkexist, getlun
  
  implicit none
  
  integer, parameter :: mxslen = 1024
  type(idfobj) :: pidf, sidf
  character(len=mxslen) :: f, fp, s, spref, ppref, fdiff, fts, ftp, fpart
  character(len=1) :: cdum
  integer :: iu, iup, np, nlay, ip, gnrow, gncol, ilay, irow, icol
  integer :: ic1, ic2, ir1, ir2, nwarn
  integer, dimension(:,:), allocatable :: gix
  double precision, parameter :: pbnd = 500.d0
  real :: xll, yll, cs, gnodata
  real, parameter :: gnodata2 = -999.
  double precision :: sv, pv, d
  real, dimension(:,:), allocatable :: gx, gxts, gxtp
  real :: dhuge = huge(1.)
  double precision :: rmse
  integer :: nrmse
  
  type tObs
    double precision :: v = 0.
    integer          :: ilay = 0
    integer          :: irow = 0
    integer          :: icol = 0
  end type tObs
  type(tObs) :: dmin, dmax
  
  type tPart
    integer :: ncol = 0
    integer :: nrow = 0
    integer :: nodes = 0
    integer :: icolmin = 0
    integer :: icolmax = 0
    integer :: irowmin = 0
    integer :: irowmax = 0
  end type tPart
  type(tPart), dimension(:), allocatable :: part
  
  ! read idf
  call getarg(1, f)
  call chkexist(f)
  call imod_utl_openasc(iu, f, 'r')
  read(iu,*) np, nlay, gnrow, gncol, xll, yll, cs, gnodata
  read(iu,*) fp
  read(iu,*) ppref
  read(iu,*) spref
  read(iu,*) fdiff
  read(iu,*) fts
  read(iu,*) ftp
  read(iu,*) fpart
  close(iu)
  !
  ! read the partitions
  allocate(part(np))
  call imod_utl_openasc(iu, fp, 'r')
  do ip = 1, np
    read(iu,*) cdum, cdum, part(ip)%ncol, part(ip)%nrow, part(ip)%nodes, &
      part(ip)%icolmin, part(ip)%icolmax, part(ip)%irowmin, part(ip)%irowmax
  end do
  close(iu)
  
  dmin%v = dhuge; dmax%v = -dhuge
  rmse = 0.d0; nrmse = 0
  nwarn = 0
  do ilay = 1, nlay
    write(s,*) ilay
    write(f,'(4a)') trim(spref), '_l', trim(adjustl(s)), '.idf'
    call chkexist(f); call imod_utl_printtext('Reading '//trim(f),0)
    if (.not.idfread(sidf,f,1)) then
      call imod_utl_printtext('Could not read '//trim(f),2)
    end if
    if (sidf%ncol /= gncol) then
      call imod_utl_printtext('Invalid number of columns serial IDF',2)
    end if
    if (sidf%nrow /= gnrow) then
      call imod_utl_printtext('Invalid number of columns serial IDF',2)
    end if
    !
    if (ilay == 1) then
      allocate(gx(gncol,gnrow), gxts(gncol,gnrow), gxtp(gncol,gnrow), gix(gncol,gnrow))
      !gnodata = pidf%nodata
      do irow = 1, gnrow
        do icol = 1, gncol
          gx(icol,irow) = 0.
          gxts(icol,irow) = gnodata
          gxtp(icol,irow) = gnodata
          gix(icol,irow) = -1
        end do
      end do
    end if
    
    do irow = 1, sidf%nrow
      do icol = 1, sidf%ncol
!        if (sidf%x(icol,irow) /= sidf%nodata) then
        if ((sidf%x(icol,irow) /= gnodata).and.(sidf%x(icol,irow) /= gnodata2)) then
          if (gxts(icol,irow) == gnodata) then
            gxts(icol,irow) = sidf%x(icol,irow)
          end if
        end if
      end do
    end do
    
    do ip = 1, np
      write(f,'(4a,i3.3,a)') trim(ppref), '_l', trim(adjustl(s)), '_p', ip-1, '.idf'
      call chkexist(f); !call imod_utl_printtext('Reading '//trim(f),0)
      if (.not.idfread(pidf,f,1)) then
        call imod_utl_printtext('Could not read '//trim(f),2)
      end if
      ! checks
      if (pidf%ncol /= part(ip)%ncol) then
        call imod_utl_printtext('Invalid number of columns parallel IDF',2)
      end if
      if (pidf%nrow /= part(ip)%nrow) then
        call imod_utl_printtext('Invalid number of columns parallel IDF',2)
      end if
      ! fill
      do irow = 1, pidf%nrow
        ir1 = irow + part(ip)%irowmin - 1
        do icol = 1, pidf%ncol
          ic1 = icol + part(ip)%icolmin - 1
!          if (pidf%x(icol,irow) /= pidf%nodata) then
          if ((pidf%x(icol,irow) /= gnodata).and.(pidf%x(icol,irow) /= gnodata2)) then
            sv = dble(sidf%x(ic1,ir1)); pv = dble(pidf%x(icol,irow))
            if ((abs(sv) > pbnd) .or. (abs(pv) > pbnd)) then
              nwarn = nwarn + 1
              !write(*,*) 'Warning! sv, pv =', sv, pv
              !write(*,'(a,3(i4.4,a))') '(icol,irow,ilay)=(',ic1,',',ir1,',',ilay,')'
              !pause
            end if
            d = sv - pv !serial - parallel
            rmse = rmse + d**2; nrmse = nrmse + 1
            if (d < dmin%v) then
              dmin%v = d
              dmin%ilay = ilay; dmin%irow = ir1; dmin%icol = ic1
            end if
            if (d > dmax%v) then
              dmax%v = d
              dmax%ilay = ilay; dmax%irow = ir1; dmax%icol = ic1
            end if
            d = abs(d)
            gx(ic1,ir1) = max(gx(ic1,ir1), d)
            !
            ! set top
            if (gxtp(ic1,ir1) == gnodata) then
              gxtp(ic1,ir1) = pv
            end if
            if (gix(ic1,ir1) == -1) then
              gix(ic1,ir1) = ip-1
            end if
          end if
        end do
      end do
      call idfdeallocatex(pidf)
    end do
    call idfdeallocatex(sidf)
  end do
  
  write(s,*) dmin%v
  write(*,'(a,3(i4.4,a))') 'Min. serial - parallel: '//trim(adjustl(s))//' (icol,irow,ilay)=(',dmin%icol,',',dmin%irow,',',dmin%ilay,')'
  write(s,*) dmax%v
  write(*,'(a,3(i4.4,a))') 'Max. serial - parallel: '//trim(adjustl(s))//' (icol,irow,ilay)=(',dmax%icol,',',dmax%irow,',',dmax%ilay,')'
  write(*,*) 'RMSE =',sqrt(rmse/nrmse)
  write(*,*) '# warnings:',nwarn
  
  call writeidf(fdiff, gx, gncol, gnrow, dble(xll), dble(yll), dble(cs), dble(0.))
  call writeidf(fts, gxts, gncol, gnrow, dble(xll), dble(yll), dble(cs), dble(gnodata))
  call writeidf(ftp, gxtp, gncol, gnrow, dble(xll), dble(yll), dble(cs), dble(gnodata))
  call writeidf(fpart, gix, gncol, gnrow, xll, yll, cs, -1.)

 end program
