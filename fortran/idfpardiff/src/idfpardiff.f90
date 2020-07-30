program idfpardiff
  use imod_idf
  use utilsmod, only: writeidf
  use utils, only: chkexist, getlun
  
  implicit none
  
  integer, parameter :: mxslen = 1024
  type(idfobj) :: pidf, sidf
  character(len=mxslen) :: f, fp, s, spref, ppref, fdiff, fts, ftp, fpart, dirp, dirs, fst
  character(len=mxslen), dimension(100) :: sa
  character(len=mxslen), dimension(:), allocatable :: fidf
  character(len=1) :: cdum
  integer :: i, iu, ju, idum, iup, np, nlay, ip, gnrow, gncol, ilay, irow, icol, iper
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
  integer :: nper
  
  type tObs
    double precision :: v = 0.
    integer          :: n = 0
    integer          :: ilay = 0
    integer          :: irow = 0
    integer          :: icol = 0
  end type tObs
  type(tObs), dimension(:), allocatable :: dmin, dmax, davg
  
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
  read(iu,*) nlay, gnrow, gncol, xll, yll, cs, gnodata
  nlay = 10 !DEBUG
  read(iu,'(a)') s
  read(s,*) dirp, np
  allocate(part(np))
  if (np == 1) then
    part(1)%ncol = gncol
    part(1)%nrow = gnrow
    part(1)%nodes = 0 !dummy
    part(1)%icolmin = 1
    part(1)%icolmax = gncol
    part(1)%irowmin = 1
    part(1)%irowmax = gnrow
  else
    read(s,*) cdum, idum, fp
    call imod_utl_openasc(ju, fp, 'r')
    do ip = 1, np
      read(ju,*) cdum, cdum, part(ip)%ncol, part(ip)%nrow, part(ip)%nodes, &
        part(ip)%icolmin, part(ip)%icolmax, part(ip)%irowmin, part(ip)%irowmax
    end do
    close(ju)
  end if
  read(iu,*) dirs
  read(iu,*) nper
  allocate(fidf(nper))
  do iper = 1, nper
    read(iu,*) fidf(iper)
  end do
  read(iu,*) fdiff
  read(iu,*) fts
  read(iu,*) ftp
  read(iu,*) fst
  close(iu)
  !
  ! init
  allocate(dmin(nper), dmax(nper), davg(nper))
  do iper = 1, nper
    dmin(iper)%v = dhuge
    dmax(iper)%v = -dhuge
    davg(iper)%v = 0.d0
  end do
  rmse = 0.d0; nrmse = 0
  nwarn = 0
  do iper = 1, nper
    write(*,'(a,i3.3,a)') 'Processing stress period ', iper, '...'
    do ilay = 1, nlay
      write(s,*) ilay
      write(f,'(5a)') trim(dirs), trim(fidf(iper)), '_l', trim(adjustl(s)), '.idf'
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
        if (allocated(gx))   deallocate(gx)
        if (allocated(gxts)) deallocate(gxts)
        if (allocated(gxtp)) deallocate(gxtp)
        if (allocated(gix))  deallocate(gix)
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
!          if (sidf%x(icol,irow) /= sidf%nodata) then
          if ((sidf%x(icol,irow) /= gnodata).and.(sidf%x(icol,irow) /= gnodata2)) then
            if (gxts(icol,irow) == gnodata) then
              gxts(icol,irow) = sidf%x(icol,irow)
            end if
          end if
        end do
      end do
      
      do ip = 1, np
        if (np == 1) then
          write(f,'(5a,i3.3,a)') trim(dirp), trim(fidf(iper)), '_l', trim(adjustl(s)), '.idf'
        else
          write(f,'(5a,i3.3,a)') trim(dirp), trim(fidf(iper)), '_l', trim(adjustl(s)), '_p', ip-1, '.idf'
        end if
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
!            if (pidf%x(icol,irow) /= pidf%nodata) then
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
              if (d < dmin(iper)%v) then
                dmin(iper)%v = d
                dmin(iper)%ilay = ilay; dmin(iper)%irow = ir1; dmin(iper)%icol = ic1
              end if
              if (d > dmax(iper)%v) then
                dmax(iper)%v = d
                dmax(iper)%ilay = ilay; dmax(iper)%irow = ir1; dmax(iper)%icol = ic1
              end if
              d = abs(d)
              davg(iper)%v = davg(iper)%v + d
              davg(iper)%n = davg(iper)%n + 1
              !
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
    davg(iper)%v = davg(iper)%v/davg(iper)%n
    
    write(f,'(2a,i3.3,a)') trim(fdiff), '_', iper, '.idf'
    call writeidf(f, gx, gncol, gnrow, dble(xll), dble(yll), dble(cs), dble(0.))
    write(f,'(2a,i3.3,a)') trim(fts), '_', iper, '.idf'
    call writeidf(f, gxts, gncol, gnrow, dble(xll), dble(yll), dble(cs), dble(gnodata))
    write(f,'(2a,i3.3,a)') trim(ftp), '_', iper, '.idf'
    call writeidf(f, gxtp, gncol, gnrow, dble(xll), dble(yll), dble(cs), dble(gnodata))
    
  end do
  
  ! write the statistics
  call imod_utl_openasc(iu, fst, 'w')
  write(sa(1),'(g14.6)') sqrt(rmse/nrmse)
  write(iu,'(a)')(trim(adjustl(sa(i))),i=1,1)
  do iper = 1, nper
    write(sa( 1),'(g14.6)') dmin(iper)%v
    write(sa( 2),*) ','
    write(sa( 3),*) dmin(iper)%ilay
    write(sa( 4),*) ','
    write(sa( 5),*) dmin(iper)%irow
    write(sa( 6),*) ','
    write(sa( 7),*) dmin(iper)%icol
    write(sa( 8),*) ','
    write(sa( 9),'(g14.6)') dmax(iper)%v
    write(sa(10),*) ','
    write(sa(11),*) dmax(iper)%ilay
    write(sa(12),*) ','
    write(sa(13),*) dmax(iper)%irow
    write(sa(14),*) ','
    write(sa(15),*) dmax(iper)%icol
    write(sa(16),*) ','
    write(sa(17),'(g14.6)') davg(iper)%v
    
    write(iu,'(17a)')(trim(adjustl(sa(i))),i=1,17)
  end do
  close(iu)
  
  write(*,*) '# warnings:',nwarn
  
  !call writeidf(fdiff, gx, gncol, gnrow, dble(xll), dble(yll), dble(cs), dble(0.))
  !call writeidf(fts, gxts, gncol, gnrow, dble(xll), dble(yll), dble(cs), dble(gnodata))
  !call writeidf(ftp, gxtp, gncol, gnrow, dble(xll), dble(yll), dble(cs), dble(gnodata))
  !call writeidf(fpart, gix, gncol, gnrow, xll, yll, cs, -1.)

 end program
