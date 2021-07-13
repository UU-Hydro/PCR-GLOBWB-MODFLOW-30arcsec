program merit
  ! modules
  use pcrModule
  use utilsmod, only: open_file, getdigits, writeasc, readflt, writeflt, writeidf, &
    calc_unique, ta, errmsg, logmsg, i1b, r4b, r8b, DZERO, tUnp, fileexist, fillgap

  implicit none
  
  type(tMap), pointer :: map
  type(tUnp), dimension(:), allocatable :: regbb
  
  logical :: ok, skip, first
  character(len=1) :: slon, slat
  character(len=7) :: tile
  character(len=1024) :: s, f, fp, fpoint, lumpout
  integer(i4b), parameter :: gncol = 72, gnrow = 36
  
  integer(i1b) :: i1nodata_r
  integer(i1b), dimension(:,:), allocatable :: i1wrk
  integer(i4b) :: iopt, ncol, nrow, iu, ios, ntile, ir, ic, nt, nn, ilat, ilon, jlat, jlon, nrc, i4nodata
  integer(i4b) :: gic, gir, nreg, idum, jr, jc, kr, kc, lr, lc, ic0, ic1, ir0, ir1, np, mp, iact, ip, i4v
  integer(i4b), dimension(:), allocatable :: ixy
  integer(i4b), dimension(:,:), allocatable :: lumpcnt, lumpcnt_fin, i4wrk, regun, regunw
  
  integer(i8b) :: n
  real(r4b) :: my_r4nodata = -9999.
  real(r4b) :: r4nodata, dmin, dmax, r4nodata_r
  real(r4b), dimension(:,:), allocatable :: r4wrk, cat
  real(r8b) :: xul, yul, xmin, xmax, ymin, ymax, cs, x, y, x0, x1, y0, y1
  real(r8b), dimension(:,:), allocatable :: xy

  integer(i4b) :: ncol_r, nrow_r, i4nodata_r
  real(r8b) :: xmin_r, ymin_r, cs_r
! ------------------------------------------------------------------------------
  
  call getarg(1,s); read(s,*) iopt
  call logmsg('***** option: '//ta((/iopt/))//' *****')
  
  call getarg(2,f)
  call getarg(3,fp)
  call getarg(4,lumpout)
  if (iopt == 3) then
    call getarg(5,fpoint)
    call open_file(fpoint, iu, 'r')
    do iact = 1, 2
      read(unit=iu,iostat=ios,fmt='(a)') !header
      np = 0
      do while(.true.)
        read(unit=iu,iostat=ios,fmt=*) x, y
        if (ios /=0) exit
        np = np + 1
        if (iact == 2) then
          xy(1,np) = x; xy(2,np) = y
        end if
      end do
      if (iact == 1) then
        allocate(xy(2,np),ixy(np))
        rewind(iu)
      else
        close(iu)
      end if
    end do
  end if
  
  call open_file(f, iu, 'r')
  read(unit=iu,iostat=ios,fmt='(a)') !header
  ntile = 0; n = 0
  do while(.true.)
    read(unit=iu,iostat=ios,fmt='(a)') f
    if (ios /=0) exit
    ntile = ntile + 1
  end do
  rewind(iu)
  read(unit=iu,iostat=ios,fmt='(a)') !header
  
  allocate(lumpcnt(72,36))
  if (iopt >= 2) then
    call readflt(lumpout, lumpcnt, ncol_r, nrow_r, xmin_r, ymin_r, cs_r, i4nodata_r)
  else
    do ir = 1, gnrow
      do ic = 1, gncol
        lumpcnt(ic,ir) = 0
      end do
    end do
  end if
  !
  if (iopt == 3) then
    allocate(lumpcnt_fin(72,36))
    do ir = 1, gnrow
      do ic = 1, gncol
        lumpcnt_fin(ic,ir) = 0
      end do
    end do
  end if
  !
  n = 0; nt = 0
  do while(.true.)
    read(unit=iu,iostat=ios,fmt='(a)') s
    if (ios /=0) exit

    !if (trim(s) /= 'n10w005') cycle
    
    !
    nt = nt + 1
    call logmsg('Processing '//ta((/nt/))//'/'//ta((/ntile/))//'...')
    !
    ! parse the tile name
    read(s(2:3),*) ilat
    read(s(5:7),*) ilon
    !
    
    if (s(1:1) == 'n') then
     y0 = real(ilat,r8b); y1 = y0 + 5.d0
     gir = (90-ilat)/5
    else
     y0 = -real(ilat,r8b); y1 = y0 + 5.d0
     gir = 18 + ilat/5
    end if
    if (s(4:4) == 'e') then
     x0 = real(ilon,r8b); x1 = x0 + 5.d0
     gic = 36 + ilon/5 + 1
    else
     x0 = -real(ilon,r8b); x1 = x0 + 5.d0
     gic = 36 - ilon/5 + 1
    end if
    !
    if (iopt == 1) then
      f = trim(s)//trim(fp)//trim('.map')
      allocate(map)
      ! read the map file
      ok = map%init(f)
      cs  = map%header%cellsizex
      xul = map%header%xul
      yul = map%header%yul
      ncol = map%header%nrcols
      nrow = map%header%nrrows
      !
      call map%read_data()
      call map%get_r4ar(r4wrk, r4nodata, dmin, dmax)
      
      do ir = 1, nrow
        do ic = 1, ncol
          if (r4wrk(ic,ir) == r4nodata) then
            r4wrk(ic,ir) = my_r4nodata
          end if
        end do
      end do
      call map%close()
      call map%clean()
      deallocate(map)
      
      nn = 0
      do ir = 1, nrow
        do ic = 1, ncol
          if (r4wrk(ic,ir) /= my_r4nodata) then
            n = n + 1
            nn = nn + 1
          end if
        end do
      end do
      lumpcnt(gic,gir) = nn
    else if (iopt == 2) then
      ncol = 6000; nrow = 6000
      ! check if unique values need to be computed
      if (lumpcnt(gic,gir) == ncol*nrow) cycle
      call logmsg('Number of cells found for '//trim(s)//': '//ta((/lumpcnt(gic,gir)/)))
      ! read P
      ! read the map file
      f = trim(s)//trim(fp)//trim('.map')
      allocate(map); ok = map%init(f)
      cs  = map%header%cellsizex; xul = map%header%xul; yul = map%header%yul
      ncol = map%header%nrcols; nrow = map%header%nrrows
      call map%close(); call map%clean(); deallocate(map)
      if(.not.allocated(i4wrk)) allocate(i4wrk(3*ncol,3*nrow))
      do ir = 1, 3*nrow
        do ic = 1, 3*ncol
          i4wrk(ic,ir) = 1
        end do
      end do
      !
      do jr = -1, 1
        do jc = -1, 1
          kr = gir + jr; kc = gic + jc
          if ((kr >= 1).and.(kr <= 36).and.(kc >= 1).and.(kc <= 72)) then
            if (lumpcnt(kc,kr) < ncol*nrow) then
              if (kr <= 18) then !n
                slat = 'n'
                !kr = (90-jlat)/5
                jlat = 90-5*kr
              else !s
                slat = 's'
                !kr = 18 + jlat/5
                jlat = 5*(kr - 18)
              end if
              if (kc <= 36) then !w
                slon  = 'w'
                !kc = 36 - jlon/5 + 1
                jlon = 5*(36 + 1 - kc)
              else !e
                slon = 'e'
                !kc = 36 + jlon/5 + 1
                jlon = 5*(kc - 36- 1)
              end if
              ! create file name
              write(tile,'(a,i2.2,a,i3.3)') slat, jlat, slon, jlon
              f = tile//trim(fp)//trim('.map')
              if (fileexist(f)) then
                allocate(map); ok = map%init(f)
                call map%read_data()
                call map%get_r4ar(r4wrk, r4nodata, dmin, dmax)
                call map%close(); call map%clean(); deallocate(map)
                do ir = 1, nrow
                  do ic = 1, ncol
                    if (r4wrk(ic,ir) == r4nodata) then
                      lc = (jc+1)*ncol + ic; lr = (jr+1)*nrow + ir
                      i4wrk(lc,lr) = 0
                    end if
                  end do
                end do
              else
                call logmsg('**** skipping reading '//trim(f)//': file does not exist.')
             end if 
            end if
          end if
        end do
      end do
      !
      call calc_unique(i4wrk, 9, regun, regbb, nreg, idum, 0., 0., 0.)
      f = trim(s)//'_unique'
      if (.not.allocated(regunw)) allocate(regunw(ncol,nrow))
      do ir = 1, nrow
        do ic = 1, ncol
           regunw(ic,ir) = regun(ic+ncol,ir+nrow)
        end do
      end do
      call writeflt(f, regunw, ncol, nrow, xul, yul-nrow*cs, cs, 0)
!      call map%read_data()
!      call map%get_r4ar(r4wrk, nodata, dmin, dmax)
!      do ir = 1, nrow
!        do ic = 1, ncol
!          if (r4wrk(ic,ir) == nodata) then
!            i4wrk(ic,ir) = 0
!            r4wrk(ic,ir) = my_nodata
!         else
!          i4wrk(ic,ir) = 1
!        end if
!      end do
!    end do
!    call map%close()
!    call map%clean()
!    deallocate(map)
    else if (iopt == 3) then
      ! read the header
      f = trim(s)//trim(fp)//trim('.map')
      allocate(map)
      ! read the map file
      ok = map%init(f)
      cs  = map%header%cellsizex
      xul = map%header%xul; yul = map%header%yul
      ncol = map%header%nrcols; nrow = map%header%nrrows
      call map%close(); call map%clean(); deallocate(map)
      !
      ! check if unique values need to be computed
      if (lumpcnt(gic,gir) == ncol*nrow) then
        if (.not.allocated(i1wrk)) allocate(i1wrk(ncol,nrow))
        do ir = 1, nrow
          do ic = 1, ncol
            i1wrk(ic,ir) = int(1,i1b)
          end do
        end do
        n = n + ncol*nrow
        f = trim(s)//'_pointer'
        !call writeflt(f, i1wrk, ncol, nrow, x0, y0,cs, int(0,i1b))
        lumpcnt_fin(gic,gir) = ncol*nrow
        cycle
      end if
      !
      call logmsg('Number of cells found for '//trim(s)//': '//ta((/lumpcnt(gic,gir)/)))
      do ip = 1, np
        ixy(ip) = 0
      end do
      mp = 0
      do ip = 1, np
        if ((x0 <= xy(1,ip)).and.(xy(1,ip) <= x1).and. &
            (y0 <= xy(2,ip)).and.(xy(2,ip) <= y1)) then
          mp = mp + 1
          ixy(ip) = 1
        end if
      end do
      if (mp == 0) then
        call logmsg('Could not find point for tile '//trim(s))
      end if
      !
      f = trim(s)//trim('_unique')
      call readflt(f, regun, ncol_r, nrow_r, xmin_r, ymin_r, cs_r, i4nodata_r)
      if (.not.allocated(i1wrk)) allocate(i1wrk(ncol,nrow))
      do ir = 1, nrow
        do ic = 1, ncol
          i1wrk(ic,ir) = 0
        end do
      end do
      do ip = 1, np
        if (ixy(ip) == 1) then
          x = xy(1,ip); y = xy(2,ip)
          jc = int((x - x0)/cs_r, i4b) + 1
          jr = nrow - int((y - y0)/cs_r, i4b)
          if ((jc < 0).or.(jc > ncol).or. &
              (jr < 0).or.(jr > nrow)) then
           call errmsg('Invalid coordinate!')
          end if
          i4v = regun(jc,jr)
          do ir = 1, nrow
            do ic = 1, ncol
              if (regun(ic,ir) == i4v) then
                i1wrk(ic,ir) = int(1,i1b)
              end if
            end do
          end do
        end if
      end do
      !
      nn = 0
      do ir = 1, nrow
        do ic = 1, ncol
          if (i1wrk(ic,ir) == int(1,i1b)) then
            n = n + 1
            nn = nn + 1
          end if
        end do
      end do
      lumpcnt_fin(gic,gir) = nn
      !
      ! write
      f = trim(s)//'_pointer_coast'
      call writeflt(f, i1wrk, ncol, nrow, x0, y0, cs, int(0,i1b))
    else if (iopt == 4) then
      !
      ncol = 6000; nrow = 6000
      if (allocated(cat)) deallocate(cat)
      allocate(cat(3*ncol,3*nrow))
      
      first = .true.
      do jr = -1, 1
        do jc = -1, 1
          kr = gir + jr; kc = gic + jc
          if ((kr >= 1).and.(kr <= 36).and.(kc >= 1).and.(kc <= 72)) then
            !
            if (kr <= 18) then !n
              slat = 'n'; jlat = 90-5*kr
            else !s
              slat = 's'; jlat = 5*(kr - 18)
            end if
            if (kc <= 36) then !w
              slon  = 'w'; jlon = 5*(36 + 1 - kc)
            else !e
              slon = 'e'; jlon = 5*(kc - 36- 1)
            end if
            write(tile,'(a,i2.2,a,i3.3)') slat, jlat, slon, jlon
            ! check file existence
            !f = tile//'_catch_Europe'
            f = './catch/'//tile//'_catch'
            if (.not.fileexist(trim(f)//'.flt')) then
              call logmsg('**** skipping reading '//trim(f)//': file does not exist.')
              cycle
            end if
            call readflt(f, r4wrk, ncol_r, nrow_r, xmin_r, ymin_r, cs_r, r4nodata_r)
            if ((jr == 0).and.(jc == 0)) then
              xmin = xmin_r; ymin = ymin_r; cs = cs_r; r4nodata = r4nodata_r
            end if
            !
            if (lumpcnt(kc,kr) < ncol*nrow) then
              f = tile//'_pointer_coast'
              call readflt(f, i1wrk, ncol_r, nrow_r, xmin_r, ymin_r, cs_r, i1nodata_r)
              do ir = 1, nrow
                do ic = 1, ncol
                  if (i1wrk(ic,ir) == int(1,i1b)) then !pointer
                    if (r4wrk(ic,ir) == r4nodata_r) then
                      r4wrk(ic,ir) = -1.0
                    end if
                  else
                    r4wrk(ic,ir) = r4nodata_r
                  end if
                end do
              end do
            end if
            !
            if (first) then
              do lr = 1, 3*nrow
                do lc = 1, 3*ncol
                  cat(lc,lr) = r4nodata_r
                end do
              end do
              first =.false.
            end if
            do ir = 1, nrow
              do ic = 1, ncol
                lc = (jc+1)*ncol + ic; lr = (jr+1)*nrow + ir
                cat(lc,lr) = r4wrk(ic,ir)
              end do
            end do
          end if
        end do
      end do
      !
      call fillgap(cat, r4nodata_r, -1.0)
      !
      do ir = 1, nrow
        do ic = 1, ncol
          r4wrk(ic,ir) = cat(ic+ncol,ir+nrow)
          if (r4wrk(ic,ir) == -1.0) then
            call errmsg('Program error: -1.0 detected.')
          end if
        end do
      end do
      f = trim(s)//'_catch_interpolated'
      call writeflt(f, r4wrk, ncol, nrow, xmin, ymin, cs, r4nodata)
    end if
      
!      ! check of all cells are active
!    skip = .false.
!    if (iopt == 2) then
!      if (lumpcnt(gic,gir) == ncol*nrow) then
!        skip = .true.
!      end if
!    end if
!    !
!    if (skip) then
!      call logmsg('*** skipping ***')
!      call map%close()
!      call map%clean()
!      deallocate(map)
!      cycle
!    end if
!    !
!    call map%read_data()
!    call map%get_r4ar(r4wrk, nodata, dmin, dmax)
!    if(.not.allocated(i4wrk)) allocate(i4wrk(ncol,nrow))
!    
!    do ir = 1, nrow
!      do ic = 1, ncol
!        if (r4wrk(ic,ir) == nodata) then
!          i4wrk(ic,ir) = 0
!          r4wrk(ic,ir) = my_nodata
!        else
!          i4wrk(ic,ir) = 1
!        end if
!      end do
!    end do
!    call map%close()
!    call map%clean()
!    deallocate(map)
!    !
!    if (iopt == 2) then
!     call calc_unique(i4wrk, 9, regun, regbb, nreg, idum, 0., 0., 0.)
!     f = trim(s)//'_unique'
!     call writeflt(f, regun, ncol, nrow, xul, yul-nrow*cs, cs, 0)
!    end if
!    !
!    ! fp, x, ncol, nrow, xll, yll, cs, nodata)
!!    call writeflt('n00e005_elv', wrk, ncol, nrow, xul, yul-nrow*cs, cs, my_nodata)
!!    allocate(wrki1(ncol,nrow))
!!    do ir = 1, nrow
!!      do ic = 1, ncol
!!        if (wrk(ic,ir) /= my_nodata) then
!!          wrki1(ic,ir) = 1
!!        else
!!          wrki1(ic,ir) = 0
!!        end if
!!      end do
!!    end do
!!    call writeflt('n00e005_elv_ptr', wrki1, ncol, nrow, xul, yul-nrow*cs, cs, int(0,i1b))
!!    stop
!    nn = 0
!    do ir = 1, nrow
!      do ic = 1, ncol
!        if (r4wrk(ic,ir) /= my_nodata) then
!          n = n + 1
!          nn = nn + 1
!        end if
!      end do
!    end do
!    if (iopt == 1) then
!      lumpcnt(gic,gir) = nn
!    end if
!    !
!    !if (nt == 100) exit
  end do
  !
  if (iopt == 1) then
   call writeflt(lumpout, lumpcnt, gncol, gnrow, -180.0d0, -90.d0, 5d0, 0)
  end if
  if (iopt == 3) then
    f = trim(lumpout)//'_final'
   call writeflt(f, lumpcnt_fin, gncol, gnrow, -180.0d0, -90.d0, 5d0, 0)
  end if
  !
  call logmsg('# cells: '//ta((/n/)))
  
end program
