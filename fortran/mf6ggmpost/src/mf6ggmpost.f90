program mf6ggmpost
  ! -- modules
  use utilsmod, only: i4b, mxslen, open_file, logmsg, ta
  use mf6_post_module, only: tPostSol, tPostMod, &
    gncol, gnrow, gnlay, gxmin, gymin, gcs, &
    sdate, tilebb, top
  !
  implicit none
  !
  ! -- locals
  type(tPostSol), pointer :: postsol => null()
  integer(i4b) :: iu, i, npost
  character(len=mxslen) :: f, s
! ------------------------------------------------------------------------------
  
  call getarg(1, f)
  call open_file(f, iu, 'r')
  read(iu,*) gncol, gnrow, gnlay, gxmin, gymin, gcs
  read(iu,*) sdate
  read(iu,'(a)') tilebb
  read(iu,'(a)') top
  read(iu,*) npost
  allocate(postsol)
  do i = 1, npost
    call logmsg('***** Processing '//ta((/i/))//'/'//ta((/npost/))//'...')
    read(iu,'(a)') s
    call postsol%init(s)
    call postsol%write()
    call postsol%clean()
  end do
  close(iu)
  
  
  ! modules
  !use utilsmod, only: mxslen, i4b, r4b, r8b, open_file, ta, chkexist, &
  !  readidf, writeidf, errmsg, logmsg
  !
  !implicit none
  !
  !character(len=mxslen) :: rootdir, hdsext, solname, hdspref, f, s
  !character(len=mxslen) :: modelname
  !integer(i4b) :: iu, ju, n, m, nmod, isol, sol1, sol2, i, idum
  !integer(i4b) :: ggncol, ggnrow, gncol, gnrow, gnlay, gir0, gir1, gic0, gic1
  !integer(i4b) :: lncol, lnrow, lir0, lir1, lic0, lic1
  !integer(i4b) :: imod, ir, ic, il, jr, jc, kr, kc
  !real(r4b) :: xll, yll, cs, nodata
  !character(len=16) :: text_in ! ulasav
  !integer(i4b) :: kstp_in, kper_in, ncol_in, nrow_in, ilay_in !ulasav
  !real(r8b) :: pertim_in, totim_in
  !real(r4b), dimension(:,:), allocatable :: r4wrk1, r4wrk2
  !real(r8b), dimension(:), allocatable :: hds
  !integer(i4b) :: ncol_idf, nrow_idf
  !real(r4b) :: r4dum, nodata_idf
  !! ------------------------------------------------------------------------------
  !! 1  2        3                                                 4   5    6   7                   8
  !! .\ .ext.hds g:\models\pcr-globwb-30arcsec\model\s49\mappings\ s49 -180 -90 8.333333333333E-003 -9999.0
  !call getarg(1,rootdir)
  !call getarg(2,hdsext)
  !call getarg(3,s); read(s,*) sol1
  !call getarg(4,s); read(s,*) sol2
  !call getarg(5,s); read(s,*) xll
  !call getarg(6,s); read(s,*) yll
  !call getarg(7,s); read(s,*) cs
  !call getarg(8,s); read(s,*) nodata
  !call getarg(9,s); read(s,*) gnlay
  !!
  !do il = 1, gnlay
  !  do isol = sol1, sol2
  !    !
  !    write(solname,'(a,i2.2)') 's', isol
  !    f = trim(rootdir)//trim(solname)//'\mappings\'//trim(solname)//'.asc'
  !    call open_file(f, iu)
  !    read(iu,*) ggncol, ggnrow, gncol, gnrow, idum, gir0, gir1, gic0, gic1
  !    read(iu,*) nmod
  !    !
  !    if (.not.allocated(r4wrk1)) then
  !      allocate(r4wrk1(ggncol,ggnrow))
  !    end if
  !    if (isol == sol1) then
  !      do ir = 1, ggnrow
  !        do ic = 1, ggncol
  !          r4wrk1(ic,ir) = nodata
  !        end do
  !      end do
  !    end if
  !    do imod = 1, nmod
  !      read(iu,*) modelname, lncol, lnrow, lir0, lir1, lic0, lic1
  !      ! read the heads
  !      f = trim(rootdir)//trim(solname)//'\results\'//trim(modelname)//trim(hdsext)
  !      call open_file(f, ju, 'r', .true.)
  !      read(ju) kstp_in, kper_in, pertim_in, totim_in, text_in, ncol_in, nrow_in, ilay_in
  !      if (allocated(hds)) then
  !        deallocate(hds)
  !      end if
  !      allocate(hds(ncol_in))
  !      read(ju) hds
  !      close(ju)
  !      !
  !      ! read the nodmap
  !      if (allocated(r4wrk2)) then
  !        deallocate(r4wrk2)
  !      end if
  !      allocate(r4wrk2(lncol,lnrow))
  !      do ir = 1, lnrow
  !        do ic = 1, lncol
  !          r4wrk2(ic,ir) = 0
  !        end do
  !      end do
  !      !f = trim(mapdir)//'nodmap_'//trim(modelname)//'_l'//ta((/il/))//'.bin'
  !      !call chkexist(f)
  !      !call readidf(f, r4wrk2, ncol_idf, nrow_idf, r4dum, r4dum, r4dum, nodata_idf)
  !      !if ((lncol /= ncol_idf).or.(lncol /= ncol_idf)) then
  !      !  call errmsg('Invalid IDF dimensions.')
  !      !end if
  !      f = trim(rootdir)//trim(solname)//'\mappings\nodmap_'//trim(modelname)//'_l'//ta((/il/))//'.bin'
  !      call open_file(f, ju, 'r', .true.)
  !      read(ju) n
  !      do i = 1, n
  !        read(ju) ic, ir, m
  !        r4wrk2(ic,ir) = m
  !      end do
  !      close(ju)
  !      !
  !      do ir = lir0, lir1
  !        do ic = lic0, lic1
  !          jr = ir - lir0 + 1; jc = ic - lic0 + 1
  !          n = int(r4wrk2(jc,jr))
  !          if (n /= 0) then
  !            if (n < 0 .or. n > ncol_in) then
  !              call errmsg('Invalid node.')
  !            end if
  !            if (r4wrk1(ic,ir) /= nodata) then
  !              if (r4wrk1(ic,ir) /= hds(n)) then
  !                call errmsg('Overlapping data')
  !              end if
  !            else
  !              r4wrk1(ic,ir) = hds(n)
  !            end if
  !            !kr = ir - gir0 + 1; kc = ic - gic0 + 1
  !            !if (r4wrk1(kc,kr) /= nodata) then
  !            !  call errmsg('Overlapping data')
  !            !else
  !            !  r4wrk1(kc,kr) = hds(n)
  !            !end if
  !          end if
  !        end do
  !      end do
  !    end do !model
  !    !rewind(iu)
  !    !read(iu,*) ggncol, ggnrow, gncol, gnrow, idum, gir0, gir1, gic0, gic1
  !    !read(iu,*) nmod
  !    close(iu)
  !    
  !  end do !isol
  !  !
  !  f = trim(hdsext)//'_heads_l'//ta((/il/))//'.idf'
  !  call writeidf(f, r4wrk1, size(r4wrk1,1), size(r4wrk1,2), &
  !      xll, yll, cs, nodata)
  !end do !ilay
  !!
  !!  
  !close(iu)
  !
end program