program mf6ggmpost
  ! modules
  use utilsmod, only: mxslen, i4b, r4b, r8b, open_file, ta, chkexist, &
    readidf, writeidf, errmsg, logmsg
  
  implicit none
  
  character(len=mxslen) :: hdsdir, hdsext, mapdir, solname, hdspref, f, s
  character(len=mxslen) :: modelname
  integer(i4b) :: iu, ju, n, nmod
  integer(i4b) :: ggncol, ggnrow, gncol, gnrow, gnlay, gir0, gir1, gic0, gic1
  integer(i4b) :: lncol, lnrow, lir0, lir1, lic0, lic1
  integer(i4b) :: imod, ir, ic, il, jr, jc, kr, kc
  real(r4b) :: xll, yll, cs, nodata
  character(len=16) :: text_in ! ulasav
  integer(i4b) :: kstp_in, kper_in, ncol_in, nrow_in, ilay_in !ulasav
  real(r8b) :: pertim_in, totim_in
  real(r4b), dimension(:,:), allocatable :: r4wrk1, r4wrk2
  real(r8b), dimension(:), allocatable :: hds
  integer(i4b) :: ncol_idf, nrow_idf
  real(r4b) :: r4dum, nodata_idf
  ! ------------------------------------------------------------------------------
  ! 1  2        3                                                 4   5    6   7                   8
  ! .\ .ext.hds g:\models\pcr-globwb-30arcsec\model\s49\mappings\ s49 -180 -90 8.333333333333E-003 -9999.0
  call getarg(1,hdsdir)
  call getarg(2,hdsext)
  call getarg(3,mapdir)
  call getarg(4,solname)
  call getarg(5,s); read(s,*) xll
  call getarg(6,s); read(s,*) yll
  call getarg(7,s); read(s,*) cs
  call getarg(8,s); read(s,*) nodata
  ! 
  f = trim(mapdir)//trim(solname)//'.asc'
  call open_file(f, iu)
  read(iu,*) ggncol, ggnrow, gncol, gnrow, gnlay, gir0, gir1, gic0, gic1
  read(iu,*) nmod
  !
  allocate(r4wrk1(gncol,gnrow))
  !
  do il = 1, gnlay
    do ir = 1, gnrow
      do ic = 1, gncol
        r4wrk1(ic,ir) = nodata
      end do
    end do
    !
    do imod = 1, nmod
      read(iu,*) modelname, lncol, lnrow, lir0, lir1, lic0, lic1
      ! read the heads
      f = trim(hdsdir)//trim(modelname)//trim(hdsext)
      call open_file(f, ju, 'r', .true.)
      read(ju) kstp_in, kper_in, pertim_in, totim_in, text_in, ncol_in, nrow_in, ilay_in
      if (allocated(hds)) then
        deallocate(hds)
      end if
      allocate(hds(ncol_in))
      read(ju) hds
      close(ju)
      !
      ! read the nodmap
      if (allocated(r4wrk2)) then
        deallocate(r4wrk2)
      end if
      allocate(r4wrk2(lncol,lncol))
      f = trim(mapdir)//'nodmap_'//trim(modelname)//'_l'//ta((/il/))//'.idf'
      call chkexist(f)
      call readidf(f, r4wrk2, ncol_idf, nrow_idf, r4dum, r4dum, r4dum, nodata_idf)
      if ((lncol /= ncol_idf).or.(lncol /= ncol_idf)) then
        call errmsg('Invalid IDF dimensions.')
      end if
      do ir = lir0, lir1
        do ic = lic0, lic1
          jr = ir - lir0 + 1; jc = ic - lic0 + 1
          n = int(r4wrk2(jc,jr))
          if (n /= 0) then
            if (n < 0 .or. n > ncol_in) then
              call errmsg('Invalid node.')
            end if
            kr = ir - gir0 + 1; kc = ic - gic0 + 1
            if (r4wrk1(kc,kr) /= nodata) then
              call errmsg('Overlapping data')
            else
              r4wrk1(kc,kr) = hds(n)
            end if
          end if
        end do
      end do
    end do
    rewind(iu)
    read(iu,*) ggncol, ggnrow, gncol, gnrow, gnlay, gir0, gir1, gic0, gic1
    read(iu,*) nmod
    !
    f = trim(solname)// trim(hdsext)//'_heads_l'//ta((/il/))//'.idf'
    call writeidf(f, r4wrk1, size(r4wrk1,1), size(r4wrk1,2), &
        xll+(gic0-1)*cs, yll+(ggnrow-gir1)*cs, cs, nodata)
  end do !ilay
  
  close(iu)
  !
end program