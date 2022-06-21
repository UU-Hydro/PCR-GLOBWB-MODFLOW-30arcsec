program filteridf
! ******************************************************************************
  ! --- local
  use imod_idf_par, only: idfobj
  use imod_idf
  use utilsmod, only: mxslen, i4b, r8b, writeasc, writeidf, errmsg, chkexist, &
    logmsg, ta, DZERO, readidf_block, open_file, logmsg, ta
  use ehdrModule, only: writeflt
  !
  implicit none
  !
  ! --- local
  
  type(idfobj) :: idf1, idf2, idf3
  !
  character(len=mxslen) :: s, f_in_1, f_in_2, f_in_3, f_out
  logical :: lfound
  integer(i4b) :: iopt, iu, n
  integer(i4b) :: ic, ir, nr, nc, gir0, gir1, gic0, gic1
  real(r8b) :: gcs, r8v1, r8v2, r8v3, d, rmse, sqe
! ------------------------------------------------------------------------------
  !
  call getarg(1,f_out)
  call getarg(2,s); read(s,*) iopt
  call getarg(3,f_in_1); call chkexist(f_in_1)
  call getarg(4,f_in_2); call chkexist(f_in_2)
  !
  ! check
  select case (iopt)
  case(1)
    call logmsg("Option 1: mask")
  case(2)
    call logmsg("Option 2: negative")
  case(3)
    call logmsg("Option 3: area correction")
  case(4)
    call logmsg("Option 4: transmissivity")
    call getarg(5,f_in_3); call chkexist(f_in_3)
  case(5)
    call logmsg("Option 5: sum")
  case(6)
    call logmsg("Option 6: mask")
  case(7)
    call logmsg("Option 7: RMSE")
    call getarg(5,f_in_3); call chkexist(f_in_3)
  case default
    call errmsg("Unknown option.")
  end select  
  
  call logmsg("Reading "//trim(f_in_1)//"...")
  if (.not.idfread(idf1, f_in_1, 1)) then
    call errmsg('Could not read '//trim(f_in_1))
  end if
  call logmsg("Reading "//trim(f_in_2)//"...")
  if (iopt == 7) then
    call open_file(f_in_3, iu, 'r')
    read(iu,*) gic0, gic1, gir0, gir1
    close(iu)
    !
    call logmsg('gic0, gic1, gir0, gir1: '//ta((/gic0, gic1, gir0, gir1/)))
    !
    if (.not.idfread(idf2, f_in_2, 0)) then
      call errmsg('Could not read '//trim(f_in_2))
    end if
    !
    call readidf_block(idf2, gir0, gir1, gic0, gic1, idf2%x)
    !
    idf2%ncol = size(idf2%x,1)
    idf2%nrow = size(idf2%x,2)
  else
    if (.not.idfread(idf2, f_in_2, 1)) then
      call errmsg('Could not read '//trim(f_in_2))
    end if
  end if
  if (iopt == 4) then
    call logmsg("Reading "//trim(f_in_3)//"...")
    if (.not.idfread(idf3, f_in_3, 1)) then
      call errmsg('Could not read '//trim(f_in_3))
    end if
  end if
  !
  ! check
  if ((idf1%ncol /= idf2%ncol).or.(idf1%nrow /= idf2%nrow)) then
    call errmsg("Inconsistent ncol/nrow for ")
  end if
  if ((iopt /= 7).and.(idf1%dx /= idf2%dx)) then
    call errmsg("Inconsistent cellsize for ")
  end if
  !
  call logmsg("Computing...")
  
  if (iopt == 1) then
    do ir = 1, idf1%nrow
      do ic = 1, idf1%ncol
        if (idf1%x(ic,ir) /= idf1%nodata) then
          if (idf2%x(ic,ir) == idf2%nodata) then
            idf1%x(ic,ir) = idf1%nodata
          end if
        end if
      end do
    end do
  end if
  !
  if (iopt == 2) then
    do ir = 1, idf1%nrow
      do ic = 1, idf1%ncol
        if (idf2%x(ic,ir) /= idf2%nodata) then
          if (idf1%x(ic,ir) /= idf1%nodata) then
            idf1%x(ic,ir) = -idf1%x(ic,ir)
          end if
        end if
      end do
    end do
  end if
  !
  if (iopt == 3) then
    gcs = idf1%dx
    do ir = 1, idf1%nrow
      do ic = 1, idf1%ncol
        if (idf1%x(ic,ir) /= idf1%nodata) then
          if (idf2%x(ic,ir) /= idf2%nodata) then
            idf1%x(ic,ir) = idf1%x(ic,ir) * gcs*gcs / idf2%x(ic,ir)
          end if
        end if
      end do
    end do
  end if  
  !
  if (iopt == 4) then
    do ir = 1, idf1%nrow
      do ic = 1, idf1%ncol
        r8v1 = idf1%x(ic,ir)
        r8v2 = idf2%x(ic,ir)
        r8v3 = idf3%x(ic,ir)
        if ((r8v1/= idf1%nodata).and.(r8v2/= idf2%nodata).and.(r8v3/= idf3%nodata)) then
          d = r8v2 - r8v3
          if (d <= DZERO) then
            call errmsg("d <= 0")
          end if
          idf1%x(ic,ir) = r8v1 * d
        end if
      end do
    end do
  end if
  !
  if (iopt == 5) then
    do ir = 1, idf1%nrow
      do ic = 1, idf1%ncol
        r8v1 = idf1%x(ic,ir)
        r8v2 = idf2%x(ic,ir)
        if ((r8v1/= idf1%nodata).and.(r8v2 /= idf2%nodata)) then
          idf1%x(ic,ir) = r8v1 + r8v2
        end if
      end do
    end do
  end if
  !
  if (iopt == 6) then
    do ir = 1, idf1%nrow
      do ic = 1, idf1%ncol
        r8v1 = idf1%x(ic,ir)
        r8v2 = idf2%x(ic,ir)
        if ((r8v1 == idf1%nodata).and.(r8v2 /= idf2%nodata)) then
          idf1%x(ic,ir) = r8v2
        end if
      end do
    end do
  end if
  !
  if (iopt == 7) then
    n = 0
    rmse = DZERO
    do ir = 1, idf1%nrow
      do ic = 1, idf1%ncol
        r8v1 = idf1%x(ic,ir)
        r8v2 = idf2%x(ic,ir)
        if ((r8v1 /= idf1%nodata).and.(r8v2 /= idf2%nodata)) then
          n = n + 1
          sqe = (r8v1 - r8v2)**2
          rmse = rmse + sqe
          idf1%x(ic,ir) = sqe
        else
          idf1%x(ic,ir) = idf1%nodata
        end if
      end do
    end do
    !
    do ir = 1, idf1%nrow
      do ic = 1, idf1%ncol
        r8v1 = idf1%x(ic,ir)
        if (r8v1 /= idf1%nodata) then
           idf1%x(ic,ir) = sqrt(r8v1 / n)
        end if
      end do
    end do
    !
    rmse = sqrt(rmse/n)
    !
    call logmsg("RSME = "//ta((/rmse/)))
  end if
  !
  if (index(f_out,'.idf',back=.true.) > 0) then
    call writeidf(trim(f_out), idf1%x, idf1%ncol, idf1%nrow, idf1%xmin, idf1%ymin, idf1%dx, idf1%nodata)
  else if (index(f_out,'.asc',back=.true.) > 0) then
    call writeasc(trim(f_out), idf1%x, idf1%ncol, idf1%nrow, idf1%xmin, idf1%ymin, idf1%dx, idf1%nodata)
  else
    call writeflt(trim(f_out), idf1%x, idf1%ncol, idf1%nrow, idf1%xmin, idf1%ymin, idf1%dx, idf1%nodata)
  end if
  !
end program
  