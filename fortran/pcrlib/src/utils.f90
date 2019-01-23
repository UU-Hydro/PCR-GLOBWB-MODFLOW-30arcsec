module utils
  implicit none

contains
  
  !###====================================================================
  function getlun() result(lun)
  !###====================================================================
    ! -- dummy
    integer :: lun
    ! locals
    logical :: lex

    do lun=10,100
      inquire(unit=lun,opened=lex)
      if(.not.lex)exit
    end do

  end function getlun

  !###====================================================================
  subroutine chkexist(fname)
  !###====================================================================
    ! arguments
    character(len=*) :: fname
    ! locals
    logical :: lex

    inquire(file=fname,exist=lex)
    if (.not.lex) then
      write(*,*) 'Error: cannot find ',trim(fname),' !'
      stop
    end if
  end subroutine chkexist

  !###====================================================================
  function getdigits(n) result(ndig)
  !###====================================================================
    integer, intent(in) :: n
    integer :: ndig
    ndig = 1
    if (abs(n) > 10) ndig = 2
    if (abs(n) > 100) ndig = 3
    if (abs(n) > 1000) ndig = 4
    if (n < 0) ndig = ndig + 1
  end function getdigits

  !###====================================================================
  subroutine writeasc(f,ix,ncol,nrow,xll,yll,cs,nodata)
  !###====================================================================
    character(len=*), intent(in) :: f
    integer, intent(in) :: ncol, nrow
    integer, dimension(ncol,nrow), intent(in) :: ix
    real, intent(in) :: xll,yll,cs,nodata

    integer :: lun, ndig, icol, irow, imin, imax, nd, ndmin, ndmax
    character(len=100) :: fmtstr

    imin = minval(ix)
    imax = maxval(ix)
    ndmin = getdigits(imin)
    ndmax = getdigits(imax)
    nd = max(ndmin,ndmax)

    lun = getlun()
    open(unit=lun,file=f,status='replace')
    write(lun,*) 'ncols',ncol
    write(lun,*) 'nrows',nrow
    write(lun,*) 'xllcorner',xll
    write(lun,*) 'yllcorner',yll
    write(lun,*) 'cellsize',cs
    write(lun,*) 'nodata_value',nodata
    write(fmtstr,*) '(',ncol,'i',nd+1,')'
    write(lun,fmtstr)((ix(icol,irow),icol=1,ncol),irow=1,nrow)
    close(lun)

    return
  end subroutine writeasc

  !###====================================================================
  function change_case(str, opt) result (string)
  !###====================================================================
    character(*), intent(in) :: str
    character(len=1) :: opt
    character(len(str))      :: string

    integer :: ic, i

    character(26), parameter :: cap = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    character(26), parameter :: low = 'abcdefghijklmnopqrstuvwxyz'

  !   Capitalize each letter if it is lowecase
    string = str
    if ((opt == 'U') .or. (opt == 'u')) then
      do i = 1, LEN_TRIM(str)
        ic = INDEX(low, str(i:i))
        if (ic > 0) string(i:i) = cap(ic:ic)
      end do
    end if
    if ((opt == 'L') .or. (opt == 'l')) then
      do i = 1, LEN_TRIM(str)
        ic = INDEX(cap, str(i:i))
        if (ic > 0) string(i:i) = low(ic:ic)
      end do
    end if

  end function change_case

  subroutine errmsg(s)
    character(len=*), intent(in) :: s
    write(*,'(a,1x,a)') 'Error:', trim(s)
    stop 1
  end subroutine errmsg

  !###====================================================================
  subroutine error(s)
  !###====================================================================
    character(len=*), intent(in) :: s
  
    write(*,'(a,1x,a)') 'Error:',trim(s)
    write(*,*) 'Stopping...'
    stop 1
  
  end subroutine error
  
end module utils

