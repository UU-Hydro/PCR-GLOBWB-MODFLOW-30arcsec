program merit
  ! modules
  use pcrModule
  use utilsmod, only: open_file, getdigits, writeasc, writeflt, writeidf, &
    addboundary, ta, logmsg, i1b, r4b, r8b

  implicit none
  
  type(tMap), pointer :: map
  logical :: ok
  character(len=1024) :: f
  integer(i1b), dimension(:,:), allocatable :: wrki1
  integer(i4b) :: ncol, nrow, iu, ios, ntile, ir, ic, nt
  integer(i8b) :: n
  real(r4b) :: my_nodata = -9999.
  real(r4b) :: nodata, dmin, dmax
  real(r4b), dimension(:,:), allocatable :: wrk
  real(r8b) :: xul, yul, xmin, xmax, ymin, ymax, cs
! ------------------------------------------------------------------------------
  
  call getarg(1,f)
  call open_file(f, iu, 'r')
  ntile = 0; n = 0
  do while(.true.)
    read(unit=iu,iostat=ios,fmt='(a)') f
    if (ios /=0) exit
    ntile = ntile + 1
  end do
  rewind(iu)
  
  n = 0; nt = 0
  do while(.true.)
    read(unit=iu,iostat=ios,fmt='(a)') f
    if (ios /=0) exit
    nt = nt + 1
    call logmsg('Processing '//ta((/nt/))//'/'//ta((/ntile/))//'...')
    !
    allocate(map)
    ! read the map file
    ok = map%init(f)
    cs  = map%header%cellsizex
    xul = map%header%xul
    yul = map%header%yul
    ncol = map%header%nrcols
    nrow = map%header%nrrows
    call map%read_data()
    call map%get_r4ar(wrk, nodata, dmin, dmax)
    do ir = 1, nrow
      do ic = 1, ncol
        if (wrk(ic,ir) == nodata) then
          wrk(ic,ir) = my_nodata
        end if
      end do
    end do
    call map%close()
    call map%clean()
    deallocate(map)
    !
    ! fp, x, ncol, nrow, xll, yll, cs, nodata)
    call writeflt('n00e005_elv', wrk, ncol, nrow, xul, yul-nrow*cs, cs, my_nodata)
    allocate(wrki1(ncol,nrow))
    do ir = 1, nrow
      do ic = 1, ncol
        if (wrk(ic,ir) /= my_nodata) then
          wrki1(ic,ir) = 1
        else
          wrki1(ic,ir) = 0
        end if
      end do
    end do
    call writeflt('n00e005_elv_ptr', wrki1, ncol, nrow, xul, yul-nrow*cs, cs, int(0,i1b))
    stop
    
    do ir = 1, nrow
      do ic = 1, ncol
        if (wrk(ic,ir) /= my_nodata) then
          n = n + 1
        end if
      end do
    end do
    !if (nt == 100) exit
  end do
  !
  call logmsg('# cells: '//ta((/n/)))
  
end program
