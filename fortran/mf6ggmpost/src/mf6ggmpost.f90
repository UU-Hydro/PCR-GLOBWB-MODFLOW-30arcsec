program mf6ggmpost
  ! -- modules
  use utilsmod, only: i4b, mxslen, open_file, logmsg, ta, parse_line
  use mf6_post_module, only: tPostSol, tPostSer, &
    gncol, gnrow, gnlay, gxmin, gymin, gcs, gnsol, &
    sdate, tilebb, top, comment, mask, maskmap
  !
  implicit none
  !
  ! -- locals
  type(tPostSol), pointer :: postsol => null()
  type(tPostSer), pointer :: postser => null()
  character(len=1) :: cdum, ssol
  character(len=mxslen) :: f, s, in_dir
  character(len=mxslen), dimension(:), allocatable :: sa
  logical :: lex, lok
  integer(i4b) :: iu, i, npost, na
! ------------------------------------------------------------------------------
  
  call getarg(1, f)
  call open_file(f, iu, 'r')
  read(iu,*) gncol, gnrow, gnlay, gxmin, gymin, gcs, gnsol
  read(iu,*) sdate
  read(iu,'(a)') tilebb
  read(iu,'(a)') mask
  read(iu,'(a)') top
  read(iu,*) npost
  !
  ! read the mask file
  inquire(file=mask, exist=lex)
  if (lex) then
    allocate(maskmap)
    lok = maskmap%init(mask)
    call maskmap%set_nodata()
    call logmsg('***** Found mask! *****')
  else
    call logmsg('***** Could not find mask! *****')
  end if
  !
  allocate(postsol, postser)
  do i = 1, npost
    call logmsg('***** Processing '//ta((/i/))//'/'//ta((/npost/))//'...')
    read(iu,'(a)') s
    !
    !return in case of comment
    if (s(1:1) == comment) then
      call logmsg('Skipping...')
      cycle
    end if
    !
    call parse_line(s, sa)
    !
    select case(sa(2))
      case('m', 's','smodid','r')
        call postsol%init(sa)
        call postsol%write()
        call postsol%clean()
      case('t')
        call postser%init(sa)
        call postser%write()
        call postser%write_summary()
        call postser%clean()
    end select
  end do
  close(iu)
  !
end program