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
  !
end program