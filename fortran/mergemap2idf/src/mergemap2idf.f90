program mergemap2idf
  ! modules
  use pcrModule
  use utilsmod, only: writeasc, writeidf, DZERO
  
  implicit none
  
  type(tMap), pointer :: map
 
  logical :: ok, lex, lwritetile
  character(len=1024) :: s, f, fo, fp
  integer(i4b) :: iact, ic, ir, nc, nr, np, lun, ios, ip, ir0, ir1, ic0, ic1, dir0, dir1, dic0, dic1, na
  integer(i4b), dimension(:,:), allocatable :: iwrk, itile
  real(r4b), dimension(:,:), allocatable :: gx, lx
  integer(i4b), dimension(:), allocatable :: icmin, icmax, irmin, irmax
  integer(i4b), dimension(:), allocatable :: dicmin, dicmax, dirmin, dirmax
  real(r4b) :: lmv, lmin, lmax, gval, lval
  real(r4b) :: gxll, gyll, gcs, gmv
  character(len=1024) :: s1, s2
  
  ! read input
  !### rcb.idf 8.txt -180 90 rcb_{1}-{2}.map  
  call getarg(1,fo)
  call getarg(2,f)
  write(*,*) 'Reading '//trim(f)//'...'
  open(file=f, newunit=lun, action='read')
  read(lun,*) np, nc, nr
  allocate(icmin(np), icmax(np), irmin(np), irmax(np))
  allocate(dicmin(np), dicmax(np), dirmin(np), dirmax(np))
  do ip = 1, np
    read(lun,*,iostat=ios) icmin(ip), icmax(ip), irmin(ip), irmax(ip), &
      dicmin(ip), dicmax(ip), dirmin(ip), dirmax(ip)
  end do
  close(lun)
  call getarg(3,s); read(s,*) gxll
  call getarg(4,s); read(s,*) gyll
  call getarg(5,fp)
  !
  allocate(gx(nc,nr), iwrk(nc,nr))
  !
  na = nargs()
  if (na == 7) then
    lwritetile = .true.
    allocate(itile(nc,nr))
    do ir = 1, nr
      do ic = 1, nc
        itile(nc,nr) = 0
      end do
    end do
  else
    lwritetile = .false.
  end if
  !
  allocate(map)
  do iact = 1, 2
    do ip = 1, np
    !do ip = 47, 48
      call get_map_fname(f, fp, ip, np)
      inquire(file=f,exist=lex)
      if (.not.lex) then
        write(*,*) 'Skipping '//trim(f)//'...'
        cycle
      end if
      ok = map%init(f)
      call map%read_data()
      call map%get_r4ar(lx, lmv, lmin, lmax)
      if ((ip == 1) .and. (iact == 1)) then
         gcs = real(map%header%cellsizex)
         gmv = lmv
         do ir = 1, nr
           do ic = 1, nc
             gx(ic,ir) = lmv
             iwrk(ic,ir) = 0
           end do
        end do
      end if
      ir0 = irmin(ip); ir1 = irmax(ip); ic0 = icmin(ip); ic1 = icmax(ip)
      dir0 = dirmin(ip); dir1 = dirmax(ip); dic0 = dicmin(ip); dic1 = dicmax(ip)
      if (iact == 2) then
        dir0 = dir0 - 1; dir0 = max(0, dir0)
        dir1 = dir1 - 1; dir1 = max(0, dir1)
        dic0 = dic0 - 1; dic0 = max(0, dic0)
        dic1 = dic1 - 1; dic1 = max(0, dic1)
      end if
      if (iact == 1) then
        do ir = ir0+dir0, ir1-dir1
          do ic = ic0+dic0, ic1-dic1
            lval = lx(ic-ic0+1, ir-ir0+1)
            if (lval /= lmv) then
              ! check
              gval = gx(ic,ir)
              if (gval /= gmv) then
                if (lval /= gval) then
                  write(s1,*) ic; write(s2,*) ir
                  write(*,*) 'Error, inconsistent data for '//trim(adjustl(s1))//' '//trim(adjustl(s2))//'!'
                  stop 1
                end if
              end if
              ! set
              gx(ic,ir) = lval
              iwrk(ic,ir) = 1
              if (lwritetile) then
                itile(ic,ir) = ip
              end if
            end if
          end do
        end do
      else
        do ir = ir0+dir0, ir1-dir1
          do ic = ic0+dic0, ic1-dic1
            if (iwrk(ic,ir) /= 1) then
              lval = lx(ic-ic0+1, ir-ir0+1)
              if (lval /= lmv) then
                gval = gx(ic,ir)
                ! check
                if ((iwrk(ic,ir) == -1) .and. (gval /= gmv)) then
                  if (lval /= gval) then
                    write(s1,*) ic; write(s2,*) ir
                    write(*,*) 'Warning, inconsistent data for '//trim(adjustl(s1))//' '//trim(adjustl(s2))//'!'
                  end if
                end if
                ! set data
                if (gval == gmv) then
                  gx(ic,ir) = lval
                  iwrk(ic, ir) = -1
                  if (lwritetile) then
                    itile(ic,ir) = DZERO
                  end if
                end if
              end if
            end if
          end do
        end do
      end if
      call map%close()
      call map%clean()
    end do
  end do !iact
  !
  if (index(fo,'.idf',back=.true.) > 0) then
    call writeidf(fo, gx, nc, nr, dble(gxll), dble(gyll), dble(gcs), dble(gmv))
  elseif (index(fo,'.asc',back=.true.) > 0) then
    call writeasc(fo, gx, nc, nr, dble(gxll), dble(gyll), dble(gcs), dble(gmv))
  end if
  !
  if (lwritetile) then
    call writeidf("tile.idf", itile, nc, nr, dble(gxll), dble(gyll), dble(gcs), DZERO)
    call writeasc("tile.asc", itile, nc, nr, dble(gxll), dble(gyll), dble(gcs), DZERO)
  end if
end program
  
subroutine get_map_fname(f, fp, ip, np)
  implicit none
  integer, intent(in) :: ip, np
  character(len=*), intent(in) :: fp
  character(len=*), intent(out) :: f

  integer :: i, j, n
  character(len=128) :: s1, s2, s3
  
  n = len_trim(fp)
  i = index(fp,'{1}')
  j = index(fp,'{2}')
  s1 = fp(1:i-1)
  s2 = fp(i+3:j-1)
  s3 = fp(j+3:n)
  !
!  if (np < 10) then
!    write(f,'(a,i1,a,i1,a)') trim(s1), ip, trim(s2), np, trim(s3)
!  end if
!  if ((np >= 10) .and. (np < 100)) then
!    write(f,'(a,i2.2,a,i2.2,a)') trim(s1), ip, trim(s2), np, trim(s3)
!  end if
!  if ((np >= 100) .and. (np < 1000)) then
    write(f,'(a,i3.3,a,i3.3,a)') trim(s1), ip, trim(s2), np, trim(s3)
!  end if

end subroutine get_map_fname