program mergemap2idf
  ! modules
  use pcrModule
  use utilsmod, only: writeidf
  
  implicit none
  
  type(tMap), pointer :: map
 
  logical :: ok, lex
  character(len=1024) :: s, f, fo, fp
  integer(i4b) :: ic, ir, nc, nr, np, lun, ios, ip, ir0, ir1, ic0, ic1
  real(r4b), dimension(:,:), allocatable :: gx, lx
  integer(i4b), dimension(:), allocatable :: icmin, icmax, irmin, irmax
  real(r4b) :: lmv, lmin, lmax, gval, lval
  real(r4b) :: gxll, gyll, gcs, gmv
  
  ! read input
  !### rcb.idf 8.txt -180 90 rcb_{1}-{2}.map  
  call getarg(1,fo)
  call getarg(2,f)
  write(*,*) 'Reading '//trim(f)//'...'
  open(file=f, newunit=lun, action='read')
  read(lun,*) np, nc, nr
  allocate(icmin(np), icmax(np), irmin(np), irmax(np))
  do ip = 1, np
    read(lun,*,iostat=ios) icmin(ip), icmax(ip), irmin(ip), irmax(ip)
  end do
  close(lun)
  call getarg(3,s); read(s,*) gxll
  call getarg(4,s); read(s,*) gyll
  call getarg(5,fp)
  !
  allocate(gx(nc,nr))
  !
  allocate(map)
  do ip = 1, np
    call get_map_fname(f, fp, ip, np)
    inquire(file=f,exist=lex)
    if (.not.lex) then
      write(*,*) 'Skipping '//trim(f)//'...'
      cycle
    end if
    ok = map%read_header(f)
    call map%read_data()
    call map%get_r4ar(lx, lmv, lmin, lmax)
    if (ip == 1) then
       gcs = real(map%header%cellsizex)
       gmv = lmv
       do ir = 1, nr
         do ic = 1, nc
           gx(ic,ir) = lmv
         end do
      end do
    end if
    ir0 = irmin(ip); ir1 = irmax(ip)
    ic0 = icmin(ip); ic1 = icmax(ip)
    do ir = ir0, ir1
      do ic = ic0, ic1
        lval = lx(ic-ic0+1,ir-ir0+1) 
        if (lval /= lmv) then
          ! check
          gval = gx(ic,ir)
          if (gval /= gmv) then
            if (lval /= gval) then
              write(*,*) 'Inconsistent data!'
              stop 1
            end if
          end if
          ! set
          gx(ic,ir) = lval
        end if
      end do
    end do
    call map%close()
    call map%clean()
  end do
  !
  call writeidf(fo, gx, nc, nr, gxll, gyll, gcs, gmv)
  
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
  if (np < 10) then
    write(f,'(a,i1,a,i1,a)') trim(s1), ip, trim(s2), np, trim(s3)
  end if
  if ((np >= 10) .and. (np < 100)) then
    write(f,'(a,i2.2,a,i2.2,a)') trim(s1), ip, trim(s2), np, trim(s3)
  end if
  if ((np >= 100) .and. (np < 1000)) then
    write(f,'(a,i3.3,a,i3.3,a)') trim(s1), ip, trim(s2), np, trim(s3)
  end if

end subroutine get_map_fname