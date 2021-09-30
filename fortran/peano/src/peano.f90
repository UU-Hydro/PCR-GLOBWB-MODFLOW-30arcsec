module peano_mod
  use utilsmod, only: mxslen, i1b, i2b, i4b, i8b, r4b, r8b, logmsg, errmsg
  use pcrModule, only: jsw, js, jse, jw, jp, je, jnw, jn, jne, st, jperm
  !
  implicit none
  !
  private
  !
  type tChan
    integer(i4b) :: ic0
    integer(i4b) :: ir0
    integer(i4b) :: ic1
    integer(i4b) :: ir1
    integer(i4b) :: length
    integer(i4b) :: flowdir
  end type tChan
  !
  public :: tChan
  public :: split_channel
!
contains
  recursive subroutine split_channel(p_chan, maxlength, xi4)
! ******************************************************************************
    ! -- arguments
    type(tChan), intent(in) :: p_chan
    integer(i4b), intent(in) :: maxlength
    integer(i4b), dimension(:,:), intent(inout) :: xi4
    !
    ! -- locals
    type(tChan) :: c1_chan, c2_chan, c3_chan, c4_chan
    integer(i4b) :: ir, ic, ic_cut, ir_cut, halflength
! ------------------------------------------------------------------------------
    !
    do ir = p_chan%ir0, p_chan%ir1
      do ic = p_chan%ic0, p_chan%ic1
        if (xi4(ic,ir) == 0) then
          xi4(ic,ir) = p_chan%flowdir
        end if
      end do
    end do
    select case(p_chan%flowdir)
    case(js,je)
      if (xi4(p_chan%ic0,p_chan%ir0) > 0) then
        xi4(p_chan%ic0,p_chan%ir0) = -1
      end if
    case(jn,jw)
      if (xi4(p_chan%ic1,p_chan%ir1) > 0) then
        xi4(p_chan%ic1,p_chan%ir1) = -1
      end if
    end select
    !
    if (p_chan%length <= maxlength) then
      return
    end if
    !
    halflength = (p_chan%length - 1)/2
    !
    ! determine cut point
    c1_chan%length = halflength
    c2_chan%length = halflength
    c3_chan%length = halflength
    c4_chan%length = halflength
    if ((p_chan%flowdir == jn).or.(p_chan%flowdir == js)) then ! vertical line
      ic_cut = p_chan%ic0; ir_cut = p_chan%ir0 + 1 + halflength
      !xi4(ic_cut,ir_cut) = -2
      xi4(ic_cut,ir_cut) = -p_chan%flowdir
      !
      c1_chan%flowdir = je
      c2_chan%flowdir = jw
      !
      c1_chan%ir0 = ir_cut; c1_chan%ir1 = ir_cut
      c2_chan%ir0 = ir_cut; c2_chan%ir1 = ir_cut
      !
      c1_chan%ic0 = ic_cut - halflength - 1; c1_chan%ic1 = ic_cut
      c2_chan%ic0 = ic_cut                 ; c2_chan%ic1 = ic_cut + halflength + 1
      !
      c3_chan%flowdir = p_chan%flowdir
      c3_chan%ic0 = p_chan%ic0; c3_chan%ir0 = p_chan%ir0
      c3_chan%ic1 = p_chan%ic0; c3_chan%ir1 = ir_cut
      !
      c4_chan%flowdir = p_chan%flowdir
      c4_chan%ic0 = p_chan%ic0; c4_chan%ir0 = ir_cut
      c4_chan%ic1 = p_chan%ic0; c4_chan%ir1 = p_chan%ir1
      !
      call split_channel(c1_chan, maxlength, xi4) ! west
      call split_channel(c2_chan, maxlength, xi4) ! east
      call split_channel(c3_chan, maxlength, xi4) ! north
      call split_channel(c4_chan, maxlength, xi4) ! south
    else ! horizontal line
      ic_cut = p_chan%ic0 + 1 + halflength; ir_cut = p_chan%ir0
      !xi4(ic_cut,ir_cut) = -2
      xi4(ic_cut,ir_cut) = -p_chan%flowdir
      !
      c1_chan%flowdir = js
      c2_chan%flowdir = jn
      !
      c1_chan%ic0 = ic_cut; c1_chan%ic1 = ic_cut
      c2_chan%ic0 = ic_cut; c2_chan%ic1 = ic_cut
      !
      c1_chan%ir0 = ir_cut - halflength - 1; c1_chan%ir1 = ir_cut
      c2_chan%ir0 = ir_cut                 ; c2_chan%ir1 = ir_cut + halflength + 1
      !
      c3_chan%flowdir = p_chan%flowdir
      c3_chan%ic0 = p_chan%ic0; c3_chan%ir0 = p_chan%ir0
      c3_chan%ic1 = ic_cut;     c3_chan%ir1 = p_chan%ir1
      !
      c4_chan%flowdir = p_chan%flowdir
      c4_chan%ic0 = ic_cut;     c4_chan%ir0 = p_chan%ir0
      c4_chan%ic1 = p_chan%ic1; c4_chan%ir1 = p_chan%ir1
      !
      call split_channel(c1_chan, maxlength, xi4) ! north
      call split_channel(c2_chan, maxlength, xi4) ! south
      call split_channel(c3_chan, maxlength, xi4) ! west
      call split_channel(c4_chan, maxlength, xi4) ! east
    end if
    !
    return
  end subroutine split_channel
end module peano_mod
!
program peano
  ! -- modules
  use utilsmod, only: mxslen, i1b, i2b, i4b, i8b, r4b, r8b, logmsg, errmsg, &
    open_file, create_dir, change_work_dir, swap_slash, RZERO, DZERO, DONE, ta
  use ehdrModule, only: writeflt
  use pcrModule, only: js, jw, je, jn, st, jperm
  use peano_mod
  !
  implicit none
  !
  ! -- local
  real(r8b), parameter :: xll = DZERO, yll = DZERO
  !
  type(tChan) :: chan
  character(len=mxslen) :: f
  integer(i4b), dimension(4) :: lsten
  data lsten/js,jw,je,jn/
  logical :: ldone
  integer(i4b), dimension(:,:), allocatable :: ldd, strahler
  real(r8b), dimension(:,:), allocatable :: npass
  integer(i4b) :: na, exp, nc, nr, ir, ic, jr, jc, kr, kc, ninlet, iinlet, istrat
  integer(i4b) :: length, maxlength, ic0, ir0, ic1, ir1, i, isten, lddval
  real(r8b) :: cs
  ! ------------------------------------------------------------------------------
  na = nargs()-1
  !
  exp = 5 
  !exp = 11
  cs = 1.d0
  maxlength = 1
  !
  nc = 2**exp + 1; nr = nc
  call logmsg('Creating Peano network for ('//ta((/nc/))//','//ta((/nr/))//')...')
  !
  allocate(ldd(nc,nr), npass(nc,nr), strahler(nc,nr))
  do ir = 1, nr
    do ic = 1, nc
      ldd(ic,ir) = 0
      npass(ic,ir) = DZERO
      strahler(ic,ir) = 0
    end do
  end do
  !
  chan%length = nc - 2
  chan%ic0 = (nc - 1)/2 + 1; chan%ic1 = chan%ic0
  chan%ir0 = 1; chan%ir1 = nr
  chan%flowdir = js
  !
  ! create one quarte of peano network
  call logmsg('Computing the local drainage direction...')
  call split_channel(chan, maxlength, ldd)
  
  do ir = 1, nr
    do ic = 1, nc
      if (ldd(ic,ir) < -1) then
        ldd(ic,ir) = abs(ldd(ic,ir))
      end if
    end do
  end do
  if (.false.)then
  do ir = 1, nr
    do ic = 1, nc
      if ((ir ==21).and.ic==13)then
        write(*,*)'break'
      end if
      
      if (ldd(ic,ir) == -1) then
        ! N
        jr = ir - 1; jc = ic
        if (jr >= 1) then
          if (ldd(jc,jr) /= 0) ldd(jc,jr) = -abs(ldd(jc,jr))
        end if
        ! S
        jr = ir + 1; jc = ic
        if (jr <= nr) then
          if (ldd(jc,jr) /= 0) ldd(jc,jr) = -abs(ldd(jc,jr))
        end if
        ! W
        jr = ir; jc = ic - 1
        if (jc >= 1) then
          if (ldd(jc,jr) /= 0) ldd(jc,jr) = -abs(ldd(jc,jr))
        end if
        ! W
        jr = ir; jc = ic + 1
        if (jc <= nc) then
          if (ldd(jc,jr) /= 0) ldd(jc,jr) = -abs(ldd(jc,jr))
        end if
        ldd(ic,ir) = 0
      end if
    end do
  end do
  end if
  !
  !label the outlet
  ldd(chan%ic1,chan%ir1) = -10
  !
  ! compute pass grid
  call logmsg('Computing the number of passes...')
  ninlet = 0
  do ir = 1, nr
    do ic = 1, nc
      if (ldd(ic,ir) == -1) then
        ninlet = ninlet + 1
      end if
    end do
  end do
  call logmsg('# inlets found: '//ta((/ninlet/)))
  !
  iinlet = 0
  do ir = 1, nr
    do ic = 1, nc
      if (ldd(ic,ir) == -1) then
        npass(ic,ir) = DONE
        ! process the inlet
        iinlet = iinlet + 1
        if ((mod(iinlet,int(ninlet/50)) == 0).or.(iinlet == 1).or.(iinlet == ninlet)) then
          call logmsg('Processing inlet '//ta((/iinlet/),'(i8.8)')// &
            '/'//ta((/ninlet/),'(i8.8)')//': ('//ta((/ic/))//','//ta((/ir/))//')...')
        end if
        do i = 1, 4
          isten = lsten(i)
          jc = ic + st(1,isten); jr = ir + st(2,isten)
          if ((jc < 1).or.(jc > nc).or.(jr < 1).or.(jr > nr)) cycle
          lddval = ldd(jc,jr)
          if (lddval == 0) cycle
          !
          ldone = .false.
          npass(jc,jr) = npass(jc,jr) + DONE
          istrat = 1
          strahler(jc,jr) = istrat
          do while (.not.ldone)
            kc = jc + st(1,abs(lddval)); kr = jr + st(2,abs(lddval))
            if ((kc < 1).or.(kc > nc).or.(kr < 0).or.(kr > nr)) then
              call errmsg('Program error')
            end if
            strahler(kc,kr) = 1
            npass(kc,kr) = npass(kc,kr) + DONE
            lddval = ldd(kc,kr)
            if (lddval == -10) then
              ldone = .true.
            end if
            jc = kc; jr = kr
          end do
        end do
      end if
    end do
  end do
  !
  ! convert to logarithmic valued
  if (.false.)then
  do ir = 1, nr
    do ic = 1, nc
      if (npass(ic,ir) /= DZERO) then
        npass(ic,ir) = log(npass(ic,ir))
      end if
    end do
  end do
  end if
  !
  f = 'peano_network'
  call writeflt(f, ldd, nc, nr, xll, yll, cs, 0)
  !
  f = 'peano_network_npass'
  call writeflt(f, npass, nc, nr, xll, yll, cs, DZERO)
  !
  f = 'peano_network_strahler'
  call writeflt(f, strahler, nc, nr, xll, yll, cs, 0)
  !
end program

