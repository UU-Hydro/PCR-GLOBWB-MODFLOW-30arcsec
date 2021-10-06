module peano_mod
  use utilsmod, only: mxslen, i1b, i2b, i4b, i8b, r4b, r8b, logmsg, errmsg, DZERO
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
  interface rotate
    module procedure :: rotate_i4
    module procedure :: rotate_r8
  end interface
  
  public :: tChan
  public :: split_channel, rotate
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
  
subroutine rotate_i4(x_in, x_out)
! ******************************************************************************
    ! -- arguments
    integer(i4b), dimension(:,:), intent(inout) :: x_in
    integer(i4b), dimension(:,:), allocatable, intent(inout) :: x_out
    !
    ! -- locals
    integer(i4b), parameter :: noff = 1
    integer(i4b) :: ir, ic, jr, jc, nc_in, nr_in, nc, nr, n, m, nmid, ic0, ic1
! ------------------------------------------------------------------------------
    nc_in = size(x_in,1); nr_in = nc_in
    nc = nr_in; nr = nc
    nmid = (nc - 1)/2 + 1
    !
    ! debug
    if (.false.)then
    x_in = 0
    n = 0; m = 0
    do ir = 1, nmid
       ic0=nmid-n; ic1=nmid+n
       do ic = ic0, ic1
         m = m + 1
         x_in(ic,ir) = m
       end do
       n = n + 1
    end do
    n = 1
    do ir = nmid + 1, nr
       ic0=1+n; ic1=nc-n
       do ic = ic0, ic1
         m = m + 1
         x_in(ic,ir) = m
       end do
       n = n + 1
    end do
    end if
    !
    if (allocated(x_out)) deallocate(x_out)
    !
    allocate(x_out(nc,nr))
    do ir = 1, nr
      do ic = 1, nc
        x_out(ic,ir) = 0
      end do
    end do
    !
    ic0 = nmid; ic1 = ic0; n = 0
    do ir = 1, nmid
      m = 1
      do ic = ic0, ic1
        jc = nc-2*n + m - 1; jr = m
        x_out(jc,jr) = x_in(ic,ir)
        m = m + 1
      end do
      n = n + 1
      ic0=nmid-n; ic1=nmid+n
    end do
    ic0 = 2; ic1 = nc-1; n = 1
    do ir = nmid+1, nr
      m = 1
      do ic = ic0, ic1
        jc = m; jr = 1 + 2*n + m - 1
        x_out(jc,jr) = x_in(ic,ir)
        m = m + 1
      end do
      n = n + 1
      ic0=1+n; ic1=nc-n
    end do
    !
    return
end subroutine rotate_i4
  
subroutine rotate_r8(x_in, x_out)
! ******************************************************************************
    ! -- arguments
    real(r8b), dimension(:,:), intent(in) :: x_in
    real(r8b), dimension(:,:), allocatable, intent(inout) :: x_out
    !
    ! -- locals
    integer(i4b) :: ir, ic, jr, jc, nc_in, nr_in, nc, nr, n, m
! ------------------------------------------------------------------------------
    !
    ! TODO
    !
    return
  end subroutine rotate_r8
end module peano_mod
!
program peano
  ! -- modules
  use utilsmod, only: mxslen, i1b, i2b, i4b, i8b, r4b, r8b, logmsg, errmsg, &
    open_file, create_dir, change_work_dir, swap_slash, RZERO, DZERO, DONE, ta
  use ehdrModule, only: writeflt
  use pcrModule, only: jsw, js, jse, jw, jp, je, jnw, jn, jne, st, jperm
  !
  !PCR LDD definition:
  !     NW  N  NE 
  !       7 8 9
  !     W 4 5 6 E
  !       1 2 3
  !     SW  S  SE
  !
  use peano_mod
  !
  implicit none
  !
  ! -- local
  integer(i4b), parameter :: maxlength = 1 ! recursion parameter
  real(r8b), parameter :: xll = DZERO, yll = DZERO
  integer(i4b), dimension(9) :: kperm = (/3, 2, 1, 6, 5, 4, 9, 8, 7/)
  !
  type(tChan) :: chan
  character(len=mxslen) :: f, s, fp
  logical :: ldone, linflow
  integer(i1b), dimension(:,:), allocatable :: i1wk2d
  integer(i4b), dimension(4) :: lddvec
  integer(i4b), dimension(:,:), allocatable :: ldd, lddd, ldddfull, i4wk2d, lst
  integer(i4b) :: na, exp, nc, nr, ir, ic, jr, jc, kr, kc, ninlet, iinlet
  integer(i4b) :: ic0, ic1, ir1, i, inbr
  integer(i4b) :: lddval
  integer(i4b) :: ncd, nrd, nout, src_ldd, tgt_ldd, n0, n1, it, nstem, iact
  real(r4b) :: expEnergy
  !real(r8b), dimension(:), allocatable :: stemleng, stemarea, stemdz
  real(r8b), dimension(:,:), allocatable :: area, areafull, deltaZ, z, zfull
  real(r8b) :: cs, slope0, leng, diagleng, sumdz, dslope0
  real(r8b) :: zMin, zMax, areaTot,  areaMax, tgtDz, r8v
  ! ------------------------------------------------------------------------------
  na = nargs()-1
  if (na < 6) call errmsg('Invalid program arguments.')
  ! reference arguments: 3 1. 0 1 0.5 peano_network
  call getarg(1, s); read(s,*) exp
  call getarg(2, s); read(s,*) cs
  call getarg(3, s); read(s,*) zMin
  call getarg(4, s); read(s,*) zMax
  call getarg(5, s); read(s,*) expEnergy
  call getarg(6, fp)
  !
  nc = 2**exp + 1; nr = nc
  call logmsg('Creating Peano network for ('//ta((/nc/))//','//ta((/nr/))//')...')
  call logmsg('Output will be written for ('//ta((/2*nc/))//','//ta((/2*nr/))//')...')
  !
  allocate(ldd(nc,nr))
  do ir = 1, nr
    do ic = 1, nc
      ldd(ic,ir) = 0
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
  !
  do ir = 1, nr
    do ic = 1, nc
      if (ldd(ic,ir) < -1) then
        ldd(ic,ir) = abs(ldd(ic,ir))
      end if
    end do
  end do
  !
  !f = 'peano_network'
  !call writeflt(f, ldd, size(ldd,1), size(ldd,2), xll, yll, cs, 0)
  !
  ! rotate the temporary ldd 45 degrees clockwise
  call rotate(ldd, i4wk2d)
  !
  ! set the rotated ldd direction
  do ir = 1, size(i4wk2d,1)
    do ic = 1, size(i4wk2d,2)
      lddval = i4wk2d(ic,ir)
      if (lddval > 0) then
        select case(lddval)
        case(jn)
          i4wk2d(ic,ir) = jne
        case(js)
          i4wk2d(ic,ir) = jsw
        case(je)
          i4wk2d(ic,ir) = jse
        case(jw)
          i4wk2d(ic,ir) = jnw
        end select
      end if
    end do
  end do
  !
  ! dual mesh ldd (final ldd)
  ncd = nc-1; nrd = ncd
  allocate(lddd(ncd,nrd))
  do ir = 1, nrd
    do ic = 1, ncd
      lddd(ic,ir) = 0
    end do
  end do
  !
  do ir = 1, nrd
    do ic = 1, ncd
      lddvec = 0
      ! NW
      jr = ir; jc = ic; lddval = i4wk2d(jc,jr)
      if (lddval > 0) lddvec(1) = lddval
      ! NE
      jr = ir; jc = ic+1; lddval = i4wk2d(jc,jr)
      if (lddval > 0) lddvec(2) = lddval
      ! SW
      jr = ir+1; jc = ic; lddval = i4wk2d(jc,jr)
      if (lddval > 0) lddvec(3) = lddval
      ! SE
      jr = ir+1; jc = ic+1; lddval = i4wk2d(jc,jr)
      if (lddval > 0) lddvec(4) = lddval
      !
      nout = 0; tgt_ldd = 0
      !
      ! NW
      src_ldd = lddvec(1)
      if (src_ldd /= 0) then
        if ((src_ldd == jw).or.(src_ldd == jsw)) then !HOR
          nout = nout + 1; tgt_ldd = jw
        end if
        if ((src_ldd == jn).or.(src_ldd == jne)) then !VER
          nout = nout + 1; tgt_ldd = jn
        end if
      end if
      ! NE
      src_ldd = lddvec(2)
      if (src_ldd /= 0) then
        if ((src_ldd == je).or.(src_ldd == jse)) then !HOR
          nout = nout + 1; tgt_ldd = je
        end if
        if ((src_ldd == jn).or.(src_ldd == jnw)) then !VER
          nout = nout + 1; tgt_ldd = jn
        end if
      end if
      ! SW
      src_ldd = lddvec(3)
      if (src_ldd /= 0) then
        if ((src_ldd == jw).or.(src_ldd == jnw)) then !HOR
          nout = nout + 1; tgt_ldd = jw
        end if
        if ((src_ldd == js).or.(src_ldd == jse)) then !VER
          nout = nout + 1; tgt_ldd = js
        end if
      end if
      ! SE
      src_ldd = lddvec(4)
      if (src_ldd /= 0) then
        if ((src_ldd == je).or.(src_ldd == jne)) then !HOR
          nout = nout + 1; tgt_ldd = je
        end if
        if ((src_ldd == js).or.(src_ldd == jsw)) then !VER
          nout = nout + 1; tgt_ldd = js
        end if
      end if
      if (nout == 0) then
        ! check
        tgt_ldd = maxval(lddvec)
        do i = 1, 4
          src_ldd = lddvec(i)
          if (src_ldd > 0) then
            if (src_ldd /= tgt_ldd) then
              call errmsg('Program error 1')
            end if
          end if
        end do
      else
        if (nout > 1) then
          call errmsg('Program error 1')
        end if
      end if
      lddd(ic,ir) = tgt_ldd
    end do
  end do
  ! set the outlet
  lddd(1,nrd) = jp
  !
  !f = trim(fp)//'_ldd'
  !call writeflt(f, lddd, size(lddd,1), size(lddd,2), xll, yll, cs, 0)
  !
  allocate(ldddfull(2*ncd,2*nrd))
  do ir = 1, 2*nrd
    do ic = 1, 2*ncd
      ldddfull(ic,ir) = DZERO
    end do
  end do
  do ir = 1, nrd
    do ic = 1, ncd
      ldddfull(ic,ir+nrd) = lddd(ic,ir)
      ldddfull(2*ncd-ic+1,ir+nrd) = kperm(lddd(ic,ir))
    end do
  end do
  do ir = 1, nrd
    do ic = 1, 2*ncd
      ldddfull(ic,ir) = jperm(kperm(ldddfull(ic,2*nrd-ir+1)))
    end do
  end do
  !
  f = trim(fp)//'_ldd_full'
  call writeflt(f, ldddfull, size(ldddfull,1), size(ldddfull,2), xll, yll, cs, 0)
  if (allocated(ldddfull)) deallocate(ldddfull)
  !
  !======================================================
  ! AREA
  !======================================================
  !
  ! process only the upper triangular
  allocate(area(ncd,nrd))
  do ir = 1, nrd
    do ic = 1, ncd
      area(ic,ir) = DZERO
    end do
  end do
  !
  ! label the inlet cells
  ninlet = 0
  do ir = 1, nrd
    do ic = 1, ncd-ir+1
    !do ic = 1, ncd
      linflow = .false.
      do inbr = 1, 9
        if (inbr == 5) cycle
        jc = ic + st(1,inbr); jr = ir + st(2,inbr)
        if ((jc < 1).or.(jc > ncd).or.(jr < 1).or.(jr > nrd)) cycle
        lddval = lddd(jc,jr) !neighbor ldd
        if (lddval == jperm(inbr)) linflow = .true.
      end do
      if (.not.linflow) then
        area(ic,ir) = cs**2
        ninlet = ninlet + 1
      end if
    end do
  end do
  call logmsg('# inlets found: '//ta((/ninlet/)))
  !
  if (allocated(i4wk2d)) deallocate(i4wk2d)
  allocate(i1wk2d(ncd,nrd), i4wk2d(ncd,nrd))
  do ir = 1, nrd
    do ic = 1, ncd
      i1wk2d(ic,ir) = 0
      i4wk2d(ic,ir) = 0
    end do
  end do
  !
  allocate(lst(2,ninlet)); n0 = 0
  do ir = 1, nrd
    do ic = 1, ncd-ir+1
      if (area(ic,ir) == cs**2) then
        n0 = n0 + 1
        lst(1,n0) = ic; lst(2,n0) = ir
      end if
    end do
  end do
  !
  ! set ldd lower diagonal to negative value!
  do ir = 2, nrd
    ic0 = ncd-ir+2; ic1 = ncd
    do ic = ic0, ic1
      lddd(ic,ir) = -abs(lddd(ic,ir))
    end do
  end do
  !
  call logmsg('Computing area...')
  ldone = .false.; it = 0
  do while(.not.ldone)
    it = it + 1
    !call logmsg('Iteration '//ta((/it/))//': # cells to process: '//ta((/n0/),'(i10.10)'))
    do i = 1, n0
      ic = lst(1,i); ir = lst(2,i)
      i4wk2d(ic,ir) = it
    end do
    n1 = 0
    do i = 1, n0 ! loop over node list
      ic = lst(1,i); ir = lst(2,i); lddval = lddd(ic,ir)
      jc = ic + st(1,lddval); jr = ir + st(2,lddval) ! downstream neighbor
      !
      ! check for neighbors of neighbors
      linflow = .true.
      do inbr = 1, 9
        if (inbr == 5) cycle
        kc = jc + st(1,inbr); kr = jr + st(2,inbr)
        if ((kc < 1).or.(kc > ncd).or.(kr < 1).or.(kr > nrd)) cycle
        lddval = lddd(kc,kr) !neighbor ldd
        if (lddval < 0) cycle ! diagonal
        if (lddval == jperm(inbr)) then
          if (i4wk2d(kc,kr) == 0) then
            linflow = .false.
          end if
        end if
      end do
      if (.not.linflow) cycle
      !
      ! add the area
      area(jc,jr) = cs**2
      do inbr = 1, 9
        if (inbr == 5) cycle
        kc = jc + st(1,inbr); kr = jr + st(2,inbr)
        if ((kc < 1).or.(kc > ncd).or.(kr < 1).or.(kr > nrd)) cycle
        lddval = lddd(kc,kr) !neighbor ldd
        if (lddval < 0) cycle ! diagonal
        if (lddval == jperm(inbr)) then
          if (area(kc,kr) == DZERO) then
             call errmsg('Program error.')
          end if
         area(jc,jr) = area(jc,jr) + area(kc,kr)
        end if
      end do
      if ((ic == 1).and.(ir == nrd)) then ! outlet reached
        ! do nothing
      else
        if (i1wk2d(jc,jr) == 0) then
          n1 = n1 + 1
        end if
        i1wk2d(jc,jr) = 1
      end if
    end do ! end loop over node list
    !
    ! add neighboring node
    if (n1 == 0) then
      ldone = .true.
    else
      n0 = n1
      if (allocated(lst)) deallocate(lst)
      allocate(lst(2,n0))
      !
      n0 = 0
      do ir = 1, nrd
        do ic = 1, ncd
          if (i1wk2d(ic,ir) == 1) then
            n0 = n0 + 1
            lst(1,n0) = ic; lst(2,n0) = ir
            i1wk2d(ic,ir) = 0
          end if
        end do
      end do
      if (n0 /= n1) then
        call errmsg('Program error with listing')
      end if
    end if
  end do
  !
  ! copy the lower triangular and update the main diagonal
  do ir = 1, nrd
    do ic = 1, ncd-ir+1
      jr = nrd - ir + 1; jc = ncd - ic + 1
      area(jc,jr) = area(jc,jr) + area(ir,ic)
    end do
  end do
  ! correct the diagonal
  do ir = 1, nrd
    ic = nrd-ir+1
    area(ic,ir) = area(ic,ir) - ir*cs*cs
  end do
  !
  ! check
  if (real(ncd*nrd*cs**2,r8b) /= area(1,nrd)) then
    call errmsg('Program error')
  end if
  !
  allocate(areafull(2*ncd,2*nrd))
  do ir = 1, nrd
    do ic = 1, ncd
      areafull(ic,ir+nrd)         = area(ic,ir)
      areafull(2*ncd-ic+1,ir+nrd) = area(ic,ir)
    end do
  end do
  do ir = 1, nrd
    do ic = 1, 2*ncd
      areafull(ic,ir) = areafull(ic,2*nrd-ir+1)
    end do
  end do
  
  f = trim(fp)//'_area_full'
  call writeflt(f, areafull, size(areafull,1), size(areafull,2), xll, yll, cs, DZERO)
  !
  r8v = DZERO
  do ir = 1, 2*nrd
    do ic = 1, 2*ncd
      areafull(ic,ir) = log10(areafull(ic,ir)/(cs**2))
      r8v = max(r8v,areafull(ic,ir))
    end do
  end do
  do ir = 1, 2*nrd
    do ic = 1, 2*ncd
      areafull(ic,ir) = areafull(ic,ir)/r8v
    end do
  end do
  f = trim(fp)//'_area_full_log'
  call writeflt(f, areafull, size(areafull,1), size(areafull,2), xll, yll, cs, -9999d0)
  !
  if (allocated(areafull)) deallocate(areafull)
  !
  !======================================================
  ! DZ
  !======================================================
  !
  diagleng = sqrt(cs**2 + cs**2)
  areaTot = nrd*ncd*cs**2
  !
  ! determine the main stem
  call logmsg('Computing main stem length and slope0...')
  !
  nstem = 0; sumdz = DZERO; tgtDz = zMax-zMin
  do ir = nrd-1, 1, -1
    nstem = nstem + 1
    ic = ncd-ir+1
    sumdz = sumdz + diagleng*((area(ic,ir)/areaTot)**(expEnergy-1.))
  end do
  slope0 = 10**(log10(tgtDz/sumdz)/(expEnergy-1.))  
  call logmsg('nstem: '//ta((/nstem/)))
  call logmsg('slope0: '//ta((/slope0/)))
  !
  allocate(deltaZ(ncd,nrd))
  do ir = 1, nrd
    do ic = 1, ncd
      deltaZ(ic,ir) = DZERO
    end do
  end do
  do ir = 1, nrd
    do ic = 1, ncd-ir+1
      if ((ir == nrd).and.(ic == 1)) then
        cycle
      end if
      lddval = abs(lddd(ic,ir))
      select case(lddval)
      case(jn,js,jw,je)
        leng = cs
      case(jnw,jne,jsw,jse)
        leng = diagleng
      end select
      deltaZ(ic,ir) = slope0*area(ic,ir)/areaTot
      deltaZ(ic,ir) = deltaZ(ic,ir)**(expEnergy-1.)
      deltaZ(ic,ir) = leng*deltaZ(ic,ir)
    end do
  end do
  !
  f = trim(fp)//'_deltaZ'
  call writeflt(f, deltaZ, size(deltaZ,1), size(deltaZ,2), xll, yll, cs, DZERO)
  !
  !======================================================
  ! Z
  !======================================================
  if (allocated(area)) deallocate(area)
  allocate(z(ncd,nrd))
  !
  do ir = 1, nrd
    do ic = 1, ncd
      i1wk2d(ic,ir) = 0
      i4wk2d(ic,ir) = 0
      z(ic,ir) = DZERO
    end do
  end do
  z(1,nrd) = zmin
  !
  ldone = .false.; it = 0; n0 = 1
  if (allocated(lst)) deallocate(lst)
  allocate(lst(2,1))
  lst(1,1) = 1; lst(2,1) = nrd ! outlet
  !
  call logmsg('Computing z...')  
  do while(.not.ldone)
    it = it + 1
    !call logmsg('Iteration '//ta((/it/))//': # cells to process: '//ta((/n0/),'(i10.10)'))
    do i = 1, n0
      ic = lst(1,i); ir = lst(2,i)
      i4wk2d(ic,ir) = it
    end do
    n1 = 0
    do i = 1, n0 ! loop over node list
      ic = lst(1,i); ir = lst(2,i)
      do inbr = 1, 9
        if (inbr == 5) cycle
        kc = ic + st(1,inbr); kr = ir + st(2,inbr)
        if ((kc < 1).or.(kc > ncd).or.(kr < 1).or.(kr > nrd)) cycle
        lddval = lddd(kc,kr) !neighbor ldd
        if (lddval < 0) cycle ! diagonal
        if (lddval == jperm(inbr)) then
          z(kc,kr) = z(ic,ir) + deltaZ(kc,kr)
          if (i1wk2d(kc,kr) == 0) then
            n1 = n1 + 1
          end if
          i1wk2d(kc,kr) = 1
        end if
      end do
    end do ! end loop over node list
    !
    ! add neighborign node
    if (n1 == 0) then
      ldone = .true.
    else
      n0 = n1
      if (allocated(lst)) deallocate(lst)
      allocate(lst(2,n0))
      !
      n0 = 0
      do ir = 1, nrd
        do ic = 1, ncd
          if (i1wk2d(ic,ir) == 1) then
            n0 = n0 + 1
            lst(1,n0) = ic; lst(2,n0) = ir
            i1wk2d(ic,ir) = 0
          end if
        end do
      end do
      if (n0 /= n1) then
        call errmsg('Program error with listing')
      end if
    end if
  end do
  !
  ! copy the lower triangular
  do ir = 1, nrd-1
    do ic = 1, ncd-ir
      jr = nrd - ir + 1; jc = ncd - ic + 1
      z(jc,jr) = z(jc,jr) + z(ir,ic)
    end do
  end do
  !
  zMax = zMin
  do ir = 1, nrd
    do ic = 1, ncd
      zMax = max(z(ic,ir),zMax)
    end do
  end do
  call logmsg('Min/max z-value: '//ta((/zMin/))//', '//ta((/zMax/)))
  !
  f = trim(fp)//'_z'
  call writeflt(f, z, size(z,1), size(z,2), xll, yll, cs, -9999d0)
  !
  ! clean up
  if (allocated(deltaZ)) deallocate(deltaZ)
  if (allocated(ldd)) deallocate(ldd)
  if (allocated(lddd)) deallocate(lddd)
  if (allocated(i1wk2d)) deallocate(i1wk2d)
  if (allocated(i4wk2d)) deallocate(i4wk2d)  
  !======================================================
  ! Z FULL
  !======================================================
  allocate(zfull(2*ncd,2*nrd))
  do ir = 1, nrd
    do ic = 1, ncd
      zfull(ic,ir+nrd)         = z(ic,ir)
      zfull(2*ncd-ic+1,ir+nrd) = z(ic,ir)
    end do
  end do
  do ir = 1, nrd
    do ic = 1, 2*ncd
      zfull(ic,ir) = zfull(ic,2*nrd-ir+1)
    end do
  end do
  !
  f = trim(fp)//'_z_full'
  call writeflt(f, zfull, size(zfull,1), size(zfull,2), xll, yll, cs, -9999d0)
  !
end program

