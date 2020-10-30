  module interface_mod
  ! interfaces
  interface  
    subroutine calc_catch(cat, catid0, catptr, bb, ldd, lddmv, catbb, in_mxcatid)
      use pcrModule, only: i1b, i4b 
      use utilsmod, only: tBB
      implicit none
      integer(i4b), dimension(:,:), intent(inout) :: cat
      integer(i4b), intent(in) :: catid0
      integer(i1b), dimension(:,:), intent(in) :: catptr
      type(tBB), intent(in) :: bb
      integer(i1b), dimension(:,:), intent(in) :: ldd
      integer(i1b), intent(in) :: lddmv
      type(tBB), dimension(:), allocatable, intent(out) :: catbb
      integer(i4b), optional, intent(in) :: in_mxcatid
    end subroutine calc_catch
    !
    subroutine calc_acc(acc, cat, icat, bb, ldd, lddmv, icir_pit)
      use pcrModule, only: i1b, i4b, r8b
      use utilsmod, only: tBB
      implicit none
      real(r8b), dimension(:,:), intent(inout) :: acc
      integer(i4b), dimension(:,:), intent(in) :: cat
      integer(i4b), intent(in) :: icat
      type(tBB), intent(in) :: bb
      integer(i1b), dimension(:,:), intent(inout) :: ldd
      integer(i1b), intent(in) :: lddmv
      integer(i4b), dimension(2), intent(out) :: icir_pit
    end subroutine calc_acc
    !
    subroutine calc_stem(acc, icir_pit, cat, icat, bb, ldd, lddmv, nstem, icir_stem, acc_stem)
      use pcrModule, only: i1b, i4b, r8b
      use utilsmod, only: tBB
      implicit none
      real(r8b), dimension(:,:), intent(inout) :: acc
      integer(i4b), dimension(2), intent(in) :: icir_pit
      integer(i4b), dimension(:,:), intent(in) :: cat
      integer(i4b), intent(in) :: icat
      type(tBB), intent(in) :: bb
      integer(i1b), dimension(:,:), intent(inout) :: ldd
      integer(i1b), intent(in) :: lddmv
      integer(i4b), intent(out) :: nstem
      integer(i4b), dimension(:,:), allocatable, intent(out) :: icir_stem
      real(r8b), dimension(:), allocatable, intent(out) :: acc_stem
    end subroutine calc_stem
    !
    function cat_size(cat, icat, bb, areag) result(area)
      use pcrModule, only: i4b, r8b
      use utilsmod, only: tBB
      implicit none
      integer(i4b), dimension(:,:), intent(in) :: cat
      integer(i4b), intent(in) :: icat
      type(tBB), intent(in) :: bb
      real(r8b), dimension(:), intent(in) :: areag
      real(r8b) :: area
    end function cat_size
    !
    subroutine cat_unique(cat, bb, area, areag, gir0)
      use pcrModule, only: i4b, r8b
      use utilsmod, only: tBB
      implicit none
      integer(i4b), dimension(:,:), intent(inout) :: cat
      type(tBB), dimension(:), allocatable, intent(out) :: bb
      real(r8b), dimension(:), allocatable :: area
      real(r8b), dimension(:), intent(in) :: areag
      integer(i4b), intent(in) :: gir0
    end subroutine cat_unique
    !
    recursive subroutine refine_catch(cat, icat, jcat, pbb, catptr, acc, &
      icir_pit, ldd, lddmv, &
      idepth, maxdepth, maxtrib, minarea, areag, achk)
      use pcrModule, only: i1b, i4b, r8b
      use utilsmod, only: tBB
      implicit none
      integer(i4b), dimension(:,:), intent(inout) :: cat
      integer(i4b),                 intent(in)    :: icat
      integer(i4b),                 intent(inout) :: jcat
      type(tBB),                    intent(in)    :: pbb
      integer(i1b), dimension(:,:), intent(inout) :: catptr
      real(r8b),    dimension(:,:), intent(inout) :: acc
      integer(i4b), dimension(2),   intent(in)    :: icir_pit
      integer(i1b), dimension(:,:), intent(inout) :: ldd
      integer(i1b),                 intent(in)    :: lddmv
      integer(i4b),                 intent(inout) :: idepth
      integer(i4b),                 intent(in)    :: maxdepth
      integer(i4b),                 intent(in)    :: maxtrib
      real(r8b),                    intent(in)    :: minarea
      real(r8b), dimension(:),      intent(in)    :: areag
      integer(i4b),                 intent(inout) :: achk
    end subroutine refine_catch
  end interface
  
  end module interface_mod
  
  program createcatch
    ! modules
    use interface_mod
    use pcrModule
    use utilsmod, only: writeasc, writeidf, tPol, readgen, filtergen_i1, tBB, DZERO, quicksort_d, ta, errmsg
    use imod_idf, only: idfobj, idfread, imod_utl_printtext, idfdeallocatex
    
    implicit none
    
    real(r8b), parameter :: sqkm = 1000000.d0 !m2
    
    type(tMap), pointer :: map
    type(tPol), dimension(:), allocatable :: p
    type(tBB) :: catptrbb
    type(tBB), dimension(:), allocatable :: bb, bb_sub
    logical :: ok, flg
    character(len=1024) :: f_ldd, f_gen, f_area, s, fmt
    integer(i1b) :: lddmv
    integer(i1b), dimension(:,:), allocatable :: ldd, catptr
    integer(i4b) :: maxtrib
    integer(i4b) :: nr, nc, ir, ic, jr, jc, kr, kc, ic0, ic1, ir0, ir1
    integer(i4b) :: catid, mxcatid, n, m, inbr, icat, jcat, kcat, icat0, icat1
    integer(i4b) :: idepth, maxdepth
    integer(i4b), dimension(2) :: icir_pit
    integer(i4b), dimension(:,:), allocatable :: cat !, wrk
    integer(i4b), dimension(:,:), allocatable :: icir_stem
    real(r8b) :: area, minarea
    real(r8b), dimension(:), allocatable :: areag
    real(r8b), dimension(:), allocatable :: cat_area
    real(r8b), dimension(:,:), allocatable :: acc
    real(r8b), dimension(:), allocatable :: acc_stem
    !
    integer(i4b), dimension(:), allocatable :: ind, indinv
    !
    real(r4b) :: cs, xul, yul, xmin, ymin
    logical :: lstop = .false.
    integer(i4b) :: areachk
    !
    type(idfobj) :: idf
! ------------------------------------------------------------------------------
    
    call getarg(1,f_ldd)
    call getarg(2,f_gen)
    call getarg(3,f_area)
   
    ! read the gen file for clipping
    p = readgen(f_gen)
    !
    ! read the map file
    allocate(map)
    ok = map%read_header(f_ldd, 0)
    cs  = map%header%cellsizex
    xul = map%header%xul
    yul = map%header%yul
    nc = map%header%nrcols
    nr = map%header%nrrows
    lddmv = 0
    !
    ! read the area
    call imod_utl_printtext('Reading '//trim(f_area),0)
    if (.not.idfread(idf,f_area,1)) then
      call imod_utl_printtext('Could not read '//trim(f_area),2)
    end if
    call imod_utl_printtext('Done reading '//trim(f_area),0)
    ! check
    if (idf%nrow /= nr) then
      call errmsg("Invalid number of rows")
    end if
    allocate(areag(nr))
    do ir = 1, nr
      areag(ir) = idf%x(1,ir)
    end do
    call idfdeallocatex(idf)
    !
    flg = .false.; ir0 = 1; ir1 = nr; ic0 = 1; ic1 = nc
    !
    ! debug: seperate catchments
    !flg = .true.; ir0 = 1; ir1 = 9000; ic0 = 1; ic1 = 16000; kcat = 641508 ! Missisippi
    !flg = .true.; ir0 = 9000; ir1 = 17000; ic0 = 11500; ic1 = 17500; kcat = 30715 ! Amazone
    !flg = .true.; ir0 = 6000; ir1 = 15000; ic0 = 19500; ic1 = 28000; kcat = 114403 ! Africa
    flg = .true.; ir0 = 4380; ir1 = 5300; ic0 = 21800; ic1 = 23100; kcat = 2790 ! Rhine-Meuse
    
    xmin = xul + (ic0-1)*cs; ymin = yul-ir1*cs ! ymin = yul-9000*cs
    !
    call map%read_block(ldd, ir0, ir1, ic0, ic1, lddmv)
    call map%close(); call map%clean()
    
    if (.not.flg) call filtergen_i1(p, ldd, xmin, ymin, cs, lddmv) !-->GLOBAL
    !
    nc = size(ldd,1); nr = size(ldd,2)
    !
    ! set boundary
    write(*,*) 'Setting ldd boundary...'
    do ir = 1, nr
      if (ldd(1,ir) /= lddmv) then
        ldd(1,ir) = jp
      end if
      if (ldd(nc,ir) /= lddmv) then
        ldd(nc,ir) = jp
      end if
    end do
    do ic = 1, nc
      if (ldd(ic,1) /= lddmv) then
        ldd(ic,1) = jp
      end if
      if (ldd(ic,nr) /= lddmv) then
        ldd(ic,nr) = jp
      end if
    end do
    !
    write(*,*) 'Computing catchments...'
    allocate(cat(nc,nr),catptr(nc,nr))
    do ir = 1, nr
      do ic = 1, nc
        if (ldd(ic,ir) /= lddmv) then
          catptr(ic,ir) = 1
          catptrbb%ir0 = min(catptrbb%ir0, ir); catptrbb%ir1 = max(catptrbb%ir1, ir)
          catptrbb%ic0 = min(catptrbb%ic0, ic); catptrbb%ic1 = max(catptrbb%ic1, ic)
        else
          catptr(ic,ir) = 0
        end if
      end do
    end do
    catptrbb%ncol = catptrbb%ic1 - catptrbb%ic0 + 1; catptrbb%nrow = catptrbb%ir1 - catptrbb%ir0 + 1
    
    call calc_catch(cat, 0, catptr, catptrbb, ldd, lddmv, bb)
    !call writeidf('cat_init.idf', cat, size(cat,1), size(cat,2), &
    !  dble(xmin), dble(ymin), dble(cs), DZERO); stop
    mxcatid = size(bb)
    !
    write(*,*) 'Refining catchments...'
    !
    allocate(acc(nc,nr))!, wrk(nc,nr))
    do ir = 1, nr
      do ic = 1, nc
        acc(ic,ir) = DZERO
      end do
    end do
    !
    ! first, find the catchment boundary
    maxtrib = 4
    catid = mxcatid + 1
!    minarea = 100.d0 * sqkm !m2
    minarea = 1.d0 * sqkm !m2
    minarea = 0.d0 !m2
    maxdepth = 8
    !
    jcat = mxcatid
    !
    if (.not.flg) then
      icat0 = 1; icat1 = mxcatid
    else
      icat0 = kcat; icat1 = kcat
    end if
    !
    do icat = icat0, icat1
      area = cat_size(cat, icat, bb(icat), areag)
      if (area < minarea) cycle
      write(s,*) area
      write(*,'(a,i2.2,a,i7.7,a,i7.7,a)') 'Processing ', maxdepth,' pfafstetter levels for catchment ', icat, '/', mxcatid, ' area '//trim(s)//'...'
      call calc_acc(acc, cat, icat, bb(icat), ldd, lddmv, icir_pit)
      idepth = 0
      areachk = 1
      call refine_catch(cat, icat, jcat, bb(icat), &
        catptr, acc, icir_pit, ldd, lddmv, &
        idepth, maxdepth, maxtrib, minarea, areag, areachk)
      if (flg) then
        do ir = 1, nr
          do ic = 1, nc
            if (cat(ic,ir) < mxcatid) then
              cat(ic,ir) = 0
            end if
          end do
        end do
      end if
    end do
    !
    do ir = 1, nr
      do ic = 1, nc
        cat(ic,ir) = abs(cat(ic,ir))
      end do
    end do
    !
    call cat_unique(cat, bb, cat_area, areag, ir0)
    write(*,'(a)') '# catchments, min. area (km2), max. area (km2): ' // &
      ta((/size(bb)/)) // ', ' // ta((/minval(cat_area)/sqkm/)) // ', ' // &
      ta((/maxval(cat_area)/sqkm/))
    
    !mxcatid = size(bb)
    !
    call writeidf('cat_lev'//ta((/maxdepth/))//'.idf', cat, size(cat,1), size(cat,2), dble(xmin), dble(ymin), dble(cs), DZERO)
    call writeasc('cat_lev'//ta((/maxdepth/))//'.asc', cat, size(cat,1), size(cat,2), dble(xmin), dble(ymin), dble(cs), DZERO)
    !call writeidf('tmp.idf', wrk, size(wrk,1), size(wrk,2), dble(xmin), dble(ymin), dble(cs), DZERO)
    !call writeidf('acc.idf', acc, size(acc,1), size(acc,2), dble(xmin), dble(ymin), dble(cs), DZERO)
    !call writeidf('tmp.idf', tmp, size(tmp,1), size(tmp,2), xmin, ymin, cs, 0.)
    
    !call map%read_data()
    !call map%get_val(1, 1, i1val, i1mv)
    !call map%get_val(13529, 9550, i1val, i1mv)
    !call map%get_i1ar(ldd, nodata, imin, imax)
  end program

  subroutine calc_catch(cat, catid0, catptr, bb, ldd, lddmv, catbb, in_mxcatid)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use pcrModule, only: i1b, i4b, &
      jsw, js, jse, jw, jp, je, jnw, jn, jne, st
    use utilsmod, only: tBB, writeidf
    !
    implicit none
    !
    ! -- dummy
    integer(i4b), dimension(:,:), intent(inout) :: cat
    integer(i4b), intent(in) :: catid0
    integer(i1b), dimension(:,:), intent(in) :: catptr
    type(tBB), intent(in) :: bb
    integer(i1b), dimension(:,:), intent(in) :: ldd
    integer(i1b), intent(in) :: lddmv
    type(tBB), dimension(:), allocatable, intent(out) :: catbb
    integer(i4b), optional, intent(in) :: in_mxcatid
    !
    ! -- local
    logical :: lfull, lsetcat
    integer(i1b) :: v, w
    integer(i4b) :: nc, nr, n, catid, mxcatid, i, j, &
      ic, ir, jc, jr, kc, kr, mxi4wk
    integer(i4b), dimension(:,:), allocatable :: i4wk
! ------------------------------------------------------------------------------
    !
    lsetcat = .true.
    if (present(in_mxcatid)) then
      lsetcat = .false.
      mxcatid = in_mxcatid
    end if
    !
    nc = size(cat,1); nr = size(cat,2)
    !
    mxi4wk = 2*nr + 2*nc
    allocate(i4wk(2,mxi4wk))
    !
    if (lsetcat) then
      catid = catid0
      do ir = bb%ir0, bb%ir1
        do ic = bb%ic0, bb%ic1
          v = ldd(ic,ir)
          if (v == lddmv) cycle
          if (catptr(ic,ir) == 0) cycle
          cat(ic,ir) = 0
          if (v == jp) then
            catid = catid + 1
            cat(ic,ir) = catid
          end if
        end do
      end do
      mxcatid = catid
    end if
    !
    if (allocated(catbb)) then
      deallocate(catbb)
    end if
    allocate(catbb(max(1,mxcatid-catid0)))
    !
    do ir = bb%ir0, bb%ir1
      do ic = bb%ic0, bb%ic1
        v = ldd(ic,ir)
        if (v == lddmv) cycle
        if (catptr(ic,ir) == 0) cycle
        if (v == jp) cycle
        if (cat(ic,ir) > 0) cycle
        !
        ! begin trace
        catid = 0
        jc = ic; jr = ir; w = v
        n = 1; i4wk(1,n) = jc; i4wk(2,n) = jr !starting point
        lfull = .false.
        do while(.true.)
          kc = jc + st(1,w); kr = jr + st(2,w) !next point
          if ((kc < 1).or.(kc > nc).or.(kr < 1).or.(kr > nr)) exit
          !if (catptr(kc,kr) == 0) cycle
          w = ldd(kc,kr)
          if (w == lddmv) then
            write(*,*) 'Subroutine "catch": program error 1.'
            stop
          end if
          n = n + 1
          if (n > mxi4wk) then
            write(*,*) 'Subroutine "catch": increase mxi4wk.'
            stop
          end if
          i4wk(1,n) = kc; i4wk(2,n) = kr
          if (w == jp) then
            catid = cat(kc,kr)
            lfull = .true.
            exit
          end if
          if (cat(kc,kr) > 0) then
            catid = cat(kc,kr)
            exit
          end if
          !
          jc = kc; jr = kr
        end do
        !
        if (catid == 0) then
          call writeidf('error.idf', cat, size(cat,1), size(cat,2), -180d0, 15d0, dble(8.3333338E-03), 0.d0)
          write(*,*) 'Subroutine "catch": program error 2.'
          stop
        end if
        !
        ! fill in the catchment and set bb
        do i = 1, n
          kc = i4wk(1,i); kr = i4wk(2,i)
          cat(kc,kr) = catid
        end do
        !
      end do
    end do
    
    do ir = bb%ir0, bb%ir1
      do ic = bb%ic0, bb%ic1
        if (catptr(ic,ir) == 0) cycle
        catid = cat(ic,ir)
        if ((catid < catid0).or.(catid > mxcatid)) then
          write(*,*) 'Subroutine "catch": program error 3.'
        end if
        i = catid - catid0
        catbb(i)%ir0 = min(catbb(i)%ir0, ir)
        catbb(i)%ir1 = max(catbb(i)%ir1, ir)
        catbb(i)%ic0 = min(catbb(i)%ic0, ic)
        catbb(i)%ic1 = max(catbb(i)%ic1, ic)
      end do
    end do
    do i = 1, size(catbb)
      catbb(i)%ncol = catbb(i)%ic1 - catbb(i)%ic0 + 1
      catbb(i)%nrow = catbb(i)%ir1 - catbb(i)%ir0 + 1
    end do
    !
    return
  end subroutine calc_catch
  
  subroutine calc_acc(acc, cat, icat, bb, ldd, lddmv, icir_pit)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use pcrModule, only: i1b, i4b, r8b, &
      jsw, js, jse, jw, jp, je, jnw, jn, jne, st, jperm 
    use utilsmod, only: tBB, DZERO, DONE, i4wk2d
    !
    implicit none
    !
    ! -- dummy
    real(r8b), dimension(:,:), intent(inout) :: acc
    integer(i4b), dimension(:,:), intent(in) :: cat
    integer(i4b), intent(in) :: icat
    type(tBB), intent(in) :: bb
    integer(i1b), dimension(:,:), intent(inout) :: ldd
    integer(i1b), intent(in) :: lddmv
    integer(i4b), dimension(2), intent(out) :: icir_pit
    !
    ! -- local
    logical :: linflow
    integer(i1b) :: v, w
    integer(i4b) :: nc, nr, ic, ir, jc, jr, kc, kr, i, it, n, m, inbr, jnbr
    integer(i4b) :: ntrib, iact
    !
    integer(i4b) :: mxwk1, mxwk2
    integer(i4b), dimension(:,:), allocatable :: wk1, wk2
    real(r8b), dimension(9) :: r8wk
! ------------------------------------------------------------------------------
    !
    ! init work arrays
    !
    nc = size(acc,1); nr = size(acc,2)
    if (.not.allocated(i4wk2d)) then
      allocate(i4wk2d(nc,nr))
    end if
    !
    do ir = max(bb%ir0-1,1), min(bb%ir1+1,nr)
      do ic = max(bb%ic0,1), min(bb%ic1+1,nc)
        i4wk2d(ic,ir) = 0
      end do
    end do
    !    
    if (allocated(wk1)) deallocate(wk1)
    if (allocated(wk2)) deallocate(wk2)
    mxwk1 =2*bb%nrow*bb%ncol; mxwk2 = mxwk1
    allocate(wk1(2,mxwk1), wk2(2,mxwk2))
    !
    ! set inital cells
    n = 0
    do ir = bb%ir0, bb%ir1
      do ic = bb%ic0, bb%ic1
        if (cat(ic,ir) == icat) then
          ! loop over my neighbors, check for inflow
          linflow = .false.
          do inbr = 1, 9
            jc = ic + st(1,inbr); jr = ir + st(2,inbr)
            if ((jc < 1).or.(jc > nc).or.(jr < 1).or.(jr > nr)) cycle
            if (cat(jc,jr) /= icat) cycle
            v = ldd(jc,jr) !neighbor ldd
            if (v == jperm(inbr)) linflow = .true.
          end do
          if (.not.linflow) then
            n = n + 1
            if (n > mxwk1) then
              write(*,*) 'Subroutine "calc_acc": increase mxwk1.'; stop
            end if
            wk1(1,n) = ic; wk1(2,n) = ir
          end if
        end if
      end do
    end do
    !
    if (n == 0) then
      !write(*,*) 'subroutine "calc_acc": no cells found.'
      return
    end if
    !
    do i = 1, n
      ic = wk1(1,i); ir = wk1(2,i)
      acc(ic,ir) = DONE
    end do
    !
    ! trace upstream -> downstream
    it = 0
    do while(.true.)
      !
      ! label the cells
      it = it + 1
      do i = 1, n
        ic = wk1(1,i); ir = wk1(2,i)
        i4wk2d(ic,ir) = it
      end do
      !
      m = 0
      do i = 1, n
        ic = wk1(1,i); ir = wk1(2,i); v = ldd(ic,ir)
        if ((v == jp).and.(cat(ic,ir) == icat)) then !pit
          icir_pit(1) = ic; icir_pit(2) = ir
          !write(*,*) 'Subroutine "calc_acc": pit found, exiting...'
          exit
        end if
        !
        jc = ic + st(1,v); jr = ir + st(2,v) ! downstream neighbor
        if ((jc < 1).or.(jc > nc).or.(jr < 1).or.(jr > nr)) cycle
        !
        ! check for neighbors
        linflow = .true.
        do inbr = 1, 9
          if (inbr == 5) cycle
          kc = jc + st(1,inbr); kr = jr + st(2,inbr)
          if ((kc < 1).or.(kc > nc).or.(kr < 1).or.(kr > nr)) cycle
          if (cat(kc,kr) /= icat) cycle
          w = ldd(kc,kr) !neighbor ldd
          if (w == jperm(inbr)) then
            if (i4wk2d(kc,kr) == 0) then
              linflow = .false.
            end if
          end if
        end do
        if (.not.linflow) cycle
        !
        ! add flux
        acc(jc,jr) = 1
        do inbr = 1, 9
          if (inbr == 5) cycle
          kc = jc + st(1,inbr); kr = jr + st(2,inbr)
          if ((kc < 1).or.(kc > nc).or.(kr < 1).or.(kr > nr)) cycle
          if (cat(kc,kr) /= icat) cycle
          w = ldd(kc,kr) !neighbor ldd
          if (w == jperm(inbr)) then
            acc(jc,jr) = acc(jc,jr) + acc(kc,kr)
          end if
        end do
        !
        ! add cell to list
        m = m + 1
        if (m > mxwk2) then
          write(*,*) 'Subroutine "calc_acc": increase mxwk2.'; stop
        end if
        wk2(1,m) = jc; wk2(2,m) = jr
      end do
      !
      if (m == 0) then
        !write(*,*) 'Subroutine "calc_acc": done, exiting.'
        exit !done
      end if
      !
      ! mask unique cells
      do i = 1, m
        jc = wk2(1,i); jr = wk2(2,i)
        if (ldd(jc,jr) > 0) then
          ldd(jc,jr) = -abs(ldd(jc,jr))
        else
          wk2(1,i) = -wk2(1,i) ! label jc
        end if
      end do
      !
      ! get unique cells
      n = 0
      do i = 1, m
        jc = wk2(1,i); jr = wk2(2,i)
        if (jc > 0) then
          n = n + 1
          wk1(1,n) = jc; wk1(2,n) = jr
          ldd(jc,jr) = abs(ldd(jc,jr))
        end if
      end do
      !write(*,*) 'm --> n', m, n
    end do
    !
    return
  end subroutine calc_acc
  
  subroutine calc_stem(acc, icir_pit, cat, icat, bb, ldd, lddmv, &
    nstem, icir_stem, acc_stem)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use pcrModule, only: i1b, i4b, r8b, &
      jsw, js, jse, jw, jp, je, jnw, jn, jne, st, jperm 
    use utilsmod, only: tBB, DZERO, DONE
    !
    implicit none
    !
    ! -- dummy
    real(r8b), dimension(:,:), intent(inout) :: acc
    integer(i4b), dimension(2), intent(in) :: icir_pit
    integer(i4b), dimension(:,:), intent(in) :: cat
    integer(i4b), intent(in) :: icat
    type(tBB), intent(in) :: bb
    integer(i1b), dimension(:,:), intent(inout) :: ldd
    integer(i1b), intent(in) :: lddmv
    integer(i4b), intent(out) :: nstem
    integer(i4b), dimension(:,:), allocatable, intent(out) :: icir_stem
    real(r8b), dimension(:), allocatable, intent(out) :: acc_stem
    !
    ! -- local
    logical :: linflow
    integer(i1b) :: v
    integer(i4b) :: nr, nc, iact, ic, ir, jc, jr, inbr, jnbr
    real(r8b), dimension(9) :: r8wk
! ------------------------------------------------------------------------------
    nc = size(acc,1); nr = size(acc,2)
    !
    nstem = 0
    !
    ! trace the main stem
    do iact = 1, 2
      nstem = 1
      ic = icir_pit(1); ir = icir_pit(2)
      !if ((ic == 12720).and.(ir==815))then
      !  write(*,*) 'Break'
      !end if
      !
      ! add the pit
      if (iact == 2) then
        icir_stem(1,nstem) = ic; icir_stem(2,nstem) = ir
        acc_stem(nstem) = acc(ic,ir)
      end if
      !
      do while(.true.)
        ! loop over inflow neighbors
        r8wk = DZERO
        linflow = .false.
        do inbr = 1, 9
          if (inbr == jp) cycle
          jc = ic + st(1,inbr); jr = ir + st(2,inbr)
          if ((jc < 1).or.(jc > nc).or.(jr < 1).or.(jr > nr)) cycle
          if (cat(jc,jr) /= icat) cycle
          v = ldd(jc,jr) !neighbor ldd
          if (v == jperm(inbr)) then
            r8wk(inbr) = acc(jc,jr)
            linflow = .true.
          end if
        end do
        if (.not.linflow) then
          exit ! done
        end if
        !
        nstem = nstem + 1
        !
        inbr = maxloc(r8wk, dim=1,kind=i4b)
        jc = ic + st(1,inbr); jr = ir + st(2,inbr)
        !
        if (iact == 2) then
          !if ((ic == 9506).and.(ir==5194)) then
          !  write(*,*) 'Break'
          !end if
          icir_stem(1,nstem) = ic; icir_stem(2,nstem) = ir
          r8wk(inbr) = DZERO
          jnbr = maxloc(r8wk, dim=1,kind=i4b)
          if (jnbr == inbr) then
            acc_stem(nstem) = DZERO
          else
            acc_stem(nstem) = r8wk(jnbr)
          end if
        end if
        !
        ic = jc; ir = jr
      end do
      !
      if (iact == 1) then
        if (allocated(icir_stem)) deallocate(icir_stem)
        if (allocated(acc_stem))  deallocate(acc_stem)
        allocate(icir_stem(2,max(1,nstem)))
        allocate(acc_stem(max(1,nstem)))
      end if
    end do !iact
 
    return
  end subroutine calc_stem
  
  function cat_size(cat, icat, bb, areag) result(area)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use pcrModule, only: i4b, r8b
    use utilsmod, only: tBB, DZERO, DONE
    !
    implicit none
    !
    ! -- dummy
    integer(i4b), dimension(:,:), intent(in) :: cat
    integer(i4b), intent(in) :: icat
    type(tBB), intent(in) :: bb
    real(r8b), dimension(:), intent(in) :: areag
    real(r8b) :: area
    !
    ! -- local
    integer(i4b) :: ic, ir
! ------------------------------------------------------------------------------
    area = DZERO
    do ir = bb%ir0, bb%ir1
      do ic = bb%ic0, bb%ic1
        if (cat(ic,ir) == icat) then
          area = area + areag(ir)
        end if
      end do
    end do
  end function cat_size
  !
  subroutine cat_unique(cat, bb, area, areag, gir0)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use pcrModule, only: i4b, r8b
    use utilsmod, only: tBB, DZERO, DONE
    !
    implicit none
    !
    ! -- dummy
    integer(i4b), dimension(:,:), intent(inout) :: cat
    type(tBB), dimension(:), allocatable, intent(out) :: bb
    real(r8b), dimension(:), allocatable :: area
    real(r8b), dimension(:), intent(in) :: areag
    integer(i4b), intent(in) :: gir0
    !
    ! -- local
    integer(i4b) :: nc, nr, ic, ir, n, m, v, w, i
    integer(i4b), dimension(:), allocatable :: i4wk
    real(r8b) :: ar
! ------------------------------------------------------------------------------
    nc = size(cat,1); nr = size(cat,2)
    n = maxval(cat)
    !
    if (allocated(bb)) deallocate(bb)
    if (allocated(area)) deallocate(area)
    !
    allocate(i4wk(max(n,1)))
    do i = 1, n
      i4wk(i) = 0
    end do
    !
    do ir = 1, nr
      do ic = 1, nc
        v = cat(ic,ir)
        if (v /= 0) then
          i4wk(v) = 1
        end if
      end do
    end do
    !
    m = 0
    do i = 1, n
      if (i4wk(i) > 0) then
        m = m + 1
        i4wk(i) = m
      end if
    end do
    !
    if (m == 0) then
      write(*,*) 'Subroutine "cat_unique": no IDs found'
      return
    end if
    !
    allocate(bb(m), area(m))
    !
    area = DZERO
    !
    do ir = 1, nr
      do ic = 1, nc
        v = cat(ic,ir)
        if (v /= 0) then
          w = i4wk(v)
          cat(ic,ir) = w
          bb(w)%ir0 = min(bb(w)%ir0, ir); bb(w)%ir1 = max(bb(w)%ir1, ir)
          bb(w)%ic0 = min(bb(w)%ic0, ic); bb(w)%ic1 = max(bb(w)%ic1, ic)
          area(w) = area(w) + areag(ir+gir0+1)
        end if
      end do
    end do
    !
    deallocate(i4wk)
    !
    return
  end subroutine cat_unique
  
  recursive subroutine refine_catch(cat, icat, jcat, pbb, catptr, acc, &
    icir_pit, ldd, lddmv, &
    idepth, maxdepth, maxtrib, minarea, areag, achk)
! ******************************************************************************
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use interface_mod
    use pcrModule, only: i1b, i4b, r8b, jp
    use utilsmod, only: tBB, quicksort_d, DZERO
  
    implicit none
    !
    ! -- dummy
    integer(i4b), dimension(:,:), intent(inout) :: cat
    integer(i4b),                 intent(in)    :: icat
    integer(i4b),                 intent(inout) :: jcat
    type(tBB),                    intent(in)    :: pbb
    integer(i1b), dimension(:,:), intent(inout) :: catptr
    real(r8b),    dimension(:,:), intent(inout) :: acc
    integer(i4b), dimension(2),   intent(in)    :: icir_pit
    integer(i1b), dimension(:,:), intent(inout) :: ldd
    integer(i1b),                 intent(in)    :: lddmv
    integer(i4b),                 intent(inout) :: idepth
    integer(i4b),                 intent(in)    :: maxdepth
    integer(i4b),                 intent(in)    :: maxtrib
    real(r8b),                    intent(in)    :: minarea
    real(r8b), dimension(:),      intent(in)    :: areag
    integer(i4b), intent(inout) :: achk
    !
    ! -- local
    type(tBB), dimension(:), allocatable :: cbb
    type(tBB) :: catptrbb
    integer(i4b), dimension(:), allocatable :: ind
    integer(i1b), dimension(:), allocatable :: ldd_stem
    integer(i4b), dimension(:,:), allocatable :: icir_stem
    integer(i4b) :: nstem, i, j, ic, ir, jcat0, jdepth
    integer(i4b), dimension(2) :: jcjr_pit
    real(r8b) :: area
    real(r8b), dimension(:), allocatable :: acc_stem
    integer(i4b) :: n
    integer(i4b), parameter :: mxoutlet = 1000
    integer(i4b), dimension(2, mxoutlet) :: icir_outlet
    integer(i4b), dimension(:), allocatable :: areachk
! ------------------------------------------------------------------------------
    idepth = idepth + 1
    if (idepth > maxdepth) then
      return
    end if
    !
    area = cat_size(cat, icat, pbb, areag)
    if ((area < minarea) .and. (achk == 1)) then
      !write(*,*) 'Area,jcat',area,jcat
      achk = -1
      return
    end if
    !
    call calc_stem(acc, icir_pit, cat, icat, pbb, ldd, lddmv, &
      nstem, icir_stem, acc_stem)
    !
    if (nstem == 0) return
    if (nstem < maxtrib+1) return
    !
    ! sort
    allocate(ind(nstem))
    do i = 1, nstem
      ind(i) = i
    end do
    call quicksort_d(acc_stem, ind, nstem)
    !
    n = 1
    icir_outlet(1,n) = icir_stem(1,n)
    icir_outlet(2,n) = icir_stem(2,n)
    !
    do i = nstem-1, nstem-maxtrib, -1
      j = ind(i); ic =  icir_stem(1,j); ir = icir_stem(2,j)
      n = n + 1
      icir_outlet(1,n) = icir_stem(1,j)
      icir_outlet(2,n) = icir_stem(2,j)
      if (j < nstem) then
        n = n + 1
        icir_outlet(1,n) = icir_stem(1,j+1)
        icir_outlet(2,n) = icir_stem(2,j+1)
      end if
    end do
    deallocate(ind)
    !
    do ir = pbb%ir0, pbb%ir1
      do ic = pbb%ic0, pbb%ic1
        if (cat(ic,ir) == icat) then
          catptr(ic,ir) = 1
          catptrbb%ir0 = min(catptrbb%ir0, ir); catptrbb%ir1 = max(catptrbb%ir1, ir)
          catptrbb%ic0 = min(catptrbb%ic0, ic); catptrbb%ic1 = max(catptrbb%ic1, ic)
          cat(ic,ir) = 0
        else
          catptr(ic,ir) = 0
        end if
      end do
    end do
    catptrbb%ncol = catptrbb%ic1 - catptrbb%ic0 + 1
    catptrbb%nrow = catptrbb%ir1 - catptrbb%ir0 + 1
    !
    jcat0 = jcat
    do i = 1, n
      ic = icir_outlet(1,i); ir = icir_outlet(2,i)
      jcat = jcat + 1
      cat(ic,ir) = jcat
      ldd(ic,ir) = jp
    end do
    
    !  icir_stem(1,1) = -abs(icir_stem(1,1))
    !  do i = nstem, 2, -1
    !    ic = icir_stem(1,i)
    !    if ((ic < 0).and.(i < nstem)) then
    !      icir_stem(1,i+1) = -abs(icir_stem(1,i+1))
    !    end if
    !  end do
    !  do i = 1, nstem
    !    ic = icir_stem(1,i); ir = icir_stem(2,i)
    !    if (ic < 0) then
    !      ic = abs(ic)
    !      jcat = jcat + 1
    !      cat(ic,ir) = jcat
    !      ldd_stem(i) = ldd(ic,ir)
    !      ldd(ic,ir) = jp
    !    end if
    !  end do
    !end if
    call calc_catch(cat, jcat0, catptr, catptrbb, ldd, lddmv, cbb, jcat)
    !
    ! restore ldd
    !do i = 1, nstem
    !  ic = abs(icir_stem(1,i)); ir = icir_stem(2,i)
    !  ldd(ic,ir) = ldd_stem(i)
    !end do
    !
    ! clean up
    !deallocate(ind, icir_stem, acc_stem)
    !
    ! recursion
    allocate(areachk(size(cbb)))
    do i = 1, size(cbb)
      areachk(i) = 1
    end do
    !do i = 1, size(cbb), 2
    !  areachk(i) = 0
    !end do
    !
    do i = 1, size(cbb)
      jdepth = idepth
      jcjr_pit = icir_outlet(:,i)
      call refine_catch(cat, jcat0+i, jcat, pbb, &
        catptr, acc, jcjr_pit, ldd, lddmv, &
        jdepth, maxdepth, maxtrib, minarea, areag, areachk(i))
    end do
    !
    do i = 1, size(cbb)
      if (areachk(i) == -1) then
        do ir = cbb(i)%ir0, cbb(i)%ir1
          do ic = cbb(i)%ic0, cbb(i)%ic1
            if (cat(ic,ir) == jcat0+i) then
              cat(ic,ir) = jcat
            end if
          end do
        end do
      end if
    end do
    !
    deallocate(cbb, areachk)
    !
    return
  end subroutine refine_catch
  
  