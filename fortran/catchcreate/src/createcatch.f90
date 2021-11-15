  module catchMod
    use pcrModule, only: jsw, js, jse, jw, jp, je, jnw, jn, jne, st, jperm 
    use utilsmod, only: i1b, i4b, r8b, tBB, quicksort_d, DZERO, DONE, i4wk2d, &
      writeasc, writeidf, errmsg
    
    implicit none
    
    real(r8b), parameter :: sqkm = 1000000.d0 !m2
    
    integer(i4b) :: gic0, gic1, gir0, gir1, gncol, gnrow, lncol, lnrow
    real(r8b) :: gcs, gxmin, gymin, lxmin, lymin
    
  contains
  
    subroutine calc_catch(cat, catid0, catptr, bb, ldd, lddmv, catbb, in_mxcatid)
  ! ******************************************************************************
  ! ******************************************************************************
  !
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------------------
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
      if (size(catbb) < 9) then
       ! write(*,*) '@@@@'
      end if
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
            call writeidf('error.idf', cat, size(cat,1), size(cat,2), lxmin, lymin, gcs, 0.d0)
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
      !
      if ((.false.).and.(catid0==7096)) then
        do ir = 1, size(cat,2)
          do ic = 1, size(cat,1)
            if (cat(ic,ir) /= 7097) cat(ic,ir) = 0
          end do
        end do
        call writeidf('cat.idf', cat, size(cat,1), size(cat,2), lxmin, lymin, gcs, 0.d0)
        stop
      end if
      
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
    
    subroutine calc_acc(acc, cat, icat, bb, ldd, lddmv, icir_pit, areag)
  ! ******************************************************************************
  ! ******************************************************************************
  !
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------------------
      !
      ! -- dummy
      real(r8b), dimension(:,:), intent(inout) :: acc
      integer(i4b), dimension(:,:), intent(in) :: cat
      integer(i4b), intent(in) :: icat
      type(tBB), intent(in) :: bb
      integer(i1b), dimension(:,:), intent(inout) :: ldd
      integer(i1b), intent(in) :: lddmv
      integer(i4b), dimension(2), intent(out) :: icir_pit
      real(r8b), dimension(:), intent(in) :: areag
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
      !
      !do ir = 1, nr
      !  do ic = 1, nc
      !    acc(ic,ir) = DZERO
      !  end do
      !end do
      !
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
        kr = gir0 + ir - 1;
        !acc(ic,ir) = DONE
        acc(ic,ir) = areag(kr)
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
          !acc(jc,jr) = 1
          kr = gir0 + jr - 1
          acc(jc,jr) = areag(kr)
          do inbr = 1, 9
            if (inbr == 5) cycle
            kc = jc + st(1,inbr); kr = jr + st(2,inbr)
            if ((kc < 1).or.(kc > nc).or.(kr < 1).or.(kr > nr)) cycle
            if (cat(kc,kr) /= icat) cycle
            w = ldd(kc,kr) !neighbor ldd
            if (w == jperm(inbr)) then
              if (acc(kc,kr) == DZERO) then
                call errmsg('Subroutine "calc_acc": zero acc.')
              end if
              acc(jc,jr) = acc(jc,jr) + acc(kc,kr)
            end if
          end do
          !
          ! add cell to list
          m = m + 1
          if (m > mxwk2) then
            call errmsg('Subroutine "calc_acc": increase mxwk2.')
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
      !
      ! -- dummy
      integer(i4b), dimension(:,:), intent(in) :: cat
      integer(i4b), intent(in) :: icat
      type(tBB), intent(in) :: bb
      real(r8b), dimension(:), intent(in) :: areag
      real(r8b) :: area
      !
      ! -- local
      integer(i4b) :: ic, ir, jr
  ! ------------------------------------------------------------------------------
      area = DZERO
      do ir = bb%ir0, bb%ir1
        do ic = bb%ic0, bb%ic1
          if (cat(ic,ir) == icat) then
            jr = gir0 + ir - 1
            area = area + areag(jr)
          end if
        end do
      end do
    end function cat_size
    !
    subroutine cat_unique(cat, bb, area, areag)
  ! ******************************************************************************
  ! ******************************************************************************
  !
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------------------
      !
      ! -- dummy
      integer(i4b), dimension(:,:), intent(inout) :: cat
      type(tBB), dimension(:), allocatable, intent(out) :: bb
      real(r8b), dimension(:), allocatable :: area
      real(r8b), dimension(:), intent(in) :: areag
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
            area(w) = area(w) + areag(ir + gir0 - 1)
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
      idepth, maxdepth, maxtrib, minarea, minareasb, areag)
  ! ******************************************************************************
  ! ******************************************************************************
  !
  !    SPECIFICATIONS:
  ! ------------------------------------------------------------------------------
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
      real(r8b),                    intent(in)    :: minareasb
      real(r8b), dimension(:),      intent(in)    :: areag
      !
      ! -- local
      type(tBB), dimension(:), allocatable :: cbb
      type(tBB) :: catptrbb
      integer(i4b), dimension(:), allocatable :: ind
      integer(i1b), dimension(:), allocatable :: ldd_stem
      integer(i4b), dimension(:,:), allocatable :: icir_stem
      integer(i4b) :: nstem, i, j, ic, ir, jcat0, jdepth
      integer(i4b), dimension(2) :: jcjr_pit
      real(r8b), dimension(:), allocatable :: acc_stem
      integer(i4b) :: n, m, nc, nr
      integer(i4b), parameter :: mxoutlet = 1000
      integer(i4b), dimension(mxoutlet) :: basin_flag
      integer(i4b), dimension(2, mxoutlet) :: icir_outlet
      real(r8b) :: area, areasub
  !    integer(i4b), dimension(:), allocatable :: areachk
  ! ------------------------------------------------------------------------------
      idepth = idepth + 1
      if (idepth > maxdepth) then
        return
      end if
      !
      area = cat_size(cat, icat, pbb, areag)
      !
      call calc_stem(acc, icir_pit, cat, icat, pbb, ldd, lddmv, &
        nstem, icir_stem, acc_stem)
      !
      if (nstem == 0) then
        return
      end if
      if (nstem < maxtrib+1) then
        return
      end if
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
      m = 0
      do i = 1, n
        ic = icir_outlet(1,i); ir = icir_outlet(2,i)
        jcat = jcat + 1
        cat(ic,ir) = jcat
        ldd(ic,ir) = jp
        basin_flag(i) = 0
        if (mod(i,2) == 0) then
          basin_flag(i) = 1
        end if
      end do
      !
      call calc_catch(cat, jcat0, catptr, catptrbb, ldd, lddmv, cbb, jcat)
      !
      do i = 1, size(cbb)
        areasub = cat_size(cat, jcat0+i, cbb(i), areag)
        if ((basin_flag(i) == 1).and.(areasub < minarea)) then ! basins
          cycle
        end if
        if ((basin_flag(i) == 0).and.(areasub < minareasb)) then ! sub-basins
          cycle
        end if
        !
        jdepth = idepth
        jcjr_pit = icir_outlet(:,i)
        call refine_catch(cat, jcat0+i, jcat, pbb, &
          catptr, acc, jcjr_pit, ldd, lddmv, &
          jdepth, maxdepth, maxtrib, minarea, minareasb, areag)
      end do
      !
      if (allocated(cbb)) deallocate(cbb)
      !
      return
    end subroutine refine_catch
    
  end module catchMod
  
  program createcatch
    ! modules
    use catchMod
    use pcrModule
    use utilsmod, only: writeasc, writeidf, tPol, readgen, filtergen_i1, &
      tBB, DZERO, quicksort_d, ta, errmsg, tUnp, calc_unique, logmsg
    use imod_idf, only: idfobj, idfread, imod_utl_printtext, idfdeallocatex
    use ehdrModule, only: writeflt
    use metis_module, only: tMetis
    
    implicit none
    
    type(tMap), pointer :: map
    type(tPol), dimension(:), allocatable :: p
    type(tBB) :: catptrbb
    type(tBB), dimension(:), allocatable :: bb, bb_sub
    type(tUnp), dimension(:), allocatable :: regbb
    type(tMetis), pointer :: met => null()
    logical :: ok, flg
    character(len=1024) :: f_ldd, f_gen, f_area, s, fmt
    integer(i1b) :: lddmv
    integer(i1b), dimension(:,:), allocatable :: ldd, catptr
    integer(i4b) :: maxtrib, ir, ic, jr, jc, kr, kc, ic0, ic1, ir0, ir1
    integer(i4b) :: catid, mxcatid, n, m, inbr, icat, jcat, kcat, lcat, icat0, icat1, ncat
    integer(i4b) :: idepth, maxdepth, nreg, ireg, idum, nparts, nc, nr, ipart, ncoast_old, ncoast_new
    integer(i4b), dimension(2) :: icir_pit
    integer(i4b), dimension(:), allocatable :: coastcatflg
    integer(i4b), dimension(:,:), allocatable :: cat, coastcat, regun !, wrk
    integer(i4b), dimension(:,:), allocatable :: icir_stem
    real(r8b) :: area, minarea, minareasb, area_tot
    real(r8b), dimension(:), allocatable :: areag
    real(r8b), dimension(:), allocatable :: cat_area
    real(r8b), dimension(:,:), allocatable :: acc
    real(r8b), dimension(:), allocatable :: acc_stem
    !
    integer(i4b), dimension(:), allocatable :: ind, indinv
    !
    logical :: lstop = .false.
    integer(i4b) :: areachk
    !
    type(idfobj) :: idf
! ------------------------------------------------------------------------------
    
    call getarg(1,f_ldd)
    call getarg(2,f_gen)
    call getarg(3,f_area)
    call getarg(4,s); read(s,*) minarea
    call getarg(5,s); read(s,*) minareasb
    call getarg(6,s); read(s,*) maxdepth
    !
    call logmsg('***** Using minimal area '//ta((/minarea/))//' and maxdepth '//ta((/maxdepth/))//' *****')
    !
    ! read the gen file for clipping
    p = readgen(f_gen)
    !
    ! read the map file
    allocate(map)
    ok = map%read_header(f_ldd, 0)
    gcs  = map%header%cellsizex
    gxmin = map%header%xul
    gymin = map%header%yul
    gncol = map%header%nrcols
    gnrow = map%header%nrrows
    lddmv = 0
    !
    ! read the area
    call imod_utl_printtext('Reading '//trim(f_area),0)
    if (.not.idfread(idf,f_area,1)) then
      call imod_utl_printtext('Could not read '//trim(f_area),2)
    end if
    call imod_utl_printtext('Done reading '//trim(f_area),0)
    ! check
    if (idf%nrow /= gnrow) then
      call errmsg("Invalid number of rows")
    end if
    allocate(areag(gnrow))
    do ir = 1, gnrow
      areag(ir) = idf%x(1,ir)
    end do
    call idfdeallocatex(idf)
    !
    flg = .false.; ir0 = 1; ir1 = gnrow; ic0 = 1; ic1 = gncol
    !
    ! debug: seperate catchments
    !flg = .true.; ir0 = 1; ir1 = 9000; ic0 = 1; ic1 = 16000; kcat = 641508 ! Missisippi
    !flg = .true.; ir0 = 9000; ir1 = 17000; ic0 = 11500; ic1 = 17500; kcat = 30715 ! Amazone
    !flg = .true.; ir0 = 6000; ir1 = 15000; ic0 = 19500; ic1 = 28000; kcat = 114403 ! Africa
    !flg = .true.; ir0 = 4380; ir1 = 5300; ic0 = 21800; ic1 = 23100; kcat = 2790 ! Rhine-Meuse
    !flg = .false.; ir0 = 4380; ir1 = 5300; ic0 = 21800; ic1 = 23100
    !flg = .false.; ir0 =  12100; ir1 = 14000; ic0 = 26700; ic1 = 27700 ! Madagascar
    !
    gir0 = ir0; gir1 = ir1; gic0 = ic0; gic1 = ic1
    lnrow = gir1 - gir0 + 1; lncol = gic1 - gic0 + 1
    !
    lxmin = gxmin + (gic0-1)*gcs; lymin = gymin-gir1*gcs ! ymin = yul-9000*cs
    !
    call map%read_block(ldd, ir0, ir1, ic0, ic1, lddmv)
    call map%close(); call map%clean()
    
    if (.not.flg) call filtergen_i1(p, ldd, gxmin, gymin, gcs, lddmv) !-->GLOBAL
    !
    lncol = size(ldd,1); lnrow = size(ldd,2)
    !
    ! set boundary
    write(*,*) 'Setting ldd boundary...'
    do ir = 1, lnrow
      if (ldd(1,ir) /= lddmv) then
        ldd(1,ir) = jp
      end if
      if (ldd(lncol,ir) /= lddmv) then
        ldd(lncol,ir) = jp
      end if
    end do
    do ic = 1, lncol
      if (ldd(ic,1) /= lddmv) then
        ldd(ic,1) = jp
      end if
      if (ldd(ic,lnrow) /= lddmv) then
        ldd(ic,lnrow) = jp
      end if
    end do
    !
    write(*,*) 'Computing catchments...'
    allocate(cat(lncol,lnrow),catptr(lncol,lnrow), coastcat(lncol,lnrow))
    do ir = 1, lnrow
      do ic = 1, lncol
        coastcat(ic,ir) = 0
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
    call writeflt('cat_lev'//ta((/maxdepth/))//'_init', cat, size(cat,1), size(cat,2), lxmin, lymin, gcs, 0)
    mxcatid = size(bb)
    !
    write(*,*) 'Refining catchments...'
    !
    allocate(acc(lncol,lnrow))!, wrk(nc,nr))
    do ir = 1, lnrow
      do ic = 1, lncol
        acc(ic,ir) = DZERO
      end do
    end do
    !
    ! first, find the catchment boundary
    maxtrib = 4
    catid = mxcatid + 1
    minarea = minarea * sqkm !m2
    minareasb = minareasb * sqkm !m2
!    minarea = 100.d0 * sqkm !m2
!    minarea = 250.d0 * sqkm !m2
!    maxdepth = 12
    !
    if (.not.flg) then
      icat0 = 1; icat1 = mxcatid
    else
      icat0 = kcat; icat1 = kcat
      do icat = icat0, icat1
        do ir = 1, lnrow
          do ic = 1, lncol
            if ((cat(ic,ir) < icat0).or.(cat(ic,ir) > icat1)) then
              cat(ic,ir) = 0
            end if
          end do
        end do
      end do 
    end if
    !
    if (.true.) then
    
    ! treat the coastal catchments
    allocate(coastcatflg(icat1))
    !
    ncoast_old = 0; ncat = 0
    do icat = icat0, icat1
      area = cat_size(cat, icat, bb(icat), areag)
      if (area < minarea) then
        ncoast_old = ncoast_old + 1
        do ir = bb(icat)%ir0, bb(icat)%ir1
          do ic = bb(icat)%ic0, bb(icat)%ic1
            if (cat(ic,ir) == icat) then
              coastcat(ic,ir) = 1
            end if
          end do
        end do
      else
        ncat = ncat + 1
        coastcatflg(icat) = 0
      end if
    end do
    !
    call calc_unique(coastcat, 5, regun, regbb, nreg, idum, 0., 0., 0.)
    ncoast_new = 0
    do ireg = 1, nreg ! loop over unique regions
      do icat = icat0, icat1
        coastcatflg(icat) = 0
      end do
      do ir = regbb(ireg)%ir0, regbb(ireg)%ir1
        do ic = regbb(ireg)%ic0, regbb(ireg)%ic1
         if (regun(ic,ir) == ireg) then
           icat = cat(ic,ir)
           coastcatflg(icat) = 1
         end if
        end do
      end do
      !
      area_tot = DZERO
      do icat = icat0, icat1
        if (coastcatflg(icat) == 1) then
          area = cat_size(cat, icat, bb(icat), areag)
          area_tot = area_tot + area
        end if
      end do
      !
      if (area_tot < minarea) then
        mxcatid = mxcatid + 1
        ncoast_new = ncoast_new + 1
        do ir = regbb(ireg)%ir0, regbb(ireg)%ir1
          do ic = regbb(ireg)%ic0, regbb(ireg)%ic1
            if (regun(ic,ir) == ireg) then
              cat(ic,ir) = mxcatid
            end if
          end do
        end do
        !
      else ! METIS
        !
        if (allocated(i4wk2d)) deallocate(i4wk2d)
        nc = regbb(ireg)%ncol; nr = regbb(ireg)%nrow
        allocate(i4wk2d(nc,nr))
        do ir = regbb(ireg)%ir0, regbb(ireg)%ir1
          do ic = regbb(ireg)%ic0, regbb(ireg)%ic1
            jr = ir - regbb(ireg)%ir0 + 1
            jc = ic - regbb(ireg)%ic0 + 1
            if (regun(ic,ir) == ireg) then
              i4wk2d(jc,jr) = 1
            else
              i4wk2d(jc,jr) = 0
            end if
          end do
        end do
        !
        nparts = max(2,nint(area_tot/minarea))
        allocate(met)
        call met%init(i4wk2d, nparts)
        call met%set_opts()
        call met%recur()
        !
        mxcatid = mxcatid + 1
        ncoast_new = ncoast_new + nparts
        !
        n = 0
        do ir = regbb(ireg)%ir0, regbb(ireg)%ir1
          do ic = regbb(ireg)%ic0, regbb(ireg)%ic1
            jr = ir - regbb(ireg)%ir0 + 1
            jc = ic - regbb(ireg)%ic0 + 1
            if (regun(ic,ir) == ireg) then
              n = n + 1
              ipart = met%part(n)
              if (cat(ic,ir) == 0) then
                call errmsg('Program error')
              end if
              if ((ic.eq.453).and.(ir.eq.40)) then
                write(*,*) 'break'
              end if
              cat(ic,ir) = mxcatid + ipart
            end if
          end do
        end do
        !
        call met%clean()
        deallocate(met)
      end if
    end do
    !
    if (allocated(coastcat)) deallocate(coastcat)
    if (allocated(coastcatflg)) deallocate(coastcatflg)
    if (allocated(regun)) deallocate(regun)
    if (allocated(i4wk2d)) deallocate(i4wk2d)
    
    call logmsg('# Coastal catchments: '//ta((/ncoast_old/))//' --> '//ta((/ncoast_new/)))
    call writeflt('cat_lev'//ta((/maxdepth/))//'_init_coast_adj', cat, size(cat,1), size(cat,2), lxmin, lymin, gcs, 0)
    !
    end if 
    !
    jcat = mxcatid
    !
    ! non-coastal catchments
    lcat = 0
    do icat = icat0, icat1
      area = cat_size(cat, icat, bb(icat), areag)
      if (area < minarea) cycle
      lcat = lcat + 1
      !
      write(s,*) area
      write(*,'(a,i2.2,a,i7.7,a,i7.7,a)') 'Processing ', maxdepth,' pfafstetter levels for catchment ', lcat, '/', ncat, ' area '//trim(s)//'...'
      call calc_acc(acc, cat, icat, bb(icat), ldd, lddmv, icir_pit, areag)
      if (.false.) then
        call writeidf('acc.idf', acc, size(acc,1), size(acc,2), lxmin, lymin, gcs, DZERO)
      end if
      idepth = 0
      areachk = 1
      call refine_catch(cat, icat, jcat, bb(icat), &
        catptr, acc, icir_pit, ldd, lddmv, &
        idepth, maxdepth, maxtrib, minarea, minareasb, areag)!, areachk)
    end do
    !
    do ir = 1, lnrow
      do ic = 1, lncol
        cat(ic,ir) = abs(cat(ic,ir))
      end do
    end do
    !
    call cat_unique(cat, bb, cat_area, areag)
    write(*,'(a)') '# catchments, min. area (km2), max. area (km2): ' // &
      ta((/size(bb)/)) // ', ' // ta((/minval(cat_area)/sqkm/)) // ', ' // &
      ta((/maxval(cat_area)/sqkm/))
    !
    call writeidf('cat_lev'//ta((/maxdepth/))//'.idf', cat, size(cat,1), size(cat,2), lxmin, lymin, gcs, DZERO)
    !call writeasc('cat_lev'//ta((/maxdepth/))//'.asc', cat, size(cat,1), size(cat,2), lxmin, lymin, gcs, DZERO)
    call writeflt('cat_lev'//ta((/maxdepth/)), cat, size(cat,1), size(cat,2), lxmin, lymin, gcs, 0)
    !
  end program


  