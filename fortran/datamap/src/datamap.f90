program datamap
! ******************************************************************************
  ! --- local
  use imod_idf_par, only: idfobj
  use imod_idf
  use utilsmod, only: mxslen, i1b, i4b, r8b, DZERO, DONE, &
    readidf_block, writeidf, tBB, errmsg, quicksort_d, addboundary, open_file
  !
  implicit none
  !
  interface
    recursive subroutine label_node(ia, ja, id, i4wk1d, isol)
      use utilsmod, only: i4b
      implicit none
      integer(i4b), dimension(:), intent(in) :: ia
      integer(i4b), dimension(:), intent(in) :: ja
      integer(i4b), intent(in) :: id
      integer(i4b), dimension(:), intent(inout) :: i4wk1d
      integer(i4b), intent(in) :: isol
    end subroutine label_node
  end interface
  !
  ! -- parameters
  integer(i4b), parameter :: js = 1
  integer(i4b), parameter :: jn = 2
  integer(i4b), parameter :: je = 3
  integer(i4b), parameter :: jw = 4
  integer(i4b), parameter :: nst = jw
  integer(i4b), dimension(2,nst), parameter :: st = (/0, 1, 0, -1, 1, 0, -1, 0/)
  integer(i4b), parameter :: mxnnb = 100000
  !
  ! --- types
  type tIntf
    integer(i4b)                          :: nb_gid = 0 ! neighboring local id
    integer(i4b)                          :: nb_lid = 0 ! neighboring local id
    integer(i4b)                          :: n = 0 ! number of nodes
    integer(i4b), dimension(:,:), pointer :: my_gicir => null()
    integer(i4b), dimension(:,:), pointer :: nb_gicir => null()
  end type tIntf
  !
  type tCat
    integer(i4b)                       :: gid = 0   ! global ID
    integer(i4b)                       :: lid = 0   ! local ID
    integer(i4b)                       :: isol = 0  ! solution number
    integer(i4b)                       :: nlnd = 0  ! number of land cells
    integer(i4b)                       :: nsea  = 0 ! number of sea cells
    type(tBB), pointer                 :: gbb => null() ! global bounding box
    type(tBB), pointer                 :: lbb => null() ! local bounding box
    integer(i4b)                       :: nnb  = 0! number of neighbors
    type(tIntf), dimension(:), pointer :: intf => null() ! interfaces
  end type tCat
  type(tCat), dimension(:), pointer :: cat => null()
  !
  type tSol
    integer(i4b) :: n = 0
    integer(i4b), dimension(:), pointer :: ind => null()
    real(r8b), dimension(:), pointer :: xid => null()
  end type tSol
  !
  ! --- functions
  integer(i4b) :: cfn_unique_i
  !
  ! --- local
  type(idfobj) :: idf
  type(tBB), pointer :: bb => null()
  type(tIntf), pointer :: intf => null()
  type(tSol), dimension(:), pointer :: solwk => null()
  !
  character(len=mxslen) :: f
  logical :: ldone, lfound
  integer(i4b) :: p, ofs, itile
  integer(i4b) :: gir0, gir1, gic0, gic1
  integer(i4b) :: ir, ic, jr, jc, nbr, m, n, iact, nja, mja, nsol, nlnd, nsea
  integer(i4b) :: i, j, k, ist, nc, nr, nb, ir0, ir1, ic0, ic1, mxgid, mxlid
  integer(i4b) :: id, id1, id2, iu
  integer(i4b), dimension(:), allocatable :: i4wk1d1, i4wk1d2, i4wk1d3
  integer(i4b), dimension(:,:), allocatable :: i4wk2d
  integer(i4b), dimension(:), allocatable :: g2l, l2g, cnt, ia, gja, lja, sol
  integer(i4b), dimension(:,:), pointer :: xid
  real(r8b), dimension(:), allocatable :: solcnt, r8wk1d
! ------------------------------------------------------------------------------
  
  call getarg(1,f)
  !
  if (.not.idfread(idf, f, 0)) then
    call errmsg('Could not read '//trim(f))
  end if
  gir0 = 1; gir1 = idf%nrow; gic0 = 1; gic1 = idf%ncol
!  gir0 = 1; gir1 = 9000; gic0 = 1; gic1 = 16000
!  gir0 = 1; gir1 = 9000; gic0 = 6500; gic1 = 16000 !USA
!  gir0 = 10000; gir1 = 16300; gic0 = 33000; gic1 = 42000 !AUS
  call readidf_block(idf, gir0, gir1, gic0, gic1, xid)
  nc = size(xid,1); nr = size(xid,2)
  !
  if (.false.) then
  call writeidf('tmp.idf', xid, nc, nr, &
    idf%xmin + (gic0-1)*idf%dx, idf%ymin + (idf%nrow-gir1)*idf%dx, &
    idf%dx, 0.D0)
  end if
  !
  ! label the sea cells
  call addboundary(xid, nc, nr)
  !
  mxgid = maxval(xid)
  !
  allocate(g2l(mxgid))
  do i = 1, mxgid
    g2l(i) = 0
  end do
  !
  ! determine global <--> local ID mapping
  n = 0
  do ir = 1, nr
    do ic = 1, nc
      id = abs(xid(ic,ir))
      if (id /= 0) then
        g2l(id) = 1
      else
        n = n + 1
      end if
    end do
  end do
  !
  write(*,*) 'Cells other than land/sea (%): ',100.*real(n)/real(nr*nc)
  !
  mxlid = 0
  do i = 1, mxgid
    if (g2l(i) > 0) then
      mxlid = mxlid + 1
      g2l(i) = mxlid
    end if
  end do
  !
  allocate(l2g(mxlid))
  do i = 1, mxgid
    id = g2l(i)
    if (id > 0) then
      l2g(id) = i
    end if
  end do
  !
  write(*,*) 'Total # catchments (M):', real(mxlid)/1000000.
  !
  ! determine the bounding boxes and counts
  allocate(cat(mxlid))
  do i = 1, mxlid
    allocate(cat(i)%gbb, cat(i)%lbb)
    cat(i)%lid = i
    cat(i)%gid = l2g(i)
  end do
  !
  nlnd = 0
  nsea = 0
  do ir = 1, nr
    do ic = 1, nc
      id1 = abs(xid(ic,ir))
      if (id1 /= 0) then
        id2 = g2l(id1); bb => cat(id2)%lbb
        bb%ir0 = min(bb%ir0, ir); bb%ir1 = max(bb%ir1, ir)
        bb%ic0 = min(bb%ic0, ic); bb%ic1 = max(bb%ic1, ic)
        if (xid(ic,ir) > 0) then
          cat(id2)%nlnd = cat(id2)%nlnd + 1; nlnd = nlnd + 1
        else
          cat(id2)%nsea = cat(id2)%nsea + 1; nsea = nsea + 1
        end if
      end if
    end do
  end do
  
  do i = 1, mxlid
    bb => cat(i)%lbb
    bb%nrow = bb%ir1 - bb%ir0 + 1
    bb%ncol = bb%ic1 - bb%ic0 + 1
    !
    ! set global bounding box
    cat(i)%gbb%ir0 = bb%ir0 + gir0 - 1; cat(i)%gbb%ir1 = cat(i)%gbb%ir0 + bb%nrow - 1
    cat(i)%gbb%ic0 = bb%ic0 + gic0 - 1; cat(i)%gbb%ic1 = cat(i)%gbb%ic0 + bb%ncol - 1
    cat(i)%gbb%nrow = bb%nrow; cat(i)%gbb%ncol = bb%ncol
  end do
  !
  write(*,*) 'Total # land cells (M):', real(nlnd)/1000000.
  write(*,*) 'Total #  sea cells (M):', real(nsea)/1000000.
  !
  ! determine the neighboring cells
  allocate(i4wk1d1(mxnnb), i4wk2d(5,mxnnb))
  allocate(ia(mxlid+1))
  do iact = 1, 2
    nja = 0
    do i = 1, mxlid
      id1 = l2g(i) ! global ID
      bb => cat(i)%lbb
      ir0 = bb%ir0; ir1 = bb%ir1; ic0 = bb%ic0; ic1 = bb%ic1 !BB
      !
      ! label the neighboring cells
      nbr = 0 ! initialize number of neighbors
      do ir = ir0, ir1
        do ic = ic0, ic1
          if (id1 == abs(xid(ic,ir))) then
            do ist = 1, nst ! loop over the 8 neighbors
              jc = ic+st(1,ist); jc = max(1,jc); jc = min(nc,jc)
              jr = ir+st(2,ist); jr = max(1,jr); jr = min(nr,jr)
              id2 = abs(xid(jc,jr)) ! neighboring global ID
              if (id1 /= id2) then ! neighbor found
                if (nbr > mxnnb) then
                  call errmsg('Increase mxnnb')
                end if
                nbr = nbr + 1
                i4wk1d1(nbr) = id2; i4wk2d(1,nbr) = id2
                i4wk2d(2,nbr) = ic+gic0-1; i4wk2d(3,nbr) = ir+gir0-1 !my global nodes
                i4wk2d(4,nbr) = jc+gic0-1; i4wk2d(5,nbr) = jr+gir0-1 ! neihbor global nodes
              end if
            end do
          end if
        end do
      end do
      !
      ! get the unique values
      n = cfn_unique_i(i4wk1d1(1:nbr), nbr, 0)
      !
      if (iact == 1) then ! init
        cat(i)%nnb = n
        if (n > 0) then
          allocate(cat(i)%intf(n))
        end if
        nja = nja + n + 1
      else ! store
        nja = nja + 1
        ia(i) = nja
        gja(nja) = l2g(i)
        lja(nja) = i
        do j = 1, n
          intf => cat(i)%intf(j) ! get interface
          id = i4wk1d1(j) ! global neighbor ID
          intf%nb_gid = id; intf%nb_lid = g2l(id)
          nja = nja + 1
          gja(nja) = id
          lja(nja) = g2l(id)
          do k = 1, nbr
            if (i4wk2d(1,k) == id) then
              intf%n = intf%n + 1
            end if
          end do
          allocate(intf%my_gicir(2,intf%n), intf%nb_gicir(2,intf%n))
          intf%n = 0
          do k = 1, nbr
            if (i4wk2d(1,k) == id) then
              intf%n = intf%n + 1
              intf%my_gicir(1,intf%n) = i4wk2d(2,k); intf%my_gicir(2,intf%n) = i4wk2d(3,k)
              intf%nb_gicir(1,intf%n) = i4wk2d(4,k); intf%nb_gicir(2,intf%n) = i4wk2d(5,k)
            end if
          end do
        end do
      end if
    end do
    if (iact == 1) then
      allocate(gja(nja), lja(nja))
    end if
  end do
  ia(mxlid+1) = nja+1
  deallocate(i4wk2d, i4wk1d1)
  !
  ! check if the CRS is sorted
  do i = 2, mxlid
    id1 = lja(ia(i-1))
    id2 = lja(ia(i))
    if (id2 <= id1) then
      call errmsg('CRS not sorted.')
    end if
  end do
  do i = 1, mxlid
    do j = ia(i)+2, ia(i+1)-1
      id1 = lja(j-1)
      id2 = lja(j)
      if (id2 <= id1) then
        call errmsg('CRS not sorted.')
      end if
    end do
  end do
  !
  ! determine the unique solutions
  nsol = 0
  allocate(sol(mxlid))
  do i = 1, mxlid
    sol(i) = 0
  end do
  nsol = 0
  do i = 1, mxlid
    if (sol(i) == 0) then
      nsol = nsol + 1
      call label_node(ia, lja, i, sol, nsol)
    end if
  end do
  !
  ! sort the solutions by size
  allocate(i4wk1d1(nsol), i4wk1d2(nsol), solcnt(nsol))
  do i = 1, nsol
    i4wk1d1(i) = i
    solcnt(i) = DZERO
  end do
  do i = 1, mxlid
    j = sol(i)
    solcnt(j) = solcnt(j) - real(cat(i)%nlnd + cat(i)%nsea,r8b) 
  end do
  !
  call quicksort_d(solcnt, i4wk1d1, nsol)
  !
  ! relabel the solutions
  do i = 1, nsol
    solcnt(i) = abs(solcnt(i))
    j = i4wk1d1(i)
    i4wk1d2(j) = i
  end do
  !
  do i = 1, mxlid
    j = sol(i)
    sol(i) = i4wk1d2(j)
    cat(i)%isol = sol(i)
  end do
  !
  deallocate(i4wk1d1, i4wk1d2)
  !
  write(*,*) 'Top 5 solutions:'
  do i = 1, 5
    write(*,*) ' # cells (M):', real(solcnt(i))/1000000.
  end do
  !
  ! debug output
  if (.false.) then
  do ir = 1, nr
    do ic = 1, nc
      id = xid(ic,ir)
      if (abs(id) /= 0) then
        if (id > 0) then
          xid(ic,ir) = sol(g2l(id))
        else
          xid(ic,ir) = -sol(g2l(-id))
        end if
      end if
    end do
  end do
  call writeidf('sol.idf', xid, nc, nr, &
    idf%xmin + (gic0-1)*idf%dx, idf%ymin + (idf%nrow-gir1)*idf%dx, &
    idf%dx, 0.D0)
  end if
  !
  allocate(i4wk1d1(mxlid + 1))
  allocate(i4wk1d2(7*mxlid + nja*3))
  allocate(i4wk1d3(nja))
  !
  mja = 0
  do i = 1, mxlid
    bb => cat(i)%gbb
    mja = mja + 1
    i4wk1d1(i) = mja
    i4wk1d2(mja) = bb%ir0; mja = mja + 1 ! 1
    i4wk1d2(mja) = bb%ir1; mja = mja + 1 ! 2
    i4wk1d2(mja) = bb%ic0; mja = mja + 1 ! 3
    i4wk1d2(mja) = bb%ic1; mja = mja + 1 ! 4
    i4wk1d2(mja) = cat(i)%nlnd; mja = mja + 1 ! 5
    i4wk1d2(mja) = cat(i)%nsea; mja = mja + 1 ! 6
    i4wk1d2(mja) = l2g(lja(ia(i))) ! 7
    ! add the IDs
    do j = ia(i), ia(i+1)-1
      mja = mja + 1
      i4wk1d2(mja) = lja(j)
    end do
    ! add number of cells
    mja = mja + 1; i4wk1d2(mja) = cat(i)%nsea + cat(i)%nlnd
    k = 0
    do j = ia(i)+1, ia(i+1)-1
      k = k + 1; intf => cat(i)%intf(k)
      ! check
      if (intf%nb_lid /= lja(j)) then
        call errmsg('Program error.')
      end if
      mja = mja + 1
      i4wk1d2(mja) = intf%n
    end do
    ! add pointer to cells
    do j = ia(i), ia(i+1)-1
      mja = mja + 1
      i4wk1d2(mja) = -1
    end do
  end do
  i4wk1d1(mxlid+1) = mja
  !
  ! set pointers to nodes in map.nodes.bin
  p = 1
  do i = 1, mxlid
    j = i4wk1d1(i) + 4; nlnd = i4wk1d2(j)
    j = i4wk1d1(i) + 5; nsea = i4wk1d2(j)
    n = (i4wk1d1(i+1) - i4wk1d1(i) - 7)/3
    j = i4wk1d1(i) + 7 + 2*n
    i4wk1d2(j) = p
    p = p + 2*(nlnd+nsea)
  end do 

  ! set pointers to nodes in map.intfnodes.bin
  p = 1
  do i = 1, mxlid
    n = (i4wk1d1(i+1) - i4wk1d1(i) - 7)/3
    do j = i4wk1d1(i) + 7 + 2*n + 1, i4wk1d1(i+1) - 1
      i4wk1d2(j) = p
      p = p + 4*i4wk1d2(j-n)
    end do
  end do 
  !
  ! ---------------
  ! write the catchments
  f = 'map.catch.bin'
  call open_file(f, iu, 'w', .true.)
  write(iu) mxlid
  write(iu) mja
  write(iu) (i4wk1d1(i),i=1,mxlid + 1)
  write(iu) (i4wk1d2(i),i=1,mja)
  !
  ! ---------------
  ! write the solutions
  f = 'map.sol.bin'
  call open_file(f, iu, 'w', .true.)
  write(iu) nsol
  write(iu) (solcnt(i),i=1,nsol)
  deallocate(solcnt)
  if (allocated(i4wk1d1)) deallocate(i4wk1d1)
  if (allocated(i4wk1d2)) deallocate(i4wk1d2)
  allocate(i4wk1d1(nsol+1))
  allocate(i4wk1d2(mxlid))
  allocate(solwk(nsol))
  do i = 1, mxlid
    j = sol(i)
    solwk(j)%n = solwk(j)%n + 1
  end do
  do i = 1, nsol
    n = solwk(i)%n
    allocate(solwk(i)%ind(n), solwk(i)%xid(n))
    do j = 1, n
      solwk(i)%ind(j) = j
    end do
    solwk(i)%n = 0
  end do
  do i = 1, mxlid
    j = sol(i)
    solwk(j)%n = solwk(j)%n + 1
    n = solwk(j)%n
    solwk(j)%xid(n) = real(i,r8b)
  end do
  mja = 1
  do i = 1, nsol
    call quicksort_d(solwk(i)%xid, solwk(i)%ind, solwk(i)%n)
    i4wk1d1(i) = mja
    do j = 1, solwk(i)%n
      i4wk1d2(mja) = int(solwk(i)%xid(j),i4b)
      mja = mja + 1
    end do
  end do
  i4wk1d1(nsol+1) = mja
  write(iu)(i4wk1d1(i),i=1,nsol+1)
  write(iu)(i4wk1d2(i),i=1,mja-1)
  close(iu)
  !
  ! ---------------
  ! write the nodes
  n = 0
  do i = 1, mxlid
    n = n + cat(i)%nlnd + cat(i)%nsea
  end do
  if (allocated(i4wk2d)) deallocate(i4wk2d)
  allocate(i4wk2d(2,n))
  n = 0
  do i = 1, mxlid
    bb => cat(i)%lbb
    do ir = bb%ir0, bb%ir1
      do ic = bb%ic0, bb%ic1
        id = xid(ic,ir)
        if ((id /= 0).and.(abs(id) == l2g(i))) then
          n = n + 1
          i4wk2d(1,n) = ic+gic0-1; i4wk2d(2,n) = ir+gir0-1
          if (id < 0) then
            i4wk2d(1,n) = -i4wk2d(1,n)
          end if
        end if
      end do
    end do
  end do
  f = 'map.nodes.bin'
  call open_file(f, iu, 'w', .true.)
  write(iu)((i4wk2d(j,i),i=1,n),j=1,2)
  close(iu)
  !
  ! ---------------
  ! write the interfaces
  n = 0
  do i = 1, mxlid
    do j = 1, cat(i)%nnb
      n = n + cat(i)%intf(j)%n
    end do
  end do
  if (allocated(i4wk2d)) deallocate(i4wk2d)
  allocate(i4wk2d(4,n))
  n = 0
  do i = 1, mxlid
    do j = 1, cat(i)%nnb
      intf => cat(i)%intf(j)
      do k = 1, intf%n
        n = n + 1
        i4wk2d(1,n) = intf%my_gicir(1,k)
        i4wk2d(2,n) = intf%my_gicir(2,k)
        i4wk2d(3,n) = intf%nb_gicir(1,k)
        i4wk2d(4,n) = intf%nb_gicir(2,k)
      end do
    end do
  end do
  f = 'map.intfnodes.bin'
  call open_file(f, iu, 'w', .true.)
  write(iu)((i4wk2d(j,i),i=1,n),j=1,4)
  close(iu)
  
end program

recursive subroutine label_node(ia, ja, id1, i4wk1d, isol)
  use utilsmod, only: i4b
  implicit none
  integer(i4b), dimension(:), intent(in) :: ia
  integer(i4b), dimension(:), intent(in) :: ja
  integer(i4b), intent(in) :: id1
  integer(i4b), dimension(:), intent(inout) :: i4wk1d
  integer(i4b), intent(in) :: isol
  !
  integer(i4b) :: i, id2
  !
  i4wk1d(id1) = isol
  !
  do i = ia(id1)+1, ia(id1+1)-1
    id2 = ja(i)
    if (i4wk1d(id2) == 0) then
      call label_node(ia, ja, id2, i4wk1d, isol)
    end if
  end do
end subroutine label_node
  