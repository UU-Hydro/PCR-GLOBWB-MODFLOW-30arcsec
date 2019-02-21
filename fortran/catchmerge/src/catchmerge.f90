program catmerge
  use imod_idf
  use utilsmod, only: chkexist, quicksort_d, writeidf
  
  implicit none
  
  ! parameters
  integer, parameter :: DZERO = 0.D0
  integer, parameter :: mxslen = 1024
  
  type(idfobj) :: catg, lddg, areag
  character(len=mxslen) :: f, s
  integer :: ngen, igen, iu, iact, ncat, mcat, m, ios, i, j, k, is, ip, inbr, ips, ipe, it, jt
  integer :: ic0, ic1, ir0, ir1, ic, ir, jc, jr, iv, ivmin, ivmax, nmerge, lddp, lddn, nbr
  integer, dimension(:), allocatable :: catmap, catmapinv, cid, cidinv, iwrk
  integer, dimension(:,:), allocatable :: bb, bord
  double precision :: areamin, xmin, ymin, xmax, ymax, nbrarea
  double precision, dimension(:), allocatable :: area, areas
  !
  ! stencil parameters
  integer, parameter :: jsw = 1 
  integer, parameter :: js  = 2 
  integer, parameter :: jse = 3 
  integer, parameter :: jw  = 4 
  integer, parameter :: jp  = 5 
  integer, parameter :: je  = 6 
  integer, parameter :: jnw = 7 
  integer, parameter :: jn  = 8 
  integer, parameter :: jne = 9 
  integer, parameter :: nst = jne
  integer, dimension(2,nst) :: st
  integer, dimension(2,nst) :: stldd
  
  ! stencil
  st(1,jsw) = -1; st(2,jsw) =  1
  st(1,js)  =  0; st(2,js)  =  1
  st(1,jse) =  1; st(2,jse) =  1
  st(1,jw)  = -1; st(2,jw)  =  0
  st(1,jp)  =  0; st(2,jp)  =  0
  st(1,je)  =  1; st(2,je)  =  0
  st(1,jnw) = -1; st(2,jnw) = -1
  st(1,jn)  =  0; st(2,jn)  = -1
  st(1,jne) =  1; st(2,jne) = -1
  !
  ! ldd stencil
  ! P             N
  stldd(1,jsw) = 1; stldd(2,jsw) = 9
  stldd(1,js)  = 2; stldd(2,js)  = 8
  stldd(1,jse) = 3; stldd(2,jse) = 7
  stldd(1,jw)  = 4; stldd(2,jw)  = 6
  stldd(1,jp)  = 0; stldd(2,jp)  = 0
  stldd(1,je)  = 6; stldd(2,je)  = 4
  stldd(1,jnw) = 7; stldd(2,jnw) = 3
  stldd(1,jn)  = 8; stldd(2,jn)  = 2
  stldd(1,jne) = 9; stldd(2,jne) = 1
  
  ! read catchments
  call getarg(1, f)
  call imod_utl_printtext('Reading '//trim(f),0)
  if (.not.idfread(catg,f,1)) then
    call imod_utl_printtext('Could not read '//trim(f),2)
  end if
  call imod_utl_printtext('Done reading '//trim(f),0)

  ! read ldd
  call getarg(2, f)
  call imod_utl_printtext('Reading '//trim(f),0)
  if (.not.idfread(lddg,f,1)) then
    call imod_utl_printtext('Could not read '//trim(f),2)
  end if
  call imod_utl_printtext('Done reading '//trim(f),0)
  
  !
  ! read area
  call getarg(3, f)
  call imod_utl_printtext('Reading '//trim(f),0)
  if (.not.idfread(areag,f,1)) then
    call imod_utl_printtext('Could not read '//trim(f),2)
  end if
  call imod_utl_printtext('Done reading '//trim(f),0)
  !
  call getarg(4, s); read(s,*) areamin
  areamin = areamin * 1000 * 1000
  
  ! count the number of catchments
  ncat = 0
  ivmin = huge(0)
  ivmax = -huge(0)
  do ir = 1, catg%nrow
    do ic = 1, catg%ncol
      if (catg%x(ic,ir) /= catg%nodata) then
        iv = int(catg%x(ic,ir))
        ivmin = min(ivmin,iv)
        ivmax = max(ivmax,iv)
        ncat = ncat + 1
      end if
    end do
  end do
  !
  allocate(catmap(ncat))
  do i = 1, ncat
    catmap(i) = 0
  end do
  
  do ir = 1, catg%nrow
    do ic = 1, catg%ncol
      if (catg%x(ic,ir) /= catg%nodata) then
        iv = int(catg%x(ic,ir))
        catmap(iv) = 1
      end if
    end do
  end do
  !
  ! mappings
  mcat = 0
  do i = 1, ncat
    if (catmap(i) == 1) then
      mcat = mcat + 1
      catmap(i) = mcat
    end if
  end do
  allocate(catmapinv(mcat))
  do i = 1, ncat
    j = catmap(i)
    if (j > 0) then
      catmapinv(j) = i
    end if
  end do
  
  allocate(area(mcat), bb(4,mcat))
  do i = 1, mcat
    area(i) = DZERO
    bb(1,i) = catg%nrow+1
    bb(2,i) = 0
    bb(3,i) = catg%ncol+1
    bb(4,i) = 0
  end do
  
  do ir = 1, catg%nrow
    do ic = 1, catg%ncol
      if (catg%x(ic,ir) /= catg%nodata) then
        iv = int(catg%x(ic,ir))
        i = catmap(iv)
        area(i) = area(i) + areag%x(1,ir)
        catg%x(ic,ir) = real(i)
        bb(1,i) = min(bb(1,i),ir) ! row start
        bb(2,i) = max(bb(2,i),ir) ! row end
        bb(3,i) = min(bb(3,i),ic) ! col start
        bb(4,i) = max(bb(4,i),ic) ! col end
      end if
    end do
  end do
  !
  ! label the borders
  write(*,*) 'Computing border...'
  allocate(bord(catg%ncol,catg%nrow))
  do ir = 1, catg%nrow
    do ic = 1, catg%ncol
      bord(ic,ir) = 0
    end do
  end do
  do i = 1, mcat
    do ir = bb(1,i), bb(2,i)
      do ic = bb(3,i), bb(4,i)
        if (catg%x(ic,ir) /= catg%nodata) then
          if (catg%x(ic,ir) == real(i)) then
            do is = 1, nst
              jc = ic + st(1,is); jr = ir + st(2,is)
              jc = max(1, jc); jc = min(catg%ncol, jc)
              jr = max(1, jr); jr = min(catg%nrow, jr)
              if (catg%x(jc,jr) /= catg%nodata) then
                if (catg%x(jc,jr) /= real(i)) then
                  bord(jc,jr) = 1
                end if
              end if
            end do
          end if
        end if
      end do
    end do
  end do
  !
  ! set border
  do ir = 1, catg%nrow
    bord(1,ir) = 0
    bord(catg%ncol,ir) = 0
  end do
  write(*,*) 'Done computing border...'
  
  !deallocate(catg%x)
  !call writeidf("border.idf", bord, &
  !  catg%ncol, catg%nrow, catg%xmin, catg%ymin, catg%dx, 0.)
  !stop
    
  !
  allocate(cid(mcat), cidinv(mcat), iwrk(mcat), areas(mcat))
  nmerge = 0
  !
  ! initial sorting
  do i = 1, mcat
    cid(i) = i
    areas(i) = area(i)
  end do
  call quicksort_d(areas, cid, mcat)
  do i = 1, mcat
    j = cid(i)
    cidinv(j) = i
  end do
  !
  do while (.true.)
    ! init
    do i = 1, mcat
      iwrk(i) = 0
    end do
    ! sort
    if (areas(1) > areamin) then
      exit
    end if
    !
    ! loop over range of same cell sizes
    ip = cid(1)
    nmerge = nmerge + 1
    !if (nmerge == 1000) exit !@@@@debug
    nbr = 0
    if ((nmerge == 1) .or. (mod(nmerge,1000) == 0)) then
      write(*,*) 'Merging ',nmerge, catmapinv(ip),areas(1)
    end if
    !write(*,*) '  area = ',areas(ip)/(1000.*1000.),' sq. km'
    !write(*,*) '  bb = ',bb(1,ip), bb(2,ip), bb(3,ip), bb(4,ip)
    !write(*,*) '  (ncol,nrow)= ', bb(4,ip)-bb(3,ip)+1, bb(2,ip)-bb(1,ip)+1
    do ir =  bb(1,ip), bb(2,ip)
      do ic = bb(3,ip), bb(4,ip)
        if (bord(ic,ir) == 1.) then
          if (catg%x(ic,ir) == real(ip)) then
            lddp = int(lddg%x(ic,ir))
            !
            do is = 1, nst
              jc = ic + st(1,is); jr = ir + st(2,is)
              if (jc >= 1 .and. jc <= catg%ncol .and. jr >= 1 .and. jr <= catg%nrow) then
                if (catg%x(jc,jr) /= catg%nodata) then
                  lddn = int(lddg%x(jc,jr))
                  if ((lddp == stldd(1,is)).or.(lddn == stldd(2,is))) then
                    nbr = nbr + 1
                    iv = int(catg%x(jc,jr))
                    iwrk(iv) = iwrk(iv) + 1
                    catg%x(jc,jr) = -catg%x(jc,jr)
                  end if
                end if
              end if
            end do
            !jc = ic + st(1,ldd); jr = ir + st(2,ldd)
            !if (catg%x(jc,jr) /= catg%nodata) then
            !  iv = int(catg%x(jc,jr))
            !  if (iv /= ip) then
            !    nbr = nbr + 1
            !    iwrk(iv) = iwrk(iv) + 1
            !    catg%x(jc,jr) = -catg%x(jc,jr)
            !  end if  
            !end if
            !do is = 1, nst
            !  jc = ic + st(1,is); jr = ir + st(2,is)
            !  if (catg%x(jc,jr) /= catg%nodata) then
            !    iv = int(catg%x(jc,jr))
            !    if (iv > 0) then
            !      lnbr = .true.
            !      iwrk(iv) = iwrk(iv) + 1
            !      catg%x(jc,jr) = -catg%x(jc,jr)
            !    end if
            !  end if
            !end do
          end if
        end if
      end do
    end do
    !
    ir0 = max(1,        bb(1,ip)-1)
    ir1 = min(catg%nrow,bb(2,ip)+1)
    ic0 = max(1,        bb(3,ip)-1)
    ic1 = min(catg%ncol,bb(4,ip)+1)
    if (nbr > 0) then ! neighbor found
      write(*,*) '@@@nbr=',nbr
      inbr = maxloc(iwrk,1)
      area(inbr) = area(inbr) + area(ip)
      nbrarea = area(inbr)
      !
      do ir = ir0, ir1
        do ic = ic0, ic1
          if (catg%x(ic,ir) /= catg%nodata) then
            catg%x(ic,ir) = abs(catg%x(ic,ir))
            if (catg%x(ic,ir) == real(ip)) then
              catg%x(ic,ir) = real(inbr)
            end if
          end if
        end do
      end do
      !
      ! correct bounding box
      bb(1,inbr) = min(bb(1,inbr), bb(1,ip))
      bb(2,inbr) = max(bb(2,inbr), bb(2,ip))
      bb(3,inbr) = min(bb(3,inbr), bb(3,ip))
      bb(4,inbr) = max(bb(4,inbr), bb(4,ip))
      !
      ! correct the sorting list
      it = cidinv(inbr)
      do i = it, mcat-1
        if (areas(i+1) >= nbrarea) then
          jt = i
          exit
        end if
      end do
      do i = 1, it-2
        areas(i) = areas(i+1)
        cid(i) = cid(i+1)
      end do
      do i = it-1, jt-2
        areas(i) = areas(i+2)
        cid(i) = cid(i+2)
      end do
      areas(jt-1) = nbrarea
      cid(jt-1)   = inbr
      do i = jt, mcat-1
        areas(i) = areas(i+1)
        cid(i) = cid(i+1)
      end do
      areas(mcat) = huge(DZERO)
      cid(mcat) = ip
      do i = 1, mcat
        j = cid(i)
        cidinv(j) = i
      end do
    else ! no neighbor found
      do ir = ir0, ir1
        do ic = ic0, ic1
          if (catg%x(ic,ir) /= catg%nodata) then
            catg%x(ic,ir) = abs(catg%x(ic,ir))
          end if
        end do
      end do
      !
      ! correct the sorting list
      do i = 1, mcat-1
        areas(i) = areas(i+1)
        cid(i) = cid(i+1)
      end do
      area(mcat) = huge(DZERO)
      cid(mcat) = ip
      do i = 1, mcat
        j = cid(i)
        cidinv(j) = i
      end do
    end if
  end do
  
  ! write the IDF
  do ir = 1, catg%nrow
    do ic = 1, catg%ncol
      if (catg%x(ic,ir) /= catg%nodata) then
        iv = int(catg%x(ic,ir))
        iv = catmapinv(iv)
        catg%x(ic,ir) = real(iv)
      end if
    end do
  end do
  call getarg(5, f)
  call imod_utl_printtext('Writing '//trim(f),0)
  if (.not.idfwrite(catg,f,1)) then
    call imod_utl_printtext('Could not write '//trim(f),2)
  end if

end program
