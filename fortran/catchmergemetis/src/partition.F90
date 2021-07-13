! repos: https://repos.deltares.nl/repos/modflow6/trunk/utils_deltares/metis/partition
! revision: 73
!
! METIS 5.1.0
module metisvar

  implicit none

! parameters (these should be consistent with metis.h)
  integer, parameter :: METIS_NOPTIONS=40
  integer, parameter :: METIS_OK              = 1    !,    /*!< Returned normally */
  integer, parameter :: METIS_ERROR_INPUT     = -2   !,   /*!< Returned due to erroneous inputs and/or options */
  integer, parameter :: METIS_ERROR_MEMORY    = -3   !,   /*!< Returned due to insufficient memory */
  integer, parameter :: METIS_ERROR           = -4   !    /*!< Some other errors */

!                       1234567890123456789012
  integer, parameter :: METIS_OPTION_PTYPE     = 1
  integer, parameter :: METIS_OPTION_OBJTYPE   = 2   ! kway
  integer, parameter :: METIS_OPTION_CTYPE     = 3   ! recursive/kway
  integer, parameter :: METIS_OPTION_IPTYPE    = 4   ! recursive/kway
  integer, parameter :: METIS_OPTION_RTYPE     = 5   ! recursive/kway
  integer, parameter :: METIS_OPTION_DBGLVL    = 6   ! recursive/kway
  integer, parameter :: METIS_OPTION_NITER     = 7   ! recursive/kway
  integer, parameter :: METIS_OPTION_NCUTS     = 8   ! recursive/kway
  integer, parameter :: METIS_OPTION_SEED      = 9   ! recursive!kway
  integer, parameter :: METIS_OPTION_NO2HOP    = 10  ! recursive/kway
  integer, parameter :: METIS_OPTION_MINCONN   = 11  ! kway
  integer, parameter :: METIS_OPTION_CONTIG    = 12  ! kway
  integer, parameter :: METIS_OPTION_COMPRESS  = 13
  integer, parameter :: METIS_OPTION_CCORDER   = 14
  integer, parameter :: METIS_OPTION_PFACTOR   = 15
  integer, parameter :: METIS_OPTION_NSEPS     = 16
  integer, parameter :: METIS_OPTION_UFACTOR   = 17  ! recursive/kway
  integer, parameter :: METIS_OPTION_NUMBERING = 18  ! recursive/kway

! /* Used for command-line parameter purposes */
  integer, parameter :: METIS_OPTION_HELP      = 19
  integer, parameter :: METIS_OPTION_TPWGTS    = 20
  integer, parameter :: METIS_OPTION_NCOMMON   = 21
  integer, parameter :: METIS_OPTION_NOOUTPUT  = 22
  integer, parameter :: METIS_OPTION_BALANCE   = 23
  integer, parameter :: METIS_OPTION_GTYPE     = 24
  integer, parameter :: METIS_OPTION_UBVEC     = 25

  integer, parameter :: nopt = METIS_OPTION_UBVEC
  integer, parameter :: iint = 1, ikey = 2
  type tKey
     character(len=30) :: key
     integer :: val
  end type tKey
   type tOpt
     character(len=25) :: name
     logical           :: ldefonly = .true.
     logical           :: lset     = .false.
     integer           :: typ = 0
     integer           :: ival = -1
     integer           :: ikey = 0
     integer           :: nvalidkeys = 0
     type(tKey), dimension(:), allocatable :: validkeys
  end type tOpt
  type(tOpt), dimension(nopt), save :: metisopt

!    Types of objectives
  integer, parameter :: METIS_OBJTYPE_CUT  = 0
  integer, parameter :: METIS_OBJTYPE_VOL  = 1
  integer, parameter :: METIS_OBJTYPE_NODE = 2
!    Coarsening Schemes
  integer, parameter :: METIS_CTYPE_RM   = 0
  integer, parameter :: METIS_CTYPE_SHEM = 1
!    Initial partitioning schemes
  integer, parameter :: METIS_IPTYPE_GROW    = 0
  integer, parameter :: METIS_IPTYPE_RANDOM  = 1
  integer, parameter :: METIS_IPTYPE_EDGE    = 2
  integer, parameter :: METIS_IPTYPE_NODE    = 3
  integer, parameter :: METIS_IPTYPE_METISRB = 4
!    Refinement schemes
  integer, parameter :: METIS_RTYPE_FM        = 0
  integer, parameter :: METIS_RTYPE_GREEDY    = 1
  integer, parameter :: METIS_RTYPE_SEP2SIDED = 2
  integer, parameter :: METIS_RTYPE_SEP1SIDED = 3
!    Debug Levels
  integer, parameter :: METIS_DBG_INFO       = 1    ! Shows various diagnostic messages
  integer, parameter :: METIS_DBG_TIME       = 2    ! Perform timing analysis
  integer, parameter :: METIS_DBG_COARSEN    = 4	  ! Show the coarsening progress
  integer, parameter :: METIS_DBG_REFINE     = 8	  ! Show the refinement progress
  integer, parameter :: METIS_DBG_IPART      = 16   ! Show info on initial partitioning
  integer, parameter :: METIS_DBG_MOVEINFO   = 32   ! Show info on vertex moves during refinement
  integer, parameter :: METIS_DBG_SEPINFO    = 64   ! Show info on vertex moves during sep refinement
  integer, parameter :: METIS_DBG_CONNINFO   = 128  ! Show info on minimization of subdomain connectivity
  integer, parameter :: METIS_DBG_CONTIGINFO = 256  ! Show info on elimination of connected components
  integer, parameter :: METIS_DBG_MEMORY     = 2048 ! Show info related to wspace allocation

  ! locals (METIS arguments, see manual p. 26)
  integer                            :: nvtxs         ! number of vertices in the graph
  integer                            :: ncon          ! number of balancing constraints
  integer, dimension(:), allocatable :: xadj, adjncy  ! adjacency structure of the graph
  integer, dimension(:), allocatable :: vwgt          ! the weights of the vertices (dim: nvtxs)
  integer, dimension(:), allocatable :: vsize         ! size of vertices for computing the total communication volume (dim: nvtxs)
  integer, dimension(:), allocatable :: adjwgt        ! weights of the edges
  integer                            :: nparts        ! the number of partitions in the graph
  real, dimension(:), allocatable    :: tpwgts        ! desired weight for each partition and constraint (dim: nparts*ncon)
  real, dimension(:), allocatable    :: ubvec         ! allowed load imbalance for each constraint (dim: ncon)
  integer, dimension(METIS_NOPTIONS) :: opts          ! option array (options in METIS manual)
  integer                            :: objval        ! edge-cut ot total communication volume
  integer, dimension(:), allocatable :: part          ! partition vector of the graph (dim: nvtxs)

end module metisvar

module utils
  implicit none

  contains

  function getlun() result(lun)
! ******************************************************************************
    ! -- arguments
    integer :: lun
    ! -- locals
    logical :: lex
! ------------------------------------------------------------------------------

    do lun=10, 100
      inquire(unit=lun,opened=lex)
      if(.not.lex)exit
    end do

  end function getlun

  subroutine chkexist(fname)
! ******************************************************************************
    ! -- arguments
    character(len=*), intent(in) :: fname
    ! -- locals
    logical :: lex
! ------------------------------------------------------------------------------

    inquire(file=fname,exist=lex)
    if (.not.lex) then
      call errmsg('cannot find '//trim(fname))
    end if
  end subroutine chkexist

  function getdigits(n) result(ndig)
! ******************************************************************************
    ! -- arguments
    integer, intent(in) :: n
    integer :: ndig
! ------------------------------------------------------------------------------
    ndig = 1
    if (abs(n) > 10) ndig = 2
    if (abs(n) > 100) ndig = 3
    if (abs(n) > 1000) ndig = 4
    if (n < 0) ndig = ndig + 1
  end function

  subroutine writeasc(f, ix, ncol, nrow, xll, yll, cs, nodata)
! ******************************************************************************
    ! -- arguments
    character(len=*), intent(in) :: f
    integer, intent(in) :: ncol, nrow
    integer, dimension(ncol,nrow), intent(in) :: ix
    real, intent(in) :: xll, yll, cs, nodata
    ! -- locals
    integer :: lun, icol, irow, imin, imax, nd, ndmin, ndmax
    character(len=100) :: fmtstr
! ------------------------------------------------------------------------------

    write(*,'(1x,a,1x,2a)') 'Writing',trim(f),'...'

    imin = minval(ix)
    imax = maxval(ix)
    ndmin = getdigits(imin)
    ndmax = getdigits(imax)
    nd = max(ndmin,ndmax)

    lun = getlun()
    open(unit=lun,file=f,status='replace')
    write(lun,*) 'ncols',ncol
    write(lun,*) 'nrows',nrow
    write(lun,*) 'xllcorner',xll
    write(lun,*) 'yllcorner',yll
    write(lun,*) 'cellsize',cs
    write(lun,*) 'nodata_value',nodata
    write(fmtstr,*) '(',ncol,'i',nd+1,')'
    write(lun,fmtstr)((ix(icol,irow),icol=1,ncol),irow=1,nrow)
    close(lun)

  end subroutine writeasc

  subroutine errmsg(msg)
! ******************************************************************************
    ! -- arguments
    character(len=*), intent(in) :: msg
! ------------------------------------------------------------------------------
    write(*,'(a)') 'Error: '//trim(msg)
    stop 1

  end subroutine errmsg
end module utils

program  partmetis
! ******************************************************************************
   ! -- modules
   use metisvar
   use utils

   implicit none

   !--  general parameters
   integer, parameter :: mxstrlen = 1000
   integer, parameter :: inorth = 1, isouth = 2, iwest = 3, ieast = 4
   logical, parameter :: ldebug = .false.

   ! -- some METIS parameters
   real, parameter    :: tolimbal = 1.00001  ! load imbalance tolerance
   integer, parameter :: niter    = 100      ! number of iterations for the refinement algorithms at each stage of the uncoarsening process.
   integer, parameter :: contig   = 1        ! try to produce partitions that are contiguous
   logical, parameter :: pgrec    = .true.   ! multilevel recursive bisection

   ! -- local types
   type connType
     integer :: n = 0
     integer, dimension(:), allocatable :: m1_icol
     integer, dimension(:), allocatable :: m1_irow
     integer, dimension(:), allocatable :: m2_icol
     integer, dimension(:), allocatable :: m2_irow
   end type connType
   type intType
     integer :: nbr = 0
     integer, dimension(:), allocatable :: g2lpart
     type(connType), dimension(:), allocatable :: conn
   end type intType

   ! -- locals
   type(intType), dimension(:), allocatable :: partint
   integer :: ncol, nrow, ncolp, nrowp
   real :: xll, yll, cs
   character(len=1), parameter :: slash = '\'
   integer :: n, m, lun, ndig, nexg
   integer :: icol, irow, i, j, k, i1, i2, j1, j2, ip, jp, iact
   integer, dimension(3,4) :: inbr
   character :: cdum
   character(len=mxstrlen) :: f, str, vwgtfile, partfile, topfile, fmtstr
   character(len=mxstrlen), dimension(:), allocatable :: strarr
   integer(kind=4), dimension(:,:), allocatable :: wrk, wrkp
   integer(kind=4), dimension(:,:), allocatable :: topol
   integer, dimension(:,:), allocatable :: bb
   real :: totload, imbal
   real, dimension(:), allocatable :: load
   logical :: ldummy, lasc, lbnd
! ------------------------------------------------------------------------------

   ! read command line arguments
   n = nargs()
   if (n < 3) then
      call errmsg('invalid number of arguments')
   end if

   ! check if the partitioning is is for a dummy block
   ldummy = .false.
   if (n == 11) then
     ldummy = .true.
   end if

   call getarg(1,str); read(str,*) nparts
   if (.not.ldummy) then
     call getarg(2,vwgtfile)
     call chkexist(vwgtfile)
   end if
   !
   ! check is the boundary file is of ESRI ASCII format
   lasc = .false.
   if (.not. ldummy) then
     lun = getlun()
     open(unit=lun, file=vwgtfile, status='old')
     read(lun,'(a)') str
     if ((index(str,'NCOLS') > 0).or.(index(str,'ncols') > 0)) then
       lasc = .true.
       rewind(lun)
       read(lun,*) cdum, ncol
       read(lun,*) cdum, nrow
       read(lun,*) cdum, xll
       read(lun,*) cdum, yll
       read(lun,*) cdum, cs
     endif
     close(lun)
   end if
   !
   call getarg(3,partfile)
   call getarg(4,topfile)
   if (.not.lasc) then
     call getarg(5,str); read(str,*) ncol
     call getarg(6,str); read(str,*) nrow
     call getarg(7,str); read(str,*) xll
     call getarg(8,str); read(str,*) yll
     call getarg(9,str); read(str,*) cs
   endif
   ndig = getdigits(nparts)

   ! allocate some stuff
   allocate(wrk(ncol,nrow))
   allocate(strarr(max(nparts,10)))

   write(strarr(1),*) ncol
   write(strarr(2),*) nrow
   write(*,'(1x,a)') 'Problem size (ncol,nrow)=(' &
     //trim(adjustl(strarr(2)))//','//trim(adjustl(strarr(2)))//')'

   ! read the boundary file
   if (.not.ldummy) then
     write(*,'(a,1x,2a)') 'Reading nodal weights from',trim(vwgtfile),'...'
     lun = getlun()
     open(unit=lun,file=vwgtfile,status='old')
     if (lasc) then
       do i = 1, 6
         read(lun,'(a)') str
         write(*,*) 'Skipping',trim(str)
       enddo
     endif
     read(lun,*)((wrk(icol,irow),icol=1,ncol),irow=1,nrow)
     close(lun)
   else
     write(*,*) '*** using dummy ***'
     wrk = 1
   endif
   !
   nvtxs = count(wrk /= 0)

   ! allocate and fill other variables
   allocate(vwgt(nvtxs),vsize(nvtxs),part(nvtxs),xadj(nvtxs+1), &
     adjncy(4*nvtxs),adjwgt(4*nvtxs))
   ncon = 1
   allocate(tpwgts(nparts*ncon),ubvec(ncon))
   ! set vertex weights
   n = 0
   do irow = 1, nrow
     do icol = 1, ncol
       if (wrk(icol,irow) /= 0) then
         n = n + 1
         vwgt(n) = wrk(icol,irow)
       endif
     enddo
   enddo
   adjwgt = 1
   ubvec(1) = tolimbal
   tpwgts = 1./real(Nparts)
   vsize = 1

   ! set the node number if wrk
   n = 0
   do irow = 1, nrow
     do icol = 1, ncol
       if (wrk(icol,irow) /= 0) then
         n = n + 1
         wrk(icol,irow) = n
       endif
     enddo
   enddo

   ! fill xadj and adjncy
   xadj(1) = 0
   n = 1
   m = 0
   do irow = 1, nrow
     do icol = 1, ncol
       if (wrk(icol,irow) /= 0) then
         inbr = 0
         ! north
         if (irow > 1) then
           inbr(1,inorth) = wrk(icol,irow-1)
         endif
         ! south
         if (irow < nrow) then
           inbr(1,isouth) = wrk(icol,irow+1)
         endif
         ! west
         if (icol > 1) then
           inbr(1,iwest) = wrk(icol-1,irow)
         endif
         ! east
         if (icol < ncol) then
           inbr(1,ieast) = wrk(icol+1,irow)
         endif
         n = n + 1
         xadj(n) = xadj(n-1)
         do i = 1, 4
           if (inbr(1,i) > 0) then
             m = m + 1
             adjncy(m) = inbr(1,i) - 1
             xadj(n) = xadj(n) + 1
           endif
         enddo
       endif
     enddo
   enddo
   deallocate(wrk)

   ! debug output
   if (ldebug) then
     ndig = getdigits(nvtxs)
     do i = 1, nvtxs
       i1 = xadj(i); i2 = xadj(i+1)-1
       i1 = i1 + 1; i2 = i2 + 1 ! C --> Fortran
       n = i2-i1+1
       write(fmtstr,*) '(',n+1,'i',ndig+1,')'
       write(*,fmtstr) i,(adjncy(m),m=i1,i2)
     enddo
   endif

   ! set default options and overrule
   call METIS_SetDefaultOptions(opts)
   !opts(METIS_OPTION_NUMBERING) = 1 !Fortran-style
   opts(METIS_OPTION_NITER)  = niter
   if (ldebug) then
     opts(METIS_OPTION_DBGLVL) = METIS_DBG_INFO
   end if
   opts(METIS_OPTION_CONTIG) = contig

   ! call METIS
   if (pgrec) then
     write(*,*) 'Begin calling METIS using recursive bisection...'
     call METIS_PARTGRAPHRECURSIVE(nvtxs, ncon, xadj, adjncy, vwgt, vsize, &
       adjwgt, nparts, tpwgts, ubvec, opts, objval, part)
   else
     write(*,*) 'Begin calling METIS using k-way partitioning...'
     call METIS_PARTGRAPHKWAY(     nvtxs, ncon, xadj, adjncy, vwgt, vsize, &
       adjwgt, nparts, tpwgts, ubvec, opts, objval, part)
   end if
   write(*,*) 'End calling METIS...'

   ! clean up
   deallocate(xadj,adjncy,adjwgt,ubvec)

   ! read the boundary file
   allocate(wrk(ncol,nrow))
   if (.not.ldummy) then
     write(*,'(a,1x,2a)') 'Reading',trim(vwgtfile),'...'
     lun = getlun()
     open(unit=lun,file=vwgtfile,status='old')
     if (lasc) then
       do i = 1, 6
         read(lun,'(a)') str
         write(*,*) 'Skipping',trim(str)
       enddo
     endif
     read(lun,*)((wrk(icol,irow),icol=1,ncol),irow=1,nrow)
     close(lun)
   else
     write(*,*) '*** using dummy ***'
     wrk = 1
   endif

   ! set the paritions in the raster and determine load
   allocate(load(nparts))
   load = 0.
   n = 0
   do irow = 1, nrow
     do icol = 1, ncol
       if (wrk(icol,irow) /= 0) then
         n = n + 1
         wrk(icol,irow) = part(n) + 1
         ip = wrk(icol,irow)
         load(ip) = load(ip) + real(vwgt(n))
       endif
     enddo
   end do
   totload= sum(load)
   do ip = 1, nparts
     load(ip) = load(ip)/(totload*tpwgts(ip))
   end do
   imbal = maxval(load)
   !
   ! external boundaries
   do irow = 1, nrow
     do icol = 1, ncol
        lbnd = .false.
        if (irow > 1) then
          if (wrk(icol,irow-1) == 0) lbnd = .true.
        endif
        ! south
        if (irow < nrow) then
          if (wrk(icol,irow+1) == 0) lbnd = .true.
        endif
        ! west
        if (icol > 1) then
          if (wrk(icol-1,irow) == 0) lbnd = .true.
        endif
        ! east
        if (icol < ncol) then
          if (wrk(icol+1,irow) == 0) lbnd = .true.
        endif
        if (lbnd) then
          wrk(icol,irow) = -abs(wrk(icol,irow))
        endif
      enddo
   enddo

   ! clean-up
   deallocate(part,vwgt,tpwgts)

   ! determine the load imbalance and print
   write(strarr(1),*) objval; write(strarr(2),*) imbal
   write(*,*) repeat('=',50)
   write(*,'(1x,3(a,1x),a)') 'METIS results: #edge-cut =', &
     trim(adjustl(strarr(1))), ', imbalance =',trim(adjustl(strarr(2)))
   write(*,*) repeat('=',50)

   ! determine the tolopogy and bounding boxes
   allocate(topol(nparts,nparts))
   allocate(bb(4,nparts))
   do i = 1, nparts
     bb(1,i) =  ncol
     bb(2,i) =  1
     bb(3,i) =  nrow
     bb(4,i) =  1
   enddo
   topol = 0
   do iact = 1, 2
     do irow = 1, nrow
       do icol = 1, ncol
         ip = abs(wrk(icol,irow))
         if (ip /= 0) then
           bb(1,ip) = min(icol,bb(1,ip)); bb(2,ip) = max(icol,bb(2,ip))
           bb(3,ip) = min(irow,bb(3,ip)); bb(4,ip) = max(irow,bb(4,ip))
           inbr = 0
           ! north
           if (irow > 1) then
             inbr(1,inorth) = abs(wrk(icol,irow-1))
             inbr(2,inorth) = icol; inbr(3,inorth) = irow-1
           endif
           ! south
           if (irow < nrow) then
             inbr(1,isouth) = abs(wrk(icol,irow+1))
             inbr(2,isouth) = icol; inbr(3,isouth) = irow+1
           endif
           ! west
           if (icol > 1) then
             inbr(1,iwest) = abs(wrk(icol-1,irow))
             inbr(2,iwest) = icol-1; inbr(3,iwest) = irow
           endif
           ! east
           if (icol < ncol) then
             inbr(1,ieast) = abs(wrk(icol+1,irow))
             inbr(2,ieast) = icol+1; inbr(3,ieast) = irow
           endif
           do i = 1, 4
             jp = inbr(1,i)
             if ((jp > 0) .and. (ip /= jp)) then
               if (iact == 1) then
                 topol(jp,ip) = topol(jp,ip) + 1
               else
                 if (topol(jp,ip) > 0) then
                   j = partint(ip)%g2lpart(jp)
                   n = partint(ip)%conn(j)%n
                   n = n + 1
                   partint(ip)%conn(j)%m1_icol(n) = icol
                   partint(ip)%conn(j)%m1_irow(n) = irow
                   partint(ip)%conn(j)%m2_icol(n) = inbr(2,i)
                   partint(ip)%conn(j)%m2_irow(n) = inbr(3,i)
                   partint(ip)%conn(j)%n = n
                 endif
               endif
             endif
           enddo
         endif
       enddo
     enddo
     if (iact == 1) then
       ! label lower triangle
       do ip = 1, nparts
          do jp = 1, nparts
            if (ip > jp) then
              topol(jp,ip) = -topol(jp,ip)
            endif
          enddo
       enddo
       ! allocate
       allocate(partint(nparts))
       do ip = 1, nparts
          n = 0
          do jp = 1, nparts
            if (topol(jp,ip) > 0) then
              n = n + 1
            endif
          enddo
          partint(ip)%nbr = n
          if (n > 0) then
            allocate(partint(ip)%conn(n))
            allocate(partint(ip)%g2lpart(nparts))
            partint(ip)%g2lpart = 0
            n = 0
            do jp = 1, nparts
              m = topol(jp,ip)
              if (m > 0) then
                n = n + 1
                partint(ip)%g2lpart(jp) = n
                allocate(partint(ip)%conn(n)%m1_icol(m))
                allocate(partint(ip)%conn(n)%m1_irow(m))
                allocate(partint(ip)%conn(n)%m2_icol(m))
                allocate(partint(ip)%conn(n)%m2_irow(m))
              endif
            enddo
          endif
       enddo
     endif
   enddo

   ! write the total partition raster
   call writeasc(partfile,wrk,ncol,nrow,xll,yll,cs,0.)

   ! write the separate partitions
   n = index(partfile,'.',back=.true.)
   do ip = 1, nparts
     j1 = bb(1,ip); j2 = bb(2,ip)
     i1 = bb(3,ip); i2 = bb(4,ip)
     ncolp = j2-j1+1; nrowp = i2-i1+1
     allocate(wrkp(ncolp,nrowp))
     wrkp = 0
     do irow = i1, i2
       do icol = j1, j2
         if (abs(wrk(icol,irow)) == ip) then
           wrkp(icol-j1+1,irow-i1+1) = wrk(icol,irow)
         end if
       enddo
     end do
     do jp = 1, nparts
        if (partint(ip)%nbr > 0) then
          i = partint(ip)%g2lpart(jp)
          if (i > 0) then
            do j = 1, partint(ip)%conn(i)%n
               icol = partint(ip)%conn(i)%m1_icol(j)
               irow = partint(ip)%conn(i)%m1_irow(j)
               wrkp(icol-j1+1,irow-i1+1) = jp
            enddo
          endif
        endif
     enddo

     if (n > 0) then
       write(f,'(2a,i4.4,a)') partfile(1:n-1),'_p',ip,'.asc'
     else
       write(f,'(2a,i4.4,a)') trim(partfile),'_p',ip,'.asc'
     endif
     call writeasc(f,wrkp,ncolp,nrowp,xll+(j1-1)*cs,yll+(nrow-i2)*cs,cs,0.)
     deallocate(wrkp)
   enddo

   ! write the topology
   write(*,'(1x,a,1x,2a)') 'Writing',trim(topfile),'...'
   lun = getlun()
   open(unit=lun,file=topfile,status='replace')
   write(lun,'(a)') '#partitions'
   write(str,*) nparts
   write(lun,'(a)') trim(adjustl(str))
   write(lun,'(a)') '#(connected partitions), partitions'
   do ip = 1, nparts
     do jp = 1, nparts
       if (abs(topol(jp,ip))> 0) then
         topol(jp,ip) = 1
       endif
     enddo
     n = sum(topol(:,ip))
     write(strarr(1),*) n
     n = 1
     do jp = 1, nparts
       if ((topol(jp,ip)> 0)) then
         n = n + 1
         write(strarr(n),*) jp
       endif
     enddo
     write(fmtstr,*) '(a,',n-1,'(1x,a))'
     write(lun,fmtstr)(trim(adjustl(strarr(j))),j=1,n)
   enddo
   write(lun,'(a)') 'global icol_min, icol_max, irow_min, irow_max'
   write(fmtstr,*) '(a,',3,'(1x,a))'
   do ip = 1, nparts
     do i = 1, 4
       write(strarr(i),*) bb(i,ip)
     enddo
     write(lun,fmtstr)(trim(adjustl(strarr(j))),j=1,4)
   enddo
   close(lun)

   ! write the numerical exchange files
   n = 0
   do ip = 1, nparts
     n = n + partint(ip)%nbr
   enddo
   nexg = n
   n = 0
   do ip = 1, nparts
     do jp = 1, nparts
       if (partint(ip)%nbr > 0) then
         j = partint(ip)%g2lpart(jp)
         if (j > 0) then
           n = n + 1
           write(f,'(4(i3.3,a))') n,'_',nexg,'_s',ip,'s',jp,'.exg'
           write(*,'(1x,a,1x,2a)') 'Writing',trim(f),'...'
           lun = getlun()
           open(unit=lun,file=f,status='replace')
           write(lun,'(a)') 'BEGIN options'
           write(lun,'(a)') 'END options'//new_line('')
           write(lun,'(a)') 'BEGIN dimensions'
           write(str,*) partint(ip)%conn(j)%n
           write(lun,'(3x,a)') 'NEXG '//trim(adjustl(str))
           write(lun,'(a)') 'END dimensions'//new_line('')
           write(lun,'(a)') 'BEGIN exchangedata'
           do i = 1, partint(ip)%conn(j)%n
             j1 = partint(ip)%conn(j)%m1_icol(i)
             i1 = partint(ip)%conn(j)%m1_irow(i)
             j2 = partint(ip)%conn(j)%m2_icol(i)
             i2 = partint(ip)%conn(j)%m2_irow(i)
             !
             ! global --> local index
             j1 = j1 - bb(1,ip) + 1
             i1 = i1 - bb(3,ip) + 1
             j2 = j2 - bb(1,jp) + 1
             i2 = i2 - bb(3,jp) + 1
             write(strarr(1),*) 1
             write(strarr(2),*) i1
             write(strarr(3),*) j1
             write(strarr(4),*) 1
             write(strarr(5),*) i2
             write(strarr(6),*) j2
             write(strarr(7),*) 1
             write(strarr(8),*) cs/2
             write(strarr(9),*) cs/2
             write(strarr(10),*) cs
             write(lun,'(3x,9(a,1x),a)') (trim(adjustl(strarr(k))),k=1,10)
           enddo
           write(lun,'(a)') 'END exchangedata'
           close(lun)
         endif
       endif
     enddo
   enddo

end program