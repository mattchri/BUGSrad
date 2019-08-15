

! CVS:  $Id: driver_read.F,v 1.10 2003/11/11 21:55:13 norm Exp $
! CVS:  $Name:  $


!-----------------------------------------------------------------------
      program driver_read

      use kinds, only:  int_kind, dbl_kind
      use bugsrad_physconst, only:  gravity, cp_dry_air, sol_const

      implicit none

!-----------------------------------------------------------------------
! driver_read is the main routine for running the CSU radiative transfer
! code offline (that is, apart from the CSU GCM).  It reads a profile
! from a file and also specifies variables that are not read in.  It
! then calls BUGSrad to do the radiative transfer.  This driver is not
! used when the code is compiled online with the CSU GCM.

! REFERENCES:
! Phil Partain /wombat (04-04-00).

! MODIFICATIONS:
! * changed declarations to adapt the code from BUGS4 to BUGS5.
!   Laura D. Fowler/slikrock (02-01-00).

! SUBROUTINES CALLED:
!     bugs_rad   :The radiative transfer code

! FUNCTIONS CALLED:
!     none.
 
! INCLUDED COMMON BLOCKS:
!     none.
 
! LOCAL VARIABLES:
      integer (kind=int_kind):: &
       nlen,&      !Length of total domain.
       len,&       !Length of sub domain.
       nlm,&       !Number of layers.
       i,l

      real (kind=dbl_kind), dimension(:), allocatable:: &
        ts   ,&      !Surface temperature                             (K).
        amu0 ,&      !Cosine of solar zenith angle                    (-).
        slr  ,&      !Fraction of daylight                            (-).
        alvdr,&      !Visible direct surface albedo                   (-).
        alndr,&      !Near-IR direct surface albedo                   (-).
        alvdf,&      !Visible diffuse surface albedo                  (-).
        alndf,&      !Near-IR diffuse surface albedo                  (-).
        umco2,&      !Col-avg concentration CO2                     (ppm).
        umch4,&      !Col-avg concentration CH4                     (ppm).
        umn2o        !Col-avg concentration N2O                     (ppm).

      real (kind=dbl_kind), dimension(:,:), allocatable:: &
        pl,&         !Layer pressure                                (hPa).
        dpl,&        !Layer thickness                               (hPa).
        tl ,&        !Temperature                                     (K).
        ql ,&        !Specific humidity                           (kg/kg).
        qcwl ,&      !Cloud water mixing ratio                    (kg/kg).
        qcil,&       !Cloud ice mixing ratio                      (kg/kg).
        qrwl,&       !Rain mixing ratio                           (kg/kg).
        qril,&       !Snow mixing ratio                           (kg/kg).
        o3l,&        !Ozone mixing ratio                          (kg/kg).
        acld         !Radiative cloud fraction                        (-).

      !Note that rain mixing ratio is unused by BUGSrad, but I've put it
      !into the std_profile.dat file for completeness

      real (kind=dbl_kind), dimension(:,:), allocatable:: &
        pl2          !Level pressure                                (hPa).

      real (kind=dbl_kind), dimension(:,:), allocatable:: &
        atl ,&       !All-sky LW radiative heating rate             (K/s).
        asl ,&       !All-sky SW radiative heating rate             (K/s).
        fulw,&       !All-sky LW upwelling flux                   (W/m^2).
        fdlw,&       !All-sky LW downwelling flux                 (W/m^2).
        fusw,&       !All-sky SW upwelling flux                   (W/m^2).
        fdsw         !All-sky SW downwelling flux                 (W/m^2).

      !For timing
      real, dimension(2) :: tarray
      real :: dtime, elapsed
      
      ! For reading the file
      character(LEN=1)   ::  l0
      character(LEN=200) ::  line
!-----------------------------------------------------------------------

!---- 1. READ PROFILE DATA FROM FILE:
      write(*,*) 'Reading profile data from profile.dat.'
!      open(10,file='profile.dat',action='read')
      
      l0='#'
      do while (l0=='#')
          read(*,"(A)") line
          read(line(1:1),"(A)") l0
      enddo      
      read(line,*) nlm
      nlen = 1    !to do timing tests
      len = nlen
!---- ALLOCATE ARRAYS
       allocate(ts(nlen) , amu0(nlen) , slr(nlen))

       allocate(alvdr(nlen) , alndr(nlen) , alvdf(nlen), alndf(nlen))
       allocate(umco2(nlen), umch4(nlen), umn2o(nlen))     
       allocate(pl(nlen,nlm), dpl(nlen,nlm), tl(nlen,nlm), ql(nlen,nlm), &
       qcwl(nlen,nlm) , qcil(nlen,nlm), qrwl(nlen,nlm), qril(nlen,nlm),  &
       o3l(nlen,nlm), acld(nlen,nlm))

       allocate(pl2(nlen,nlm+1))

       allocate(atl(nlen,nlm), asl(nlen,nlm), fulw(nlen,nlm+1), &
        fdlw(nlen,nlm+1), fusw(nlen,nlm+1), fdsw(nlen,nlm+1))
!----
      do l=1,nlm
        l0='#'
        do while (l0=='#')
            read(*,"(A)") line
            read(line(1:1),"(A)") l0
        enddo
        read(line,*) i,pl(1,l),pl2(1,l),tl(1,l),ql(1,l),o3l(1,l), &
                   qcwl(1,l), qcil(1,l), qrwl(1,l), qril(1,l), acld(1,l)
        pl2(1,l) = pl2(1,l)/100.       !convert from Pascals to millibars
        pl(1,l)  = pl(1,l)/100.        !convert from Pascals to millibars
      enddo
      l0='#'
      do while (l0=='#')
          read(*,"(A)") line
          read(line(1:1),"(A)") l0
      enddo
      read(line,*) pl2(1,nlm+1),ts(1), amu0(1), alvdr(1), alvdf(1), &
                 alndr(1), alndf(1), umco2(1), umch4(1), umn2o(1)
      pl2(1,nlm+1) = pl2(1,nlm+1)/100. !convert from Pascals to millibars
      close(10)

      do l=1,nlm
        dpl(1,l) = pl2(1,l+1)-pl2(1,l)
      enddo

! clouds?  Hardcoded here, can read in if you want to.
!
!      qcwl(1,:) = 0.0
!      qcil(1,:) = 0.0
!      qril(1,:) = 0.0
!
!      acld(1,:) = 0.0
!      acld(1,8) = 0.2
!      acld(1,9) = 0.3
!      acld(1,12) = 0.25
!      acld(1,13) = 0.25
!      acld(1,17) = 0.4
!      acld(1,18) = 0.3
!
!      qcil(1,8) = 0.0003
!      qcil(1,9) = 0.0003
!      qcil(1,12) = 0.0003
!      qcil(1,13) = 0.0003
!      qcil(1,17) = 0.0003
!      qcil(1,18) = 0.0003
!

!---- 2. COPY PROFILE TO ALL COLUMNS:
! copy the same column to all columns (only useful if testing multiple
! identical columns for timing, otherwise, it doesn't hurt)
      do i=1,nlen       
        pl2(i,:) = pl2(1,:)
        pl(i,:) = pl(1,:)
        dpl(i,:) = dpl(1,:)
        tl(i,:) = tl(1,:)
        ql(i,:) = ql(1,:)
        o3l(i,:) = o3l(1,:)
        acld(i,:) = acld(1,:)
        qcwl(i,:) = qcwl(1,:)
        qcil(i,:) = qcil(1,:)
        qril(i,:) = qril(1,:)
        amu0(i) = amu0(1)
        alvdr(i) = alvdr(1)
        alvdf(i) = alvdf(1)
        alndr(i) = alndr(1)
        alndf(i) = alndf(1)
      enddo

!---- 3. SPECIFY OTHER VARIABLES:
!      amu0(:) = 1.0
!
!      alvdr(:) = 0.2
!      alvdf(:) = 0.2
!      alndr(:) = 0.2
!      alndf(:) = 0.2

      slr(:) = 1.0

!---- 4. CALL THE RADIATIVE TRANSFER CODE:
      elapsed = dtime(tarray)
      call bugs_rad(nlen,len,nlm,pl2,pl,dpl,tl,ql,qcwl,qcil,qril, &
                    o3l,ts,amu0,slr,alvdf,alndf,alvdr,alndr,sol_const, &
                    gravity,cp_dry_air,asl,atl,fdsw,fusw,fdlw,fulw, &
                    acld, umco2, umch4, umn2o)
      elapsed = dtime(tarray)

!---- 5. OUTPUT RESULTS:
! print fluxes in W/m2, heating rates in K/day.
      print *, "Dtime: ", elapsed
      print *, " Fluxes   Plev       SW_DN       SW_UP       LW_DN       LW_UP"
      print *, "            Pa       W/m^2       W/m^2       W/m^2       W/m^2"
      do l=1,nlm+1
        print '(I4,5(F12.3))',l,pl2(1,l),fdsw(1,l),fusw(1,l), &
                                         fdlw(1,l),fulw(1,l)
      enddo
      print *, 'Heating Rates   Play              SW            LW'
      print *, '                  Pa           K/day         K/day'
      do l=1,nlm  
        print '(I4,6X, F12.3,2(F15.5))', &
               l,pl(1,l),asl(1,l)*86400.,atl(1,l)*86400. !K/day
      enddo

      end program driver_read

!-----------------------------------------------------------------------
