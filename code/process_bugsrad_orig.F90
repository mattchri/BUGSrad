!-------------------------------------------------------------------------------
! Name: process_bugsrad.F90
!
! Purpose:
! Main code to process BUGSrad broadband radaitive transfer code.
! Primarily for sensitivity testing. For full implementation into ORAC
! see https://github.com/ORAC-CC/orac/
!
! History:
! 2015/08/15, MC: Initial upload to repo
!-------------------------------------------------------------------------------

PROGRAM PROCESS_BUGSRAD_ORIG

!BUGSrad setup
integer, parameter :: NL = 29
integer, parameter :: NLS = NL+1
real, dimension(NLS) :: pxZ, pxP, pxT, pxQ, pxO3
real :: pxTSI        ! Total incoming solar irradiance (true-earth)
real :: pxTheta      ! Cosine of solar zenith angle
real :: pxAsfcSWRdr  ! DIRECT visible surface albedo
real :: pxAsfcNIRdr  ! DIRECT near-infrared surface albedo
real :: pxAsfcSWRdf  ! DIFFUSE visible surface albedo
real :: pxAsfcNIRdf  ! DIFFUSE near-infrared surface albedo
real :: pxts         ! land/sea surface temperature
real :: pxPhaseFlag(2) ! cloud phase type
integer :: ml_flag
real :: pxREF(2)       ! cloud effective radius
real :: pxCOT(2)       ! cloud optical depth
real :: pxHctop(2)     ! input cloud top height
real :: pxHcbase(2)    ! input cloud base height
integer :: pxHctopID(2),pxHcbaseID(2)

! Radiation flux profiles
real (kind=8), dimension(1,NL) :: &
      ulwfx   ,& ! all-sky upward longwave flux
      dlwfx   ,& ! all-sky downward longwave flux
      uswfx   ,& ! all-sky upward shortwave flux
      dswfx   ,& ! all-sky downward shortwave flux
      ulwfxclr,& ! clear-sky upward longwave flux
      dlwfxclr,& ! clear-sky downward longwave flux
      uswfxclr,& ! clear-sky upward shortwave flux
      dswfxclr   ! clear-sky downward shortwave flux

! Flux & PAR variables
real :: &
      pxtoalwup,pxtoaswdn,pxtoaswup ,&          ! All-sky TOA fluxes
      pxtoalwupclr,pxtoaswupclr,&               ! Clear-Sky TOA fluxes
      pxboalwup,pxboalwdn,pxboaswdn,pxboaswup,& ! All-sky BOA fluxes
      pxboalwupclr,pxboalwdnclr,pxboaswdnclr,pxboaswupclr,& ! clear-sky BOA fluxes
      tpar   ,& ! TOA PAR total
      bpardif,& ! BOA PAR diffuse
      bpar      ! BOA PAR total

real :: rho_0d_bugsrad(6),rho_dd_bugsrad(6),emis_bugsrad(12)

integer :: pxYear    ! Year

!Set Constants
pxYEAR = 2008
pxTSI = 1361.
pxtheta = .5
emis_bugsrad(:) = 0.99 !pure blakbody surface
rho_0d_bugsrad(:) = 0.15 !direct beam surface reflectance  (typical ocean)
rho_dd_bugsrad(:) = 0.15 !diffuse beam surface reflectance (typical ocean)
pxts = 300.0       !surface temperature (K)

! Cloud base & top
pxREF(:)       = -999.
pxCOT(:)       = -999.
pxHctop(:)     = -999.
pxHcbase(:)    = -999.
pxPhaseFlag(:) = -999.
pxHctopID(:)   = -999.
pxHcbaseID(:)  = -999.

ml_flag = 1 !one cloud layer
pxREF(1)       = 15.0
pxCOT(1)       = 10.0
pxHctop(1)     = 4.0 !cloud top height
pxHcbase(1)    = 1.0 !cloud base height
pxPhaseFlag(1) = 1   !water cloud
pxHctopID(1)   = 26   !cloud top height
pxHcbaseID(1)  = 29   !cloud base height


!Read Thermodynamic Profile
call midlatsum1(pxZ,pxP,pxT,pxQ,pxO3,NLS)
pxQ = pxQ/1000.

call driver_for_bugsrad_orig(NL,pxTSI,pxtheta,pxAsfcSWRdr,&
      pxAsfcNIRdr,pxAsfcSWRdf,pxAsfcNIRdf,pxts,&
      pxPhaseFlag,ml_flag,pxREF,pxCOT,pxHctop,pxHcbase,&
      pxHctopID,pxHcbaseID,&
      pxZ,pxP,pxT,pxQ,pxO3,&
      pxtoalwup,pxtoaswdn,pxtoaswup,&
      pxboalwup,pxboalwdn,pxboaswdn,pxboaswup,&
      pxtoalwupclr,pxtoaswupclr,&
      pxboalwupclr,pxboalwdnclr,pxboaswupclr,pxboaswdnclr,&
      bpar,bpardif,tpar,&
      ulwfx,dlwfx,uswfx,dswfx,&
      ulwfxclr,dlwfxclr,uswfxclr,dswfxclr,&
      emis_bugsrad,rho_0d_bugsrad,rho_dd_bugsrad,pxYEAR)

! print fluxes in W/m2
      print *, " Fluxes   Plev       SW_DN       SW_UP       LW_DN       LW_UP"
      print *, "            Pa       W/m^2       W/m^2       W/m^2       W/m^2"
      do l=1,NLS
         print '(I4,5(F12.3))',l,pxP(l),dswfx(1,l),uswfx(1,l),dlwfx(1,l),ulwfx(1,l)
      enddo

print*,'top of atmosphere incoming shortwave flux',pxtoaswdn
print*,'top of atmosphere upwelling shortwave flux',pxtoaswup
print*,'top of atmosphere upwelling longwave flux',pxtoalwup
print*,'bottom of atmosphere incoming shortwave flux',pxboaswdn
print*,'bottom of atmosphere upwelling shortwave flux',pxboaswup
print*,'bottom of atmosphere incoming longwave flux',pxboalwdn
print*,'bottom of atmosphere upwelling longwave flux',pxboalwup
print*,'top of atmosphere PAR',tpar
print*,'bottom of atmosphere PAR',bpar
print*,'bottom of atmosphere diffuse PAR',bpardif

END PROGRAM PROCESS_BUGSRAD_ORIG
