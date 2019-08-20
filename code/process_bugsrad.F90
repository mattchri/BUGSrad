!-------------------------------------------------------------------------------
! Name: process_bugsrad.F90
!
! Purpose:
! Main code to process BUGSrad broadband radaitive transfer code.
! Primarily for sensitivity testing. For full implementation into ORAC
! see https://github.com/ORAC-CC/orac/
!
! example
! ./process_bugsrad ../profiles/bugsrad_input.txt ../profiles/output/bugsrad.txt
!
! History:
! 2015/08/15, MC: Initial upload to repo
!-------------------------------------------------------------------------------
PROGRAM PROCESS_BUGSRAD

character(100) :: fileIn, fileOut
INTEGER :: nlev
REAL, dimension (:), allocatable :: zA, pA, tA, qA, qlA, qiA, qrA, o3A, cfA

integer, parameter :: NL = 129
integer, parameter :: NLS = NL+1
real :: pxTSI        ! Total incoming solar irradiance (true-earth)
real :: pxTheta      ! Cosine of solar zenith angle
real :: pxAsfcSWRdr  ! DIRECT visible surface albedo
real :: pxAsfcNIRdr  ! DIRECT near-infrared surface albedo
real :: pxAsfcSWRdf  ! DIFFUSE visible surface albedo
real :: pxAsfcNIRdf  ! DIFFUSE near-infrared surface albedo
real :: pxts         ! land/sea surface temperature
real :: pxemis      ! surface emissivity
real :: pxREF(1)     ! cloud effective radius

real :: pxasfcdir  ! DIRECT visible surface albedo
real :: pxasfcdif  ! DIFFUSE visible surface albedo

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
!-------------------------------------------------------------------------------

!Set Constants
pxYEAR = 2008
!pxTSI = 1361.
!pxtheta = 1.0
!pxts = 300.0       !surface temperature (K)

!Read Thermodynamic Profile
call get_command_argument(1,fileIn)
call get_command_argument(2,fileOut)
print*,fileIn
print*,fileOut


! Read Meteorological File
call read_line_numbers(fileIn,nlev)
print*,'number of model levels: ',nlev

allocate(zA(nlev))
allocate(pA(nlev))
allocate(tA(nlev))
allocate(qA(nlev))
allocate(qlA(nlev))
allocate(qiA(nlev))
allocate(qrA(nlev))
allocate(o3A(nlev))
allocate(cfA(nlev))
call read_profile(fileIn,nlev,pxTSI,pxtheta,pxts, pxemis, pxasfcdir, &
     pxasfcdif, zA, pA, tA, qA, qlA, qiA, qrA, o3A, cfA)
   print*,'z, p, t, q, ql, qi, qr o3, cf'
   do i=1, nlev
      print *,i,'  ',zA(i),tA(i),qA(i),qlA(i),qiA(i),qrA(i),o3A(i),cfA(i)
   end do

print*,nlev
print*,pxTSI,pxtheta,pxts, pxemis, pxasfcdir, pxasfcdif

emis_bugsrad(:) = pxemis !pure blakbody surface
rho_0d_bugsrad(:) = pxasfcdir !direct beam surface reflectance  (typical ocean)
rho_dd_bugsrad(:) = pxasfcdif !diffuse beam surface reflectance (typical ocean)

! effective radius
pxREF(1) = 10.0 !used regardless in the radiation code

call driver_for_bugsrad(nlev-1,pxTSI,pxtheta,pxAsfcSWRdr,&
      pxAsfcNIRdr,pxAsfcSWRdf,pxAsfcNIRdf,pxts,&
      pxREF,&
      zA,pA,tA,qA,o3A,qlA,qiA,qrA,cfA,&
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
      do l=1,nlev
         print '(5(F12.3))',pA(l),dswfx(1,l),uswfx(1,l),dlwfx(1,l),ulwfx(1,l)
      enddo

print*,'top of atmosphere incoming shortwave flux',pxtoaswdn
print*,'top of atmosphere upwelling shortwave flux (obs,clr)',pxtoaswup,pxtoaswupclr
print*,'top of atmosphere upwelling longwave flux (obs,clr)',pxtoalwup,pxtoalwupclr
print*,'bottom of atmosphere incoming shortwave flux (obs,clr)',pxboaswdn,pxboaswdnclr
print*,'bottom of atmosphere upwelling shortwave flux (obs,clr)',pxboaswup,pxboaswupclr
print*,'bottom of atmosphere incoming longwave flux (obs,clr)',pxboalwdn,pxboalwdnclr
print*,'bottom of atmosphere upwelling longwave flux (obs,clr)',pxboalwup,pxboalwupclr
print*,'top of atmosphere PAR',tpar
print*,'bottom of atmosphere PAR',bpar
print*,'bottom of atmosphere diffuse PAR',bpardif

   ! output data into a file 
   open(1, file = fileOut, status = 'replace')  
      write(1,*) "   Plev     SW_DN     SW_UP     LW_DN     LW_UP     SWDN_CLR   SWUP_CLR  LWDN_CLR  LWUP_CLR"
      write(1,*) "   hPa      W/m^2     W/m^2     W/m^2     W/m^2     W/m^2      W/m^2     W/m^2     W/m^2"
      do l=1,nlev
         write(1,'(9(F10.1, ","))') pA(l),dswfx(1,l),uswfx(1,l),dlwfx(1,l),ulwfx(1,l),&
              dswfxclr(1,l),uswfxclr(1,l),dlwfxclr(1,l),ulwfxclr(1,l)
      enddo
   close(1) 


END PROGRAM PROCESS_BUGSRAD
