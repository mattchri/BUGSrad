subroutine read_profile(fname,nlev,TSI,theta,Ts, emis, asfcdir, asfcdif, zA, pA, tA, qA, qlA, qiA, qrA, o3A, cfA)

   implicit none

   ! Input arguments
   CHARACTER(100), intent(in) :: fname
   INTEGER, intent(in) :: nlev
   REAL :: TSI, theta, Ts, emis, asfcdir, asfcdif
   REAL :: z, p, t, q, ql, qi, qr, o3, cf
   INTEGER :: io, i
   REAL, dimension (nlev) :: zA, pA, tA, qA, qlA, qiA, qrA, o3A, cfA
   INTEGER :: tnlev
   
   tnlev = 1
   OPEN(UNIT = 7, FILE = trim(fname) )
   READ(7,*, IOSTAT=io) TSI, theta, Ts, emis, asfcdir, asfcdif
   print*,TSI,theta,Ts,emis, asfcdir, asfcdif
   
   io = 0
   DO WHILE (io == 0)
   READ(7,*, IOSTAT=io) z, p, t, q, ql, qi, qr, o3, cf
   IF (io == 0) THEN
    zA(tnlev) = z
    pA(tnlev) = p
    tA(tnlev) = t
    qA(tnlev) = q
    qlA(tnlev) = ql
    qiA(tnlev) = qi
    qrA(tnlev) = qr
    o3A(tnlev) = o3
    cfA(tnlev) = cf
    tnlev = tnlev + 1
   ENDIF
   enddo
   CLOSE(UNIT=7)

end subroutine read_profile
