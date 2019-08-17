subroutine read_profile(fname,nlev,zA, pA, tA, qA, qlA, qiA, qrA)

   implicit none

   ! Input arguments
   CHARACTER(100), intent(in) :: fname
   INTEGER, intent(in) :: nlev
   REAL :: z, p, t, q, ql, qi, qr
   INTEGER :: io, i
   REAL, dimension (nlev) :: zA, pA, tA, qA, qlA, qiA, qrA
   INTEGER :: tnlev
   
   tnlev = 1
   OPEN(UNIT = 7, FILE = trim(fname) )
   io = 0
   DO WHILE (io == 0)
   READ(7,*, IOSTAT=io) z, p, t, q, ql, qi, qr
   IF (io == 0) THEN
    zA(tnlev) = z
    pA(tnlev) = p
    tA(tnlev) = t
    qA(tnlev) = q
    qlA(tnlev) = ql
    qiA(tnlev) = qi
    qrA(tnlev) = qr
    tnlev = tnlev + 1
   ENDIF
   enddo
   CLOSE(UNIT=7)

   !do i=1, nlev
   !   print *,i,'  ',zA(i),tA(i),qA(i),qlA(i),qiA(i),qrA(i)
   !end do
   
end subroutine read_profile
