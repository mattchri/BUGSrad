subroutine read_line_numbers(fname,nlev)

   implicit none

   ! Input arguments
   CHARACTER(100), intent(in) :: fname
   REAL :: TSI, theta, Ts, emis, asfcdir, asfcdif
   REAL :: z, p, t, q, ql, qi, qr, o3, cf
   INTEGER :: io
   INTEGER :: nlev
   
   nlev = 0
   OPEN(UNIT = 7, FILE = trim(fname) )
   READ(7,*, IOSTAT=io) TSI, theta, Ts, emis, asfcdir, asfcdif
   io = 0
   DO WHILE (io == 0)
   READ(7,*, IOSTAT=io) z, p, t, q, ql, qi, qr, o3, cf
    IF (io == 0) THEN
     nlev = nlev + 1
    ENDIF
   enddo
   CLOSE(UNIT=7)
   
end subroutine read_line_numbers
