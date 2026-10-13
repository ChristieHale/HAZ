c  ***** PEER NGAEast Models (2018) **********

c ---------------------------------------------------------------------
c ** Goulet et al., 2018 (PEER Report 2018/08) **
c    Central and Eastern North America Ground-Motion Characterization
c    NGA-East Final Report
c
c    Applicable Range:
c    M 4.0 - 8.2
c    Rrup < 1500 km
c
c    Reference Site Conditions:
c    VS30 = 3000 m/s, kappa = 0.006 s
c    
c    Appendix H (17 median models)
c ---------------------------------------------------------------------
      subroutine S34_NGAEast_Med ( m, Rrup, specT, imod, period2, lnY, iflag )

      implicit none
      include 'pfrisk.h'

      integer MAX_FREQ
      parameter (MAX_FREQ=25)
      integer iflag, iflagRead(17), imod, isc, nsc, ifreq, f1, f2, m1d1, m1d2, m2d1, m2d2
      real m, Rrup, specT, period2, lnY, freq(MAX_FREQ), table_m(MAX_EAST),
     1     table_r(MAX_EAST), table_lnSA(17,MAX_FREQ,MAX_EAST), dum, specF,
     2     lnSAf(4), lnSAfm(2), period1
      character*80 table, dummy, freq_ch(MAX_FREQ)
      character*10 number

      common/GMTable/ iflagRead, table_m, table_r, table_lnSA, nsc

      data freq / 0.100, 0.133, 0.200, 0.250, 0.333, 0.500, 0.667, 1.000, 1.333,
     1            2.000, 2.500, 3.333, 4.000, 5.000, 6.667, 10.000, 13.333,
     2            20.000, 25.000, 33.333, 40.000, 50.000, 100.000, 100.000, -1.0 /

      data freq_ch / 'F0.100', 'F0.133', 'F0.200', 'F0.250', 'F0.333', 'F0.500', 'F0.667',
     1            'F1.000', 'F1.333', 'F2.000', 'F2.500', 'F3.333', 'F4.000', 'F5.000',
     2            'F6.667', 'F10.000', 'F13.333', 'F20.000', 'F25.000', 'F33.333', 'F40.000',
     3            'F50.000', 'F100.000', 'PGA', 'PGV' /

c     number of scenarios in the NGA-East Tables (rows)
      nsc = 374

c     write the model number integer into a string
      write(number, '(i0)') imod

c     read in the NGA-East Tables
      if (iflagRead(imod) .eq. 0) then
        do ifreq=1,MAX_FREQ
          table = 'H.7.1_Median_Models/'//TRIM(freq_ch(ifreq))//'/NGA_East_Model_'//trim(adjustl(number))//'.csv'
          open (404,file=table,status='old')
          read (404,*) dummy
          do isc=1,nsc
            if (ifreq .eq. 1) then
              read (404,*) table_m(isc), table_r(isc), table_lnSA(imod,ifreq,isc)
            else
              read (404,*) dum, dum, table_lnSA(imod,ifreq,isc)
            endif
          enddo
          close (404)
        enddo
        iflagRead(imod) = 1
      endif

c     m or Rrup is outside range defined by ground motion model
      if (m .lt. 4. .or. m .gt. 8.2) then
        write (*,*)
        write (*,*) 'The NGA-East ground motion models'
        write (*,*) '(Goulet et al., 2018) are not defined'
        write (*,*) 'for a magnitude less than 4 or greater than 8.2 '
        write (*,'(a27,f10.5,a11)') 'The specific magnitude of: ', m, 'is invalid.'
        write (*,*) 'Please check the input file.'
        write (*,*)
        stop 99
      elseif (Rrup .gt. 1500.) then
        write (*,*)
        write (*,*) 'The NGA-East ground motion models'
        write (*,*) '(Goulet et al., 2018) are not defined'
        write (*,*) 'for Rrup greater than 1500 km'
        write (*,'(a22,f10.5,a14)') 'The specific Rrup of: ', Rrup, 'km is invalid.'
        write (*,*) 'Please check the input file.'
        write (*,*)
        stop 99
      endif

c     Check for PGA, PGV
      if (specT .eq. 0.0) then
        specF = freq(24)
        goto 1019
      elseif (specT .eq. -1.0) then
        specF = freq(25)
        goto 1019
      endif

c     find frequencies for interpolation
      if (specT .gt. 0.0) then
        specF = 1./specT
        f1 = 0
        f2 = 0
        do ifreq=1,MAX_FREQ
          if (specF .ge. freq(ifreq) .and. specF .le. freq(ifreq+1) ) then
            f1 = ifreq
            f2 = ifreq+1
            goto 1019
          endif
        enddo
      endif

c     specified spectral period is outside range defined by ground motion model
      write (*,*)
      write (*,*) 'The NGA-East ground motion models'
      write (*,*) '(Goulet et al., 2018) are not defined'
      write (*,'(a26,f10.5,a2)') ' for a spectral period of: ', specT, ' s'
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

c     find magnitudes and distances for interpolation
 1019 m1d1 = 0
      m1d2 = 0
      m2d1 = 0
      m2d2 = 0
      do isc=1,nsc
        if (m .ge. table_m(isc) .and. m .le. table_m(isc+34) ) then
          if (Rrup .ge. table_r(isc) .and. Rrup .le. table_r(isc+1)) then
            m1d1 = isc
            m1d2 = isc + 1
            m2d1 = isc + 34
            m2d2 = isc + 35
            goto 1020
          endif
        endif
      enddo

c     Set lnSAf for PGA, PGV
 1020 if (specT .eq. 0.0) then
        lnSAf(1) = table_lnSA(imod,24,m1d1)
        lnSAf(2) = table_lnSA(imod,24,m1d2)
        lnSAf(3) = table_lnSA(imod,24,m2d1)
        lnSAf(4) = table_lnSA(imod,24,m2d2)
        goto 1021
      elseif (specT .eq. -1.0) then
        lnSAf(1) = table_lnSA(imod,25,m1d1)
        lnSAf(2) = table_lnSA(imod,25,m1d2)
        lnSAf(3) = table_lnSA(imod,25,m2d1)
        lnSAf(4) = table_lnSA(imod,25,m2d2)
        goto 1021
      endif

c     interpolate between frequencies
      lnSAf(1) = ((((table_lnSA(imod,f1,m1d1)-table_lnSA(imod,f2,m1d1))/(alog(freq(f1))-alog(freq(f2))))
     1                *(alog(freq(f1))-alog(specF)))-table_lnSA(imod,f1,m1d1))*(-1)
      lnSAf(2) = ((((table_lnSA(imod,f1,m1d2)-table_lnSA(imod,f2,m1d2))/(alog(freq(f1))-alog(freq(f2))))
     1                *(alog(freq(f1))-alog(specF)))-table_lnSA(imod,f1,m1d2))*(-1)
      lnSAf(3) = ((((table_lnSA(imod,f1,m2d1)-table_lnSA(imod,f2,m2d1))/(alog(freq(f1))-alog(freq(f2))))
     1                *(alog(freq(f1))-alog(specF)))-table_lnSA(imod,f1,m2d1))*(-1)
      lnSAf(4) = ((((table_lnSA(imod,f1,m2d2)-table_lnSA(imod,f2,m2d2))/(alog(freq(f1))-alog(freq(f2))))
     1                *(alog(freq(f1))-alog(specF)))-table_lnSA(imod,f1,m2d2))*(-1)

c     interpolate between magnitudes
 1021 lnSAfm(1) = ((((lnSAf(1)-lnSAf(3))/(table_m(m1d1)-table_m(m2d1)))
     1                *(table_m(m1d1)-m))-lnSAf(1))*(-1)
      lnSAfm(2) = ((((lnSAf(2)-lnSAf(4))/(table_m(m1d2)-table_m(m2d2)))
     1                *(table_m(m1d2)-m))-lnSAf(2))*(-1)

c     interpolate between distances
      if (Rrup .le. 1.) then
        lnY = ((((lnSAfm(1)-lnSAfm(2))/(table_r(m1d1)-table_r(m1d2)))
     1                *(table_r(m1d1)-Rrup))-lnSAfm(1))*(-1)
      else
        lnY = ((((lnSAfm(1)-lnSAfm(2))/(alog(table_r(m1d1))-alog(table_r(m1d2))))
     1                *(alog(table_r(m1d1))-alog(Rrup)))-lnSAfm(1))*(-1)
      endif

c     convert ground motion to units of gals (ok even for PGV, gets converted back)
      lnY = lnY + 6.89

      period1 = specT
      period2 = period1

      return
      end


c ---------------------------------------------------------------------
c ** Stewart et al. (2020) and Hashhash et al. (2020) NGA-East site amplification factors **
c    Site conditions are relative to NGA-East reference site conditions with
c    VS30 = 3000 m/s, kappa = 0.006 s
c    Site amplification model is based on factor for Vs30=760 and an additional
c         Flin (linear) and Fnl (nonlinear) for the selected Vs30 value.
c    For the Fnl the PGA for the reference site condition is required [PGAHR].
c
c ---------------------------------------------------------------------
      subroutine S34_NGAEast_SiteAmp ( vs, PGAHR, specT, siteAmp, iflag )

      implicit none
      include 'pfrisk.h'

      integer MAXPER, i, nPer
      parameter (MAXPER=40)
      real vs, PGAHR, SiteAmp, period2, specT, Vref, Fv, F2
      real Vl, Vu, Verf, w1, w2, Vw1, Vw2
      real wimp1, wimp2, wimp3, wimp, wgr
      real period(MAXPER), c(MAXPER), F760imp(MAXPER), F760gr(MAXPER)
      real V1(MAXPER), V2(MAXPER)
      real F3(MAXPER), F4(MAXPER), F5(MAXPER), Vc(MAXPER)
      real cT, F760impT, F760grT, V1T, V2T
      real F3T, F4T, F5T, VcT
      real F760, Flin, Fnl, Vrefprime
      integer iFlag, count1, count2

      data period / -1.0, 0.0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.075, 0.08, 0.1, 0.11,
     1             0.112, 0.113, 0.114, 0.115, 0.116, 0.117, 0.118, 0.119, 0.12, 0.125,
     2             0.13, 0.135, 0.14, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.75, 0.8, 1.0,
     3             1.5, 2.0, 3.0, 4.0, 5.0, 7.5, 10.0 /

      data c / -0.449, -0.29, -0.29, -0.303, -0.315, -0.331, -0.344, -0.348, -0.358, -0.372,
     1         -0.3741, -0.37456, -0.37479, -0.37503, -0.37526, -0.37549, -0.37573, -0.37597,
     2         -0.37621, -0.37645, -0.37768, -0.37898, -0.38036, -0.38182, -0.385, -0.403,
     3         -0.417, -0.426, -0.452, -0.48, -0.51, -0.523, -0.557, -0.574, -0.584, -0.588,
     4         -0.579, -0.558, -0.544, -0.507 /
      data V1 / 331.0, 319.0, 319.0, 319.0, 319.0, 319.0, 319.0, 319.0, 318.38, 317.13, 315.27,
     2          314.78, 314.52, 314.25, 313.98, 313.7, 313.41, 313.12, 312.82, 312.51, 310.9,
     3          309.19, 307.38, 305.51, 301.63, 279.0, 249.88, 224.5, 216.5, 216.88, 226.88,
     4          235.0, 254.75, 275.5, 296, 311.5, 321.25, 324.25, 325.0, 325.0 /
      data V2 / 760.0, 760.0, 760.0, 760.0, 810.0, 900.0, 1010.0, 1380.0, 1450.0, 1900.0,
     1          2000.0, 2000.0, 2000.0, 2000.0, 2000.0, 2000.0, 2000.0, 2000.0, 2000.0, 2000.0,
     2          2000.0, 2000.0, 1800.0, 1775.0, 1500.0, 1072.91, 944.81, 867.45, 842.72, 822.12,
     3          814.21, 810.0000, 790.00, 805.00, 810.00, 819.94, 821.33, 825.0, 819.76, 820.0 /
      data F760imp / 0.3753, 0.185, 0.185, 0.185, 0.224, 0.283, 0.337, 0.475, 0.512, 0.674, 0.72998,
     2               0.74137, 0.74742, 0.753, 0.75759, 0.76065, 0.76178, 0.7611, 0.75894, 0.75562,
     3               0.73234, 0.71641, 0.6687, 0.66026, 0.586, 0.419, 0.332, 0.27, 0.209, 0.175,
     4               0.127, 0.12, 0.095, 0.083, 0.079, 0.073, 0.066, 0.064, 0.056, 0.053 /
      data F760gr / 0.297, 0.121, 0.121, 0.031, 0.0, 0.012, 0.062, 0.211, 0.237, 0.338, 0.377,
     1              0.384, 0.388, 0.391, 0.394, 0.397, 0.4, 0.403, 0.406, 0.409, 0.422, 0.434,
     2              0.444, 0.454, 0.47, 0.509, 0.509, 0.498, 0.473, 0.447, 0.386, 0.378, 0.344,
     3              0.289, 0.258, 0.233, 0.224, 0.22, 0.216, 0.218 /


      data F3 / 0.06089, 0.089417, 0.075204, 0.056603, 0.103599, 0.118356, 0.16781, 0.173858,
     1          0.162486, 0.150834, 0.1436, 0.14122, 0.14004, 0.13887, 0.13772, 0.13657, 0.13543,
     2          0.13431, 0.13319, 0.132082, 0.131419, 0.130782, 0.13309, 0.135578, 0.142721, 0.128154,
     3          0.132857, 0.130701, 0.094143, 0.09888, 0.061011, 0.073572, 0.043672, 0.004796,
     4          0.00164, 0.007458, 0.002694, 0.002417, 0.042192, 0.053289 /
      data F4 / -0.08344, -0.44895, -0.43755, -0.41511, -0.49871, -0.48734, -0.58073, -0.53646,
     1          -0.50667, -0.44661, -0.42607, -0.41883, -0.41525, -0.41171, -0.4082, -0.40472,
     2          -0.40126, -0.39784, -0.39445, -0.39108, -0.38483, -0.37882, -0.37777, -0.37864,
     3          -0.38264, -0.30481, -0.27506, -0.22825, -0.11591, -0.07793, -0.0178, -0.01592,
     4          -0.00478, -0.00086, -0.00236, -0.00626, -0.00331, -0.00256, -0.00536, -0.00631 /
      data F5 / -0.00667, -0.00175, -0.00131, -0.00098, -0.00127, -0.00169, -0.00187, -0.00259,
     1          -0.00273, -0.00335, -0.00359, -0.00364, -0.00367, -0.00369, -0.00372, -0.00374,
     2          -0.00377, -0.00379, -0.00382, -0.00384, -0.00387, -0.0039, -0.00392, -0.00396,
     3          -0.0041, -0.00488, -0.00564, -0.00655, -0.00872, -0.01028, -0.01456, -0.01515,
     4          -0.01823, -0.02, -0.01296, -0.01043, -0.01215, -0.01325, -0.01418, -0.01403 /
      data Vc /2260.0, 2990.0, 2990.0, 2990.0, 2990.0, 2990.0, 2990.0, 2990.0, 2990.0, 2990.0,
     1         2990.0, 2990.0, 2990.0, 2990.0, 2990.0, 2990.0, 2990.0, 2990.0, 2990.0, 2990.0,
     2         2990.0, 2990.0, 2990.0, 2990.0, 2335.376, 1532.801, 1317.97, 1151.791, 1018.069,
     3         938.516, 834.7709, 832.4085, 950.9678, 882.2115, 878.8883, 894.3879, 874.8681,
     4         856.1077, 831.6034, 837.0494 /

C     Set period independent Coefficients
      Vl = 200.0
      Vu = 2000.0
      Vref = 760.0
      w1 = 0.767
      w2 = 0.1
      Vw1 = 600.0
      Vw2 = 400.0
      nPer = 40

C     Check that VS30 is within the range of the model (200 <= VS30 <= 3000 m/s)
      if (vs .lt. Vl .or. vs .gt. 3000.0) then
         write (*,*)
         write (*,*) 'Stewart et al. (2020) Site Amplification'
         write (*,*) ' model is not defined for a VS30 of: '
         write (*,'(a10,f10.2)') ' VS30   = ', vs
         write (*,*) 'The model is defined for 200 <= VS30 <= 3000 m/s.'
         write (*,*) 'Please check the input file.'
         write (*,*)
         stop 99
      endif

C First check for the PGV case
      if (specT .eq. -1.0) then
         cT = c(1)
         V1T = V1(1)
         V2T = V2(1)
         F760impT = F760imp(1)
         F760grT = F760gr(1)
         F3T = F3(1)
         F4T = F4(1)
         F5T = F5(1)
         VcT = Vc(1)
         goto 1011
C Next check for PGA case
      elseif (specT .eq. 0.0) then
         cT = c(2)
         V1T = V1(2)
         V2T = V2(2)
         F760impT = F760imp(2)
         F760grT = F760gr(2)
         F3T = F3(2)
         F4T = F4(2)
         F5T = F5(2)
         VcT = Vc(2)
         goto 1011
      endif

C   For other periods, loop over the spectral period range of the site amplification model.
      do i=3,nper-1
         if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
            count1 = i
            count2 = i+1
            goto 1020
         endif
      enddo

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'Stewart et al. (2020) Site Amplification'
      write (*,*) ' model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),c(count1),c(count2),
     +                   specT,cT,iflag)
      call S24_interp (period(count1),period(count2),V1(count1),V1(count2),
     +                   specT,V1T,iflag)
      call S24_interp (period(count1),period(count2),V2(count1),V2(count2),
     +                   specT,V2T,iflag)
      call S24_interp (period(count1),period(count2),F760imp(count1),F760imp(count2),
     +                   specT,F760impT,iflag)
      call S24_interp (period(count1),period(count2),F760gr(count1),F760gr(count2),
     +                   specT,F760grT,iflag)
      call S24_interp (period(count1),period(count2),F3(count1),F3(count2),
     +                   specT,F3T,iflag)
      call S24_interp (period(count1),period(count2),F4(count1),F4(count2),
     +                   specT,F4T,iflag)
      call S24_interp (period(count1),period(count2),F5(count1),F5(count2),
     +                   specT,F5T,iflag)
      call S24_interp (period(count1),period(count2),Vc(count1),Vc(count2),
     +                   specT,VcT,iflag)

 1011 period2 = specT

C     First compute the weight parameters
      if (vs .ge. Vw1) then
         wimp = w1
      elseif (vs .lt. Vw2) then
         wimp = w2
      else
         wimp = (w1-w2)*(alog(Vs/Vw2)/alog(Vw1/Vw2)) + w2
      endif
      wgr = 1.0 - wimp

C     Now compute the F760 term
      F760 = wimp*F760impT + wgr*F760grT

C     Now compute the Fv term
      if (vs .ge. Vl .and.  vs .le. V1T) then
         Fv = cT*alog(V1T/Vref)
      elseif (vs .gt. V1T .and. vs .le. V2T) then
         Fv = cT*alog(Vs/Vref)
      elseif (vs .gt. V2T .and. vs .le. Vu) then
         Fv = cT*alog(V2T/Vref)
      elseif (vs .gt. Vu .and. vs .le. 3000.0) then
         Fv = cT*alog(V2T/Vref) - (cT*alog(V2T/Vref)+F760)*(alog(vs/Vu)/alog(3000.0/Vu))
      endif

      Flin = F760 + Fv



C     Now compute the nonlinear part of the site amplification model
C     Apply the limit of Vref=760 for T<0.4sec and Vref=3000 for T>=0.4sec consistent with USGS implementation
      if (specT .ge. 0.4) then
          Vrefprime = 3000.0
      else
          Vrefprime = 760.0
      endif
      F2 = F4T*(Exp(F5T*(min(vs,Vrefprime)-360.0)) - Exp(F5T*(Vrefprime - 360.0)))
      if (Vs .lt. VcT) then
         Fnl = F2*alog((PGAHR+F3T)/F3T)
      else
         Fnl = 0.0
      endif

      SiteAmp = Flin + Fnl

      return
      end

c ---------------------------------------------------------------------
c ** Ramos-Sepulveda et al., 2024 (Earthquake Spectra with erratum to Table 1) **
c    Data-driven adjustments for combined use of NGA-East hard-rock ground motion
c    and site amplification models
c
c ---------------------------------------------------------------------
      subroutine S34_NGAEast_RS24Adj ( vs30, specT, period2, lnadj, iflag )

      implicit none
      include 'pfrisk.h'

      integer MAXPER
      parameter (MAXPER=26)
      integer iflag, nper, i, count1, count2
      real vs30, specT, lnY, period2
      real period(MAXPER), mu(MAXPER), b(MAXPER)
      real period1, muT, bT, lnadj, ck

      data period / 0.000, -1.000, 0.010, 0.020, 0.030, 0.040, 0.050, 0.075, 0.080, 0.100,
     1       0.150, 0.200, 0.250, 0.300, 0.400, 0.500, 0.750, 0.800, 1.00, 1.50,
     2       2.00, 3.00, 4.00, 5.00, 7.50, 10.0 /
      data mu / -0.040, -0.085, -0.070, -0.210, -0.211, -0.211, -0.212, -0.205, -0.205,
     1      -0.205, -0.192, -0.149, -0.132, -0.121, -0.097, -0.090, -0.040, -0.037, -0.010,
     2      0.045, 0.140, 0.270, 0.315, 0.377, 0.448, 0.401 /
      data b / -0.346, -0.250, -0.352, -0.305, -0.261, -0.230, -0.220, -0.220, -0.220,
     1      -0.220, -0.220, -0.220, -0.220, -0.220, -0.220, -0.220, -0.205, -0.193, -0.175,
     2      -0.173, -0.173, -0.173, -0.173, -0.173, -0.173, -0.173 /

c     Find the requested spectral period and corresponding coefficients
      nper = 26

c     First check for the PGA or PGV case
      if (specT .eq. 0.0) then
        period1 = period(1)
        muT = mu(1)
        bT = b(1)
        goto 1011
      elseif (specT .eq. -1.0) then
        period1 = period(2)
        muT = mu(2)
        bT = b(2)
        goto 1011
      endif

c     For other periods, loop over the spectral period range of the attenuation relationship.
      do i=3,nper-1
        if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
          count1 = i
          count2 = i+1
          goto 1020
        endif
      enddo

c     Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'Ramos-Sepulveda et al. (2024) adjustment factors'
      write (*,*) 'model is not defined for a spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

c     Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),mu(count1),mu(count2),
     +                   specT,muT,iflag)
      call S24_interp (period(count1),period(count2),b(count1),b(count2),
     +                   specT,bT,iflag)

 1011 period1 = specT

c     Compute ck from Equation 6 (stiff site modification)
      if (vs30 .le. 1000.) then
        ck = 0.0
      else if (vs30 .lt. 2000.) then
        ck = bT*alog(vs30/1000.)
      else
        ck = bT*alog(2.)
      endif

c     Combine base adjustment with stiff site modification
      lnadj = muT + ck

      return
      end
