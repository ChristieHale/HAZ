C     Various Empirical Attenuation Models

C -------------------------------------------------------------------
C****** Abrahamson and Silva (1997), Horizontal *********************
C -------------------------------------------------------------------

      subroutine S02_AS_97_H ( mag, dist, mech, soil, hw, specT, period1,
     1               lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=27)
      integer hw, nper, count1, count2, iflag, i
      real mag, dist, mech, mech1, n, c4(MAXPER), rockPGA
      real mag1, c5, period(MAXPER), mu, b1(MAXPER), b2(MAXPER)
      real lnY, sigma, soil, specT
      real a1(MAXPER), a2, a3(MAXPER), a4, a5(MAXPER), a6(MAXPER),
     1     a9(MAXPER), a10(MAXPER), a11(MAXPER), a12(MAXPER), a13
      real a1pga, a3pga, a5pga, a6pga, a9pga, a10pga, a11pga, a12pga
      real c4pga, b1pga, b2pga
      real a1sa, a3sa, a5sa, a6sa, a9sa, a10sa, a11sa, a12sa
      real c4sa, b1sa, b2sa, period1, soil1, rockPga1

      data b1 / 0.700, 0.700, 0.705, 0.713, 0.720, 0.728,
     1         0.735, 0.739, 0.746, 0.754, 0.759, 0.765, 0.772,
     2         0.780, 0.787, 0.791, 0.796, 0.799, 0.806, 0.814,
     3         0.819, 0.825, 0.840, 0.851, 0.866, 0.877, 0.885 /
      data b2 /  0.135, 0.135, 0.135, 0.135, 0.135, 0.135,
     1          0.135, 0.135, 0.135, 0.135, 0.135, 0.135, 0.135,
     2          0.135, 0.135, 0.135, 0.132, 0.130, 0.127, 0.123,
     3          0.121, 0.118, 0.110, 0.105, 0.097, 0.092, 0.087 /

      data period / 0.00, 0.03, 0.04, 0.05, 0.06, 0.075, 0.09, 0.10,
     1   0.12, 0.15, 0.17, 0.20, 0.24, 0.30, 0.36, 0.40, 0.46, 0.50,
     2         0.60 ,0.75, 0.85, 1.00, 1.50, 2.00, 3.00, 4.00, 5.00 /
      data c4 / 5.60, 5.60, 5.60, 5.60, 5.60, 5.58, 5.54, 5.50, 5.39,
     1          5.27, 5.20, 5.10, 4.97, 4.80, 4.62, 4.52, 4.38, 4.30,
     2          4.12, 3.90, 3.81, 3.70, 3.55, 3.50, 3.50, 3.50, 3.50 /
      data a1 / 1.640, 1.690, 1.780, 1.870, 1.940, 2.037, 2.100, 2.160,
     1          2.272, 2.407, 2.430, 2.406, 2.293, 2.114, 1.955, 1.860,
     2          1.717, 1.615, 1.428, 1.160, 1.020, 0.828, 0.260,-0.150,
     3         -0.690, -1.130, -1.460 /
      data a3 / -1.145, -1.145, -1.145, -1.145, -1.145, -1.145, -1.145,
     1          -1.145, -1.145, -1.145, -1.135, -1.115, -1.079, -1.035,
     2          -1.005, -0.988, -0.965, -0.952, -0.922, -0.885, -0.865,
     3          -0.838, -0.772, -0.725, -0.725, -0.725, -0.725 /
      data a5 / 0.610, 0.610, 0.610, 0.610, 0.610, 0.610, 0.610, 0.610,
     1          0.610, 0.610, 0.610, 0.610, 0.610, 0.610, 0.610, 0.610,
     2          0.592, 0.581, 0.557, 0.528, 0.512, 0.490, 0.438, 0.400,
     3          0.400, 0.400, 0.400 /
      data a6 / 0.260, 0.260, 0.260, 0.260, 0.260, 0.260, 0.260, 0.260,
     1          0.260, 0.260, 0.260, 0.260, 0.232, 0.198, 0.170, 0.154,
     2          0.132, 0.119, 0.091, 0.057, 0.038, 0.013,-0.049,-0.094,
     3         -0.156, -0.200, -0.200 /
      data a9 / 0.37, 0.37, 0.37, 0.37, 0.37, 0.37, 0.37, 0.37, 0.37,
     1          0.37, 0.37, 0.37, 0.37, 0.37, 0.37, 0.37, 0.37, 0.37,
     2    0.37, 0.331, 0.309, 0.281, 0.210, 0.160, 0.089, 0.039,0.0 /
      data a10 /-0.417, -0.470, -0.555, -0.620, -0.665, -0.628, -0.609,
     1          -0.598, -0.591, -0.577, -0.522, -0.445, -0.350, -0.219,
     2          -0.123, -0.065,  0.020,  0.085,  0.194,  0.320,  0.370,
     3            0.423, 0.600, 0.610, 0.630, 0.640, 0.664 /
      data a11 /-0.230, -0.230, -0.251, -0.267, -0.280, -0.280, -0.280,
     1          -0.280, -0.280, -0.280, -0.265, -0.245, -0.223, -0.195,
     2          -0.173, -0.160, -0.136, -0.121, -0.089, -0.050, -0.028,
     3           0.000,  0.040,  0.040,  0.040,  0.040,  0.040 /
      data a12 /0.0000, 0.0143, 0.0245, 0.0280, 0.0300, 0.0300, 0.0300,
     1   0.0280, 0.0180, 0.0050, -0.0040, -0.0138, -0.0238, -0.0360,
     2  -0.0460, -0.0518, -0.0594, -0.0635, -0.0740, -0.0862,
     3  -0.0927, -0.1020, -0.1200, -0.1400, -0.1726, -0.1956, -0.2150 /
      data mag1,n,a2,a4,a13,c5 / 6.4, 2.0, 0.512, -0.144, 0.17, 0.03 /

C Find the requested spectral period.
      nper = 27

C First Check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1=period(1)
         a1pga  = a1(1)
         a3pga  = a3(1)
         a5pga  = a5(1)
         a6pga  = a6(1)
         a9pga  = a9(1)
         a10pga = a10(1)
         a11pga = a11(1)
         a12pga = a12(1)
         c4pga  = c4(1)
         b1pga  = b1(1)
         b2pga  = b2(1)
         goto 1002
      elseif (specT .ne. 0.0) then
C Now interpolate the cofficients for different spectral periods.
C Interpolation is only done for the second spectral period and
C the last spectral period.

C Set PGA coefficients for non-pga spectral periods.
         a1pga  = a1(1)
         a3pga  = a3(1)
         a5pga  = a5(1)
         a6pga  = a6(1)
         a9pga  = a9(1)
         a10pga = a10(1)
         a11pga = a11(1)
         a12pga = a12(1)
         c4pga  = c4(1)

         do i=2, nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1001
            endif
         enddo

C Requested spectral period is not in the period range of the attenuation model.
         write (*,*)
         write (*,*) 'Abrahamson and Silva (1997) Horizontal atttenuation model'
         write (*,*) 'is not defined for a spectral period of: '
         write (*,'(a10,f10.5)') ' Period = ',specT
         write (*,*) 'This spectral period is outside the defined'
         write (*,*) 'period range in the code or beyond the range'
         write (*,*) 'of spectral periods for interpolation.'
         write (*,*) 'Please check the input file.'
         write (*,*)
         stop 99

C Interpolate the coefficients for the requested spectral period.
 1001       call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +                   specT,a1sa,iflag)
            call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +                   specT,a3sa,iflag)
            call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +                   specT,a5sa,iflag)
            call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +                   specT,a6sa,iflag)
            call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +                   specT,a9sa,iflag)
            call S24_interp (period(count1),period(count2),a10(count1),a10(count2),
     +                   specT,a10sa,iflag)
            call S24_interp (period(count1),period(count2),a11(count1),a11(count2),
     +                   specT,a11sa,iflag)
            call S24_interp (period(count1),period(count2),a12(count1),a12(count2),
     +                   specT,a12sa,iflag)
            call S24_interp (period(count1),period(count2),c4(count1),c4(count2),
     +                   specT,c4sa,iflag)
            call S24_interp (period(count1),period(count2),b1(count1),b1(count2),
     +                   specT,b1sa,iflag)
            call S24_interp (period(count1),period(count2),b2(count1),b2(count2),
     +                   specT,b2sa,iflag)
      endif

 1002 period1 = specT

c     Set normal faulting to Stike-slip
      mech1 = mech
      if ( mech .lt. 0. ) mech1 = 0.

c     Compute the rock PGA
      soil1 = 0.0
      rockPga1 = 0.
      call S02_Calcarg_as97 (mag,dist,mech1,soil1, hw, 0.0, rockPGA, rockpga1,
     1  a1pga, a2, a3pga, a4, a5pga, a6pga, a9pga, a10pga, a11pga,
     2  a12pga, a13, c4pga, c5, mag1, n)

c     Compute the soil PGA if requested
      if (soil .eq. 1) then
         call S02_Calcarg_as97 (mag,dist,mech1,soil, hw, 0.0, mu, rockpga,
     1        a1pga, a2, a3pga, a4, a5pga, a6pga, a9pga, a10pga, a11pga,
     2        a12pga, a13, c4pga, c5, mag1, n)
      else
         mu = rockPGA
      endif

c     Compute the Sa
      if (specT .ne. 0.0) then
         call S02_Calcarg_as97 (mag,dist, mech1, soil, hw, specT, mu, rockpga,
     1        a1sa, a2, a3sa, a4, a5sa, a6sa, a9sa, a10sa, a11sa, a12sa,
     2        a13, c4sa, c5, mag1, n)
      endif

       lnY = mu + 6.89

C     Set Standard Error.
      if (specT .eq. 0.0) then
         if (mag .le. 5.0) then
            sigma = b1pga
         elseif (mag .gt. 5.0 .and. mag .lt. 7.0) then
            sigma = b1pga - b2pga*(mag-5.0)
         elseif (mag .ge. 7.0) then
            sigma = b1pga - 2.0*b2pga
         endif
      else
         if (mag .le. 5.0) then
            sigma = b1sa
         elseif (mag .gt. 5.0 .and. mag .lt. 7.0) then
            sigma = b1sa - b2sa*(mag-5.0)
         elseif (mag .ge. 7.0) then
            sigma = b1sa - 2.0*b2sa
         endif
      endif

      return
      end

c ----------------------------------------------------------------------
C****** Abrahamson and Silva (1997), Horizontal with Normal Faulting ***
c ----------------------------------------------------------------------

      subroutine S02_AS_97_H_NF ( mag, dist, mech, soil, hw, specT, period1,
     1               lnY, sigma,iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=27)
      integer hw, nper, count1, count2, iflag, i
      real mag, dist, mech, mech1, n, c4(MAXPER), rockPGA
      real mag1, c5, period(MAXPER), mu, b1(MAXPER), b2(MAXPER)
      real lnY, sigma, soil, specT
      real a1(MAXPER), a2, a3(MAXPER), a4, a5(MAXPER), a6(MAXPER),
     1     a9(MAXPER), a10(MAXPER), a11(MAXPER), a12(MAXPER), a13
      real a1pga, a3pga, a5pga, a6pga, a9pga, a10pga, a11pga, a12pga
      real c4pga, b1pga, b2pga
      real a1sa, a3sa, a5sa, a6sa, a9sa, a10sa, a11sa, a12sa
      real c4sa, b1sa, b2sa, period1, soil1, rockPga1

      data b1 / 0.700, 0.700, 0.705, 0.713, 0.720, 0.728,
     1         0.735, 0.739, 0.746, 0.754, 0.759, 0.765, 0.772,
     2         0.780, 0.787, 0.791, 0.796, 0.799, 0.806, 0.814,
     3         0.819, 0.825, 0.840, 0.851, 0.866, 0.877, 0.885 /
      data b2 /  0.135, 0.135, 0.135, 0.135, 0.135, 0.135,
     1          0.135, 0.135, 0.135, 0.135, 0.135, 0.135, 0.135,
     2          0.135, 0.135, 0.135, 0.132, 0.130, 0.127, 0.123,
     3          0.121, 0.118, 0.110, 0.105, 0.097, 0.092, 0.087 /

      data period / 0.00, 0.03, 0.04, 0.05, 0.06, 0.075, 0.09, 0.10,
     1   0.12, 0.15, 0.17, 0.20, 0.24, 0.30, 0.36, 0.40, 0.46, 0.50,
     2        0.60 ,0.75, 0.85, 1.00, 1.50, 2.00, 3.00, 4.00,  5.00/
      data c4 / 5.60, 5.60, 5.60, 5.60, 5.60, 5.58, 5.54, 5.50, 5.39,
     1          5.27, 5.20, 5.10, 4.97, 4.80, 4.62, 4.52, 4.38, 4.30,
     2          4.12, 3.90, 3.81, 3.70, 3.55, 3.50, 3.50, 3.50, 3.50 /
      data a1 / 1.640, 1.690, 1.780, 1.870, 1.940, 2.037, 2.100, 2.160,
     1          2.272, 2.407, 2.430, 2.406, 2.293, 2.114, 1.955, 1.860,
     2          1.717, 1.615, 1.428, 1.160, 1.020, 0.828, 0.260,-0.150,
     3         -0.690, -1.130, -1.460 /
      data a3 / -1.145, -1.145, -1.145, -1.145, -1.145, -1.145, -1.145,
     1          -1.145, -1.145, -1.145, -1.135, -1.115, -1.079, -1.035,
     2          -1.005, -0.988, -0.965, -0.952, -0.922, -0.885, -0.865,
     3          -0.838, -0.772, -0.725, -0.725, -0.725, -0.725 /
      data a5 / 0.610, 0.610, 0.610, 0.610, 0.610, 0.610, 0.610, 0.610,
     1          0.610, 0.610, 0.610, 0.610, 0.610, 0.610, 0.610, 0.610,
     2          0.592, 0.581, 0.557, 0.528, 0.512, 0.490, 0.438, 0.400,
     3          0.400, 0.400, 0.400 /
      data a6 / 0.260, 0.260, 0.260, 0.260, 0.260, 0.260, 0.260, 0.260,
     1          0.260, 0.260, 0.260, 0.260, 0.232, 0.198, 0.170, 0.154,
     2          0.132, 0.119, 0.091, 0.057, 0.038, 0.013,-0.049,-0.094,
     3         -0.156, -0.200, -0.200 /
      data a9 / 0.37, 0.37, 0.37, 0.37, 0.37, 0.37, 0.37, 0.37, 0.37,
     1          0.37, 0.37, 0.37, 0.37, 0.37, 0.37, 0.37, 0.37, 0.37,
     2    0.37, 0.331, 0.309, 0.281, 0.210, 0.160, 0.089, 0.039,0.0 /
      data a10 /-0.417, -0.470, -0.555, -0.620, -0.665, -0.628, -0.609,
     1          -0.598, -0.591, -0.577, -0.522, -0.445, -0.350, -0.219,
     2          -0.123, -0.065,  0.020,  0.085,  0.194,  0.320,  0.370,
     3            0.423, 0.600, 0.610, 0.630, 0.640, 0.664 /
      data a11 /-0.230, -0.230, -0.251, -0.267, -0.280, -0.280, -0.280,
     1          -0.280, -0.280, -0.280, -0.265, -0.245, -0.223, -0.195,
     2          -0.173, -0.160, -0.136, -0.121, -0.089, -0.050, -0.028,
     3           0.000,  0.040,  0.040,  0.040,  0.040,  0.040 /
      data a12 /0.0000, 0.0143, 0.0245, 0.0280, 0.0300, 0.0300, 0.0300,
     1   0.0280, 0.0180, 0.0050, -0.0040, -0.0138, -0.0238, -0.0360,
     2  -0.0460, -0.0518, -0.0594, -0.0635, -0.0740, -0.0862,
     3  -0.0927, -0.1020, -0.1200, -0.1400, -0.1726, -0.1956, -0.2150 /
      data mag1,n,a2,a4,a13,c5 / 6.4, 2.0, 0.512, -0.144, 0.17, 0.03 /

C Find the requested spectral period.
      nper = 27

C First Check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1=period(1)
         a1pga  = a1(1)
         a3pga  = a3(1)
         a5pga  = a5(1)
         a6pga  = a6(1)
         a9pga  = a9(1)
         a10pga = a10(1)
         a11pga = a11(1)
         a12pga = a12(1)
         c4pga  = c4(1)
         b1pga  = b1(1)
         b2pga  = b2(1)
         goto 1004
      elseif (specT .ne. 0.0) then
C Now interpolate the cofficients for different spectral periods.
C Interpolation is only done for the second spectral period and
C the last spectral period.

C Set PGA coefficients for non-pga spectral periods.
         a1pga  = a1(1)
         a3pga  = a3(1)
         a5pga  = a5(1)
         a6pga  = a6(1)
         a9pga  = a9(1)
         a10pga = a10(1)
         a11pga = a11(1)
         a12pga = a12(1)
         c4pga  = c4(1)

         do i=2, nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1003
            endif
         enddo

C Requested spectral period is not in the period range of the attenuation model.
         write (*,*)
         write (*,*) 'Abrahamson and Silva (1997) Horizontal atttenuation model'
         write (*,*) 'for normal faulting regimes'
         write (*,*) 'is not defined for a spectral period of: '
         write (*,'(a10,f10.5)') ' Period = ',specT
         write (*,*) 'This spectral period is outside the defined'
         write (*,*) 'period range in the code or beyond the range'
         write (*,*) 'of spectral periods for interpolation.'
         write (*,*) 'Please check the input file.'
         write (*,*)
         stop 99

C Interpolate the coefficients for the requested spectral period.
 1003       call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +                   specT,a1sa,iflag)
            call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +                   specT,a3sa,iflag)
            call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +                   specT,a5sa,iflag)
            call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +                   specT,a6sa,iflag)
            call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +                   specT,a9sa,iflag)
            call S24_interp (period(count1),period(count2),a10(count1),a10(count2),
     +                   specT,a10sa,iflag)
            call S24_interp (period(count1),period(count2),a11(count1),a11(count2),
     +                   specT,a11sa,iflag)
            call S24_interp (period(count1),period(count2),a12(count1),a12(count2),
     +                   specT,a12sa,iflag)
            call S24_interp (period(count1),period(count2),c4(count1),c4(count2),
     +                   specT,c4sa,iflag)
            call S24_interp (period(count1),period(count2),b1(count1),b1(count2),
     +                   specT,b1sa,iflag)
            call S24_interp (period(count1),period(count2),b2(count1),b2(count2),
     +                   specT,b2sa,iflag)
      endif

 1004 period1 = specT

c     Set normal faulting to Stike-slip ( for using the existing model)
      mech1 = mech
      if ( mech .lt. 0. ) mech1 = 0.

c     Compute the rock PGA
      soil1 = 0.0
      rockPga1 = 0.
      call S02_Calcarg_as97 (mag,dist,mech,soil1, hw, 0.0, rockPGA, rockpga1,
     1  a1pga, a2, a3pga, a4, a5pga, a6pga, a9pga, a10pga, a11pga,
     2  a12pga, a13, c4pga, c5, mag1, n)

c     Compute the soil PGA if requested
      if (soil .eq. 1) then
         call S02_Calcarg_as97 (mag,dist,mech,soil, hw, 0.0, mu, rockpga,
     1        a1pga, a2, a3pga, a4, a5pga, a6pga, a9pga, a10pga, a11pga,
     2        a12pga, a13, c4pga, c5, mag1, n)
      else
         mu = rockPGA
      endif

c     Compute the Sa
      if (specT .ne. 0.0) then
         call S02_Calcarg_as97 (mag,dist, mech, soil, hw, specT, mu, rockpga,
     1        a1sa, a2, a3sa, a4, a5sa, a6sa, a9sa, a10sa, a11sa, a12sa,
     2        a13, c4sa, c5, mag1, n)
       endif

C Now apply the 20% reduction in ground motion due to the normal faulting regime.
c    This applies at all freq
       if ( mech .lt. 0. ) then
         lnY = lnY + 0.2231 * mech
       endif

       lnY = mu + 6.89

C     Set Standard Error.
      if (specT .eq. 0.0) then
         if (mag .le. 5.0) then
            sigma = b1pga
         elseif (mag .gt. 5.0 .and. mag .lt. 7.0) then
            sigma = b1pga - b2pga*(mag-5.0)
         elseif (mag .ge. 7.0) then
            sigma = b1pga - 2.0*b2pga
         endif
      else
         if (mag .le. 5.0) then
            sigma = b1sa
         elseif (mag .gt. 5.0 .and. mag .lt. 7.0) then
            sigma = b1sa - b2sa*(mag-5.0)
         elseif (mag .ge. 7.0) then
            sigma = b1sa - 2.0*b2sa
         endif
      endif

      return
      end

c -------------------------------------------------------------------
C****** Abrahamson and Silva (1997), Vertical ***********************
c -------------------------------------------------------------------

      subroutine S02_AS_97_V ( mag, dist, mech, soil, hw, specT, period1,
     1                      lnY, sigma,iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=27)

      integer hw,iflag, count1, count2, nper, i
      real mag, dist, mech, n, c4(MAXPER), rockPGA
      real mag1, c5, period(MAXPER), mu, b5(MAXPER), b6(MAXPER)
      real lnY, sigma, soil, specT
      real a1(MAXPER), a2, a3(MAXPER), a4, a5(MAXPER), a6(MAXPER),
     1     a9(MAXPER), a10(MAXPER), a11(MAXPER), a12(MAXPER), a13

      real a1pga, a3pga, a5pga, a6pga, a9pga, a10pga, a11pga, a12pga
      real c4pga, b5pga, b6pga
      real a1sa, a3sa, a5sa, a6sa, a9sa, a10sa, a11sa, a12sa
      real c4sa, b5sa, b6sa, period1, soil1, rockPga1

      data b5 / 0.760, 0.760, 0.760, 0.760, 0.760, 0.760,
     1         0.760, 0.760, 0.740, 0.720, 0.700, 0.690, 0.690,
     2         0.690, 0.690, 0.690, 0.690, 0.690, 0.690, 0.690,
     3         0.690, 0.690, 0.690, 0.690, 0.720, 0.750, 0.780 /
      data b6 /  0.085, 0.085, 0.085, 0.085, 0.085, 0.085,
     1          0.085, 0.085, 0.075, 0.063, 0.056, 0.050, 0.050,
     2          0.050, 0.050, 0.050, 0.050, 0.050, 0.050, 0.050,
     3          0.050, 0.050, 0.050, 0.050, 0.050, 0.050, 0.050 /
      data period /0.00, 0.03, 0.04, 0.05, 0.06, 0.075, 0.09, 0.10,
     1     0.12, 0.15, 0.17, 0.20, 0.24, 0.30, 0.36, 0.40,  0.46, 0.50,
     2           0.60 ,0.75, 0.85, 1.00, 1.50, 2.00, 3.00, 4.00,  5.00/
      data c4 / 6.00, 6.00, 6.00, 6.00, 6.00, 6.00, 6.00, 6.00, 6.00,
     1          6.00, 5.72, 5.35, 4.93, 4.42, 4.01, 3.77, 3.45, 3.26,
     2          2.85, 2.50, 2.50, 2.50, 2.50, 2.50, 2.50, 2.50, 2.50 /
      data a1 / 1.642, 2.100, 2.420, 2.620, 2.710, 2.750, 2.730, 2.700,
     1          2.480, 2.170, 1.960, 1.648, 1.312, 0.878, 0.617, 0.478,
     2    0.271, 0.145, -0.087, -0.344, -0.469, -0.602, -0.966, -1.224,
     3         -1.581, -1.857, -2.053 /
      data a3 /-1.2520,-1.3168,-1.3700,-1.3700,-1.3700,-1.3700, -1.3700,
     1    -1.3700, -1.2986, -1.2113, -1.1623, -1.0987, -1.0274, -0.9400,
     2    -0.9004, -0.8776, -0.8472, -0.8291, -0.7896, -0.7488, -0.7451,
     3    -0.7404, -0.7285, -0.7200, -0.7200, -0.7200, -0.7200 /
      data a5 / 0.390, 0.432, 0.469, 0.496, 0.518, 0.545, 0.567, 0.580,
     1          0.580, 0.580, 0.580, 0.580, 0.580, 0.580, 0.571, 0.539,
     2          0.497, 0.471, 0.416, 0.348, 0.309, 0.260, 0.260, 0.260,
     3          0.260, 0.260, 0.260 /
      data a6 / -0.050, -0.050, -0.050, -0.050, -0.050, -0.050, -0.050,
     1  -0.050,-0.017, 0.024, 0.047, 0.076, 0.109, 0.150, 0.150, 0.150,
     2          0.150, 0.150, 0.150, 0.150, 0.150, 0.150, 0.058,-0.008,
     3         -0.100, -0.100, -0.100 /
      data a9 /0.630,0.630,0.630,0.630,0.630,0.630,0.630,0.630,0.630,
     1   0.630, 0.604, 0.571, 0.533, 0.488, 0.450, 0.428, 0.400, 0.383,
     2   0.345, 0.299, 0.273, 0.240, 0.240, 0.240, 0.240, 0.240 ,0.240/
      data a10 / -0.140, -0.140, -0.140, -0.140, -0.140, -0.129, -0.119,
     1           -0.114, -0.104, -0.093, -0.087, -0.078, -0.069, -0.057,
     2           -0.048, -0.043, -0.035, -0.031, -0.022, -0.010, -0.004,
     3            0.004,  0.025,  0.040,  0.040,  0.040,  0.040 /
      data a11 / -0.220, -0.220, -0.220, -0.220, -0.220, -0.220, -0.220,
     1           -0.220, -0.220, -0.220, -0.220, -0.220, -0.220, -0.220,
     2           -0.220, -0.220, -0.220, -0.220, -0.220, -0.220, -0.220,
     3           -0.220, -0.220, -0.220, -0.220, -0.220, -0.220 /
      data a12 /0.0000,0.0000,0.0000,-0.0002,-0.0004,-0.0007,-0.0009,
     1   -0.0010, -0.0015, -0.0022, -0.0025, -0.0030, -0.0035, -0.0042,
     2            -0.0047, -0.0050, -0.0056, -0.0060, -0.0068, -0.0083,
     3   -0.0097, -0.0115, -0.0180, -0.0240, -0.0431, -0.0565, -0.0670/
      data mag1,n,a2,a4,a13,c5 / 6.4, 3.0, 0.909, 0.275, 0.06, 0.3/

C Find the requested spectral period.
      nper = 27

C First Check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1=period(1)
         a1pga  = a1(1)
         a3pga  = a3(1)
         a5pga  = a5(1)
         a6pga  = a6(1)
         a9pga  = a9(1)
         a10pga = a10(1)
         a11pga = a11(1)
         a12pga = a12(1)
         c4pga  = c4(1)
         b5pga  = b5(1)
         b6pga  = b6(1)
         goto 1005
      elseif (specT .ne. 0.0) then
C Now interpolate the cofficients for different spectral periods.
C Interpolation is only done for the second spectral period and
C the last spectral period.

C Set PGA coefficients for non-pga spectral periods.
         a1pga  = a1(1)
         a3pga  = a3(1)
         a5pga  = a5(1)
         a6pga  = a6(1)
         a9pga  = a9(1)
         a10pga = a10(1)
         a11pga = a11(1)
         a12pga = a12(1)
         c4pga  = c4(1)

         do i=2, nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1006
            endif
         enddo

C Requested spectral period is not in the period range of the attenuation model.
         write (*,*)
         write (*,*) 'Abrahamson and Silva (1997) Vertical atttenuation model'
         write (*,*) 'is not defined for a spectral period of: '
         write (*,'(a10,f10.5)') ' Period = ',specT
         write (*,*) 'This spectral period is outside the defined'
         write (*,*) 'period range in the code or beyond the range'
         write (*,*) 'of spectral periods for interpolation.'
         write (*,*) 'Please check the input file.'
         write (*,*)
         stop 99

C Interpolate the coefficients for the requested spectral period.
 1006       call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +                   specT,a1sa,iflag)
            call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +                   specT,a3sa,iflag)
            call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +                   specT,a5sa,iflag)
            call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +                   specT,a6sa,iflag)
            call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +                   specT,a9sa,iflag)
            call S24_interp (period(count1),period(count2),a10(count1),a10(count2),
     +                   specT,a10sa,iflag)
            call S24_interp (period(count1),period(count2),a11(count1),a11(count2),
     +                   specT,a11sa,iflag)
            call S24_interp (period(count1),period(count2),a12(count1),a12(count2),
     +                   specT,a12sa,iflag)
            call S24_interp (period(count1),period(count2),c4(count1),c4(count2),
     +                   specT,c4sa,iflag)
            call S24_interp (period(count1),period(count2),b5(count1),b5(count2),
     +                   specT,b5sa,iflag)
            call S24_interp (period(count1),period(count2),b6(count1),b6(count2),
     +                   specT,b6sa,iflag)
      endif

 1005 period1 = specT

c     Compute the rock PGA
      soil1 = 0.0
      rockPga1 = 0.
      call S02_Calcarg_as97 (mag,dist,mech,soil1, hw, 0., rockPGA, rockpga1,
     1  a1pga, a2, a3pga, a4, a5pga, a6pga, a9pga, a10pga, a11pga,
     2  a12pga, a13, c4pga, c5, mag1, n)

c     Compute the soil PGA if requested
      if (soil .eq. 1) then
         call S02_Calcarg_as97 (mag,dist,mech,soil, hw, 0., mu, rockpga,
     1        a1pga, a2, a3pga, a4, a5pga, a6pga, a9pga, a10pga, a11pga,
     2        a12pga, a13, c4pga, c5, mag1, n)
      else
         mu = rockPGA
      endif

c     Compute the Sa
      if (specT .ne. 0.0) then
         call S02_Calcarg_as97 (mag,dist, mech, soil, hw, specT, mu, rockpga,
     1        a1sa, a2, a3sa, a4, a5sa, a6sa, a9sa, a10sa, a11sa, a12sa,
     2        a13, c4sa, c5, mag1, n)
       endif

       lnY = mu + 6.89

C     Set Standard Error.
      if (specT .eq. 0.0) then
         if (mag .le. 5.0) then
            sigma = b5pga
         elseif (mag .gt. 5.0 .and. mag .lt. 7.0) then
            sigma = b5pga - b6pga*(mag-5.0)
         elseif (mag .ge. 7.0) then
            sigma = b5pga - 2.0*b6pga
         endif
      else
         if (mag .le. 5.0) then
            sigma = b5sa
         elseif (mag .gt. 5.0 .and. mag .lt. 7.0) then
            sigma = b5sa - b6sa*(mag-5.0)
         elseif (mag .ge. 7.0) then
            sigma = b5sa - 2.0*b6sa
         endif
      endif

      return
      end

c --------------------------------------------------------------------
C****** Abrahamson and Silva (1997), Vertical with Normal Faulting ***
c --------------------------------------------------------------------

      subroutine S02_AS_97_V_NF ( mag, dist, mech, soil, hw, specT, period1,
     1                      lnY, sigma,iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=27)
      integer hw, nper, count1, count2, iflag, i
      real mag, dist, mech, mech1, n, c4(MAXPER), rockPGA
      real mag1, c5, period(MAXPER), mu, b5(MAXPER), b6(MAXPER)
      real lnY, sigma, soil, specT, period1, soil1, rockPga1
      real a1(MAXPER), a2, a3(MAXPER), a4, a5(MAXPER), a6(MAXPER),
     1     a9(MAXPER), a10(MAXPER), a11(MAXPER), a12(MAXPER), a13
      real a1pga, a3pga, a5pga, a6pga, a9pga, a10pga, a11pga, a12pga
      real c4pga, b5pga, b6pga
      real a1sa, a3sa, a5sa, a6sa, a9sa, a10sa, a11sa, a12sa
      real c4sa, b5sa, b6sa

      data b5 / 0.760, 0.760, 0.760, 0.760, 0.760, 0.760,
     1         0.760, 0.760, 0.740, 0.720, 0.700, 0.690, 0.690,
     2         0.690, 0.690, 0.690, 0.690, 0.690, 0.690, 0.690,
     3         0.690, 0.690, 0.690, 0.690, 0.720, 0.750, 0.780 /
      data b6 /  0.085, 0.085, 0.085, 0.085, 0.085, 0.085,
     1          0.085, 0.085, 0.075, 0.063, 0.056, 0.050, 0.050,
     2          0.050, 0.050, 0.050, 0.050, 0.050, 0.050, 0.050,
     3          0.050, 0.050, 0.050, 0.050, 0.050, 0.050, 0.050 /
      data period /0.00, 0.03, 0.04, 0.05, 0.06, 0.075, 0.09, 0.10,
     1     0.12, 0.15, 0.17, 0.20, 0.24, 0.30, 0.36, 0.40,  0.46, 0.50,
     2           0.60 ,0.75, 0.85, 1.00, 1.50, 2.00, 3.00, 4.00,  5.00/
      data c4 / 6.00, 6.00, 6.00, 6.00, 6.00, 6.00, 6.00, 6.00, 6.00,
     1          6.00, 5.72, 5.35, 4.93, 4.42, 4.01, 3.77, 3.45, 3.26,
     2          2.85, 2.50, 2.50, 2.50, 2.50, 2.50, 2.50, 2.50, 2.50 /
      data a1 / 1.642, 2.100, 2.420, 2.620, 2.710, 2.750, 2.730, 2.700,
     1          2.480, 2.170, 1.960, 1.648, 1.312, 0.878, 0.617, 0.478,
     2    0.271, 0.145, -0.087, -0.344, -0.469, -0.602, -0.966, -1.224,
     3         -1.581, -1.857, -2.053 /
      data a3 /-1.2520,-1.3168,-1.3700,-1.3700,-1.3700,-1.3700, -1.3700,
     1    -1.3700, -1.2986, -1.2113, -1.1623, -1.0987, -1.0274, -0.9400,
     2    -0.9004, -0.8776, -0.8472, -0.8291, -0.7896, -0.7488, -0.7451,
     3    -0.7404, -0.7285, -0.7200, -0.7200, -0.7200, -0.7200 /
      data a5 / 0.390, 0.432, 0.469, 0.496, 0.518, 0.545, 0.567, 0.580,
     1          0.580, 0.580, 0.580, 0.580, 0.580, 0.580, 0.571, 0.539,
     2          0.497, 0.471, 0.416, 0.348, 0.309, 0.260, 0.260, 0.260,
     3          0.260, 0.260, 0.260 /
      data a6 / -0.050, -0.050, -0.050, -0.050, -0.050, -0.050, -0.050,
     1  -0.050,-0.017, 0.024, 0.047, 0.076, 0.109, 0.150, 0.150, 0.150,
     2          0.150, 0.150, 0.150, 0.150, 0.150, 0.150, 0.058,-0.008,
     3         -0.100, -0.100, -0.100 /
      data a9 /0.630,0.630,0.630,0.630,0.630,0.630,0.630,0.630,0.630,
     1   0.630, 0.604, 0.571, 0.533, 0.488, 0.450, 0.428, 0.400, 0.383,
     2   0.345, 0.299, 0.273, 0.240, 0.240, 0.240, 0.240, 0.240 ,0.240/
      data a10 / -0.140, -0.140, -0.140, -0.140, -0.140, -0.129, -0.119,
     1           -0.114, -0.104, -0.093, -0.087, -0.078, -0.069, -0.057,
     2           -0.048, -0.043, -0.035, -0.031, -0.022, -0.010, -0.004,
     3            0.004,  0.025,  0.040,  0.040,  0.040,  0.040 /
      data a11 / -0.220, -0.220, -0.220, -0.220, -0.220, -0.220, -0.220,
     1           -0.220, -0.220, -0.220, -0.220, -0.220, -0.220, -0.220,
     2           -0.220, -0.220, -0.220, -0.220, -0.220, -0.220, -0.220,
     3           -0.220, -0.220, -0.220, -0.220, -0.220, -0.220 /
      data a12 /0.0000,0.0000,0.0000,-0.0002,-0.0004,-0.0007,-0.0009,
     1   -0.0010, -0.0015, -0.0022, -0.0025, -0.0030, -0.0035, -0.0042,
     2            -0.0047, -0.0050, -0.0056, -0.0060, -0.0068, -0.0083,
     3   -0.0097, -0.0115, -0.0180, -0.0240, -0.0431, -0.0565, -0.0670/
      data mag1,n,a2,a4,a13,c5 / 6.4, 3.0, 0.909, 0.275, 0.06, 0.3/

C Find the requested spectral period.
      nper = 27

C First Check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1=period(1)
         a1pga  = a1(1)
         a3pga  = a3(1)
         a5pga  = a5(1)
         a6pga  = a6(1)
         a9pga  = a9(1)
         a10pga = a10(1)
         a11pga = a11(1)
         a12pga = a12(1)
         c4pga  = c4(1)
         b5pga  = b5(1)
         b6pga  = b6(1)
         goto 1004
      elseif (specT .ne. 0.0) then
C Now interpolate the cofficients for different spectral periods.
C Interpolation is only done for the second spectral period and
C the last spectral period.

C Set PGA coefficients for non-pga spectral periods.
         a1pga  = a1(1)
         a3pga  = a3(1)
         a5pga  = a5(1)
         a6pga  = a6(1)
         a9pga  = a9(1)
         a10pga = a10(1)
         a11pga = a11(1)
         a12pga = a12(1)
         c4pga  = c4(1)

         do i=2, nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1003
            endif
         enddo

C Requested spectral period is not in the period range of the attenuation model.
         write (*,*)
         write (*,*) 'Abrahamson and Silva (1997) Vertical atttenuation model'
         write (*,*) 'for normal faulting regimes'
         write (*,*) 'is not defined for a spectral period of: '
         write (*,'(a10,f10.5)') ' Period = ',specT
         write (*,*) 'This spectral period is outside the defined'
         write (*,*) 'period range in the code or beyond the range'
         write (*,*) 'of spectral periods for interpolation.'
         write (*,*) 'Please check the input file.'
         write (*,*)
         stop 99

C Interpolate the coefficients for the requested spectral period.
 1003       call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +                   specT,a1sa,iflag)
            call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +                   specT,a3sa,iflag)
            call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +                   specT,a5sa,iflag)
            call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +                   specT,a6sa,iflag)
            call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +                   specT,a9sa,iflag)
            call S24_interp (period(count1),period(count2),a10(count1),a10(count2),
     +                   specT,a10sa,iflag)
            call S24_interp (period(count1),period(count2),a11(count1),a11(count2),
     +                   specT,a11sa,iflag)
            call S24_interp (period(count1),period(count2),a12(count1),a12(count2),
     +                   specT,a12sa,iflag)
            call S24_interp (period(count1),period(count2),c4(count1),c4(count2),
     +                   specT,c4sa,iflag)
            call S24_interp (period(count1),period(count2),b5(count1),b5(count2),
     +                   specT,b5sa,iflag)
            call S24_interp (period(count1),period(count2),b6(count1),b6(count2),
     +                   specT,b6sa,iflag)
      endif

 1004 period1 = specT

c     Set normal faulting to Stike-slip
      mech1 = mech
      if ( mech .lt. 0. ) mech1 = 0.

c     Compute the rock PGA
      soil1 = 0.0
      rockPga1 = 0.
      call S02_Calcarg_as97 (mag,dist,mech1,soil1, hw, 0., rockPGA, rockpga1,
     1  a1pga, a2, a3pga, a4, a5pga, a6pga, a9pga, a10pga, a11pga,
     2  a12pga, a13, c4pga, c5, mag1, n)

c     Compute the soil PGA if requested
      if (soil .eq. 1) then
         call S02_Calcarg_as97 (mag,dist,mech1,soil, hw, 0., mu, rockpga,
     1        a1pga, a2, a3pga, a4, a5pga, a6pga, a9pga, a10pga, a11pga,
     2        a12pga, a13, c4pga, c5, mag1, n)
      else
         mu = rockPGA
      endif

c     Compute the Sa
      if (specT .ne. 0.0) then
         call S02_Calcarg_as97 (mag,dist, mech1, soil, hw, specT, mu, rockpga,
     1        a1sa, a2, a3sa, a4, a5sa, a6sa, a9sa, a10sa, a11sa, a12sa,
     2        a13, c4sa, c5, mag1, n)
       endif

C Now apply the 20% reduction in ground motion due to the normal faulting regime.
       if ( mech .lt. 0. ) then
         lnY = lnY + 0.2231 * mech
       endif

       lnY = mu + 6.89

C     Set Standard Error.
      if (specT .eq. 0.0) then
         if (mag .le. 5.0) then
            sigma = b5pga
         elseif (mag .gt. 5.0 .and. mag .lt. 7.0) then
            sigma = b5pga - b6pga*(mag-5.0)
         elseif (mag .ge. 7.0) then
            sigma = b5pga - 2.0*b6pga
         endif
      else
         if (mag .le. 5.0) then
            sigma = b5sa
         elseif (mag .gt. 5.0 .and. mag .lt. 7.0) then
            sigma = b5sa - b6sa*(mag-5.0)
         elseif (mag .ge. 7.0) then
            sigma = b5sa - 2.0*b6sa
         endif
      endif

      return
      end

c ---------------------------------------------------------------------

      subroutine S02_Calcarg_as97 (mag,dist,mech,soil,hw,specT,mu,rockPGA,
     1 a1, a2, a3, a4, a5, a6, a9, a10, a11, a12, a13, c4, c5, mag1, n)

      implicit none

      integer hw
      real a1, a3, a5, a6, a9, a10, a11, a12, specT,
     1     mag, dist, mech, mu, n, c4, rockPGA, a2, a4, a13
      real x1, x2, x3, x4, t1, mag1, c5
      real soilFactor, r, soil

c     Rock model
      r = sqrt(dist**2+c4**2)

c Set the maximum magnitude value at M=8.5. Magnitude larger than this value
c would result in lower ground motions for larger magnitude events.
      if (mag .gt. 8.5) then
         mag = 8.5
      endif

      mu = a1 + a12*(8.5-mag)**(n) + a3 * alog(r)
     1     + a13*(mag - mag1)*alog(r)
      if ( mag .lt. mag1 ) then
         mu = mu + a2*(mag-mag1)
      else
         mu = mu + a4*(mag-mag1)
      endif

c     mech model
      x1 = 5.8
      x2 = mag1
      if (mag .lt. x1 ) then
        mu = mu + a5*mech
      elseif ( mag .lt. x2 ) then
        mu = mu + a5*mech* (1. - (mag-x1)/(x2-x1) ) +
     1         a6*mech*(mag-x1)/(x2-x1)
      else
        mu = mu + a6*mech
      endif

c     HW model
      t1 = 0.
      if ( mag .gt. 5.5 .and. hw .eq. 1 ) then
        x1 = 4.
        x2 = 8.
        x3 = 18.
        x4 = 25.
        if ( dist .lt. x1 ) then
        t1 = 0.
        elseif ( dist .lt. x2 ) then
          t1 =  (dist-x1)/(x2-x1)
        elseif ( dist .lt. x3 ) then
          t1 = 1.
        elseif ( dist .lt. x4 ) then
        t1 = 1. - (dist-x3)/(x4-x3)
        else
        t1 = 0.
        endif
        if ( mag .lt. 6.5 ) then
        t1 = t1 * (mag-5.5)
        endif
      endif
      mu = mu + t1*a9

c     Soil Model
      if ( soil .eq. 1. ) then
         soilFactor = a10 + a11*alog((exp(rockPGA) + c5))
         mu = mu + soilFactor
      endif

      return
      end

c -------------------------------------------------------------------
C **** Boore, Joyner, and Fumal (1994) Horizontal *******************
c -------------------------------------------------------------------

      subroutine S02_bjf94 ( m, dist, ftype, lnY, sigma, GB, GC, specT,
     1                   attenName, period1,iflag )
c     This  subroutine calculates the LOG SA for the ave horizontal
c     component using the Boore, Joyner and Fumal (1994) attenuation
c     relation
c     it takes into account the ftype of the fault unlike BJF 1993.

      implicit none

      integer MAXPER
      parameter (MAXPER=11)
      real ftype, ftype1, dist, m, lnY, sigma, log10Y, specT
      real period(MAXPER), b2(MAXPER), b3(MAXPER),
     1     b5(MAXPER), b1ss(MAXPER), b1rv(MAXPER),
     1     b6(MAXPER), b7(MAXPER), h(MAXPER), sigma1(MAXPER)
      character*80 attenName
      integer nper, count1, count2, iflag, i
      real b1ssT, b1rvT, b2T, b3T, b5T, b6T, b7T, hT, sigma1T,
     1     GB, GC, period1, twoPi, r

      data period / 0.00, 0.1, 0.15, 0.20,
     1              0.30, 0.40, 0.50, 0.75, 1.00, 1.5, 2. /

      data b1ss / -0.136, 1.630, 1.859, 1.928, 1.930, 1.887, 1.839,
     1          1.748, 1.701, 1.695, 1.756 /
      data b1rv /  -.051, 1.665, 1.918, 2.002, 2.019, 1.979, 1.930,
     1          1.824, 1.755, 1.701, 1.712 /
      data b2 / 0.229, 0.327, 0.305, 0.309, 0.334, 0.361, 0.384,
     1          0.425, 0.450, 0.471, 0.471 /
      data b3 / 0.0,  -0.098, -0.099, -0.090, -0.070, -0.052, -0.039,
     1         -0.020, -0.014, -0.019, -0.037/
      data b5 / -0.778, -0.934, -0.937, -0.924, -0.893, -0.867, -0.846,
     1          -0.813, -0.798, -0.796, -0.812/
      data b6 / 0.162, 0.046, 0.140, 0.190, 0.239, 0.264, 0.279, 0.300,
     1          0.314, 0.338, 0.360 /
      data b7 / 0.251, 0.136, 0.221, 0.279, 0.356, 0.405, 0.439, 0.490,
     1          0.517, 0.537, 0.537 /
      data h  / 5.57, 6.27, 7.23, 7.02, 5.94, 4.91, 4.13, 3.07, 2.90,
     1          3.92, 5.85 /
      data sigma1 / 0.230, 0.208, 0.211, 0.215, 0.226, 0.236, 0.244,
     1          0.256, 0.270, 0.285, 0.293 /
      twoPi = 2. * 3.1415926

c     Set normal faulting to Stike-slip
      ftype1 = ftype
      if ( ftype .lt. 0. ) ftype1 = 0.

c Set attenuation name
      if ( GC .eq. 1 ) then
        attenName = 'Boore, Joyner, Fumal (1994), Class C'
      elseif ( GB .eq. 1 ) then
        attenName = 'Boore, Joyner, Fumal (1994), Class B'
      else
        attenName = 'Boore, Joyner, Fumal (1994), Class A'
      endif

C Find the requested spectral period and corresponding coefficients
      nper = 11

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1 = period(1)
         b1ssT   = b1ss(1)
         b1rvT   = b1rv(1)
         b2T     = b2(1)
         b3T     = b3(1)
         b5T     = b5(1)
         b6T     = b6(1)
         b7T     = b7(1)
         sigma1T = sigma1(1)
         hT      = h(1)
         goto 1011
      elseif (specT .ne. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1010
            endif
         enddo
      endif

      write (*,*)
      write (*,*) 'Boore et al. (1994) Horizontal atttenuation model'
      write (*,*) 'is not defined for a spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1010       call S24_interp (period(count1),period(count2),b1ss(count1),b1ss(count2),
     +                   specT,b1ssT,iflag)
            call S24_interp (period(count1),period(count2),b1rv(count1),b1rv(count2),
     +                   specT,b1rvT,iflag)
            call S24_interp (period(count1),period(count2),b2(count1),b2(count2),
     +                   specT,b2T,iflag)
            call S24_interp (period(count1),period(count2),b3(count1),b3(count2),
     +                   specT,b3T,iflag)
            call S24_interp (period(count1),period(count2),b5(count1),b5(count2),
     +                   specT,b5T,iflag)
            call S24_interp (period(count1),period(count2),b6(count1),b6(count2),
     +                   specT,b6T,iflag)
            call S24_interp (period(count1),period(count2),b7(count1),b7(count2),
     +                   specT,b7T,iflag)
            call S24_interp (period(count1),period(count2),h(count1),h(count2),
     +                   specT,hT,iflag)
            call S24_interp (period(count1),period(count2),sigma1(count1),sigma1(count2),
     +                   specT,sigma1T,iflag)

 1011 period1 = specT

      r = sqrt( dist**2 + hT**2 )
      log10Y = b1ssT + (b1rvT-b1ssT)*ftype1
     1         + b2T*(m-6) + b3T*(m-6)**2
     1         + b5T*alog10(r)
     1         + b6T*GB + b7T*GC

c     Convert units spectral acceleration in gal
      if ( specT .eq. 0.0 ) then
        lnY = log10Y * alog(10.) + 6.89
      else
        lnY = log10Y * alog(10.) + alog( twoPi / specT )
      endif

c     Set standard error
      sigma = sigma1T * alog(10.)

      return
      end

c -------------------------------------------------------------------
C **** Boore, Joyner, and Fumal (1997) Horizontal *******************
c -------------------------------------------------------------------

      subroutine S02_bjf97 ( m, dist, ftype, lnY, sigma, specT,
     1                   attenName, period1, vs,iflag )
c     This  subroutine calculates the LOG SA for the ave horizontal
c     component using the Boore, Joyner and Fumal (1994) attenuation
c     relation
c     it takes into account the ftype of the fault unlike BJF 1993.

      implicit none

      integer MAXPER
      parameter (MAXPER=12)
      real ftype, dist, m, lnY, sigma, period1, vs, r, ftype1
      real period(MAXPER), b2(MAXPER), b3(MAXPER),
     1     b5(MAXPER), b1ss(MAXPER), b1rv(MAXPER),
     1     bv(MAXPER), va(MAXPER), h(MAXPER), sigma1(MAXPER)
      character*80 attenName
      real specT, b1ssT, b1rvT, b2T, b3T, b5T, bvT, vaT, hT, sigma1T
      integer count1, count2, nper, iflag, i

      data period / 0.00, 0.1, 0.15, 0.20, 0.24,
     1              0.30, 0.40, 0.50, 0.75, 1.00, 1.5, 2. /

      data b1ss /-0.313, 1.006, 1.128, 0.999, 0.847, 0.598, 0.212,
     1          -0.122, -0.737, -1.133, -1.552, -1.699 /
      data b1rv /  -0.117, 1.087, 1.264, 1.170, 1.033, 0.803, 0.423,
     1           0.087, -0.562, -1.009, -1.538, -1.801 /
      data b2 / 0.527, 0.753, 0.702, 0.711, 0.732, 0.769, 0.831,
     1         0.884, 0.979, 1.036, 1.085, 1.085 /
      data b3 / 0.0, -0.226, -0.228, -0.207, -0.189, -0.161, -0.120,
     1         -0.090, -0.046, -0.032, -0.044, -0.085 /
      data b5 / -0.778, -0.934, -0.937, -0.924, -0.912, -0.893, -0.867,
     1          -0.846, -0.813, -0.798, -0.796, -0.812/
      data bv / -0.371, -0.212, -0.238, -0.292, -0.338, -0.401, -0.487,
     1           -0.553, -0.653, -0.698, -0.704, -0.655/
      data va / 1396., 1112., 1820., 2118., 2178., 2133., 1954., 1782.,
     1          1507., 1406., 1479., 1795. /
      data h  / 5.57, 6.27, 7.23, 7.02, 6.62, 5.94, 4.91, 4.13, 3.07,
     1          2.90, 3.92, 5.85 /
      data sigma1 / 0.52, 0.479, 0.492, 0.502, 0.511, 0.522, 0.538,
     1          0.556, 0.587, 0.613, 0.649, 0.672 /

C Find the requested spectral period and corresponding coefficients
      nper = 12

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1 = period(1)
         b1ssT   = b1ss(1)
         b1rvT   = b1rv(1)
         b2T     = b2(1)
         b3T     = b3(1)
         b5T     = b5(1)
         bvT     = bv(1)
         vaT     = va(1)
         sigma1T = sigma1(1)
         hT      = h(1)
         goto 1011
      elseif (specT .ne. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

      write (*,*)
      write (*,*) 'Boore et al. (1997) Horizontal atttenuation model'
      write (*,*) 'is not defined for a spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020       call S24_interp (period(count1),period(count2),b1ss(count1),b1ss(count2),
     +                   specT,b1ssT,iflag)
            call S24_interp (period(count1),period(count2),b1rv(count1),b1rv(count2),
     +                   specT,b1rvT,iflag)
            call S24_interp (period(count1),period(count2),b2(count1),b2(count2),
     +                   specT,b2T,iflag)
            call S24_interp (period(count1),period(count2),b3(count1),b3(count2),
     +                   specT,b3T,iflag)
            call S24_interp (period(count1),period(count2),b5(count1),b5(count2),
     +                   specT,b5T,iflag)
            call S24_interp (period(count1),period(count2),bv(count1),bv(count2),
     +                   specT,bvT,iflag)
            call S24_interp (period(count1),period(count2),va(count1),va(count2),
     +                   specT,vaT,iflag)
            call S24_interp (period(count1),period(count2),h(count1),h(count2),
     +                   specT,hT,iflag)
            call S24_interp (period(count1),period(count2),sigma1(count1),sigma1(count2),
     +                   specT,sigma1T,iflag)

 1011 period1 = specT

c     Set normal faulting to Stike-slip
      ftype1 = ftype
      if ( ftype .lt. 0. ) ftype1 = 0.

c     Set atten name
      attenName = 'Boore, Joyner, Fumal (1997), inc sigmaComp'

      r = sqrt( dist**2 + hT**2 )

      lnY = b1ssT + (b1rvT-b1ssT)*ftype1
     1         + b2T*(m-6) + b3T*(m-6)**2
     1         + b5T*alog(r)
     1         + bvT*alog(vs/vaT)

c     Convert units spectral acceleration in gal

      lnY = lnY + 6.89

c     Set standard error
      sigma = sigma1T

      return
      end

c -------------------------------------------------------------
C **** Campbell (1990) Horizontal Rock*************************
c -------------------------------------------------------------

      subroutine S02_Camp90 ( m, dist, ftype, lnY, sigma, baseDepth, specT,
     1                    attenName, period1,iflag )

c     This  subroutine calculates the LOG SA for the ave horizontal
c     component using the Campbell (1990) attenuation relation.
c     Sigma1 and Sigma2 are directly from Campbells paper
c      for M < 6.2 and M>= 6.2, respectively.

      implicit none

      integer MAXPER
      parameter (MAXPER=16)
      real ftype, ftype1, dist, m, lnY, sigma, baseDepth, period1
      character*80 attenName
      real period(MAXPER), specT, twoPi
      real a(MAXPER), f1(MAXPER), g1(MAXPER), sigma1(MAXPER),
     1     sigma2(MAXPER), b, c1, c2, d, e, f2, f3, g2
      real aT, f1T, g1T, sigma1T, sigma2T
      integer nper, count1, count2, iflag, i

      data period / 0.00, 0.04, 0.05, 0.075, 0.10, 0.15, 0.20,
     1              0.30, 0.40, 0.50, 0.75, 1.00, 1.5, 2., 3., 4. /
      data a  / -2.245, -0.402, -0.141, 0.489, 0.987, 1.625, 1.988,
     1           2.370,  2.153,  2.086, 1.802, 1.398, 0.795, 0.411,
     2          -0.141, -0.188 /
      data f1 / 0., 0., 0., 0., 0.,  0., 0., 0., 0.514, 0.738, 1.23,
     1          1.59, 1.93, 2.23, 2.39, 2.03 /
      data g1 /  0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
     1          0.183, 0.488, 0.634, 0.836, 1.17 /
      data sigma1 / 0.517, 0.716, 0.631, 0.703, 0.703, 0.754, 0.722,
     1              0.597, 0.671, 0.722, 0.776, 0.751, 0.687, 0.591,
     2              0.628, 0.647 /
      data sigma2 / 0.387, 0.387, 0.492, 0.430, 0.427, 0.440, 0.421,
     1              0.382, 0.342, 0.330, 0.420, 0.426, 0.478, 0.496,
     2              0.520, 0.532 /

      twoPi = 2. * 3.1415926
      attenName = 'Campbell (1990), horizontal'

C Find the requested spectral period and corresponding coefficients
      nPer = 16

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1 = period(1)
         aT      = a(1)
         f1T     = f1(1)
         g1T     = g1(1)
         sigma1T  = sigma1(1)
         sigma2T  = sigma2(1)
         goto 1011
      elseif (specT .ne. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

      write (*,*)
      write (*,*) 'Campbell (1990) Horizontal atttenuation model'
      write (*,*) 'is not defined for a spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020       call S24_interp (period(count1),period(count2),a(count1),a(count2),
     +                   specT,aT,iflag)
            call S24_interp (period(count1),period(count2),f1(count1),f1(count2),
     +                   specT,f1T,iflag)
            call S24_interp (period(count1),period(count2),g1(count1),g1(count2),
     +                   specT,g1T,iflag)
            call S24_interp (period(count1),period(count2),sigma1(count1),sigma1(count2),
     +                   specT,sigma1T,iflag)
            call S24_interp (period(count1),period(count2),sigma2(count1),sigma2(count2),
     +                   specT,sigma2T,iflag)

 1011 period1 = specT

      b = 1.09
      c1 = 0.361
      c2 = 0.576
      d = -1.89
      e = 0.213
      f2 = 0.659
      f3 = -4.7
      g2 = 0.574


c     Convert oblique to reverse for Campbell
      ftype1 = ftype
      if ( ftype1 .eq. 0.5 ) then
        ftype1=1.0
      endif
c     Set normal faulting to Stike-slip
      if ( ftype .lt. 0. ) ftype1 = 0.

      lnY = aT + b*m + d*alog(dist+c1*exp(c2*m)) + e*ftype1
     1      + f1T * tanh( f2*(m+f3) )
     2      + g1T*tanh(g2*baseDepth)

c     Convert units spectral acceleration in gal
      if ( specT .eq. 0.0 ) then
        lnY = lnY + 6.89
      else
        lnY = lnY + alog( twoPi/specT )
      endif

c     Set standard error
      if ( m .ge. 6.2 ) then
        sigma = sigma2T
      else
        sigma = sigma1T
      endif

      return
      end

c ----------------------------------------------------------
C **** Campbell (1990) Vertical Rock*************************
c ----------------------------------------------------------

      subroutine S02_Camp90v ( m, dist, ftype, lnY, sigma, baseDepth, specT,
     1                    attenName, period1,iflag )

c     This  subroutine calculates the LOG SA for the ave horizontal
c     component using the Campbell (1990) attenuation relation.
c     Sigma1 and Sigma2 are directly from Campbells paper
c      for M < 6.2 and M>= 6.2, respectively.

      implicit none

      integer MAXPER
      parameter (MAXPER=16)
      real ftype, ftype1, dist, m, lnY, sigma, baseDepth, period1
      integer iflag
      character*80 attenName
      real period(MAXPER), twoPi
      real a(MAXPER), f1(MAXPER), g1(MAXPER), sigma1(MAXPER),
     1     sigma2(MAXPER), b, c1, c2, d, e, f2, f3, g2
      real aT,f1T,g1T,sigma1T,sigma2T, specT
      integer nper, count1, count2, i

      data period / 0.00, 0.04, 0.05, 0.075, 0.10, 0.15, 0.20,
     1              0.30, 0.40, 0.50, 0.75, 1.00, 1.5, 2., 3., 4. /
      data a  / -3.829, -1.901, -1.465, -0.722, -0.304, 0.054, 0.263,
     1           0.388,  0.290,  0.055,  0.014, -0.420, -1.012, -1.214,
     2           -1.451, -1.536 /
      data f1 / 0., 0., 0., 0., 0.,  0., 0., 0., 0.181, 0.463, 0.669,
     1          1.13, 1.52, 1.65, 1.28, 1.15 /
      data g1 /  0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
     1          0.177, 0.568, 0.613, 1.07, 1.26 /
      data sigma1 / 0.668, 0.957, 0.991, 0.909, 0.905, 0.870, 0.761,
     1              0.743, 0.789, 0.827, 0.808, 0.832, 0.939, 0.764,
     2              1.050, 0.845 /
      data sigma2 / 0.575, 0.745, 0.715, 0.661, 0.631, 0.641, 0.590,
     1              0.573, 0.602, 0.589, 0.603, 0.630, 0.615, 0.665,
     2              0.713, 0.716 /
      twoPi = 2. * 3.1415926
      attenName = 'Campbell (1990), vertical'

C Find the requested spectral period and corresponding coefficients
      nPer = 16

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1 = period(1)
         aT      = a(1)
         f1T     = f1(1)
         g1T     = g1(1)
         sigma1T  = sigma1(1)
         sigma2T  = sigma2(1)
         goto 1011
      elseif (specT .ne. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

      write (*,*)
      write (*,*) 'Campbell (1990) Vertical atttenuation model'
      write (*,*) 'is not defined for a spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020       call S24_interp (period(count1),period(count2),a(count1),a(count2),
     +                   specT,aT,iflag)
            call S24_interp (period(count1),period(count2),f1(count1),f1(count2),
     +                   specT,f1T,iflag)
            call S24_interp (period(count1),period(count2),g1(count1),g1(count2),
     +                   specT,g1T,iflag)
            call S24_interp (period(count1),period(count2),sigma1(count1),sigma1(count2),
     +                   specT,sigma1T,iflag)
            call S24_interp (period(count1),period(count2),sigma2(count1),sigma2(count2),
     +                   specT,sigma2T,iflag)

 1011 period1 = specT

      b = 0.991
      c1 = 0.079
      c2 = 0.661
      d = -1.50
      e = 0.111
      f2 = 0.711
      f3 = -4.7
      g2 = 0.513

c     Convert oblique to reverse for Campbell
      ftype1 = ftype
      if ( ftype1 .eq. 0.5 ) then
        ftype1=1.0
      endif
c     Set normal faulting to Stike-slip
      if ( ftype .lt. 0. ) ftype1 = 0.

      lnY = aT + b*m + d*alog(dist+c1*exp(c2*m)) + e*ftype1
     1      + f1T * tanh( f2*(m+f3) )
     2      + g1T*tanh(g2*baseDepth)

c     Convert units spectral acceleration in gal
      if ( specT .eq. 0.0 ) then
        lnY = lnY + 6.89
      else
        lnY = lnY + alog( twoPi/specT )
      endif

c     Set standard error
      if ( m .ge. 6.2 ) then
        sigma = sigma2T
      else
        sigma = sigma1T
      endif

      return
      end

c -------------------------------------------------------------
C *** Cambell (1990;1994) Horizontal, Rock ********************
c -------------------------------------------------------------

      subroutine S02_Camp90_94 ( m, dist, ftype, lnY, sigma, baseDepth,
     1                    specT,attenName, period1,iflag )
c     This  subroutine calculates the LOG SA/PGA for the ave horizontal
c     component using the Campbell (1990) attenuation relation.
c     The LOG SA curve is calculated from the LOG PGA of Campbell 1994
c     Sigma1 and Sigma2 are directly from Campbells paper
c      for M < 6.2 and M>= 6.2, respectively.

      implicit none

      integer MAXPER
      parameter (MAXPER=16)
      real ftype, ftype1, dist, m, lnY, sigma, baseDepth, period1
      real lnPGA94, lnPGA90, twoPi
      character*80 attenName
      real period(MAXPER), d
      real a(MAXPER), f1(MAXPER), g1(MAXPER), sigma1(MAXPER),
     1     sigma2(MAXPER), b, c(12), c1, c2, e, f2, f3, g2
      real specT, aT, f1T, g1T, sigma1T, sigma2T, softrock,
     1     hardrock, r, scale
      integer nper, count1, count2, iflag, i

      data period / 0.00, 0.04, 0.05, 0.075, 0.10, 0.15, 0.20,
     1              0.30, 0.40, 0.50, 0.75, 1.00, 1.5, 2., 3., 4. /
      data a  / -2.245, -0.402, -0.141, 0.489, 0.987, 1.625, 1.988,
     1           2.370,  2.153,  2.086, 1.802, 1.398, 0.795, 0.411,
     2          -0.141, -0.188 /
      data f1 / 0., 0., 0., 0., 0.,  0., 0., 0., 0.514, 0.738, 1.23,
     1          1.59, 1.93, 2.23, 2.39, 2.03 /
      data g1 /  0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.,
     1          0.183, 0.488, 0.634, 0.836, 1.17 /
      data c /  -3.512, 0.904, -1.328, 0.149, 0.647, 1.125, -0.112,
     1          -0.0957, 0.440, -0.171, 0.405, -0.222 /
      data sigma1 / 0.517, 0.716, 0.631, 0.703, 0.703, 0.754, 0.722,
     1              0.597, 0.671, 0.722, 0.776, 0.751, 0.687, 0.591,
     2              0.628, 0.647 /
      data sigma2 / 0.387, 0.387, 0.492, 0.430, 0.427, 0.440, 0.421,
     1              0.382, 0.342, 0.330, 0.420, 0.426, 0.478, 0.496,
     2              0.520, 0.532 /
      twoPi = 2. * 3.1415926
      attenName = 'Campbell (1990;1994), horizontal'

C Find the requested spectral period and corresponding coefficients
      nPer = 16

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1 = period(1)
         aT      = a(1)
         f1T     = f1(1)
         g1T     = g1(1)
         sigma1T  = sigma1(1)
         sigma2T  = sigma2(1)
         goto 1011
      elseif (specT .ne. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

      write (*,*)
      write (*,*) 'Campbell (1990;94) Horizontal atttenuation model'
      write (*,*) 'is not defined for a spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020       call S24_interp (period(count1),period(count2),a(count1),a(count2),
     +                   specT,aT,iflag)
            call S24_interp (period(count1),period(count2),f1(count1),f1(count2),
     +                   specT,f1T,iflag)
            call S24_interp (period(count1),period(count2),g1(count1),g1(count2),
     +                   specT,g1T,iflag)
            call S24_interp (period(count1),period(count2),sigma1(count1),sigma1(count2),
     +                   specT,sigma1T,iflag)
            call S24_interp (period(count1),period(count2),sigma2(count1),sigma2(count2),
     +                   specT,sigma2T,iflag)

 1011 period1 = specT

      b = 1.09
      c1 = 0.361
      c2 = 0.576
      d = -1.89
      e = 0.213
      f2 = 0.659
      f3 = -4.7
      g2 = 0.574

c     Convert oblique to reverse for Campbell
      ftype1 = ftype
      if ( ftype1 .eq. 0.5 ) then
        ftype1=1.0
      endif
c     Set normal faulting to Stike-slip
      if ( ftype .lt. 0. ) ftype1 = 0.

C Compute PGA for 1994 relation.

      softrock = 0
      hardrock = 0

      r = sqrt( dist**2 + (c(4)*exp(c(5)*m))**2)
      lnPGA94 = c(1) + c(2)*m + c(3)*alog(r)
     1          + (c(6)+c(7)*alog(dist)+c(8)*m) * ftype1
     2          + (c(9)+c(10)*alog(dist)) * softRock
     3          + (c(11)+c(12)*alog(dist)) * hardRock
      if ( m .lt. 7.4 ) then
        sigma = 0.889 - 0.0691*m
      else
        sigma = 0.38
      endif

      if ( specT .eq. 0.0 ) then
        lnY = lnPGA94 + 6.89
        return
      endif

c     For Sa, compute the ratio of the PGA94/PGA90 for scaling
      lnPGA90 = a(1) + b*m + d*alog(dist+c1*exp(c2*m)) + e*ftype1
     1      + f1(1) * tanh( f2*(m+f3) )
     2      + g1(1)*tanh(g2*baseDepth)
      scale = lnPGA94 - lnPGA90

C Compute SA using the Campbell 1990 relation.
      lnY = aT + b*m + d*alog(dist+c1*exp(c2*m)) + e*ftype1
     1      + f1T * tanh( f2*(m+f3) )
     2      + g1T*tanh(g2*baseDepth)

       lnY = lnY + scale

c     Convert units spectral acceleration in gal
      lnY = lnY + alog( twoPi/specT )

c     Set standard error
      if ( m .ge. 6.2 ) then
        sigma = sigma2T
      else
        sigma = sigma1T
      endif

      return
      end

c -----------------------------------------------------------
C *** Campbell (1994) Horizontal ****************************
c -----------------------------------------------------------

      subroutine S02_Campbell_94 ( m, d, ftype, lnY, sigma, specT,
     1                         soilFlag, softRock, hardRock, baseDepth,
     2                         attenName, period1,iflag )
c     This subroutine uses the Campbell 1994 PGA with the 1993 Sa/a

      implicit none

      integer MAXPER
      parameter (MAXPER=16)
      real m, d, ftype, lnY, sigma, baseDepth, period1, ftype1
      integer soilFlag, softRock, hardRock,iflag
      real lnPGA93, lnPGA94, alpha
      real b0(MAXPER), b1(MAXPER), b2(MAXPER), b3(MAXPER), b4(MAXPER),
     1     b5(MAXPER), sig0(MAXPER), c(12), period(MAXPER)
      character*80 attenName
      real specT, b0T, b1T, b2T, b3T, b4T, b5T, sig0T, r, scale
      integer nper, count1, count2, i

      data period / 0.0, 0.04, 0.05, 0.075, 0.10, 0.15, 0.2, 0.3, 0.4,
     1               0.5, 0.75, 1.0, 1.5, 2.0, 3.0, 4.0 /
      data b0 / -3.15, -3.14, -3.09, -2.83, -2.61, -2.37, -2.32, -2.36,
     1          -3.02, -3.36, -4.03, -4.73, -5.61, -6.24, -7.12, -7.47/
      data b1 / 0., 0., 0., 0., 0., 0., 0., 0., 0.60, 0.75, 1.06, 1.37,
     1          1.73, 1.96, 2.19, 2.00 /
      data b2 / 0.00, 0.22, 0.18, 0.18, 0.08, -0.09, -0.21, -0.42,
     1         -0.46, -0.50, -0.49, -0.41, -0.29, -0.32, -0.13, -0.20 /
      data b3 / 0., 0., 0., 0., 0., 0., 0., 0., 0.12, 0.25, 0.37, 0.57,
     1          0.72, 0.83, 0.86, 1.05 /
      data b4 / 0.0150, 0.0158, 0.0161, 0.0174, 0.0174, 0.0160, 0.0139,
     1          0.0115, 0.0103, 0.00825, 0.00734, 0.00655, 0.00557,
     2          0.00496, 0.00422, 0.00376 /
      data b5 / -0.000995, -0.00105, -0.00105, -0.00109, -0.000988,
     1          -0.000730, -0.000470, -0.000273, -0.000212,
     2          0., 0., 0., 0., 0., 0., 0. /
      data sig0 / 0.50, 0.53, 0.57, 0.56, 0.58, 0.60, 0.64, 0.61,
     1            0.65, 0.67, 0.69, 0.72, 0.55, 0.52, 0.51, 0.56 /
      data c /  -3.512, 0.904, -1.328, 0.149, 0.647, 1.125, -0.112,
     1          -0.0957, 0.440, -0.171, 0.405, -0.222 /

C Find the requested spectral period and corresponding coefficients
      nPer = 16

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1 = period(1)
         b0T     = b0(1)
         b1T     = b1(1)
         b2T     = b2(1)
         b3T     = b3(1)
         b4T     = b4(1)
         b5T     = b5(1)
         sig0T   = sig0(1)
         goto 1011
      elseif (specT .ne. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

      write (*,*)
      write (*,*) 'Campbell (1994) Horizontal atttenuation model'
      write (*,*) 'is not defined for a spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020       call S24_interp (period(count1),period(count2),b0(count1),b0(count2),
     +                   specT,b0T,iflag)
            call S24_interp (period(count1),period(count2),b1(count1),b1(count2),
     +                   specT,b1T,iflag)
            call S24_interp (period(count1),period(count2),b2(count1),b2(count2),
     +                   specT,b2T,iflag)
            call S24_interp (period(count1),period(count2),b3(count1),b3(count2),
     +                   specT,b3T,iflag)
            call S24_interp (period(count1),period(count2),b4(count1),b4(count2),
     +                   specT,b4T,iflag)
            call S24_interp (period(count1),period(count2),b5(count1),b5(count2),
     +                   specT,b5T,iflag)
            call S24_interp (period(count1),period(count2),sig0(count1),sig0(count2),
     +                   specT,sig0T,iflag)

 1011 period1 = specT

c     Set name
      if ( soilFlag .eq. 0 ) then
        attenName = 'Campbell (1993;1994), soil'
      elseif ( softRock .eq. 1 ) then
        attenName = 'Campbell (1993;1994), soft-rock'
      elseif ( hardRock .eq. 1 ) then
        attenName = 'Campbell (1993;1994), hard-rock'
      else
        write (*,'( 2x,''bad site flags for Campbell 94'')')
        stop 99
      endif

c     Convert oblique to reverse for Campbell
      ftype1 = ftype
      if ( ftype1 .eq. 0.5 ) then
        ftype1=1.0
      endif
c     Set normal faulting to Stike-slip
      if ( ftype .lt. 0. ) ftype1 = 0.

c     Compute pga for 1994 relation
      r = sqrt( d**2 + (c(4)*exp(c(5)*m))**2)
      lnPGA94 = c(1) + c(2)*m + c(3)*alog(r)
     1          + (c(6)+c(7)*alog(d)+c(8)*m) * ftype1
     2          + (c(9)+c(10)*alog(d)) * softRock
     3          + (c(11)+c(12)*alog(d)) * hardRock
      if ( m .lt. 7.4 ) then
        sigma = 0.889 - 0.0691*m
      else
        sigma = 0.38
      endif

      if ( specT .eq. 0.0 ) then
        lnY = lnPGA94 + 6.89
        return
      endif

c     For Sa, compute the ratio of the PGA94/PGA93 for scaling
      r = sqrt( d**2 + (0.0586*exp(0.683*m))**2 )
      alpha = b4(1) + b5(1)*m
      lnPGA93 = b0(1) + 0.683*m + b1(1)*tanh(0.647*(m-4.7))
     1          - alog(r) - alpha*d + 0.27*ftype1
     2          + ( b2(1) - 0.105*alog(d) ) * SoilFlag
     3          + b3(1)*tanh(0.620*baseDepth)
      scale = lnPGA94 - lnPGA93

c     Compute Sa using 93 relation
      r = sqrt( d**2 + (0.0586*exp(0.683*m))**2 )
      alpha = b4T + b5T*m
      lnY = b0T + 0.683*m + b1T*tanh(0.647*(m-4.7))
     1          - alog(r) - alpha*d + 0.27*ftype1
     2          + ( b2T - 0.105*alog(d))*SoilFlag
     3          + b3T*tanh(0.620*baseDepth)
      lnY = LNY + scale
      sigma = sig0T

c     Convert to gal
      lnY = lnY + 6.89

      return
      end

c ------------------------------------------------------------------
C *** Campbell (1997) Horizontal ***********************************
c ------------------------------------------------------------------

      subroutine S02_Camp97_H ( m, dist, ftype, lnY, sigma, baseDepth,
     1                    specT,attenName, period1, Ssr, Shr,iflag )

c     Campbell (1997) - Horizontal

      implicit none

      integer MAXPER
      parameter (MAXPER=14)
      real ftype, dist, m, lnY, sigma, baseDepth, period1
      integer Ssr, Shr,iflag
      character*80 attenName
      real period(MAXPER), lndist, twoPi, ftype1, accH, fSA
      real c1(MAXPER), c2(MAXPER), c3(MAXPER), c4(MAXPER),
     1     c5(MAXPER), c6(MAXPER), c7(MAXPER), c8(MAXPER)
      real specT, c1T, c2T, c3T, c4T, c5T, c6T, c7T, c8T
      integer nper, count1, count2, i

      data period / 0.00, 0.05, 0.075, 0.10, 0.15, 0.20,
     1              0.30, 0.50, 0.75, 1.00, 1.5, 2., 3., 4. /
      data c1 / 0.0, 0.05, 0.27, 0.48, 0.72, 0.79, 0.77, -0.28,
     1         -1.08, -1.79, -2.65, -3.28, -4.07, -4.26 /
      data c2 / 0., 0., 0., 0., 0., 0., 0., 0.74, 1.23, 1.59, 1.98,
     1          2.23, 2.39, 2.03 /
      data c3 / 0., 0., 0., 0., 0., 0., 0., 0.66,  0.66, 0.66,
     1          0.66, 0.66, 0.66, 0.66/
      data c4 / 0.0, -.0011, -.0024, -.0024, -.0010, 0.0011, 0.0035,
     1    0.0068, 0.0077, 0.0085, 0.0094, 0.0100, 0.0108, 0.0112 /
      data c5 / 0., .000055, .000095, .000007, -.00027, -.00053,
     1      -.00072, -.00100, -.00100, -.00100, -.00100, -.00100,
     2      -.00100, -.00100 /
      data c6 / 0.0, 0.20, 0.22, 0.14, -0.02, -0.18, -0.40, -0.42,
     1       -0.44, -0.38, -0.32, -0.36, -0.22, -0.30 /
      data c7 / 0., 0., 0., 0., 0., 0., 0., 0.25, 0.37, 0.57, 0.72,
     1        0.83, 0.86, 1.05 /
      data c8 / 0., 0., 0., 0., 0., 0., 0., 0.62,  0.62, 0.62,
     1        0.62, 0.62, 0.62, 0.62/
      twoPi = 2. * 3.1415926

C Find the requested spectral period and corresponding coefficients
      nPer = 14

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1 = period(1)
         c1T     = c1(1)
         c2T     = c2(1)
         c3T     = c3(1)
         c4T     = c4(1)
         c5T     = c5(1)
         c6T     = c6(1)
         c7T     = c7(1)
         c8T     = c8(1)
         goto 1011
      elseif (specT .ne. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

      write (*,*)
      write (*,*) 'Campbell (1997) Horizontal atttenuation model'
      write (*,*) 'is not defined for a spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020       call S24_interp (period(count1),period(count2),c1(count1),c1(count2),
     +                   specT,c1T,iflag)
            call S24_interp (period(count1),period(count2),c2(count1),c2(count2),
     +                   specT,c2T,iflag)
            call S24_interp (period(count1),period(count2),c3(count1),c3(count2),
     +                   specT,c3T,iflag)
            call S24_interp (period(count1),period(count2),c4(count1),c4(count2),
     +                   specT,c4T,iflag)
            call S24_interp (period(count1),period(count2),c5(count1),c5(count2),
     +                   specT,c5T,iflag)
            call S24_interp (period(count1),period(count2),c6(count1),c6(count2),
     +                   specT,c6T,iflag)
            call S24_interp (period(count1),period(count2),c7(count1),c7(count2),
     +                   specT,c7T,iflag)
            call S24_interp (period(count1),period(count2),c8(count1),c8(count2),
     +                   specT,c8T,iflag)

 1011 period1 = specT

c     Set atten name
      if ( Shr .eq. 1 ) then
        attenName = 'Campbell (1997), horizontal, hard-rock'
      elseif ( Ssr .eq. 1 ) then
        attenName = 'Campbell (1997), horizontal, soft-rock'
      else
        attenName = 'Campbell (1997), horizontal, soil'
      endif

c     Convert oblique to reverse for Campbell
      ftype1 = ftype
      if ( ftype1 .eq. 0.5 ) then
        ftype1=1.0
      endif
c     Set normal faulting to Stike-slip
      if ( ftype .lt. 0. ) ftype1 = 0.

c     Compute peak horizontal acc
      lndist = alog(dist)
      accH = -3.512 + 0.904*m -
     1       1.328*alog(sqrt(dist**2+(0.149*exp(0.647*m))**2))
     2       + (1.125-0.112*lndist-0.0957*m)*ftype1
     3       + (0.440-0.171*lndist)*Ssr
     4       + (0.405-0.222*lndist)*Shr
      lnY = accH
      if ( specT .ne. 0.0 ) then
          if ( baseDepth .ge. 1. ) then
               fSA = 0.
             else
               fSA = c6T*(1-baseDepth)*((1.-Shr)+0.5*Ssr)
             endif
          lnY = accH + c1T + c2T*tanh(c3T*(m-4.7))
     1  + (c4T+c5T*m)*dist + 0.5*Ssr*c6T + c6T*Shr
     2  + c7T*tanh(c8T*baseDepth)*(1.-Shr)+fSA
      endif
c
c     Set sigma for pga
      if ( m .lt. 7.4 ) then
            sigma = 0.889-0.0691*m
          else
            sigma = 0.38
          endif
          if (specT .ne. 0.0 ) then
            sigma = sqrt( sigma**2 + 0.27**2)
          endif

c     Convert units spectral acceleration in gal
      lnY = lnY + 6.89

      return
      end

c ------------------------------------------------------------------
C *** Campbell (1997) Vertical *************************************
c ------------------------------------------------------------------

      subroutine S02_Camp97_Z ( m, dist, ftype, lnY, sigma, baseDepth,
     1                    specT,attenName, period1, Ssr, Shr,iflag )

c     Campbell (1997) - Vertical

      implicit none

      integer MAXPER
      parameter (MAXPER=14)
      real ftype, dist, m, lnY, sigma, baseDepth, period1
      real lnY_H, ftype1
      integer Ssr, Shr,iflag
      character*80 attenName
      real period(MAXPER)
      real c1(MAXPER), c2(MAXPER), c3(MAXPER), c4(MAXPER), c5(MAXPER)
      real specT, c1T, c2T, c3T, c4T, c5T
      integer nper, count1, count2, i

      data period / 0.00, 0.05, 0.075, 0.10, 0.15, 0.20,
     1              0.30, 0.50, 0.75, 1.00, 1.5, 2., 3., 4. /
      data c1 / 0., -1.32, -1.21, -1.29, -1.57, -1.73, -1.98, -2.03,
     1          -1.79, -1.82, -1.81, -1.65, -1.31, -1.35 /
      data c2 / 0., 0., 0., 0., 0., 0., 0., 0.46, 0.67, 1.13,
     1          1.52, 1.65, 1.28, 1.15 /
      data c3 / 0., 0., 0., 0., 0., 0., 0., -0.74, -1.23, -1.59,
     1         -1.98, -2.23, -2.39, -2.03 /
      data c4 / 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.18, 0.57,
     1          0.61, 1.07, 1.26 /
      data c5 / 0., 0., 0., 0., 0., 0., 0., 0., 0., -0.18, -0.49,
     1          -0.63, -0.84, -1.17 /

C Find the requested spectral period and corresponding coefficients
      nPer = 14

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1 = period(1)
         c1T     = c1(1)
         c2T     = c2(1)
         c3T     = c3(1)
         c4T     = c4(1)
         c5T     = c5(1)
         goto 1011
      elseif (specT .ne. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

      write (*,*)
      write (*,*) 'Campbell (1997) Vertical atttenuation model'
      write (*,*) 'is not defined for a spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020       call S24_interp (period(count1),period(count2),c1(count1),c1(count2),
     +                   specT,c1T,iflag)
            call S24_interp (period(count1),period(count2),c2(count1),c2(count2),
     +                   specT,c2T,iflag)
            call S24_interp (period(count1),period(count2),c3(count1),c3(count2),
     +                   specT,c3T,iflag)
            call S24_interp (period(count1),period(count2),c4(count1),c4(count2),
     +                   specT,c4T,iflag)
            call S24_interp (period(count1),period(count2),c5(count1),c5(count2),
     +                   specT,c5T,iflag)

 1011 period1 = specT

c     Convert oblique to reverse for Campbell
      ftype1 = ftype
      if ( ftype1 .eq. 0.5 ) then
        ftype1=1.0
      endif
c     Set normal faulting to Stike-slip
      if ( ftype .lt. 0. ) ftype1 = 0.

c     Compute horizontal Sa
      call S02_Camp97_H ( m, dist, ftype1, lnY_H, sigma, baseDepth,
     1                    specT,attenName, period1, Ssr, Shr,iflag )

c     Set atten name
      if ( Shr .eq. 1 ) then
        attenName = 'Campbell (1997), vertical, hard-rock'
      elseif ( Ssr .eq. 1 ) then
        attenName = 'Campbell (1997), vertical, soft-rock'
      else
        attenName = 'Campbell (1997), vertical, soil'
      endif

c     Scale to get vertical
      if ( specT .eq. 0.0 ) then
         lnY = lnY_H - 1.58-0.10*m
     1       - 1.5*alog(dist+0.079*exp(0.661*m))
     2       + 1.89*alog(dist +0.361*exp(0.576*m))-0.11*ftype1
      else
        lnY = lnY_H + c1T - 0.10*m
     1      + c2T*tanh(0.71*(m-4.7))
     2      +c3T*tanh(0.66*(m-4.7))
     3      -1.50*alog(dist+0.071*exp(0.661*m))
     4      +1.89*alog(dist+0.361*exp(0.576*m)) - 0.11*ftype1
     5      +c4T*tanh(0.51*baseDepth)
     6      + c5T*tanh(0.57*baseDepth)
      endif

c     Set sigma for pga
      if (specT .eq. 0.0 ) then
         sigma = sqrt( sigma**2 + 0.36**2)
      else
         sigma = sqrt( sigma**2 + 0.39**2)
      endif

      return
      end

c ------------------------------------------------------------
C *** Idriss (1991) Horizontal, Rock *************************
c ------------------------------------------------------------

      subroutine S02_Idriss91_rock ( m, d, ftype, lnY, sigma, specT,
     1                           attenName, period1, iflag )
c     This  subroutine calculates the LOG SA using the
c     Idriss (1991) attenuation relation for horizontal Sa.
c     (Ave of horizontal comp)

      implicit none

      real ftype, ftype1
      real d,m,lnY,sigma, specT, period1
      integer iflag
      character*80 attenName
      attenName = 'Idriss (1991), rock'

c     Set normal faulting to Stike-slip
      ftype1 = ftype
      if ( ftype .lt. 0. ) ftype1 = 0.

      if ( m .lt. 6.0 ) then
         call S02_Idriss91_rock_m6 ( m, d, ftype1, lnY, sigma,specT,period1,
     1          iflag)
      else
         call S02_Idriss91_rock_m61 ( m, d, ftype1, lnY, sigma,specT,period1,
     1          iflag)
      endif

      return
      end

c ------------------------------------------------------------

      subroutine S02_Idriss91_rock_m6 ( m, d, ftype, lnY, sigma,
     1           specT, period1,iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=24)
      real ftype, d, m, lnY, sigma, period1
      real a0(MAXPER), a1(MAXPER), a2(MAXPER), b0(MAXPER), sig0(MAXPER),
     1     period(MAXPER)
      real specT, a0T, a1T, a2T, b0T, sig0T, b1, b2
      integer nper, count1, count2, iflag, i

      data period / 0.0, 0.03, 0.05, 0.075, 0.1, 0.11, 0.13, 0.15, 0.2,
     1              0.25, 0.30, 0.35, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0,
     2              1.5, 2.0, 3.0, 4.0, 5.0 /
      data a0 / -0.150, -0.150, -0.278, -0.308, -0.318, -0.328, -0.338,
     1          -0.348, -0.358, -0.429, -0.486, -0.535, -0.577, -0.648,
     2          -0.705, -0.754, -0.796, -0.834, -0.867, -0.970, -1.046,
     3          -1.143, -1.177, -1.214 /
      data a1 / 2.261, 2.261, 2.365, 2.334, 2.319, 2.294, 2.255, 2.219,
     1          2.146, 2.073, 2.010, 1.977, 1.921, 1.818, 1.704, 1.644,
     2          1.593, 1.482, 1.432, 1.072, 0.762, 0.194, -0.466,
     3         -1.361 /
      data a2 / -0.083, -0.083, -0.092, -0.081, -0.075, -0.070, -0.062,
     1          -0.055, -0.042, -0.030, -0.020, -0.016, -0.009,  0.003,
     2           0.017,  0.022,  0.025,  0.039,  0.043,  0.084,  0.121,
     3           0.191,  0.280, 0.410 /
      data b0 / 0.000, 0.000, 0.066, 0.070, 0.072, 0.073, 0.075, 0.076,
     1          0.078, 0.080, 0.082, 0.087, 0.092, 0.099, 0.105, 0.111,
     2          0.115, 0.119, 0.123, 0.136, 0.146, 0.160, 0.169, 0.177/
      data sig0 / 1.39, 1.39, 1.39, 1.39, 1.42, 1.42, 1.42, 1.42, 1.42,
     1            1.42, 1.44, 1.44, 1.44, 1.46, 1.46, 1.48, 1.48, 1.48,
     2            1.48, 1.48, 1.52, 1.52, 1.52, 1.52 /

C Find the requested spectral period and corresponding coefficients
      nPer = 24

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1 = period(1)
         a0T     = a0(1)
         a1T     = a1(1)
         a2T     = a2(1)
         b0T     = b0(1)
         sig0T   = sig0(1)
         goto 1011
      elseif (specT .ne. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

      write (*,*)
      write (*,*) 'Idriss (1991) Horizontal atttenuation model'
      write (*,*) 'is not defined for a spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020       call S24_interp (period(count1),period(count2),a0(count1),a0(count2),
     +                   specT,a0T,iflag)
            call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +                   specT,a1T,iflag)
            call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +                   specT,a2T,iflag)
            call S24_interp (period(count1),period(count2),b0(count1),b0(count2),
     +                   specT,b0T,iflag)
            call S24_interp (period(count1),period(count2),sig0(count1),sig0(count2),
     +                   specT,sig0T,iflag)

 1011 period1 = specT

      b1 = 1.602
      b2 = -0.142
      lnY = a0T + exp(a1T+a2T*m) +
     1      (b0T - exp(b1+b2*m)) * alog(d+20) + 0.2*ftype
      sigma = sig0T - 0.14*m

c     Convert units to spectral acceleration in gal
      lnY = lnY + 6.89

      return
      end

c ------------------------------------------------------------

      subroutine S02_Idriss91_rock_m61 ( m, d, ftype, lnY, sigma,
     1           specT, period1,iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=24)
      real ftype, d, m, lnY, sigma, period1
      real a0(MAXPER), a1(MAXPER), a2(MAXPER), b0(MAXPER), sig0(MAXPER),
     1     period(MAXPER), sig1(MAXPER)
      real specT, a0T, a1T, a2T, b0T, sig0T, sig1T, b1, b2
      integer nper, count1, count2, iflag, i

      data period / 0.0, 0.03, 0.05, 0.075, 0.1, 0.11, 0.13, 0.15, 0.2,
     1              0.25, 0.30, 0.35, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9,
     2              1.0, 1.5, 2.0, 3.0, 4.0, 5.0 /
      data a0 / -0.050, -0.050, -0.278, -0.308, -0.318, -0.328, -0.338,
     1          -0.348, -0.358, -0.429, -0.486, -0.535, -0.577, -0.648,
     2          -0.705, -0.754, -0.796, -0.834, -0.867, -0.970, -1.046,
     3          -1.143, -1.177, -1.214 /
      data a1 / 3.477, 3.477, 3.426, 3.359, 3.327, 3.289, 3.233, 3.185,
     1          3.100, 3.034, 2.982, 2.943, 2.906, 2.850, 2.803, 2.765,
     2          2.728, 2.694, 2.662, 2.536, 2.447, 2.295, 2.169, 2.042/

      data a2 / -0.284, -0.284, -0.269, -0.252, -0.243, -0.236, -0.225,
     1          -0.216, -0.201, -0.190, -0.182, -0.177, -0.173, -0.169,
     2          -0.166, -0.165, -0.164, -0.163, -0.162, -0.160, -0.160,
     3          -0.159, -0.159, -0.157 /

      data b0 / 0.000, 0.000, 0.066, 0.070, 0.072, 0.073, 0.075, 0.076,
     1          0.078, 0.080, 0.082, 0.087, 0.092, 0.099, 0.105, 0.111,
     2          0.115, 0.119, 0.123, 0.136, 0.146, 0.160, 0.169, 0.177/
      data sig0 / 1.39, 1.39, 1.39, 1.39, 1.42, 1.42, 1.42, 1.42, 1.42,
     1            1.42, 1.44, 1.44, 1.44, 1.46, 1.46, 1.48, 1.48, 1.48,
     2            1.48, 1.48, 1.52, 1.52, 1.52, 1.52 /
      data sig1 / 0.38, 0.38, 0.38, 0.38, 0.41, 0.41, 0.41, 0.41, 0.41,
     1            0.41, 0.43, 0.43, 0.43, 0.45, 0.45, 0.47, 0.47, 0.47,
     2            0.47, 0.47, 0.51, 0.51, 0.51, 0.51 /

C Find the requested spectral period and corresponding coefficients
      nPer = 24

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1 = period(1)
         a0T     = a0(1)
         a1T     = a1(1)
         a2T     = a2(1)
         b0T     = b0(1)
         sig0T   = sig0(1)
         sig1T   = sig1(1)
         goto 1011
      elseif (specT .ne. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

      write (*,*)
      write (*,*) 'Idriss (1991) Horizontal atttenuation model'
      write (*,*) 'is not defined for a spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020       call S24_interp (period(count1),period(count2),a0(count1),a0(count2),
     +                   specT,a0T,iflag)
            call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +                   specT,a1T,iflag)
            call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +                   specT,a2T,iflag)
            call S24_interp (period(count1),period(count2),b0(count1),b0(count2),
     +                   specT,b0T,iflag)
            call S24_interp (period(count1),period(count2),sig0(count1),sig0(count2),
     +                   specT,sig0T,iflag)
            call S24_interp (period(count1),period(count2),sig1(count1),sig1(count2),
     +                   specT,sig1T,iflag)

 1011 period1 = specT

      b1 = 2.475
      b2 = -0.286
      lnY = a0T + exp(a1T+a2T*m)
     1      + (b0T - exp(b1+b2*m)) * alog(d+20) + 0.2*ftype
      if ( m .lt. 7.25 ) then
        sigma = sig0T - 0.14*m
      else
        sigma = sig1T
      endif

c     Convert units to spectral acceleration in gal
      lnY = lnY + 6.89

      return
      end

c -------------------------------------------------------------------
C *** Idriss (1991) Horizontal Soft Soil (PGA only) *****************
c -------------------------------------------------------------------

      subroutine S02_Idriss91_soft ( m, d, ftype, lnY, sigma,
     1                           attenName, period1 )
c     This  subroutine calculates the LOG SA using the
c     Idriss (1991) attenuation relation for horizontal Sa.
c     (Ave of horizontal comp)

      implicit none

      real ftype, ftype1
      real d,m,lnY,sigma, period1, b1, b2, a1, a2
      character*80 attenName
      attenName = 'Idriss (1991), soft-soil'

c     Set normal faulting to Stike-slip
      ftype1 = ftype
      if ( ftype .lt. 0. ) ftype1 = 0.

      if ( m .lt. 6.0 ) then
        b1 = 1.285
        b2 = -0.206
            a1 = 1.673
            a2 = -0.137
      else
        b1 = 2.015
        b2 = -0.328
            a1 = 2.952
            a2 = -0.35
      endif
      period1 = 0.
      lnY = exp(a1+a2*m) - exp(b1+b2*m) * alog(d+20) + 0.2*ftype1
      if ( m .le. 7. ) then
            sigma = 1.39 - 0.14*m
      else
            sigma = 0.38
          endif

c     Convert units to spectral acceleration in gal
      lnY = lnY + 6.89
      return
      end

c -------------------------------------------------------------------
C *** Idriss (1997) Horizontal Soft Soil (PGA only) *****************
c -------------------------------------------------------------------

      subroutine S02_Idriss97_soft ( m, d, ftype, lnY, sigma,
     1                           attenName, period1 )
c     This  subroutine calculates the LOG SA using the
c     Idriss (1991) attenuation relation for horizontal Sa.
c     (Ave of horizontal comp)

      implicit none

      real ftype
      real d,m,lnY,sigma, period1, ftype1, b1, b2, a0, a1, a2, f
      character*80 attenName
      attenName = 'Idriss (1997), soft-soil'

c     Set normal faulting to Stike-slip
      ftype1 = ftype
      if ( ftype .lt. 0. ) ftype1 = 0.

      if ( m .lt. 6.0 ) then
        b1 = 0.563
        b2 = -0.133
        a0 = -0.03
        a1 = -0.472
        a2 = 0.106
        f = 0.
      else
        b1 = 1.957
        b2 = -0.368
        a0 = 0.
        a1 = 2.638
        a2 = -0.425
        f = 0.05*(1-tanh(d/20.))
      endif
      period1 = 0.
      lnY = a0 + exp(a1+a2*m) - exp(b1+b2*m) * alog(d+10) + 0.28*ftype1
     1      + f
        sigma = 1.29 - 0.12*m
      if ( sigma .lt. 0.42 ) sigma = 0.42

c     Convert units to spectral acceleration in gal
      lnY = lnY + 6.89
      return
      end

c ------------------------------------------------------------
C *** Idriss (1991:1995) Horizontal, Rock ********************
c ------------------------------------------------------------

      subroutine S02_Idriss91_95_rock ( m, d, ftype, lnY, sigma, specT,
     1                           attenName, period1,iflag )

c     This  subroutine calculates the LOG SA using the
c     Idriss (1991) attenuation relation for horizontal Sa.
c     and the PGA from the 1995 relationship (Ave of horizontal comp)

      implicit none

      real ftype
      real d,m,lnY,sigma,specT, period1, ftype1
      integer iflag
      character*80 attenName
      attenName = 'Idriss (1991;1995), Rock'

c     Set normal faulting to Stike-slip
      ftype1 = ftype
      if ( ftype .lt. 0. ) ftype1 = 0.

      if ( m .lt. 6.0 ) then
         call S02_Idriss91_95_rock_m6 ( m, d, ftype1, lnY, sigma,specT,
     1          period1,iflag)
      else
         call S02_Idriss91_95_rock_m61 ( m, d, ftype1, lnY, sigma,specT,
     1          period1,iflag)
      endif

      return
      end

c ------------------------------------------------------------

      subroutine S02_Idriss91_95_rock_m6 ( m, d, ftype, lnY, sigma,
     1           specT, period1,iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=24)
      real ftype, d, m, lnY, sigma, period1, b1, b2
      real a0(MAXPER), a1(MAXPER), a2(MAXPER), b0(MAXPER), sig0(MAXPER),
     1     period(MAXPER)
      real specT, a0T, a1T, a2T, b0T, sig0T, pga91, pga95
      integer nper, count1, count2, iflag, i

      data period / 0.0, 0.03, 0.05, 0.075, 0.1, 0.11, 0.13, 0.15, 0.2,
     1              0.25, 0.30, 0.35, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0,
     2              1.5, 2.0, 3.0, 4.0, 5.0 /
      data a0 / -0.150, -0.150, -0.278, -0.308, -0.318, -0.328, -0.338,
     1          -0.348, -0.358, -0.429, -0.486, -0.535, -0.577, -0.648,
     2          -0.705, -0.754, -0.796, -0.834, -0.867, -0.970, -1.046,
     3          -1.143, -1.177, -1.214 /
      data a1 / 2.261, 2.261, 2.365, 2.334, 2.319, 2.294, 2.255, 2.219,
     1          2.146, 2.073, 2.010, 1.977, 1.921, 1.818, 1.704, 1.644,
     2          1.593, 1.482, 1.432, 1.072, 0.762, 0.194, -0.466,
     3         -1.361 /
      data a2 / -0.083, -0.083, -0.092, -0.081, -0.075, -0.070, -0.062,
     1          -0.055, -0.042, -0.030, -0.020, -0.016, -0.009,  0.003,
     2           0.017,  0.022,  0.025,  0.039,  0.043,  0.084,  0.121,
     3           0.191,  0.280, 0.410 /
      data b0 / 0.000, 0.000, 0.066, 0.070, 0.072, 0.073, 0.075, 0.076,
     1          0.078, 0.080, 0.082, 0.087, 0.092, 0.099, 0.105, 0.111,
     2          0.115, 0.119, 0.123, 0.136, 0.146, 0.160, 0.169, 0.177/
      data sig0 / 1.29, 1.29, 1.29, 1.29, 1.32, 1.33, 1.34, 1.35, 1.37,
     1            1.38, 1.39, 1.40, 1.41, 1.42, 1.43, 1.44, 1.45, 1.46,
     2            1.47, 1.47, 1.47, 1.47, 1.47, 1.47 /

C Find the requested spectral period and corresponding coefficients
      nPer = 24

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1 = period(1)
         a0T     = a0(1)
         a1T     = a1(1)
         a2T     = a2(1)
         b0T     = b0(1)
         sig0T   = sig0(1)
         goto 1011
      elseif (specT .ne. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

      write (*,*)
      write (*,*) 'Idriss (1991:1995) Horizontal atttenuation model'
      write (*,*) 'is not defined for a spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020       call S24_interp (period(count1),period(count2),a0(count1),a0(count2),
     +                   specT,a0T,iflag)
            call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +                   specT,a1T,iflag)
            call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +                   specT,a2T,iflag)
            call S24_interp (period(count1),period(count2),b0(count1),b0(count2),
     +                   specT,b0T,iflag)
            call S24_interp (period(count1),period(count2),sig0(count1),sig0(count2),
     +                   specT,sig0T,iflag)

 1011 period1 = specT

c Compute the Idriss (1991) PGA value.
      pga91 = a0(1) + exp(a1(1)+a2(1)*m) +
     1      (b0(1) - exp(1.602-0.142*m)) * alog(d+20) + 0.2*ftype

c Compute the Idriss (1995) PGA value.
      pga95 = exp(1.127+0.011*m) +
     1      (-exp(1.126-0.106*m)) * alog(d+10) + 0.28*ftype

c Compute the Idriss (1991) spectral value.
      b1 = 1.602
      b2 = -0.142
      lnY = a0T + exp(a1T+a2T*m) +
     1      (b0T - exp(b1+b2*m)) * alog(d+20) + 0.2*ftype

c Now apply the scaling factor for spectral values.
      if (specT .ne. 0.0) then
         lnY = alog((exp(lnY)/exp(pga91))*exp(pga95))
      else
         lnY = pga95
      endif

      sigma = sig0T - 0.12*m

c     Convert units to spectral acceleration in gal
      lnY = lnY + 6.89

      return
      end

c ------------------------------------------------------------

      subroutine S02_Idriss91_95_rock_m61 ( m, d, ftype, lnY, sigma,
     1           specT, period1,iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=24)
      real ftype, d, m, lnY, sigma, period1, b1, b2
      real a0(MAXPER), a1(MAXPER), a2(MAXPER), b0(MAXPER), sig0(MAXPER),
     1     period(MAXPER), sig1(MAXPER)
      real specT, a0T, a1T, a2T, b0T, sig0T, sig1T, pga91, pga95
      integer nper, count1, count2, iflag, i

      data period / 0.0, 0.03, 0.05, 0.075, 0.1, 0.11, 0.13, 0.15, 0.2,
     1              0.25, 0.30, 0.35, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9,
     2              1.0, 1.5, 2.0, 3.0, 4.0, 5.0 /
      data a0 / -0.050, -0.050, -0.278, -0.308, -0.318, -0.328, -0.338,
     1          -0.348, -0.358, -0.429, -0.486, -0.535, -0.577, -0.648,
     2          -0.705, -0.754, -0.796, -0.834, -0.867, -0.970, -1.046,
     3          -1.143, -1.177, -1.214 /
      data a1 / 3.477, 3.477, 3.426, 3.359, 3.327, 3.289, 3.233, 3.185,
     1          3.100, 3.034, 2.982, 2.943, 2.906, 2.850, 2.803, 2.765,
     2          2.728, 2.694, 2.662, 2.536, 2.447, 2.295, 2.169, 2.042/
      data a2 / -0.284, -0.284, -0.269, -0.252, -0.243, -0.236, -0.225,
     1          -0.216, -0.201, -0.190, -0.182, -0.177, -0.173, -0.169,
     2          -0.166, -0.165, -0.164, -0.163, -0.162, -0.160, -0.160,
     3          -0.159, -0.159, -0.157 /
      data b0 / 0.000, 0.000, 0.066, 0.070, 0.072, 0.073, 0.075, 0.076,
     1          0.078, 0.080, 0.082, 0.087, 0.092, 0.099, 0.105, 0.111,
     2          0.115, 0.119, 0.123, 0.136, 0.146, 0.160, 0.169, 0.177/
      data sig0 / 1.29, 1.29, 1.29, 1.29, 1.32, 1.33, 1.34, 1.35, 1.37,
     1            1.38, 1.39, 1.40, 1.41, 1.42, 1.43, 1.44, 1.45, 1.46,
     2            1.47, 1.47, 1.47, 1.47, 1.47, 1.47 /
      data sig1 / 0.42, 0.42, 0.42, 0.42, 0.45, 0.46, 0.47, 0.48, 0.50,
     1            0.51, 0.52, 0.53, 0.54, 0.55, 0.56, 0.57, 0.58, 0.59,
     2            0.60, 0.60, 0.60, 0.60, 0.60, 0.60 /

C Find the requested spectral period and corresponding coefficients
      nPer = 24

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1 = period(1)
         a0T     = a0(1)
         a1T     = a1(1)
         a2T     = a2(1)
         b0T     = b0(1)
         sig0T   = sig0(1)
         sig1T   = sig1(1)
         goto 1011
      elseif (specT .ne. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

      write (*,*)
      write (*,*) 'Idriss (1991:1995) Horizontal atttenuation model'
      write (*,*) 'is not defined for a spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020       call S24_interp (period(count1),period(count2),a0(count1),a0(count2),
     +                   specT,a0T,iflag)
            call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +                   specT,a1T,iflag)
            call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +                   specT,a2T,iflag)
            call S24_interp (period(count1),period(count2),b0(count1),b0(count2),
     +                   specT,b0T,iflag)
            call S24_interp (period(count1),period(count2),sig0(count1),sig0(count2),
     +                   specT,sig0T,iflag)
            call S24_interp (period(count1),period(count2),sig1(count1),sig1(count2),
     +                   specT,sig1T,iflag)

 1011 period1 = specT

c Compute the Idriss (1991) PGA value.
      pga91 = a0(1) + exp(a1(1)+a2(1)*m) +
     1      (b0(1) - exp(2.475-0.286*m)) * alog(d+20) + 0.2*ftype

c Compute the Idriss (1995) PGA value.
      pga95 = exp(2.763-0.262*m) +
     1      (-exp(2.215-0.288*m)) * alog(d+10) + 0.28*ftype

c Compute the Idriss (1991) spectral value.
      b1 = 2.475
      b2 = -0.286
      lnY = a0T + exp(a1T+a2T*m)
     1      + (b0T - exp(b1+b2*m)) * alog(d+20) + 0.2*ftype

c Now apply the scaling factor for spectral values.
      if (specT .ne. 0.0) then
         lnY = alog((exp(lnY)/exp(pga91))*exp(pga95))
      else
         lnY = pga95
      endif

c     Set the Sigma value.
      if ( m .lt. 7.25 ) then
        sigma = sig0T - 0.12*m
      else
        sigma = sig1T
      endif

c     Convert units to spectral acceleration in gal
      lnY = lnY + 6.89

      return
      end

c ----------------------------------------------------------------
C *** Geomatrix (1993) Vertical Rock *****************************
c ----------------------------------------------------------------

      subroutine S02_Geomatrix93_V_rock ( m, d, ftype, lnY, sigma, specT,
     1                                attenName, period1,iflag )

c     This  subroutine calculates the LOG SA using the
c     Sadigh et al (1993) attenuation relation for vertical Sa.

      implicit none

      real ftype, specT, ftype1
      real d,m,lnY,sigma, period1
      integer iflag
      character*80 attenName
      attenName = 'Geomatrix (1993) vertical Rock'

c     Set normal faulting to Stike-slip
      ftype1 = ftype
      if ( ftype .lt. 0. ) ftype1 = 0.

      if ( m .lt. 6.5 ) then
         call S02_Geomatrix93_V_rock_mLT65 ( m, d, ftype1, lnY, sigma, specT,
     1       period1,iflag )
      else
         call S02_Geomatrix93_V_rock_mGE65 ( m, d, ftype1, lnY, sigma, specT,
     1       period1,iflag )
      endif
      return
      end

c ------------------------------------------------------------

      subroutine S02_Geomatrix93_V_rock_mLT65 ( m, d, ftype, lnY, sigma,
     1            specT, period1,iflag )

      implicit none

      real ftype, d, m, lnY, sigma, period1
      real specT, c1T, c3T, c4T, c2, c5, c6, c8
      integer nper, count1, count2, iflag, MAXPER, i
      parameter (MAXPER=22)
      real c1(MAXPER), c3(MAXPER), c4(MAXPER), period(MAXPER)

      data period / 0.00, 0.04, 0.05, 0.06, 0.07, 0.09, 0.10, 0.12,
     1              0.14, 0.15, 0.17, 0.20, 0.24, 0.30, 0.40, 0.50,
     2              0.75, 1.00, 1.50, 2.00, 2.50, 3.00 /
      data c1 / -0.4300, 0.3379, 0.5041, 0.6095, 0.6896, 0.6718,
     1            0.6252, 0.5535, 0.3813, 0.2524, 0.0122, -0.3005,
     2           -0.6678, -1.1392, -1.7656, -2.2748, -3.2062, -3.8818,
     3           -4.2618, -4.5719, -4.8167, -5.0364 /
      data c3 / 0.0, 0.0, 0.0, 0.0, 0.0, -0.0033, -0.00468, -0.00707,
     1          -0.00909, -0.01000, -0.01462, -0.02061, -0.02734,
     2           -0.03558, -0.04619, -0.05442, -0.06939, -0.08000,
     3           -0.08554, -0.08946, -0.09251, -0.09500 /
      data c4 / -2.300, -2.450, -2.450, -2.450, -2.450, -2.420, -2.400,
     1          -2.380, -2.333, -2.300, -2.241, -2.164, -2.077, -1.971,
     2        -1.835, -1.729, -1.536, -1.400, -1.400, -1.4, -1.4, -1.4/

C Find the requested spectral period and corresponding coefficients
      nPer = 22

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1 = period(1)
         c1T     = c1(1)
         c3T     = c3(1)
         c4T     = c4(1)
         goto 1011
      elseif (specT .ne. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

      write (*,*)
      write (*,*) 'Geomatrix (1993) Vertical atttenuation model'
      write (*,*) 'is not defined for a spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),c1(count1),c1(count2),
     +             specT,c1T,iflag)
      call S24_interp (period(count1),period(count2),c3(count1),c3(count2),
     +             specT,c3T,iflag)
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),
     +             specT,c4T,iflag)

 1011 period1 = specT

      c2 = 1.0
      c5 = 1.2726
      c6 = 0.228
      c8 = 0.0953

c Set the maximum magnitude value at M=8.5. Magnitude larger than this value
c would result in lower ground motions for larger magnitude events.
      if (m .gt. 8.5) then
         m = 8.5
      endif

      lnY = c1T + c2*m + c3T*(8.5-m)**(2.5)
     1      + c4T*alog(d+exp(c5+c6*m)) + c8*ftype
      if ( specT .eq. 0.0) then
        if ( m .lt. 6.0 ) then
          sigma = 0.68
        else
          sigma = 3.08 - 0.4*m
        endif
      else
         if ( m .lt. 6.0 ) then
          sigma = 0.75
        else
          sigma = 2.91 - 0.36*m
        endif
      endif

c     Convert units to spectral acceleration in gal
      lnY = lnY + 6.89
      return
      end

c ------------------------------------------------------------

      subroutine S02_Geomatrix93_V_rock_mGE65 ( m, d, ftype, lnY, sigma,
     1           specT, period1,iflag )

      implicit none

      real ftype, d, m, lnY, sigma, period1, specT
      real c1T, c3T, c4T, c2, c5, c6, c8
      integer nper, count1, count2, iflag, MAXPER, i
      parameter (MAXPER=22)
      real c1(MAXPER), c3(MAXPER), c4(MAXPER), period(MAXPER)

      data period / 0.00, 0.04, 0.05, 0.06, 0.07, 0.09, 0.10, 0.12,
     1              0.14, 0.15, 0.17, 0.20, 0.24, 0.30, 0.40, 0.50,
     2              0.75, 1.00, 1.50, 2.00, 2.50, 3.00 /
      data c1 / -1.080, -0.3121, -0.1459, -0.0405, 0.03956, 0.0218,
     1          -0.0248, -0.0965, -0.2687, -0.3976, -0.6378, -0.9505,
     2          -1.3178, -1.7893, -2.4157, -2.9248, -3.8562, -4.5318,
     3          -4.9118, -5.2219, -5.4667, -5.6864 /
      data c3 / 0.0, 0.0, 0.0, 0.0, 0.0, -0.0033, -0.00468, -0.00707,
     1          -0.00909, -0.01000, -0.01462, -0.02061, -0.02734,
     2           -0.03558, -0.04619, -0.05442, -0.06939, -0.08000,
     3           -0.08554, -0.08946, -0.09251, -0.09500 /
      data c4 / -2.300, -2.450, -2.450, -2.450, -2.450, -2.420, -2.400,
     1          -2.380, -2.333, -2.300, -2.241, -2.164, -2.077, -1.971,
     2          -1.835, -1.729, -1.536, -1.400, -1.4, -1.4, -1.4, -1.4/

C Find the requested spectral period and corresponding coefficients
      nPer = 22

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1 = period(1)
         c1T     = c1(1)
         c3T     = c3(1)
         c4T     = c4(1)
         goto 1011
      elseif (specT .ne. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

      write (*,*)
      write (*,*) 'Geomatrix (1993) Vertical atttenuation model'
      write (*,*) 'is not defined for a spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),c1(count1),c1(count2),
     +             specT,c1T,iflag)
      call S24_interp (period(count1),period(count2),c3(count1),c3(count2),
     +             specT,c3T,iflag)
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),
     +             specT,c4T,iflag)

 1011 period1 = specT

      c2 = 1.1
      c5 = -0.3524
      c6 = 0.478
      c8 = 0.0953

c Set the maximum magnitude value at M=8.5. Magnitude larger than this value
c would result in lower ground motions for larger magnitude events.
      if (m .gt. 8.5) then
         m = 8.5
      endif

      lnY = c1T + c2*m + c3T*(8.5-m)**(2.5)
     1      + c4T*alog(d+exp(c5+c6*m)) + c8*ftype
      if ( specT .eq. 0.0) then
        sigma = 0.48
      else
        sigma = 0.57
      endif

c     Convert units to spectral acceleration in gal
      lnY = lnY + 6.89
      return
      end

c ------------------------------------------------------------
C *** Sadigh et al. (1997) Horizontal Rock *******************
c ------------------------------------------------------------

      subroutine S02_Geomatrix93_H_rock ( m, d, ftype, lnY, sigma, specT,
     1                                attenName, period1,iflag )
c     This  subroutine calculates the LOG SA using the
c     Sadigh et al (1993) attenuation relation for horizontal Sa.
c     (Ave of horizontal comp)

      implicit none

      real ftype, period1
      real d,m,lnY,sigma, specT, ftype1
      integer iflag
      character*80 attenName
      attenName = 'Sadigh et al. (1997), Horizontal, rock'

c     Set normal faulting to Stike-slip
      ftype1 = ftype
      if ( ftype .lt. 0. ) ftype1 = 0.

      if ( m .lt. 6.5 ) then
         call S02_Geomatrix93_H_rock_mLT65 ( m, d, ftype1, lnY, sigma, specT,
     1       period1,iflag )
      else
         call S02_Geomatrix93_H_rock_mGE65 ( m, d, ftype1, lnY, sigma, specT,
     1       period1,iflag )
      endif
      return
      end

c ------------------------------------------------------------

      subroutine S02_Geomatrix93_H_rock_mLT65 ( m, d, ftype, lnY, sigma,
     1           specT, period1,iflag )

      implicit none

      real ftype, d, m, lnY, sigma, period1, specT
      integer nPer, count1, count2, iflag, MAXPER, i
      real c1T, c3T, c4T, c7T, sig0T, c2, c5, c6
      parameter (MAXPER=22)
      real c1(MAXPER), c3(MAXPER), c4(MAXPER), c7(MAXPER),
     1     sig0(MAXPER), period(MAXPER)

      data period / 0.00, 0.05, 0.07, 0.09, 0.10, 0.12, 0.14, 0.15,
     1              0.17, 0.20, 0.24, 0.30, 0.40, 0.50, 0.75, 1.00,
     2              1.50, 2.00, 3.00, 4.00, 5.00, 7.50 /
      data c1 / -0.624, -0.090,  0.110,  0.212,  0.275,  0.348,  0.307,
     1           0.285,  0.239,  0.153,  0.060, -0.075, -0.298, -0.588,
     2          -1.208, -1.705, -2.407, -2.945, -3.700, -4.230, -4.714,
     3          -5.530 /
      data c3 / 0.000, 0.006, 0.006, 0.006, 0.006, 0.005, 0.004, 0.002,
     1          0.000, -0.004, -0.011, -0.017, -0.028, -0.040, -0.050,
     2         -0.055, -0.065, -0.070, -0.080, -0.100, -0.100, -0.110 /
      data c4 / -2.100, -2.128, -2.128, -2.140, -2.148, -2.162, -2.144,
     1          -2.130, -2.110, -2.080, -2.053, -2.028, -1.990, -1.945,
     2          -1.865, -1.800, -1.725, -1.670, -1.615, -1.570, -1.540,
     3          -1.510 /
      data c7 / 0.0, -0.082, -0.082, -0.052, -0.041, -0.014, 0., 0.,0.,
     1           0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0./
      data sig0 / 1.39, 1.39, 1.40, 1.40, 1.41, 1.41, 1.42, 1.42, 1.42,
     1            1.43, 1.44, 1.45, 1.48, 1.50, 1.52, 1.53, 1.53, 1.53,
     2            1.53, 1.53, 1.53, 1.53/

C Find the requested spectral period and corresponding coefficients
      nPer = 22

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1 = period(1)
         c1T     = c1(1)
         c3T     = c3(1)
         c4T     = c4(1)
         c7T     = c7(1)
         sig0T   = sig0(1)
         goto 1011
      elseif (specT .ne. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

      write (*,*)
      write (*,*) 'Geomatrix (1997) Horizontal Rock atttenuation model'
      write (*,*) 'is not defined for a spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),c1(count1),c1(count2),
     +             specT,c1T,iflag)
      call S24_interp (period(count1),period(count2),c3(count1),c3(count2),
     +             specT,c3T,iflag)
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),
     +             specT,c4T,iflag)
      call S24_interp (period(count1),period(count2),c7(count1),c7(count2),
     +             specT,c7T,iflag)
      call S24_interp (period(count1),period(count2),sig0(count1),sig0(count2),
     +             specT,sig0T,iflag)

 1011 period1 = specT

      c2 = 1.0
      c5 = 1.29649
      c6 = 0.250

c Set the maximum magnitude value at M=8.5. Magnitude larger than this value
c would result in lower ground motions for larger magnitude events.
      if (m .gt. 8.5) then
         m = 8.5
      endif

      lnY = c1T + c2*m + c3T*(8.5-m)**(2.5)
     1      + c4T*alog(d+exp(c5+c6*m)) + c7T*alog(d+2)
     2      + 0.1823*ftype
      sigma = sig0T - 0.14*m

c     Convert units to spectral acceleration in gal
      lnY = lnY + 6.89
      return
      end

c ------------------------------------------------------------

      subroutine S02_Geomatrix93_H_rock_mGE65 ( m, d, ftype, lnY, sigma,
     1           specT, period1,iflag )

      implicit none

      real ftype, d, m, lnY, sigma, period1, specT
      real c1T, c3T, c4T, c7T, sig0T, sig1T
      integer nper, count1, count2, iflag, MAXPER, i
      parameter (MAXPER=22)
      real c1(MAXPER), c3(MAXPER), c4(MAXPER), c7(MAXPER),
     1     sig0(MAXPER), sig1(MAXPER), period(MAXPER), c2,
     2     c5, c6

      data period / 0.00, 0.05, 0.07, 0.09, 0.10, 0.12, 0.14, 0.15,
     1              0.17, 0.20, 0.24, 0.30, 0.40, 0.50, 0.75, 1.00,
     2              1.50, 2.00, 3.00, 4.00, 5.00, 7.50 /
      data c1 / -1.274, -0.740, -0.540, -0.438, -0.375, -0.302, -0.343,
     1          -0.365, -0.411, -0.497, -0.590, -0.707, -0.948, -1.238,
     2          -1.858, -2.355, -3.057, -3.595, -4.350, -4.880, -5.364,
     3          -6.180 /
      data c3 / 0.000, 0.006, 0.006, 0.006, 0.006, 0.005, 0.004, 0.002,
     1          0.000, -0.004, -0.011, -0.017, -0.028, -0.040, -0.050,
     2         -0.055, -0.065, -0.070, -0.080, -0.100, -0.100, -0.110 /
      data c4 / -2.100, -2.128, -2.128, -2.140, -2.148, -2.162, -2.144,
     1          -2.130, -2.110, -2.080, -2.053, -2.028, -1.990, -1.945,
     2          -1.865, -1.800, -1.725, -1.670, -1.615, -1.570, -1.540,
     3          -1.510 /
      data c7 / 0.0, -0.082, -0.082, -0.052, -0.041, -0.014, 0., 0., 0.,
     1           0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0./
      data sig0 / 1.39, 1.39, 1.40, 1.40, 1.41, 1.41, 1.42, 1.42, 1.42,
     1            1.43, 1.44, 1.45, 1.48, 1.50, 1.52, 1.53, 1.53, 1.53,
     2            1.53, 1.53, 1.53, 1.53/
      data sig1 / 0.38, 0.38, 0.39, 0.39, 0.40, 0.40, 0.41, 0.41, 0.41,
     1            0.42, 0.43, 0.44, 0.47, 0.49, 0.51, 0.52, 0.52, 0.52,
     2            0.52, 0.52, 0.52, 0.52/

C Find the requested spectral period and corresponding coefficients
      nPer = 22

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1 = period(1)
         c1T     = c1(1)
         c3T     = c3(1)
         c4T     = c4(1)
         c7T     = c7(1)
         sig0T   = sig0(1)
         sig1T   = sig1(1)
         goto 1011
      elseif (specT .ne. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

      write (*,*)
      write (*,*) 'Geomatrix (1997) Horizontal Rock atttenuation model'
      write (*,*) 'is not defined for a spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)

      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),c1(count1),c1(count2),
     +             specT,c1T,iflag)
      call S24_interp (period(count1),period(count2),c3(count1),c3(count2),
     +             specT,c3T,iflag)
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),
     +             specT,c4T,iflag)
      call S24_interp (period(count1),period(count2),c7(count1),c7(count2),
     +             specT,c7T,iflag)
      call S24_interp (period(count1),period(count2),sig0(count1),sig0(count2),
     +             specT,sig0T,iflag)
      call S24_interp (period(count1),period(count2),sig1(count1),sig1(count2),
     +             specT,sig1T,iflag)

 1011 period1 = specT

      c2 = 1.1
      c5 = -0.48451
      c6 = 0.524

c Set the maximum magnitude value at M=8.5. Magnitude larger than this value
c would result in lower ground motions for larger magnitude events.
      if (m .gt. 8.5) then
         m = 8.5
      endif

      lnY = c1T + c2*m + c3T*(8.5-m)**(2.5)
     1      + c4T*alog(d+exp(c5+c6*m)) + c7T*alog(d+2)
     2      + 0.1823*ftype
      if ( m .lt. 7.25 ) then
        sigma = sig0T - 0.14*m
      else
        sigma = sig1T
      endif

c     Convert units to spectral acceleration in gal
      lnY = lnY + 6.89
      return
      end

c ------------------------------------------------------------
C *** Sadigh et al. (1997) Horizontal Soil *******************
c ------------------------------------------------------------

      subroutine S02_Sadigh97_H_soil ( m, dist, ftype, lnY, sigma, specT,
     1           attenName, period1,iflag )

      implicit none

      real ftype, dist, m, lnY, sigma, c1ss, c1rv, c2, c3, c4,
     1     c5
      real specT, c6ssT, c6rvT, c7T, sig0T, ftype1
      integer iflag, count1, count2, MAXPER, nPer, i
      character*80 attenName
      parameter (MAXPER=13)
      real c7(MAXPER),
     1     sig0(MAXPER), period(MAXPER), period1 ,
     2     c6ss(MAXPER), c6rv(MAXPER)

      data period / 0.00, 0.075, 0.1, 0.2, 0.3, 0.4, 0.5, 0.75,
     1        1.0, 1.5, 2.0, 3.0, 4.0 /
      data c6ss / 0., 0.4572, 0.6395, 0.9187, 0.9547, 0.9251,
     1     0.8494, 0.7010, 0.5665, 0.3235, 0.1001, -0.2801, -0.6274 /
      data c6rv / 0.0, 0.4572, 0.6395, 0.9187, 0.9547, 0.9005,
     1     0.8285, 0.6802, 0.5075, 0.2215, -0.0526, -0.4905, -0.8907 /
      data c7 / 0., 0.005, 0.005, -0.004, -0.014, -0.024, -0.033,
     1     -0.051, -0.065, -0.090, -0.108, -0.139, -0.160 /

      data sig0 / 1.52, 1.54, 1.54, 1.565, 1.58, 1.595, 1.61, 1.635,
     1      1.66, 1.69, 1.70, 1.71, 1.71/

      attenName = 'Sadigh et al. (1997), soil'

C Find the requested spectral period and corresponding coefficients
      nPer = 13

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1 = period(1)
         c6ssT   = c6ss(1)
         c6rvT   = c6rv(1)
         c7T     = c7(1)
         sig0T   = sig0(1)
         goto 1011
      elseif (specT .ne. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

      write (*,*)
      write (*,*) 'Geomatrix (1997) Horizontal Soil atttenuation model'
      write (*,*) 'is not defined for a spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),c6ss(count1),c6ss(count2),
     +             specT,c6ssT,iflag)
      call S24_interp (period(count1),period(count2),c6rv(count1),c6rv(count2),
     +             specT,c6rvT,iflag)
      call S24_interp (period(count1),period(count2),c7(count1),c7(count2),
     +             specT,c7T,iflag)
      call S24_interp (period(count1),period(count2),sig0(count1),sig0(count2),
     +             specT,sig0T,iflag)

 1011 period1 = specT

c     Set normal faulting to Stike-slip
      ftype1 = ftype
      if ( ftype .lt. 0. ) ftype1 = 0.

      c1ss = -2.17
      c1rv = -1.92
      c2 = 1.0
      c3 = 1.70
      if ( m .le. 6.5 ) then
         c4 = 2.1863
         c5 = 0.32
      else
         c4 = 0.3825
         c5 = 0.5882
      endif

c Set the maximum magnitude value at M=8.5. Magnitude larger than this value
c would result in lower ground motions for larger magnitude events.
      if (m .gt. 8.5) then
         m = 8.5
      endif

      lnY = c1ss + c2*m + c7T*(8.5-m)**(2.5)
     1      - c3*alog(dist+c4*exp(c5*m)) + c6ssT
     2      + (c1rv-c1ss)*ftype1 + (c6rvT-c6ssT)*ftype1

c     Set sigma
      if ( m .le. 7.0 ) then
         sigma = sig0T - 0.16*m
      else
         sigma = sig0T - 0.16*7.0
      endif

c   write(*,'( 10f10.4)')m,dist,lnY,c1ss,c2,c3,c4,c5,
C    +                          alog(dist+c4*exp(c5*m))

c     Convert units to spectral acceleration in gal
      lnY = lnY + 6.89
      return
      end

c -------------------------------------------------------------------
C *** Spudich et al. (1997) Horizontal for Extensional Regimes ******
c -------------------------------------------------------------------
      subroutine S02_Spudich96 ( m, dist, lnY, sigma, iSite, specT,
     1                   attenName, period1, iflag )
c     This  subroutine calculates the LOG SA for the ave horizontal
c     component using the Spudich (1996) attenuation relation
c     Sigma is sqrt( sigma1**2+sigma2**2) log10 units
c     iSite = 0 for rock, 1 for soil

      implicit none

      integer MAXPER
      parameter (MAXPER=11)
      real dist, m, lnY, sigma, log10Y, period1, twoPi, r
      real period(MAXPER), b2(MAXPER), b3(MAXPER),
     1     b5(MAXPER), b1(MAXPER),
     1     b6(MAXPER), h(MAXPER), sigma0(MAXPER)
      real specT, b1T, b2T, b3T, b5T, b6T, hT, sigma0T
      integer nper, count1, count2, iflag, iSite, i
      character*80 attenName

      data period / 0.00, 0.1, 0.15, 0.20,
     1              0.30, 0.40, 0.50, 0.75, 1.00, 1.5, 2. /

      data b1 / 0.156, 1.772, 1.964, 2.023, 2.030, 2.001, 1.971,
     1          1.922, 1.912, 1.964, 2.068 /
      data b2 / 0.229, 0.327, 0.305, 0.309, 0.334, 0.361, 0.384,
     1          0.425, 0.450, 0.471, 0.471 /
      data b3 / 0.0,  -0.098, -0.099, -0.090, -0.070, -0.052, -0.039,
     1         -0.020, -0.014, -0.019, -0.037/
      data b5 / -0.946, -1.051, -1.009, -0.972, -0.915, -0.879, -0.857,
     1          -0.833, -0.837, -0.879, -0.940/
      data b6 / 0.077, 0.079, 0.127, 0.154, 0.183, 0.198, 0.206, 0.214,
     1          0.214, 0.209, 0.200 /
      data h  / 5.57, 6.27, 7.23, 7.02, 5.94, 4.91, 4.13, 3.07, 2.90,
     1          3.92, 5.85 /
      data sigma0 / 0.216, 0.268, 0.277, 0.286, 0.301, 0.313, 0.323,
     1          0.345, 0.361, 0.387,0.407 /
      twoPi = 2. * 3.1415926

C Find the requested spectral period and corresponding coefficients
      nPer = 11

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1 = period(1)
         b1T     = b1(1)
         b2T     = b2(1)
         b3T     = b3(1)
         b5T     = b5(1)
         b6T     = b6(1)
         hT      = h(1)
         sigma0T = sigma0(1)
         goto 1011
      elseif (specT .ne. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

      write (*,*)
      write (*,*) 'Spudich et al. (1997) Horizontal atttenuation model'
      write (*,*) 'is not defined for a spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),b1(count1),b1(count2),
     +             specT,b1T,iflag)
      call S24_interp (period(count1),period(count2),b2(count1),b2(count2),
     +             specT,b2T,iflag)
      call S24_interp (period(count1),period(count2),b3(count1),b3(count2),
     +             specT,b3T,iflag)
      call S24_interp (period(count1),period(count2),b5(count1),b5(count2),
     +             specT,b5T,iflag)
      call S24_interp (period(count1),period(count2),b6(count1),b6(count2),
     +             specT,b6T,iflag)
      call S24_interp (period(count1),period(count2),h(count1),h(count2),
     +             specT,hT,iflag)
      call S24_interp (period(count1),period(count2),sigma0(count1),sigma0(count2),
     +             specT,sigma0T,iflag)

 1011 period1 = specT

c     Set atten name
      if ( iSite .eq. 0 ) then
        attenName = 'Spudich et al. (1996), Rock'
      elseif ( iSite .eq. 1 ) then
        attenName = 'Spudich et al. (1996), Soil'
      else
        write (*,'( 2x,''iSite is bad in Spudich 96'')')
        write (*,'( 2x,''iSite ='',i5)') iSite
        stop 99
      endif

      r = sqrt( dist**2 + hT**2 )
      log10Y = b1T
     1         + b2T*(m-6) + b3T*(m-6)**2
     1         + b5T*alog10(r)
     1         + b6T*iSite

c     Convert units spectral acceleration in gal
      if ( specT .eq. 0.0 ) then
        lnY = log10Y * alog(10.) + 6.89
      else
        lnY = log10Y * alog(10.) + alog( twoPi / specT )
      endif

c     Set standard error
      sigma = sigma0T * alog(10.)

      return
      end

c -------------------------------------------------------------------
C *** Youngs et al. (1993) Horizontal Rock for Subduction Zones *****
c -------------------------------------------------------------------

      subroutine S02_youngs93 ( mag, rupDist, lnY, sigma, attenName, period,
     1           specT, flag,iflag )

      implicit none

      real mag, rupDist, lnY, sigma, period, c1, c2, c4, c5, c8,
     1     c9, c10, c11
      character*80 attenName
      real flag
      real c3(10), c6(10), c7(10), period1(10)
      real specT, c3T, c6T, c7T
      integer nper, count1, count2, iflag, i
      data period1 / 0.0, 0.067, 0.1, 0.2, 0.3, 0.4, 0.5, 1.0, 2.0,
     1               3.0 /
      data c3 /2.5526, 2.72782, 2.654613, 2.527905, 2.453786, 2.4011998,
     1         2.360407, 2.2337, 2.10699, 2.031874 /
      data c6 / 0, 1.300967, 1.18809, 0.722043, 0.245977, -0.1147,
     1         -0.39985, -1.7364, -3.32764, -4.51081 /
      data c7 / 0, -0.00018, -0.0011, -0.0027, -0.00363, -0.00429,
     1           -0.00481, -0.0064, -0.00799, -0.00893 /

      attenName ='Youngs (1993) subduction - rock'

C Find the requested spectral period and corresponding coefficients
      nPer = 10

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period  = period1(1)
         c3T     = c3(1)
         c6T     = c6(1)
         c7T     = c7(1)
         goto 1011
      elseif (specT .ne. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period1(i) .and. specT .le. period1(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

      write (*,*)
      write (*,*) 'Youngs et al. (1993) Subduction atttenuation model'
      write (*,*) 'is not defined for a spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period1(count1),period1(count2),c3(count1),c3(count2),
     +             specT,c3T,iflag)
      call S24_interp (period1(count1),period1(count2),c6(count1),c6(count2),
     +             specT,c6T,iflag)
      call S24_interp (period1(count1),period1(count2),c7(count1),c7(count2),
     +             specT,c7T,iflag)

 1011 period = specT

c     Set c1 (flag=0 for mega thrust at 20 km)
c            (flag=1 for intraslab at 55 km)
      if ( flag .eq. 0. ) then
        c1 = 0.3633
      elseif ( flag .eq. 1. ) then
        c1 = 0.9908
      else
        write (*,'( 2x,''bad flag in youngs93'', i5)') flag
        stop 99
      endif
      c2 = 1.4142
      c4 = 1.7818
      c5 = 0.554
      c8 = 3
      c9 = 10.
      c10 = 1.45
      c11 = -0.1

      lnY = c1 + c2*mag - c3T*alog(rupDist+c4*exp(c5*mag))
     1       + c6T + c7T*(c9-mag)**(c8)

      if ( mag .lt. 8.0 ) then
        sigma = c10 + c11*mag
      else
        sigma = c10 + c11*8.0
      endif

c     Convert from g to gal
      lnY = lnY + 6.89

      return
      end

c -------------------------------------------------------------------
C *** Youngs et al. (1997) Horizontal Rock for Subduction Zones *****
c -------------------------------------------------------------------

      subroutine S02_youngs97_rock ( mag, rupDist, lnY, sigma,
     1   attenName, period, specT, ftype, depth, iflag )

c     Youngs et al (1997) -- subduction (Rock)

      implicit none

      real mag, rupDist, lnY, sigma, period, depth, c5
      character*80 attenName
      real ftype
      real c1(12), c2(12), c3(12), c4(12), period1(12)
      real specT, c1T, c2T, c3T, c4T
      integer nper, count1, count2, iflag, i

      data period1 / 0.0, 0.075, 0.1, 0.2, 0.3, 0.4, 0.5, 0.75,
     1               1.0, 1.5, 2.0, 3.0 /
      data c1 / 0.0, 1.275, 1.188, 0.722, 0.246, -0.115, -0.400,
     1          -1.149, -1.736, -2.634, -3.328, -4.511 /
      data c2 / 0.0, 0.0, -.0011, -.0027, -.0036, -.0043, -.0048,
     1          -.0057, -.0064, -.0073, -.0080, -.0089 /
      data c3 / -2.552, -2.707, -2.655, -2.528, -2.454, -2.401,
     1          -2.360, -2.286, -2.234, -2.160, -2.107, -2.033 /
      data c4 / 1.45, 1.45, 1.45, 1.45, 1.45, 1.45, 1.45, 1.45,
     1          1.45, 1.50, 1.55, 1.65 /

C Find the requested spectral period and corresponding coefficients
      nPer = 12

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period  = period1(1)
         c1T     = c1(1)
         c2T     = c2(1)
         c3T     = c3(1)
         c4T     = c4(1)
         goto 1011
      elseif (specT .ne. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period1(i) .and. specT .le. period1(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

      write (*,*)
      write (*,*) 'Youngs et al. (1997) Rock Subduction atttenuation model'
      write (*,*) 'is not defined for a spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period1(count1),period1(count2),c1(count1),c1(count2),
     +             specT,c1T,iflag)
      call S24_interp (period1(count1),period1(count2),c2(count1),c2(count2),
     +             specT,c2T,iflag)
      call S24_interp (period1(count1),period1(count2),c3(count1),c3(count2),
     +             specT,c3T,iflag)
      call S24_interp (period1(count1),period1(count2),c4(count1),c4(count2),
     +             specT,c4T,iflag)

 1011 period = specT

      c5 = -0.1
      attenName ='Youngs (1997) Rock, Subduction'
      period = specT

      lnY = 0.2418 + 1.414*mag + c1T + c2T*(10.-mag)**3 +
     1      c3T*alog(rupDist+1.7818*exp(0.554*mag))
     2      + 0.00607*depth + 0.3846*ftype

      if ( mag .lt. 8.0 ) then
        sigma = c4T + c5*mag
      else
        sigma = c4T + c5*8.0
      endif

c     Convert from g to gal
      lnY = lnY + 6.89

      return
      end

c -------------------------------------------------------------------
C *** Youngs et al. (1997) Horizontal Soil for Subduction Zones *****
c -------------------------------------------------------------------

      subroutine S02_youngs97_soil ( mag, rupDist, lnY, sigma,
     1           attenName, period, specT, ftype, depth, iflag )

c     Youngs et al (1997) -- subduction (Soil)

      implicit none

      real mag, rupDist, lnY, sigma, period, depth, c5
      character*80 attenName
      real ftype,specT, c1T, c2T, c3T, c4T
      integer iflag, count1, count2, nper, i
      real c1(13), c2(13), c3(13), c4(13), period1(13)

      data period1 / 0.0, 0.075, 0.1, 0.2, 0.3, 0.4, 0.5, 0.75,
     1               1.0, 1.5, 2.0, 3.0, 4.0 /
      data c1 / 0.0, 2.400, 2.516, 1.549, 0.793, 0.144, -0.438,
     1         -1.704, -2.870, -5.101, -6.433, -6.672, -7.618 /
      data c2 / 0.0, -0.0019, -0.0019, -0.0019, -.0020, -.0020,
     1   -.0035, -.0048, -.0066, -.0114, -.0164, -.0221, -.0235 /
      data c3 / -2.329, -2.697, -2.697, -2.464, -2.327, -2.230,
     1   -2.140, -1.952, -1.785, -1.470, -1.290, -1.347, -1.272 /
      data c4 / 1.45, 1.45, 1.45, 1.45, 1.45, 1.45, 1.45, 1.45,
     1          1.45, 1.50, 1.55, 1.65, 1.65 /

C Find the requested spectral period and corresponding coefficients
      nPer = 13

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period  = period1(1)
         c1T     = c1(1)
         c2T     = c2(1)
         c3T     = c3(1)
         c4T     = c4(1)
         goto 1011
      elseif (specT .ne. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period1(i) .and. specT .le. period1(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

      write (*,*)
      write (*,*) 'Youngs et al. (1997) Soil Subduction atttenuation model'
      write (*,*) 'is not defined for a spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period1(count1),period1(count2),c1(count1),c1(count2),
     +             specT,c1T,iflag)
      call S24_interp (period1(count1),period1(count2),c2(count1),c2(count2),
     +             specT,c2T,iflag)
      call S24_interp (period1(count1),period1(count2),c3(count1),c3(count2),
     +             specT,c3T,iflag)
      call S24_interp (period1(count1),period1(count2),c4(count1),c4(count2),
     +             specT,c4T,iflag)

 1011 period = specT

      c5 = -0.1
      attenName ='Youngs (1997) Soil, Subduction'

      lnY = -0.6687 + 1.438*mag + c1T + c2T*(10.-mag)**3 +
     1      c3T*alog(rupDist+1.097*exp(0.617*mag))
     2      + 0.00648*depth + 0.3643*ftype

      if ( mag .lt. 8.0 ) then
        sigma = c4T + c5*mag
      else
        sigma = c4T + c5*8.0
      endif

c     Convert from g to gal
      lnY = lnY + 6.89

      return
      end

c ------------------------------------------------------------------
C *** Atkinson and Boore (2003) Horizontal for Subduction Zones ****
c ------------------------------------------------------------------

      subroutine S02_AB03 ( mag, rupdist, lnY, sigma, specT,
     1  attenName, period,iflag, ftype, depth, Sc, Sd, Se)

c     Atkinson and Boore (2003) Horizontal Subduction
C     This relationship has the following constraints:
c          Interface Events -->  M=8.5 for M>8.5
c          Interslab Events -->  M=8.0 for M>8.0
c          Hypocentral depth --> h=100 or h>100
C     Site Classses are as follows:
c          NEHRP B --> Sc=0, Sd=0, Se=0   Vs>760m/s
c          NEHRP C --> Sc=1, Sd=0, Se=0   360<Vs<760
c          NEHRP D --> Sc=0, Sd=1, Se=0   180<Vs<360
c          NEHRP E --> Sc=0, Sd=0, Se=1   Vs<180

      implicit none

      real mag, rupDist, lnY, sigma, period, mag1, depth1, sigT,
     1     sigsT, g
      character*80 attenName
      real ftype, depth, Sc, Sd, Se
      real c1(8), c2(8), c3(8), c4(8), c5(8), c6(8), c7(8)
      real c1s(8), c2s(8), c3s(8), c4s(8), c5s(8), c6s(8), c7s(8)
      real period1(8), sig(8), sigs(8), r, delta, PGArx, sl
      real specT, c1T, c2T, c3T, c4T, c5T, c6T, c7T
      real c1sT, c2sT, c3sT, c4sT, c5sT, c6sT, c7sT
      integer nper, count1, count2, iflag, i

      data period1 / 0.0, 0.04, 0.1, 0.2, 0.4, 1.0, 2.0, 3.0 /
      data c1 /  2.991, 2.8753, 2.7789, 2.6638, 2.5249, 2.1442,
     1          2.1907, 2.301 /
      data c2 / 0.03525, 0.07052, 0.09841, 0.12386, 0.1477,  0.1345,
     1          0.07148, 0.02237 /
      data c3 / 0.00759, 0.01004, 0.00974, 0.00884, 0.00728, 0.00521,
     1          0.00224, 0.00012 /
      data c4 / -0.00206, -0.00278, -0.00287, -0.00280, -0.00235,
     1          -0.00110,  0.00000,  0.00000 /
      data c5 / 0.19, 0.15, 0.15, 0.15, 0.13, 0.10, 0.10, 0.10 /
      data c6 / 0.24, 0.20, 0.23, 0.27, 0.37, 0.30, 0.25, 0.25 /
      data c7 / 0.29, 0.20, 0.20, 0.25, 0.38, 0.55, 0.40, 0.36 /
      data sig / 0.23, 0.26, 0.27, 0.28, 0.29, 0.34, 0.34, 0.36 /
      data c1s / -0.04713,  0.50697,  0.43928, 0.51589, 0.005445,
     1           -1.02133, -2.39234, -3.70012 /
      data c2s / 0.6909, 0.63273, 0.66675, 0.69186, 0.7727, 0.8789,
     1           0.9964, 1.1169 /
      data c3s / 0.01130, 0.01275, 0.01080, 0.00572, 0.00173, 0.00130,
     1          0.00364, 0.00615 /
      data c4s / -0.00202, -0.00234, -0.00219, -0.00192, -0.00178,
     1          -0.00173,  -0.00118, -0.00045 /
      data c5s / 0.19, 0.15, 0.15, 0.15, 0.13, 0.10, 0.10, 0.10 /
      data c6s / 0.24, 0.20, 0.23, 0.27, 0.37, 0.30, 0.25, 0.25 /
      data c7s / 0.29, 0.20, 0.20, 0.25, 0.38, 0.55, 0.40, 0.36 /
      data sigs / 0.27, 0.25, 0.28, 0.28, 0.28, 0.29, 0.30, 0.30 /

C Find the requested spectral period and corresponding coefficients
      nPer = 8

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period  = period1(1)
         c1T     = c1(1)
         c2T     = c2(1)
         c3T     = c3(1)
         c4T     = c4(1)
         c5T     = c5(1)
         c6T     = c6(1)
         c7T     = c7(1)
         sigT    = sig(1)
         c1sT     = c1s(1)
         c2sT     = c2s(1)
         c3sT     = c3s(1)
         c4sT     = c4s(1)
         c5sT     = c5s(1)
         c6sT     = c6s(1)
         c7sT     = c7s(1)
         sigsT    = sigs(1)
         goto 1011
      elseif (specT .ne. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period1(i) .and. specT .le. period1(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

      write (*,*)
      write (*,*) 'Atkinson and Boore (2003) Subduction atttenuation model'
      write (*,*) 'is not defined for a spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period1(count1),period1(count2),c1(count1),c1(count2),
     +             specT,c1T,iflag)
      call S24_interp (period1(count1),period1(count2),c2(count1),c2(count2),
     +             specT,c2T,iflag)
      call S24_interp (period1(count1),period1(count2),c3(count1),c3(count2),
     +             specT,c3T,iflag)
      call S24_interp (period1(count1),period1(count2),c4(count1),c4(count2),
     +             specT,c4T,iflag)
      call S24_interp (period1(count1),period1(count2),c5(count1),c5(count2),
     +             specT,c5T,iflag)
      call S24_interp (period1(count1),period1(count2),c6(count1),c6(count2),
     +             specT,c6T,iflag)
      call S24_interp (period1(count1),period1(count2),c7(count1),c7(count2),
     +             specT,c7T,iflag)
      call S24_interp (period1(count1),period1(count2),sig(count1),sig(count2),
     +             specT,sigT,iflag)

      call S24_interp (period1(count1),period1(count2),c1s(count1),c1s(count2),
     +             specT,c1sT,iflag)
      call S24_interp (period1(count1),period1(count2),c2s(count1),c2s(count2),
     +             specT,c2sT,iflag)
      call S24_interp (period1(count1),period1(count2),c3s(count1),c3s(count2),
     +             specT,c3sT,iflag)
      call S24_interp (period1(count1),period1(count2),c4s(count1),c4s(count2),
     +             specT,c4sT,iflag)
      call S24_interp (period1(count1),period1(count2),c5s(count1),c5s(count2),
     +             specT,c5sT,iflag)
      call S24_interp (period1(count1),period1(count2),c6s(count1),c6s(count2),
     +             specT,c6sT,iflag)
      call S24_interp (period1(count1),period1(count2),c7s(count1),c7s(count2),
     +             specT,c7sT,iflag)
      call S24_interp (period1(count1),period1(count2),sigs(count1),sigs(count2),
     +             specT,sigsT,iflag)

 1011 period = specT

c     First check the upper magnitude limit
      if (ftype .eq. 0) then
         if (mag .gt. 8.5) then
             mag1 = 8.5
         else
             mag1 = mag
         endif
      elseif (ftype .eq. 1) then
         if (mag .gt. 8.0) then
             mag1 = 8.0
         else
             mag1 = mag
         endif
      endif

C     Now check for the maximum depth
      if (depth .gt. 100.0) then
          depth1 = 100.0
      else
          depth1 = depth
      endif

C    Set the attenuation model name based on site class
      if (Se .eq. 1.0) then
         attenName ='Atkinson and Boore (2003) Subduction, NEHRP-E'
      elseif (Sd .eq. 1.0) then
         attenName ='Atkinson and Boore (2003) Subduction, NEHRP-D'
      elseif (Sc .eq. 1.0) then
         attenName ='Atkinson and Boore (2003) Subduction, NEHRP-C'
      elseif (Sc .eq. 0.0 .and. Sd .eq. 0.0 .and. Se .eq. 0.0) then
         attenName ='Atkinson and Boore (2003) Subduction, NEHRP-B'
      endif

      period = specT
      delta = 0.00724*10.0**(0.507*mag1)
      r = sqrt(rupdist*rupdist + delta*delta)

C     Compute the ground motion for Subduction Interface Events (ftype=0)
      if (ftype .eq. 0.0) then
         g = 10.0**(1.2-0.18*mag1)
         pgarx = c1(1) + c2(1)*mag1 + c3(1)*depth1 + c4(1)*r -g*alog10(r)
         pgarx = 10.0**(pgarx)
c     Now set Sl function based on PGArx and period
         if (specT .ge. 1.0) then
            sl = 1.0
         elseif (specT .lt. 1.0 .and. specT .gt. 0.5) then
            if (pgarx .le. 100.0) then
               sl = 1.0
            elseif (pgarx .gt. 100.0 .and. pgarx .lt. 500.0) then
               sl = 1.0 - ((1./specT) - 1.)*(pgarx-100.)/400.
            elseif (pgarx .ge. 500.0) then
               sl = 1.0 - ((1./specT) - 1.)
            endif
         elseif (specT .le. 0.5) then
            if (pgarx .le. 100.0) then
               sl = 1.0
            elseif (pgarx .gt. 100.0 .and. pgarx .lt. 500.0) then
               sl = 1.0 - (pgarx-100.)/400.
            elseif (pgarx .ge. 500.0) then
                sl = 0.0
            endif
         endif

         lnY = c1T + c2T*mag1 + c3T*depth1 + c4T*r - g*alog10(r) +
     1         c5T*sl*Sc + c6T*sl*Sd + c7T*sl*Se
         sigma = sigT

C     Compute the ground motion for Subduction Intraslab Events (ftype=1)
      elseif (ftype .eq. 1.0) then
         g = 10.0**(0.301-0.01*mag1)
         pgarx = c1s(1) + c2s(1)*mag1 + c3s(1)*depth1 + c4s(1)*r -g*alog10(r)
         pgarx = 10.0**(pgarx)
c     Now set Sl function based on PGArx and period
         if (specT .ge. 1.0) then
            sl = 1.0
         elseif (specT .lt. 1.0 .and. specT .gt. 0.5) then
            if (pgarx .le. 100.0) then
               sl = 1.0
            elseif (pgarx .gt. 100.0 .and. pgarx .lt. 500.0) then
               sl = 1.0 - ((1./specT) - 1.)*(pgarx-100.)/400.
            elseif (pgarx .ge. 500.0) then
               sl = 1.0 - ((1./specT) - 1.)
            endif
         elseif (specT .le. 0.5) then
            if (pgarx .le. 100.0) then
               sl = 1.0
            elseif (pgarx .gt. 100.0 .and. pgarx .lt. 500.0) then
               sl = 1.0 - (pgarx-100.)/400.
            elseif (pgarx .ge. 500.0) then
                sl = 0.0
            endif
         endif

         lnY = c1sT + c2sT*mag1 + c3sT*depth1 + c4sT*r - g*alog10(r) +
     1         c5sT*sl*Sc + c6sT*sl*Sd + c7sT*sl*Se
         sigma = sigsT

      else
         write (*,*) 'Wrong Ftype for Atkinson and Boore (2003)'
         write (*,*) 'attenuation relationship. Check fault file.'
         stop 99
      endif

C     Now convert to Ln Units in gals.
      lnY = alog(10.0)*lnY
      sigma = alog(10.0)*sigma
      return
      end

c --------------------------------------------------------------------------
C *** Atkinson and Boore (2003) Horizontal for Cascadia Subduction Zone ****
c --------------------------------------------------------------------------

      subroutine S02_AB03Cas ( mag, rupdist, lnY, sigma, specT,
     1  attenName, period,iflag, ftype, depth, Sc, Sd, Se)

c     Atkinson and Boore (2003) Horizontal Subduction
C     This relationship has the following constraints:
c          Interface Events -->  M=8.5 for M>8.5
c          Interslab Events -->  M=8.0 for M>8.0
c          Hypocentral depth --> h=100 or h>100
C     Site Classses are as follows:
c          NEHRP B --> Sc=0, Sd=0, Se=0   Vs>760m/s
c          NEHRP C --> Sc=1, Sd=0, Se=0   360<Vs<760
c          NEHRP D --> Sc=0, Sd=1, Se=0   180<Vs<360
c          NEHRP E --> Sc=0, Sd=0, Se=1   Vs<180
C     This version is specifically for the Cascadia Subduction Source Zone

      implicit none

      real mag, rupDist, lnY, sigma, period, mag1, depth1, g
      character*80 attenName
      real ftype, depth, Sc, Sd, Se, sigT, sigsT
      real c1(8), c2(8), c3(8), c4(8), c5(8), c6(8), c7(8)
      real c1s(8), c2s(8), c3s(8), c4s(8), c5s(8), c6s(8), c7s(8)
      real period1(8), sig(8), sigs(8), r, delta, PGArx, sl
      real specT, c1T, c2T, c3T, c4T, c5T, c6T, c7T
      real c1sT, c2sT, c3sT, c4sT, c5sT, c6sT, c7sT
      integer nper, count1, count2, iflag, i

      data period1 / 0.0, 0.04, 0.1, 0.2, 0.4, 1.0, 2.0, 3.0 /
      data c1 /  2.79, 2.60, 2.50, 2.54, 2.50, 2.18,
     1           2.33, 2.36 /
      data c2 / 0.03525, 0.07052, 0.09841, 0.12386, 0.1477,  0.1345,
     1          0.07148, 0.02237 /
      data c3 / 0.00759, 0.01004, 0.00974, 0.00884, 0.00728, 0.00521,
     1          0.00224, 0.00012 /
      data c4 / -0.00206, -0.00278, -0.00287, -0.00280, -0.00235,
     1          -0.00110,  0.00000,  0.00000 /
      data c5 / 0.19, 0.15, 0.15, 0.15, 0.13, 0.10, 0.10, 0.10 /
      data c6 / 0.24, 0.20, 0.23, 0.27, 0.37, 0.30, 0.25, 0.25 /
      data c7 / 0.29, 0.20, 0.20, 0.25, 0.38, 0.55, 0.40, 0.36 /
      data sig / 0.23, 0.26, 0.27, 0.28, 0.29, 0.34, 0.34, 0.36 /

      data c1s / -0.25,  0.23,  0.16, 0.40, -0.01,
     1           -0.98, -2.25, -3.64 /
      data c2s / 0.6909, 0.63273, 0.66675, 0.69186, 0.7727, 0.8789,
     1           0.9964, 1.1169 /
      data c3s / 0.01130, 0.01275, 0.01080, 0.00572, 0.00173, 0.00130,
     1          0.00364, 0.00615 /
      data c4s / -0.00202, -0.00234, -0.00219, -0.00192, -0.00178,
     1          -0.00173,  -0.00118, -0.00045 /
      data c5s / 0.19, 0.15, 0.15, 0.15, 0.13, 0.10, 0.10, 0.10 /
      data c6s / 0.24, 0.20, 0.23, 0.27, 0.37, 0.30, 0.25, 0.25 /
      data c7s / 0.29, 0.20, 0.20, 0.25, 0.38, 0.55, 0.40, 0.36 /
      data sigs / 0.27, 0.25, 0.28, 0.28, 0.28, 0.29, 0.30, 0.30 /

C Find the requested spectral period and corresponding coefficients
      nPer = 8

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period  = period1(1)
         c1T     = c1(1)
         c2T     = c2(1)
         c3T     = c3(1)
         c4T     = c4(1)
         c5T     = c5(1)
         c6T     = c6(1)
         c7T     = c7(1)
         sigT    = sig(1)
         c1sT     = c1s(1)
         c2sT     = c2s(1)
         c3sT     = c3s(1)
         c4sT     = c4s(1)
         c5sT     = c5s(1)
         c6sT     = c6s(1)
         c7sT     = c7s(1)
         sigsT    = sigs(1)
         goto 1011
      elseif (specT .ne. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period1(i) .and. specT .le. period1(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

      write (*,*)
      write (*,*) 'Atkinson and Boore (2003) Cascadia'
      write (*,*) 'Subduction attenuation model'
      write (*,*) 'is not defined for a spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period1(count1),period1(count2),c1(count1),c1(count2),
     +             specT,c1T,iflag)
      call S24_interp (period1(count1),period1(count2),c2(count1),c2(count2),
     +             specT,c2T,iflag)
      call S24_interp (period1(count1),period1(count2),c3(count1),c3(count2),
     +             specT,c3T,iflag)
      call S24_interp (period1(count1),period1(count2),c4(count1),c4(count2),
     +             specT,c4T,iflag)
      call S24_interp (period1(count1),period1(count2),c5(count1),c5(count2),
     +             specT,c5T,iflag)
      call S24_interp (period1(count1),period1(count2),c6(count1),c6(count2),
     +             specT,c6T,iflag)
      call S24_interp (period1(count1),period1(count2),c7(count1),c7(count2),
     +             specT,c7T,iflag)
      call S24_interp (period1(count1),period1(count2),sig(count1),sig(count2),
     +             specT,sigT,iflag)

      call S24_interp (period1(count1),period1(count2),c1s(count1),c1s(count2),
     +             specT,c1sT,iflag)
      call S24_interp (period1(count1),period1(count2),c2s(count1),c2s(count2),
     +             specT,c2sT,iflag)
      call S24_interp (period1(count1),period1(count2),c3s(count1),c3s(count2),
     +             specT,c3sT,iflag)
      call S24_interp (period1(count1),period1(count2),c4s(count1),c4s(count2),
     +             specT,c4sT,iflag)
      call S24_interp (period1(count1),period1(count2),c5s(count1),c5s(count2),
     +             specT,c5sT,iflag)
      call S24_interp (period1(count1),period1(count2),c6s(count1),c6s(count2),
     +             specT,c6sT,iflag)
      call S24_interp (period1(count1),period1(count2),c7s(count1),c7s(count2),
     +             specT,c7sT,iflag)
      call S24_interp (period1(count1),period1(count2),sigs(count1),sigs(count2),
     +             specT,sigsT,iflag)

 1011 period = specT

c     First check the upper magnitude limit
      if (ftype .eq. 0) then
         if (mag .gt. 8.5) then
             mag1 = 8.5
         else
             mag1 = mag
         endif
      elseif (ftype .eq. 1) then
         if (mag .gt. 8.0) then
             mag1 = 8.0
         else
             mag1 = mag
         endif
      endif

C     Now check for the maximum depth
      if (depth .gt. 100.0) then
          depth1 = 100.0
      else
          depth1 = depth
      endif

C    Set the attenuation model name based on site class
      if (Se .eq. 1.0) then
         attenName ='Atkinson and Boore (2003) Cascadia-Sub, NEHRP-E'
      elseif (Sd .eq. 1.0) then
         attenName ='Atkinson and Boore (2003) Cascadia-Sub, NEHRP-D'
      elseif (Sc .eq. 1.0) then
         attenName ='Atkinson and Boore (2003) Cascadia-Sub, NEHRP-C'
      elseif (Sc .eq. 0.0 .and. Sd .eq. 0.0 .and. Se .eq. 0.0) then
         attenName ='Atkinson and Boore (2003) Cascadia-Sub, NEHRP-B'
      endif

      period = specT
      delta = 0.00724*10.0**(0.507*mag1)
      r = sqrt(rupdist*rupdist + delta*delta)

C     Compute the ground motion for Subduction Interface Events (ftype=0)
      if (ftype .eq. 0.0) then
         g = 10.0**(1.2-0.18*mag1)
         pgarx = c1(1) + c2(1)*mag1 + c3(1)*depth1 + c4(1)*r -g*alog10(r)
         pgarx = 10.0**(pgarx)
c     Now set Sl function based on PGArx and period
         if (specT .ge. 1.0) then
            sl = 1.0
         elseif (specT .lt. 1.0 .and. specT .gt. 0.5) then
            if (pgarx .le. 100.0) then
               sl = 1.0
            elseif (pgarx .gt. 100.0 .and. pgarx .lt. 500.0) then
               sl = 1.0 - ((1./specT) - 1.)*(pgarx-100.)/400.
            elseif (pgarx .ge. 500.0) then
               sl = 1.0 - ((1./specT) - 1.)
            endif
         elseif (specT .le. 0.5) then
            if (pgarx .le. 100.0) then
               sl = 1.0
            elseif (pgarx .gt. 100.0 .and. pgarx .lt. 500.0) then
               sl = 1.0 - (pgarx-100.)/400.
            elseif (pgarx .ge. 500.0) then
                sl = 0.0
            endif
         endif

         lnY = c1T + c2T*mag1 + c3T*depth1 + c4T*r - g*alog10(r) +
     1         c5T*sl*Sc + c6T*sl*Sd + c7T*sl*Se
         sigma = sigT

C     Compute the ground motion for Subduction Intraslab Events (ftype=1)
      elseif (ftype .eq. 1.0) then
         g = 10.0**(0.301-0.01*mag1)
         pgarx = c1s(1) + c2s(1)*mag1 + c3s(1)*depth1 + c4s(1)*r -g*alog10(r)
         pgarx = 10.0**(pgarx)
c     Now set Sl function based on PGArx and period
         if (specT .ge. 1.0) then
            sl = 1.0
         elseif (specT .lt. 1.0 .and. specT .gt. 0.5) then
            if (pgarx .le. 100.0) then
               sl = 1.0
            elseif (pgarx .gt. 100.0 .and. pgarx .lt. 500.0) then
               sl = 1.0 - ((1./specT) - 1.)*(pgarx-100.)/400.
            elseif (pgarx .ge. 500.0) then
               sl = 1.0 - ((1./specT) - 1.)
            endif
         elseif (specT .le. 0.5) then
            if (pgarx .le. 100.0) then
               sl = 1.0
            elseif (pgarx .gt. 100.0 .and. pgarx .lt. 500.0) then
               sl = 1.0 - (pgarx-100.)/400.
            elseif (pgarx .ge. 500.0) then
                sl = 0.0
            endif
         endif

         lnY = c1sT + c2sT*mag1 + c3sT*depth1 + c4sT*r - g*alog10(r) +
     1         c5sT*sl*Sc + c6sT*sl*Sd + c7sT*sl*Se
         sigma = sigsT

      else
         write (*,*) 'Wrong Ftype for Atkinson and Boore (2003)'
         write (*,*) 'attenuation relationship. Check fault file.'
         stop 99
      endif

C     Now convert to Ln Units in gals.
      lnY = alog(10.0)*lnY
      sigma = alog(10.0)*sigma

      return
      end

c --------------------------------------------------------------------------
C *** Atkinson and Boore (2003) Horizontal for Japanese Subduction Zone ****
c --------------------------------------------------------------------------

      subroutine S02_AB03Jap ( mag, rupdist, lnY, sigma, specT,
     1  attenName, period,iflag, ftype, depth, Sc, Sd, Se)

c     Atkinson and Boore (2003) Horizontal Subduction
C     This relationship has the following constraints:
c          Interface Events -->  M=8.5 for M>8.5
c          Interslab Events -->  M=8.0 for M>8.0
c          Hypocentral depth --> h=100 or h>100
C     Site Classses are as follows:
c          NEHRP B --> Sc=0, Sd=0, Se=0   Vs>760m/s
c          NEHRP C --> Sc=1, Sd=0, Se=0   360<Vs<760
c          NEHRP D --> Sc=0, Sd=1, Se=0   180<Vs<360
c          NEHRP E --> Sc=0, Sd=0, Se=1   Vs<180
C     This version is specifically for the Japanese Subduction Source Zone

      implicit none

      real mag, rupDist, lnY, sigma, period, mag1, depth1, sigT, sigsT
      character*80 attenName
      real ftype, depth, Sc, Sd, Se, g
      real c1(8), c2(8), c3(8), c4(8), c5(8), c6(8), c7(8)
      real c1s(8), c2s(8), c3s(8), c4s(8), c5s(8), c6s(8), c7s(8)
      real period1(8), sig(8), sigs(8), r, delta, PGArx, sl
      real specT, c1T, c2T, c3T, c4T, c5T, c6T, c7T
      real c1sT, c2sT, c3sT, c4sT, c5sT, c6sT, c7sT
      integer nper, count1, count2, iflag, i

      data period1 / 0.0, 0.04, 0.1, 0.2, 0.4, 1.0, 2.0, 3.0 /
      data c1 /  3.14, 3.05, 2.95, 2.84, 2.58, 2.18,
     1           2.14, 2.27 /
      data c2 / 0.03525, 0.07052, 0.09841, 0.12386, 0.1477,  0.1345,
     1          0.07148, 0.02237 /
      data c3 / 0.00759, 0.01004, 0.00974, 0.00884, 0.00728, 0.00521,
     1          0.00224, 0.00012 /
      data c4 / -0.00206, -0.00278, -0.00287, -0.00280, -0.00235,
     1          -0.00110,  0.00000,  0.00000 /
      data c5 / 0.19, 0.15, 0.15, 0.15, 0.13, 0.10, 0.10, 0.10 /
      data c6 / 0.24, 0.20, 0.23, 0.27, 0.37, 0.30, 0.25, 0.25 /
      data c7 / 0.29, 0.20, 0.20, 0.25, 0.38, 0.55, 0.40, 0.36 /
      data sig / 0.23, 0.26, 0.27, 0.28, 0.29, 0.34, 0.34, 0.36 /

      data c1s /  0.10,  0.68,  0.61, 0.70, 0.07,
     1           -0.98, -2.44, -3.73 /
      data c2s / 0.6909, 0.63273, 0.66675, 0.69186, 0.7727, 0.8789,
     1           0.9964, 1.1169 /
      data c3s / 0.01130, 0.01275, 0.01080, 0.00572, 0.00173, 0.00130,
     1          0.00364, 0.00615 /
      data c4s / -0.00202, -0.00234, -0.00219, -0.00192, -0.00178,
     1          -0.00173,  -0.00118, -0.00045 /
      data c5s / 0.19, 0.15, 0.15, 0.15, 0.13, 0.10, 0.10, 0.10 /
      data c6s / 0.24, 0.20, 0.23, 0.27, 0.37, 0.30, 0.25, 0.25 /
      data c7s / 0.29, 0.20, 0.20, 0.25, 0.38, 0.55, 0.40, 0.36 /
      data sigs / 0.27, 0.25, 0.28, 0.28, 0.28, 0.29, 0.30, 0.30 /

C Find the requested spectral period and corresponding coefficients
      nPer = 8

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period  = period1(1)
         c1T     = c1(1)
         c2T     = c2(1)
         c3T     = c3(1)
         c4T     = c4(1)
         c5T     = c5(1)
         c6T     = c6(1)
         c7T     = c7(1)
         sigT    = sig(1)
         c1sT     = c1s(1)
         c2sT     = c2s(1)
         c3sT     = c3s(1)
         c4sT     = c4s(1)
         c5sT     = c5s(1)
         c6sT     = c6s(1)
         c7sT     = c7s(1)
         sigsT    = sigs(1)
         goto 1011
      elseif (specT .ne. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period1(i) .and. specT .le. period1(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

      write (*,*)
      write (*,*) 'Atkinson and Boore (2003) Cascadia'
      write (*,*) 'Subduction attenuation model'
      write (*,*) 'is not defined for a spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period1(count1),period1(count2),c1(count1),c1(count2),
     +             specT,c1T,iflag)
      call S24_interp (period1(count1),period1(count2),c2(count1),c2(count2),
     +             specT,c2T,iflag)
      call S24_interp (period1(count1),period1(count2),c3(count1),c3(count2),
     +             specT,c3T,iflag)
      call S24_interp (period1(count1),period1(count2),c4(count1),c4(count2),
     +             specT,c4T,iflag)
      call S24_interp (period1(count1),period1(count2),c5(count1),c5(count2),
     +             specT,c5T,iflag)
      call S24_interp (period1(count1),period1(count2),c6(count1),c6(count2),
     +             specT,c6T,iflag)
      call S24_interp (period1(count1),period1(count2),c7(count1),c7(count2),
     +             specT,c7T,iflag)
      call S24_interp (period1(count1),period1(count2),sig(count1),sig(count2),
     +             specT,sigT,iflag)

      call S24_interp (period1(count1),period1(count2),c1s(count1),c1s(count2),
     +             specT,c1sT,iflag)
      call S24_interp (period1(count1),period1(count2),c2s(count1),c2s(count2),
     +             specT,c2sT,iflag)
      call S24_interp (period1(count1),period1(count2),c3s(count1),c3s(count2),
     +             specT,c3sT,iflag)
      call S24_interp (period1(count1),period1(count2),c4s(count1),c4s(count2),
     +             specT,c4sT,iflag)
      call S24_interp (period1(count1),period1(count2),c5s(count1),c5s(count2),
     +             specT,c5sT,iflag)
      call S24_interp (period1(count1),period1(count2),c6s(count1),c6s(count2),
     +             specT,c6sT,iflag)
      call S24_interp (period1(count1),period1(count2),c7s(count1),c7s(count2),
     +             specT,c7sT,iflag)
      call S24_interp (period1(count1),period1(count2),sigs(count1),sigs(count2),
     +             specT,sigsT,iflag)

 1011 period = specT

c     First check the upper magnitude limit
      if (ftype .eq. 0) then
         if (mag .gt. 8.5) then
             mag1 = 8.5
         else
             mag1 = mag
         endif
      elseif (ftype .eq. 1) then
         if (mag .gt. 8.0) then
             mag1 = 8.0
         else
             mag1 = mag
         endif
      endif

C     Now check for the maximum depth
      if (depth .gt. 100.0) then
          depth1 = 100.0
      else
          depth1 = depth
      endif

C    Set the attenuation model name based on site class
      if (Se .eq. 1.0) then
         attenName ='Atkinson and Boore (2003) Japan-Sub, NEHRP-E'
      elseif (Sd .eq. 1.0) then
         attenName ='Atkinson and Boore (2003) Japan-Sub, NEHRP-D'
      elseif (Sc .eq. 1.0) then
         attenName ='Atkinson and Boore (2003) Japan-Sub, NEHRP-C'
      elseif (Sc .eq. 0.0 .and. Sd .eq. 0.0 .and. Se .eq. 0.0) then
         attenName ='Atkinson and Boore (2003) Japan-Sub, NEHRP-B'
      endif

      period = specT
      delta = 0.00724*10.0**(0.507*mag1)
      r = sqrt(rupdist*rupdist + delta*delta)

C     Compute the ground motion for Subduction Interface Events (ftype=0)
      if (ftype .eq. 0.0) then
         g = 10.0**(1.2-0.18*mag1)
         pgarx = c1(1) + c2(1)*mag1 + c3(1)*depth1 + c4(1)*r -g*alog10(r)
         pgarx = 10.0**(pgarx)
c     Now set Sl function based on PGArx and period
         if (specT .ge. 1.0) then
            sl = 1.0
         elseif (specT .lt. 1.0 .and. specT .gt. 0.5) then
            if (pgarx .le. 100.0) then
               sl = 1.0
            elseif (pgarx .gt. 100.0 .and. pgarx .lt. 500.0) then
               sl = 1.0 - ((1./specT) - 1.)*(pgarx-100.)/400.
            elseif (pgarx .ge. 500.0) then
               sl = 1.0 - ((1./specT) - 1.)
            endif
         elseif (specT .le. 0.5) then
            if (pgarx .le. 100.0) then
               sl = 1.0
            elseif (pgarx .gt. 100.0 .and. pgarx .lt. 500.0) then
               sl = 1.0 - (pgarx-100.)/400.
            elseif (pgarx .ge. 500.0) then
                sl = 0.0
            endif
         endif

         lnY = c1T + c2T*mag1 + c3T*depth1 + c4T*r - g*alog10(r) +
     1         c5T*sl*Sc + c6T*sl*Sd + c7T*sl*Se
         sigma = sigT

C     Compute the ground motion for Subduction Intraslab Events (ftype=1)
      elseif (ftype .eq. 1.0) then
         g = 10.0**(0.301-0.01*mag1)
         pgarx = c1s(1) + c2s(1)*mag1 + c3s(1)*depth1 + c4s(1)*r -g*alog10(r)
         pgarx = 10.0**(pgarx)
c     Now set Sl function based on PGArx and period
         if (specT .ge. 1.0) then
            sl = 1.0
         elseif (specT .lt. 1.0 .and. specT .gt. 0.5) then
            if (pgarx .le. 100.0) then
               sl = 1.0
            elseif (pgarx .gt. 100.0 .and. pgarx .lt. 500.0) then
               sl = 1.0 - ((1./specT) - 1.)*(pgarx-100.)/400.
            elseif (pgarx .ge. 500.0) then
               sl = 1.0 - ((1./specT) - 1.)
            endif
         elseif (specT .le. 0.5) then
            if (pgarx .le. 100.0) then
               sl = 1.0
            elseif (pgarx .gt. 100.0 .and. pgarx .lt. 500.0) then
               sl = 1.0 - (pgarx-100.)/400.
            elseif (pgarx .ge. 500.0) then
                sl = 0.0
            endif
         endif

         lnY = c1sT + c2sT*mag1 + c3sT*depth1 + c4sT*r - g*alog10(r) +
     1         c5sT*sl*Sc + c6sT*sl*Sd + c7sT*sl*Se
         sigma = sigsT

      else
         write (*,*) 'Wrong Ftype for Atkinson and Boore (2003)'
         write (*,*) 'attenuation relationship. Check fault file.'
         stop 99
      endif

C     Now convert to Ln Units in gals.
      lnY = alog(10.0)*lnY
      sigma = alog(10.0)*sigma

      return
      end

c -------------------------------------------------------------------
C *** Gregor et al. (2002) Horizontal Cascadia Interface, Rock ******
c -------------------------------------------------------------------

      subroutine S02_Gregor02CasR ( mag, rupdist, lnY, sigma, specT,
     1                  attenName, period,iflag )

C     This relationship is constrained to a limited magnitude range of:
c             8.0 < M < 9.0
c     This magnitude range is checked before calling this subroutine.

      implicit none

      real mag, rupDist, lnY, sigma, period
      character*80 attenName
      real c1(25), c2(25), c3(25), c4(25), c5(25), c6(25)
      real period1(25), sig(25)
      real specT, c1T, c2T, c3T, c4T, c5T, c6T, sigT
      integer nper, count1, count2, iflag, i

      data period1 / 0.000, 0.020, 0.025, 0.032, 0.040, 0.050, 0.056,
     1               0.063, 0.071, 0.083, 0.100, 0.125, 0.143, 0.167,
     2               0.200, 0.250, 0.333, 0.400, 0.500, 0.769, 1.000,
     3               1.667, 2.000, 2.500, 5.000 /
      data c1 / 21.0686, 21.072, 21.152, 21.366, 17.525, 19.347,
     1          20.774,  21.331, 24.221, 24.95,  30.005, 39.719,
     2          43.414,  39.579, 39.345, 37.69,  34.787, 33.393,
     3          29.159,  15.279,  6.528,  7.467,  8.657,  6.637,
     4           8.013 /
      data c2 / -1.7712, -1.772, -1.779, -1.797, -1.339, -1.519,
     1          -1.625,  -1.672, -1.924, -1.979, -2.349, -3.09,
     2          -3.385,  -2.957, -3.087, -2.96,  -2.899, -2.776,
     3          -2.424,  -1.22,  -0.406, -0.676, -0.851, -0.651,
     4          -0.943 /
      data c3 / -5.0631, -5.0529, -5.0663, -5.1036, -4.8602,
     1          -4.9731, -5.1875, -5.2561, -5.625,  -5.6696,
     2          -6.3862, -7.8541, -8.3122, -7.9723, -7.6002,
     3          -7.379,  -6.7855, -6.9595, -6.2114, -4.324,
     4          -3.1991, -2.6465, -2.7398, -2.3124, -2.4087 /
      data c4 / 0.4153, 0.4142, 0.4154, 0.4187, 0.3868,
     1          0.396,  0.4118, 0.4173, 0.4478, 0.4493,
     2          0.5009, 0.6161, 0.6513, 0.6139, 0.5972,
     3          0.5842, 0.5616, 0.5863, 0.5216, 0.3618,
     4          0.2589, 0.2193, 0.2339, 0.1879, 0.2154 /
      data c5 / 4.2, 4.2, 4.2, 4.2, 4.2,
     1          4.2, 4.3, 4.3, 4.4, 4.4,
     2          4.7, 5.1, 5.2, 5.2, 5.1,
     3          5.1, 4.9, 4.9, 4.7, 3.9,
     4          3.2, 2.8, 2.8, 2.8, 2.3 /
      data c6 / 0.0017,  0.0025,  0.0023,  0.0017, -0.0318,
     1         -0.0155, -0.0155, -0.0146, -0.0071, -0.0018,
     2         -0.0019, -0.0064, -0.0001, -0.0264,  0.006,
     3         -0.0023,  0.0256, -0.0039,  0.0161, -0.0011,
     4         -0.0225,  0.0416,  0.037,   0.0364,  0.0647 /
      data sig / 0.724,  0.7195, 0.7235, 0.7221, 0.6969,
     1           0.7086, 0.7215, 0.7302, 0.7326, 0.7815,
     2           0.7954, 0.8605, 0.8544, 0.8478, 0.8679,
     3           0.8444, 0.8776, 0.8801, 0.8039, 0.8295,
     4           0.7567, 0.6943, 0.6305, 0.6657, 0.773  /

C Find the requested spectral period and corresponding coefficients
      nPer = 25

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period  = period1(1)
         c1T     = c1(1)
         c2T     = c2(1)
         c3T     = c3(1)
         c4T     = c4(1)
         c5T     = c5(1)
         c6T     = c6(1)
         sigT    = sig(1)
         goto 1011
      elseif (specT .ne. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period1(i) .and. specT .le. period1(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

      write (*,*)
      write (*,*) 'Gregor et al. (2002) Cascadia'
      write (*,*) 'Subduction attenuation model'
      write (*,*) 'is not defined for a spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period1(count1),period1(count2),c1(count1),c1(count2),
     +             specT,c1T,iflag)
      call S24_interp (period1(count1),period1(count2),c2(count1),c2(count2),
     +             specT,c2T,iflag)
      call S24_interp (period1(count1),period1(count2),c3(count1),c3(count2),
     +             specT,c3T,iflag)
      call S24_interp (period1(count1),period1(count2),c4(count1),c4(count2),
     +             specT,c4T,iflag)
      call S24_interp (period1(count1),period1(count2),c5(count1),c5(count2),
     +             specT,c5T,iflag)
      call S24_interp (period1(count1),period1(count2),c6(count1),c6(count2),
     +             specT,c6T,iflag)
      call S24_interp (period1(count1),period1(count2),sig(count1),sig(count2),
     +             specT,sigT,iflag)

 1011 period = specT

      attenName ='Gregor et al. (2002) Cascadia, Rock'

      period = specT

      lnY = c1T + c2T*mag + (c3T + c4T*mag)*alog(rupdist+exp(c5T)) +
     1      c6T*(mag-10.0)**3.0

         sigma = sigT


C     Now convert to gals.
      lnY = lnY + 6.89

      return
      end

c -------------------------------------------------------------------
C *** Gregor et al. (2002) Horizontal Cascadia Interface, Soil ******
c -------------------------------------------------------------------

      subroutine S02_Gregor02CasS ( mag, rupdist, lnY, sigma, specT,
     1                  attenName, period,iflag )

C     This relationship is constrained to a limited magnitude range of:
c             8.0 < M < 9.0
c     This magnitude range is checked before calling this subroutine.

      implicit none

      real mag, rupDist, lnY, sigma, period
      character*80 attenName
      real c1(25), c2(25), c3(25), c4(25), c5(25), c6(25)
      real period1(25), sig(25), sigT
      real specT, c1T, c2T, c3T, c4T, c5T, c6T
      integer nper, count1, count2, iflag, i

      data period1 / 0.000, 0.020, 0.025, 0.032, 0.040, 0.050, 0.056,
     1               0.063, 0.071, 0.083, 0.100, 0.125, 0.143, 0.167,
     2               0.200, 0.250, 0.333, 0.400, 0.500, 0.769, 1.000,
     3               1.667, 2.000, 2.500, 5.000  /
      data c1 / 23.8613,  25.4339, 25.42,   25.3849, 22.7042,
     1          23.2948,  23.2165, 24.7067, 24.9425, 26.5395,
     2          29.9693,  35.666,  50.7368, 55.6402, 75.8218,
     3          100.3357, 71.7967, 67.372,  56.0088, 26.3013,
     4          17.233,   11.9971, 17.9124, 16.1666,  7.4856 /
      data c2 / -2.2742, -2.4185, -2.4168, -2.4127, -2.1004,
     1          -2.1619, -2.1528, -2.2814, -2.3045, -2.4402,
     2          -2.7254, -3.1853, -4.5292, -4.9662, -6.8396,
     3          -9.0324, -6.499,  -6.1755, -5.1176, -2.4482,
     4          -1.5506, -1.118,  -1.7505, -1.5091, -0.836  /
      data c3 /-4.8803,  -5.1044,  -5.1026, -5.0977,  -4.9006,
     1         -4.8855,  -4.8744,  -5.0947, -5.0672,  -5.3025,
     2         -5.8054,  -6.6251,  -8.7213, -9.5555, -12.0687,
     3        -15.3511, -11.6056, -11.1567, -9.5083,  -5.3818,
     4         -4.3287,  -2.9451,  -3.815,  -3.7101,  -2.0627 /
      data c4 / 0.4399, 0.4602, 0.46,   0.4594, 0.4353,
     1          0.4332, 0.4319, 0.4509, 0.4476, 0.4677,
     2          0.5098, 0.5769, 0.7649, 0.8435, 1.0753,
     3          1.3731, 1.0415, 1.0167, 0.8632, 0.4957,
     4          0.393,  0.2639, 0.3574, 0.3344, 0.1779 /
      data c5 / 4.7, 4.8, 4.8, 4.8, 4.8,
     1          4.8, 4.8, 4.9, 4.9, 5.0,
     2          5.2, 5.5, 5.9, 6.0, 6.3,
     3          6.6, 6.2, 6.1, 5.9, 4.8,
     4          4.2, 3.7, 4.1, 4.1, -0.2 /
      data c6 / 0.0366, 0.037,  0.0369, 0.0366, 0.0164,
     1          0.0263, 0.0255, 0.0245, 0.0295, 0.0276,
     2          0.0226, 0.0123, 0.0108, -0.007, 0.0096,
     3         -0.0043, 0.0102, 0.0035, 0.0164, 0.0259,
     4          0.0133, 0.0538, 0.0583, 0.0473, 0.0821 /
      data sig / 0.5436, 0.5422, 0.5464, 0.5422, 0.5241,
     1           0.5319, 0.5413, 0.548,  0.5413, 0.5835,
     2           0.5926, 0.6665, 0.6532, 0.6393, 0.6618,
     3           0.6371, 0.6431, 0.6699, 0.6139, 0.7256,
     4           0.6606, 0.6837, 0.6276, 0.6676, 0.8207  /

C Find the requested spectral period and corresponding coefficients
      nPer = 25

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period  = period1(1)
         c1T     = c1(1)
         c2T     = c2(1)
         c3T     = c3(1)
         c4T     = c4(1)
         c5T     = c5(1)
         c6T     = c6(1)
         sigT    = sig(1)
         goto 1011
      elseif (specT .ne. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period1(i) .and. specT .le. period1(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

      write (*,*)
      write (*,*) 'Gregor et al. (2002) Cascadia'
      write (*,*) 'Subduction attenuation model'
      write (*,*) 'is not defined for a spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period1(count1),period1(count2),c1(count1),c1(count2),
     +             specT,c1T,iflag)
      call S24_interp (period1(count1),period1(count2),c2(count1),c2(count2),
     +             specT,c2T,iflag)
      call S24_interp (period1(count1),period1(count2),c3(count1),c3(count2),
     +             specT,c3T,iflag)
      call S24_interp (period1(count1),period1(count2),c4(count1),c4(count2),
     +             specT,c4T,iflag)
      call S24_interp (period1(count1),period1(count2),c5(count1),c5(count2),
     +             specT,c5T,iflag)
      call S24_interp (period1(count1),period1(count2),c6(count1),c6(count2),
     +             specT,c6T,iflag)
      call S24_interp (period1(count1),period1(count2),sig(count1),sig(count2),
     +             specT,sigT,iflag)

 1011 period = specT

      attenName ='Gregor et al. (2002) Cascadia, Soil'

      period = specT

      lnY = c1T + c2T*mag + (c3T + c4T*mag)*alog(rupdist+exp(c5T)) +
     1      c6T*(mag-10.0)**3.0

         sigma = sigT

C     Now convert to gals.
      lnY = lnY + 6.89

      return
      end

c -------------------------------------------------------------------
C *** Gregor et al. (2006) Horizontal Cascadia Interface, Vs30 ******
c -------------------------------------------------------------------

      subroutine S02_Gregor06Cas ( mag, rupdist, lnY, sigma, specT,
     1                  attenName, vs30, period,iflag )

      implicit none

      real mag, rupDist, lnY, sigma, period, pgaRock
      character*80 attenName
      real c1(25), c2(25), c3(25), c4(25), c5(25), c6(25)
      real period1(25), sig(25), soilamp
      real specT, c1T, c2T, c3T, c4T, c5T, c6T, vs30
      integer nper, count1, count2, iflag, i
      real n, c, b_soil(25), theta10(25), vref(25), sampadj(25)
      real b_soilT, Theta10T, vrefT, sampadjT, sigT, sigrock

      data Period1 / 0.00, 0.02, 0.025, 0.03, 0.04, 0.05, 0.056, 0.063, 0.071428571,
     1               0.083, 0.1, 0.125, 0.143, 0.167, 0.2, 0.25, 0.333, 0.4,
     2               0.5, 0.769, 1.0, 1.667, 2.0, 2.5, 5.0 /

      data c1      / 9.30979, 9.41101, 9.55386, 9.93963, 9.89636, 10.43056,
     1              10.96241, 11.18537, 11.6955, 11.97711, 13.45255, 13.31905,
     2              14.11414, 14.24239, 13.70461, 14.01509, 12.17016, 11.26958,
     3              10.7543, 7.75352, 6.00913, 4.08439, 5.54081, 4.79477, 5.13609 /

      data c2      / -0.72025, -0.72744, -0.73843, -0.76664, -0.75422,
     1               -0.77911, -0.80698, -0.81486, -0.83129, -0.82809,
     2               -0.93278, -0.89503, -0.95795, -0.94895, -0.91753,
     3               -0.9636,  -0.81309, -0.73763, -0.73886, -0.51697,
     4               -0.38538, -0.30129, -0.50319, -0.46478, -0.61463 /

      data c5      / 2.8, 2.8, 2.8, 2.8, 2.7, 2.7, 2.8, 2.8, 2.9, 3.0,
     1               3.2, 3.3, 3.4, 3.5, 3.5, 3.5, 3.5, 3.5, 3.4, 3.2,
     2               2.9, 2.7, 2.6, 2.4, 2.1 /

      data c3      / -3.10553, -3.12269, -3.14878, -3.21488, -3.21742,
     1               -3.23785, -3.31652, -3.34308, -3.44266, -3.47499,
     2               -3.61826, -3.64879, -3.64193, -3.70331, -3.65089,
     3               -3.6063,  -3.53255, -3.51567, -3.31553, -3.12098,
     4               -2.75873, -2.49346, -2.47444, -2.16295, -1.95111 /

      data c4      / 0.23705, 0.2383, 0.2404, 0.2453, 0.24439, 0.24094,
     1               0.24457, 0.24497, 0.24982, 0.24837, 0.25549, 0.2567,
     2               0.25266, 0.2571, 0.257, 0.25674, 0.2595, 0.26323,
     3               0.25162, 0.25014, 0.22003, 0.20614, 0.20765,
     4               0.177, 0.16358 /

      data c6      / 0.03739, 0.03731, 0.03715, 0.03691, 0.03598, 0.03894,
     1               0.03911, 0.03914, 0.03752, 0.03751, 0.04228, 0.03879,
     2               0.04621, 0.04379, 0.04239, 0.04843, 0.03491, 0.02456,
     3               0.03481, 0.01789, 0.02998, 0.02732, 0.0438, 0.05213, 0.07373 /

      data sig     / 0.7028, 0.7062, 0.7135, 0.7221, 0.7290, 0.7422,
     1               0.7516, 0.7610, 0.7655, 0.8060, 0.8240, 0.8690,
     2               0.8536, 0.8374, 0.8240, 0.8093, 0.7914, 0.7962,
     3               0.7237, 0.7871, 0.7095, 0.6570, 0.5959, 0.6552, 0.7897 /
      data b_soil  / -1.186, -1.219, -1.248718345, -1.273, -1.308, -1.346,
     1               -1.380937866, -1.417248955, -1.455958581, -1.534878688,
     2               -1.624, -1.792954292, -1.894815052, -2.026908441, -2.188,
     3               -2.378282827, -2.568423866, -2.657, -2.669, -2.362214398,
     4               -1.955, -0.758605507, -0.299, -0.134448426, 0.0 /

      data Vref    / 865.1, 865.1, 888.5995058, 907.8, 994.5, 1053.5,
     1              1062.499994, 1071.853731, 1081.825331, 1058.230721,
     2              1032.5, 947.2523784, 895.8574867, 829.3099131, 748.2,
     3               655.3244676, 556.5917476, 503.0, 456.6, 409.5868861,
     4               400.0, 400.0, 400.0, 400.0, 400.0 /

      data Theta10 / 0.9255, 0.9647, 0.997445213, 1.0242, 1.057, 1.1022,
     1               1.147199972, 1.193968654, 1.243826653, 1.335426432,
     2               1.4285, 1.62722767, 1.747038486, 1.897524986,
     3               2.0788, 2.278848596, 2.463524828, 2.5514, 2.528,
     4               2.075643639, 1.506, -0.00763144, -0.5703,
     5              -0.742391228, -0.7993 /
      data sampadj / -0.118149083, -0.118084273, -0.105904522, -0.09610953,
     1               -0.05344769, -0.025393821, -0.021093876, -0.016729322,
     2               -0.01219193, -0.022721662, -0.035306933, -0.077444948,
     3               -0.10477571, -0.144077725, -0.198421599, -0.277997736,
     4               -0.391539256, -0.462139859, -0.552012084, -0.709610012,
     5               -0.817439183, -0.921429142, -0.942183118, -0.919647528,
     6               -0.815806142 /

C Find the requested spectral period and corresponding coefficients
      nPer = 25

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period   = period1(1)
         c1T      = c1(1)
         c2T      = c2(1)
         c3T      = c3(1)
         c4T      = c4(1)
         c5T      = c5(1)
         c6T      = c6(1)
         sigT     = sig(1)
         b_soilT  = b_soil(1)
         vrefT    = vref(1)
         Theta10T = theta10(1)
         sampadjT = sampadj(1)
         goto 1011
      elseif (specT .ne. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period1(i) .and. specT .le. period1(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

      write (*,*)
      write (*,*) 'Gregor et al. (2006) Cascadia'
      write (*,*) 'Subduction attenuation model'
      write (*,*) 'is not defined for a spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period1(count1),period1(count2),c1(count1),c1(count2),
     +             specT,c1T,iflag)
      call S24_interp (period1(count1),period1(count2),c2(count1),c2(count2),
     +             specT,c2T,iflag)
      call S24_interp (period1(count1),period1(count2),c3(count1),c3(count2),
     +             specT,c3T,iflag)
      call S24_interp (period1(count1),period1(count2),c4(count1),c4(count2),
     +             specT,c4T,iflag)
      call S24_interp (period1(count1),period1(count2),c5(count1),c5(count2),
     +             specT,c5T,iflag)
      call S24_interp (period1(count1),period1(count2),c6(count1),c6(count2),
     +             specT,c6T,iflag)
      call S24_interp (period1(count1),period1(count2),sig(count1),sig(count2),
     +             specT,sigT,iflag)
      call S24_interp (period1(count1),period1(count2),b_soil(count1),b_soil(count2),
     +             specT,b_soilT,iflag)
      call S24_interp (period1(count1),period1(count2),vref(count1),vref(count2),
     +             specT,vrefT,iflag)
      call S24_interp (period1(count1),period1(count2),Theta10(count1),Theta10(count2),
     +             specT,Theta10T,iflag)
      call S24_interp (period1(count1),period1(count2),sampadj(count1),sampadj(count2),
     +             specT,sampadjT,iflag)

 1011 period = specT

      attenName ='Gregor et al. (2006) Cascadia'

      period = specT

      lnY = c1T + c2T*mag + (c3T + c4T*mag)*alog(rupdist+exp(c5T)) +
     1      c6T*(mag-10.0)**3.0

C     Now apply Walling and Abrahamson site amplification model.
c     Site response
c     Compute PGARock (i.e., Vs=1100 m/sec)
      pgaRock =  9.28996 -0.71918*mag + (-3.10146 + 0.23683*mag)*alog(rupdist+exp(2.8)) +
     1      0.03741*(mag-10.0)**3.0
      pgarock = exp(pgarock)

      sigrock = 0.7038
      c = 1.88
      n = 1.18

      if (vs30 .lt. vrefT) then
          soilamp = theta10T*alog(vs30/vrefT) - 1.0*b_soilT*alog(c+pgaRock)
     1              + b_soilT*alog(pgaRock+c*((vs30/vrefT)**(n)) )
      else
        soilamp = (theta10T + b_soilT*n) * alog(vs30/vrefT)
      endif

C     Now correct for reference Vs30m = 1100 m/sec
      soilamp = soilamp - sampadjT

      lnY = lnY + soilamp
C     Now convert to gals.
      lnY = lnY + 6.89
      sigma = sigT

      return
      end

c ---------------------------------------------------------------------
C     *** Kanno et al. (2006, BSSA) ***
c ---------------------------------------------------------------------
      subroutine S02_kanno2006 ( mag, Rrup, specT,
     1                     period2, lnY, sigma, iflag, vs30, depth )

      implicit none

      integer MAXPER
      parameter (MAXPER=38)
      REAL Period(MAXPER), a1(MAXPER), b1(MAXPER), c1(MAXPER), d1(MAXPER)
      real a2(MAXPER), b2(MAXPER), c2(MAXPER)
      real sig1(MAXPER), sig2(MAXPER), p(MAXPER), q(MAXPER)

      REAL MAG, Rrup, VS30, logY, lnY, sigma, specT, period2, depth, e1
      INTEGER iFlag, count1, count2, nPer, i
      real a1T, b1T, c1T, d1T, sig1T, a2T, b2T, c2T, sig2T
      real pT, qT, period1, X

      data period / 0,0.05,0.06,0.07,0.08,0.09,0.1,0.11,0.12,0.13,0.15,0.17,
     1         0.2,0.22,0.25,0.3,0.35,0.4,0.45,0.5,0.6,0.7,0.8,0.9,1,1.1,
     2         1.2,1.3,1.5,1.7,2,2.2,2.5,3,3.5,4,4.5,5 /
      data a1 / 0.56,0.54,0.54,0.53,0.52,0.52,0.52,0.5,0.51,0.51,0.52,0.53,0.54,
     1     0.54,0.54,0.56,0.56,0.58,0.59,0.59,0.62,0.63,0.65,0.68,0.71,0.72,
     2     0.73,0.74,0.77,0.79,0.8,0.82,0.84,0.86,0.9,0.92,0.94,0.92 /
      data b1 / -0.0031,-0.0035,-0.0037,-0.0039,-0.004,-0.0041,-0.0041,
     1          -0.004,-0.004,-0.0039,-0.0038,-0.0037,-0.0034,-0.0032,
     2          -0.0029,-0.0026,-0.0024,-0.0021,-0.0019,-0.0016,-0.0014,
     3          -0.0012,-0.0011,-0.0009,-0.0009,-0.0007,-0.0006,-0.0006,
     4          -0.0005,-0.0005,-0.0004,-0.0004,-0.0003,-0.0002,-0.0003,
     5          -0.0005,-0.0007,-0.0004 /
      data c1 / 0.26,0.48,0.57,0.67,0.75,0.8,0.85,0.96,0.93,0.91,0.89,0.84,
     1          0.76,0.73,0.66,0.51,0.42,0.26,0.13,0.04,-0.22,-0.37,-0.54,
     2          -0.8,-1.04,-1.19,-1.32,-1.44,-1.7,-1.89,-2.08,-2.24,-2.46,
     3          -2.72,-2.99,-3.21,-3.39,-3.35/
      data d1 / 0.0055,0.0061,0.0065,0.0066,0.0069,0.0071,0.0073,0.0061,
     1          0.0062,0.0062,0.006,0.0056,0.0053,0.0048,0.0044,0.0039,
     2          0.0036,0.0033,0.003,0.0022,0.0025,0.0022,0.002,0.0019,
     3          0.0021,0.0018,0.0014,0.0014,0.0017,0.0019,0.002,0.0022,
     4          0.0023,0.0021,0.0032,0.0045,0.0064,0.003 /
      data sig1 / 0.37,0.37,0.38,0.38,0.39,0.4,0.4,0.4,0.4,0.4,0.41,0.41,
     1            0.4,0.4,0.4,0.39,0.4,0.4,0.41,0.41,0.41,0.41,0.41,0.41,
     2            0.41,0.41,0.41,0.41,0.4,0.39,0.39,0.38,0.38,0.38,0.37,
     3            0.38,0.38,0.38 /

      data a2 / 0.41,0.39,0.39,0.38,0.38,0.38,0.38,0.38,0.38,0.38,0.39,
     1          0.4,0.4,0.4,0.41,0.43,0.43,0.45,0.46,0.47,0.49,0.51,0.53,
     2          0.56,0.57,0.59,0.6,0.62,0.64,0.66,0.68,0.69,0.71,0.73,0.75,
     3          0.77,0.79,0.82 /
      data b2 / -0.0039,-0.004,-0.0041,-0.0042,-0.0042,-0.0043,-0.0043,-0.0044,
     1          -0.0044,-0.0044,-0.0044,-0.0043,-0.0042,-0.0041,-0.004,-0.0038,
     2          -0.0036,-0.0034,-0.0032,-0.003,-0.0028,-0.0026,-0.0025,-0.0023,
     3          -0.0022,-0.0022,-0.0021,-0.002,-0.002,-0.0018,-0.0017,-0.0017,
     4          -0.0017,-0.0017,-0.0017,-0.0016,-0.0016,-0.0017/
      data c2 / 1.56,1.76,1.86,1.96,2.03,2.08,2.12,2.14,2.14,2.13,2.12,2.08,2.02,
     1          1.99,1.88,1.75,1.62,1.49,1.33,1.19,0.95,0.72,0.49,0.27,0.08,-0.08,
     2          -0.24,-0.4,-0.63,-0.83,-1.12,-1.27,-1.48,-1.72,-1.97,-2.22,-2.45,-2.7 /
      data sig2 / 0.4,0.42,0.43,0.45,0.45,0.46,0.46,0.46,0.46,0.46,0.46,0.45,0.44,
     1            0.43,0.42,0.42,0.41,0.41,0.41,0.4,0.4,0.4,0.4,0.4,0.41,0.41,0.41,
     2            0.41,0.41,0.4,0.4,0.4,0.39,0.39,0.38,0.37,0.36,0.35/
      data p  / -0.55,-0.32,-0.26,-0.24,-0.26,-0.29,-0.32,-0.35,-0.39,-0.43,-0.53,
     1        -0.61,-0.68,-0.72,-0.75,-0.8,-0.85,-0.87,-0.89,-0.91,-0.92,-0.96,
     2        -0.98,-0.97,-0.93,-0.92,-0.91,-0.88,-0.85,-0.83,-0.78,-0.76,-0.72,
     3        -0.68,-0.66,-0.62,-0.6,-0.59 /
      data q / 1.35,0.8,0.65,0.6,0.64,0.72,0.78,0.84,0.94,1.04,1.28,1.47,1.65,
     1         1.74,1.82,1.96,2.09,2.13,2.18,2.25,2.3,2.41,2.46,2.44,2.32,2.3,
     2         2.26,2.2,2.12,2.06,1.92,1.88,1.8,1.7,1.64,1.54,1.5,1.46 /

C First check for the PGA case (i.e., specT=0.0)
      nPer = 38
      if (specT .eq. 0.0) then
         period1 = period(1)
         a1T = a1(1)
         b1T = b1(1)
         c1T = c1(1)
         d1T = d1(1)
         a2T = a2(1)
         b2T = b2(1)
         c2T = c2(1)
         sig1T = sig1(1)
         sig2T = sig2(1)
         pT = p(1)
         qT = q(1)
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'Kanno et al (2006) Horizontal'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020       call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +                   specT,a1T,iflag)
            call S24_interp (period(count1),period(count2),b1(count1),b1(count2),
     +                   specT,b1T,iflag)
            call S24_interp (period(count1),period(count2),c1(count1),c1(count2),
     +                   specT,c1T,iflag)
            call S24_interp (period(count1),period(count2),d1(count1),d1(count2),
     +                   specT,d1T,iflag)
            call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +                   specT,a2T,iflag)
            call S24_interp (period(count1),period(count2),b2(count1),b2(count2),
     +                   specT,b2T,iflag)
            call S24_interp (period(count1),period(count2),c2(count1),c2(count2),
     +                   specT,c2T,iflag)
            call S24_interp (period(count1),period(count2),sig1(count1),sig1(count2),
     +                   specT,sig1T,iflag)
            call S24_interp (period(count1),period(count2),sig2(count1),sig2(count2),
     +                   specT,sig2T,iflag)
            call S24_interp (period(count1),period(count2),p(count1),p(count2),
     +                   specT,pT,iflag)
            call S24_interp (period(count1),period(count2),q(count1),q(count2),
     +                   specT,qT,iflag)

 1011 period1 = specT

      e1 = 0.50
      X = rrup
      if ( depth .lt. 30. ) then
        logY = a1T*mag + b1T*X - alog10(X+d1T*10.0**(e1*mag)) + c1T
        sigma = sig1T
      else
        logY = a2T*mag + b2T*X - alog10(X) + c2T
        sigma = sig2T
      endif
      logY = logY + pT*alog10(vs30)+qT
      lnY = logY * alog(10.)
      sigma = sigma * alog(10.)
      period2 = period1

C     Convert ground motion to units of gals.
c      lnY = lnY + 6.89

      return
      END

c -------------------------------------------------------------------
C **** Zhao et al. 2006 (BSSA, Vol 96, No. 3) *************
c -------------------------------------------------------------------

      subroutine S02_Zhaoetal2006 ( m, dist, ftype, lnY, sigma, sclass, specT,
     1                   attenName, period1, iflag, sourcetype, hypo, phi, tau )

c     This  subroutine calculates the spectral acceleration from the
c     Zhao et al. (2006) attenuation model which is defined for crustal
c     and subduction events. Five separate site classes are also modeled.

      implicit none

      integer MAXPER
      parameter (MAXPER=22)
      real ftype, dist, m, lnY, sigma, specT, period1
      real a(MAXPER), b(MAXPER), c(MAXPER), d(MAXPER), e(MAXPER)
      real Sr(MAXPER), Si(MAXPER), Ss(MAXPER), Ssl(MAXPER)
      real Ch(MAXPER), C1(MAXPER), C2(MAXPER), C3(MAXPER), C4(MAXPER)
      real tauc(MAXPER), taui(MAXPER), taus(MAXPER)
      real Qc(MAXPER), Wc(MAXPER), Qi(MAXPER), Wi(MAXPER), Qs(MAXPER), Ws(MAXPER)
      real Ps(MAXPER)
      real sigma1(MAXPER), period(MAXPER)
      character*80 attenName
      integer nper, count1, count2, iflag, i
      real aT, bT, cT, dT, eT, sourcetype, sclass
      real SrT, SiT, SsT, SslT
      real ChT, C1T, C2T, C3T, C4T, sigma1T
      real taucT, tauiT, tausT
      real QcT, WcT, QiT, WiT, QsT, WsT, PsT
      real hc, mech, deltac, classterm, msq, hypo
      real phi, tau, r

      data period / 0.00, 0.01, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.40, 0.50,
     1              0.60, 0.70, 0.80, 0.90, 1.00, 1.25, 1.50, 2.00, 2.50,
     1              3.00, 4.00, 5.00 /
      data a      / 1.101, 1.101, 1.076, 1.118, 1.134, 1.147, 1.149, 1.163,
     1              1.200, 1.250, 1.293, 1.336, 1.386, 1.433, 1.479,
     1              1.551, 1.621, 1.694, 1.748, 1.759, 1.826, 1.825 /
      data b      / -0.00564,-0.00564, -0.00671, -0.00787, -0.00722, -0.00659,
     1              -0.00590, -0.00520, -0.00422, -0.00338, -0.00282,
     1              -0.00258, -0.00242, -0.00232, -0.00220, -0.00207,
     1              -0.00224, -0.00201, -0.00187, -0.00147, -0.00195,
     1              -0.00237 /
      data c      / 0.0055, 0.0055, 0.0075, 0.0090, 0.0100, 0.0120, 0.0140,
     1              0.0150, 0.0100, 0.0060, 0.0030, 0.0025, 0.0022,
     1              0.0020, 0.0020, 0.0020, 0.0020, 0.0025, 0.0028,
     1              0.0032, 0.0040, 0.0050 /
      data d      / 1.080, 1.080, 1.060, 1.083, 1.053, 1.014, 0.966, 0.934,
     1              0.959, 1.008, 1.088, 1.084, 1.088, 1.109, 1.115,
     1              1.083, 1.091, 1.055, 1.052, 1.025, 1.044, 1.065 /
      data e      / 0.01412, 0.01412, 0.01463, 0.01423, 0.01509, 0.01462,
     1              0.01459, 0.01458, 0.01257, 0.01114, 0.01019,
     1              0.00979, 0.00944, 0.00972, 0.01005, 0.01003,
     1              0.00928, 0.00833, 0.00776, 0.00644, 0.00590, 0.00510 /
      data Sr     / 0.251, 0.251, 0.251, 0.240, 0.251, 0.260, 0.269, 0.259,
     1              0.248, 0.247, 0.233, 0.220, 0.232, 0.220, 0.211,
     1              0.251, 0.248, 0.263, 0.262, 0.307, 0.353, 0.248 /
      data Si     / 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     1             -0.041, -0.053, -0.103, -0.146, -0.164, -0.206,
     1             -0.239, -0.256, -0.306, -0.321, -0.337, -0.331,
     1             -0.390, -0.498 /
      data Ss     / 2.607, 2.607, 2.764, 2.156, 2.161, 1.901, 1.814, 2.181,
     1              2.432, 2.629, 2.702, 2.654, 2.480, 2.332, 2.233,
     1              2.029, 1.589, 0.966, 0.789, 1.037, 0.561, 0.225 /
      data Ssl    / -0.528, -0.528, -0.551, -0.420, -0.431, -0.372, -0.360,
     1              -0.450, -0.506, -0.554, -0.575, -0.572, -0.540,
     1              -0.522, -0.509, -0.469, -0.379, -0.248, -0.221,
     1              -0.263, -0.169, -0.120 /
      data Ch     / 0.293, 0.293, 0.939, 1.499, 1.462, 1.280, 1.121, 0.852,
     1              0.365, -0.207, -0.705, -1.144, -1.609, -2.023,
     1             -2.451, -3.243, -3.888, -4.783, -5.444, -5.839,
     1             -6.598, -6.752 /
      data C1     / 1.111, 1.111, 1.684, 2.061, 1.916, 1.669, 1.468, 1.172,
     1              0.655, 0.071, -0.429, -0.866, -1.325, -1.732,
     1             -2.152, -2.923, -3.548, -4.410, -5.049, -5.431,
     1             -6.181, -6.347 /
      data C2     / 1.344, 1.344, 1.793, 2.135, 2.168, 2.085, 1.942, 1.683,
     1              1.127, 0.515, -0.003, -0.449, -0.928, -1.349,
     1             -1.776, -2.542, -3.169, -4.039, -4.698, -5.089,
     1             -5.882, -6.051 /
      data C3     / 1.355, 1.355, 1.747, 2.031, 2.052, 2.001, 1.941, 1.808,
     1              1.482, 0.934, 0.394, -0.111, -0.620, -1.066, -1.523,
     1              -2.327, -2.979, -3.871, -4.496, -4.893, -5.698, -5.873 /
      data C4     / 1.420, 1.420, 1.814, 2.082, 2.113, 2.030, 1.937, 1.770,
     1              1.397, 0.955, 0.559, 0.188, -0.246, -0.643, -1.084,
     1             -1.936, -2.661, -3.640, -4.341, -4.758, -5.588,
     1             -5.798 /
      data sigma1 / 0.604, 0.604, 0.640, 0.694, 0.702, 0.692, 0.682, 0.670,
     1              0.659, 0.653, 0.653, 0.652, 0.647, 0.653, 0.657,
     1              0.660, 0.664, 0.669, 0.671, 0.667, 0.647, 0.643 /
      data tauc   / 0.303, 0.303, 0.326, 0.342, 0.331, 0.312, 0.298, 0.300,
     1              0.346, 0.338, 0.349, 0.351, 0.356, 0.348, 0.338,
     1              0.313, 0.306, 0.283, 0.287, 0.278, 0.273, 0.275 /
      data taui   / 0.308, 0.308, 0.343, 0.403, 0.367, 0.328, 0.289, 0.280,
     1              0.271, 0.277, 0.296, 0.313, 0.329, 0.324, 0.328,
     1              0.339, 0.352, 0.360, 0.356, 0.338, 0.307, 0.272 /
      data taus   / 0.321, 0.321, 0.378, 0.420, 0.372, 0.324, 0.294, 0.284,
     1              0.278, 0.272, 0.285, 0.290, 0.299, 0.289, 0.286,
     1              0.277, 0.282, 0.300, 0.292, 0.274, 0.281, 0.296 /
      data Qc     / 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     1              0.000, -0.0126, -0.0329, -0.0501, -0.0650, -0.0781,
     1             -0.0899, -0.1148, -0.1351, -0.1672, -0.1921, -0.2124,
     1             -0.2445, -0.2694 /
      data Wc     / 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     1              0.000,  0.0116, 0.0202, 0.0274, 0.0336, 0.0391,
     1              0.0440, 0.0545, 0.0630, 0.0764, 0.0869, 0.0954,
     1              0.1088, 0.1193 /
      data Qi     / 0.000, 0.000, 0.000, 0.000, -0.0138, -0.0256, -0.0348,
     1             -0.0423, -0.0541, -0.0632, -0.0707, -0.0771,
     1             -0.0825, -0.0874, -0.0917, -0.1009, -0.1083,
     1             -0.1202, -0.1293, -0.1368, -0.1486, -0.1578 /
      data Wi     / 0.000, 0.000, 0.000, 0.000, 0.0286, 0.0352, 0.0403,
     1              0.0445, 0.0511, 0.0562, 0.0604, 0.0639, 0.0670,
     1              0.0697, 0.0721, 0.0772, 0.0814, 0.0880, 0.0931,
     1              0.0972, 0.1038, 0.1090  /
      data Qs     / 0.1584, 0.1584, 0.1932, 0.2057, 0.1984, 0.1856, 0.1714,
     1              0.1573, 0.1309, 0.1078, 0.0878, 0.0705, 0.0556,
     1              0.0426, 0.0314, 0.0093, -0.0062, -0.0235, -0.0287,
     1             -0.0261, -0.0065, 0.0246 /
      data Ws     / -0.0529, -0.0529, -0.0841, -0.0877, -0.0773, -0.0644, -0.0515,
     1              -0.0395, -0.0183, -0.0008, 0.0136, 0.0254, 0.0352,
     1               0.0432, 0.0498, 0.0612, 0.0674, 0.0692, 0.0622,
     1               0.0496, 0.0150, -0.0268 /
      data Ps     / 0.1392, 0.1392, 0.1636, 0.1690, 0.1669, 0.1631, 0.1588,
     1              0.1544, 0.1460, 0.1381, 0.1307, 0.1239, 0.1176,
     1              0.1116, 0.1060, 0.0933, 0.0821, 0.0628, 0.0465,
     1              0.0322, 0.0083, -0.0117 /

      hc = 15.0

c Set attenuation name
c     Sourcetype = 0 Crustal
c     Sourcetype = 1 Subduction - Interface
c     Sourcetype = 2 Subduction - Slab
c     Sclass = 0 Hard Rock
c     Sclass = 1 SC I
c     Sclass = 2 SC II
c     Sclass = 3 SC III
c     Sclass = 4 SC IV

      if (sourcetype .eq. 0.0) then
         if (sclass .eq. 0.0) then
            attenName = 'Zhao etal. (2006)-Crustal, Hard Rock'
         elseif (sclass .eq. 1.0) then
            attenName = 'Zhao etal. (2006)-Crustal, Site Class I'
         elseif (sclass .eq. 2.0) then
            attenName = 'Zhao etal. (2006)-Crustal, Site Class II'
         elseif (sclass .eq. 3.0) then
            attenName = 'Zhao etal. (2006)-Crustal, Site Class III'
         elseif (sclass .eq. 4.0) then
            attenName = 'Zhao etal. (2006)-Crustal, Site Class IV'
         endif
      elseif (sourcetype .eq. 1.0) then
         if (sclass .eq. 0.0) then
            attenName = 'Zhao etal. (2006)-Sub.Int, Hard Rock'
         elseif (sclass .eq. 1.0) then
            attenName = 'Zhao etal. (2006)-Sub.Int, Site Class I'
         elseif (sclass .eq. 2.0) then
            attenName = 'Zhao etal. (2006)-Sub.Int, Site Class II'
         elseif (sclass .eq. 3.0) then
            attenName = 'Zhao etal. (2006)-Sub.Int, Site Class III'
         elseif (sclass .eq. 4.0) then
            attenName = 'Zhao etal. (2006)-Sub.Int, Site Class IV'
         endif
      elseif (sourcetype .eq. 2.0) then
         if (sclass .eq. 0.0) then
            attenName = 'Zhao etal. (2006)-Sub.Slab, Hard Rock'
         elseif (sclass .eq. 1.0) then
            attenName = 'Zhao etal. (2006)-Sub.Slab, Site Class I'
         elseif (sclass .eq. 2.0) then
            attenName = 'Zhao etal. (2006)-Sub.Slab, Site Class II'
         elseif (sclass .eq. 3.0) then
            attenName = 'Zhao etal. (2006)-Sub.Slab, Site Class III'
         elseif (sclass .eq. 4.0) then
            attenName = 'Zhao etal. (2006)-Sub.Slab, Site Class IV'
         endif
      endif

C Find the requested spectral period and corresponding coefficients
      nper = 22

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1 = period(1)
         aT = a(1)
         bT = b(1)
         cT = c(1)
         dT = d(1)
         eT = e(1)
         SrT = Sr(1)
         SiT = Si(1)
         SsT = Ss(1)
         SslT = Ssl(1)
         ChT = Ch(1)
         C1T = C1(1)
         C2T = C2(1)
         C3T = C3(1)
         C4T = C4(1)
         sigma1T = sigma1(1)
         taucT = tauc(1)
         tauiT = taui(1)
         tausT = taus(1)
         QcT = Qc(1)
         WcT = Wc(1)
         QiT = Qi(1)
         WiT = Wi(1)
         QsT = Qs(1)
         WsT = Ws(1)
         PsT = Ps(1)
         goto 1011
      elseif (specT .ne. 0.0) then

C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1010
            endif
         enddo
      endif

      write (*,*)
      write (*,*) 'Zhao etal. (2006) Horizontal atttenuation model'
      write (*,*) 'is not defined for a spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1010       call S24_interp (period(count1),period(count2),a(count1),a(count2),
     +                   specT,aT,iflag)
            call S24_interp (period(count1),period(count2),b(count1),b(count2),
     +                   specT,bT,iflag)
            call S24_interp (period(count1),period(count2),c(count1),c(count2),
     +                   specT,cT,iflag)
            call S24_interp (period(count1),period(count2),d(count1),d(count2),
     +                   specT,dT,iflag)
            call S24_interp (period(count1),period(count2),e(count1),e(count2),
     +                   specT,eT,iflag)
            call S24_interp (period(count1),period(count2),Sr(count1),Sr(count2),
     +                   specT,SrT,iflag)
            call S24_interp (period(count1),period(count2),Si(count1),Si(count2),
     +                   specT,SiT,iflag)
            call S24_interp (period(count1),period(count2),Ss(count1),Ss(count2),
     +                   specT,SsT,iflag)
            call S24_interp (period(count1),period(count2),Ssl(count1),Ssl(count2),
     +                   specT,SslT,iflag)
            call S24_interp (period(count1),period(count2),Ch(count1),Ch(count2),
     +                   specT,ChT,iflag)
            call S24_interp (period(count1),period(count2),C1(count1),C1(count2),
     +                   specT,C1T,iflag)
            call S24_interp (period(count1),period(count2),C2(count1),C2(count2),
     +                   specT,C2T,iflag)
            call S24_interp (period(count1),period(count2),C3(count1),C3(count2),
     +                   specT,C3T,iflag)
            call S24_interp (period(count1),period(count2),C4(count1),C4(count2),
     +                   specT,C4T,iflag)
            call S24_interp (period(count1),period(count2),sigma1(count1),sigma1(count2),
     +                   specT,sigma1T,iflag)
            call S24_interp (period(count1),period(count2),tauc(count1),tauc(count2),
     +                   specT,taucT,iflag)
            call S24_interp (period(count1),period(count2),taui(count1),taui(count2),
     +                   specT,tauiT,iflag)
            call S24_interp (period(count1),period(count2),taus(count1),taus(count2),
     +                   specT,tausT,iflag)
            call S24_interp (period(count1),period(count2),Qc(count1),Qc(count2),
     +                   specT,QcT,iflag)
            call S24_interp (period(count1),period(count2),Wc(count1),Wc(count2),
     +                   specT,WcT,iflag)
            call S24_interp (period(count1),period(count2),Qi(count1),Qi(count2),
     +                   specT,QiT,iflag)
            call S24_interp (period(count1),period(count2),Wi(count1),Wi(count2),
     +                   specT,WiT,iflag)
            call S24_interp (period(count1),period(count2),Qs(count1),Qs(count2),
     +                   specT,QsT,iflag)
            call S24_interp (period(count1),period(count2),Ws(count1),Ws(count2),
     +                   specT,WsT,iflag)
            call S24_interp (period(count1),period(count2),Ps(count1),Ps(count2),
     +                   specT,PsT,iflag)

 1011 period1 = specT

C     Compute base model first (units of cm/s*s)
      if (hypo .lt. hc) then
         deltac = 0.0
      else
         deltac =  1.0
      endif
      if (hypo .gt. 125.0) then
          hypo = 125.0
      endif

C     Set mechanism term and magnitude squared term.
      if (sourcetype .eq. 0 ) then
         if (ftype .eq. 1) then
            mech = SrT
         else
            mech = 0.0
         endif
         msq = QcT*(m - 6.3)*(m-6.3) + WcT
      elseif (sourcetype .eq. 1) then
          mech = SiT
          msq = QiT*(m - 6.3)*(m-6.3) + WiT
      elseif (sourcetype .eq. 2) then
          mech = SsT + SslT*alog(dist)
          msq = PsT*(m-6.5) + QsT*(m - 6.5)*(m-6.5) + WsT
      endif

C     Set Site class term
      if (sclass .eq. 0) then
         classterm = ChT
      elseif (sclass .eq. 1) then
         classterm = C1T
      elseif (sclass .eq. 2) then
         classterm = C2T
      elseif (sclass .eq. 3) then
         classterm = C3T
      elseif (sclass .eq. 4) then
         classterm = C4T
      endif

      r = dist + cT*exp(dT*m)

      lnY = aT*m + bT*dist - alog(r) +eT*deltac*(hypo-hc) +
     1       mech + classterm + msq

c     Set standard error
      phi = sigma1T
      if (sourcetype .eq. 0) then
         sigma = sqrt( sigma1T*sigma1T + taucT*taucT )
         tau = taucT
      elseif (sourcetype .eq. 1) then
         sigma = sqrt( sigma1T*sigma1T + tauiT*tauiT )
         tau = tauiT
      elseif (sourcetype .eq. 2) then
         sigma = sqrt( sigma1T*sigma1T + tausT*tausT )
         tau = tausT
      endif

      return
      end

c -------------------------------------------------------------------
C *** Atkinson and Boore (1984) Horizontal EUS Hard Rock ************
c -------------------------------------------------------------------

      subroutine S02_AB95 ( m, dist, lnY, sigma, specT,
     1                  attenName, period1,iflag )

c     Atkinson and Boore (1995) Eastern North America - Eq model

      implicit none

      integer MAXPER
      real lnY, m, dist, sigma, period1, twoPi
      real specT, c1T, c2T, c3T, c4T, sig0T
      integer nper, count1, count2, iflag, i
      character*80 attenName

      parameter (MAXPER=12)
      real c1(MAXPER), c2(MAXPER), c3(MAXPER), c4(MAXPER)
      REAL period(MAXPER), sig0(MAXPER)
      data period / 0.0, 0.05, 0.077, 0.10, 0.127, 0.20, 0.313, 0.50,
     1              0.769, 1.0, 1.25, 2.0 /
      data c1 / 1.841, 2.762, 2.463, 2.301, 2.140, 1.749, 1.265, 0.620,
     1          -0.094, -0.508, -0.900, -1.660 /
      data c2 / 0.686, 0.755, 0.797, 0.829, 0.864, 0.963, 1.094, 1.267,
     1        1.391, 1.428, 1.462, 1.460 /
      data c3 / -0.123, -0.110, -0.113, -0.121, -0.129, -0.148,
     1       -0.165, -0.147, -0.118, -0.094, -0.0710, -0.039 /
      data c4 / 0.00311, 0.00520, 0.00352, 0.00279, 0.00207,
     1          0.00105, 0.00024, 0.00, 0.00, 0.00, 0.00, 0.00 /

      data sig0 / 0.622, 0.622, 0.622, 0.622, 0.616, 0.599, 0.577,
     1  0.553, 0.553, 0.553, 0.553, 0.553 /

      twoPi = 2.0 * 3.1415296

C Find the requested spectral period and corresponding coefficients
      nPer = 12

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1  = period(1)
         c1T     = c1(1)
         c2T     = c2(1)
         c3T     = c3(1)
         c4T     = c4(1)
         sig0T   = sig0(1)
         goto 1011
      elseif (specT .ne. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

      write (*,*)
      write (*,*) 'Atkinson and Boore (1995) EUS (Mw) atttenuation model'
      write (*,*) 'is not defined for a spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),c1(count1),c1(count2),
     +             specT,c1T,iflag)
      call S24_interp (period(count1),period(count2),c2(count1),c2(count2),
     +             specT,c2T,iflag)
      call S24_interp (period(count1),period(count2),c3(count1),c3(count2),
     +             specT,c3T,iflag)
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),
     +             specT,c4T,iflag)
      call S24_interp (period(count1),period(count2),sig0(count1),sig0(count2),
     +             specT,sig0T,iflag)

 1011 period1 = specT

c     Set atten name
      attenName = 'Atkinson & Boore, (1995), Mw, Eq Model'

      lnY = c1T + c2T*(m-6) + c3T*(m-6)**2 -
     1          alog(dist) - c4T*dist

c     Convert to spectral acceleration in gal
      lnY = lnY + 6.89

      sigma = sig0T

      return
      end

c ------------------------------------------------------------------------
C *** Atkinson and Boore (1984) Horizontal EUS Hard Rock MLg magnitude ***
c ------------------------------------------------------------------------

      subroutine S02_AB95Mn ( m, dist, lnY, sigma, specT,
     1                  attenName, period1,iflag )

      implicit none

      integer MAXPER
      real lnY, m, dist, sigma, period1, mgsigma, sgmod
      real specT, c1T, c2T, c3T, c4T, sig0T, twoPi
      integer nper, count1, count2, iflag, i
      character*80 attenName

      parameter (MAXPER=12)
      real c1(MAXPER), c2(MAXPER), c3(MAXPER), c4(MAXPER)
      real period(MAXPER), sig0(MAXPER)
      data period / 0.0, 0.05, 0.077, 0.10, 0.127, 0.20, 0.313, 0.50,
     1              0.769, 1.0, 1.25, 2.0 /
      data c1 / 1.838, 2.759, 2.460, 2.299, 2.138, 1.746, 1.263, 0.618,
     1         -0.096, -0.510, -0.902, -1.662 /
      data c2 / 0.686, 0.755, 0.797, 0.829, 0.863, 0.962, 1.094, 1.266,
     1          1.391, 1.428, 1.462, 1.460 /
      data c3 / -0.1234, -0.1098, -0.1133, -0.1213, -0.1294, -0.1483,
     1        -0.1651, -0.1474, -0.1177, -0.0942, -0.0709, -0.0391 /
      data c4 / 0.003108, 0.005204, 0.003523, 0.002786, 0.002068,
     1    0.001052, 0.000244, 0.00, 0.00, 0.00, 0.00, 0.00 /

      data sig0 / 0.668, 0.668, 0.668, 0.668, 0.682, 0.714, 0.747,
     1     0.783, 0.910, 0.990, 0.990, 0.990 /
      twoPi = 2.0 * 3.1415296

C Find the requested spectral period and corresponding coefficients
      nPer = 12

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1  = period(1)
         c1T     = c1(1)
         c2T     = c2(1)
         c3T     = c3(1)
         c4T     = c4(1)
         sig0T   = sig0(1)
         goto 1011
      elseif (specT .ne. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

      write (*,*)
      write (*,*) 'Atkinson and Boore (1995) EUS (mbLG) atttenuation model'
      write (*,*) 'is not defined for a spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),c1(count1),c1(count2),
     +             specT,c1T,iflag)
      call S24_interp (period(count1),period(count2),c2(count1),c2(count2),
     +             specT,c2T,iflag)
      call S24_interp (period(count1),period(count2),c3(count1),c3(count2),
     +             specT,c3T,iflag)
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),
     +             specT,c4T,iflag)
      call S24_interp (period(count1),period(count2),sig0(count1),sig0(count2),
     +             specT,sig0T,iflag)

 1011 period1 = specT

c     Set atten name
      attenName = 'Atkinson & Boore, (1995), mbLG, Eq Model '

      lnY = c1T + c2T*(m-6) + c3T*(m-6)**2 -
     1             alog(dist) - c4T*dist

C    Calculate the sigma from the estimate in magnitude conversion.
c    Take partial of ground motion wrt magnitude.
       mgsigma = (c2T*m + 2.0*c3T*(m-6.0))*(c2T*m +
     1            2.0*c3T*(m-6.0))*0.15*0.15

c     Convert to spectral acceleration in gal
      lnY = lnY + 6.89

      sgmod = sig0T
      sigma = sqrt(mgsigma*mgsigma+sgmod*sgmod)
      return
      end

c -------------------------------------------------------------------
C *** Toro et al. (1996) Horizontal MidCon. *************************
c -------------------------------------------------------------------

      subroutine S02_TAS96 ( m, dist, lnY, sigma, specT,
     1                  attenName, period1,iflag )

      implicit none

      integer MAXPER
      real lnY, m, dist, sigma, period1, rm, rmmax, twoPi
      real specT, c1T, c2T, c3T, c4T, c5T, c6T, c7T
      integer nper, count1, count2, iflag, i
      character*80 attenName
      real sigma_aMag, sigma_aRjb
      real sig_aMag1T, sig_aMag2T, sig_aMag3T
      real sig_aRjb1T, sig_aRjb2T

      parameter (MAXPER=8)
      real c1(MAXPER), c2(MAXPER), c3(MAXPER), c4(MAXPER), c5(MAXPER)
      real c6(MAXPER), c7(MAXPER), period(MAXPER)
      real sig_aMag1(MAXPER), sig_aMag2(MAXPER), sig_aMag3(MAXPER)
      real sig_aRjb1(MAXPER), sig_aRjb2(MAXPER)
      data period / 0.0, 0.029, 0.04, 0.10, 0.20, 0.40, 1.00, 2.00 /
      data c1 / 2.20, 4.00, 3.68, 2.37, 1.73, 1.07, 0.09, -0.74 /
      data c2 / 0.81, 0.79, 0.80, 0.81, 0.84, 1.05, 1.42, 1.86 /
      data c3 / 0.00, 0.00, 0.00, 0.00, 0.00, -0.10, -0.20, -0.31 /
      data c4 / 1.27, 1.57, 1.46, 1.10, 0.98, 0.93, 0.90, 0.92 /
      data c5 / 1.16, 1.83, 1.77, 1.02, 0.66, 0.56, 0.49, 0.46 /
      data c6 / 0.0021, 0.0008, 0.0013, 0.0040, 0.0042, 0.0033, 0.0023,
     1          0.0017 /
      data c7 /  9.3, 11.1, 10.5,  8.3,  7.5,  7.1,  6.8,  6.9 /
      data sig_aMag1 / 0.55, 0.62, 0.62, 0.59, 0.60, 0.63, 0.63, 0.61 /
      data sig_aMag2 / 0.59, 0.63, 0.63, 0.61, 0.64, 0.68, 0.64, 0.62 /
      data sig_aMag3 / 0.50, 0.50, 0.50, 0.50, 0.56, 0.64, 0.67, 0.66 /
      data sig_aRjb1 / 0.54, 0.62, 0.57, 0.50, 0.45, 0.45, 0.45, 0.45 /
      data sig_aRjb2 / 0.20, 0.35, 0.29, 0.17, 0.12, 0.12, 0.12, 0.12 /
      twoPi = 2.0 * 3.1415296

C Find the requested spectral period and corresponding coefficients
      nPer = 8

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1 = period(1)
         c1T     = c1(1)
         c2T     = c2(1)
         c3T     = c3(1)
         c4T     = c4(1)
         c5T     = c5(1)
         c6T     = c6(1)
         c7T     = c7(1)
         sig_aMag1T = sig_aMag1(1)
         sig_aMag2T = sig_aMag2(1)
         sig_aMag3T = sig_aMag2(1)
         sig_aRjb1T = sig_aRjb1(1)
         sig_aRjb2T = sig_aRjb1(1)
         goto 1011
      elseif (specT .ne. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

      write (*,*)
      write (*,*) 'Toro et al. (1996) MidCont. (Mw) atttenuation model'
      write (*,*) 'is not defined for a spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),c1(count1),c1(count2),
     +             specT,c1T,iflag)
      call S24_interp (period(count1),period(count2),c2(count1),c2(count2),
     +             specT,c2T,iflag)
      call S24_interp (period(count1),period(count2),c3(count1),c3(count2),
     +             specT,c3T,iflag)
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),
     +             specT,c4T,iflag)
      call S24_interp (period(count1),period(count2),c5(count1),c5(count2),
     +             specT,c5T,iflag)
      call S24_interp (period(count1),period(count2),c6(count1),c6(count2),
     +             specT,c6T,iflag)
      call S24_interp (period(count1),period(count2),c7(count1),c7(count2),
     +             specT,c7T,iflag)
      call S24_interp (period(count1),period(count2),sig_aMag1(count1),sig_aMag1(count2),
     +             specT,sig_aMag1T,iflag)
      call S24_interp (period(count1),period(count2),sig_aMag2(count1),sig_aMag2(count2),
     +             specT,sig_aMag2T,iflag)
      call S24_interp (period(count1),period(count2),sig_aMag3(count1),sig_aMag3(count2),
     +             specT,sig_aMag3T,iflag)
      call S24_interp (period(count1),period(count2),sig_aRjb1(count1),sig_aRjb1(count2),
     +             specT,sig_aRjb1T,iflag)
      call S24_interp (period(count1),period(count2),sig_aRjb2(count1),sig_aRjb2(count2),
     +             specT,sig_aRjb2T,iflag)

 1011 period1 = specT

c     Set atten name
      attenName = 'Toro et al. MidCont. (1996)'

      rm = sqrt(dist*dist+c7T*c7T)
      if (rm .le. 100) then
         rmmax=0.0
      else
         rmmax = alog(rm/100.0)
      endif

      lnY = c1T + c2T*(m-6) + c3T*(m-6)**2 -
     1    c4T*alog(rm) - rmmax*(c5T-c4T) - c6T*rm

c     Convert to spectral acceleration in gal
      lnY = lnY + 6.89

C     Set the sigma value.
c      if (specT .eq. 2.0) then
c         sigma = 0.34 + 0.06*(m-6.0)
c      else
c         sigma = 0.36 + 0.07*(m-6.0)
c      endif
c     Aleatory uncertainty - Magnitude Dependence
      if ( m <= 5 ) then
        sigma_aMag = sig_aMag1T
      elseif ( m <= 5.5 ) then
        sigma_aMag = sig_aMag1T+(sig_aMag2T-sig_aMag1T)/(5.5-5) * (m-5)
      elseif ( m <= 8.0 ) then
        sigma_aMag = sig_aMag2T+(sig_aMag3T-sig_aMag2T)/(8.0-5.5) * (m-5.5)
      elseif ( m > 8.0 ) then
        sigma_aMag = sig_aMag3T
      endif

c     Aleatory uncertainty - Distance Dependence
      if ( dist <= 5 ) then
        sigma_aRjb = sig_aRjb1T
      elseif ( dist <= 20 ) then
        sigma_aRjb = sig_aRjb1T + (sig_aRjb2T - sig_aRjb1T)/(20.-5.) * (dist-5)
      elseif ( dist > 20 ) then
        sigma_aRjb = sig_aRjb2T
      endif

      sigma = SQRT(sigma_aMag**2 + sigma_aRjb**2)

      return
      end

c -------------------------------------------------------------------
C *** Toro et al. (1996) Horizontal MidCon., MLg magnitude **********
c -------------------------------------------------------------------

      subroutine S02_TAS96MLg ( m, dist, lnY, sigma, specT,
     1                  attenName, period1,iflag )

      implicit none

      integer MAXPER
      real lnY, m, dist, sigma, period1, rm, rmmax, twoPi
      real specT, c1T, c2T, c3T, c4T, c5T, c6T, c7T
      integer nper, count1, count2, iflag, i
      character*80 attenName
      real sigma_aMag, sigma_aRjb
      real sig_aMag1T, sig_aMag2T, sig_aMag3T
      real sig_aRjb1T, sig_aRjb2T

      parameter (MAXPER=8)
      real c1(MAXPER), c2(MAXPER), c3(MAXPER), c4(MAXPER), c5(MAXPER)
      real c6(MAXPER), c7(MAXPER), period(MAXPER)
      real sig_aMag1(MAXPER), sig_aMag2(MAXPER), sig_aMag3(MAXPER)
      real sig_aRjb1(MAXPER), sig_aRjb2(MAXPER)
      data period / 0.0, 0.029, 0.04, 0.10, 0.20, 0.40, 1.00, 2.00 /
      data c1 / 2.07, 3.87, 3.54, 2.36, 1.60, 0.90, -0.12, -0.97 /
      data c2 / 1.20, 1.19, 1.19, 1.23, 1.24, 1.70, 2.05, 2.52 /
      data c3 / 0.00, 0.00, 0.00, 0.00, 0.00, -0.26, -0.34, -0.47 /
      data c4 / 1.28, 1.58, 1.46, 1.12, 0.98, 0.94, 0.90, 0.93 /
      data c5 / 1.23, 1.90, 1.84, 1.05, 0.74, 0.65, 0.59, 0.60 /
      data c6 / 0.0018, 0.0005, 0.0010, 0.0043, 0.0039, 0.0030, 0.0019,
     1          0.0012 /
      data c7 /  9.3, 11.1, 10.5,  8.5,  7.5,  7.2,  6.8,  7.0 /
      data sig_aMag1 / 0.58, 0.57, 0.57, 0.54, 0.54, 0.58, 0.62, 0.63 /
      data sig_aMag2 / 0.58, 0.58, 0.58, 0.57, 0.63, 0.70, 0.81, 0.81 /
      data sig_aMag3 / 0.44, 0.44, 0.44, 0.44, 0.51, 0.59, 0.61, 0.61 /
      data sig_aRjb1 / 0.54, 0.62, 0.57, 0.50, 0.45, 0.45, 0.45, 0.45 /
      data sig_aRjb2 / 0.20, 0.35, 0.29, 0.17, 0.12, 0.12, 0.12, 0.12 /
      twoPi = 2.0 * 3.1415296

C Find the requested spectral period and corresponding coefficients
      nPer = 8

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1 = period(1)
         c1T     = c1(1)
         c2T     = c2(1)
         c3T     = c3(1)
         c4T     = c4(1)
         c5T     = c5(1)
         c6T     = c6(1)
         c7T     = c7(1)
         sig_aMag1T = sig_aMag1(1)
         sig_aMag2T = sig_aMag2(1)
         sig_aMag3T = sig_aMag2(1)
         sig_aRjb1T = sig_aRjb1(1)
         sig_aRjb2T = sig_aRjb1(1)
         goto 1011
      elseif (specT .ne. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

      write (*,*)
      write (*,*) 'Toro et al. (1996) MidCont. (MLg) atttenuation model'
      write (*,*) 'is not defined for a spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),c1(count1),c1(count2),
     +             specT,c1T,iflag)
      call S24_interp (period(count1),period(count2),c2(count1),c2(count2),
     +             specT,c2T,iflag)
      call S24_interp (period(count1),period(count2),c3(count1),c3(count2),
     +             specT,c3T,iflag)
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),
     +             specT,c4T,iflag)
      call S24_interp (period(count1),period(count2),c5(count1),c5(count2),
     +             specT,c5T,iflag)
      call S24_interp (period(count1),period(count2),c6(count1),c6(count2),
     +             specT,c6T,iflag)
      call S24_interp (period(count1),period(count2),c7(count1),c7(count2),
     +             specT,c7T,iflag)
      call S24_interp (period(count1),period(count2),sig_aMag1(count1),sig_aMag1(count2),
     +             specT,sig_aMag1T,iflag)
      call S24_interp (period(count1),period(count2),sig_aMag2(count1),sig_aMag2(count2),
     +             specT,sig_aMag2T,iflag)
      call S24_interp (period(count1),period(count2),sig_aMag3(count1),sig_aMag3(count2),
     +             specT,sig_aMag3T,iflag)
      call S24_interp (period(count1),period(count2),sig_aRjb1(count1),sig_aRjb1(count2),
     +             specT,sig_aRjb1T,iflag)
      call S24_interp (period(count1),period(count2),sig_aRjb2(count1),sig_aRjb2(count2),
     +             specT,sig_aRjb2T,iflag)

 1011 period1 = specT

c     Set atten name
      attenName = 'Toro et al. MidCon. (1996), MLg Magnitude'

      rm = sqrt(dist*dist+c7T*c7T)

      if (rm .le. 100) then
         rmmax=0.0
      else
         rmmax = alog(rm/100.0)
      endif

      lnY = c1T + c2T*(m-6) + c3T*(m-6)**2 -
     1    c4T*alog(rm) - rmmax*(c5T-c4T) - c6T*rm

c     Convert to spectral acceleration in gal
      lnY = lnY + 6.89

C     Aleatory uncertainty - Magnitude Dependence
      if ( m <= 5 ) then
        sigma_aMag = sig_aMag1T
      elseif ( m <= 6.0 ) then
        sigma_aMag = sig_aMag1T+(sig_aMag2T-sig_aMag1T)/(6.0-5) * (m-5)
      elseif ( m <= 7.5 ) then
        sigma_aMag = sig_aMag2T+(sig_aMag3T-sig_aMag2T)/(7.5-6.0) * (m-6.0)
      elseif ( m > 7.5 ) then
        sigma_aMag = sig_aMag3T
      endif
C     Aleatory uncertainty - Distance Dependence
      if ( dist <= 5 ) then
        sigma_aRjb = sig_aRjb1T
      elseif ( dist <= 20 ) then
        sigma_aRjb = sig_aRjb1T + (sig_aRjb2T - sig_aRjb1T)/(20.-5.) * (dist-5)
      elseif ( dist > 20 ) then
        sigma_aRjb = sig_aRjb2T
      endif

      sigma = SQRT(sigma_aMag**2 + sigma_aRjb**2)

      return
      end

c -------------------------------------------------------------------
C *** Toro et al. (1996) Horizontal Gulf ****************************
c -------------------------------------------------------------------

      subroutine S02_TAS96Gulf ( m, dist, lnY, sigma, specT,
     1                  attenName, period1,iflag )

      implicit none

      integer MAXPER
      real lnY, m, dist, sigma, period1, rm, rmmax, twoPi
      real specT, c1T, c2T, c3T, c4T, c5T, c6T, c7T
      integer nper, count1, count2, iflag, i
      character*80 attenName
      real sigma_aMag, sigma_aRjb
      real sig_aMag1T, sig_aMag2T, sig_aMag3T
      real sig_aRjb1T, sig_aRjb2T

      parameter (MAXPER=8)
      real c1(MAXPER), c2(MAXPER), c3(MAXPER), c4(MAXPER), c5(MAXPER)
      real c6(MAXPER), c7(MAXPER), period(MAXPER)
      real sig_aMag1(MAXPER), sig_aMag2(MAXPER), sig_aMag3(MAXPER)
      real sig_aRjb1(MAXPER), sig_aRjb2(MAXPER)
      data period / 0.0, 0.029, 0.04, 0.10, 0.20, 0.40, 1.00, 2.00 /
      data c1 / 2.91, 4.81, 5.19, 5.08, 3.10, 1.64, 0.24, -0.81 /
      data c2 / 0.92, 0.91, 0.91, 1.00, 0.92, 1.06, 1.31, 1.72 /
      data c3 / 0.00, 0.00, 0.00, 0.00, 0.00, -0.08, -0.15, -0.26 /
      data c4 / 1.49, 1.89, 1.96, 1.87, 1.34, 0.99, 0.79, 0.74 /
      data c5 / 1.61, 1.80, 1.96, 2.52, 1.95, 1.27, 0.82, 0.71 /
      data c6 / 0.0014, 0.0008, 0.0004, 0.0002, 0.0017, 0.0036, 0.0034,
     1          0.0025 /
      data c7 / 10.9, 11.9, 12.9, 14.1, 11.4,  8.9,  7.2,  6.6 /
      data sig_aMag1 / 0.55, 0.62, 0.62, 0.59, 0.60, 0.63, 0.63, 0.61 /
      data sig_aMag2 / 0.59, 0.63, 0.63, 0.61, 0.64, 0.68, 0.64, 0.62 /
      data sig_aMag3 / 0.50, 0.50, 0.50, 0.50, 0.56, 0.64, 0.67, 0.66 /
      data sig_aRjb1 / 0.48, 0.68, 0.63, 0.53, 0.50, 0.50, 0.51, 0.54 /
      data sig_aRjb2 / 0.30, 0.47, 0.47, 0.38, 0.33, 0.34, 0.39, 0.39 /
      twoPi = 2.0 * 3.1415296

C Find the requested spectral period and corresponding coefficients
      nPer = 8

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1 = period(1)
         c1T     = c1(1)
         c2T     = c2(1)
         c3T     = c3(1)
         c4T     = c4(1)
         c5T     = c5(1)
         c6T     = c6(1)
         c7T     = c7(1)
         sig_aMag1T = sig_aMag1(1)
         sig_aMag2T = sig_aMag2(1)
         sig_aMag3T = sig_aMag2(1)
         sig_aRjb1T = sig_aRjb1(1)
         sig_aRjb2T = sig_aRjb1(1)
         goto 1011
      elseif (specT .ne. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

      write (*,*)
      write (*,*) 'Toro et al. (1996) Gulf (Mw) atttenuation model'
      write (*,*) 'is not defined for a spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),c1(count1),c1(count2),
     +             specT,c1T,iflag)
      call S24_interp (period(count1),period(count2),c2(count1),c2(count2),
     +             specT,c2T,iflag)
      call S24_interp (period(count1),period(count2),c3(count1),c3(count2),
     +             specT,c3T,iflag)
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),
     +             specT,c4T,iflag)
      call S24_interp (period(count1),period(count2),c5(count1),c5(count2),
     +             specT,c5T,iflag)
      call S24_interp (period(count1),period(count2),c6(count1),c6(count2),
     +             specT,c6T,iflag)
      call S24_interp (period(count1),period(count2),c7(count1),c7(count2),
     +             specT,c7T,iflag)
      call S24_interp (period(count1),period(count2),sig_aMag1(count1),sig_aMag1(count2),
     +             specT,sig_aMag1T,iflag)
      call S24_interp (period(count1),period(count2),sig_aMag2(count1),sig_aMag2(count2),
     +             specT,sig_aMag2T,iflag)
      call S24_interp (period(count1),period(count2),sig_aMag3(count1),sig_aMag3(count2),
     +             specT,sig_aMag3T,iflag)
      call S24_interp (period(count1),period(count2),sig_aRjb1(count1),sig_aRjb1(count2),
     +             specT,sig_aRjb1T,iflag)
      call S24_interp (period(count1),period(count2),sig_aRjb2(count1),sig_aRjb2(count2),
     +             specT,sig_aRjb2T,iflag)

 1011 period1 = specT

c     Set atten name
      attenName = 'Toro et al. Gulf (1996)'

      rm = sqrt(dist*dist+c7T*c7T)

      if (rm .le. 100) then
         rmmax=0.0
      else
         rmmax = alog(rm/100.0)
      endif

      lnY = c1T + c2T*(m-6) + c3T*(m-6)**2 -
     1    c4T*alog(rm) - rmmax*(c5T-c4T) - c6T*rm

c     Convert to spectral acceleration in gal
      lnY = lnY + 6.89

C     Set the sigma value.
c      if (specT .eq. 2.00) then
c         sigma = 0.34 + 0.06*(m-6.0)
c      else
c         sigma = 0.36 + 0.07*(m-6.0)
c      endif
C     Aleatory uncertainty - Magnitude Dependence
      if ( m <= 5 ) then
        sigma_aMag = sig_aMag1T
      elseif ( m <= 5.5 ) then
        sigma_aMag = sig_aMag1T+(sig_aMag2T-sig_aMag1T)/(5.5-5) * (m-5)
      elseif ( m <= 8.0 ) then
        sigma_aMag = sig_aMag2T+(sig_aMag3T-sig_aMag2T)/(8.0-5.5) * (m-5.5)
      elseif ( m > 8.0 ) then
        sigma_aMag = sig_aMag3T
      endif
C     Aleatory uncertainty - Distance Dependence
      if ( dist <= 5 ) then
        sigma_aRjb = sig_aRjb1T
      elseif ( dist <= 20 ) then
        sigma_aRjb = sig_aRjb1T + (sig_aRjb2T - sig_aRjb1T)/(20.-5.) * (dist-5)
      elseif ( dist > 20 ) then
        sigma_aRjb = sig_aRjb2T
      endif

      sigma = SQRT(sigma_aMag**2 + sigma_aRjb**2)

      return
      end

c -------------------------------------------------------------------
C *** Toro et al. (1996) Horizontal Gulf, MLg Magnitude *************
c -------------------------------------------------------------------

      subroutine S02_TAS96GulfMLg ( m, dist, lnY, sigma, specT,
     1                  attenName, period1,iflag )

      implicit none

      integer MAXPER
      real lnY, m, dist, sigma, period1, rm, rmmax, twoPi
      real specT, c1T, c2T, c3T, c4T, c5T, c6T, c7T
      integer nper, count1, count2, iflag, i
      character*80 attenName
      real sigma_aMag, sigma_aRjb
      real sig_aMag1T, sig_aMag2T, sig_aMag3T
      real sig_aRjb1T, sig_aRjb2T

      parameter (MAXPER=8)
      real c1(MAXPER), c2(MAXPER), c3(MAXPER), c4(MAXPER), c5(MAXPER)
      real c6(MAXPER), c7(MAXPER), period(MAXPER)
      real sig_aMag1(MAXPER), sig_aMag2(MAXPER), sig_aMag3(MAXPER)
      real sig_aRjb1(MAXPER), sig_aRjb2(MAXPER)
      data period / 0.0, 0.029, 0.04, 0.10, 0.20, 0.40, 1.00, 2.00 /
      data c1 / 2.80, 4.68, 5.08, 4.65, 3.00, 1.49, 0.06, -1.01 /
      data c2 / 1.31, 1.30, 1.29, 1.30, 1.31, 1.74, 1.97, 2.38 /
      data c3 / 0.00, 0.00, 0.00, 0.00, 0.00, -0.26, -0.32, -0.42 /
      data c4 / 1.49, 1.89, 1.97, 1.78, 1.35, 1.00, 0.80, 0.75 /
      data c5 / 1.68, 1.88, 2.04, 2.41, 2.03, 1.36, 0.92, 0.83 /
      data c6 / 0.0017, 0.0005, 0.0000, 0.0000, 0.0014, 0.0032, 0.0030,
     1          0.0032 /
      data c7 / 10.9, 11.9, 12.9, 13.8, 11.4,  9.0,  7.3,  6.8 /
      data sig_aMag1 / 0.58, 0.57, 0.57, 0.54, 0.54, 0.58, 0.62, 0.63 /
      data sig_aMag2 / 0.58, 0.58, 0.58, 0.57, 0.63, 0.70, 0.81, 0.81 /
      data sig_aMag3 / 0.44, 0.44, 0.44, 0.44, 0.51, 0.59, 0.61, 0.61 /
      data sig_aRjb1 / 0.48, 0.68, 0.63, 0.53, 0.50, 0.50, 0.51, 0.54 /
      data sig_aRjb2 / 0.30, 0.47, 0.47, 0.38, 0.33, 0.34, 0.39, 0.39 /
      twoPi = 2.0 * 3.1415296

C Find the requested spectral period and corresponding coefficients
      nPer = 8

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1 = period(1)
         c1T     = c1(1)
         c2T     = c2(1)
         c3T     = c3(1)
         c4T     = c4(1)
         c5T     = c5(1)
         c6T     = c6(1)
         c7T     = c7(1)
         sig_aMag1T = sig_aMag1(1)
         sig_aMag2T = sig_aMag2(1)
         sig_aMag3T = sig_aMag2(1)
         sig_aRjb1T = sig_aRjb1(1)
         sig_aRjb2T = sig_aRjb1(1)
         goto 1011
      elseif (specT .ne. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

      write (*,*)
      write (*,*) 'Toro et al. (1996) Gulf (MLg) atttenuation model'
      write (*,*) 'is not defined for a spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),c1(count1),c1(count2),
     +             specT,c1T,iflag)
      call S24_interp (period(count1),period(count2),c2(count1),c2(count2),
     +             specT,c2T,iflag)
      call S24_interp (period(count1),period(count2),c3(count1),c3(count2),
     +             specT,c3T,iflag)
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),
     +             specT,c4T,iflag)
      call S24_interp (period(count1),period(count2),c5(count1),c5(count2),
     +             specT,c5T,iflag)
      call S24_interp (period(count1),period(count2),c6(count1),c6(count2),
     +             specT,c6T,iflag)
      call S24_interp (period(count1),period(count2),c7(count1),c7(count2),
     +             specT,c7T,iflag)
      call S24_interp (period(count1),period(count2),sig_aMag1(count1),sig_aMag1(count2),
     +             specT,sig_aMag1T,iflag)
      call S24_interp (period(count1),period(count2),sig_aMag2(count1),sig_aMag2(count2),
     +             specT,sig_aMag2T,iflag)
      call S24_interp (period(count1),period(count2),sig_aMag3(count1),sig_aMag3(count2),
     +             specT,sig_aMag3T,iflag)
      call S24_interp (period(count1),period(count2),sig_aRjb1(count1),sig_aRjb1(count2),
     +             specT,sig_aRjb1T,iflag)
      call S24_interp (period(count1),period(count2),sig_aRjb2(count1),sig_aRjb2(count2),
     +             specT,sig_aRjb2T,iflag)

 1011 period1 = specT

c     Set atten name
      attenName = 'Toro et al. Gulf (1996), MLg magnitude'

      rm = sqrt(dist*dist+c7T*c7T)

      if (rm .le. 100) then
         rmmax=0.0
      else
         rmmax = alog(rm/100.0)
      endif

      lnY = c1T + c2T*(m-6) + c3T*(m-6)**2 -
     1    c4T*alog(rm) - rmmax*(c5T-c4T) - c6T*rm

c     Convert to spectral acceleration in gal
      lnY = lnY + 6.89

c     Aleatory uncertainty - Magnitude Dependence
      if ( m <= 5 ) then
        sigma_aMag = sig_aMag1T
      elseif ( m <= 6.0 ) then
        sigma_aMag = sig_aMag1T+(sig_aMag2T-sig_aMag1T)/(6.0-5) * (m-5)
      elseif ( m <= 7.5 ) then
        sigma_aMag = sig_aMag2T+(sig_aMag3T-sig_aMag2T)/(7.5-6.0) * (m-6.0)
      elseif ( m > 7.5 ) then
        sigma_aMag = sig_aMag3T
      endif

c     Aleatory uncertainty - Distance Dependence
      if ( dist <= 5 ) then
        sigma_aRjb = sig_aRjb1T
      elseif ( dist <= 20 ) then
        sigma_aRjb = sig_aRjb1T + (sig_aRjb2T - sig_aRjb1T)/(20.-5.) * (dist-5)
      elseif ( dist > 20 ) then
        sigma_aRjb = sig_aRjb2T
      endif

      sigma = SQRT(sigma_aMag**2 + sigma_aRjb**2)

      return
      end

c ---------------------------------------------------------------
C *** McVerry et al. (1993) New Zealand Horizontal (PGA only) ***
c ---------------------------------------------------------------

      subroutine S02_mcverry93 ( mag, rupDist, lnY, sigma, attenName, ftype)

      implicit none

      real mag, rupDist, lnY, sigma, pgalog10, ftype
      character*80 attenName
      attenName = 'McVerry et al. (1993)'

c     Model A (all depths)
C Note the Log(rupDist) term was increased by 1 km to prevent the
C code from bombing for a rupDist of 0 km.
C 11/15/95

      pgalog10 = 0.209*mag - 0.00297*rupDist - 0.449*alog10(rupDist+1)
     1           - 1.434
      lnY = 2.303 * pgaLog10 + 6.889
      sigma = 0.63

c     modify for depth (50% increase)  *NAA modification*
      if ( ftype .eq. 1 ) then
        lnY = lnY + 0.4
      endif

      return
      end

c ------------------------------------------------------------
C *** Fukushima (1990) Horizontal Rock (PGA only) ************
c ------------------------------------------------------------

      subroutine S02_fukushima90 ( mag, rupDist, lnY, sigma, attenName )

      implicit none

      real mag, rupDist, lnY, sigma, pgaLog10
      character*80 attenName
      attenName = 'McVerry et al. (1993)'

c     Model A (all depths)
      pgaLog10 = 0.41*mag - 0.0034*rupDist
     1           - alog10(rupDist+0.032*10.**(0.41*mag)) + 1.3
      lnY = 2.303 * pgaLog10
      sigma = 0.48

c     modify for rock (60 percent of soil)
      lnY = lnY - 0.5

      return
      end

c ------------------------------------------------------------
C *** Loh High Speed Rail Horizontal (PGA only) **************
c ------------------------------------------------------------

      subroutine S02_HighSpeedRail ( mag, rupDist, lnY, sigma,
     1            attenName, period )

      implicit none

      real mag, rupDist, lnY, sigma, period, lnY1, lnY2, lnY3
      character*80 attenName

c     Kanai form
      lnY1  = 0.137 + 0.708*mag -1.7266*alog(rupDist+40)

c     Campbell From
      lnY2 = -4.02 + 1.298*mag -1.739*alog(rupDist+0.177*exp(0.746*mag))

c     New Joyner Boore Form
      lnY3 = -5.22 + 0.7529*mag - 0.013*mag**2 - 0.0027*rupDist
     1       -0.5258*alog(rupDist+1)

c     lnY = (lnY1+lnY2+lnY3)/3.
c     sigma = 0.647
      lnY = lnY3
      sigma = 0.617
      attenName = 'High Speed Rail - Modified Joyner Boore, PGA'
      period = 0.0

c     Convert from g to gal
      lnY = lnY + 6.89
      return
      end

c ------------------------------------------------------------
C *** Loh (1996) Horizontal Rock (PGA only) ******************
c ------------------------------------------------------------
      subroutine S02_Loh96 ( mag, rupDist, lnY, sigma, attenName, period )

      implicit none

      real mag, rupDist, lnY, sigma, period
      character*80 attenName

c     Loh 1996
      lnY = -3.601 + 1.1158*mag -
     1      1.6471*alog(rupdist+0.141*exp(0.656*mag))

      sigma = 0.617
      attenName = 'Loh (1996), PGA'
      period = 0.0

c     Convert from g to gal
      lnY = lnY + 6.89
      return
      end

c ------------------------------------------------------------
C *** Campbell and Bozorgnia (2003) Horizontal ***************
c ------------------------------------------------------------
      subroutine S02_Camp03_H ( m, seismodist, jbDist, lnY, sigma,
     1                      specT, period1, Svfs, Ssr, Sfr,
     2                      Frv, Fth, hwflag, iflag )

c     Campbell and Bozorgnia (2003) - Horizontal

      implicit none

      integer MAXPER
      parameter (MAXPER=15)
      real seismodist, m, lnY, sigma, period1, jbDist, gs
      integer iflag, hwflag, i
      real Svfs, Ssr, Sfr, Frv, Fth, sum
      real period(MAXPER)
      real c1(MAXPER), c2(MAXPER), c3(MAXPER), c4(MAXPER),
     1     c5(MAXPER), c6(MAXPER), c7(MAXPER), c8(MAXPER),
     2     c9(MAXPER), c10(MAXPER), c11(MAXPER), c12(MAXPER),
     3     c13(MAXPER), c14(MAXPER), c15(MAXPER), c16(MAXPER),
     4     c17(MAXPER)
      real specT, c1T, c2T, c3T, c4T, c5T, c6T, c7T, c8T
      real c9T, c10T, c11T, c12T, c13T, c14T, c15T, c16T, c17T
      integer nper, count1, count2
      real hw, fhwm, fhws, f1, f2, f3, f4, f5

      data period / 0.00, 0.05, 0.075, 0.10, 0.15, 0.20, 0.30,
     1              0.40, 0.50, 0.75,  1.00, 1.50, 2.00, 3.00, 4.00 /
      data c1 / -4.033, -3.740, -3.076, -2.661, -2.270, -2.771,
     1          -2.999, -3.511, -3.556, -3.709, -3.867, -4.093,
     2          -4.311, -4.817, -5.211 /
      data c2 / 0.812, 0.812, 0.812, 0.812, 0.812, 0.812, 0.812,
     1          0.812, 0.812, 0.812, 0.812, 0.812, 0.812, 0.812,
     2          0.812 /
      data c3 / 0.036,  0.036,  0.050,  0.060,  0.041,  0.030, 0.007,
     1         -0.015, -0.035, -0.071, -0.101, -0.150, -0.180,
     2         -0.193, -0.202 /
      data c4 /-1.061, -1.121, -1.252, -1.308, -1.324, -1.153,
     1         -1.080, -0.964, -0.964, -0.964, -0.964, -0.964, -0.964,
     2         -0.964, -0.964 /
      data c5 / 0.041, 0.058, 0.121, 0.166, 0.212, 0.098, 0.059,
     1          0.024, 0.023, 0.021, 0.019, 0.019, 0.019, 0.019, 0.019 /
      data c6 / -0.005, -0.004, -0.005, -0.009, -0.033, -0.014, -0.007,
     1          -0.002, -0.002, -0.002,  0.000,  0.000,  0.000,  0.000,
     2           0.000 /
      data c7 / -0.018, -0.028, -0.051, -0.068, -0.081, -0.038, -0.022,
     1          -0.005, -0.004, -0.002,  0.000,  0.000,  0.000,  0.000,
     2           0.000 /
      data c8 / 0.766, 0.724, 0.648, 0.621, 0.613, 0.704, 0.752, 0.842,
     1          0.842, 0.842, 0.842, 0.842, 0.842, 0.842, 0.842 /
      data c9 / 0.034,  0.032,  0.040,  0.046,  0.031,  0.026, 0.007,
     1         -0.016, -0.036, -0.074, -0.105, -0.155, -0.187,
     2         -0.200, -0.209 /
      data c10 / 0.343, 0.302, 0.243, 0.224, 0.318, 0.296, 0.359,
     1           0.379, 0.406, 0.347, 0.329, 0.217, 0.060,
     2          -0.079, -0.061 /
      data c11 / 0.351, 0.362, 0.333, 0.313, 0.344, 0.342, 0.385,
     1           0.438, 0.479, 0.419, 0.338, 0.188, 0.064, 0.021,
     2           0.057 /
      data c12 / -0.123, -0.140, -0.150, -0.146, -0.176,
     1           -0.148, -0.162, -0.078, -0.122, -0.108, -0.073,
     2           -0.079, -0.124, -0.154, -0.054 /
      data c13 / -0.138, -0.158, -0.196, -0.253, -0.267, -0.183,
     1           -0.157, -0.129, -0.130, -0.124, -0.072, -0.056,
     2           -0.116, -0.117, -0.261 /
      data c14 / -0.289, -0.205, -0.208, -0.258, -0.284, -0.359,
     1           -0.585, -0.557, -0.701, -0.796, -0.858, -0.954,
     2           -0.916, -0.873, -0.889 /
      data c15 / 0.370, 0.370, 0.370, 0.370, 0.370, 0.370, 0.370,
     1           0.370, 0.370, 0.331, 0.281, 0.210, 0.160, 0.089,
     2           0.039 /
      data c16 / 0.920, 0.940, 0.952, 0.958, 0.974, 0.981, 0.984,
     1           0.987, 0.990, 1.021, 1.021, 1.021, 1.021, 1.021,
     2           1.021 /
      data c17 / 0.219, 0.239, 0.251, 0.257, 0.273, 0.280, 0.283,
     1           0.286, 0.289, 0.320, 0.320, 0.320, 0.320, 0.320,
     2           0.320 /

C Find the requested spectral period and corresponding coefficients
      nPer = 15
C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1 = period(1)
         c1T     = c1(1)
         c2T     = c2(1)
         c3T     = c3(1)
         c4T     = c4(1)
         c5T     = c5(1)
         c6T     = c6(1)
         c7T     = c7(1)
         c8T     = c8(1)
         c9T     = c9(1)
         c10T    = c10(1)
         c11T    = c11(1)
         c12T    = c12(1)
         c13T    = c13(1)
         c14T    = c14(1)
         c15T    = c15(1)
         c16T    = c16(1)
         c17T    = c17(1)
         goto 1011
      elseif (specT .ne. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

      write (*,*)
      write (*,*) 'Campbell and Bozorgnia (2003) Horizonal'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020       call S24_interp (period(count1),period(count2),c1(count1),c1(count2),
     +                   specT,c1T,iflag)
            call S24_interp (period(count1),period(count2),c2(count1),c2(count2),
     +                   specT,c2T,iflag)
            call S24_interp (period(count1),period(count2),c3(count1),c3(count2),
     +                   specT,c3T,iflag)
            call S24_interp (period(count1),period(count2),c4(count1),c4(count2),
     +                   specT,c4T,iflag)
            call S24_interp (period(count1),period(count2),c5(count1),c5(count2),
     +                   specT,c5T,iflag)
            call S24_interp (period(count1),period(count2),c6(count1),c6(count2),
     +                   specT,c6T,iflag)
            call S24_interp (period(count1),period(count2),c7(count1),c7(count2),
     +                   specT,c7T,iflag)
            call S24_interp (period(count1),period(count2),c8(count1),c8(count2),
     +                   specT,c8T,iflag)
            call S24_interp (period(count1),period(count2),c9(count1),c9(count2),
     +                   specT,c9T,iflag)
            call S24_interp (period(count1),period(count2),c10(count1),c10(count2),
     +                   specT,c10T,iflag)
            call S24_interp (period(count1),period(count2),c11(count1),c11(count2),
     +                   specT,c11T,iflag)
            call S24_interp (period(count1),period(count2),c12(count1),c12(count2),
     +                   specT,c12T,iflag)
            call S24_interp (period(count1),period(count2),c13(count1),c13(count2),
     +                   specT,c13T,iflag)
            call S24_interp (period(count1),period(count2),c14(count1),c14(count2),
     +                   specT,c14T,iflag)
            call S24_interp (period(count1),period(count2),c15(count1),c15(count2),
     +                   specT,c15T,iflag)
            call S24_interp (period(count1),period(count2),c16(count1),c16(count2),
     +                   specT,c16T,iflag)
            call S24_interp (period(count1),period(count2),c17(count1),c17(count2),
     +                   specT,c17T,iflag)
 1011 period1 = specT

C     Compute ground motion:
      f1 = c2T*m + c3T*(8.5-m)**2.0
      gs = c5T + c6T*(Svfs + Ssr) + c7T*Sfr
      f2 = seismoDist*seismoDist + gs*gs*
     1       (exp(c8T*m+c9T*(8.5-m)**2.0) )**2.0
      f3 = c10T*Frv + c11T*Fth
      f4 = c12T*Svfs + c13T*Ssr + c14T*Sfr

C     Check for HW factor.
      sum = 0.0
      sum = Svfs + Ssr + Sfr
      if (sum .eq. 0.0) then
         f5 = 0.0
      else
         if (hwflag .eq. 1) then
c     Compute HW term
            if (jbDist .lt. 5.0) then
               hw = sum*(5.0 - jbDist)/5.0
            else
               hw = 0.0
            endif
c     Compute Magnitude term
            if (m .lt. 5.5) then
               fhwm = 0.0
            elseif (m .le. 6.5) then
               fhwm = m - 5.5
            else
               fhwm = 1.0
            endif
c     Compute SeismoDist term
            if (seismoDist .ge. 8.0) then
               fhws = c15T
            else
               fhws = c15T*(seismoDist/8.0)
            endif
            f5 = hw*fhwm*fhws*(Frv+Fth)
         else
            f5 = 0.0
         endif

      endif

      LnY = c1T + f1 + c4T*alog(sqrt(f2)) + f3 + f4 + f5

c     Set sigma as a function of Mw
      if ( m .lt. 7.4 ) then
         sigma = c16T - 0.07*m
      else
         sigma = c16T - 0.518
      endif

c     Convert units spectral acceleration in gal
      lnY = lnY + 6.89

      return
      end

c ------------------------------------------------------------------
C *** Campbell and Bozorgnia (2003) Vertical ***********************
c ------------------------------------------------------------------
      subroutine S02_Camp03_V ( m, seismodist, jbDist, lnY, sigma,
     1                      specT, period1, Svfs, Ssr, Sfr,
     2                      Frv, Fth, hwflag, iflag )

c     Campbell and Bozorgnia (2003) - Vertical

      implicit none

      integer MAXPER
      parameter (MAXPER=15)
      real seismodist, m, lnY, sigma, period1, jbDist, gs, sum
      integer iflag, hwflag, i
      real Svfs, Ssr, Sfr, Frv, Fth
      real period(MAXPER)
      real c1(MAXPER), c2(MAXPER), c3(MAXPER), c4(MAXPER),
     1     c5(MAXPER), c6(MAXPER), c7(MAXPER), c8(MAXPER),
     2     c9(MAXPER), c10(MAXPER), c11(MAXPER), c12(MAXPER),
     3     c13(MAXPER), c14(MAXPER), c15(MAXPER), c16(MAXPER),
     4     c17(MAXPER)
      real specT, c1T, c2T, c3T, c4T, c5T, c6T, c7T, c8T
      real c9T, c10T, c11T, c12T, c13T, c14T, c15T, c16T, c17T
      integer nper, count1, count2
      real hw, fhwm, fhws, f1, f2, f3, f4, f5

      data period / 0.00, 0.05, 0.075, 0.10, 0.15, 0.20, 0.30,
     1              0.40, 0.50, 0.75,  1.00, 1.50, 2.00, 3.00, 4.00 /
      data c1 / -3.108, -1.918, -1.504, -1.672, -2.323, -2.998,
     1          -3.721, -4.536, -4.651, -4.903, -4.950, -5.073,
     2          -5.292, -5.748, -6.042 /
      data c2 / 0.756, 0.756, 0.756, 0.756, 0.756, 0.756, 0.756,
     1          0.756, 0.756, 0.756, 0.756, 0.756, 0.756, 0.756,
     2          0.756 /
      data c3 / 0.000,  0.000,  0.000,  0.000,  0.000,  0.000, 0.007,
     1         -0.015, -0.035, -0.071, -0.101, -0.150, -0.180,
     2         -0.193, -0.202 /
      data c4 /-1.287, -1.517, -1.551, -1.473, -1.280, -1.131,
     1         -1.028, -0.812, -0.812, -0.812, -0.812, -0.812, -0.812,
     2         -0.812, -0.812 /
      data c5 / 0.142, 0.309, 0.343, 0.282, 0.171, 0.089, 0.050,
     1          0.012, 0.012, 0.012, 0.012, 0.012, 0.012, 0.012, 0.012 /
      data c6 / 0.046, 0.069, 0.083, 0.062, 0.045, 0.028, 0.010,
     1          0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     2          0.000 /
      data c7 / -0.040, -0.023,  0.000,  0.001,  0.008,  0.004,  0.004,
     1           0.004,  0.000,  0.000,  0.000,  0.000,  0.000,  0.000,
     2           0.000 /
      data c8 / 0.587, 0.498, 0.487, 0.513, 0.591, 0.668, 0.736, 0.931,
     1          0.931, 0.931, 0.931, 0.931, 0.931, 0.931, 0.931 /
      data c9 / 0.000,  0.000,  0.000,  0.000,  0.000,  0.000, 0.007,
     1         -0.018, -0.043, -0.087, -0.124, -0.184, -0.222,
     2         -0.238, -0.248 /
      data c10 / 0.253, 0.058, 0.135, 0.168, 0.223, 0.234, 0.249,
     1           0.299, 0.243, 0.295, 0.266, 0.171, 0.114,
     2           0.179, 0.237 /
      data c11 / 0.173, 0.100, 0.182, 0.210, 0.238, 0.256, 0.328,
     1           0.317, 0.354, 0.418, 0.315, 0.211, 0.115, 0.159,
     2           0.134 /
      data c12 / -0.135, -0.195, -0.224, -0.198, -0.170,
     1           -0.098, -0.026, -0.017, -0.020,  0.078,  0.043,
     2           -0.038,  0.033, -0.010, -0.059 /
      data c13 / -0.138, -0.274, -0.303, -0.275, -0.175, -0.041,
     1            0.082,  0.022,  0.092,  0.091,  0.101, -0.018,
     2           -0.022, -0.047, -0.267 /
      data c14 / -0.256, -0.219, -0.263, -0.252, -0.270, -0.311,
     1           -0.265, -0.257, -0.293, -0.349, -0.481, -0.518,
     2           -0.503, -0.539, -0.606 /
      data c15 / 0.630, 0.630, 0.630, 0.630, 0.630, 0.571, 0.488,
     1           0.428, 0.383, 0.299, 0.240, 0.240, 0.240, 0.240,
     2           0.240 /
      data c16 / 0.975, 1.031, 1.031, 1.031, 1.031, 1.031, 1.031,
     1           1.031, 1.031, 1.031, 1.031, 1.031, 1.031, 1.031,
     2           1.031 /
      data c17 / 0.274, 0.330, 0.330, 0.330, 0.330, 0.330, 0.330,
     1           0.330, 0.330, 0.330, 0.330, 0.330, 0.330, 0.330,
     2           0.330 /

C Find the requested spectral period and corresponding coefficients
      nPer = 15
C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1 = period(1)
         c1T     = c1(1)
         c2T     = c2(1)
         c3T     = c3(1)
         c4T     = c4(1)
         c5T     = c5(1)
         c6T     = c6(1)
         c7T     = c7(1)
         c8T     = c8(1)
         c9T     = c9(1)
         c10T    = c10(1)
         c11T    = c11(1)
         c12T    = c12(1)
         c13T    = c13(1)
         c14T    = c14(1)
         c15T    = c15(1)
         c16T    = c16(1)
         c17T    = c17(1)
         goto 1011
      elseif (specT .ne. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

      write (*,*)
      write (*,*) 'Campbell and Bozorgnia (2003) Vertical'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020       call S24_interp (period(count1),period(count2),c1(count1),c1(count2),
     +                   specT,c1T,iflag)
            call S24_interp (period(count1),period(count2),c2(count1),c2(count2),
     +                   specT,c2T,iflag)
            call S24_interp (period(count1),period(count2),c3(count1),c3(count2),
     +                   specT,c3T,iflag)
            call S24_interp (period(count1),period(count2),c4(count1),c4(count2),
     +                   specT,c4T,iflag)
            call S24_interp (period(count1),period(count2),c5(count1),c5(count2),
     +                   specT,c5T,iflag)
            call S24_interp (period(count1),period(count2),c6(count1),c6(count2),
     +                   specT,c6T,iflag)
            call S24_interp (period(count1),period(count2),c7(count1),c7(count2),
     +                   specT,c7T,iflag)
            call S24_interp (period(count1),period(count2),c8(count1),c8(count2),
     +                   specT,c8T,iflag)
            call S24_interp (period(count1),period(count2),c9(count1),c9(count2),
     +                   specT,c9T,iflag)
            call S24_interp (period(count1),period(count2),c10(count1),c10(count2),
     +                   specT,c10T,iflag)
            call S24_interp (period(count1),period(count2),c11(count1),c11(count2),
     +                   specT,c11T,iflag)
            call S24_interp (period(count1),period(count2),c12(count1),c12(count2),
     +                   specT,c12T,iflag)
            call S24_interp (period(count1),period(count2),c13(count1),c13(count2),
     +                   specT,c13T,iflag)
            call S24_interp (period(count1),period(count2),c14(count1),c14(count2),
     +                   specT,c14T,iflag)
            call S24_interp (period(count1),period(count2),c15(count1),c15(count2),
     +                   specT,c15T,iflag)
            call S24_interp (period(count1),period(count2),c16(count1),c16(count2),
     +                   specT,c16T,iflag)
            call S24_interp (period(count1),period(count2),c17(count1),c17(count2),
     +                   specT,c17T,iflag)

 1011 period1 = specT

C     Compute ground motion:
      f1 = c2T*m+c3T*(8.5-m)**2.0
      gs = c5T + c6T*(Svfs + Ssr) + c7T*Sfr
      f2 = seismoDist*seismoDist + gs*gs*
     1       (exp(c8T*m+c9T*(8.5-m)**2.0) )**2.0
      f3 = c10T*Frv + c11T*Fth
      f4 = c12T*Svfs + c13T*Ssr + c14T*Sfr

C     Check for HW factor.
      sum = 0.0
      sum = Svfs + Ssr + Sfr
      if (sum .eq. 0.0) then
         f5 = 0.0
      else
         if (hwflag .eq. 1) then
c     Compute HW term
            if (jbDist .lt. 5.0) then
               hw = sum*(5.0-jbDist)/5.0
            else
               hw = 0.0
            endif
c     Compute Magnitude term
            if (m .lt. 5.5) then
               fhwm = 0.0
            elseif (m .le. 6.5) then
               fhwm = m - 5.5
            else
               fhwm = 1.0
            endif
c     Compute SeismoDist term
            if (seismoDist .ge. 8.0) then
               fhws = c15T
            else
               fhws = c15T*(seismoDist/8.0)
            endif
            f5 = hw*fhwm*fhws*(Frv+Fth)
         else
            f5 = 0.0
         endif

      endif

      LnY = c1T + f1 + c4T*alog(sqrt(f2)) + f3 + f4 + f5

c     Set sigma as a function of Mw
      if ( m .lt. 7.4 ) then
            sigma = c16T - 0.07*m
          else
            sigma = c16T - 0.518
          endif

c     Convert units spectral acceleration in gal
      lnY = lnY + 6.89

      return
      end

c ------------------------------------------------------------------
C *** Garcia et al. (2005) Horizontal for Subduction Zones, Rock ****
c ------------------------------------------------------------------

      subroutine S02_GarciaH05 ( mag, rupdist, specT, period, lnY, sigma,
     1  iflag, depth )

c     Garcia et al. (2005) Horizontal Subduction (inslab only), Rock
C     Rock Site Conditions

      implicit none

      real mag, rupDist, lnY, sigma, period
      real depth, sigT
      real c1(16), c2(16), c3(16), c4(16), c5(16)
      real specT, c1T, c2T, c3T, c4T, c5T, R, Delta
      integer nper, count1, count2, iflag, i

      real period1(16), sig(16)

      data period1 / 0.0, 0.04, 0.05, 0.075, 0.1, 0.2, 0.3, 0.4, 0.5,
     1               0.75, 1.00, 1.50, 2.0, 3.0, 4.0, 5.0 /
      data c1 /  -0.20, 0.03, 0.10, 0.20, 0.40, 0.05, -0.30, -0.60,
     1           -0.80, -1.30, -1.70, -2.30, -2.70, -3.30, -3.90, -4.30 /
      data c2 / 0.59, 0.59, 0.58, 0.57, 0.55, 0.59, 0.63, 0.64, 0.67,
     1          0.71, 0.75, 0.81, 0.85, 0.89, 0.94, 0.97 /
      data c3 / -0.0039, -0.0043, -0.0043, -0.0043, -0.0041, -0.0037,
     &          -0.0033, -0.0028, -0.0024, -0.0020, -0.0017, -0.0014,
     &          -0.0012, -0.0009, -0.0008, -0.0007 /
      data c4 / 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     1          1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 /
      data c5 / 0.008, 0.007, 0.008, 0.008, 0.008, 0.007, 0.005, 0.005,
     &          0.004, 0.004, 0.003, 0.002, 0.001, 0.0009, 0.0009, 0.001 /
      data sig / 0.28, 0.32, 0.34, 0.34, 0.33, 0.28, 0.28, 0.27, 0.26,
     &           0.27, 0.28, 0.28, 0.26, 0.26, 0.25, 0.25 /

C Find the requested spectral period and corresponding coefficients
      nPer = 16

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period  = period1(1)
         c1T     = c1(1)
         c2T     = c2(1)
         c3T     = c3(1)
         c4T     = c4(1)
         c5T     = c5(1)
         sigT    = sig(1)
         goto 1011
      elseif (specT .ne. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period1(i) .and. specT .le. period1(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

      write (*,*)
      write (*,*) 'Garcia et al. (2005) Sub-Inslab Hor. atttenuation model'
      write (*,*) 'is not defined for a spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period1(count1),period1(count2),c1(count1),c1(count2),
     +             specT,c1T,iflag)
      call S24_interp (period1(count1),period1(count2),c2(count1),c2(count2),
     +             specT,c2T,iflag)
      call S24_interp (period1(count1),period1(count2),c3(count1),c3(count2),
     +             specT,c3T,iflag)
      call S24_interp (period1(count1),period1(count2),c4(count1),c4(count2),
     +             specT,c4T,iflag)
      call S24_interp (period1(count1),period1(count2),c5(count1),c5(count2),
     +             specT,c5T,iflag)
      call S24_interp (period1(count1),period1(count2),sig(count1),sig(count2),
     +             specT,sigT,iflag)

 1011 period = specT

C     Compute the ground motions.
C     Note that equation is in log10 and in units of cm/s*s
      delta = 0.00750*10.0**(0.507*mag)
      R = sqrt( rupdist*rupdist + delta*delta )
      lnY = c1T + c2T*mag + c3T*R - c4T*alog10(R) + c5T*depth
      sigma = sigT

C     Now convert to Ln Units in gals.
      lnY = alog(10.0)*lnY
      sigma = alog(10.0)*sigma

      return
      end

c ------------------------------------------------------------------
C *** Garcia et al. (2005) Vertical for Subduction Zones, Rock ****
c ------------------------------------------------------------------

      subroutine S02_GarciaV05 ( mag, rupdist, specT, period, lnY, sigma,
     1  iflag, depth )

c     Garcia et al. (2005) Vertical Subduction (inslab only), Rock
C     Rock Site Conditions

      implicit none

      real mag, rupDist, lnY, sigma, period
      real depth
      real c1(16), c2(16), c3(16), c4(16), c5(16)
      real specT, c1T, c2T, c3T, c4T, c5T, R, delta, sigT
      integer nper, count1, count2, iflag, i

      real period1(16), sig(16)

      data period1 / 0.0, 0.04, 0.05, 0.075, 0.1, 0.2, 0.3, 0.4, 0.5,
     1               0.75, 1.00, 1.50, 2.0, 3.0, 4.0, 5.0 /
      data c1 /  -0.40, -0.30, -0.2, -0.06, -0.04, -0.07, -0.20, -0.70,
     1           -0.90, -1.30, -1.80, -2.40, -2.80, -3.30, -4.00, -4.40 /
      data c2 / 0.60, 0.62, 0.62, 0.60, 0.59, 0.59, 0.60, 0.64, 0.66,
     1          0.69, 0.75, 0.80, 0.83, 0.88, 0.95, 0.98 /
      data c3 / -0.0036, -0.0041, -0.0043, -0.0041, -0.0039, -0.0033,
     &          -0.0029, -0.0022, -0.0018, -0.0014, -0.0010, -0.0008,
     &          -0.0006, -0.0005, -0.0004, -0.0003 /
      data c4 / 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     1          1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 /
      data c5 / 0.006, 0.006, 0.007, 0.007, 0.007, 0.004, 0.003, 0.003,
     &          0.002, 0.002, 0.001, 0.0004, -0.0005, -0.0004, -0.0003, -0.0002 /
      data sig / 0.27, 0.31, 0.32, 0.32, 0.31, 0.26, 0.26, 0.26, 0.26,
     &           0.25, 0.27, 0.26, 0.27, 0.28, 0.27, 0.26 /

C Find the requested spectral period and corresponding coefficients
      nPer = 16

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period  = period1(1)
         c1T     = c1(1)
         c2T     = c2(1)
         c3T     = c3(1)
         c4T     = c4(1)
         c5T     = c5(1)
         sigT    = sig(1)
         goto 1011
      elseif (specT .ne. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period1(i) .and. specT .le. period1(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

      write (*,*)
      write (*,*) 'Garcia et al. (2005) Sub-Inslab Ver. atttenuation model'
      write (*,*) 'is not defined for a spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period1(count1),period1(count2),c1(count1),c1(count2),
     +             specT,c1T,iflag)
      call S24_interp (period1(count1),period1(count2),c2(count1),c2(count2),
     +             specT,c2T,iflag)
      call S24_interp (period1(count1),period1(count2),c3(count1),c3(count2),
     +             specT,c3T,iflag)
      call S24_interp (period1(count1),period1(count2),c4(count1),c4(count2),
     +             specT,c4T,iflag)
      call S24_interp (period1(count1),period1(count2),c5(count1),c5(count2),
     +             specT,c5T,iflag)
      call S24_interp (period1(count1),period1(count2),sig(count1),sig(count2),
     +             specT,sigT,iflag)

 1011 period = specT

C     Compute the ground motions.
C     Note that equation is in log10 and in units of cm/s*s
      delta = 0.00750*10.0**(0.507*mag)
      R = sqrt( rupdist*rupdist + delta*delta )
      lnY = c1T + c2T*mag + c3T*R - c4T*alog10(R) + c5T*depth
      sigma = sigT

C     Now convert to Ln Units in gals.
      lnY = alog(10.0)*lnY
      sigma = alog(10.0)*sigma

      return
      end

c ------------------------------------------------------------------
C *** Lin and Lee (2008) Horizontal for Subduction Zones, Rock ****
c ------------------------------------------------------------------

      subroutine S02_LinLee08rock ( mag, rupdist, specT, period, lnY, sigma,
     1  iflag, depth, ftype )

c     Lin and Lee (2008) Horizontal Subduction, Rock
C     Rock Site Conditions

      implicit none

      real mag, rupDist, lnY, sigma, period
      real depth, ftype, sigT
      real c1(28), c2(28), c3(28)
      real specT, c1T, c2T, c3T, c4, c5, c6, c7
      integer nper, count1, count2, iflag, i
      real period1(28), sig(28)

      data period1 / 0.00, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.09,
     &               0.10, 0.12, 0.15, 0.17, 0.20, 0.24, 0.30, 0.36,
     &               0.40, 0.46, 0.50, 0.60, 0.75, 0.85, 1.00, 1.50,
     &               2.00, 3.00, 4.00, 5.00 /

      data c1 / -2.500, -2.500, -2.490, -2.280, -2.000, -1.900, -1.725,
     &          -1.265, -1.220, -1.470, -1.675, -1.846, -2.170, -2.585,
     &          -3.615, -4.160, -4.595, -5.020, -5.470, -6.095, -6.675,
     &          -7.320, -8.000, -9.240, -10.200, -11.470, -12.550, -13.390 /
      data c2 / 1.205, 1.205, 1.200, 1.155, 1.100, 1.090, 1.065, 1.020,
     &          1.000, 1.040, 1.045, 1.065, 1.085, 1.105, 1.215, 1.255,
     &          1.285, 1.325, 1.365, 1.420, 1.465, 1.545, 1.620, 1.705,
     &          1.770, 1.830, 1.845, 1.805 /
      data c3 / -1.905, -1.895, -1.880, -1.875, -1.860, -1.855, -1.840,
     &          -1.815, -1.795, -1.770, -1.730, -1.710, -1.675, -1.630,
     &          -1.570, -1.535, -1.500, -1.495, -1.465, -1.455, -1.450,
     &          -1.450, -1.450, -1.440, -1.430, -1.370, -1.260, -1.135 /
      data sig / 0.5268, 0.5218, 0.5189, 0.5235, 0.5352, 0.5370, 0.5544,
     &           0.5818, 0.5806, 0.5748, 0.5817, 0.5906, 0.6059, 0.6315,
     &           0.6656, 0.7010, 0.7105, 0.7148, 0.7145, 0.7177, 0.7689,
     &           0.7787, 0.7983, 0.8411, 0.8766, 0.8590, 0.8055, 0.7654  /

C Find the requested spectral period and corresponding coefficients
      nPer = 28

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period  = period1(1)
         c1T     = c1(1)
         c2T     = c2(1)
         c3T     = c3(1)
         sigT    = sig(1)
         goto 1011
      elseif (specT .ne. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period1(i) .and. specT .le. period1(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

      write (*,*)
      write (*,*) 'Lin and Lee (2008) Sub-Hor. Rock atttenuation model'
      write (*,*) 'is not defined for a spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period1(count1),period1(count2),c1(count1),c1(count2),
     +             specT,c1T,iflag)
      call S24_interp (period1(count1),period1(count2),c2(count1),c2(count2),
     +             specT,c2T,iflag)
      call S24_interp (period1(count1),period1(count2),c3(count1),c3(count2),
     +             specT,c3T,iflag)
      call S24_interp (period1(count1),period1(count2),sig(count1),sig(count2),
     +             specT,sigT,iflag)

 1011 period = specT

C     Compute the ground motions.
      c4 = 0.51552
      c5 = 0.63255
      c6 = 0.0075
      c7 = 0.275

      lnY = c1T + c2T*mag + c3T*alog(Rupdist+c4*exp(c5*mag) ) + c6*depth
     1          + c7*ftype

      sigma = sigT

C     Now convert to Ln Units in gals.
      lnY = lnY + 6.89

      return
      end

c ------------------------------------------------------------------
C *** Lin and Lee (2008) Horizontal for Subduction Zones, Soil ****
c ------------------------------------------------------------------

      subroutine S02_LinLee08soil ( mag, rupdist, specT, period, lnY, sigma,
     1  iflag, depth, ftype )

c     Lin and Lee (2008) Horizontal Subduction, Soil
C     Rock Site Conditions

      implicit none

      real mag, rupDist, lnY, sigma, period, sigT
      real depth, ftype
      real c1(28), c2(28), c3(28)
      real specT, c1T, c2T, c3T, c4, c5, c6, c7
      integer nper, count1, count2, iflag, i
      real period1(28), sig(28)

      data period1 / 0.00, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.09,
     &               0.10, 0.12, 0.15, 0.17, 0.20, 0.24, 0.30, 0.36,
     &               0.40, 0.46, 0.50, 0.60, 0.75, 0.85, 1.00, 1.50,
     &               2.00, 3.00, 4.00, 5.00 /

      data c1 / -0.900, -2.200, -2.290, -2.340, -2.215, -1.895, -1.110,
     &          -0.210, -0.055,  0.055, -0.040, -0.340, -0.800, -1.575,
     &          -3.010, -3.680, -4.250, -4.720, -5.220, -5.700, -6.450,
     &          -7.250, -8.150, -10.300, -11.620, -12.630, -13.420, -13.750 /
      data c2 / 1.000, 1.085, 1.085, 1.095, 1.090, 1.055, 1.010, 0.945,
     &          0.920, 0.935, 0.955, 1.020, 1.045, 1.120, 1.315, 1.380,
     &          1.415, 1.430, 1.455, 1.470, 1.500, 1.565, 1.605, 1.800,
     &          1.860, 1.890, 1.870, 1.835 /
      data c3 / -1.900, -1.750, -1.730, -1.720, -1.730, -1.755, -1.835,
     &          -1.890, -1.880, -1.895, -1.880, -1.885, -1.820, -1.755,
     &          -1.695, -1.660, -1.600, -1.545, -1.490, -1.445, -1.380,
     &          -1.325, -1.235, -1.165, -1.070, -1.060, -0.990, -0.975 /
      data sig / 0.6277, 0.5800, 0.5730, 0.5774, 0.5808, 0.5937, 0.6123,
     &           0.6481, 0.6535, 0.6585, 0.6595, 0.6680, 0.6565, 0.6465,
     &           0.6661, 0.6876, 0.7002, 0.7092, 0.7122, 0.7280, 0.7752,
     &           0.7931, 0.8158, 0.8356, 0.8474, 0.8367, 0.7937, 0.7468  /

C Find the requested spectral period and corresponding coefficients
      nPer = 28

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period  = period1(1)
         c1T     = c1(1)
         c2T     = c2(1)
         c3T     = c3(1)
         sigT    = sig(1)
         goto 1011
      elseif (specT .ne. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period1(i) .and. specT .le. period1(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

      write (*,*)
      write (*,*) 'Lin and Lee (2008) Sub-Hor. Soil atttenuation model'
      write (*,*) 'is not defined for a spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period1(count1),period1(count2),c1(count1),c1(count2),
     +             specT,c1T,iflag)
      call S24_interp (period1(count1),period1(count2),c2(count1),c2(count2),
     +             specT,c2T,iflag)
      call S24_interp (period1(count1),period1(count2),c3(count1),c3(count2),
     +             specT,c3T,iflag)
      call S24_interp (period1(count1),period1(count2),sig(count1),sig(count2),
     +             specT,sigT,iflag)

 1011 period = specT

C     Compute the ground motions.
      c4 = 0.99178
      c5 = 0.52632
      c6 = 0.004
      c7 = 0.310

      lnY = c1T + c2T*mag + c3T*alog(Rupdist+c4*exp(c5*mag) ) + c6*depth
     1          + c7*ftype

      sigma = sigT

C     Now convert to Ln Units in gals.
      lnY = lnY + 6.89

      return
      end

c -------------------------------------------------------------------
C *** Atkinson and Macias (2009) Cascadia, Horizontal NEHRP B/C *****
c -------------------------------------------------------------------

      subroutine S02_AM09_Cas ( mag, rupDist, lnY, sigma,
     1   specT, period, iflag )

      implicit none

      real mag, rupDist, lnY, sigma, period, R
      real c0(25), c1(25), c2(25), c3(25), c4(25), period1(25), sig(25)
      real specT, c0T, c1T, c2T, c3T, c4T, sigT, h
      integer nper, count1, count2, iflag, i

      data  period1 / 0.01, 0.05, 0.0631, 0.0794, 0.1, 0.125, 0.1587, 0.2, 0.25,
     1    0.3165, 0.4, 0.5, 0.6329, 0.7937, 1.0, 1.2658, 1.5873, 2.0, 2.5, 3.125,
     2    4.0, 5.0, 6.25, 7.6923, 10.0 /
      data c0 / 5.006, 5.843, 5.823, 5.676, 5.49, 5.209, 4.93, 4.746, 4.472, 4.303,
     1          4.167, 3.999, 3.856, 3.733, 3.621, 3.453, 3.393, 3.241, 3.104, 2.978,
     2          2.814, 2.671, 2.569, 2.489, 2.338 /
      data c1 / -1.5573, -1.9391, -1.8889, -1.7633, -1.6257, -1.4404, -1.2671, -1.1691,
     1          -1.0133, -0.9322, -0.8854, -0.8211, -0.7746, -0.7473, -0.7376, -0.6885,
     2          -0.7101, -0.6741, -0.6585, -0.6431, -0.6108, -0.5942, -0.6048,
     3          -0.6412, -0.6311 /
      data c2 / -0.00034, 0.0, -0.00022, -0.00071, -0.00115, -0.00163, -0.00204,
     1          -0.00212, -0.00234, -0.00231, -0.00211, -0.00195, -0.00179,
     2          -0.00159, -0.00128, -0.00119, -0.00089, -0.00081, -0.00063,
     3          -0.00057, -0.00046, -0.0004, -0.00024, -0.00003, 0.0 /
      data c3 / 0.1774, 0.1813, 0.1845, 0.1784, 0.1736, 0.1788, 0.1645, 0.1593, 0.1713,
     1          0.1713, 0.1802, 0.187, 0.201, 0.2035, 0.2116, 0.2417, 0.2483, 0.2696,
     2          0.299, 0.3258, 0.349, 0.3822, 0.4324, 0.476, 0.5357 /
      data c4 / 0.0827, 0.0199, 0.016, 0.0245, 0.0261, 0.0151, 0.0301, 0.0432, 0.0255,
     1          0.027, 0.0258, 0.0271, 0.0153, 0.0292, 0.0328, 0.0125, 0.0103, -0.0064,
     2         -0.0074, -0.0103, -0.0299, -0.0417, -0.0641, -0.0629, -0.0737 /
      data sig / 0.5526, 0.5987, 0.5987, 0.6217, 0.6217, 0.6217, 0.6217, 0.6217,
     1             0.6217, 0.6217, 0.6217, 0.6217, 0.6447, 0.6447, 0.6677, 0.6677,
     2             0.6677, 0.6908, 0.6908, 0.6908, 0.6908, 0.7368, 0.7829, 0.8059, 0.8750 /

C Find the requested spectral period and corresponding coefficients
      nPer = 25

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period  = period1(1)
         c0T     = c0(1)
         c1T     = c1(1)
         c2T     = c2(1)
         c3T     = c3(1)
         c4T     = c4(1)
         sigT    = sig(1)
         goto 1011
      elseif (specT .ne. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period1(i) .and. specT .le. period1(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

      write (*,*)
      write (*,*) 'Atkinson&Macias (2009) Subduction atttenuation model'
      write (*,*) 'is not defined for a spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period1(count1),period1(count2),c0(count1),c0(count2),
     +             specT,c0T,iflag)
      call S24_interp (period1(count1),period1(count2),c1(count1),c1(count2),
     +             specT,c1T,iflag)
      call S24_interp (period1(count1),period1(count2),c2(count1),c2(count2),
     +             specT,c2T,iflag)
      call S24_interp (period1(count1),period1(count2),c3(count1),c3(count2),
     +             specT,c3T,iflag)
      call S24_interp (period1(count1),period1(count2),c4(count1),c4(count2),
     +             specT,c4T,iflag)
      call S24_interp (period1(count1),period1(count2),sig(count1),sig(count2),
     +             specT,sigT,iflag)

 1011 period = specT

      h = mag*mag - 3.1*mag - 14.55
      R = sqrt (rupDist*rupDist + h*h)

      lnY = 10**(C0T + c1T*alog10(R) + c2T*R + c3T*(mag-8.0) + c4T*(mag-8.0)*(mag-8.0))

C     Convert to Natural log in gals.
      lnY = alog(lnY)
      sigma = sigT

      return
      end

c ------------------------------------------------------------
      real function csratio (mag)
c     This function computes the scale factor to convert an acceleration
c     from a magnitude "mag" earthquake to the equivalent acceleration
c     from a magnitude 7.5 earthquake for liquifaction potential.
c     See Seed and Idriss (1982) pg 110.

      implicit none

      real mag, xnode(6), ynode(6), temp
      integer i, n
      data xnode /5.0, 5.25, 6.0, 6.75, 7.5, 8.5/
      data ynode /1.58, 1.50, 1.32, 1.13, 1.00, 0.89/
      n = 6
      if (mag .lt. xnode(1)) then
        write (*,'( 2x,''Error - magnitude too small for csratio'')')
        goto 100
      endif
      if (mag .gt. xnode(6)) then
        write (*,'( 2x,''Error - magnitude too large for csratio'')')
        goto 100
      endif
      do i=2,n
        if (mag .lt. xnode(i)) then
          temp = (mag-xnode(i-1))/(xnode(i)-xnode(i-1))
          csratio = temp*(ynode(i)-ynode(i-1)) + ynode(i-1)
          csratio = alog(csratio)
          return
        endif
      enddo
      return
 100  stop 99
      end

c ---------------------------------------------------------------------
C     *** Akkar and Cagnan (2010) ***
c ---------------------------------------------------------------------

      subroutine S02_AC_2010 ( mag, Rbjf, specT,
     1                     period2, lnY, sigma, iflag, vs, ftype, pga4nl )

      implicit none

      integer MAXPER
      parameter (MAXPER=17)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER), a4(MAXPER)
      Real a5(MAXPER), a6(MAXPER), a7(MAXPER), a8(MAXPER), a9(MAXPER)
      real sigtot(MAXPER), Blin(MAXPER), b1(MAXPER), b2(MAXPER)
      real specT, a1T, a2T, a3T, a4T, a5T, a6T, a7T, a8T, a9T, sigmaT
      real mag, Rbjf, Vs, Ftype, Fn, Fr, pga4nl, period2
      real blinT, b1T, b2T, c1, vref, v1, v2, pga_low, bnl, LnY
      real deltaX, deltaY, a1slope, a2slope, c, d, term1, term3, sigma
      real a1p, a2p, a3p, a4p, a5p, a6p, a7p, a8p, a9p, period1
      INTEGER iFlag, count1, count2, nPer, i

      Data Period / 0.0, -1.0, 0.01, 0.03, 0.05, 0.075, 0.1, 0.15, 0.2,
     1              0.25, 0.3, 0.4, 0.5, 0.75, 1.0, 1.5, 2.0  /
      Data a1 / 8.92418, 5.60931, 8.92418, 8.85984, 9.05262, 9.5667,
     1          9.85606, 10.43715, 10.63516, 10.12551, 10.12745, 9.47855,
     2          8.95147, 8.10498, 7.61737, 7.20427, 6.70845 /
      Data a2 / -0.513, -0.513, -0.513, -0.513, -0.513, -0.513, -0.513,
     1          -0.513, -0.513, -0.513, -0.513, -0.513, -0.513, -0.513,
     2          -0.513, -0.513, -0.513 /
      Data a3 / -0.695, -0.695, -0.695, -0.695, -0.695, -0.695, -0.695,
     1          -0.695, -0.695, -0.695, -0.695, -0.695, -0.695, -0.695,
     2          -0.695, -0.695, -0.695 /
      Data a4 / -0.18555, -0.258, -0.18555, -0.17123, -0.15516, -0.1384,
     1          -0.11563, -0.17897, -0.21034, -0.25565, -0.2702, -0.30498,
     2          -0.29877, -0.3349, -0.35366, -0.39858, -0.39528 /
      Data a5 / -1.25594, -0.90393, -1.25594, -1.25132, -1.28796, -1.38817,
     1          -1.43846, -1.46786, -1.44625, -1.27388, -1.26899, -1.09793,
     2          -1.01703, -0.84365, -0.7584, -0.70134, -0.70766	 /
      Data a6 / 0.18105, 0.21576, 0.18105, 0.18421, 0.1984, 0.20246, 0.21833,
     1          0.15588, 0.1159, 0.09426, 0.08352, 0.06082, 0.09099, 0.08647,
     2          0.09623, 0.11219, 0.12032 /
      Data a7 / 7.33617, 5.57472, 7.33617, 7.46968, 7.26552, 8.03646, 8.84202,
     1          9.39515, 9.60868, 7.54353, 8.03144, 6.24042, 5.67936, 4.93842,
     2          4.1259, 3.46535, 3.8822 /
      Data a8 / -0.02125, -0.10481, -0.02125, -0.0134, 0.02076, 0.07311,
     1           0.11044, 0.03555, -0.03536, -0.10685, -0.10685, -0.11197,
     2          -0.10118, -0.0456, -0.01936, -0.02618, -0.03215 /
      Data a9 / 0.01851, 0.07791, 0.01851, 0.03512, 0.01484, 0.02492, -0.0062,
     1          0.19751, 0.18594, 0.13574, 0.13574, 0.16555, 0.23546, 0.10993,
     2          0.19729, 0.21977, 0.20584 /
      Data sigtot / 0.8322, 0.8096, 0.8322, 0.8279, 0.8327, 0.8566, 0.871,
     1              0.8863, 0.8912, 0.9002, 0.8833, 0.8898, 0.8666, 0.8934,
     2              0.9116, 0.9234, 0.9066 /
      Data blin / -0.36, -0.6, -0.36, -0.33, -0.29, -0.23, -0.25, -0.28,
     1            -0.31, -0.39, -0.44, -0.5, -0.6, -0.69, -0.7, -0.72, -0.73 /
      Data b1 / -0.64, -0.5, -0.64, -0.62, -0.64, -0.64, -0.6, -0.53, -0.52,
     1          -0.52, -0.52, -0.51, -0.5, -0.47, -0.44, -0.4, -0.38 /
      Data b2 / -0.14, -0.06, -0.14, -0.11, -0.11, -0.11, -0.13, -0.18,
     1          -0.19, -0.16, -0.14, -0.1, -0.06, 0.0, 0.0, 0.0, 0.0 /


C     Set parameters
      c1 = 6.5
      pga_low = 0.06
      vref = 760.0
      v1 = 180.0
      v2 = 300.0

c     Set Parameters to compute PGA rock
      a1p = 8.92418
      a2p = -0.513
      a3p = -0.695
      a4p = -0.18555
      a5p = -1.25594
      a6p = 0.18105
      a7p = 7.33617
      a8p = -0.02125
      a9p = 0.01851

C First check for the PGA case (i.e., specT=0.0)
      nPer = 17
      if (specT .eq. 0.0) then
         period1 = period(1)
         a1T = a1(1)
         a2T = a2(1)
         a3T = a3(1)
         a4T = a4(1)
         a5T = a5(1)
         a6T = a6(1)
         a7T = a7(1)
         a8T = a8(1)
         a9T = a9(1)
         blinT = blin(1)
         b1T = b1(1)
         b2T = b2(1)
         sigmaT = sigTot(1)
         goto 1011
C Check for the PGV case (i.e., specT=-1.0)
      elseif (specT .eq. -1.0) then
         period1 = period(2)
         a1T = a1(2)
         a2T = a2(2)
         a3T = a3(2)
         a4T = a4(2)
         a5T = a5(2)
         a6T = a6(2)
         a7T = a7(2)
         a8T = a8(2)
         a9T = a9(2)
         blinT = blin(2)
         b1T = b1(2)
         b2T = b2(2)
         sigmaT = sigTot(2)
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=3,nper-1+2
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'Akkar&Cagan (2010) Horizontal'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020       call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +                   specT,a1T,iflag)
            call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +                   specT,a2T,iflag)
            call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +                   specT,a3T,iflag)
            call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +                   specT,a4T,iflag)
            call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +                   specT,a5T,iflag)
            call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +                   specT,a6T,iflag)
            call S24_interp (period(count1),period(count2),a7(count1),a7(count2),
     +                   specT,a7T,iflag)
            call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +                   specT,a8T,iflag)
            call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +                   specT,a9T,iflag)
            call S24_interp (period(count1),period(count2),blin(count1),blin(count2),
     +                   specT,blinT,iflag)
            call S24_interp (period(count1),period(count2),b1(count1),b1(count2),
     +                   specT,b1T,iflag)
            call S24_interp (period(count1),period(count2),b2(count1),b2(count2),
     +                   specT,b2T,iflag)
            call S24_interp (period(count1),period(count2),sigTot(count1),sigTot(count2),
     +                   specT,sigmaT,iflag)
 1011 period1 = specT

C.....Set the mechanism terms based on ftype............
C     Set mechanism term and corresponding Frv and Fnm values.
C     fType     Mechanism                      Rake
C     ------------------------------------------------------
C      -1       Normal                    -120 < Rake <  -60
C     -0.5      Normal/Oblique            -150 < Rake < -120
C                                          -60 < Rake <  -30
C       0       Strike-Slip               -180 < Rake < -150
C                                          -30 < Rake <   30
C                                          150 < Rake <  180
C      0.5      Reverse/Oblique             30 < Rake <   60
C                                          120 < Rake <  150
C       1       Reverse                     60 < Rake <  120
      if (ftype .eq. -1.0) then
         Fr = 0.0
         Fn = 1.0
      elseif (ftype .eq. -0.5) then
         Fr = 0.0
         Fn = 1.0
      elseif (ftype .eq. 0.0) then
         Fr = 0.0
         Fn = 0.0
      elseif (ftype .eq. 0.5) then
         Fr = 1.0
         Fn = 0.0
      elseif (ftype .eq. 1.0) then
         Fr = 1.0
         Fn = 0.0
      endif

C.....First compute the Reference Rock PGA value.........
      if (mag .le. c1) then
         term1 = a1p + a2p*(mag-c1) + a4p*(8.5-mag)**2.0 +
     1           ( a5p + a6p*(mag-c1) ) * alog(sqrt(Rbjf*Rbjf+a7p*a7p)) +
     2           a8p*Fn + a9p*Fr
      else
         term1 = a1p + a3p*(mag-c1) + a4p*(8.5-mag)**2.0 +
     1           ( a5p + a6p*(mag-c1) ) * alog(sqrt(Rbjf*Rbjf+a7p*a7p)) +
     2           a8p*Fn + a9p*Fr
      endif

      pga4nl = exp(term1)

C.....Now compute the requested ground motion value........
      if (mag .le. c1) then
         term1 = a1T + a2T*(mag-c1) + a4T*(8.5-mag)**2.0 +
     1           ( a5T + a6T*(mag-c1) ) * alog(sqrt(Rbjf*Rbjf+a7T*a7T)) +
     2           a8T*Fn + a9T*Fr
      else
         term1 = a1T + a3T*(mag-c1) + a4T*(8.5-mag)**2.0 +
     1           ( a5T + a6T*(mag-c1) ) * alog(sqrt(Rbjf*Rbjf+a7T*a7T)) +
     2           a8T*Fn + a9T*Fr
      endif

C.....Site Response Term.........
C.....First compute Bnl slope term......
      if (vs .le. v1) then
         bnl = b1T
      elseif (vs .le. v2) then
        bnl = (b1T-b2T)*(alog(vs/v2)/alog(v1/v2)) + b2T
      elseif (vs .lt. vref) then
        bnl = b2t*alog(vs/vref)/alog(v2/vref)
      else
        bnl = 0.0
      endif

c.....next compute the slope coefficients for model....
      a1slope = 0.03
      a2slope = 0.09
      deltaX = alog(a2slope/a1slope)
      deltaY = bnl*alog(a2slope/pga_low)
      c = (3.0*deltaY-bnl*deltaX)/(deltaX*deltaX)
      d = -1.0*(2.0*deltaY-bnl*deltaX)/(deltaX*deltaX*deltaX)

C.....Now compute the site term........
      if (pga4nl .le. a1slope) then
          TERM3 = blinT*alog(vs/vref) + bnl*alog(pga_low/0.1)
      elseif (pga4nl .le. a2slope) then
          TERM3 = blinT*alog(vs/vref) + bnl*alog(pga_low/0.1)
     1            + c*(alog(pga4nl/a1slope))**2.0
     2            + d*(alog(pga4nl/a1slope))**3.0
      elseif (pga4nl .gt. a2slope) then
          TERM3 = blinT*alog(vs/vref) + bnl*alog(pga4nl/0.1)
      endif

      lnY = term1 + term3

      period2 = period1

      sigma = sigmaT

      return
      END


c ---------------------------------------------------------------------
C     *** Akkar and Bommer (2010) ***
c ---------------------------------------------------------------------

      subroutine S02_AB_2010 ( mag, Rbjf, specT,
     1                     period2, lnY, sigma, iflag, ftype, Ss, Sa )

      implicit none

      integer MAXPER
      parameter (MAXPER=63)
      REAL Period(MAXPER), b1(MAXPER), b2(MAXPER), b3(MAXPER), b4(MAXPER)
      Real b5(MAXPER), b6(MAXPER), b7(MAXPER), b8(MAXPER), b9(MAXPER)
      real b10(MAXPER), sigtot(MAXPER), sigma
      real specT, b1T, b2T, b3T, b4T, b5T, b6T, b7T, b8T, b9T, b10T, sigmaT
      real mag, Rbjf, Ftype, Fn, Fr, Ss, Sa, Term1, lnY, period2, period1
      INTEGER iFlag, count1, count2, nPer, i

      data period / 0.00, -1.0, 0.01, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35,
     1              0.40, 0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85,
     2              0.90, 0.95, 1.00, 1.05, 1.10, 1.15, 1.20, 1.25, 1.30, 1.35,
     3              1.40, 1.45, 1.50, 1.55, 1.60, 1.65, 1.70, 1.75, 1.80, 1.85,
     4              1.90, 1.95, 2.00, 2.05, 2.10, 2.15, 2.20, 2.25, 2.30, 2.35,
     5              2.40, 2.45, 2.50, 2.55, 2.60, 2.65, 2.70, 2.75, 2.80, 2.85,
     6              2.90, 2.95, 3.00 /
      data b1 / 1.04159,  -2.12833,  1.04159, 2.11528,  2.11994,  1.64489,  0.92065,
     1           0.13978, -0.84006, -1.32207, -1.70320, -1.97201, -2.76925, -3.51672,
     1          -3.92759, -4.49490, -4.62925, -4.95053, -5.32863, -5.75799, -5.82689,
     1          -5.90592, -6.17066, -6.60337, -6.90379, -6.96180, -6.99236, -6.74613,
     1          -6.51719, -6.55821, -6.61945, -6.62737, -6.71787, -6.80776, -6.83632,
     1          -6.88684, -6.94600, -7.09166, -7.22818, -7.29772, -7.35522, -7.40716,
     1          -7.50404, -7.55598, -7.53463, -7.50811, -8.09168, -8.11057, -8.16272,
     1          -7.94704, -7.96679, -7.97878, -7.88403, -7.68101, -7.72574, -7.53288,
     1          -7.41587, -7.34541, -7.24561, -7.07107, -6.99332, -6.95669, -6.92924 /
      data b2 / 0.91333, 1.21448, 0.91333, 0.72571, 0.75179, 0.83683, 0.96815, 1.13068,
     1          1.37439, 1.47055, 1.55930, 1.61645, 1.83268, 2.02523, 2.08471, 2.21154,
     1          2.21764, 2.29142, 2.38389, 2.50635, 2.50287, 2.51405, 2.58558, 2.69584,
     1          2.77044, 2.75857, 2.73427, 2.62375, 2.51869, 2.52238, 2.52611, 2.49858,
     1          2.49486, 2.50291, 2.51009, 2.54048, 2.57151, 2.62938, 2.66824, 2.67565,
     1          2.67749, 2.68206, 2.71004, 2.72737, 2.71709, 2.71035, 2.91159, 2.92087,
     1          2.93325, 2.85328, 2.85363, 2.84900, 2.81817, 2.75720, 2.82043, 2.74824,
     1          2.69012, 2.65352, 2.61028, 2.56123, 2.52699, 2.51006, 2.45899 /
      data b3 / -0.08140, -0.08137, -0.08140, -0.07351, -0.07448, -0.07544, -0.07903,
     1          -0.08761, -0.10349, -0.10873, -0.11388, -0.11742, -0.13202, -0.14495,
     1          -0.14648, -0.15522, -0.15491, -0.15983, -0.16571, -0.17479, -0.17367,
     1          -0.17417, -0.17938, -0.18646, -0.19171, -0.18890, -0.18491, -0.17392,
     1          -0.16330, -0.16307, -0.16274, -0.15910, -0.15689, -0.15629, -0.15676,
     1          -0.15995, -0.16294, -0.16794, -0.17057, -0.17004, -0.16934, -0.16906,
     1          -0.17130, -0.17291, -0.17221, -0.17212, -0.18920, -0.19044, -0.19155,
     1          -0.18539, -0.18561, -0.18527, -0.18320, -0.17905, -0.18717, -0.18142,
     1          -0.17632, -0.17313, -0.16951, -0.16616, -0.16303, -0.16142, -0.15513 /
      data b4 / -2.92728, -2.46942, -2.92728, -3.33201, -3.10538, -2.75848, -2.49264,
     1          -2.33824, -2.19123, -2.12993, -2.12718, -2.16619, -2.12969, -2.04211,
     1          -1.88144, -1.79031, -1.79800, -1.81321, -1.77273, -1.77068, -1.76295,
     1          -1.79854, -1.80717, -1.73843, -1.71109, -1.66588, -1.59120, -1.52886,
     1          -1.46527, -1.48223, -1.48257, -1.43310, -1.35301, -1.31227, -1.33260,
     1          -1.40931, -1.47676, -1.54037, -1.54273, -1.50936, -1.46988, -1.43816,
     1          -1.44395, -1.45794, -1.46662, -1.49679, -1.55644, -1.59537, -1.60461,
     1          -1.57428, -1.57833, -1.57728, -1.60381, -1.65212, -1.88782, -1.89525,
     1          -1.87041, -1.86079, -1.85612, -1.90422, -1.89704, -1.90132, -1.76801 /
      data b5 / 0.28120, 0.22349, 0.28120, 0.33534, 0.30253, 0.25490, 0.21790, 0.20089,
     1          0.18139, 0.17485, 0.17137, 0.17700, 0.16877, 0.15617, 0.13621, 0.12916,
     1          0.13495, 0.13920, 0.13273, 0.13096, 0.13059, 0.13535, 0.13599, 0.12485,
     1          0.12227, 0.11447, 0.10265, 0.09129, 0.08005, 0.08173, 0.08213, 0.07577,
     1          0.06379, 0.05697, 0.05870, 0.06860, 0.07672, 0.08428, 0.08325, 0.07663,
     1          0.07065, 0.06525, 0.06602, 0.06774, 0.06940, 0.07429, 0.08428, 0.09052,
     1          0.09284, 0.09077, 0.09288, 0.09428, 0.09887, 0.10680, 0.14049, 0.14356,
     1          0.14283, 0.14340, 0.14444, 0.15127, 0.15039, 0.15081, 0.13314 /
      data b6 / 7.86638, 7.86638, 6.41443, 7.74734, 8.21405, 8.31786, 8.21914, 7.20688,
     1          6.54299, 6.24751, 6.57173, 6.78082, 7.17423, 6.76170, 6.10103, 5.19135,
     1          4.46323, 4.27945, 4.37011, 4.62192, 4.65393, 4.84540, 4.97596, 5.04489,
     1          5.00975, 5.08902, 5.03274, 5.08347, 5.14423, 5.29006, 5.33490, 5.19412,
     1          5.15750, 5.27441, 5.54539, 5.93828, 6.36599, 6.82292, 7.11603, 7.31928,
     1          7.25988, 7.25344, 7.26059, 7.40320, 7.46168, 7.51273, 7.77062, 7.87702,
     1          7.91753, 7.61956, 7.59643, 7.50338, 7.53947, 7.61893, 8.12248, 7.92236,
     1          7.49999, 7.26668, 7.11861, 7.36277, 7.45038, 7.60234, 7.21950 /
      data b7 / 0.08753, 0.20354, 0.08753, 0.04707, 0.02667, 0.02578, 0.06557, 0.09810,
     1          0.12847, 0.16213, 0.21222, 0.24121, 0.25944, 0.26498, 0.27718, 0.28574,
     1          0.30348, 0.31516, 0.32153, 0.33520, 0.34849, 0.35919, 0.36619, 0.37278,
     1          0.37756, 0.38149, 0.38120, 0.38782, 0.38862, 0.38677, 0.38625, 0.38285,
     1          0.37867, 0.37267, 0.36952, 0.36531, 0.35936, 0.35284, 0.34775, 0.34561,
     1          0.34142, 0.33720, 0.33298, 0.33010, 0.32645, 0.32439, 0.31354, 0.30997,
     1          0.30826, 0.32071, 0.31801, 0.31401, 0.31104, 0.30875, 0.31122, 0.30935,
     1          0.30688, 0.30635, 0.30534, 0.30508, 0.30362, 0.29987, 0.29772 /
      data b8 / 0.01527, 0.08484, 0.01527, -0.02426, -0.00062, 0.01703, 0.02105, 0.03919,
     1          0.04340, 0.06695, 0.09201,  0.11675,  0.13562, 0.14446, 0.15156, 0.15239,
     1          0.15652, 0.16333, 0.17366,  0.18480,  0.19061, 0.19411, 0.19519, 0.19461,
     1          0.19423, 0.19402, 0.19309,  0.19392,  0.19273, 0.19082, 0.19285, 0.19161,
     1          0.18812, 0.18568, 0.18149,  0.17617,  0.17301, 0.16945, 0.16743, 0.16730,
     1          0.16325, 0.16171, 0.15839,  0.15496,  0.15337, 0.15264, 0.14430, 0.14430,
     1          0.14412, 0.14321, 0.14301,  0.14324,  0.14332, 0.14343, 0.14255, 0.14223,
     1          0.14074, 0.14052, 0.13923,  0.13933,  0.13776, 0.13584, 0.13198 /
      data b9 / -0.04189, -0.05856, -0.04189, -0.04260, -0.04906, -0.04184, -0.02098,
     1          -0.04853, -0.05554, -0.04722, -0.05145, -0.05202, -0.04283, -0.04259,
     1          -0.03853, -0.03423, -0.04146, -0.04050, -0.03946, -0.03786, -0.02884,
     1          -0.02209, -0.02269, -0.02613, -0.02655, -0.02088, -0.01623, -0.01826,
     1          -0.01902, -0.01842, -0.01607, -0.01288, -0.01208, -0.00845, -0.00533,
     1          -0.00852, -0.01204, -0.01386, -0.01402, -0.01526, -0.01563, -0.01848,
     1          -0.02258, -0.02626, -0.02920, -0.03484, -0.03985, -0.04155, -0.04238,
     1          -0.04963, -0.04910, -0.04812, -0.04710, -0.04607, -0.05106, -0.05024,
     1          -0.04887, -0.04743, -0.04731, -0.04522, -0.04203, -0.03863, -0.03855 /
      data b10 / 0.08015, 0.01305, 0.08015, 0.08649,  0.07910,  0.07840,  0.08438,  0.08577,
     1           0.09221, 0.09003, 0.09903, 0.09943,  0.08579,  0.06945,  0.05932,  0.05111,
     1           0.04661, 0.04253, 0.03373, 0.02867,  0.02475,  0.02502,  0.02121,  0.01115,
     1           0.00140, 0.00148, 0.00413, 0.00413, -0.00369, -0.00897, -0.00876, -0.00564,
     1          -0.00215, -0.00047, -0.00006, -0.00301, -0.00744, -0.01387, -0.01492, -0.01192,
     1          -0.00703, -0.00351, -0.00486, -0.00731, -0.00871, -0.01225, -0.01927, -0.02322,
     1          -0.02626, -0.02342, -0.02570, -0.02643, -0.02769, -0.02819, -0.02966, -0.02930,
     1          -0.02963, -0.02919, -0.02751, -0.02776, -0.02615, -0.02487, -0.02469 /
      data sigtot / 0.279287236, 0.278149834, 0.279287236, 0.295001085, 0.296713212, 0.303212928,
     1              0.302102665, 0.303689661, 0.306172827, 0.316373276, 0.319377598, 0.323797746,
     1              0.329038797, 0.332384958, 0.332970704, 0.337848072, 0.339298688, 0.337714865,
     1              0.332812034, 0.329996030, 0.328795772, 0.326833291, 0.325273946, 0.323832812,
     1              0.322848958, 0.320965201, 0.321770182, 0.321060399, 0.320429243, 0.321906959,
     1              0.322356774, 0.321620553, 0.319608276, 0.319319981, 0.319549448, 0.321407685,
     1              0.322923660, 0.323928449, 0.324565556, 0.324531170, 0.325293667, 0.327358947,
     1              0.328372867, 0.328863513, 0.328417311, 0.328143581, 0.326435736, 0.326435736,
     1              0.326648588, 0.325386678, 0.326990841, 0.327915385, 0.328129319, 0.328488478,
     1              0.332946317, 0.334486263, 0.335936006, 0.337196278, 0.338202972, 0.338054803,
     1              0.338268119, 0.338045456, 0.338490783 /

C First check for the PGA case (i.e., specT=0.0)
      nPer = 63
      if (specT .eq. 0.0) then
         period1 = period(1)
         b1T = b1(1)
         b2T = b2(1)
         b3T = b3(1)
         b4T = b4(1)
         b5T = b5(1)
         b6T = b6(1)
         b7T = b7(1)
         b8T = b8(1)
         b9T = b9(1)
         b10T = b10(1)
         sigmaT = sigTot(1)
         goto 1011
C Check for the PGV case (i.e., specT=-1.0)
      elseif (specT .eq. -1.0) then
         period1 = period(2)
         b1T = b1(2)
         b2T = b2(2)
         b3T = b3(2)
         b4T = b4(2)
         b5T = b5(2)
         b6T = b6(2)
         b7T = b7(2)
         b8T = b8(2)
         b9T = b9(2)
         b10T = b10(2)
         sigmaT = sigTot(2)
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=3,nper-1+2
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'Akkar&Bommer (2010) Horizontal'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020       call S24_interp (period(count1),period(count2),b1(count1),b1(count2),
     +                   specT,b1T,iflag)
            call S24_interp (period(count1),period(count2),b2(count1),b2(count2),
     +                   specT,b2T,iflag)
            call S24_interp (period(count1),period(count2),b3(count1),b3(count2),
     +                   specT,b3T,iflag)
            call S24_interp (period(count1),period(count2),b4(count1),b4(count2),
     +                   specT,b4T,iflag)
            call S24_interp (period(count1),period(count2),b5(count1),b5(count2),
     +                   specT,b5T,iflag)
            call S24_interp (period(count1),period(count2),b6(count1),b6(count2),
     +                   specT,b6T,iflag)
            call S24_interp (period(count1),period(count2),b7(count1),b7(count2),
     +                   specT,b7T,iflag)
            call S24_interp (period(count1),period(count2),b8(count1),b8(count2),
     +                   specT,b8T,iflag)
            call S24_interp (period(count1),period(count2),b9(count1),b9(count2),
     +                   specT,b9T,iflag)
            call S24_interp (period(count1),period(count2),b10(count1),b10(count2),
     +                   specT,b10T,iflag)
            call S24_interp (period(count1),period(count2),sigTot(count1),sigTot(count2),
     +                   specT,sigmaT,iflag)
 1011 period1 = specT

C.....Set the mechanism terms based on ftype............
C     Set mechanism term and corresponding Frv and Fnm values.
C     fType     Mechanism                      Rake
C     ------------------------------------------------------
C      -1       Normal                    -120 < Rake <  -60
C     -0.5      Normal/Oblique            -150 < Rake < -120
C                                          -60 < Rake <  -30
C       0       Strike-Slip               -180 < Rake < -150
C                                          -30 < Rake <   30
C                                          150 < Rake <  180
C      0.5      Reverse/Oblique             30 < Rake <   60
C                                          120 < Rake <  150
C       1       Reverse                     60 < Rake <  120
      if (ftype .eq. -1.0) then
         Fr = 0.0
         Fn = 1.0
      elseif (ftype .eq. -0.5) then
         Fr = 0.0
         Fn = 1.0
      elseif (ftype .eq. 0.0) then
         Fr = 0.0
         Fn = 0.0
      elseif (ftype .eq. 0.5) then
         Fr = 1.0
         Fn = 0.0
      elseif (ftype .eq. 1.0) then
         Fr = 1.0
         Fn = 0.0
      endif

C.....Compute the requested ground motion value........
         term1 = b1T + b2T*mag + b3T*(mag)**2.0 +
     1           ( b4T + b5T*mag ) * alog10(sqrt(Rbjf*Rbjf+b6T*b6T)) +
     2           b7T*Ss + b8T*Sa + b9T*Fn + b10T*Fr

C     Convert from Log10 to LnY
      lnY = alog(10.0)*term1

      period2 = period1

      sigma = sigmaT*alog(10.0)

      return
      END

c ---------------------------------------------------------------------
C     *** Akkar, Sandikkaya, and Bommer (2013) ***
c ---------------------------------------------------------------------

      subroutine S02_ASB_2013 ( mag, Rbjf, specT,
     1                     period2, lnY, sigma, iflag, ftype, Vs, phiT, tauT )

      implicit none

      integer MAXPER
      parameter (MAXPER=20)
      REAL Period(MAXPER), a1(MAXPER), a3(MAXPER), a4(MAXPER), a8(MAXPER)
      Real a9(MAXPER), b1(MAXPER), b2(MAXPER), phi(MAXPER), tau(MAXPER)
      real specT, a1T, a3T, a4T, a8T, a9T, b1T, b2T, phiT, tauT, period2
      real mag, Rbjf, Ftype, Fn, Fr, Vs, lnY
      real a2, a5, a6, a7, c1, c, n, sigma, period1, pgaref
      INTEGER iFlag, count1, count2, nPer, i


      Data period / 0.00, -1.00, 0.01, 0.02, 0.03, 0.04, 0.05, 0.075, 0.1, 0.15,
     1              0.2, 0.3, 0.4, 0.5, 0.75, 1.00, 1.5, 2.00, 3.00, 4.00 /
      Data a1     / 1.85329, 5.61201, 1.87032, 1.95279, 2.07006, 2.20452, 2.35413,
     1              2.63078, 2.85412, 2.96622, 2.73872, 2.3015, 1.89372, 1.67127,
     2              0.95211, 0.52349, -0.01867, -0.42891, -1.05642, -1.37536 /
      Data a3     / -0.02807, -0.0998, -0.0274, -0.02715, -0.02403, -0.01797, -0.01248,
     1              -0.00532, -0.00925, -0.02193, -0.03462, -0.05672, -0.07684, -0.0949,
     2              -0.12347, -0.14345, -0.17187, -0.19029, -0.21392, -0.23848 /
      Data a4     / -1.23452, -0.98388, -1.23698, -1.25363, -1.27525, -1.30123, -1.32632,
     1              -1.35722, -1.38182, -1.3646, -1.28877, -1.17072, -1.0653, -1.01909,
     2              -0.88393, -0.81838, -0.75751, -0.72033, -0.69085, -0.66482 /
      Data a8     / -0.1091, -0.0616, -0.1115, -0.104, -0.0973, -0.0884, -0.0853,
     1              -0.0779, -0.0749, -0.0265, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
     2               0.00, 0.00, 0.00, 0.00 /
      Data a9     / 0.0937, 0.063, 0.0953, 0.1029, 0.1148, 0.1073, 0.1052, 0.0837,
     1              0.0761, 0.0545, 0.0493, 0.0469, 0.04, 0.0271, 0.0141, 0.00,
     2              0.00, -0.009, -0.0683, -0.2231 /
      Data b1     / -0.41997, -0.72057, -0.41729, -0.39998, -0.34799, -0.27572,
     1              -0.21231, -0.14427, -0.27064, -0.48313, -0.65315, -0.82609,
     2              -0.89517, -0.94614, -1.00786, -1.01331, -0.98071, -0.91007,
     3              -0.85793, -0.75645 /
      Data b2     / -0.28846, -0.19688, -0.28685, -0.28241, -0.26842, -0.24759,
     1              -0.22385, -0.17525, -0.29293, -0.39551, -0.44644, -0.4573,
     2              -0.43008, -0.37408, -0.28957, -0.28702, -0.24695, -0.17336,
     3              -0.13336, -0.07749 /
      Data phi    / 0.6201, 0.6014, 0.6215, 0.6266, 0.641, 0.6534, 0.6622, 0.6626,
     1              0.667, 0.6796, 0.6645, 0.6599, 0.6697, 0.6512, 0.6744, 0.6787,
     2              0.7164, 0.7254, 0.6997, 0.6196 /
      Data tau    / 0.3501, 0.3311, 0.3526, 0.3555, 0.3565, 0.3484, 0.3551, 0.3759,
     1              0.4067, 0.3893, 0.3842, 0.3816, 0.3962, 0.4021, 0.4043, 0.3943,
     2              0.3799, 0.3717, 0.4046, 0.3566 /
c      Data sigma  / 0.7121, 0.6865, 0.7146, 0.7204, 0.7335, 0.7405, 0.7514, 0.7618,
c     1              0.7812, 0.7832, 0.7676, 0.7623, 0.7781, 0.7653, 0.7863, 0.7849,
c     2              0.8109, 0.8151, 0.8083, 0.7149 /



C First check for the PGA case (i.e., specT=0.0)
      nPer = 20
      if (specT .eq. 0.0) then
         period1 = period(1)
         a1T = a1(1)
         a3T = a3(1)
         a4T = a4(1)
         a8T = a8(1)
         a9T = a9(1)
         b1T = b1(1)
         b2T = b2(1)
         phiT = phi(1)
         tauT = tau(1)
         goto 1011
C Check for the PGV case (i.e., specT=-1.0)
      elseif (specT .eq. -1.0) then
         period1 = period(2)
         a1T = a1(2)
         a3T = a3(2)
         a4T = a4(2)
         a8T = a8(2)
         a9T = a9(2)
         b1T = b1(2)
         b2T = b2(2)
         phiT = phi(2)
         tauT = tau(2)
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=3,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'Akkar,Sandikkaya&Bommer (2013) Horizontal'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020       call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +                   specT,a1T,iflag)
            call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +                   specT,a3T,iflag)
            call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +                   specT,a4T,iflag)
            call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +                   specT,a8T,iflag)
            call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +                   specT,a9T,iflag)
            call S24_interp (period(count1),period(count2),b1(count1),b1(count2),
     +                   specT,b1T,iflag)
            call S24_interp (period(count1),period(count2),b2(count1),b2(count2),
     +                   specT,b2T,iflag)
            call S24_interp (period(count1),period(count2),phi(count1),phi(count2),
     +                   specT,phiT,iflag)
            call S24_interp (period(count1),period(count2),tau(count1),tau(count2),
     +                   specT,tauT,iflag)
 1011 period1 = specT

C.....Set the mechanism terms based on ftype............
C     Set mechanism term and corresponding Frv and Fnm values.
C     fType     Mechanism                      Rake
C     ------------------------------------------------------
C      -1       Normal                    -120 < Rake <  -60
C     -0.5      Normal/Oblique            -150 < Rake < -120
C                                          -60 < Rake <  -30
C       0       Strike-Slip               -180 < Rake < -150
C                                          -30 < Rake <   30
C                                          150 < Rake <  180
C      0.5      Reverse/Oblique             30 < Rake <   60
C                                          120 < Rake <  150
C       1       Reverse                     60 < Rake <  120
      if (ftype .eq. -1.0) then
         Fr = 0.0
         Fn = 1.0
      elseif (ftype .eq. -0.5) then
         Fr = 0.0
         Fn = 1.0
      elseif (ftype .eq. 0.0) then
         Fr = 0.0
         Fn = 0.0
      elseif (ftype .eq. 0.5) then
         Fr = 1.0
         Fn = 0.0
      elseif (ftype .eq. 1.0) then
         Fr = 1.0
         Fn = 0.0
      endif

C     Set frequency independent terms
      a2 = 0.0029
      a5 = 0.2529
      a6 = 7.5
      a7 = -0.5096
      c1 = 6.75
      c = 2.5
      n = 3.2


C     Compute the PGA for reference Vs=750m/s.
      if (mag .le. c1 ) then
         pgaref = a1(1) + a2*(mag-c1) + a3(1)*(8.5-mag)**2.0 +
     1                 (a4(1)+a5*(mag-c1))*alog(sqrt(Rbjf*Rbjf+a6*a6)) +
     2                  a8(1)*Fn + a9(1)*Fr
      else
         pgaref = a1(1) + a7*(mag-c1) + a3(1)*(8.5-mag)**2.0 +
     1                 (a4(1)+a5*(mag-c1))*alog(sqrt(Rbjf*Rbjf+a6*a6)) +
     2                  a8(1)*Fn + a9(1)*Fr
      endif

      pgaref = exp(pgaref)

C.....Now compute the ground motion value........
      if (mag .le. c1 ) then
         lnY = a1T + a2*(mag-c1) + a3T*(8.5-mag)**2.0 +
     1                 (a4T+a5*(mag-c1))*alog(sqrt(Rbjf*Rbjf+a6*a6)) +
     2                  a8T*Fn + a9T*Fr
      else
         lnY = a1T + a7*(mag-c1) + a3T*(8.5-mag)**2.0 +
     1                 (a4T+a5*(mag-c1))*alog(sqrt(Rbjf*Rbjf+a6*a6)) +
     2                  a8T*Fn + a9T*Fr
      endif

C.....Now apply site amplification term......
      if (vs .le. 750.0) then
         lnY = lnY + b1T*alog(Vs/750.0) +
     1         b2T*alog( (pgaref + c*(Vs/750.0)**n) / ((pgaref+c)*(Vs/750.0)**n) )
      else
         lnY = lnY + b1T*alog( min(Vs,1000.0)/750.0)
      endif

C.....Set Sigma value.........
      sigma = sqrt (phiT*phiT + tauT*tauT)

C     Convert ground motion to units of gals.
      lnY = lnY + 6.89
      period2 = period1

      return
      END

c ---------------------------------------------------------------------
C     *** Kale, O., Akkar, S., Ansari, A., & Hamzehloo, H. (2015). ***
c ---------------------------------------------------------------------

      subroutine S02_KAAH_2015 ( mag, Rbjf, specT,
     1                     period2, lnY, sigma, iflag, vs, ftype, pgaref, region )

      implicit none

      integer MAXPER
      parameter (MAXPER=20)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER)
      real b1(MAXPER), b2(MAXPER), b3(MAXPER), b4(MAXPER), b5(MAXPER)
      Real b6(MAXPER), b7(MAXPER), b8(MAXPER), b9(MAXPER), b10(MAXPER)
	    real sd1(MAXPER), sd2(MAXPER), sb1(MAXPER), sb2(MAXPER)
      REAL da1(MAXPER), da2(MAXPER), dsd1(MAXPER), dsd2(MAXPER)
      real db1(MAXPER), db2(MAXPER), db3(MAXPER), db4(MAXPER), db5(MAXPER)
      Real db6(MAXPER), db7(MAXPER), db8(MAXPER), db9(MAXPER), db10(MAXPER)
      real specT, a1T, a2T, b1T, b2T, b3T, b4T, b5T, b6T, b7T, b8T, b9T, b10T
	    real sd1T, sd2T, sb1T, sb2T
      real a1p, a2p, b1p, b2p, b3p, b4p, b5p, b6p, b7p, b8p, b9p, b10p
	    real sd1p, sd2p, sb1p, sb2p
	    real a1a, a2a, b1a, b2a, b3a, b4a, b5a, b6a, b7a, b8a, b9a, b10a
	    real sd1a, sd2a
      real a1b, a2b, b1b, b2b, b3b, b4b, b5b, b6b, b7b, b8b, b9b, b10b
	    real sd1b, sd2b
      real mag, Rbjf, Vs, Ftype, Fn, Fr, pgaref
      real c1, vref, vcon, LnY
      real c, n, term1
      INTEGER iFlag, count1, count2, region, nPer, i
      real period2, sigma, dc1, period1, fmag, fdis, fsof, faat, fsite
      real w, phi, tau

      Data Period / 0.0, -1, 0.01, 0.02, 0.03, 0.04, 0.05, 0.075, 0.1, 0.15,
     1              0.2, 0.3, 0.4, 0.5, 0.75, 1, 1.5, 2, 3, 4 /
      Data b1   / 1.74221, 5.58266, 1.75746, 1.78825, 1.87916, 2.00393,
     1            2.16076, 2.52625, 2.72364, 2.91835, 2.85623, 2.44252,
     2            1.97772, 1.5641, 0.84856, 0.41833, -0.10161, -0.45413,
     3            -0.95276, -1.29675 /
      Data b2   / 0.193, 0.193, 0.193, 0.193, 0.193, 0.193, 0.193, 0.193,
     1            0.193, 0.193, 0.193, 0.193, 0.193, 0.193, 0.193, 0.193,
     2            0.193, 0.193, 0.193, 0.193 /
      Data b3   / -0.07049, -0.13822, -0.06981, -0.07058, -0.06976, -0.06732,
     1            -0.06226, -0.05082, -0.05217, -0.06397, -0.07494, -0.09387,
     2            -0.10977, -0.12342, -0.15056, -0.17099, -0.19999, -0.21978,
     3            -0.2453, -0.26119 /
      Data b4   / -1.18164, -0.94043, -1.18362, -1.18653, -1.19699, -1.21315,
     1            -1.24101, -1.3039, -1.32996, -1.31888, -1.27072, -1.16008,
     2            -1.05535, -0.97014, -0.83799, -0.77438, -0.72272, -0.70389,
     3            -0.69065, -0.6862 /
      Data b5   / 0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17,
     1            0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17 /
      Data b6   / 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8 /
      Data b7   / -0.354, -0.354, -0.354, -0.354, -0.354, -0.354, -0.354,
     1            -0.354, -0.354, -0.354, -0.354, -0.354, -0.354, -0.354,
     2            -0.354, -0.354, -0.354, -0.354, -0.354, -0.354 /
      Data b8   / -0.01329, -0.17037, -0.01349, -0.01189, -0.00748, 0.00788,
     1            0.03907, 0.08131, 0.1, 0.06727, 0.0162, -0.03697, -0.06582,
     2            -0.08511, -0.11756, -0.14267, -0.14621, -0.14621, -0.14621,
     3            -0.14621 /
      Data b9   / -0.09158, -0.08609, -0.09158, -0.09158, -0.09158, -0.09158,
     1            -0.09158, -0.09158, -0.09158, -0.09158, -0.09158, -0.09158,
     2            -0.09158, -0.01297, 0, 0, 0, 0, 0, 0 /
      Data b10  / -0.00156, -0.00052, -0.00156, -0.0016, -0.0017, -0.00182,
     1            -0.00197, -0.00235, -0.00267, -0.00296, -0.00275, -0.00204,
     2            -0.00161, -0.00127, -0.00066, -0.00022, 0, 0, 0, 0 /
      Data a1   / 0.57, 0.56, 0.574, 0.577, 0.581, 0.584, 0.588, 0.597, 0.606,
     1            0.624, 0.642, 0.678, 0.7, 0.673, 0.62, 0.62, 0.62, 0.62,
     2            0.62, 0.62 /
      Data a2   / 0.45, 0.46, 0.453, 0.455, 0.458, 0.46, 0.463, 0.469, 0.475,
     1            0.488, 0.5, 0.525, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55,
     2            0.55 /
      Data sd1  / 1.0521, 1.0449, 1.0444, 1.0424, 1.0459, 1.0557, 1.0609,
     1            1.0692, 1.0429, 1.0063, 0.9781, 0.9407, 0.943, 0.9519,
     2            1.0489, 1.0534, 1.0988, 1.1594, 1.1596, 1.0373 /
      Data sd2  / 0.7203, 0.6452, 0.715, 0.7137, 0.7113, 0.7155, 0.7166,
     1            0.7677, 0.7735, 0.7442, 0.7213, 0.6547, 0.6413, 0.6496,
     2            0.6582, 0.6342, 0.6173, 0.5724, 0.6251, 0.5409 /
      Data sb1  / -0.41997, -0.72057, -0.41729, -0.39998, -0.34799, -0.27572,
     1            -0.21231, -0.13909, -0.26492, -0.48496, -0.64239, -0.82052,
     2            -0.90568, -0.95097, -1.00027, -1.01881, -0.96317, -0.91305,
     3            -0.84242, -0.79231 /
      Data sb2  / -0.28846, -0.19688, -0.28685, -0.28241, -0.26842, -0.24759,
     1            -0.22385, -0.17798, -0.28832, -0.39525, -0.44574, -0.45287,
     2            -0.41105, -0.37956, -0.32233, -0.28172, -0.22449, -0.18388,
     3            -0.12665, -0.08605 /
      Data db1  / -0.21234, 0.20834, -0.22738, -0.241, -0.25702, -0.2526,
     1            -0.24029, -0.13086, -0.02321, -0.23382, -0.33645, -0.30222,
     2            -0.16785, 0.01469, 0.40612, 0.58724, 0.75311, 0.83361,
     3            0.90733, 0.82641 /
      Data db2  / -0.146, -0.146, -0.146, -0.146, -0.146, -0.146, -0.146,
     1            -0.146, -0.146, -0.146, -0.146, -0.146, -0.146, -0.146,
     2            -0.146, -0.146, -0.146, -0.146, -0.146, -0.146 /
      Data db3  / -0.03826, -0.05505, -0.03877, -0.03674, -0.0337, -0.0285,
     1            -0.02834, -0.03978, -0.03843, -0.03475, -0.03634, -0.03939,
     2            -0.04263, -0.04586, -0.05345, -0.06008, -0.07073, -0.07873,
     3            -0.08977, -0.09696 /
      Data db4  / 0.1721, 0.0489, 0.1741, 0.1759, 0.1691, 0.1603, 0.1614,
     1            0.1655, 0.1596, 0.2061, 0.2205, 0.1969, 0.1435, 0.0897,
     2            -0.0031, -0.0498, -0.0875, -0.1008, -0.1097, -0.1125 /
      Data db5  / -0.12, -0.12, -0.12, -0.12, -0.12, -0.12, -0.12, -0.12,
     1            -0.12, -0.12, -0.12, -0.12, -0.12, -0.12, -0.12, -0.12,
     2            -0.12, -0.12, -0.12, -0.12 /
      Data db6  / 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /
      Data db7  / 0.396, 0.396, 0.396, 0.396, 0.396, 0.396, 0.396, 0.396,
     1            0.396, 0.396, 0.396, 0.396, 0.396, 0.396, 0.396, 0.396,
     2            0.396, 0.396, 0.396, 0.396 /
      Data db8  / -0.11697, 0.17037, -0.11677, -0.11837, -0.12278, -0.13814,
     1            -0.16933, -0.21157, -0.23026, -0.19753, -0.14646, 0.03697,
     2            0.06582, 0.08511, 0.11756, 0.14267, 0.14621, 0.14621,
     3            0.14621, 0.14621 /
      Data db9  / 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.00185, 0.09158, 0.01297,
     1            0, 0, 0, 0, 0, 0 /
      Data db10 / 0.00156, 0.00052, 0.00156, 0.0016, 0.0017, 0.00182, 0.00197,
     1            0.00235, 0.00267, 0.00296, 0.00275, 0.00204, 0.00161, 0.00127,
     2            0.00066, 0.00022, 0, 0, 0, 0 /
      Data da1  / 0.12, 0.14, 0.116, 0.113, 0.109, 0.106, 0.122, 0.143, 0.154,
     1            0.136, 0.118, 0.082, 0.06, 0.087, 0.14, 0.16, 0.16, 0.16,
     2            0.16, 0.16 /
      Data da2  / 0.05, -0.02, 0.047, 0.045, 0.042, 0.04, 0.037, 0.031, 0.025,
     1            -0.018, -0.05, -0.075, -0.1, -0.1, -0.088, -0.025, 0.05,
     2            0.05, 0.05, 0.05 /
      Data dsd1 / -0.0808, -0.0597, -0.0723, -0.0701, -0.0705, -0.0597, -0.07,
     1            -0.0994, -0.0486, 0.0394, 0.1081, 0.0759, 0.0563, 0.0209,
     2            -0.0161, -0.0793, -0.1337, -0.2183, -0.2262, -0.2279 /
      Data dsd2 / -0.325, -0.3013, -0.3231, -0.318, -0.3143, -0.3064, -0.277,
     1            -0.3045, -0.3358, -0.2839, -0.305, -0.2651, -0.2308, -0.1724,
     2            -0.2379, -0.2569, -0.2358, -0.1482, -0.1356, -0.0862 /


C     Set parameters
      vref = 750.0
      c1 = 6.75
      c = 2.5
      n = 3.2
      vcon = 1000.0

C    Region=0 for Turkey
C    Region=1 for Iran
	    if (Region .eq. 0) then
          da1  =  da1  *0
          da2  =  da2  *0
          db1  =  db1  *0
          db2  =  db2  *0
          db3  =  db3  *0
          db4  =  db4  *0
          db5  =  db5  *0
          db6  =  db6  *0
          db7  =  db7  *0
          db8  =  db8  *0
          db9  =  db9  *0
          db10 =  db10 *0
          dsd1 =  dsd1 *0
          dsd2 =  dsd2 *0
		  dc1 = 0
		  else
		  dc1 =  0.25
      endif

c     Set Parameters to compute PGA rock
      a1p  = a1(1)  +  da1(1)
      a2p  = a2(1)  +  da2(1)
      b1p  = b1(1)  +  db1(1)
      b2p  = b2(1)  +  db2(1)
      b3p  = b3(1)  +  db3(1)
      b4p  = b4(1)  +  db4(1)
      b5p  = b5(1)  +  db5(1)
      b6p  = b6(1)  +  db6(1)
      b7p  = b7(1)  +  db7(1)
      b8p  = b8(1)  +  db8(1)
      b9p  = b9(1)  +  db9(1)
      b10p = b10(1) +  db10(1)
      sb1p = sb1(1)
      sb2p = sb2(1)
      sd1p = sd1(1) +  dsd1(1)
      sd2p = sd2(1) +  dsd2(1)

C First check for the PGA case (i.e., specT=0.0)
      nPer = 20
      if (specT .eq. 0.0) then
         period1 = period(1)
         a1T = a1(1)   +  da1(1)
         a2T = a2(1)   +  da2(1)
         b1T = b1(1)   +  db1(1)
         b2T = b2(1)   +  db2(1)
         b3T = b3(1)   +  db3(1)
         b4T = b4(1)   +  db4(1)
         b5T = b5(1)   +  db5(1)
         b6T = b6(1)   +  db6(1)
         b7T = b7(1)   +  db7(1)
         b8T = b8(1)   +  db8(1)
         b9T = b9(1)   +  db9(1)
         b10T = b10(1) +  db10(1)
         sb1T = sb1(1)
         sb2T = sb2(1)
         sd1T = sd1(1) +  dsd1(1)
         sd2T = sd2(1) +  dsd2(1)
         goto 1011
C Check for the PGV case (i.e., specT=-1.0)
      elseif (specT .eq. -1.0) then
         period1 = period(2)
         a1T = a1(2)   +  da1(2)
         a2T = a2(2)   +  da2(2)
         b1T = b1(2)   +  db1(2)
         b2T = b2(2)   +  db2(2)
         b3T = b3(2)   +  db3(2)
         b4T = b4(2)   +  db4(2)
         b5T = b5(2)   +  db5(2)
         b6T = b6(2)   +  db6(2)
         b7T = b7(2)   +  db7(2)
         b8T = b8(2)   +  db8(2)
         b9T = b9(2)   +  db9(2)
         b10T = b10(2) +  db10(2)
         sb1T = sb1(2)
         sb2T = sb2(2)
         sd1T = sd1(2) +  dsd1(2)
         sd2T = sd2(2) +  dsd2(2)
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=3,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'Kale, et al. (2015) Horizontal'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.

 1020 a1a  = a1(count1) + da1(count1)
      a2a  = a2(count1) + da2(count1)
      b1a  = b1(count1) + db1(count1)
      b2a  = b2(count1) + db2(count1)
      b3a  = b3(count1) + db3(count1)
      b4a  = b4(count1) + db4(count1)
      b5a  = b5(count1) + db5(count1)
      b6a  = b6(count1) + db6(count1)
      b7a  = b7(count1) + db7(count1)
      b8a  = b8(count1) + db8(count1)
      b9a  = b9(count1) + db9(count1)
      b10a = b10(count1)+ db10(count1)
      sd1a = sd1(count1)+ dsd1(count1)
      sd2a = sd2(count1)+ dsd2(count1)

      a1b  = a1(count2) + da1(count2)
      a2b  = a2(count2) + da2(count2)
      b1b  = b1(count2) + db1(count2)
      b2b  = b2(count2) + db2(count2)
      b3b  = b3(count2) + db3(count2)
      b4b  = b4(count2) + db4(count2)
      b5b  = b5(count2) + db5(count2)
      b6b  = b6(count2) + db6(count2)
      b7b  = b7(count2) + db7(count2)
      b8b  = b8(count2) + db8(count2)
      b9b  = b9(count2) + db9(count2)
      b10b = b10(count2)+ db10(count2)
      sd1b = sd1(count2)+ dsd1(count2)
      sd2b = sd2(count2)+ dsd2(count2)

            call S24_interp (period(count1),period(count2),a1a,a1b,
     +                   specT,a1T,iflag)
            call S24_interp (period(count1),period(count2),a2a,a2b,
     +                   specT,a2T,iflag)
            call S24_interp (period(count1),period(count2),b1a,b1b,
     +                   specT,b1T,iflag)
            call S24_interp (period(count1),period(count2),b2a,b2b,
     +                   specT,b2T,iflag)
            call S24_interp (period(count1),period(count2),b3a,b3b,
     +                   specT,b3T,iflag)
            call S24_interp (period(count1),period(count2),b4a,b4b,
     +                   specT,b4T,iflag)
            call S24_interp (period(count1),period(count2),b5a,b5b,
     +                   specT,b5T,iflag)
            call S24_interp (period(count1),period(count2),b6a,b6b,
     +                   specT,b6T,iflag)
            call S24_interp (period(count1),period(count2),b7a,b7b,
     +                   specT,b7T,iflag)
            call S24_interp (period(count1),period(count2),b8a,b8b,
     +                   specT,b8T,iflag)
            call S24_interp (period(count1),period(count2),b9a,b9b,
     +                   specT,b9T,iflag)
            call S24_interp (period(count1),period(count2),b10a,b10b,
     +                   specT,b10T,iflag)
            call S24_interp (period(count1),period(count2),sd1a,sd1b,
     +                   specT,sd1T,iflag)
            call S24_interp (period(count1),period(count2),sd2a,sd2b,
     +                   specT,sd2T,iflag)
            call S24_interp (period(count1),period(count2),sb1(count1),sb1(count2),
     +                   specT,sb1T,iflag)
            call S24_interp (period(count1),period(count2),sb2(count1),sb2(count2),
     +                   specT,sb2T,iflag)

 1011 period1 = specT

C.....Set the mechanism terms based on ftype............
C     Set mechanism term and corresponding Frv and Fnm values.
C     fType     Mechanism                      Rake
C     ------------------------------------------------------
C      -1       Normal                    -120 < Rake <  -60
C     -0.5      Normal/Oblique            -150 < Rake < -120
C                                          -60 < Rake <  -30
C       0       Strike-Slip               -180 < Rake < -150
C                                          -30 < Rake <   30
C                                          150 < Rake <  180
C      0.5      Reverse/Oblique             30 < Rake <   60
C                                          120 < Rake <  150
C       1       Reverse                     60 < Rake <  120
      if (ftype .eq. -1.0) then
         Fr = 0.0
         Fn = 1.0
      elseif (ftype .eq. -0.5) then
         Fr = 0.0
         Fn = 1.0
      elseif (ftype .eq. 0.0) then
         Fr = 0.0
         Fn = 0.0
      elseif (ftype .eq. 0.5) then
         Fr = 1.0
         Fn = 0.0
      elseif (ftype .eq. 1.0) then
         Fr = 1.0
         Fn = 0.0
      endif

C.....First compute the Reference Rock PGA value.........
      if (mag .le. c1+dc1) then
         fmag = b1p + b2p*(mag-(c1+dc1)) + b3p*(8.5-mag)**2.0
      else
         fmag = b1p + b7p*(mag-(c1+dc1)) + b3p*(8.5-mag)**2.0
      endif

         fdis = ( b4p + b5p*(mag-(c1+dc1)) ) * alog(sqrt(Rbjf*Rbjf+b6p*b6p))

         fsof = b8p*Fn + b9p*Fr

      if (Rbjf .le. 80) then
         faat = 0
      else
         faat = b10p *(Rbjf-80)
      endif

      term1 = fmag + fdis + fsof + faat
      pgaref = exp(term1)

C.....magnitude term........
      if (mag .le. c1+dc1) then
         fmag = b1T + b2T*(mag-(c1+dc1)) + b3T*(8.5-mag)**2.0
      else
         fmag = b1T + b7T*(mag-(c1+dc1)) + b3T*(8.5-mag)**2.0
      endif

C.....distance term........
         fdis = ( b4T + b5T*(mag-(c1+dc1)) ) * alog(sqrt(Rbjf*Rbjf+b6T*b6T))

C.....Style of fault term........
         fsof = b8T*Fn + b9T*Fr

C.....anelastic attenuation term........
      if (Rbjf .le. 80) then
         faat = 0
      else
         faat = b10T *(Rbjf-80)
      endif

C.....Site Response Term.........
      if (vs .lt. vref) then
        fsite = sb1T*alog(vs/vref) +
     1          sb2T*alog((pgaref+c*(vs/vref)**n)/((pgaref+c)*(vs/vref)**n))
      else
        fsite = sb1T*alog(min(vs,vcon)/vref)
      endif



      lnY = fmag + fdis + fsof + faat + fsite


C     Convert ground motion to units of gals.
      lnY = lnY + 6.89

      period2 = period1

C.....magnitude-dependent sigma.....

      if (mag .lt. 6.0) then
         w = a1T
       elseif(mag .lt. 6.5 .and. mag .ge. 6.0) then
         w = a1T + (a2T - a1T)*((mag - 6.0)/0.5)
       else
         w = a2T
      endif

      phi = w * sd1T
      tau = w * sd2T

      sigma = sqrt (tau**2 + phi**2)

      return
      END

c ---------------------------------------------------------------------
C ** Bradley (2010) Horizontal **
c ---------------------------------------------------------------------
      subroutine S02_Bradley_2010 ( m, Rrup, Rbjf, specT,
     1                     period2, lnY, sigma, iflag,
     2                     vs, Delta, DTor, Ftype, depthvs10,
     3                     vs30_class, hwflag, Rx )

      implicit none

      integer MAXPER
      parameter (MAXPER=23)
      REAL Period(MAXPER), C1(MAXPER), C1a(MAXPER), C1b(MAXPER)
      REAL cn(MAXPER), cm(MAXPER), c5(MAXPER), c6(MAXPER)
      REAL c7(MAXPER), c9(MAXPER), gamma1(MAXPER), gamma2(MAXPER)
      REAL phi1(MAXPER), phi2(MAXPER), phi3(MAXPER), phi4(MAXPER)
      REAL phi5(MAXPER), phi6(MAXPER), phi7(MAXPER), phi8(MAXPER)
      REAL tau1(MAXPER), tau2(MAXPER), sigma1(MAXPER), sigma2(MAXPER)
      REAL sigma3(MAXPER), c9a(MAXPER), period1
      real c3(MAXPER), c8(MAXPER) , ctvz(MAXPER)
      REAL c1T, c1aT, c1bT, cnT, cmT, c5T, c6T, c7T, c9T, c9aT, c3T, c8T, ctvzT
      REAL gamma1T, gamma2T, phi1T, phi2T, phi3T, phi4T, sigma3T
      REAL phi5T, phi6T, phi7T, phi8T, tau1T, tau2T, sigma1T, sigma2T
      REAL c2, c4, c4a, cRB, cHM, gm, pi, d2r, gamma
      real cc, cosdelta, r1, r2, r3, r4, hw, psa_ref, psa, a, b, c
      integer iflag, count1, count2, hwflag, nPer, i, vs30_class
      REAL M, RRUP, RBJF, DTOR, Delta, specT, sigma, Ftype, Rx, vs, v1
      REAL period2, lnY, F_RV, F_NM, depthvs10, sigma_M, tau, NL, rkdepth

      data period / 0.0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.075, 0.1, 0.15, 0.2,
     1             0.25, 0.3,   0.4,  0.5, 0.75,  1.0,   1.5, 2.0,  3.0, 4.0,
     1             5.0,  7.5,  10.0 /
      data c1 / -1.1985, -1.1958, -1.1756, -1.0909, -0.9793, -0.8549, -0.6008,
     1          -0.4700, -0.4139, -0.5237, -0.6678, -0.8277, -1.1284, -1.3926,
     1          -1.8664, -2.1935, -2.6883, -3.1040, -3.7085, -4.1486, -4.4881,
     1          -5.0891, -5.5530 /
      data c1a / 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.0999,
     1           0.0997, 0.0991, 0.0936, 0.0766, 0.0022, -0.0591, -0.0931, -0.0982,
     1          -0.0994, -0.0999, -0.1 /
      data c1b / -0.455, -0.455, -0.455, -0.455, -0.455, -0.455, -0.454, -0.453,
     1           -0.45, -0.4149, -0.3582, -0.3113, -0.2646, -0.2272, -0.162, -0.14,
     1           -0.1184, -0.11, -0.104, -0.102, -0.101, -0.101, -0.1 /
      data c3 / 1.50000, 1.50299, 1.50845, 1.51549, 1.52380, 1.53319, 1.56053,
     1          1.59241, 1.66640, 1.75021, 1.84052, 1.93480, 2.12764, 2.31684,
     1          2.73064, 3.03000, 3.43384, 3.67464, 3.64933, 3.60999, 3.50000,
     1          3.45000, 3.45000 /
      data cm / 5.85000, 5.81711, 5.80023, 5.78659, 5.77472, 5.76402, 5.74056,
     1          5.72017, 5.68493, 5.65435, 5.62686, 5.60162, 5.55602, 5.51513,
     1          5.38632, 5.31000, 5.29995, 5.32730, 5.43850, 5.59770, 5.72760,
     1          5.98910, 6.19300 /
      data cn / 2.996, 2.996, 3.292, 3.514, 3.563, 3.547, 3.448, 3.312, 3.044,
     1          2.831, 2.658, 2.505, 2.261, 2.087, 1.812, 1.648, 1.511, 1.47,
     1          1.456, 1.465, 1.478, 1.498, 1.502 /
      data c5 / 6.16, 6.16, 6.158, 6.155, 6.1508, 6.1441, 6.12, 6.085, 5.9871,
     1          5.8699, 5.7547, 5.6527, 5.4997, 5.4029, 5.29, 5.248, 5.2194,
     1          5.2099, 5.204, 5.202, 5.201, 5.2, 5.2 /
      data c6 / 0.4893, 0.4893, 0.4892, 0.489, 0.4888, 0.4884, 0.4872, 0.4854,
     1          0.4808, 0.4755, 0.4706, 0.4665, 0.4607, 0.4571, 0.4531, 0.4517,
     1          0.4507, 0.4504, 0.4501, 0.4501, 0.45, 0.45, 0.45 /
      data c7 / 0.0512, 0.0512, 0.0512, 0.0511, 0.0508, 0.0504, 0.0495, 0.0489,
     1          0.0479, 0.0471, 0.0464, 0.0458, 0.0445, 0.0429, 0.0387, 0.035,
     1          0.028, 0.0213, 0.0106, 0.0041, 0.001, 0.0, 0.0 /
      data c8 / 10.0, 10.0, 10.0, 10.0, 10.0, 10.0, 10.0, 10.0, 10.0, 10.0,
     1          10.5, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 18.0, 19.0, 19.75,
     1          20.0, 20.0, 20.0 /
      data c9  / 0.79, 0.79, 0.8129, 0.8439, 0.874, 0.8996, 0.9442, 0.9677,
     1           0.966, 0.9334, 0.8946, 0.859, 0.8019, 0.7578, 0.6788,
     1           0.6196, 0.5101, 0.3917, 0.1244, 0.0086, 0.0, 0.0, 0.0 /
      data c9a / 1.5005, 1.5005, 1.5028, 1.5071, 1.5138, 1.523, 1.5597,
     1           1.6104, 1.7549, 1.9157, 2.0709, 2.2005, 2.3886, 2.5,
     1           2.6224, 2.669, 2.6985, 2.7085, 2.7145, 2.7164, 2.7172,
     1           2.7177, 2.718 /
      data gamma1 / -0.0096, -0.0096, -0.0097, -0.0101, -0.0105, -0.0109,
     1              -0.0117, -0.0117, -0.0111, -0.0100, -0.0091, -0.0082,
     1              -0.0069, -0.0059, -0.0045, -0.0037, -0.0028, -0.0023,
     1              -0.0019, -0.0018, -0.0017, -0.0017, -0.0017 /
      data gamma2 / -0.00480, -0.00481, -0.00486, -0.00503, -0.00526, -0.00549,
     1              -0.00588, -0.00591, -0.00540, -0.00479, -0.00427, -0.00384,
     1              -0.00317, -0.00272, -0.00209, -0.00175, -0.00142, -0.00143,
     1              -0.00115, -0.00104, -0.00099, -0.00094, -0.00091 /
      data ctvz / 2.0000, 2.0000, 2.0000, 2.0000, 2.0000, 2.0000, 2.0000, 2.0000,
     1            2.0000, 2.0000, 2.0000, 2.5000, 3.2000, 3.5000, 4.500, 5.0000,
     1            5.4000, 5.8000, 6.0000, 6.1500, 6.3000, 6.4250, 6.5500 /

      data phi1 / -0.4417, -0.4417, -0.434, -0.4177, -0.4, -0.3903, -0.404,
     1            -0.4423, -0.5162, -0.5697, -0.6109, -0.6444, -0.6931, -0.7246,
     1            -0.7708, -0.799, -0.8382, -0.8663, -0.9032, -0.9231, -0.9222,
     1            -0.8346, -0.7332 /
      data phi2 / -0.1417, -0.1417, -0.1364, -0.1403, -0.1591, -0.1862, -0.2538,
     1            -0.2943, -0.3113, -0.2927, -0.2662, -0.2405, -0.1975, -0.1633,
     1            -0.1028, -0.0699, -0.0425, -0.0302, -0.0129, -0.0016,  0.0, 0.0, 0.0 /
      data phi3 / -0.00701, -0.00701, -0.007279, -0.007354, -0.006977, -0.006467,
     1            -0.005734, -0.005604, -0.005845, -0.006141, -0.006439, -0.006704,
     1            -0.007125, -0.007435, -0.00812, -0.008444, -0.007707, -0.004792,
     1            -0.001828, -0.001523, -0.00144, -0.001369, -0.001361 /
      data phi4 / 0.102151, 0.102151, 0.10836, 0.119888, 0.133641, 0.148927, 0.190596,
     1            0.230662, 0.266468, 0.255253, 0.231541, 0.207277, 0.165464,
     1            0.133828, 0.085153, 0.058595, 0.031787, 0.019716, 0.009643,
     1            0.005379, 0.003223, 0.001134, 0.000515 /
      data phi5 /  0.2289, 0.2289, 0.2289, 0.2289, 0.2289, 0.229, 0.2292, 0.2297,
     1             0.2326, 0.2386, 0.2497, 0.2674, 0.312, 0.361, 0.4353, 0.4629,
     1             0.4756, 0.4785, 0.4796, 0.4799, 0.4799, 0.48, 0.48 /
      data phi6  / 0.014996, 0.014996, 0.014996, 0.014996, 0.014996, 0.014996,
     1             0.014996, 0.014996, 0.014988, 0.014964, 0.014881, 0.014639,
     1             0.013493,  0.011133, 0.006739, 0.005749, 0.005544, 0.005521,
     1             0.005517, 0.005517, 0.005517, 0.005517, 0.005517 /
      data phi7  / 580.0, 580.0, 580.0, 580.0, 579.9, 579.9, 579.6, 579.2, 577.2,
     1             573.9, 568.5, 560.5, 540.0, 512.9, 441.9, 391.8, 348.1, 332.5,
     1             324.1, 321.7, 320.9, 320.3, 320.1 /
      data phi8  / 0.07, 0.07, 0.0699, 0.0701, 0.0702, 0.0701, 0.0686, 0.0646,
     1             0.0494, -0.0019, -0.0479, -0.0756, -0.096, -0.0998, -0.0765,
     1            -0.0412, 0.014, 0.0544, 0.1232, 0.1859, 0.2295, 0.266, 0.2682 /
      data tau1  / 0.3437, 0.3437, 0.3471, 0.3603, 0.3718, 0.3848, 0.3878, 0.3835,
     1             0.3719, 0.3601, 0.3522, 0.3438, 0.3351, 0.3353, 0.3429, 0.3577,
     1             0.3769, 0.4023, 0.4406, 0.4784, 0.5074, 0.5328, 0.5542 /
      data tau2  / 0.2637, 0.2637, 0.2671, 0.2803, 0.2918, 0.3048, 0.3129, 0.3152,
     1             0.3128, 0.3076, 0.3047, 0.3005, 0.2984, 0.3036, 0.3205, 0.3419,
     1             0.3703, 0.4023, 0.4406, 0.4784, 0.5074, 0.5328, 0.5542 /
      data sigma1 / 0.4458, 0.4458, 0.4458, 0.4535, 0.4589, 0.463, 0.4702, 0.4747,
     1              0.4798, 0.4816, 0.4815, 0.4801, 0.4758, 0.471, 0.4621, 0.4581,
     1              0.4493, 0.4459, 0.4433, 0.4424, 0.442,  0.4416, 0.4414 /
      data sigma2 / 0.3459, 0.3459, 0.3459, 0.3537, 0.3592, 0.3635, 0.3713, 0.3769,
     1              0.3847, 0.3902, 0.3946, 0.3981, 0.4036, 0.4079, 0.4157, 0.4213,
     1              0.4213, 0.4213, 0.4213, 0.4213, 0.4213, 0.4213, 0.4213 /
      data sigma3 / 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.7999,
     1              0.7997, 0.7988, 0.7966, 0.7792, 0.7504, 0.7136, 0.7035,
     1              0.7006, 0.7001, 0.7, 0.7, 0.7 /


c      Note: Ctvz array is included but the model does not account for this.
C            To apply this term in the model the distance of travel within the
C            volcanic zone would need to be computed and used.

C Find the requested spectral period and corresponding coefficients
      nPer = 23
C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1  = period(1)
         c1T      = c1(1)
         c1aT     = c1a(1)
         c1bT     = c1b(1)
         cnT      = cn(1)
         cmT      = cm(1)
         c3T      = c3(1)
         c5T      = c5(1)
         c6T      = c6(1)
         c7T      = c7(1)
         c8T      = c8(1)
         c9T      = c9(1)
         c9aT     = c9a(1)
         ctvzT    = ctvz(1)
         gamma1T  = gamma1(1)
         gamma2T  = gamma2(1)
         phi1T    = phi1(1)
         phi2T    = phi2(1)
         phi3T    = phi3(1)
         phi4T    = phi4(1)
         phi5T    = phi5(1)
         phi6T    = phi6(1)
         phi7T    = phi7(1)
         phi8T    = phi8(1)
         tau1T    = tau1(1)
         tau2T    = tau2(1)
         sigma1T = sigma1(1)
         sigma2T = sigma2(1)
         sigma3T = sigma3(1)
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'Bradley (2010) Horizontal'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020       call S24_interp (period(count1),period(count2),c1(count1),c1(count2),
     +                   specT,c1T,iflag)
            call S24_interp (period(count1),period(count2),c1a(count1),c1a(count2),
     +                   specT,c1aT,iflag)
            call S24_interp (period(count1),period(count2),c1b(count1),c1b(count2),
     +                   specT,c1bT,iflag)
            call S24_interp (period(count1),period(count2),cn(count1),cn(count2),
     +                   specT,cnT,iflag)
            call S24_interp (period(count1),period(count2),cm(count1),cm(count2),
     +                   specT,cmT,iflag)
            call S24_interp (period(count1),period(count2),c3(count1),c3(count2),
     +                   specT,c3T,iflag)
            call S24_interp (period(count1),period(count2),c5(count1),c5(count2),
     +                   specT,c5T,iflag)
            call S24_interp (period(count1),period(count2),c6(count1),c6(count2),
     +                   specT,c6T,iflag)
            call S24_interp (period(count1),period(count2),c7(count1),c7(count2),
     +                   specT,c7T,iflag)
            call S24_interp (period(count1),period(count2),c8(count1),c8(count2),
     +                   specT,c8T,iflag)
            call S24_interp (period(count1),period(count2),c9(count1),c9(count2),
     +                   specT,c9T,iflag)
            call S24_interp (period(count1),period(count2),c9a(count1),c9a(count2),
     +                   specT,c9aT,iflag)
            call S24_interp (period(count1),period(count2),ctvz(count1),ctvz(count2),
     +                   specT,ctvzT,iflag)
            call S24_interp (period(count1),period(count2),gamma1(count1),gamma1(count2),
     +                   specT,gamma1T,iflag)
            call S24_interp (period(count1),period(count2),gamma2(count1),gamma2(count2),
     +                   specT,gamma2T,iflag)
            call S24_interp (period(count1),period(count2),phi1(count1),phi1(count2),
     +                   specT,phi1T,iflag)
            call S24_interp (period(count1),period(count2),phi2(count1),phi2(count2),
     +                   specT,phi2T,iflag)
            call S24_interp (period(count1),period(count2),phi3(count1),phi3(count2),
     +                   specT,phi3T,iflag)
            call S24_interp (period(count1),period(count2),phi4(count1),phi4(count2),
     +                   specT,phi4T,iflag)
            call S24_interp (period(count1),period(count2),phi5(count1),phi5(count2),
     +                   specT,phi5T,iflag)
            call S24_interp (period(count1),period(count2),phi6(count1),phi6(count2),
     +                   specT,phi6T,iflag)
            call S24_interp (period(count1),period(count2),phi7(count1),phi7(count2),
     +                   specT,phi7T,iflag)
            call S24_interp (period(count1),period(count2),phi8(count1),phi8(count2),
     +                   specT,phi8T,iflag)
            call S24_interp (period(count1),period(count2),tau1(count1),tau1(count2),
     +                   specT,tau1T,iflag)
            call S24_interp (period(count1),period(count2),tau2(count1),tau2(count2),
     +                   specT,tau2T,iflag)
            call S24_interp (period(count1),period(count2),sigma1(count1),sigma1(count2),
     +                   specT,sigma1T,iflag)
            call S24_interp (period(count1),period(count2),sigma2(count1),sigma2(count2),
     +                   specT,sigma2T,iflag)
            call S24_interp (period(count1),period(count2),sigma3(count1),sigma3(count2),
     +                   specT,sigma3T,iflag)
c            call S24_interp (period(count1),period(count2),sigma4(count1),sigma4(count2),
c     +                   specT,sigma4T,iflag)

 1011 period1 = specT

c     Set the fault mechanism term.
C     fType     Mechanism                      Rake
C     ------------------------------------------------------
C      -1       Normal                   -120 < Rake < -60.0
C     1, 0.5    Reverse and Rev/Obl        30 < Rake < 150.0
C     0,-0.5    Strike-Slip and NMl/Obl        Otherwise
         if (ftype .eq. -1) then
            F_RV = 0.0
            F_NM = 1.0
         elseif (ftype .ge. 0.5) then
            F_RV = 1.0
            F_NM = 0.0
         else
            F_RV = 0.0
            F_NM = 0.0
         endif

        c2 = 1.06
        c4 = -2.1
        c4a = -0.5
        cRB = 50.0
        cHM = 3.0
        gm = 4.0

        pi = atan(1.0)*4.0
        d2r = pi/180.0

        cc = c5T* cosh(c6T * max((M-cHM),0.0))
        gamma = gamma1T + gamma2T/cosh(max((M-gm),0.0))
        cosDELTA = cos(abs(DELTA)*d2r)

c Magnitude scaling
        r1 = c1T + c2 * (M-6.0) +
     1       (c2-c3T)/cnT *
     1             alog(1.0 + exp(-cnT*(M-cMT)))

c Near-field magnitude and distance scaling
        r2 = c4 * alog(Rrup + cc)

c Distance scaling at large distance
        r3 = (c4a-c4)/2.0 *
     1            alog( Rrup*Rrup+cRB*cRB ) +
     1       Rrup * gamma

c More source scaling (Adjusted C7-DTor term)
        r4 = c1aT*F_RV +
     1       c1bT*F_NM +
     1       c7T*(min(DTor,c8T) - 4.0)

c HW effect
        if (HWFlag .eq. 0) then
           hw = 0.0
        else
           hw = c9T * tanh(Rx*cosDelta**2.0/c9aT) *
     1          (1.0 - sqrt(Rbjf**2.0+DTor**2.0)/(Rrup + 0.001))
        endif
c Predicted median Sa on reference condition (Vs=1130 m/sec)
        psa_ref = r1+r2+r3+r4+hw

C     Compute V1
        if (specT .eq. 0.0) then
           v1 = 1800.0
        elseif (spect .gt. 0.0) then
           v1 = min(max(1130.0*(specT/0.75)**(-0.11),1130.0),1800.0)
        endif

c Linear soil amplification
        a = phi1T * alog(min(Vs,v1)/1130.0)

c Nonlinear soil amplification
        b = phi2T *
     1      (exp(phi3T*(min(Vs,1130.0)-360.0)) - exp(phi3T*(1130.0-360.0)))
        c = phi4T

C Deviation from ln(Vs30) scaling: bedrock depth (Z1) effect
C NOTE: max(0,z1-15) is capped at 300 to avoid overflow of function cosh
        rkdepth = phi5T*
     1            (1.0-1.0/cosh(phi6T*max(0.0,depthvs10*1000.0-phi7T))) +
     1            phi8T/cosh(0.15*min(max(0.0,depthvs10*1000.0-15.0),300.0))

c Sa on soil condition
        psa = psa_ref + (a + b * alog((exp(psa_ref)+c)/c)) + rkdepth

C Compute the sigma term
        NL = b*exp(psa_ref)/(exp(psa_ref)+c)
        sigma_M = sigma1T + (sigma2T-sigma1T)/2.0*(min(max(M,5.0),7.0)-5.0)
        if (vs30_class .eq. 0) then
           sigma = sigma_M*sqrt(sigma3T+(1.0+NL)**2.0)
        else
           sigma = sigma_M*sqrt(0.7+(1.0+NL)**2.0)
        endif
C Compute the tau term
        tau = tau1T + (tau2T-tau1T)/2.0*(min(max(M,5.0),7.0)-5.0)
        tau = tau*(1.0+NL)

        sigma = sqrt(tau*tau + sigma*sigma)

C     Convert ground motion to units of gals.
      lnY = psa + 6.89
      period2 = period1

      return
      end

c----------------------------------------------------------------------
      subroutine S02_McVerry_crustal_2006 ( m, Rrup, specT,
     1                     period2, lnY, sigma, iflag, Ftype,
     3                     hwflag, Sc, Sd )

      Implicit None

      integer MAXPER
      parameter (MAXPER=14)
      REAL Period(MAXPER), C1(MAXPER), C3(MAXPER), C4(MAXPER)
      REAL c5(MAXPER), c6(MAXPER), c8(MAXPER), c10(MAXPER)
      REAL c32(MAXPER) ,c33(MAXPER), sigmaM6(MAXPER), sigslope(MAXPER)
      REAL tau(MAXPER), a9(MAXPER), C29(MAXPER), c30(MAXPER), c43(MAXPER), c46(MAXPER)

      REAL c1T, c3T, c4T, c5T, c6T, c8T, c10T, c32T, c33T
      REAL sigmaM6T, sigslopeT, tauT, sigma, a9T, c29T, c30T, c43T, c46T
      REAL M, RRUP, specT, Ftype, sig, Sc, Sd, RVOL
      REAL period2, lnY, F_RV, F_NM, lnYab, period1
      REAL c1up, c3up, c4up, c5up, c6up, c8up, c10up, c29up, c30up, c43up, c46up
      REAL c32up, c33up, fhw, fhwm, fhwr, fhwp, fhwmp, fhwrp
      REAL pgaupab, pgaup, pgapab, pgap

      integer iflag, count1, count2, hwflag, nPer, i

      Data Period / 0.0, 0.01, 0.03, 0.075, 0.1, 0.2, 0.3, 0.4, 0.5, 0.75, 1.0, 1.5, 2.0, 3.0 /
      Data C1 / 0.07713, 0.07713, 0.07713, 1.2205, 1.53365, 1.22565, 0.21124, -0.10541, -0.1426,
     1         -0.65968, -0.51404, -0.95399, -1.24167, -1.5657 /
      Data C3 / 0.0, 0.0, 0.0, 0.03, 0.028, -0.0138, -0.036, -0.0518, -0.0635, -0.0862,
     1         -0.102, -0.12, -0.12, -0.1726 /
      Data C4 / -0.144, -0.144, -0.144, -0.144, -0.144, -0.144, -0.144, -0.144, -0.144,
     1          -0.144, -0.144, -0.144, -0.144, -0.144 /
      Data C5 / -0.00898, -0.00898, -0.00898, -0.00914, -0.00903, -0.00975, -0.01032,
     1          -0.00941, -0.00878, -0.00802, -0.00647, -0.00713, -0.00713, -0.00623 /
      Data C6 / 0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17,
     1          0.17, 0.17, 0.17 /
      Data C8 / -0.73728, -0.73728, -0.73728, -0.93059, -0.96506, -0.75855, -0.524, -0.50802,
     1          -0.52214, -0.47264, -0.58672, -0.49268, -0.49268, -0.52257 /
      Data C10 / 5.6, 5.6, 5.6, 5.58, 5.5, 5.1, 4.8, 4.52, 4.3, 3.9, 3.7, 3.55, 3.55, 3.5 /

      data C29 / 0.3873, 0.3873, 0.3873, 0.27879, 0.28619, 0.34064, 0.53213, 0.63272,
     1           0.58809, 0.50708, 0.33002, 0.07445, 0.07445, 0.09869 /
      data C30 / -0.23, -0.23, -0.23, -0.28, -0.28, -0.245, -0.195, -0.16, -0.121, -0.05,
     1            0.0, 0.04, 0.04, 0.04 /
      Data C32 / 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2 /
      Data C33 / 0.26, 0.26, 0.26, 0.26, 0.26, 0.26, 0.198, 0.154, 0.119, 0.057, 0.013,
     1         -0.049, -0.049, -0.156 /
      data C43 / -0.31036, -0.31036, -0.31036, -0.49068, -0.46604, -0.31282, -0.07565,
     1            0.17615, 0.34775, 0.7238, 0.89239, 0.77743, 0.77743, 0.60938 /
      data C46 / -0.03250, -0.03250, -0.03250, -0.03441, -0.03594, -0.03823, -0.03535,
     1           -0.03354, -0.03211, -0.02857, -0.02500, -0.02008, -0.02008, -0.01587 /
      Data SigmaM6 / 0.4871, 0.4871, 0.4871, 0.5297, 0.5401, 0.5599, 0.5456, 0.5556,
     1               0.5658, 0.5611, 0.5573, 0.5419, 0.5419, 0.5809 /
      Data sigslope / -0.1011, -0.1011, -0.1011, -0.0703, -0.0292, 0.0172, -0.0566, -0.1064,
     1                -0.1123, -0.0836, -0.062, 0.0385, 0.0385, 0.1403 /
      Data tau / 0.2469, 0.2469, 0.2469, 0.3139, 0.3017, 0.2583, 0.1967, 0.1802,
     1           0.144, 0.1871, 0.2073, 0.2405, 0.2405, 0.2053 /
      data a9 / 0.370, 0.370, 0.370, 0.370, 0.370, 0.370, 0.370, 0.370, 0.370,
     1          0.331, 0.281, 0.210, 0.160, 0.089 /
C     Notes Travel path term for Volcanic region is not included. This subroutine
C     is also only for crustal earthquakes and includes the Hanging Wall model
C     of Abrahamson and Silva (1997) as recommended in the McVerry et al. (2006) paper.

C Find the requested spectral period and corresponding coefficients
      nPer = 14
C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1   = period(1)
         c1T       = c1(1)
         c3T       = c3(1)
         c4T       = c4(1)
         c5T       = c5(1)
         c6T       = c6(1)
         c8T       = c8(1)
         c10T      = c10(1)
         c29T      = c29(1)
         c30T      = c30(1)
         c32T      = c32(1)
         c33T      = c33(1)
         c43T      = c43(1)
         c46T      = c46(1)
         sigmaM6T  = sigmaM6(1)
         sigslopeT = sigslope(1)
         tauT      = tau(1)
         a9T       = a9(1)
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'McVerry et al. (2006) Crustal Horizontal'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),c1(count1),c1(count2),
     +             specT,c1T,iflag)
      call S24_interp (period(count1),period(count2),c3(count1),c3(count2),
     +             specT,c3T,iflag)
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),
     +             specT,c4T,iflag)
      call S24_interp (period(count1),period(count2),c5(count1),c5(count2),
     +             specT,c5T,iflag)
      call S24_interp (period(count1),period(count2),c6(count1),c6(count2),
     +             specT,c6T,iflag)
      call S24_interp (period(count1),period(count2),c8(count1),c8(count2),
     +             specT,c8T,iflag)
      call S24_interp (period(count1),period(count2),c10(count1),c10(count2),
     +             specT,c10T,iflag)
      call S24_interp (period(count1),period(count2),c29(count1),c29(count2),
     +             specT,c29T,iflag)
      call S24_interp (period(count1),period(count2),c30(count1),c30(count2),
     +             specT,c30T,iflag)
      call S24_interp (period(count1),period(count2),c32(count1),c32(count2),
     +             specT,c32T,iflag)
      call S24_interp (period(count1),period(count2),c33(count1),c33(count2),
     +             specT,c33T,iflag)
      call S24_interp (period(count1),period(count2),c43(count1),c43(count2),
     +             specT,c43T,iflag)
      call S24_interp (period(count1),period(count2),c46(count1),c46(count2),
     +             specT,c46T,iflag)
      call S24_interp (period(count1),period(count2),sigmaM6(count1),sigmaM6(count2),
     +             specT,sigmaM6T,iflag)
      call S24_interp (period(count1),period(count2),sigslope(count1),sigslope(count2),
     +             specT,sigslopeT,iflag)
      call S24_interp (period(count1),period(count2),tau(count1),tau(count2),
     +             specT,tauT,iflag)
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +             specT,a9T,iflag)

 1011 period1 = specT

c     Set the fault mechanism term.
C     fType     Mechanism
C     ----------------------------------
C      -1       Normal
C     1, 0.5    Reverse and Rev/Obl
C     0,-0.5    Strike-Slip and NMl/Obl
      if (ftype .eq. -1.0) then
          F_RV = 0.0
          F_NM = 1.0
      elseif (ftype .ge. 0.5) then
          F_RV = 1.0
          F_NM = 0.0
      else
          F_RV = 0.0
          F_NM = 0.0
      endif

C     First compute the AS(1997) Hanging Wall
      if (HWFlag .eq. 1) then
         if(M. le. 5.5) then
            fhwm = 0.0
            fhwmp = 0.0
         elseif (M .gt. 5.5 .and. M .lt. 6.5) then
            fhwm = (M-5.5)
            fhwmp = (M-5.5)
         else
            fhwm = 1.0
            fhwmp = 1.0
         endif

         if (Rrup .lt. 4) then
            fhwr = 0.0
            fhwrp = 0.0
         elseif (Rrup .ge. 4.0 .and. Rrup .lt. 8.0) then
            fhwr = a9T*(Rrup-4.0)/4.0
            fhwrp = a9(1)*(Rrup-4.0)/4.0
         elseif (Rrup .ge. 8.0 .and. Rrup .lt. 18.0) then
            fhwr = a9T
            fhwrp = a9(1)
         elseif (Rrup .ge. 18.0 .and. Rrup .lt. 24.0) then
            fhwr = a9T*(1.0-(Rrup-18.0)/7.0)
            fhwrp = a9(1)*(1.0-(Rrup-18.0)/7.0)
         else
            fhwr = 0.0
            fhwrp = 0.0
         endif

         fhw = fhwm*fhwr
         fhwp = fhwmp*fhwrp
      else
        fhw = 0.0
        fhwp = 0.0
      endif

c     Set distance through volcanic attenuation
      RVOL = 0.0*Rrup


C     Next compute the PGA unprimed for later scaling.
      c1up = 0.14274
      c3up = 0.0
      c4up = -0.144
      c5up = -0.00989
      c6up = 0.17
      c8up = -0.68744
      c10up = 5.6
      c29up = 0.27315
      c30up = -0.23
      c43up = -0.33716
      c46up = -0.03255
      c32up = 0.2
      c33up = 0.26

      pgaupab = c1up + c4up*(M-6.0) + c3up*(8.5-M)**2.0 + c5up*Rrup +
     1        (c8up+c6up*(M-6.0))*alog(sqrt(Rrup*Rrup+c10up*c10up)) +
     2        c46up*RVOL +
     2        c32up*F_NM + c33up*F_RV + fhwp

      pgaup = pgaupab + c29up*Sc + (c30up*alog(exp(pgaupab)+0.03) + c43up)*Sd

C     Next compute the primed PGA value.
      pgapab = c1(1) + c4(1)*(M-6.0) + c3(1)*(8.5-M)**2.0 + c5(1)*Rrup +
     1        (c8(1)+c6(1)*(M-6.0))*alog(sqrt(Rrup*Rrup+c10(1)*c10(1))) +
     2        c46(1)*RVOL +
     2        c32(1)*F_NM + c33(1)*F_RV + fhwp

      pgap = pgapab + c29(1)*Sc + (c30(1)*alog(exp(pgapab)+0.03) + c43(1))*Sd

C     Now compute the ground motion for the given spectral period.
      lnYab = c1T + c4T*(M-6.0) + c3T*(8.5-M)**2.0 + c5T*Rrup +
     1        (c8T+c6T*(M-6.0))*alog(sqrt(Rrup*Rrup+c10T*c10T)) +
     2        c46T*RVOL +
     2        c32T*F_NM + c33T*F_RV + fhwp

      lnY = lnYab + c29T*Sc + (c30T*alog(exp(pgap)+0.03) + c43T)*Sd

C     Now make dataset adjustment.
      lnY = lnY + (pgaup - pgap)

C Compute the sigma term
      if (M .lt. 5.0) then
         sig = sigmaM6T - sigslopeT
      elseif (M .gt. 7.0) then
         sig = sigmaM6T + sigslopeT
      else
         sig = sigmaM6T + sigslopeT*(M-6.0)
      endif

      sigma = sqrt(tauT*tauT + sig*sig)

C     Convert ground motion to units of gals.
      lnY = lnY + 6.89
      period2 = period1

      return
      end
c----------------------------------------------------------------------
      subroutine S02_McVerry_Subduction_2006 ( m, Rrup, specT,
     1                     period2, lnY, sigma, iflag, Ftype,
     3                     depthtop, dip, width, hypodepth, Sc, Sd )

      Implicit None

      integer MAXPER
      parameter (MAXPER=14)
      REAL Period(MAXPER), C11(MAXPER), C12(MAXPER), C13(MAXPER)
      REAL c15(MAXPER), c17(MAXPER), c18(MAXPER), c19(MAXPER)
      REAL c20(MAXPER) ,c24(MAXPER), sigmaM6(MAXPER), sigslope(MAXPER)
      REAL tau(MAXPER), a9(MAXPER), C29(MAXPER), c30(MAXPER), c43(MAXPER), c46(MAXPER)

      REAL c11T, c12T, c13T, c15T, c17T, c18T, c19T, c20T, c24T
      REAL sigmaM6T, sigslopeT, tauT, sigma, a9T, c29T, c30T, c43T, c46T
      REAL M, RRUP, specT, Ftype, sig, Sc, Sd, RVOL
      REAL depthtop, dip, width, hypodepth
      REAL period2, lnY, lnYAB, period1, SI, Hc, DS
      REAL c11up, c12up, c13up, c15up, c17up, c18up, c19up, c29up, c30up, c43up, c46up
      REAL c20up, c24up
      REAL pgaupab, pgaup, pgapab, pgap

      integer iflag, count1, count2, nPer, i

      Data Period / 0.0, 0.01, 0.03, 0.075, 0.1, 0.2, 0.3, 0.4, 0.5, 0.75, 1.0, 1.5, 2.0, 3.0 /
      Data C11 / 8.08611, 8.08611, 8.08611, 8.69303, 9.30400, 10.41628, 9.21783,
     1           8.01150, 7.87495, 7.26785, 6.98741, 6.77543, 6.48775, 5.05424 /
      Data C12 / 1.41400, 1.41400, 1.41400, 1.41400, 1.41400, 1.41400, 1.41400,
     1           1.41400, 1.41400, 1.41400, 1.41400, 1.41400, 1.41400, 1.41400 /
      Data C13 / 0.00000, 0.00000, 0.00000, 0.00000, -0.00110, -0.00270, -0.00360,
     1          -0.00430, -0.00480, -0.00570, -0.00640, -0.00730, -0.00730, -0.00890 /
      Data C15 / -2.55200, -2.55200, -2.55200, -2.70700, -2.65500, -2.52800, -2.45400,
     1           -2.40100, -2.36000, -2.28600, -2.23400, -2.16000, -2.16000, -2.03300 /
      Data C17 / -2.49894, -2.49894, -2.49894, -2.55903, -2.61372, -2.70038, -2.47356,
     1           -2.30457, -2.31991, -2.28460, -2.28256, -2.27895, -2.27895, -2.05560 /
      Data C18 / 1.78180, 1.78180, 1.78180, 1.78180, 1.78180, 1.78180, 1.78180,
     1           1.78180, 1.78180, 1.78180, 1.78180, 1.78180, 1.78180, 1.78180 /
      Data C19 / 0.55400, 0.55400, 0.55400, 0.55400, 0.55400, 0.55400, 0.55400,
     1           0.55400, 0.55400, 0.55400, 0.55400, 0.55400, 0.55400, 0.55400 /
      Data C20 / 0.01590, 0.01590, 0.01590, 0.01821, 0.01737, 0.01531, 0.01304,
     1           0.01426, 0.01277, 0.01055, 0.00927, 0.00748, 0.00748, -0.00273 /
      Data C24 / -0.43223, -0.43223, -0.43223, -0.52504, -0.61452, -0.65966, -0.56604,
     1           -0.33169, -0.24374, -0.01583, 0.02009, -0.07051, -0.07051, -0.23967 /
      Data C29 / 0.3873, 0.3873, 0.3873, 0.27879, 0.28619, 0.34064, 0.53213, 0.63272,
     1           0.58809, 0.50708, 0.33002, 0.07445, 0.07445, 0.09869 /
      Data C30 / -0.23, -0.23, -0.23, -0.28, -0.28, -0.245, -0.195, -0.16, -0.121, -0.05,
     1            0.0, 0.04, 0.04, 0.04 /
      Data C43 / -0.31036, -0.31036, -0.31036, -0.49068, -0.46604, -0.31282, -0.07565,
     1            0.17615, 0.34775, 0.7238, 0.89239, 0.77743, 0.77743, 0.60938 /

      Data C46 / -0.03250, -0.03250, -0.03250, -0.03441, -0.03594, -0.03823, -0.03535,
     1           -0.03354, -0.03211, -0.02857, -0.02500, -0.02008, -0.02008, -0.01587 /


      Data SigmaM6 / 0.4871, 0.4871, 0.4871, 0.5297, 0.5401, 0.5599, 0.5456, 0.5556,
     1               0.5658, 0.5611, 0.5573, 0.5419, 0.5419, 0.5809 /
      Data sigslope / -0.1011, -0.1011, -0.1011, -0.0703, -0.0292, 0.0172, -0.0566, -0.1064,
     1                -0.1123, -0.0836, -0.062, 0.0385, 0.0385, 0.1403 /
      Data tau / 0.2469, 0.2469, 0.2469, 0.3139, 0.3017, 0.2583, 0.1967, 0.1802,
     1           0.144, 0.1871, 0.2073, 0.2405, 0.2405, 0.2053 /
      data a9 / 0.370, 0.370, 0.370, 0.370, 0.370, 0.370, 0.370, 0.370, 0.370,
     1          0.331, 0.281, 0.210, 0.160, 0.089 /
C     Notes Travel path term for Volcanic region is not included.

C Find the requested spectral period and corresponding coefficients
      nPer = 14
C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1   = period(1)
         c11T       = c11(1)
         c12T       = c12(1)
         c13T       = c13(1)
         c15T       = c15(1)
         c17T       = c17(1)
         c18T       = c18(1)
         c19T      = c19(1)
         c20T      = c20(1)
         c24T      = c24(1)
         c29T      = c29(1)
         c30T      = c30(1)
         c43T      = c43(1)
         c46T      = c46(1)
         sigmaM6T  = sigmaM6(1)
         sigslopeT = sigslope(1)
         tauT      = tau(1)
         a9T       = a9(1)
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'McVerry et al. (2006) Subduction'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),c11(count1),c11(count2),
     +             specT,c11T,iflag)
      call S24_interp (period(count1),period(count2),c12(count1),c12(count2),
     +             specT,c12T,iflag)
      call S24_interp (period(count1),period(count2),c13(count1),c13(count2),
     +             specT,c13T,iflag)
      call S24_interp (period(count1),period(count2),c15(count1),c15(count2),
     +             specT,c15T,iflag)
      call S24_interp (period(count1),period(count2),c17(count1),c17(count2),
     +             specT,c17T,iflag)
      call S24_interp (period(count1),period(count2),c18(count1),c18(count2),
     +             specT,c18T,iflag)
      call S24_interp (period(count1),period(count2),c19(count1),c19(count2),
     +             specT,c19T,iflag)
      call S24_interp (period(count1),period(count2),c20(count1),c20(count2),
     +             specT,c20T,iflag)
      call S24_interp (period(count1),period(count2),c24(count1),c24(count2),
     +             specT,c24T,iflag)
      call S24_interp (period(count1),period(count2),c29(count1),c29(count2),
     +             specT,c29T,iflag)
      call S24_interp (period(count1),period(count2),c30(count1),c30(count2),
     +             specT,c30T,iflag)
      call S24_interp (period(count1),period(count2),c43(count1),c43(count2),
     +             specT,c43T,iflag)
      call S24_interp (period(count1),period(count2),c46(count1),c46(count2),
     +             specT,c46T,iflag)
      call S24_interp (period(count1),period(count2),sigmaM6(count1),sigmaM6(count2),
     +             specT,sigmaM6T,iflag)
      call S24_interp (period(count1),period(count2),sigslope(count1),sigslope(count2),
     +             specT,sigslopeT,iflag)
      call S24_interp (period(count1),period(count2),tau(count1),tau(count2),
     +             specT,tauT,iflag)
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +             specT,a9T,iflag)

 1011 period1 = specT

c     Set the fault mechanism term.
c     fType     Mechanism
C     ----------------------------------
c     0         Interface
c     1         Intraslab

      if (ftype .eq. 0.0) then
       SI = 1.0
      elseif (ftype .eq. 1.0) then
       SI = 0.0
      else
       write(*,*) 'McVerry Subduction only defined for ftype 0 and 1.'
       write(*,*)
       stop 99
      endif

c     Set the Centroid Depth
c     The centroid is assumed to be at the center of the rupture plane.
      Hc = depthtop + width * cos(dip/180*3.14159) / 2
c     OR the centroid depth is assumed to be the same as the hypocenter depth.
      Hc = hypodepth

c     Set the location of the intraslab earthquake
c     A rupture centroid depth of 50 km separates the deep from the shallow events.
      if (Hc . ge. 50.0) then
       DS = 1.0
      else
       DS = 0.0
      endif

c     Set distance through volcanic attenuation
      RVOL = 0.0*Rrup


C     Next compute the PGA unprimed for later scaling.
      c11up = 8.57343
      c12up = 1.41400
      c13up = 0.00000
      c15up = -2.55200
      c17up = -2.56592
      c18up = 1.78180
      c19up = 0.55400
      c20up = 0.01545
      c24up = -0.49963
      c29up = 0.27315
      c30up = -0.23000
      c43up = -0.33716
      c46up = -0.03255

      pgaupab = c11up + (c12up + (c15up - c17up) * c19up)*(M-6.0) +
     1          c13up * (10.0-M)**3 +
     2          c17up * alog (Rrup + c18up * exp(c19up * M)) +
     3          c20up * Hc + c24up * SI +
     4          c46up * RVOL * (1-DS)


      pgaup = pgaupab + c29up*Sc + (c30up*alog(exp(pgaupab)+0.03) + c43up)*Sd

C     Next compute the primed PGA value.

      pgapab = c11(1) + (c12(1) + (c15(1) - c17(1)) * c19(1))*(M-6.0) +
     1          c13(1) * (10.0-M)**3 +
     2          c17(1) * alog (Rrup + c18(1) * exp(c19(1) * M)) +
     3          c20(1) * Hc + c24(1) * SI +
     4          c46(1) * RVOL * (1-DS)

      pgap = pgapab + c29(1)*Sc + (c30(1)*alog(exp(pgapab)+0.03) + c43(1))*Sd

C     Now compute the ground motion for the given spectral period.

      lnYab = c11T + (c12T + (c15T - c17T) * c19T)*(M-6.0) +
     1          c13T * (10.0-M)**3 +
     2          c17T * alog (Rrup + c18T * exp(c19T * M)) +
     3          c20T * Hc + c24T * SI +
     4          c46T * RVOL * (1-DS)

      lnY = lnYab + c29T*Sc + (c30T*alog(exp(pgap)+0.03) + c43T)*Sd

C     Now make dataset adjustment.

      lnY = lnY + (pgaup - pgap)

C Compute the sigma term
      if (M .lt. 5.0) then
         sig = sigmaM6T - sigslopeT
      elseif (M .gt. 7.0) then
         sig = sigmaM6T + sigslopeT
      else
         sig = sigmaM6T + sigslopeT*(M-6.0)
      endif

      sigma = sqrt(tauT*tauT + sig*sig)

C     Convert ground motion to units of gals.
      lnY = lnY + 6.89
      period2 = period1

      return
      end


c----------------------------------------------------------------------
      subroutine S02_Bindi_Hor_2009 ( m, jbDist, specT,
     1                     period2, lnY, sigma, iflag, Sr, Ss, Sd )

      implicit none

      integer MAXPER
      parameter (MAXPER=23)
      REAL Period(MAXPER), a(MAXPER), b1(MAXPER), b2(MAXPER)
      REAL c1(MAXPER), c2(MAXPER), h(MAXPER), Bigc0(MAXPER)
      REAL Bigc1(MAXPER), Bigc2(MAXPER), sig(MAXPER)
      REAL aT, b1T, b2T, c1T, c2T, hT, Bigc0T, Bigc1T, Bigc2T, sigT
      REAL M, jbDist, specT, sigma
      REAL period2, lnY, Sr, Ss, Sd, period1

      integer iflag, count1, count2, nPer, i


      data Period / 0.0, 0.01, 0.03, 0.04, 0.07, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35,
     1           0.4, 0.45, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.25, 1.5, 1.75, 2.0 /
      data a / 3.7691, 3.7691, 3.8802, 3.8569, 4.005, 4.0176, 4.1, 4.0808,
     1         3.9805, 3.9016, 3.8185, 3.6578, 3.5972, 3.5304, 3.3531, 3.2126,
     2         3.098, 3.0472, 3.0311, 2.821, 2.8348, 2.861, 2.7506 /
      data b1 / 0.0523, 0.0523, 0.0086, 0.0395, 0.0479, 0.0619, 0.093, 0.0633,
     1          0.1333, 0.1224, 0.1167, 0.1583, 0.1656, 0.2035, 0.2456, 0.2754,
     2          0.2949, 0.35, 0.3555, 0.3621, 0.2498, 0.1834, 0.2056 /
      data b2 / -0.1389, -0.1389, -0.1287, -0.1255, -0.1232, -0.112, -0.133,
     1          -0.1358, -0.1418, -0.1407, -0.1366, -0.147, -0.1342, -0.132,
     2          -0.1181, -0.1209, -0.0963, -0.0952, -0.0962, -0.0963, -0.1103,
     3          -0.104, -0.1139 /
      data c1 / -1.9383, -1.9383, -1.972, -1.93, -1.9197, -1.8599, -1.8769,
     1          -1.8833, -1.8756, -1.8908, -1.8992, -1.8521, -1.8678, -1.8728,
     2          -1.8463, -1.8299, -1.8318, -1.8627, -1.9011, -1.878, -1.9787,
     3          -2.0899, -2.0976 /
      data c2 / 0.4661, 0.4661, 0.471, 0.4431, 0.4212, 0.3949, 0.4125, 0.4546,
     1          0.4318, 0.4551, 0.474, 0.4727, 0.4665, 0.4519, 0.4414, 0.4396,
     2          0.4255, 0.3992, 0.4036, 0.4151, 0.5216, 0.588, 0.5953 /
      data h / 10.1057, 10.1057, 10.594, 10.0362, 10.2414, 10.4222, 10.7824,
     1         10.5949, 10.2248, 9.7928, 9.4714, 9.269, 9.3437, 9.2842, 9.0307,
     2          8.8794, 8.7481, 9.1414, 9.6044, 9.5829, 9.9923, 10.8928, 10.5615 /
      data Bigc0 / 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     1             0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 /
      data Bigc1 / 0.226, 0.226, 0.2176, 0.2221, 0.2082, 0.2572, 0.2631, 0.2126,
     1            0.1618, 0.1409, 0.1289, 0.1146, 0.0946, 0.0763, 0.0539, 0.0447,
     2            0.0436, 0.04, 0.0347, 0.0233, -0.0006, -0.0002, -0.0065 /
      data Bigc2 / 0.1043, 0.1043, 0.0866, 0.0764, 0.039, 0.058, 0.0632, 0.1212,
     1             0.1454, 0.163, 0.1892, 0.219, 0.2632, 0.2741, 0.2973, 0.3217,
     2             0.3406, 0.3663, 0.3791, 0.4091, 0.4111, 0.4133, 0.3836 /
      data sig / 0.3523, 0.3523, 0.3521, 0.3648, 0.3649, 0.3734, 0.3832, 0.3924,
     1           0.3815, 0.375, 0.3842, 0.374, 0.3744, 0.3713, 0.3732, 0.3761,
     2           0.3756, 0.3784, 0.3907, 0.4119, 0.4059, 0.3987, 0.379 /


C Find the requested spectral period and corresponding coefficients
      nPer = 23
C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1  = period(1)
         aT       = a(1)
         b1T      = b1(1)
         b2T      = b2(1)
         c1T      = c1(1)
         c2T      = c2(1)
         hT       = h(1)
         Bigc0T   = Bigc0(1)
         Bigc1T   = Bigc1(1)
         BIgc2T   = Bigc2(1)
         sigT     = sig(1)
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'Bindi et al. (2009) Horizontal'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),a(count1),a(count2),
     +             specT,aT,iflag)
      call S24_interp (period(count1),period(count2),b1(count1),b1(count2),
     +             specT,b1T,iflag)
      call S24_interp (period(count1),period(count2),b2(count1),b2(count2),
     +             specT,b2T,iflag)
      call S24_interp (period(count1),period(count2),c1(count1),c1(count2),
     +             specT,c1T,iflag)
      call S24_interp (period(count1),period(count2),c2(count1),c2(count2),
     +             specT,c2T,iflag)
      call S24_interp (period(count1),period(count2),h(count1),h(count2),
     +             specT,hT,iflag)
      call S24_interp (period(count1),period(count2),Bigc0(count1),Bigc0(count2),
     +             specT,Bigc0T,iflag)
      call S24_interp (period(count1),period(count2),Bigc1(count1),Bigc1(count2),
     +             specT,Bigc1T,iflag)
      call S24_interp (period(count1),period(count2),Bigc2(count1),Bigc2(count2),
     +             specT,Bigc2T,iflag)
      call S24_interp (period(count1),period(count2),sig(count1),sig(count2),
     +             specT,sigT,iflag)

 1011 period1 = specT

C     Compute the ground motion for the given spectral period.

      lnY = aT + b1T*(M-4.5) + b2T*(M-4.5)**2.0 +
     1      (c1T+c2T*(M-4.5))*log10(sqrt(jbdist*jbdist+hT*hT)) +
     2      Bigc0T*Sr + Bigc1T*Ss + Bigc2T*Sd

C     Set the sigma value and convert from log10 to Ln units
      sigma = sigT*alog(10.0)

C     Convert ground motion to units of gals in natural log units.
      lnY = lnY*alog(10.0)
      period2 = period1

      return
      end

c----------------------------------------------------------------------
      subroutine S02_Bindi_Hor_2011 ( m, jbDist, ftype, specT,
     1                     period2, lnY, sigma, iflag, SCa, SCb, SCc, SCd, SCe, phiT, tauT )

      implicit none

      integer MAXPER
      parameter (MAXPER=23)
      REAL Period(MAXPER), e1(MAXPER), c1(MAXPER), c2(MAXPER), h(MAXPER)
      REAL c3(MAXPER), b1(MAXPER), b2(MAXPER), SCacoef(MAXPER), SCbcoef(MAXPER)
      REAL SCccoef(MAXPER), SCdcoef(MAXPER), SCecoef(MAXPER), f1(MAXPER), f2(MAXPER)
      REAL f3(MAXPER), f4(MAXPER), phi(MAXPER), tau(MAXPER), sig(MAXPER)
      real e1T, c1T, c2T, hT, c3T, b1T, b2T, SCacoefT, SCbcoefT, SCccoefT, SCdcoefT, SCecoefT
      real f1T, f2T, f3T, f4T, phiT, tauT, sigT
      real Rref, Mref, Mh, b3, termsof, termsite, R, period1

      REAL M, jbDist, specT, sigma
      REAL period2, lnY, SCa, SCb, SCc, SCd, SCe, ftype

      integer iflag, count1, count2, nPer, i

      data period  /  0.0, -1.0, 0.01, 0.04, 0.07, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35,
     1       0.4, 0.45, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.25, 1.5, 1.75, 2.0 /
      data e1  /  3.672, 2.305, 3.672, 3.725, 3.906, 3.796, 3.799, 3.75, 3.699, 3.753,
     1       3.6, 3.549, 3.55, 3.526, 3.561, 3.485, 3.325, 3.318, 3.264, 2.896, 2.675,
     2       2.584, 2.537  /
      data c1  /  -1.94, -1.517, -1.94, -1.976, -2.05, -1.794, -1.521, -1.379, -1.34,
     1      -1.414, -1.32, -1.262, -1.261, -1.181, -1.23, -1.172, -1.115, -1.137, -1.114,
     2      -0.986, -0.96, -1.006, -1.009  /
      data c2  /  0.413, 0.326, 0.413, 0.422, 0.446, 0.415, 0.32, 0.28, 0.254, 0.255, 0.253,
     1       0.233, 0.223, 0.184, 0.178, 0.154, 0.163, 0.154, 0.14, 0.173, 0.192, 0.205, 0.193 /
      data h  /  10.322, 7.879, 10.322, 9.445, 9.81, 9.5, 9.163, 8.502, 7.912, 8.215, 7.507,
     1       6.76, 6.775, 5.992, 6.382, 5.574, 4.998, 5.231, 5.002, 4.34, 4.117, 4.505, 4.373  /
      data c3  /  0.000134, 0, 0.000134, 0.00027, 0.000758, 0.00255, 0.00372, 0.00384, 0.00326,
     1       0.00219, 0.00232, 0.00219, 0.00176, 0.00186, 0.00114, 0.000942, 0.000909, 0.000483,
     2       0.000254, 0.000783, 0.000802, 0.000427, 0.000164  /
      data b1  /  -0.262, 0.236, -0.262, -0.315, -0.375, -0.29, -0.0987, 0.0094, 0.086,
     1       0.124, 0.154, 0.225, 0.292, 0.384, 0.436, 0.529, 0.545, 0.563, 0.599, 0.579,
     2       0.575, 0.574, 0.597  /
      data b2  /  -0.0707, -0.00686, -0.0707, -0.0787, -0.0773, -0.0615, -0.0574, -0.0517,
     1      -0.0457, -0.0435, -0.0437, -0.0406, -0.0306, -0.025, -0.0227, -0.0185, -0.0215,
     2      -0.0263, -0.027, -0.0336, -0.0353, -0.0371, -0.0367  /
      data sCAcoef  /  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0  /
      data sCBcoef  /  0.162, 0.205, 0.162, 0.161, 0.154, 0.178, 0.174, 0.156, 0.182, 0.201, 0.22,
     1       0.229, 0.226, 0.218, 0.219, 0.21, 0.21, 0.212, 0.221, 0.244, 0.251, 0.252, 0.245  /
      data sCCcoef  /  0.24, 0.269, 0.24, 0.24, 0.235, 0.247, 0.24, 0.234, 0.245, 0.244, 0.257,
     1       0.255, 0.271, 0.28, 0.296, 0.303, 0.304, 0.315, 0.332, 0.365, 0.375, 0.357, 0.352  /
      data sCDcoef  /  0.105, 0.321, 0.105, 0.06, 0.057, 0.037, 0.148, 0.115, 0.154, 0.213, 0.243,
     1       0.226, 0.237, 0.263, 0.355, 0.496, 0.621, 0.68, 0.707, 0.717, 0.667, 0.593, 0.54  /
      data sCEcoef  /  0.57, 0.428, 0.57, 0.614, 0.536, 0.599, 0.74, 0.556, 0.414, 0.301, 0.235,
     1       0.202, 0.181, 0.168, 0.142, 0.134, 0.15, 0.154, 0.152, 0.183, 0.203, 0.22, 0.226  /
      data f1  /  -0.0503, -0.0308, -0.0503, -0.0442, -0.0454, -0.0656, -0.0755, -0.0733,
     1      -0.0568, -0.0564, -0.0523, -0.0565, -0.0597, -0.0599, -0.0559, -0.0461, -0.0457,
     2      -0.0351, -0.0298, -0.0207, -0.014, 0.00154, 0.00512  /
      data f2  /  0.105, 0.0754, 0.105, 0.106, 0.103, 0.111, 0.123, 0.106, 0.11, 0.0877,
     1       0.0905, 0.0927, 0.0886, 0.085, 0.079, 0.0896, 0.0795, 0.0715, 0.066, 0.0614,
     2       0.0505, 0.037, 0.035  /
      data f3  /  -0.0544, -0.0446, -0.0544, -0.0615, -0.0576, -0.0451, -0.0477, -0.0328,
     1      -0.0534, -0.0313, -0.0382, -0.0363, -0.0289, -0.0252, -0.0231, -0.0435,
     2      -0.0338, -0.0364, -0.0362, -0.0407, -0.0365, -0.0385, -0.0401  /
      data f4  /  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0  /
      data Tau  /  0.172, 0.194, 0.172, 0.154, 0.152, 0.154, 0.179, 0.209, 0.212,
     1       0.218, 0.221, 0.21, 0.204, 0.203, 0.203, 0.212, 0.213, 0.214, 0.222,
     2       0.227, 0.218, 0.219, 0.211  /
      data Phi  /  0.29, 0.27, 0.29, 0.307, 0.324, 0.328, 0.318, 0.32, 0.308, 0.29,
     1       0.283, 0.279, 0.284, 0.283, 0.283, 0.283, 0.284, 0.286, 0.283, 0.29,
     2       0.303, 0.305, 0.308  /
      data sig  /  0.337, 0.332, 0.337, 0.343, 0.358, 0.363, 0.365, 0.382, 0.374,
     1       0.363, 0.359, 0.349, 0.35, 0.349, 0.348, 0.354, 0.355, 0.357, 0.36,
     2       0.368, 0.373, 0.376, 0.373  /



C Find the requested spectral period and corresponding coefficients
      nPer = 23
C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1  = period(1)
         e1T      = e1(1)
         c1T      = c1(1)
         c2T      = c2(1)
         hT       = h(1)
         c3T      = c3(1)
         b1T      = b1(1)
         b2T      = b2(1)
         ScAcoefT = SCacoef(1)
         ScBcoefT = SCbcoef(1)
         ScCcoefT = SCccoef(1)
         ScDcoefT = SCdcoef(1)
         ScEcoefT = SCecoef(1)
         f1T      = f1(1)
         f2T      = f2(1)
         f3T      = f3(1)
         f4T      = f4(1)
         phiT     = phi(1)
         tauT     = tau(1)
         sigT     = sig(1)
         goto 1011
C     PGV Case
      elseif (specT .eq. -1.0) then
         period1  = period(2)
         e1T      = e1(2)
         c1T      = c1(2)
         c2T      = c2(2)
         hT       = h(2)
         c3T      = c3(2)
         b1T      = b1(2)
         b2T      = b2(2)
         ScAcoefT = SCacoef(2)
         ScBcoefT = SCbcoef(2)
         ScCcoefT = SCccoef(2)
         ScDcoefT = SCdcoef(2)
         ScEcoefT = SCecoef(2)
         f1T      = f1(2)
         f2T      = f2(2)
         f3T      = f3(2)
         f4T      = f4(2)
         phiT     = phi(2)
         tauT     = tau(2)
         sigT     = sig(2)
        goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=3,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'Bindi et al. (2011) Horizontal'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),e1(count1),e1(count2),
     +             specT,e1T,iflag)
      call S24_interp (period(count1),period(count2),c1(count1),c1(count2),
     +             specT,c1T,iflag)
      call S24_interp (period(count1),period(count2),c2(count1),c2(count2),
     +             specT,c2T,iflag)
      call S24_interp (period(count1),period(count2),h(count1),h(count2),
     +             specT,hT,iflag)
      call S24_interp (period(count1),period(count2),c3(count1),c3(count2),
     +             specT,c3T,iflag)
      call S24_interp (period(count1),period(count2),b1(count1),b1(count2),
     +             specT,b1T,iflag)
      call S24_interp (period(count1),period(count2),b2(count1),b2(count2),
     +             specT,b2T,iflag)
      call S24_interp (period(count1),period(count2),SCacoef(count1),SCacoef(count2),
     +             specT,SCacoefT,iflag)
      call S24_interp (period(count1),period(count2),SCbcoef(count1),SCbcoef(count2),
     +             specT,SCbcoefT,iflag)
      call S24_interp (period(count1),period(count2),SCccoef(count1),SCccoef(count2),
     +             specT,SCccoefT,iflag)
      call S24_interp (period(count1),period(count2),SCdcoef(count1),SCdcoef(count2),
     +             specT,SCdcoefT,iflag)
      call S24_interp (period(count1),period(count2),SCecoef(count1),SCecoef(count2),
     +             specT,SCecoefT,iflag)
      call S24_interp (period(count1),period(count2),f1(count1),f1(count2),
     +             specT,f1T,iflag)
      call S24_interp (period(count1),period(count2),f2(count1),f2(count2),
     +             specT,f2T,iflag)
      call S24_interp (period(count1),period(count2),f3(count1),f3(count2),
     +             specT,f3T,iflag)
      call S24_interp (period(count1),period(count2),f4(count1),f4(count2),
     +             specT,f4T,iflag)
      call S24_interp (period(count1),period(count2),phi(count1),phi(count2),
     +             specT,phiT,iflag)
      call S24_interp (period(count1),period(count2),tau(count1),tau(count2),
     +             specT,tauT,iflag)
      call S24_interp (period(count1),period(count2),sig(count1),sig(count2),
     +             specT,sigT,iflag)

 1011 period1 = specT

C     Set Constant Terms
      Mref = 5.0
      Rref = 1.0
      Mh = 6.75
      b3 = 0.0

C     Set the mechanism term.
      if (ftype .eq. 0 ) then
         termsof = f3T
      elseif (ftype .ge. 0.5) then
         termsof = f2T
      elseif (ftype .le. -0.5) then
         termsof = f1T
      endif

C     Set the site term.
      termsite = SCA*scaCoefT + SCb*SCBcoefT + SCc*SCCcoefT + SCd*SCDcoefT + SCe*SCecoefT

      R = sqrt (jbdist**2 + hT**2)

C     Compute the ground motion for the given spectral period.
      if (M .le. Mh) then
         lnY = e1T + (c1T+c2T*(M-Mref))*log10(R/Rref) - c3T*(R-Rref) +
     1       b1T*(M-Mh) + b2T*(M-Mh)**2.0 + termsite + termsof
      else
         lnY = e1T + (c1T+c2T*(M-Mref))*log10(R/Rref) - c3T*(R-Rref) +
     1       b3*(M-Mh) + termsite + termsof
      endif
C     Set the sigma value and convert from log10 to Ln units
      phiT = phiT*alog(10.0)
      tauT = tauT*alog(10.0)
      sigma = sigT*alog(10.0)

C     Convert ground motion to units of gals in natural log units.
      lnY = lnY*alog(10.0)
      period2 = period1

      return
      end

c----------------------------------------------------------------------
      subroutine S02_Bindi_Hor_2013 ( m, jbDist, ftype, specT,
     1                     period2, lnY, sigma, iflag, vs, phiT, tauT )

      implicit none

      integer MAXPER
      parameter (MAXPER=26)
      REAL Period(MAXPER), e1(MAXPER), c1(MAXPER), c2(MAXPER), h(MAXPER)
      REAL c3(MAXPER), b1(MAXPER), b2(MAXPER), b3(MAXPER), gamma(MAXPER)
      REAL sofN(MAXPER), sofR(MAXPER), sofS(MAXPER), phi(MAXPER), tau(MAXPER), sig(MAXPER), sigs2s(MAXPER)
      real e1T, c1T, c2T, hT, c3T, b1T, b2T, b3T, gammaT, sofNT, sofRT, sofST, sigs2sT
      real phiT, tauT, sigT, period1
      real Rref, Mref, Mh, R, Vref, vs

      REAL M, jbDist, specT, sigma, termsof
      REAL period2, lnY, ftype

      integer iflag, count1, count2, nPer, i

      data period / 0.0, -1.0, 0.01, 0.02, 0.04, 0.07, 0.1, 0.15, 0.2, 0.26, 0.3,
     1              0.36, 0.4, 0.46, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.3, 1.5, 1.8,
     1              2.0, 2.6, 3.0  /
      data e1 / 3.32819, 2.26481, 3.37053, 3.37053, 3.43922, 3.59651, 3.68638, 3.68632,
     1          3.68262, 3.64314, 3.63985, 3.5748, 3.53006, 3.43387, 3.40554, 3.30442,
     1          3.23882, 3.1537, 3.13481, 3.12474, 2.89841, 2.84727, 2.68016, 2.60171,
     1          2.39067, 2.25399  /
      data c1 / -1.2398, -1.22408, -1.26358, -1.26358, -1.31025, -1.29051, -1.28178,
     1          -1.17697, -1.10301, -1.08527, -1.10591, -1.09955, -1.09538, -1.06586,
     1          -1.05767, -1.05014, -1.05021, -1.04654, -1.04612, -1.0527, -0.973828,
     1          -0.983388, -0.983082, -0.979215, -0.977532, -0.940373  /
      data c2 / 0.21732, 0.202085, 0.220527, 0.220527, 0.244676, 0.231878, 0.219406,
     1          0.182662, 0.133154, 0.115603, 0.108276, 0.103083, 0.101111, 0.109066,
     1          0.112197, 0.121734, 0.114674, 0.129522, 0.114536, 0.103471, 0.104898,
     1          0.109072, 0.164027, 0.163344, 0.211831, 0.227241  /
      data h / 5.26486, 5.06124, 5.20082, 5.20082, 4.91669, 5.35922, 6.12146, 5.74154,
     1         5.31998, 5.13455, 5.12846, 4.90557, 4.95386, 4.6599, 4.43205, 4.21657,
     1         4.17127, 4.20016, 4.48003, 4.41613, 4.25821, 4.56697, 4.68008, 4.58186,
     1         5.39517, 5.74173  /
      data c3 / 0.00118624, 0.0, 0.00111816, 0.00111816, 0.00109183, 0.00182094, 0.00211443,
     1          0.00254027, 0.00242089, 0.00196437, 0.00149922, 0.00104905, 0.000851474,
     1          0.000868165, 0.000788528, 0.000487285, 0.000159408, 0.0, 0.0, 0.0, 0.0,
     1          0.0, 0.0, 0.0, 0.0, 0.0  /
      data b1 / -0.0855045, 0.162802, -0.0890554, -0.0890554, -0.116919, -0.0850124,
     1          -0.11355, -0.0928726, 0.0100857, 0.0299397, 0.0391904, 0.052103, 0.0458464,
     1           0.0600838, 0.0883189, 0.120182, 0.166933, 0.193817, 0.247547, 0.306569,
     1           0.349119, 0.384546, 0.343663, 0.331747, 0.357514, 0.385526  /
      data b2 / -0.0925639, -0.0926324, -0.0916152, -0.0916152, -0.0783789, -0.0569968,
     1          -0.0753325, -0.102433, -0.105184, -0.127173, -0.138578, -0.151385, -0.16209,
     1          -0.165897, -0.164108, -0.163325, -0.161112, -0.156553, -0.153819, -0.147558,
     1          -0.149483, -0.139867, -0.135933, -0.148282, -0.122539, -0.111445  /
      data b3 / 0.0, 0.0440301, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0739042, 0.150461, 0.178899,
     1          0.189682, 0.216011, 0.224827, 0.197716, 0.15475, 0.117576, 0.112005,
     1          0.0517285, 0.0815754, 0.0928373, 0.108209, 0.0987372, 0.0, 0.0, 0.0, 0.0  /
      data gamma / -0.301899, -0.529443, -0.294021, -0.294021, -0.241765, -0.207629,
     1         -0.173237, -0.202492, -0.291228, -0.354425, -0.39306, -0.453905, -0.492063,
     1         -0.564463, -0.596196, -0.667824, -0.73839, -0.794076, -0.821699, -0.826584,
     1         -0.845047, -0.8232, -0.778657, -0.769243, -0.769609, -0.732072  /
      data sofN / -0.0397695, -0.00947675, -0.039236, -0.039236, -0.0377204, -0.0459437,
     1            -0.0380528, -0.0267293, -0.0326537, -0.0338438, -0.0372453, -0.0279067,
     1            -0.0256309, -0.0186635, -0.0174194, -0.000486417, 0.0112033, 0.0165258,
     1             0.0164493, 0.0263071, 0.0252339, 0.0186738, 0.0113713, 0.00553545,
     1             0.0087346, 0.0229893  /
      data sofR / 0.0775253, 0.0400574, 0.0810516, 0.0810516, 0.0797783, 0.0874968,
     1            0.0847103, 0.0678441, 0.0759769, 0.074982, 0.0767011, 0.0697898,
     1            0.0725668, 0.0645993, 0.0602826, 0.0449209, 0.0281506, 0.0203522,
     1            0.0212422, 0.0186043, 0.0223621, 0.0230894, 0.0166882, 0.0198566,
     1            0.0233142, -0.020662  /
      data sofS / -0.0377558, -0.0305805, -0.0418156, -0.0418156, -0.0420579, -0.041553,
     1            -0.0466585, -0.0411147, -0.0433232, -0.0411381, -0.0394559, -0.0418832,
     1            -0.046936, -0.0459358, -0.0428632, -0.0444345, -0.0393539, -0.0368783,
     1            -0.0376913, -0.0449111, -0.0475957, -0.041763, -0.0280594, -0.025392,
     1            -0.0320486, -0.00232715  /
      data tau / 0.149977, 0.156062, 0.15867, 0.15867, 0.154621, 0.172785, 0.169691,
     1           0.152902, 0.150055, 0.151209, 0.157946, 0.165436, 0.157728, 0.173005,
     1           0.18082, 0.182233, 0.189396, 0.189074, 0.191986, 0.195026, 0.181782,
     1           0.177752, 0.163242, 0.164958, 0.17028, 0.176546  /
      data phi / 0.282398, 0.277714, 0.282356, 0.282356, 0.291143, 0.291499, 0.301967,
     1           0.305804, 0.300109, 0.302419, 0.297402, 0.294395, 0.296992, 0.291868,
     1           0.289957, 0.292223, 0.289307, 0.288815, 0.293264, 0.297907, 0.306676,
     1           0.316312, 0.326484, 0.329916, 0.320626, 0.314165 /
      data sigs2s / 0.165611, 0.120398, 0.183959, 0.183959, 0.187409, 0.199913, 0.208178,
     1           0.212124, 0.190469, 0.187037, 0.174118, 0.175848, 0.169883, 0.164162,
     1           0.16509, 0.175634, 0.168617, 0.16817, 0.183719, 0.200775, 0.209625,
     1           0.218569, 0.221367, 0.22535, 0.210193, 0.207247  /
      data sig / 0.319753, 0.31856, 0.323885, 0.323885, 0.329654, 0.33886, 0.346379,
     1             0.3419, 0.335532, 0.338114, 0.336741, 0.337694, 0.336278, 0.33929,
     1             0.341717, 0.344388, 0.345788, 0.3452, 0.350517, 0.356067, 0.356504,
     1             0.362835, 0.36502, 0.368857, 0.363037, 0.360373  /

C Find the requested spectral period and corresponding coefficients
      nPer = 26
C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1  = period(1)
         e1T      = e1(1)
         c1T      = c1(1)
         c2T      = c2(1)
         hT       = h(1)
         c3T      = c3(1)
         b1T      = b1(1)
         b2T      = b2(1)
         b3T      = b3(1)
         gammaT   = gamma(1)
         sofNT    = sofN(1)
         sofRT    = sofR(1)
         sofST    = sofS(1)
         sigs2sT  = sigs2s(1)
         phiT     = phi(1)
         tauT     = tau(1)
         sigT     = sig(1)
         goto 1011
C     PGV Case
      elseif (specT .eq. -1.0) then
         period1  = period(2)
         e1T      = e1(2)
         c1T      = c1(2)
         c2T      = c2(2)
         hT       = h(2)
         c3T      = c3(2)
         b1T      = b1(2)
         b2T      = b2(2)
         b3T      = b3(2)
         gammaT   = gamma(2)
         sofNT    = sofN(2)
         sofRT    = sofR(2)
         sofST    = sofS(2)
         sigs2sT  = sigs2s(2)
         phiT     = phi(2)
         tauT     = tau(2)
         sigT     = sig(2)
        goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=3,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'Bindi et al. (2013) Horizontal'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),e1(count1),e1(count2),
     +             specT,e1T,iflag)
      call S24_interp (period(count1),period(count2),c1(count1),c1(count2),
     +             specT,c1T,iflag)
      call S24_interp (period(count1),period(count2),c2(count1),c2(count2),
     +             specT,c2T,iflag)
      call S24_interp (period(count1),period(count2),h(count1),h(count2),
     +             specT,hT,iflag)
      call S24_interp (period(count1),period(count2),c3(count1),c3(count2),
     +             specT,c3T,iflag)
      call S24_interp (period(count1),period(count2),b1(count1),b1(count2),
     +             specT,b1T,iflag)
      call S24_interp (period(count1),period(count2),b2(count1),b2(count2),
     +             specT,b2T,iflag)
      call S24_interp (period(count1),period(count2),b3(count1),b3(count2),
     +             specT,b3T,iflag)
      call S24_interp (period(count1),period(count2),gamma(count1),gamma(count2),
     +             specT,gammaT,iflag)
      call S24_interp (period(count1),period(count2),sofN(count1),sofN(count2),
     +             specT,sofNT,iflag)
      call S24_interp (period(count1),period(count2),sofR(count1),sofR(count2),
     +             specT,sofRT,iflag)
      call S24_interp (period(count1),period(count2),sofS(count1),sofS(count2),
     +             specT,sofST,iflag)
      call S24_interp (period(count1),period(count2),sigs2s(count1),sigs2s(count2),
     +             specT,sigs2sT,iflag)
      call S24_interp (period(count1),period(count2),phi(count1),phi(count2),
     +             specT,phiT,iflag)
      call S24_interp (period(count1),period(count2),tau(count1),tau(count2),
     +             specT,tauT,iflag)
      call S24_interp (period(count1),period(count2),sig(count1),sig(count2),
     +             specT,sigT,iflag)

 1011 period1 = specT

C     Set Constant Terms
      Mref = 5.5
      Rref = 1.0
      Mh = 6.75
      Vref = 800.0

C     Set the mechanism term.
      if (ftype .eq. 0 ) then
         termsof = sofST
      elseif (ftype .ge. 0.5) then
         termsof = sofRT
      elseif (ftype .le. -0.5) then
         termsof = sofNT
      endif

      R = sqrt (jbdist**2 + hT**2)

C     Compute the ground motion for the given spectral period.
      if (M .le. Mh) then
         lnY = e1T + (c1T+c2T*(M-Mref))*log10(R/Rref) - c3T*(R-Rref) +
     1       b1T*(M-Mh) + b2T*(M-Mh)**2.0 + gammaT*log10(vs/vref) + termsof
      else
         lnY = e1T + (c1T+c2T*(M-Mref))*log10(R/Rref) - c3T*(R-Rref) +
     1       b3T*(M-Mh) + gammaT*log10(vs/vref) + termsof
      endif

C     Set the sigma value and convert from log10 to Ln units
      phiT = phiT*alog(10.0)
      tauT = tauT*alog(10.0)
      sigma = sigT*alog(10.0)
      sigs2sT = sigs2sT*alog(10.0)

C     Convert ground motion to units of gals in natural log units.
      lnY = lnY*alog(10.0)
      period2 = period1

      return
      end

c----------------------------------------------------------------------
      subroutine S02_GK_Nov2012 ( m, Rrup, specT, ftype,
     1                     period2, lnY, sigma, iflag, vs, q0, depthvs15 )

      implicit none

      integer MAXPER
      parameter (MAXPER=36)
      REAL Period(MAXPER), sig(MAXPER)
      REAL period1, sigT
      REAL M, Rrup, specT, sigma, q0, depthvs15
      REAL period2, lnY, ftype, Vs
      REAL c1, c2, c3, c4, c5, c6, c7, c8, c9, bv, Va, R1, c11, D1, sigpga
      REAL A, Y1, Frv, R0, D0, abdepth, slope, Y
      REAL e1, e2, e3, e4, a1, a2, a3, dsp, t1, t2, t3, t4, s1, s2, s3
      REAL mu, amp, si, tspo, Abdist, x0, x1
      REAL SA, SA1, SA2, Pern, temp1, temp2

      integer iflag, count1, count2, nPer, i

      data Period / 0.0, 0.01, 0.02, 0.03, 0.04, 0.06, 0.08, 0.1, 0.12, 0.14, 0.16,
     1              0.18, 0.2, 0.22, 0.24, 0.27, 0.3, 0.33, 0.36, 0.4, 0.46, 0.5,
     2              0.6, 0.75, 0.85, 1.0, 1.5, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0 /
      data Sig / 0.5305557, 0.5305557, 0.533813492, 0.535719178, 0.537071284,
     1           0.53897697, 0.540329075, 0.54137785, 0.542234761, 0.548284191,
     2           0.554920701, 0.560774518, 0.566010936, 0.570747852, 0.575072317,
     3           0.580926134, 0.586162552, 0.590899468, 0.595223933, 0.600460351,
     4           0.607406519, 0.611550585, 0.620611966, 0.631702201, 0.637922809,
     5           0.646, 0.666151616, 0.680449415, 0.700601031, 0.71489883, 0.725989064,
     6           0.735050446, 0.742711734, 0.749348245, 0.755202061, 0.760438479 /

C Find the requested spectral period and corresponding coefficients
      nPer = 36
C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1  = period(1)
         sigT     = sig(1)
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'Graizer and Kalkan et al. (Nov. 2011) Horizontal'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),sig(count1),sig(count2),
     +             specT,sigT,iflag)

 1011 period1 = specT

C     Frist compute the PGA ground motion value.

C*********Coefficients file, July_2006 ***************
C     Revised values from email from V. Grazier (4/18/13)
         c1 =  0.140
         c2 = -6.250
         c3 =  0.370
         c4 =  2.237
         c5 = -7.542
         c6 = -0.125
         c7 =  1.190
         c8 = -6.150
c04/08/13         c9 =  0.525
         c9 =  0.6
         bv = -0.240
         Va = 484.5
         R1 = 100.0
         c11= 0.345
         D1 = 0.65
         sigpga = 0.550

C     Set Mech. term
C     Strike-slip and Normal are equal, Frv=1.0
C     Reverse, Frv=1.28
C     Oblique/Reverse, Frv=1.14
      IF (Ftype .eq. 0.5) THEN
         Frv = 1.14
      ELSEIF (Ftype .eq. 1.0) THEN
         Frv = 1.28
      ELSE
         Frv = 1.00
      ENDIF

      A = (c1*atan(M+c2)+c3)*Frv
      A = A/1.12
      Y1 = alog(A)

c********* Distance R0 Factor ********************
ccccc    Corner distance R0 depends upon magnitude
      R0 = c4 * M + c5

c********* Damping D0 Factor **************************
ccccc    Damping is magnitude dependent
      D0 = c6 * cos(c7*(M + c8)) + c9

c********* Basin Effect *******************************
c-------  Depth dependance ----------------------------
      Abdepth = 1.4 / sqrt((1-(1.5/(depthvs15+0.1))**2)**2
     1           + 1.96*(1.5/(depthvs15+0.1))**2)

c-------  Distance dependance -------------------------
      Abdist = 1. / sqrt((1-(40./(Rrup+0.1))**2)**2
     1           + 1.96*(40./(Rrup+0.1))**2)

c-------  Slope of SA at long-periods ----------------
      Slope = 1.763 - 0.25 * atan(1.4*(depthvs15-1.))

c*************** Calculating non-basin PGA *****************
c------- Vs30 site effect --------------------------
         Y1 = Y1 + bv * (alog(Vs/Va))

c-------- Core Filter and Anelastic ----------------
         x0 = Rrup/R0
         x1 = sqrt(rRup/R1)

         Y = Y1 - 0.5 * alog((1-x0)**2 + 4*D0**2*x0)
     1       - (c11 * rRup/Q0)

c------SA calculations ---------------------------------------
C     Revised values from email from V. Grazier (4/18/13)
          e1=-0.0012
c04/18/13          e2=-0.4085
          e2=-0.38
          e3= 0.0006
          e4= 3.9
cnjg          e4= 3.63
          a1= 0.01686
          a2= 1.2695
          a3= 0.0001
          Dsp=0.75
cnjg          t1= 0.0022
          t1= 0.001
cnjg          t2= 0.63
c04/18/13          t2= 0.60
          t2= 0.59
          t3=-0.0005
cnjg          t4=-2.65
c04/08/13          t4=-2.1
          t4=-2.3
          s1=0.00
          s2=0.077
          s3=0.3251

          Mu = e1*rRup+e2*M+e3*Vs+e4

          Amp = (a1*M+a2)*exp(a3*rRup)

          Si = s1*rRup-(s2*M+s3)

cnjg          Tspo = t1*rRup+t2*M+t3*Vs+t4
          Tspo = max(0.3,abs(t1*rRup+t2*M+t3*Vs+t4))


          Pern = (specT/Tspo)**Slope

          temp1 = (alog(specT)+Mu)/Si
          temp2 = temp1**2

          SA1 = Amp*exp(-0.5*temp2)
          SA2 = 1./sqrt((1-Pern)**2 + 4.*Dsp**2
     1               *Pern)

          SA = SA1 + SA2
          SA1 = SA*exp(Y)
     1              * (1.+ABdist*ABdepth/1.3)

C     Set the sigma value
      sigma = sigT

C     Convert ground motion to units of gals
      lnY = alog(SA1) + 6.89
      period2 = period1

      return
      end


c----------------------------------------------------------------------
      subroutine S02_DCPP_CommonASK ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=5)
      REAL Period(MAXPER), a0(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), a7(MAXPER), a8(MAXPER), a9(MAXPER)
      real Mref1, MRef2, Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a0T, a1T, a2T, a3T, a4T, a5T, a6T
      REAL a7T, a8T, a9T
      integer iflag, count1, count2, nPer, i

      data period / 0.01, 0.2, 0.5, 1.0, 2.0 /
      data a0 / 3.228898132, 4.140227065, 3.497359438, 2.932255747, 2.214950878  /
      data a1 / -0.517312078, -0.516978997, -0.516201808, -0.515202564, -0.514580812  /
      data a2 / 0.806396741, 0.903630388, 1.130508896, 1.422209834, 1.603712641  /
      data a3 / -0.623915278, -0.623578743, -0.622793495, -0.621783888, -0.621155689  /
      data a4 / 0.087051423, 0.101941601, 0.136685351, 0.181355886, 0.209150886  /
      data a5 / -0.892881506, -0.892888426, -0.892904571, -0.892925328, -0.892938242  /
      data a6 / 0.300392371, 0.300392601, 0.300393136, 0.300393824, 0.300394253  /
      data a7 / 4.735246955, 4.735308943, 4.735453571, 4.735639501, 4.735755179  /
      data a8 / -0.006747572, -0.008147393, -0.002746975, -0.002046439, -0.001446105  /
      data a9 / 1.099685561, 1.099686305, 0.839688042, 0.569690276, 0.309691666  /

C Find the requested spectral period and corresponding coefficients
      nPer = 5
C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1  = period(1)
         a0T      = a0(1)
         a1T      = a1(1)
         a2T      = a2(1)
         a3T      = a3(1)
         a4T      = a4(1)
         a5T      = a5(1)
         a6T      = a6(1)
         a7T      = a7(1)
         a8T      = a8(1)
         a9T      = a9(1)
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'DCPP Common Model ASK Horizontal'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),a0(count1),a0(count2),
     +             specT,a0T,iflag)
      call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +             specT,a1T,iflag)
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +             specT,a2T,iflag)
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +             specT,a3T,iflag)
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +             specT,a4T,iflag)
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +             specT,a5T,iflag)
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +             specT,a6T,iflag)
      call S24_interp (period(count1),period(count2),a7(count1),a7(count2),
     +             specT,a7T,iflag)
      call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +             specT,a8T,iflag)
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +             specT,a9T,iflag)

 1011 period1 = specT

C     Set Constant Terms
      Mref1 = 5.5
      Mref2 = 6.5

C     Compute the ground motion for the given spectral period.
      if (M .lt. Mref1) then
         lnY = a0T - a4T*(8.5-Mref1)**2.0 + a2T*(M-Mref1) + a1T*(Mref1-Mref2) + a8T*Rrup
     1             + (a5T+a6T*(Mref1-Mref2))*alog(sqrt(a7T*a7T+Rrup*Rrup))
      elseif (M .ge. Mref2) then
         lnY = a0T - a4T*(8.5-M)**2.0     + a3T*(M-Mref2)                     + a8T*Rrup
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rrup*Rrup))
      else
         lnY = a0T - a4T*(8.5-M)**2.0     + a1T*(M-Mref2)                     + a8T*Rrup
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rrup*Rrup))
      endif

C     Add Ztor term
      lnY = lnY + (a9T*ztor)/20.0

      sigma = 0.65

C     Convert ground motion to units of gals in natural log units.
      lnY = lnY + 4.6052

      period2 = period1

      return
      end

c----------------------------------------------------------------------
      subroutine S02_DCPP_CommonBSSA ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=5)
      REAL Period(MAXPER), a0(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), a7(MAXPER), a8(MAXPER), a9(MAXPER)
      real Mref1, MRef2, Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a0T, a1T, a2T, a3T, a4T, a5T
      REAL a6T, a7T, a8T, a9T
      integer iflag, count1, count2, nPer, i

      data period / 0.01, 0.2, 0.5, 1.0, 2.0 /
      data a0 / 3.413425074, 4.849580469, 4.390103785, 3.942432001, 3.024220488  /
      data a1 / -0.551112061, -0.621744043, 0.206539432, 0.506563439, 0.861514793  /
      data a2 / 1.936833928, 1.988360039, 2.024580267, 2.386046162, 2.643583984  /
      data a3 / -0.506733658, -0.86125683, -0.498585794, -0.388363, -0.10725144  /
      data a4 / 3.57565E-20, 0.11961464, 0.060269709, 0.072411094, 0.073811638  /
      data a5 / -1.079527243, -1.088092907, -1.230023896, -1.316150631, -1.332304596  /
      data a6 / 0.310213127, 0.251426485, 0.21890305, 0.197164553, 0.183088381  /
      data a7 / 5.896688573, 5.941120331, 6.4671715, 6.758038317, 7.438969466  /
      data a8 / -0.000684999, -0.000539904, 0.00403903, 0.006083088, 0.006726857  /
      data a9 / 0.475928436, 0.460260633, 0.452444602, 0.450795989, 0.416434239  /

C Find the requested spectral period and corresponding coefficients
      nPer = 5
C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1  = period(1)
         a0T      = a0(1)
         a1T      = a1(1)
         a2T      = a2(1)
         a3T      = a3(1)
         a4T      = a4(1)
         a5T      = a5(1)
         a6T      = a6(1)
         a7T      = a7(1)
         a8T      = a8(1)
         a9T      = a9(1)
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'DCPP Common Model BSSA Horizontal'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),a0(count1),a0(count2),
     +             specT,a0T,iflag)
      call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +             specT,a1T,iflag)
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +             specT,a2T,iflag)
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +             specT,a3T,iflag)
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +             specT,a4T,iflag)
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +             specT,a5T,iflag)
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +             specT,a6T,iflag)
      call S24_interp (period(count1),period(count2),a7(count1),a7(count2),
     +             specT,a7T,iflag)
      call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +             specT,a8T,iflag)
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +             specT,a9T,iflag)

 1011 period1 = specT

C     Set Constant Terms
      Mref1 = 5.5
      Mref2 = 6.5

C     Compute the ground motion for the given spectral period.
      if (M .lt. Mref1) then
         lnY = a0T - a4T*(8.5-Mref1)**2.0 + a2T*(M-Mref1) + a1T*(Mref1-Mref2) + a8T*Rrup
     1             + (a5T+a6T*(Mref1-Mref2))*alog(sqrt(a7T*a7T+Rrup*Rrup))
      elseif (M .ge. Mref2) then
         lnY = a0T - a4T*(8.5-M)**2.0     + a3T*(M-Mref2)                     + a8T*Rrup
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rrup*Rrup))
      else
         lnY = a0T - a4T*(8.5-M)**2.0     + a1T*(M-Mref2)                     + a8T*Rrup
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rrup*Rrup))
      endif

C     Add Ztor term
      lnY = lnY + (a9T*ztor)/20.0

      sigma = 0.65

C     Convert ground motion to units of gals in natural log units.
      lnY = lnY + 4.6052

      period2 = period1

      return
      end


c----------------------------------------------------------------------
      subroutine S02_DCPP_Common001 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=5)
      REAL Period(MAXPER), a0(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), a7(MAXPER), a8(MAXPER), a9(MAXPER)
      real Mref1, MRef2, Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a0T, a1T, a2T, a3T, a4T, a5T, a6T
      REAL a7T, a8T, a9T
      integer iflag, count1, count2, nPer, i

      data period / 0.01, 0.2, 0.5, 1.0, 2.0 /
      data a0 / 4.153292577, 5.56843321, 4.499092916, 3.580962972, 2.483313462  /
      data a1 / -0.361676637, -0.315761961, -0.012742999, 0.35534841, 1.010158534  /
      data a2 / 1.900083399, 2.02460267, 1.890857041, 2.074235975, 2.241856312  /
      data a3 / -0.621300689, -0.791785486, -0.739262879, -0.569065001, -0.098960478  /
      data a4 / 0.027852946, 0.094607879, 0.099607707, 0.076998392, 0.011698198  /
      data a5 / -1.23462311, -1.334919221, -1.178173331, -1.132159433, -1.149259737  /
      data a6 / 0.26958419, 0.158147559, 0.218503106, 0.191236836, 0.181219265  /
      data a7 / 7.448181391, 8.773509011, 7.889588322, 7.044472113, 8.232700489  /
      data a8 / 0.004859145, 0.017747568, 0.008277642, -0.012910642, -0.00721013  /
      data a9 / 1.052689681, 0.887240265, 0.507748683, 0.351512826, 0.051133028  /

C Find the requested spectral period and corresponding coefficients
      nPer = 5
C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1  = period(1)
         a0T      = a0(1)
         a1T      = a1(1)
         a2T      = a2(1)
         a3T      = a3(1)
         a4T      = a4(1)
         a5T      = a5(1)
         a6T      = a6(1)
         a7T      = a7(1)
         a8T      = a8(1)
         a9T      = a9(1)
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'DCPP Common Model001 Horizontal'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),a0(count1),a0(count2),
     +             specT,a0T,iflag)
      call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +             specT,a1T,iflag)
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +             specT,a2T,iflag)
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +             specT,a3T,iflag)
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +             specT,a4T,iflag)
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +             specT,a5T,iflag)
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +             specT,a6T,iflag)
      call S24_interp (period(count1),period(count2),a7(count1),a7(count2),
     +             specT,a7T,iflag)
      call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +             specT,a8T,iflag)
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +             specT,a9T,iflag)

 1011 period1 = specT

C     Set Constant Terms
      Mref1 = 5.5
      Mref2 = 6.5

C     Compute the ground motion for the given spectral period.
      if (M .lt. Mref1) then
         lnY = a0T - a4T*(8.5-Mref1)**2.0 + a2T*(M-Mref1) + a1T*(Mref1-Mref2) + a8T*Rrup
     1             + (a5T+a6T*(Mref1-Mref2))*alog(sqrt(a7T*a7T+Rrup*Rrup))
      elseif (M .ge. Mref2) then
         lnY = a0T - a4T*(8.5-M)**2.0     + a3T*(M-Mref2)                     + a8T*Rrup
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rrup*Rrup))
      else
         lnY = a0T - a4T*(8.5-M)**2.0     + a1T*(M-Mref2)                     + a8T*Rrup
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rrup*Rrup))
      endif

C     Add Ztor term
      lnY = lnY + (a9T*ztor)/20.0

      sigma = 0.65

C     Convert ground motion to units of gals in natural log units.
      lnY = lnY + 4.6052

      period2 = period1

      return
      end

c----------------------------------------------------------------------
      subroutine S02_DCPP_Common002 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=5)
      REAL Period(MAXPER), a0(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), a7(MAXPER), a8(MAXPER), a9(MAXPER)
      real Mref1, MRef2, Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a0T, a1T, a2T, a3T, a4T, a5T, a6T
      REAL a7T, a8T, a9T
      integer iflag, count1, count2, nPer, i

      data period / 0.01, 0.2, 0.5, 1.0, 2.0 /
      data a0 / 3.766432545, 4.850577559, 3.800480116, 2.899595843, 1.970732776  /
      data a1 / -0.171377798, -0.040258384, 0.065134369, 0.309695977, 0.716925763  /
      data a2 / 1.351297213, 1.497600289, 1.758946183, 1.996206128, 2.195655212  /
      data a3 / -0.497645143, -0.424851869, -0.351900164, -0.260247854, -0.058024023  /
      data a4 / 0.020942219, 0.045308538, 0.005369492, 0.00479552, -0.00048382  /
      data a5 / -1.169973398, -1.223878876, -1.083289517, -1.039042926, -1.042122377  /
      data a6 / 0.316104509, 0.246186297, 0.275665105, 0.265332113, 0.284686743  /
      data a7 / 6.732787449, 7.623418904, 6.722698608, 5.970442035, 6.614965298  /
      data a8 / 0.012477431, 0.004627569, 0.01006225, 0.000274254, 0.019123099  /
      data a9 / 1.377033057, 1.22846788, 0.757969632, 0.498036735, 0.195263348  /

C Find the requested spectral period and corresponding coefficients
      nPer = 5
C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1  = period(1)
         a0T      = a0(1)
         a1T      = a1(1)
         a2T      = a2(1)
         a3T      = a3(1)
         a4T      = a4(1)
         a5T      = a5(1)
         a6T      = a6(1)
         a7T      = a7(1)
         a8T      = a8(1)
         a9T      = a9(1)
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'DCPP Common Model002 Horizontal'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),a0(count1),a0(count2),
     +             specT,a0T,iflag)
      call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +             specT,a1T,iflag)
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +             specT,a2T,iflag)
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +             specT,a3T,iflag)
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +             specT,a4T,iflag)
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +             specT,a5T,iflag)
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +             specT,a6T,iflag)
      call S24_interp (period(count1),period(count2),a7(count1),a7(count2),
     +             specT,a7T,iflag)
      call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +             specT,a8T,iflag)
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +             specT,a9T,iflag)

 1011 period1 = specT

C     Set Constant Terms
      Mref1 = 5.5
      Mref2 = 6.5

C     Compute the ground motion for the given spectral period.
      if (M .lt. Mref1) then
         lnY = a0T - a4T*(8.5-Mref1)**2.0 + a2T*(M-Mref1) + a1T*(Mref1-Mref2) + a8T*Rrup
     1             + (a5T+a6T*(Mref1-Mref2))*alog(sqrt(a7T*a7T+Rrup*Rrup))
      elseif (M .ge. Mref2) then
         lnY = a0T - a4T*(8.5-M)**2.0     + a3T*(M-Mref2)                     + a8T*Rrup
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rrup*Rrup))
      else
         lnY = a0T - a4T*(8.5-M)**2.0     + a1T*(M-Mref2)                     + a8T*Rrup
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rrup*Rrup))
      endif

C     Add Ztor term
      lnY = lnY + (a9T*ztor)/20.0

      sigma = 0.65

C     Convert ground motion to units of gals in natural log units.
      lnY = lnY + 4.6052

      period2 = period1

      return
      end

c----------------------------------------------------------------------
      subroutine S02_DCPP_Common003 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=5)
      REAL Period(MAXPER), a0(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), a7(MAXPER), a8(MAXPER), a9(MAXPER)
      real Mref1, MRef2, Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a0T, a1T, a2T, a3T, a4T, a5T, a6T
      REAL a7T, a8T, a9T
      integer iflag, count1, count2, nPer, i

      data period / 0.01, 0.2, 0.5, 1.0, 2.0 /
      data a0 / 4.26700257, 5.095267482, 5.118384786, 5.211651552, 4.753316415  /
      data a1 / -1.704621664, -1.521651883, -1.124676082, -1.008136391, -1.066098231  /
      data a2 / 1.667127313, 1.678459199, 2.142341158, 2.782649024, 3.084140079  /
      data a3 / -1.443434574, -1.316295438, -0.979791521, -1.025838031, -1.163722387  /
      data a4 / 0.314465134, 0.279948033, 0.308303208, 0.37704864, 0.468674542  /
      data a5 / -0.959040945, -0.928840584, -1.181350565, -1.346345473, -1.386584498  /
      data a6 / 0.313193998, 0.31169418, 0.225880131, 0.177047004, 0.177375041  /
      data a7 / 4.678293537, 4.795274716, 4.279893539, 3.987288865, 3.994487484  /
      data a8 / 0.002216998, 0.019973522, -0.002746898, 0.016238172, 0.011075769  /
      data a9 / 0.900833334, 0.843155253, 0.670101169, 0.60887657, 0.563006729  /

C Find the requested spectral period and corresponding coefficients
      nPer = 5
C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1  = period(1)
         a0T      = a0(1)
         a1T      = a1(1)
         a2T      = a2(1)
         a3T      = a3(1)
         a4T      = a4(1)
         a5T      = a5(1)
         a6T      = a6(1)
         a7T      = a7(1)
         a8T      = a8(1)
         a9T      = a9(1)
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'DCPP Common Model003 Horizontal'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),a0(count1),a0(count2),
     +             specT,a0T,iflag)
      call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +             specT,a1T,iflag)
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +             specT,a2T,iflag)
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +             specT,a3T,iflag)
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +             specT,a4T,iflag)
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +             specT,a5T,iflag)
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +             specT,a6T,iflag)
      call S24_interp (period(count1),period(count2),a7(count1),a7(count2),
     +             specT,a7T,iflag)
      call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +             specT,a8T,iflag)
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +             specT,a9T,iflag)

 1011 period1 = specT

C     Set Constant Terms
      Mref1 = 5.5
      Mref2 = 6.5

C     Compute the ground motion for the given spectral period.
      if (M .lt. Mref1) then
         lnY = a0T - a4T*(8.5-Mref1)**2.0 + a2T*(M-Mref1) + a1T*(Mref1-Mref2) + a8T*Rrup
     1             + (a5T+a6T*(Mref1-Mref2))*alog(sqrt(a7T*a7T+Rrup*Rrup))
      elseif (M .ge. Mref2) then
         lnY = a0T - a4T*(8.5-M)**2.0     + a3T*(M-Mref2)                     + a8T*Rrup
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rrup*Rrup))
      else
         lnY = a0T - a4T*(8.5-M)**2.0     + a1T*(M-Mref2)                     + a8T*Rrup
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rrup*Rrup))
      endif

C     Add Ztor term
      lnY = lnY + (a9T*ztor)/20.0

      sigma = 0.65

C     Convert ground motion to units of gals in natural log units.
      lnY = lnY + 4.6052

      period2 = period1

      return
      end

c----------------------------------------------------------------------
      subroutine S02_DCPP_Common004 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=5)
      REAL Period(MAXPER), a0(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), a7(MAXPER), a8(MAXPER), a9(MAXPER)
      real Mref1, MRef2, Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a0T, a1T, a2T, a3T, a4T, a5T, a6T
      REAL a7T, a8T, a9T
      integer iflag, count1, count2, nPer, i

      data period / 0.01, 0.2, 0.5, 1.0, 2.0 /
      data a0 / 3.870448951, 4.887688516, 4.514455374, 3.759666642, 2.965961472  /
      data a1 / -0.51925764, -0.566344728, -0.598851191, -0.47011131, -0.380220056  /
      data a2 / 1.049735239, 1.176785404, 1.692430218, 1.998332708, 2.230755309  /
      data a3 / -0.56846325, -0.688829892, -0.761680651, -0.782804044, -0.842190919  /
      data a4 / 0.100423374, 0.119647883, 0.150583433, 0.175749332, 0.244812228  /
      data a5 / -1.144844546, -1.117581276, -1.135977587, -1.112790925, -1.12415614  /
      data a6 / 0.285692481, 0.243559626, 0.213316478, 0.200452585, 0.196677723  /
      data a7 / 6.479603934, 6.443720517, 6.325266942, 6.302213482, 6.465155083  /
      data a8 / -0.000334545, -0.006108207, -0.00544564, -0.001299281, 0.013815162  /
      data a9 / 0.754145633, 0.673525509, 0.51831709, 0.441346775, 0.394579793  /

C Find the requested spectral period and corresponding coefficients
      nPer = 5
C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1  = period(1)
         a0T      = a0(1)
         a1T      = a1(1)
         a2T      = a2(1)
         a3T      = a3(1)
         a4T      = a4(1)
         a5T      = a5(1)
         a6T      = a6(1)
         a7T      = a7(1)
         a8T      = a8(1)
         a9T      = a9(1)
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'DCPP Common Model004 Horizontal'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),a0(count1),a0(count2),
     +             specT,a0T,iflag)
      call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +             specT,a1T,iflag)
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +             specT,a2T,iflag)
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +             specT,a3T,iflag)
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +             specT,a4T,iflag)
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +             specT,a5T,iflag)
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +             specT,a6T,iflag)
      call S24_interp (period(count1),period(count2),a7(count1),a7(count2),
     +             specT,a7T,iflag)
      call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +             specT,a8T,iflag)
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +             specT,a9T,iflag)

 1011 period1 = specT

C     Set Constant Terms
      Mref1 = 5.5
      Mref2 = 6.5

C     Compute the ground motion for the given spectral period.
      if (M .lt. Mref1) then
         lnY = a0T - a4T*(8.5-Mref1)**2.0 + a2T*(M-Mref1) + a1T*(Mref1-Mref2) + a8T*Rrup
     1             + (a5T+a6T*(Mref1-Mref2))*alog(sqrt(a7T*a7T+Rrup*Rrup))
      elseif (M .ge. Mref2) then
         lnY = a0T - a4T*(8.5-M)**2.0     + a3T*(M-Mref2)                     + a8T*Rrup
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rrup*Rrup))
      else
         lnY = a0T - a4T*(8.5-M)**2.0     + a1T*(M-Mref2)                     + a8T*Rrup
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rrup*Rrup))
      endif

C     Add Ztor term
      lnY = lnY + (a9T*ztor)/20.0

      sigma = 0.65

C     Convert ground motion to units of gals in natural log units.
      lnY = lnY + 4.6052

      period2 = period1

      return
      end

c----------------------------------------------------------------------
      subroutine S02_DCPP_Common005 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=5)
      REAL Period(MAXPER), a0(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), a7(MAXPER), a8(MAXPER), a9(MAXPER)
      real Mref1, MRef2, Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a0T, a1T, a2T, a3T, a4T, a5T, a6T
      REAL a7T, a8T, a9T
      integer iflag, count1, count2, nPer, i

      data period / 0.01, 0.2, 0.5, 1.0, 2.0 /
      data a0 / 3.493293094, 4.392226445, 3.95601353, 3.469189558, 2.770559363  /
      data a1 / -0.517413435, -0.538304154, -0.359381871, -0.29150999, -0.155053963  /
      data a2 / 1.16587349, 1.233499318, 1.462679511, 1.78422841, 2.068713002  /
      data a3 / -0.542730416, -0.627532626, -0.519420966, -0.525274929, -0.462301343  /
      data a4 / 0.107332105, 0.137442784, 0.15989904, 0.204782651, 0.241935468  /
      data a5 / -0.915960819, -0.900192462, -0.951585908, -0.971657293, -0.978575075  /
      data a6 / 0.251159906, 0.230060775, 0.205960024, 0.193677133, 0.184936972  /
      data a7 / 5.076184084, 5.414933339, 5.038544997, 4.787916605, 4.990588764  /
      data a8 / -0.006139721, -0.007045028, -0.003343442, -0.001995972, -0.001325647  /
      data a9 / 0.510976682, 0.462927961, 0.339345733, 0.262946907, 0.175755775  /

C Find the requested spectral period and corresponding coefficients
      nPer = 5
C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1  = period(1)
         a0T      = a0(1)
         a1T      = a1(1)
         a2T      = a2(1)
         a3T      = a3(1)
         a4T      = a4(1)
         a5T      = a5(1)
         a6T      = a6(1)
         a7T      = a7(1)
         a8T      = a8(1)
         a9T      = a9(1)
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'DCPP Common Model005 Horizontal'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),a0(count1),a0(count2),
     +             specT,a0T,iflag)
      call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +             specT,a1T,iflag)
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +             specT,a2T,iflag)
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +             specT,a3T,iflag)
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +             specT,a4T,iflag)
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +             specT,a5T,iflag)
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +             specT,a6T,iflag)
      call S24_interp (period(count1),period(count2),a7(count1),a7(count2),
     +             specT,a7T,iflag)
      call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +             specT,a8T,iflag)
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +             specT,a9T,iflag)

 1011 period1 = specT

C     Set Constant Terms
      Mref1 = 5.5
      Mref2 = 6.5

C     Compute the ground motion for the given spectral period.
      if (M .lt. Mref1) then
         lnY = a0T - a4T*(8.5-Mref1)**2.0 + a2T*(M-Mref1) + a1T*(Mref1-Mref2) + a8T*Rrup
     1             + (a5T+a6T*(Mref1-Mref2))*alog(sqrt(a7T*a7T+Rrup*Rrup))
      elseif (M .ge. Mref2) then
         lnY = a0T - a4T*(8.5-M)**2.0     + a3T*(M-Mref2)                     + a8T*Rrup
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rrup*Rrup))
      else
         lnY = a0T - a4T*(8.5-M)**2.0     + a1T*(M-Mref2)                     + a8T*Rrup
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rrup*Rrup))
      endif

C     Add Ztor term
      lnY = lnY + (a9T*ztor)/20.0

      sigma = 0.65

C     Convert ground motion to units of gals in natural log units.
      lnY = lnY + 4.6052

      period2 = period1

      return
      end

c----------------------------------------------------------------------
      subroutine S02_DCPP_Common006 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=5)
      REAL Period(MAXPER), a0(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), a7(MAXPER), a8(MAXPER), a9(MAXPER)
      real Mref1, MRef2, Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a0T, a1T, a2T, a3T, a4T, a5T, a6T
      REAL a7T, a8T, a9T
      integer iflag, count1, count2, nPer, i

      data period / 0.01, 0.2, 0.5, 1.0, 2.0 /
      data a0 / 3.251986267, 3.79460145, 3.214007046, 3.209422039, 2.780584602  /
      data a1 / -0.721542295, -0.457987926, 0.020037702, 0.054457977, 0.18122608  /
      data a2 / 1.374150887, 1.392020089, 1.684354703, 2.200994674, 2.526765962  /
      data a3 / -0.807794369, -0.476751375, 0.070507552, 0.027081071, 0.095441379  /
      data a4 / 0.180764078, 0.089446403, 0.071159723, 0.149870046, 0.191762188  /
      data a5 / -0.707990675, -0.693619571, -0.844621479, -0.951687138, -1.00883522  /
      data a6 / 0.312388841, 0.29851607, 0.250933355, 0.222208685, 0.209970296  /
      data a7 / 3.449899465, 4.230785374, 3.077035349, 2.225207351, 2.277830296  /
      data a8 / 0.000738982, -0.016130082, 0.008913823, -0.009884652, -0.004823573  /
      data a9 / 0.867370117, 0.762878054, 0.489665039, 0.349411054, 0.166504883  /

C Find the requested spectral period and corresponding coefficients
      nPer = 5
C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1  = period(1)
         a0T      = a0(1)
         a1T      = a1(1)
         a2T      = a2(1)
         a3T      = a3(1)
         a4T      = a4(1)
         a5T      = a5(1)
         a6T      = a6(1)
         a7T      = a7(1)
         a8T      = a8(1)
         a9T      = a9(1)
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'DCPP Common Model006 Horizontal'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),a0(count1),a0(count2),
     +             specT,a0T,iflag)
      call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +             specT,a1T,iflag)
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +             specT,a2T,iflag)
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +             specT,a3T,iflag)
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +             specT,a4T,iflag)
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +             specT,a5T,iflag)
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +             specT,a6T,iflag)
      call S24_interp (period(count1),period(count2),a7(count1),a7(count2),
     +             specT,a7T,iflag)
      call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +             specT,a8T,iflag)
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +             specT,a9T,iflag)

 1011 period1 = specT

C     Set Constant Terms
      Mref1 = 5.5
      Mref2 = 6.5

C     Compute the ground motion for the given spectral period.
      if (M .lt. Mref1) then
         lnY = a0T - a4T*(8.5-Mref1)**2.0 + a2T*(M-Mref1) + a1T*(Mref1-Mref2) + a8T*Rrup
     1             + (a5T+a6T*(Mref1-Mref2))*alog(sqrt(a7T*a7T+Rrup*Rrup))
      elseif (M .ge. Mref2) then
         lnY = a0T - a4T*(8.5-M)**2.0     + a3T*(M-Mref2)                     + a8T*Rrup
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rrup*Rrup))
      else
         lnY = a0T - a4T*(8.5-M)**2.0     + a1T*(M-Mref2)                     + a8T*Rrup
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rrup*Rrup))
      endif

C     Add Ztor term
      lnY = lnY + (a9T*ztor)/20.0

      sigma = 0.65

C     Convert ground motion to units of gals in natural log units.
      lnY = lnY + 4.6052

      period2 = period1

      return
      end

c----------------------------------------------------------------------
      subroutine S02_DCPP_Common007 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=5)
      REAL Period(MAXPER), a0(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), a7(MAXPER), a8(MAXPER), a9(MAXPER)
      real Mref1, MRef2, Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a0T, a1T, a2T, a3T, a4T, a5T, a6T
      REAL a7T, a8T, a9T
      integer iflag, count1, count2, nPer, i

      data period / 0.01, 0.2, 0.5, 1.0, 2.0 /
      data a0 / 3.522281793, 3.779815051, 3.790127509, 3.366254154, 2.993648492  /
      data a1 / -0.745505344, -0.802692452, -1.438293627, -1.680874156, -2.084533152  /
      data a2 / 0.04047644, 0.115820555, 0.731216267, 1.07504443, 1.388190912  /
      data a3 / -0.698386839, -0.543240211, -0.794857318, -1.012121099, -1.405991344  /
      data a4 / 0.241824727, 0.207003949, 0.333873833, 0.460081283, 0.575052544  /
      data a5 / -0.715813871, -0.609714344, -0.674005396, -0.652671853, -0.627627914  /
      data a6 / 0.202896307, 0.257134674, 0.198732616, 0.18934498, 0.221155607  /
      data a7 / 3.798280073, 3.661629505, 2.978961455, 2.769264077, 2.027250135  /
      data a8 / -0.02243076, -0.025143069, -0.013872542, -0.022910165, -0.012336009  /
      data a9 / 0.407627055, 0.395697911, 0.280057509, 0.164297046, 0.124698346  /

C Find the requested spectral period and corresponding coefficients
      nPer = 5
C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1  = period(1)
         a0T      = a0(1)
         a1T      = a1(1)
         a2T      = a2(1)
         a3T      = a3(1)
         a4T      = a4(1)
         a5T      = a5(1)
         a6T      = a6(1)
         a7T      = a7(1)
         a8T      = a8(1)
         a9T      = a9(1)
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'DCPP Common Model007 Horizontal'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),a0(count1),a0(count2),
     +             specT,a0T,iflag)
      call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +             specT,a1T,iflag)
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +             specT,a2T,iflag)
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +             specT,a3T,iflag)
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +             specT,a4T,iflag)
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +             specT,a5T,iflag)
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +             specT,a6T,iflag)
      call S24_interp (period(count1),period(count2),a7(count1),a7(count2),
     +             specT,a7T,iflag)
      call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +             specT,a8T,iflag)
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +             specT,a9T,iflag)

 1011 period1 = specT

C     Set Constant Terms
      Mref1 = 5.5
      Mref2 = 6.5

C     Compute the ground motion for the given spectral period.
      if (M .lt. Mref1) then
         lnY = a0T - a4T*(8.5-Mref1)**2.0 + a2T*(M-Mref1) + a1T*(Mref1-Mref2) + a8T*Rrup
     1             + (a5T+a6T*(Mref1-Mref2))*alog(sqrt(a7T*a7T+Rrup*Rrup))
      elseif (M .ge. Mref2) then
         lnY = a0T - a4T*(8.5-M)**2.0     + a3T*(M-Mref2)                     + a8T*Rrup
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rrup*Rrup))
      else
         lnY = a0T - a4T*(8.5-M)**2.0     + a1T*(M-Mref2)                     + a8T*Rrup
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rrup*Rrup))
      endif

C     Add Ztor term
      lnY = lnY + (a9T*ztor)/20.0

      sigma = 0.65

C     Convert ground motion to units of gals in natural log units.
      lnY = lnY + 4.6052

      period2 = period1

      return
      end

c----------------------------------------------------------------------
      subroutine S02_DCPP_Common008 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=5)
      REAL Period(MAXPER), a0(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), a7(MAXPER), a8(MAXPER), a9(MAXPER)
      real Mref1, MRef2, Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a0T, a1T, a2T, a3T, a4T, a5T, a6T
      REAL a7T, a8T, a9T
      integer iflag, count1, count2, nPer, i

      data period / 0.01, 0.2, 0.5, 1.0, 2.0 /
      data a0 / 3.252954349, 3.26016395, 2.995497924, 2.902377492, 2.667079617  /
      data a1 / -0.667737691, -0.559316539, -0.921398892, -1.255008316, -1.437287148  /
      data a2 / 0.326501995, 0.355401061, 0.658464726, 1.036373301, 1.414567899  /
      data a3 / -0.659992815, -0.337970183, -0.276914155, -0.42718595, -0.626393327  /
      data a4 / 0.314873913, 0.197241241, 0.289814332, 0.419103558, 0.529254838  /
      data a5 / -0.456132241, -0.38443632, -0.4197599, -0.45562668, -0.46287412  /
      data a6 / 0.145043277, 0.178941179, 0.154732928, 0.149078213, 0.125423811  /
      data a7 / 2.266717573, 3.183447009, 1.416544689, 0.332686983, -0.208385792  /
      data a8 / -0.032599211, -0.005307832, -0.01490414, -0.01195826, -0.026117257  /
      data a9 / 0.163662802, 0.111881726, -0.051753846, -0.160656287, -0.253421932  /

C Find the requested spectral period and corresponding coefficients
      nPer = 5
C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1  = period(1)
         a0T      = a0(1)
         a1T      = a1(1)
         a2T      = a2(1)
         a3T      = a3(1)
         a4T      = a4(1)
         a5T      = a5(1)
         a6T      = a6(1)
         a7T      = a7(1)
         a8T      = a8(1)
         a9T      = a9(1)
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'DCPP Common Model008 Horizontal'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),a0(count1),a0(count2),
     +             specT,a0T,iflag)
      call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +             specT,a1T,iflag)
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +             specT,a2T,iflag)
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +             specT,a3T,iflag)
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +             specT,a4T,iflag)
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +             specT,a5T,iflag)
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +             specT,a6T,iflag)
      call S24_interp (period(count1),period(count2),a7(count1),a7(count2),
     +             specT,a7T,iflag)
      call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +             specT,a8T,iflag)
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +             specT,a9T,iflag)

 1011 period1 = specT

C     Set Constant Terms
      Mref1 = 5.5
      Mref2 = 6.5

C     Compute the ground motion for the given spectral period.
      if (M .lt. Mref1) then
         lnY = a0T - a4T*(8.5-Mref1)**2.0 + a2T*(M-Mref1) + a1T*(Mref1-Mref2) + a8T*Rrup
     1             + (a5T+a6T*(Mref1-Mref2))*alog(sqrt(a7T*a7T+Rrup*Rrup))
      elseif (M .ge. Mref2) then
         lnY = a0T - a4T*(8.5-M)**2.0     + a3T*(M-Mref2)                     + a8T*Rrup
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rrup*Rrup))
      else
         lnY = a0T - a4T*(8.5-M)**2.0     + a1T*(M-Mref2)                     + a8T*Rrup
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rrup*Rrup))
      endif

C     Add Ztor term
      lnY = lnY + (a9T*ztor)/20.0

      sigma = 0.65

C     Convert ground motion to units of gals in natural log units.
      lnY = lnY + 4.6052

      period2 = period1

      return
      end

c----------------------------------------------------------------------
      subroutine S02_DCPP_Common009 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=5)
      REAL Period(MAXPER), a0(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), a7(MAXPER), a8(MAXPER), a9(MAXPER)
      real Mref1, MRef2, Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a0T, a1T, a2T, a3T, a4T, a5T, a6T
      REAL a7T, a8T, a9T
      integer iflag, count1, count2, nPer, i

      data period / 0.01, 0.2, 0.5, 1.0, 2.0 /
      data a0 / 2.87957963, 3.267451272, 2.934459189, 2.807261175, 2.375468933  /
      data a1 / -0.340091418, -0.264153471, 0.007822577, -0.055582518, -0.096382979  /
      data a2 / 0.877599706, 0.908074957, 1.278377881, 1.753596622, 2.11630298  /
      data a3 / -0.385424946, -0.240541882, 0.179140064, 0.081707406, 0.00501884  /
      data a4 / 0.132715693, 0.074441917, 0.072453275, 0.178176928, 0.250392558  /
      data a5 / -0.645626944, -0.585940757, -0.722451475, -0.790354398, -0.839573127  /
      data a6 / 0.257842701, 0.277186164, 0.230779667, 0.213220683, 0.185367497  /
      data a7 / 3.109921833, 3.427837094, 2.705180991, 2.239119649, 2.00539171  /
      data a8 / -0.005268259, -0.013346038, 2.40106E-05, -0.017669574, -0.01293008  /
      data a9 / 0.367840808, 0.329691579, 0.233494917, 0.151155273, 0.106584807  /

C Find the requested spectral period and corresponding coefficients
      nPer = 5
C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1  = period(1)
         a0T      = a0(1)
         a1T      = a1(1)
         a2T      = a2(1)
         a3T      = a3(1)
         a4T      = a4(1)
         a5T      = a5(1)
         a6T      = a6(1)
         a7T      = a7(1)
         a8T      = a8(1)
         a9T      = a9(1)
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'DCPP Common Model009 Horizontal'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),a0(count1),a0(count2),
     +             specT,a0T,iflag)
      call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +             specT,a1T,iflag)
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +             specT,a2T,iflag)
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +             specT,a3T,iflag)
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +             specT,a4T,iflag)
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +             specT,a5T,iflag)
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +             specT,a6T,iflag)
      call S24_interp (period(count1),period(count2),a7(count1),a7(count2),
     +             specT,a7T,iflag)
      call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +             specT,a8T,iflag)
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +             specT,a9T,iflag)

 1011 period1 = specT

C     Set Constant Terms
      Mref1 = 5.5
      Mref2 = 6.5

C     Compute the ground motion for the given spectral period.
      if (M .lt. Mref1) then
         lnY = a0T - a4T*(8.5-Mref1)**2.0 + a2T*(M-Mref1) + a1T*(Mref1-Mref2) + a8T*Rrup
     1             + (a5T+a6T*(Mref1-Mref2))*alog(sqrt(a7T*a7T+Rrup*Rrup))
      elseif (M .ge. Mref2) then
         lnY = a0T - a4T*(8.5-M)**2.0     + a3T*(M-Mref2)                     + a8T*Rrup
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rrup*Rrup))
      else
         lnY = a0T - a4T*(8.5-M)**2.0     + a1T*(M-Mref2)                     + a8T*Rrup
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rrup*Rrup))
      endif

C     Add Ztor term
      lnY = lnY + (a9T*ztor)/20.0

      sigma = 0.65

C     Convert ground motion to units of gals in natural log units.
      lnY = lnY + 4.6052

      period2 = period1

      return
      end

c----------------------------------------------------------------------
      subroutine S02_PVNGS_CommonASK ( m, Rjb, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=5)

      REAL Period(MAXPER), a0(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), a7(MAXPER), a8(MAXPER), a9(MAXPER)
      real Mref1, MRef2, Rjb, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a0T, a1T, a2T, a3T, a4T, a5T, a6T
      REAL a7T, a8T, a9T
      integer iflag, count1, count2, nPer, i

      data period / 0.01, 0.2, 0.5, 1.0, 2.0 /
      data a0 / 3.343907904, 4.252801795, 3.566737128, 2.938042573, 2.075468023  /
      data a1 / 0.080811823, 0.084210319, 0.073042473, 0.054685714, -0.009824295  /
      data a2 / 0.865947735, 0.963370266, 1.190967949, 1.48365227, 1.666544209  /
      data a3 / -0.095339756, -0.092834299, -0.104891355, -0.124136064, -0.185838727  /
      data a4 / 0.007702263, 0.021973272, 0.055388872, 0.098374638, 0.125433795  /
      data a5 / -1.0769308, -1.076935123, -1.063802868, -1.044096653, -0.994797979  /
      data a6 / 0.244690771, 0.244690839, 0.249686531, 0.257157478, 0.275708113  /
      data a7 / 5.086963275, 5.08699764, 5.066192565, 5.034072914, 4.948704972  /
      data a8 / -0.002933616, -0.004333507, 0.000807676, 0.001118967, 0.000744735  /
      data a9 / 1.080088531, 1.080087809, 0.821269279, 0.553048997, 0.297539821  /

C Find the requested spectral period and corresponding coefficients
      nPer = 5
C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1  = period(1)
         a0T      = a0(1)
         a1T      = a1(1)
         a2T      = a2(1)
         a3T      = a3(1)
         a4T      = a4(1)
         a5T      = a5(1)
         a6T      = a6(1)
         a7T      = a7(1)
         a8T      = a8(1)
         a9T      = a9(1)
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'PVNGS Common Model ASK Horizontal'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),a0(count1),a0(count2),
     +             specT,a0T,iflag)
      call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +             specT,a1T,iflag)
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +             specT,a2T,iflag)
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +             specT,a3T,iflag)
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +             specT,a4T,iflag)
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +             specT,a5T,iflag)
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +             specT,a6T,iflag)
      call S24_interp (period(count1),period(count2),a7(count1),a7(count2),
     +             specT,a7T,iflag)
      call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +             specT,a8T,iflag)
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +             specT,a9T,iflag)

 1011 period1 = specT

C     Set Constant Terms
      Mref1 = 5.5
      Mref2 = 6.5

C     Compute the ground motion for the given spectral period.
      if (M .lt. Mref1) then
         lnY = a0T - a4T*(8.5-Mref1)**2.0 + a2T*(M-Mref1) + a1T*(Mref1-Mref2) + a8T*Rjb
     1             + (a5T+a6T*(Mref1-Mref2))*alog(sqrt(a7T*a7T+Rjb*Rjb))
      elseif (M .ge. Mref2) then
         lnY = a0T - a4T*(8.5-M)**2.0     + a3T*(M-Mref2)                     + a8T*Rjb
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rjb*Rjb))
      else
         lnY = a0T - a4T*(8.5-M)**2.0     + a1T*(M-Mref2)                     + a8T*Rjb
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rjb*Rjb))
      endif

C     Add Ztor term
      lnY = lnY + (a9T*ztor)/20.0

      sigma = 0.65

C     Convert ground motion to units of gals in natural log units.
      lnY = lnY + 4.6052

      period2 = period1

      return
      end

c----------------------------------------------------------------------
      subroutine S02_PVNGS_CommonBindi ( m, Rjb, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=5)
      REAL Period(MAXPER), a0(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), a7(MAXPER), a8(MAXPER), a9(MAXPER)
      real Mref1, MRef2, Rjb, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a0T, a1T, a2T, a3T, a4T, a5T, a6T
      REAL a7T, a8T, a9T
      integer iflag, count1, count2, nPer, i

      data period / 0.01, 0.2, 0.5, 1.0, 2.0 /
      data a0 / 5.477789171, 6.506273744, 5.14623719, 3.78029147, 2.717755816  /
      data a1 / -0.232825243, -0.233689039, -0.207289509, -0.187375433, -0.172650217  /
      data a2 / 0.947285157, 0.993453424, 1.388735997, 1.706872295, 2.012465488  /
      data a3 / -0.472012097, -0.472300013, -0.454726948, -0.441602562, -0.431571518  /
      data a4 / 0.000692252, 0.007293516, 0.066040163, 0.113273155, 0.158347203  /
      data a5 / -1.94765912, -2.022422359, -1.632894927, -1.342400966, -1.200172069  /
      data a6 / 0.369759727, 0.370933284, 0.364894114, 0.360547331, 0.358487482  /
      data a7 / 8.119918051, 8.113741035, 8.150754423, 8.188080348, 8.210408959  /
      data a8 / 0.015925715, 0.016487051, 0.01355982, 0.011369528, 0.010293092  /
      data a9 / 0.622838681, 0.645423299, 0.527834946, 0.440359475, 0.397648621  /

C Find the requested spectral period and corresponding coefficients
      nPer = 5
C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1  = period(1)
         a0T      = a0(1)
         a1T      = a1(1)
         a2T      = a2(1)
         a3T      = a3(1)
         a4T      = a4(1)
         a5T      = a5(1)
         a6T      = a6(1)
         a7T      = a7(1)
         a8T      = a8(1)
         a9T      = a9(1)
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'PVNGS Common Model Bindi Horizontal'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),a0(count1),a0(count2),
     +             specT,a0T,iflag)
      call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +             specT,a1T,iflag)
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +             specT,a2T,iflag)
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +             specT,a3T,iflag)
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +             specT,a4T,iflag)
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +             specT,a5T,iflag)
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +             specT,a6T,iflag)
      call S24_interp (period(count1),period(count2),a7(count1),a7(count2),
     +             specT,a7T,iflag)
      call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +             specT,a8T,iflag)
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +             specT,a9T,iflag)

 1011 period1 = specT

C     Set Constant Terms
      Mref1 = 5.5
      Mref2 = 6.5

C     Compute the ground motion for the given spectral period.
      if (M .lt. Mref1) then
         lnY = a0T - a4T*(8.5-Mref1)**2.0 + a2T*(M-Mref1) + a1T*(Mref1-Mref2) + a8T*Rjb
     1             + (a5T+a6T*(Mref1-Mref2))*alog(sqrt(a7T*a7T+Rjb*Rjb))
      elseif (M .ge. Mref2) then
         lnY = a0T - a4T*(8.5-M)**2.0     + a3T*(M-Mref2)                     + a8T*Rjb
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rjb*Rjb))
      else
         lnY = a0T - a4T*(8.5-M)**2.0     + a1T*(M-Mref2)                     + a8T*Rjb
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rjb*Rjb))
      endif

C     Add Ztor term
      lnY = lnY + (a9T*ztor)/20.0

      sigma = 0.65

C     Convert ground motion to units of gals in natural log units.
      lnY = lnY + 4.6052

      period2 = period1

      return
      end

c----------------------------------------------------------------------
      subroutine S02_PVNGS_Common001 ( m, Rjb, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=5)
      REAL Period(MAXPER), a0(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), a7(MAXPER), a8(MAXPER), a9(MAXPER)
      real Mref1, MRef2, Rjb, Ztor, period1
      REAL M, specT, sigma
      REAL period2, lnY, a0T, a1T, a2T, a3T, a4T, a5T, a6T, a7T
      REAL a8T, a9T
      integer iflag, count1, count2, nPer, i

      data period / 0.01, 0.2, 0.5, 1.0, 2.0 /
      data a0 / 3.359674285, 2.999532571, 2.513715574, 1.550253211, 0.481805389  /
      data a1 / 0.259051103, 1.806534394, 1.457260206, 2.145567828, 2.423955513  /
      data a2 / 1.29166876, 1.509755983, 1.5523006, 1.811610628, 2.00622729  /
      data a3 / -0.046432184, 1.164786842, 0.916676151, 1.378670837, 1.443426906  /
      data a4 / 0.051915891, -0.220276726, -0.156106833, -0.278186332, -0.324826277  /
      data a5 / -0.944987414, -0.916703744, -0.928962513, -0.99079302, -0.933204263  /
      data a6 / 0.157927576, 0.075356957, 0.114199324, 0.149706685, 0.176127245  /
      data a7 / 4.546890862, 5.237645969, 4.367579105, 3.897887087, 4.288293044  /
      data a8 / 0.00797077, -0.013157019, -0.013825852, 0.001092373, -0.017732395  /
      data a9 / 1.48171669, 1.324991739, 0.837728298, 0.463713454, 0.034077117  /

C Find the requested spectral period and corresponding coefficients
      nPer = 5
C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1  = period(1)
         a0T      = a0(1)
         a1T      = a1(1)
         a2T      = a2(1)
         a3T      = a3(1)
         a4T      = a4(1)
         a5T      = a5(1)
         a6T      = a6(1)
         a7T      = a7(1)
         a8T      = a8(1)
         a9T      = a9(1)
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'PVNGS Common Model001 Horizontal'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),a0(count1),a0(count2),
     +             specT,a0T,iflag)
      call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +             specT,a1T,iflag)
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +             specT,a2T,iflag)
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +             specT,a3T,iflag)
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +             specT,a4T,iflag)
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +             specT,a5T,iflag)
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +             specT,a6T,iflag)
      call S24_interp (period(count1),period(count2),a7(count1),a7(count2),
     +             specT,a7T,iflag)
      call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +             specT,a8T,iflag)
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +             specT,a9T,iflag)

 1011 period1 = specT

C     Set Constant Terms
      Mref1 = 5.5
      Mref2 = 6.5

C     Compute the ground motion for the given spectral period.
      if (M .lt. Mref1) then
         lnY = a0T - a4T*(8.5-Mref1)**2.0 + a2T*(M-Mref1) + a1T*(Mref1-Mref2) + a8T*Rjb
     1             + (a5T+a6T*(Mref1-Mref2))*alog(sqrt(a7T*a7T+Rjb*Rjb))
      elseif (M .ge. Mref2) then
         lnY = a0T - a4T*(8.5-M)**2.0     + a3T*(M-Mref2)                     + a8T*Rjb
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rjb*Rjb))
      else
         lnY = a0T - a4T*(8.5-M)**2.0     + a1T*(M-Mref2)                     + a8T*Rjb
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rjb*Rjb))
      endif

C     Add Ztor term
      lnY = lnY + (a9T*ztor)/20.0

      sigma = 0.65

C     Convert ground motion to units of gals in natural log units.
      lnY = lnY + 4.6052

      period2 = period1

      return
      end

c----------------------------------------------------------------------
      subroutine S02_PVNGS_Common002 ( m, Rjb, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=5)
      REAL Period(MAXPER), a0(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), a7(MAXPER), a8(MAXPER), a9(MAXPER)
      real Mref1, MRef2, Rjb, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a0T, a1T, a2T, a3T, a4T, a5T, a6T
      REAL a7T, a8T, a9T
      integer iflag, count1, count2, nPer, i

      data period / 0.01, 0.2, 0.5, 1.0, 2.0 /
      data a0 / 4.469511159, 4.669919592, 5.026559877, 4.778429922, 3.674178555  /
      data a1 / -1.016050665, -0.318336897, -0.908739603, -0.913573926, -1.233112887  /
      data a2 / 0.611821156, 0.695479803, 1.376384997, 1.736252026, 1.937118833  /
      data a3 / -0.846562333, -0.218552638, -0.695414516, -0.818961659, -1.177798878  /
      data a4 / 0.138947695, 0.0526643, 0.263814563, 0.347699478, 0.393435099  /
      data a5 / -1.393924555, -1.299071436, -1.390369351, -1.424281402, -1.253137814  /
      data a6 / 0.333692768, 0.259002892, 0.209262158, 0.210308355, 0.28659741  /
      data a7 / 5.411275966, 5.16809154, 4.78359839, 4.869260987, 4.582358809  /
      data a8 / 0.013271239, 0.009010653, 0.000198032, 0.012491114, -0.002793928  /
      data a9 / 0.728099195, 0.757419119, 0.75456221, 0.670391643, 0.585756882  /

C Find the requested spectral period and corresponding coefficients
      nPer = 5
C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1  = period(1)
         a0T      = a0(1)
         a1T      = a1(1)
         a2T      = a2(1)
         a3T      = a3(1)
         a4T      = a4(1)
         a5T      = a5(1)
         a6T      = a6(1)
         a7T      = a7(1)
         a8T      = a8(1)
         a9T      = a9(1)
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'PVNGS Common Model002 Horizontal'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),a0(count1),a0(count2),
     +             specT,a0T,iflag)
      call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +             specT,a1T,iflag)
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +             specT,a2T,iflag)
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +             specT,a3T,iflag)
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +             specT,a4T,iflag)
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +             specT,a5T,iflag)
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +             specT,a6T,iflag)
      call S24_interp (period(count1),period(count2),a7(count1),a7(count2),
     +             specT,a7T,iflag)
      call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +             specT,a8T,iflag)
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +             specT,a9T,iflag)

 1011 period1 = specT

C     Set Constant Terms
      Mref1 = 5.5
      Mref2 = 6.5

C     Compute the ground motion for the given spectral period.
      if (M .lt. Mref1) then
         lnY = a0T - a4T*(8.5-Mref1)**2.0 + a2T*(M-Mref1) + a1T*(Mref1-Mref2) + a8T*Rjb
     1             + (a5T+a6T*(Mref1-Mref2))*alog(sqrt(a7T*a7T+Rjb*Rjb))
      elseif (M .ge. Mref2) then
         lnY = a0T - a4T*(8.5-M)**2.0     + a3T*(M-Mref2)                     + a8T*Rjb
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rjb*Rjb))
      else
         lnY = a0T - a4T*(8.5-M)**2.0     + a1T*(M-Mref2)                     + a8T*Rjb
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rjb*Rjb))
      endif

C     Add Ztor term
      lnY = lnY + (a9T*ztor)/20.0

      sigma = 0.65

C     Convert ground motion to units of gals in natural log units.
      lnY = lnY + 4.6052

      period2 = period1

      return
      end

c----------------------------------------------------------------------
      subroutine S02_PVNGS_Common003 ( m, Rjb, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=5)
      REAL Period(MAXPER), a0(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), a7(MAXPER), a8(MAXPER), a9(MAXPER)
      real Mref1, MRef2, Rjb, Ztor
      REAL M, specT, sigma, period1
      REAL period2, lnY, a0T, a1T, a2T, a3T, a4T, a5T, a6T, a7T
      REAL a8T, a9T
      integer iflag, count1, count2, nPer, i

      data period / 0.01, 0.2, 0.5, 1.0, 2.0 /
      data a0 / 3.875846016, 4.604039829, 5.09888582, 5.595457504, 4.923491709  /
      data a1 / -0.685741648, -0.282098998, -0.628985504, -1.142972715, -1.166258938  /
      data a2 / 1.777102049, 1.867105674, 2.267095367, 2.639663451, 2.958850385  /
      data a3 / -0.680100533, -0.339853919, -0.618838767, -1.181973274, -1.446096257  /
      data a4 / 0.162748119, 0.136429136, 0.247497324, 0.452750546, 0.503902043  /
      data a5 / -1.167243929, -1.100854485, -1.318943697, -1.464731932, -1.402028153  /
      data a6 / 0.23549403, 0.097212295, 0.063363647, 0.055289666, 0.08840652  /
      data a7 / 5.070646018, 5.620542857, 4.75940936, 4.41599181, 4.992410748  /
      data a8 / 0.005927228, -0.004610289, -0.006873191, -0.007525715, -0.007884014  /
      data a9 / 1.090435535, 0.865707921, 0.591183399, 0.492594083, 0.367542158  /

C Find the requested spectral period and corresponding coefficients
      nPer = 5
C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1  = period(1)
         a0T      = a0(1)
         a1T      = a1(1)
         a2T      = a2(1)
         a3T      = a3(1)
         a4T      = a4(1)
         a5T      = a5(1)
         a6T      = a6(1)
         a7T      = a7(1)
         a8T      = a8(1)
         a9T      = a9(1)
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'PVNGS Common Model003 Horizontal'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),a0(count1),a0(count2),
     +             specT,a0T,iflag)
      call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +             specT,a1T,iflag)
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +             specT,a2T,iflag)
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +             specT,a3T,iflag)
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +             specT,a4T,iflag)
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +             specT,a5T,iflag)
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +             specT,a6T,iflag)
      call S24_interp (period(count1),period(count2),a7(count1),a7(count2),
     +             specT,a7T,iflag)
      call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +             specT,a8T,iflag)
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +             specT,a9T,iflag)

 1011 period1 = specT

C     Set Constant Terms
      Mref1 = 5.5
      Mref2 = 6.5

C     Compute the ground motion for the given spectral period.
      if (M .lt. Mref1) then
         lnY = a0T - a4T*(8.5-Mref1)**2.0 + a2T*(M-Mref1) + a1T*(Mref1-Mref2) + a8T*Rjb
     1             + (a5T+a6T*(Mref1-Mref2))*alog(sqrt(a7T*a7T+Rjb*Rjb))
      elseif (M .ge. Mref2) then
         lnY = a0T - a4T*(8.5-M)**2.0     + a3T*(M-Mref2)                     + a8T*Rjb
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rjb*Rjb))
      else
         lnY = a0T - a4T*(8.5-M)**2.0     + a1T*(M-Mref2)                     + a8T*Rjb
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rjb*Rjb))
      endif

C     Add Ztor term
      lnY = lnY + (a9T*ztor)/20.0

      sigma = 0.65

C     Convert ground motion to units of gals in natural log units.
      lnY = lnY + 4.6052

      period2 = period1

      return
      end

c----------------------------------------------------------------------
      subroutine S02_PVNGS_Common004 ( m, Rjb, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=5)
      REAL Period(MAXPER), a0(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), a7(MAXPER), a8(MAXPER), a9(MAXPER)
      real Mref1, MRef2, Rjb, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a0T, a1T, a2T, a3T, a4T, a5T, a6T
      REAL a7T, a8T, a9T
      integer iflag, count1, count2, nPer, i

      data period / 0.01, 0.2, 0.5, 1.0, 2.0 /
      data a0 / 3.831573393, 2.035301483, 2.680210979, 2.127825912, 0.975642192  /
      data a1 / -0.505892635, 2.517527172, 1.235975612, 1.701487361, 1.527531384  /
      data a2 / 0.66723769, 0.95917924, 1.451925977, 1.791003984, 1.951961942  /
      data a3 / -0.527776405, 1.898778895, 0.992018092, 1.260611637, 0.908347556  /
      data a4 / 0.181773916, -0.422365062, -0.160859691, -0.190827403, -0.159870762  /
      data a5 / -1.055947659, -0.928049678, -1.026069961, -1.138189325, -0.96732317  /
      data a6 / 0.223345863, 0.11253287, 0.12542901, 0.13800759, 0.224458062  /
      data a7 / 4.239531292, 4.503190993, 3.46568974, 3.149501089, 3.044427493  /
      data a8 / -0.01950985, 0.00447459, -0.000860926, 0.003015036, -0.01290382  /
      data a9 / 1.300744622, 1.170906174, 0.870374403, 0.596846482, 0.300315471  /

C Find the requested spectral period and corresponding coefficients
      nPer = 5
C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1  = period(1)
         a0T      = a0(1)
         a1T      = a1(1)
         a2T      = a2(1)
         a3T      = a3(1)
         a4T      = a4(1)
         a5T      = a5(1)
         a6T      = a6(1)
         a7T      = a7(1)
         a8T      = a8(1)
         a9T      = a9(1)
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'PVNGS Common Model004 Horizontal'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),a0(count1),a0(count2),
     +             specT,a0T,iflag)
      call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +             specT,a1T,iflag)
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +             specT,a2T,iflag)
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +             specT,a3T,iflag)
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +             specT,a4T,iflag)
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +             specT,a5T,iflag)
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +             specT,a6T,iflag)
      call S24_interp (period(count1),period(count2),a7(count1),a7(count2),
     +             specT,a7T,iflag)
      call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +             specT,a8T,iflag)
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +             specT,a9T,iflag)

 1011 period1 = specT

C     Set Constant Terms
      Mref1 = 5.5
      Mref2 = 6.5

C     Compute the ground motion for the given spectral period.
      if (M .lt. Mref1) then
         lnY = a0T - a4T*(8.5-Mref1)**2.0 + a2T*(M-Mref1) + a1T*(Mref1-Mref2) + a8T*Rjb
     1             + (a5T+a6T*(Mref1-Mref2))*alog(sqrt(a7T*a7T+Rjb*Rjb))
      elseif (M .ge. Mref2) then
         lnY = a0T - a4T*(8.5-M)**2.0     + a3T*(M-Mref2)                     + a8T*Rjb
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rjb*Rjb))
      else
         lnY = a0T - a4T*(8.5-M)**2.0     + a1T*(M-Mref2)                     + a8T*Rjb
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rjb*Rjb))
      endif

C     Add Ztor term
      lnY = lnY + (a9T*ztor)/20.0

      sigma = 0.65

C     Convert ground motion to units of gals in natural log units.
      lnY = lnY + 4.6052

      period2 = period1

      return
      end

c----------------------------------------------------------------------
      subroutine S02_PVNGS_Common005 ( m, Rjb, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=5)
      REAL Period(MAXPER), a0(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), a7(MAXPER), a8(MAXPER), a9(MAXPER)
      real Mref1, MRef2, Rjb, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a0T, a1T, a2T, a3T, a4T, a5T, a6T
      REAL a7T, a8T, a9T
      integer iflag, count1, count2, nPer, i

      data period / 0.01, 0.2, 0.5, 1.0, 2.0 /
      data a0 / 4.305480459, 5.832489255, 5.193434708, 4.707772707, 3.781149495  /
      data a1 / -0.360669872, -0.954790898, -0.772192062, -0.878558604, -0.847823755  /
      data a2 / 1.316503941, 1.346260947, 1.643811116, 1.971809782, 2.231346853  /
      data a3 / -0.456399589, -0.898904451, -0.838833356, -0.988747707, -1.03656776  /
      data a4 / 0.050879024, 0.216757739, 0.23862625, 0.320862075, 0.359685226  /
      data a5 / -1.433196805, -1.43253025, -1.388659927, -1.351932734, -1.268368489  /
      data a6 / 0.287168587, 0.23487456, 0.220995941, 0.214671577, 0.232463154  /
      data a7 / 6.222079151, 6.393254173, 6.177291568, 6.114401188, 6.353461281  /
      data a8 / 0.006308985, 0.004963986, 0.007864319, 0.008599691, 0.007901766  /
      data a9 / 0.853827962, 0.791949454, 0.622792362, 0.509051162, 0.383062867  /

C Find the requested spectral period and corresponding coefficients
      nPer = 5
C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1  = period(1)
         a0T      = a0(1)
         a1T      = a1(1)
         a2T      = a2(1)
         a3T      = a3(1)
         a4T      = a4(1)
         a5T      = a5(1)
         a6T      = a6(1)
         a7T      = a7(1)
         a8T      = a8(1)
         a9T      = a9(1)
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'PVNGS Common Model005 Horizontal'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),a0(count1),a0(count2),
     +             specT,a0T,iflag)
      call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +             specT,a1T,iflag)
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +             specT,a2T,iflag)
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +             specT,a3T,iflag)
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +             specT,a4T,iflag)
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +             specT,a5T,iflag)
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +             specT,a6T,iflag)
      call S24_interp (period(count1),period(count2),a7(count1),a7(count2),
     +             specT,a7T,iflag)
      call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +             specT,a8T,iflag)
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +             specT,a9T,iflag)

 1011 period1 = specT

C     Set Constant Terms
      Mref1 = 5.5
      Mref2 = 6.5

C     Compute the ground motion for the given spectral period.
      if (M .lt. Mref1) then
         lnY = a0T - a4T*(8.5-Mref1)**2.0 + a2T*(M-Mref1) + a1T*(Mref1-Mref2) + a8T*Rjb
     1             + (a5T+a6T*(Mref1-Mref2))*alog(sqrt(a7T*a7T+Rjb*Rjb))
      elseif (M .ge. Mref2) then
         lnY = a0T - a4T*(8.5-M)**2.0     + a3T*(M-Mref2)                     + a8T*Rjb
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rjb*Rjb))
      else
         lnY = a0T - a4T*(8.5-M)**2.0     + a1T*(M-Mref2)                     + a8T*Rjb
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rjb*Rjb))
      endif

C     Add Ztor term
      lnY = lnY + (a9T*ztor)/20.0

      sigma = 0.65

C     Convert ground motion to units of gals in natural log units.
      lnY = lnY + 4.6052

      period2 = period1

      return
      end

c----------------------------------------------------------------------
      subroutine S02_PVNGS_Common006 ( m, Rjb, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=5)
      REAL Period(MAXPER), a0(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), a7(MAXPER), a8(MAXPER), a9(MAXPER)
      real Mref1, MRef2, Rjb, Ztor
      REAL M, specT, sigma, period1, period2, lnY
      REAL a0T, a1T, a2T, a3T, a4T, a5T, a6T, a7T, a8T, a9T
      integer iflag, count1, count2, nPer, i

      data period / 0.01, 0.2, 0.5, 1.0, 2.0 /
      data a0 / 4.86597378, 6.144703681, 5.121782642, 4.387039752, 3.546300844  /
      data a1 / -0.273088209, -0.63407702, -0.57547517, -1.041151272, -1.146959743  /
      data a2 / 0.933580946, 0.971192248, 1.407326375, 1.760001089, 2.024389433  /
      data a3 / -0.432606019, -0.69651455, -0.686341995, -1.028830191, -1.126268348  /
      data a4 / 0.003161612, 0.078029949, 0.139172231, 0.295446332, 0.373856164  /
      data a5 / -1.72560986, -1.804855901, -1.538783528, -1.317089532, -1.217781747  /
      data a6 / 0.337828643, 0.376147478, 0.342958148, 0.345931625, 0.334376914  /
      data a7 / 7.371186735, 7.239836089, 7.415332136, 7.5134082, 7.43850092  /
      data a8 / 0.018197929, 0.005151621, 0.003784379, 0.006872836, 0.008972623  /
      data a9 / 0.659007193, 0.658493883, 0.564530063, 0.481921424, 0.45283348  /

C Find the requested spectral period and corresponding coefficients
      nPer = 5
C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1  = period(1)
         a0T      = a0(1)
         a1T      = a1(1)
         a2T      = a2(1)
         a3T      = a3(1)
         a4T      = a4(1)
         a5T      = a5(1)
         a6T      = a6(1)
         a7T      = a7(1)
         a8T      = a8(1)
         a9T      = a9(1)
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'PVNGS Common Model006 Horizontal'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),a0(count1),a0(count2),
     +             specT,a0T,iflag)
      call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +             specT,a1T,iflag)
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +             specT,a2T,iflag)
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +             specT,a3T,iflag)
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +             specT,a4T,iflag)
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +             specT,a5T,iflag)
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +             specT,a6T,iflag)
      call S24_interp (period(count1),period(count2),a7(count1),a7(count2),
     +             specT,a7T,iflag)
      call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +             specT,a8T,iflag)
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +             specT,a9T,iflag)

 1011 period1 = specT

C     Set Constant Terms
      Mref1 = 5.5
      Mref2 = 6.5

C     Compute the ground motion for the given spectral period.
      if (M .lt. Mref1) then
         lnY = a0T - a4T*(8.5-Mref1)**2.0 + a2T*(M-Mref1) + a1T*(Mref1-Mref2) + a8T*Rjb
     1             + (a5T+a6T*(Mref1-Mref2))*alog(sqrt(a7T*a7T+Rjb*Rjb))
      elseif (M .ge. Mref2) then
         lnY = a0T - a4T*(8.5-M)**2.0     + a3T*(M-Mref2)                     + a8T*Rjb
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rjb*Rjb))
      else
         lnY = a0T - a4T*(8.5-M)**2.0     + a1T*(M-Mref2)                     + a8T*Rjb
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rjb*Rjb))
      endif

C     Add Ztor term
      lnY = lnY + (a9T*ztor)/20.0

      sigma = 0.65

C     Convert ground motion to units of gals in natural log units.
      lnY = lnY + 4.6052

      period2 = period1

      return
      end

c----------------------------------------------------------------------
      subroutine S02_PVNGS_Common007 ( m, Rjb, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=5)
      REAL Period(MAXPER), a0(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), a7(MAXPER), a8(MAXPER), a9(MAXPER)
      real Mref1, MRef2, Rjb, Ztor
      REAL M, specT, sigma, period1, period2, lnY
      REAL a0T, a1T, a2T, a3T, a4T, a5T, a6T, a7T, a8T, a9T
      integer iflag, count1, count2, nPer, i

      data period / 0.01, 0.2, 0.5, 1.0, 2.0 /
      data a0 / 4.844064933, 4.219255167, 3.534385513, 2.390354801, 1.425024701  /
      data a1 / -0.013603504, 1.979574394, 1.30292675, 1.497138325, 1.614937219  /
      data a2 / 1.202808487, 1.433566727, 1.740947454, 2.051187409, 2.385760131  /
      data a3 / -0.35672112, 1.183323761, 0.799970466, 0.870581461, 0.817881028  /
      data a4 / 0.062790933, -0.323330263, -0.2067445, -0.202118326, -0.156815452  /
      data a5 / -1.583046448, -1.617499558, -1.355705253, -1.187049421, -1.075971196  /
      data a6 / 0.265085563, 0.209670811, 0.240965815, 0.247088498, 0.284635387  /
      data a7 / 6.825562193, 7.368990249, 6.643172818, 6.268347176, 6.622314026  /
      data a8 / 0.02113002, 0.013027428, 0.016359414, 0.005601114, 0.022644438  /
      data a9 / 1.120855977, 0.984636607, 0.582038379, 0.381242215, 0.172633025  /

C Find the requested spectral period and corresponding coefficients
      nPer = 5
C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1  = period(1)
         a0T      = a0(1)
         a1T      = a1(1)
         a2T      = a2(1)
         a3T      = a3(1)
         a4T      = a4(1)
         a5T      = a5(1)
         a6T      = a6(1)
         a7T      = a7(1)
         a8T      = a8(1)
         a9T      = a9(1)
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'PVNGS Common Model007 Horizontal'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),a0(count1),a0(count2),
     +             specT,a0T,iflag)
      call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +             specT,a1T,iflag)
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +             specT,a2T,iflag)
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +             specT,a3T,iflag)
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +             specT,a4T,iflag)
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +             specT,a5T,iflag)
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +             specT,a6T,iflag)
      call S24_interp (period(count1),period(count2),a7(count1),a7(count2),
     +             specT,a7T,iflag)
      call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +             specT,a8T,iflag)
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +             specT,a9T,iflag)

 1011 period1 = specT

C     Set Constant Terms
      Mref1 = 5.5
      Mref2 = 6.5

C     Compute the ground motion for the given spectral period.
      if (M .lt. Mref1) then
         lnY = a0T - a4T*(8.5-Mref1)**2.0 + a2T*(M-Mref1) + a1T*(Mref1-Mref2) + a8T*Rjb
     1             + (a5T+a6T*(Mref1-Mref2))*alog(sqrt(a7T*a7T+Rjb*Rjb))
      elseif (M .ge. Mref2) then
         lnY = a0T - a4T*(8.5-M)**2.0     + a3T*(M-Mref2)                     + a8T*Rjb
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rjb*Rjb))
      else
         lnY = a0T - a4T*(8.5-M)**2.0     + a1T*(M-Mref2)                     + a8T*Rjb
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rjb*Rjb))
      endif

C     Add Ztor term
      lnY = lnY + (a9T*ztor)/20.0

      sigma = 0.65

C     Convert ground motion to units of gals in natural log units.
      lnY = lnY + 4.6052

      period2 = period1

      return
      end

c----------------------------------------------------------------------
      subroutine S02_PVNGS_Common008 ( m, Rjb, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=5)
      REAL Period(MAXPER), a0(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), a7(MAXPER), a8(MAXPER), a9(MAXPER)
      real Mref1, MRef2, Rjb, Ztor, M, specT, sigma
      REAL a0T, a1T, a2T, a3T, a4T, a5T, a6T, a7T, a8T, a9T
      REAL period1, period2, lnY
      integer iflag, count1, count2, nPer, i

      data period / 0.01, 0.2, 0.5, 1.0, 2.0 /
      data a0 / 5.196560414, 6.3779206, 6.168012597, 5.517927695, 4.200672553  /
      data a1 / -0.958403233, -1.190198693, -1.180296464, -0.679274321, -0.548498253  /
      data a2 / 1.443974775, 1.496178834, 1.850791466, 2.233309216, 2.488066584  /
      data a3 / -0.916984325, -1.007081241, -1.132443605, -0.927758145, -0.977157301  /
      data a4 / 0.144335529, 0.282672806, 0.362298721, 0.349558379, 0.330438663  /
      data a5 / -1.653295478, -1.560264395, -1.579320126, -1.609819791, -1.421728556  /
      data a6 / 0.295382242, 0.176595264, 0.160706414, 0.134372763, 0.184743563  /
      data a7 / 6.458555302, 6.77949456, 6.042271361, 5.885029003, 6.302790569  /
      data a8 / 0.011468897, 0.004122242, 0.01291962, 0.022350743, 0.024469094  /
      data a9 / 0.742172335, 0.639057402, 0.56151272, 0.548360683, 0.431827383  /

C Find the requested spectral period and corresponding coefficients
      nPer = 5
C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1  = period(1)
         a0T      = a0(1)
         a1T      = a1(1)
         a2T      = a2(1)
         a3T      = a3(1)
         a4T      = a4(1)
         a5T      = a5(1)
         a6T      = a6(1)
         a7T      = a7(1)
         a8T      = a8(1)
         a9T      = a9(1)
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'PVNGS Common Model008 Horizontal'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),a0(count1),a0(count2),
     +             specT,a0T,iflag)
      call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +             specT,a1T,iflag)
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +             specT,a2T,iflag)
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +             specT,a3T,iflag)
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +             specT,a4T,iflag)
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +             specT,a5T,iflag)
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +             specT,a6T,iflag)
      call S24_interp (period(count1),period(count2),a7(count1),a7(count2),
     +             specT,a7T,iflag)
      call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +             specT,a8T,iflag)
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +             specT,a9T,iflag)

 1011 period1 = specT

C     Set Constant Terms
      Mref1 = 5.5
      Mref2 = 6.5

C     Compute the ground motion for the given spectral period.
      if (M .lt. Mref1) then
         lnY = a0T - a4T*(8.5-Mref1)**2.0 + a2T*(M-Mref1) + a1T*(Mref1-Mref2) + a8T*Rjb
     1             + (a5T+a6T*(Mref1-Mref2))*alog(sqrt(a7T*a7T+Rjb*Rjb))
      elseif (M .ge. Mref2) then
         lnY = a0T - a4T*(8.5-M)**2.0     + a3T*(M-Mref2)                     + a8T*Rjb
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rjb*Rjb))
      else
         lnY = a0T - a4T*(8.5-M)**2.0     + a1T*(M-Mref2)                     + a8T*Rjb
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rjb*Rjb))
      endif

C     Add Ztor term
      lnY = lnY + (a9T*ztor)/20.0

      sigma = 0.65

C     Convert ground motion to units of gals in natural log units.
      lnY = lnY + 4.6052

      period2 = period1

      return
      end

c----------------------------------------------------------------------
      subroutine S02_PVNGS_Common009 ( m, Rjb, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=5)
      REAL Period(MAXPER), a0(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), a7(MAXPER), a8(MAXPER), a9(MAXPER)
      real Mref1, MRef2, Rjb, Ztor
      REAL M, specT, sigma, period1, period2, lnY
      REAL a0T, a1T, a2T, a3T, a4T, a5T, a6T, a7T, a8T, a9T
      integer iflag, count1, count2, nPer, i

      data period / 0.01, 0.2, 0.5, 1.0, 2.0 /
      data a0 / 5.049167566, 6.676980195, 5.593480621, 4.911257887, 4.152761337  /
      data a1 / -0.379786019, -1.051580715, -0.963016583, -1.687190866, -1.897015157  /
      data a2 / 0.872279072, 0.878938183, 1.402118772, 1.766470995, 2.062788978  /
      data a3 / -0.515217831, -1.07005441, -0.961301327, -1.502136917, -1.645952495  /
      data a4 / 0.016302262, 0.133263985, 0.178512293, 0.384619868, 0.508737639  /
      data a5 / -1.835895612, -1.910217102, -1.627402821, -1.36275724, -1.297798013  /
      data a6 / 0.399850156, 0.419788271, 0.373119179, 0.357938274, 0.332741067  /
      data a7 / 7.807505344, 7.524346005, 7.897750626, 8.092374624, 7.952854135  /
      data a8 / 0.016429565, 0.01295455, 0.008712308, 0.010008655, 0.023976474  /
      data a9 / 0.48894144, 0.554127648, 0.526560212, 0.495922051, 0.562533157  /
C Find the requested spectral period and corresponding coefficients
      nPer = 5
C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1  = period(1)
         a0T      = a0(1)
         a1T      = a1(1)
         a2T      = a2(1)
         a3T      = a3(1)
         a4T      = a4(1)
         a5T      = a5(1)
         a6T      = a6(1)
         a7T      = a7(1)
         a8T      = a8(1)
         a9T      = a9(1)
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'PVNGS Common Model009 Horizontal'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),a0(count1),a0(count2),
     +             specT,a0T,iflag)
      call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +             specT,a1T,iflag)
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +             specT,a2T,iflag)
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +             specT,a3T,iflag)
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +             specT,a4T,iflag)
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +             specT,a5T,iflag)
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +             specT,a6T,iflag)
      call S24_interp (period(count1),period(count2),a7(count1),a7(count2),
     +             specT,a7T,iflag)
      call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +             specT,a8T,iflag)
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +             specT,a9T,iflag)

 1011 period1 = specT

C     Set Constant Terms
      Mref1 = 5.5
      Mref2 = 6.5

C     Compute the ground motion for the given spectral period.
      if (M .lt. Mref1) then
         lnY = a0T - a4T*(8.5-Mref1)**2.0 + a2T*(M-Mref1) + a1T*(Mref1-Mref2) + a8T*Rjb
     1             + (a5T+a6T*(Mref1-Mref2))*alog(sqrt(a7T*a7T+Rjb*Rjb))
      elseif (M .ge. Mref2) then
         lnY = a0T - a4T*(8.5-M)**2.0     + a3T*(M-Mref2)                     + a8T*Rjb
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rjb*Rjb))
      else
         lnY = a0T - a4T*(8.5-M)**2.0     + a1T*(M-Mref2)                     + a8T*Rjb
     1             + (a5T+a6T*(M-Mref2))*alog(sqrt(a7T*a7T+Rjb*Rjb))
      endif

C     Add Ztor term
      lnY = lnY + (a9T*ztor)/20.0

      sigma = 0.65

C     Convert ground motion to units of gals in natural log units.
      lnY = lnY + 4.6052

      period2 = period1

      return
      end



c----------------------------------------------------------------------
C     SWUS Common Functional Model as a function of Rrup distance.
C     Coefficients are passed from input file and are only applicable for
C     the spectral period specified in the input file.

      subroutine S02_SWUS_CFRrup ( m, Rrup, Rjb, ztor, ftype, dip, Width, Rx, HWFlag,
     1                   specT, lnY, sigma, iflag, cfcoefrrup, coefcountRrup, phi, tau )

      implicit none
      include 'pfrisk.h'

      REAL a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10
      real cfcoefRrup(MAX_Atten,11)
      REAL M, specT, sigma, Rrup, Ztor, lnY, termmag, ftype
      real Rjb, dip, Width, Rx, HWFactor, phi, tau
      integer iflag, coefcountRrup, HWFlag

C     Set the Coefficients.
      a0 = cfcoefRrup(coefcountRrup,1)
      a1 = cfcoefRrup(coefcountRrup,2)
      a2 = cfcoefRrup(coefcountRrup,3)
      a3 = cfcoefRrup(coefcountRrup,4)
      a4 = cfcoefRrup(coefcountRrup,5)
      a5 = cfcoefRrup(coefcountRrup,6)
      a6 = cfcoefRrup(coefcountRrup,7)
      a7 = cfcoefRrup(coefcountRrup,8)
      a8 = cfcoefRrup(coefcountRrup,9)
      a9 = cfcoefRrup(coefcountRrup,10)
      a10 = cfcoefRrup(coefcountRrup,11)

C     Compute the ground motion.
C     First compute magnitude scaling.
      if (m .lt. 5.5) then
         termmag = a1*(5.5-6.5) + a2*(m-5.5)
      elseif (m .ge. 6.5) then
         termmag = a3*(m-6.5)
      else
         termmag = a1*(m-6.5)
      endif

C     Compute the Hanging Wall term if needed.
      if (HWFlag .eq. 1) then
         call S02_SWUS_HWModel (M, Rrup, Rjb, Rx, Dip, Width, a10, ztor, specT, HWfactor)
      else
         HWFactor = 0.0
      endif

      lnY = a0 - a7*a7*Rrup + a8*a8*Ztor + a9*a9*ftype + (a4 + a5*(m-5.0))*alog(sqrt(Rrup*Rrup+a6*a6)) +
     1       termmag + HWFactor

C     Fixed assigned sigma values.
      phi = 0.54
      tau = 0.36
      sigma = 0.65

C     Convert ground motion to units of gals in natural log units.
      lnY = lnY + 6.89
      iflag = 0

      return
      end


c----------------------------------------------------------------------
C     SWUS Common Functional Model as a function of Rjb distance.
C     Coefficients are passed from input file and are only applicable for
C     the spectral period specified in the input file.

      subroutine S02_SWUS_CFRjb ( m, Rrup, Rjb, ztor, ftype, dip, Width, Rx, HWFlag,
     1                   specT, lnY, sigma, iflag, cfcoefrjb, coefcountRjb, phi, tau )

      implicit none
      include 'pfrisk.h'

      REAL a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10
      real cfcoefRjb(MAX_ATTEN,11)
      REAL M, specT, sigma, Rrup, Ztor, lnY, termmag, ftype
      real Rjb, dip, Width, Rx, HWFactor, phi, tau
      integer iflag, coefcountRjb, HWFlag

C     Set the Coefficients.
      a0 = cfcoefRjb(coefcountRjb,1)
      a1 = cfcoefRjb(coefcountRjb,2)
      a2 = cfcoefRjb(coefcountRjb,3)
      a3 = cfcoefRjb(coefcountRjb,4)
      a4 = cfcoefRjb(coefcountRjb,5)
      a5 = cfcoefRjb(coefcountRjb,6)
      a6 = cfcoefRjb(coefcountRjb,7)
      a7 = cfcoefRjb(coefcountRjb,8)
      a8 = cfcoefRjb(coefcountRjb,9)
      a9 = cfcoefRjb(coefcountRjb,10)
      a10 = cfcoefRjb(coefcountRjb,11)

C     Compute the ground motion.
C     First compute magnitude scaling.
      if (m .lt. 5.5) then
         termmag = a1*(5.5-6.5) + a2*(m-5.5)
      elseif (m .ge. 6.5) then
         termmag = a3*(m-6.5)
      else
         termmag = a1*(m-6.5)
      endif

C     Compute the Hanging Wall term if needed.
      if (HWFlag .eq. 1) then
         call S02_SWUS_HWModel (M, Rrup, Rjb, Rx, Dip, Width, a10, ztor, specT, HWfactor)
      else
         HWFactor = 0.0
      endif

      lnY = a0 - a7*a7*Rjb + a8*a8*Ztor + a9*a9*ftype + (a4 + a5*(m-5.0))*alog(sqrt(Rjb*Rjb+a6*a6)) +
     1       termmag + HWFactor
C     Fixed assigned sigma value.
      phi = 0.54
      tau = 0.36
      sigma = 0.65

C     Convert ground motion to units of gals in natural log units.
      lnY = lnY + 6.89
      iflag = 0

      return
      end


c----------------------------------------------------------------------
C     SWUS DCPP Common Functional Model as a function of Rrup distance.
C     Coefficients are passed from input file and are only applicable for
C     the spectral period specified in the input file.
C     This version includes the fault mechanism term consistent for DCPP.

      Subroutine S02_SWUS_CFRrup_DCPP ( m, Rrup, Rjb, ztor, ftype, dip, Width, Rx, HWFlag,
     1                   specT, lnY, sigma, iflag, cfcoefrrup, coefcountRrup, phi, tau )

      implicit none
      include 'pfrisk.h'

      REAL a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10
      real cfcoefRrup(MAX_Atten,11)
      REAL M, specT, sigma, Rrup, Ztor, lnY, termmag, ftype
      real Rjb, dip, Width, Rx, HWFactor, phi, tau
      integer iflag, coefcountRrup, HWFlag, count1, count2, nper, i
      real coefMech(22), period(22), period1, coefMechT, termmech

      data period / 0.0, 0.01, 0.02, 0.03, 0.05, 0.075, 0.1, 0.15, 0.20, 0.25, 0.30,
     1              0.4, 0.5, 0.75, 1.00, 1.5, 2.0, 4.0, 4.0, 5.0, 7.50, 10.0 /
      data coefMech / -0.132, -0.132, -0.132, -0.132, -0.132, -0.132, -0.132, -0.132, -0.122, -0.113, -0.104,
     1              -0.095, -0.095, -0.086, -0.077, -0.068, -0.058, -0.039, -0.02, 0.00, 0.00, 0.00 /

C Find the requested spectral period and corresponding coefficients
      nPer = 22
C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1    = period(1)
         coefMechT  = coefMech(1)
         goto 1011
      else
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),coefMech(count1),coefMech(count2),
     +             specT,coefMechT,iflag)

 1011 continue

C     Set the Coefficients.
      a0 = cfcoefRrup(coefcountRrup,1)
      a1 = cfcoefRrup(coefcountRrup,2)
      a2 = cfcoefRrup(coefcountRrup,3)
      a3 = cfcoefRrup(coefcountRrup,4)
      a4 = cfcoefRrup(coefcountRrup,5)
      a5 = cfcoefRrup(coefcountRrup,6)
      a6 = cfcoefRrup(coefcountRrup,7)
      a7 = cfcoefRrup(coefcountRrup,8)
      a8 = cfcoefRrup(coefcountRrup,9)
      a9 = cfcoefRrup(coefcountRrup,10)
      a10 = cfcoefRrup(coefcountRrup,11)

C     Compute the ground motion.
C     First compute magnitude scaling.
      if (m .lt. 5.5) then
         termmag = a1*(5.5-6.5) + a2*(m-5.5)
      elseif (m .ge. 6.5) then
         termmag = a3*(m-6.5)
      else
         termmag = a1*(m-6.5)
      endif

C     Compute the Hanging Wall term if needed.
      if (HWFlag .eq. 1) then
         call S02_SWUS_HWModel (M, Rrup, Rjb, Rx, Dip, Width, a10, ztor, specT, HWfactor)
      else
         HWFactor = 0.0
      endif

C     Now for DCPP apply mechanism adjustments
C     Reverse Fault (ftype=1)
      if (ftype .eq. 1.0) then
         termmech = a9*a9
C     Normal Fault (ftype=-1)
      elseif (ftype .eq. -1.0) then
         termmech = coefMechT
C     Strike-slip Fault (ftype=0)
      else
         termmech = 0.0
      endif

c      if (ftype .eq. -1.0) then
c         termmech = CoefmechT
c      else
c         termmech = 0.0
c      endif

      lnY = a0 - a7*a7*Rrup + a8*a8*Ztor + (a4 + a5*(m-5.0))*alog(sqrt(Rrup*Rrup+a6*a6)) +
     1       termmag + HWFactor + termmech

C     Fixed assigned sigma values.
      phi = 0.54
      tau = 0.36
      sigma = 0.65

C     Convert ground motion to units of gals in natural log units.
      lnY = lnY + 6.89
      iflag = 0

      return
      end

c----------------------------------------------------------------------
      subroutine S02_SWUS_HWModel ( m, Rrup, Rjb, Rx, Dip, Width, a10, ztor, specT, HWFactor )

      implicit none

      integer MAXPER
      parameter (MAXPER=22)
      REAL Period(MAXPER), c11(MAXPER), c12(MAXPER), c13(MAXPER), c14(MAXPER), c15(MAXPER)
      REAL c1, c2(MAXPER), c3(MAXPER), c4(MAXPER)
      real M, Rrup, Rjb, Rx, Dip, Width, specT, HWFactor, period1, ztor
      real c11T, c12T, c13T, c14T, c15T, c2T, c3T, c4T, a10, magtaper, disttaper, ztorTaper
      integer count1, count2, nPer, i, iflag

      data Period  /  0, 0.01, 0.02, 0.03, 0.05, 0.075, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4,
     1         0.5, 0.75, 1, 1.5, 2, 3, 4, 5, 7.5, 10  /
      data c11  /  0.868, 0.868, 0.867, 0.856, 0.84, 0.857, 0.848, 0.868, 0.85, 0.868, 0.839,
     1         0.78, 0.741, 0.613, 0.621, 0.506, 0.391, 0.128, 0, 0, 0, 0  /
      data c12  /  0.982, 0.982, 0.987, 0.997, 1.027, 1.041, 1.04, 1.009, 1.005, 0.985, 0.974,
     1         0.934, 0.902, 0.869, 0.788, 0.662, 0.537, 0.245, 0.034, 0, 0, 0  /
      data c13  /  1.038, 1.038, 1.046, 1.067, 1.121, 1.133, 1.135, 1.08, 1.082, 1.044, 1.041,
     1         1.011, 0.982, 0.997, 0.872, 0.74, 0.609, 0.304, 0.088, 0, 0, 0  /
      data c14  /  1.095, 1.095, 1.106, 1.138, 1.215, 1.226, 1.231, 1.15, 1.16, 1.102, 1.108,
     1         1.089, 1.063, 1.125, 0.955, 0.818, 0.682, 0.362, 0.138, 0, 0, 0  /
      data c15  /  1.209, 1.209, 1.226, 1.278, 1.402, 1.41, 1.422, 1.292, 1.315, 1.219, 1.242,
     1         1.243, 1.223, 1.38, 1.123, 0.974, 0.828, 0.48, 0.231, 0.04, 0, 0  /
      data c2  /  0.216, 0.216, 0.2172, 0.2178, 0.2199, 0.2218, 0.2213, 0.2169, 0.2131, 0.1988,
     1         0.2019, 0.209, 0.2053, 0.1713, 0.1571, 0.1559, 0.1559, 0.1616, 0.1616, 0.1616,
     1         0.1616, 0.1616  /
      data c3  /  2.0289, 2.0289, 2.026, 2.0163, 1.987, 1.9906, 1.9974, 2.0162, 1.9746, 1.9931,
     1         2.0179, 2.0249, 2.0041, 1.8697, 1.8526, 1.8336, 1.7996, 1.674, 1.674, 1.674,
     1         1.674, 1.674  /
      data c4  /  0.1675, 0.1675, 0.1666, 0.167, 0.1699, 0.1817, 0.1717, 0.1814, 0.1834,
     1         0.1767, 0.1658, 0.1624, 0.1719, 0.1866, 0.3143, 0.3195, 0.3246, 0.3314, 0.3314,
     1         0.3314, 0.3314, 0.3314  /

C Find the requested spectral period and corresponding coefficients
      nPer = 22
C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1  = period(1)
         c11T      = c11(1)
         c12T      = c12(1)
         c13T      = c13(1)
         c14T      = c14(1)
         c15T      = c15(1)
         c2T       = c2(1)
         c3T       = c3(1)
         c4T       = c4(1)
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the SWUS Hanging Wall Model.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'SWUS Hanging Wall Model'
      write (*,*) 'is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),c11(count1),c11(count2),
     +             specT,c11T,iflag)
      call S24_interp (period(count1),period(count2),c12(count1),c12(count2),
     +             specT,c12T,iflag)
      call S24_interp (period(count1),period(count2),c13(count1),c13(count2),
     +             specT,c13T,iflag)
      call S24_interp (period(count1),period(count2),c14(count1),c14(count2),
     +             specT,c14T,iflag)
      call S24_interp (period(count1),period(count2),c14(count1),c15(count2),
     +             specT,c15T,iflag)
      call S24_interp (period(count1),period(count2),c2(count1),c2(count2),
     +             specT,c2T,iflag)
      call S24_interp (period(count1),period(count2),c3(count1),c3(count2),
     +             specT,c3T,iflag)
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),
     +             specT,c4T,iflag)

 1011 period1 = specT

C     Compute the Hanging Wall Factor.
C     First set the C1 term based on the a10 coefficient
      if (a10 .eq. 1.0) then
         c1 = c11T
      elseif (a10 .eq. 2.0) then
         c1 = c12T
      elseif (a10 .eq. 3.0) then
         c1 = c13T
      elseif (a10 .eq. 4.0) then
         c1 = c14T
      elseif (a10 .eq. 5.0) then
         c1 = c15T
      else
         HWFactor = 0.0
         return
      endif

C     Compute HW Factor.
      magtaper = (1.0 + c4T*(M-7.0))
      disttaper = (1.0 - Rjb/(Rrup+0.1))
      ztortaper = max(0.0,1.0-ztor/12.0)

      HWFactor = c1*cos(dip*3.14159/180.0) * (c2T + (1.0 - c2T)*tanh(c3T*Rx/(width*cos(dip*3.14159/180.0)))) *
     1        magtaper * disttaper * ztorTaper

      return
      end

c ---------------------------------------------------------------------
C     *** Weatherill and Cotton (2020), ESHM20 Craton ***
C   Weatherill, G. and F. Cotton (2020). A ground motion logic tree for seismic
C     hazard analysis in the stable cratonic region of Europe: regionalisation,
C     model selection and development of a scaled backbone approach,
C     Bull Earthquake Eng 18:6119-6148
C   Coefficients are from Table 1 of Weatherill and Cotton (2020)
C     Notes:
C        Median for the reference hard-rock condition, VS30 = 3000 m/s
C           (use jcalc 1242 to add the Stewart et al. 2020 / Hashash et al. 2020
C           site amplification for other VS30)
C        Distance metric is Rrup
C        Returns the median only; use with a sigma model (e.g., NGA-East 7101 - 7106)
c ---------------------------------------------------------------------

      subroutine S02_ESHM2020_Craton ( m, Rrup, specT, period2, lnY, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=24)
      real period(MAXPER), e1(MAXPER), b1(MAXPER), b2(MAXPER)
      real b3(MAXPER), c1(MAXPER), c2(MAXPER), c3(MAXPER)
      real sigma_mu(MAXPER)
      integer count1, count2, iflag, nPer, i
      real e1T, b1T, b2T, b3T, c1T, c2T, c3T, sigma_muT
      real specT, period1, period2, LNY
      real m, Rrup, Mref, Rref, Mh, h
      real fRg, fRa, fM

      data period / 0.00, 0.01, 0.02, 0.025, 0.03, 0.04, 0.05, 0.075,
     1              0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.75, 1.0,
     2              1.5, 2.0, 3.0, 4.0, 5.0, 7.5, 10.0 /
      data e1 / 0.129434, 0.441910, 0.979124, 1.043341, 1.046568,
     1          1.007663, 0.951569, 0.766899, 0.566921, 0.316925,
     2          0.116889, -0.043842, -0.198477, -0.441747, -0.637445,
     3          -1.032362, -1.372803, -1.888467, -2.334523, -3.034920,
     4          -3.576616, -4.022629, -4.876431, -5.489149 /
      data b1 / 0.516399, 0.507166, 0.464490, 0.469671, 0.476295,
     1          0.493810, 0.507031, 0.537818, 0.563265, 0.627618,
     2          0.691137, 0.744830, 0.799805, 0.897281, 0.992673,
     3          1.237960, 1.445804, 1.730211, 1.920451, 2.146848,
     4          2.262688, 2.318744, 2.373219, 2.381481 /
      data b2 / -0.120322, -0.101880, -0.113773, -0.113451, -0.114530,
     1          -0.115011, -0.117000, -0.125793, -0.139089, -0.168968,
     2          -0.191139, -0.208516, -0.223155, -0.242205, -0.253909,
     3          -0.248353, -0.229116, -0.193720, -0.161746, -0.114822,
     4          -0.088526, -0.077704, -0.064599, -0.063354 /
      data b3 / 0.209373, 0.184282, 0.167234, 0.174066, 0.188789,
     1          0.208536, 0.227663, 0.255898, 0.285966, 0.338415,
     2          0.377390, 0.406489, 0.433866, 0.483912, 0.526939,
     3          0.613138, 0.691619, 0.805619, 0.908051, 1.085141,
     4          1.227766, 1.346637, 1.529693, 1.620020 /
      data c1 / -1.498201, -1.567538, -1.628256, -1.609088, -1.578345,
     1          -1.522322, -1.476123, -1.390136, -1.329051, -1.252120,
     2          -1.205866, -1.183521, -1.165570, -1.151567, -1.144198,
     3          -1.127283, -1.109474, -1.102390, -1.094766, -1.090842,
     4          -1.090290, -1.090249, -1.107500, -1.127404 /
      data c2 / 0.220432, 0.222961, 0.226151, 0.224104, 0.220698,
     1          0.215223, 0.210021, 0.198935, 0.189119, 0.167802,
     2          0.154400, 0.146981, 0.140633, 0.133979, 0.129944,
     3          0.121478, 0.116811, 0.114304, 0.113859, 0.115717,
     4          0.117770, 0.118983, 0.131643, 0.141292 /
      data c3 / -0.219311, -0.217385, -0.244152, -0.257668, -0.270013,
     1          -0.287477, -0.298269, -0.306253, -0.296371, -0.266500,
     2          -0.236540, -0.208303, -0.179797, -0.136251, -0.112135,
     3          -0.073566, -0.058351, -0.039000, -0.029689, -0.019806,
     4          -0.013579, -0.008330, -0.000049, 0.005956 /
      data sigma_mu / 0.467518, 0.424145, 0.453414, 0.456276, 0.442618,
     1                0.432693, 0.436895, 0.445049, 0.445057, 0.408938,
     2                0.396718, 0.385803, 0.386776, 0.395065, 0.416677,
     3                0.424884, 0.435249, 0.494395, 0.529657, 0.550852,
     4                0.547912, 0.536941, 0.531853, 0.560199 /


C Find the requested spectral period and corresponding coefficients
      nPer = 24


C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1 = period(1)
         e1T = e1(1)
         b1T = b1(1)
         b2T = b2(1)
         b3T = b3(1)
         c1T = c1(1)
         c2T = c2(1)
         c3T = c3(1)
         sigma_muT = sigma_mu(1)
         goto 1011

      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'ESHM (2020)_Craton'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020       call S24_interp (period(count1),period(count2),e1(count1),e1(count2),
     +                   specT,e1T,iflag)
            call S24_interp (period(count1),period(count2),b1(count1),b1(count2),
     +                   specT,b1T,iflag)
            call S24_interp (period(count1),period(count2),b2(count1),b2(count2),
     +                   specT,b2T,iflag)
            call S24_interp (period(count1),period(count2),b3(count1),b3(count2),
     +                   specT,b3T,iflag)
            call S24_interp (period(count1),period(count2),c1(count1),c1(count2),
     +                   specT,c1T,iflag)
            call S24_interp (period(count1),period(count2),c2(count1),c2(count2),
     +                   specT,c2T,iflag)
            call S24_interp (period(count1),period(count2),c3(count1),c3(count2),
     +                   specT,c3T,iflag)
            call S24_interp (period(count1),period(count2),sigma_mu(count1),sigma_mu(count2),
     +                   specT,sigma_muT,iflag)


 1011 period1 = specT

c     Set constants
      Mref = 4.5
      Rref = 1.0
      Mh = 6.2
      h = 5.0

c     Calculate magnitude scaling term (fM)
      if (m .le. Mh) then
         fM = b1T * (m - Mh) + b2T * ((m - Mh)**2.)
      else
         fM = b3T * (m - Mh)
      endif

c     Calculate distance scaling terms
      fRg = (c1T + c2T * (m - Mref)) * alog(sqrt(((Rrup**2.)+(h**2.))/ ((Rref**2.)+(h**2.))))
      fRa = (c3T/100)*((sqrt((Rrup**2.)+(h**2.)))-(sqrt((Rref**2.)+(h**2.))))

c     Compute median ground motion
      lnY = e1T + fM + fRg + fRa

C     Convert ground motion to units of gals
      lnY = lnY + 6.89

      period1 = specT
      period2 = period1

      return
      end

c ---------------------------------------------------------------------
C     *** Allen (2022), Far-field North Australian Craton ***
C   Allen, T. I. (2022). A far-field ground-motion model for the North
C     Australian Craton from plate-margin earthquakes, Bull. Seismol. Soc. Am.
C     112, 1041-1059
C   Coefficients are from Table 1 of Allen (2022)
C     Notes:
C        Developed for sites in the North Australian Craton from
C           subduction interface and intraslab earthquakes in the Banda Sea
C        Applicable range: M <= 8.0, 500 km <= Rhypo <= 1500 km
C        Distance metric is Rhypo; also depends on the hypocentral depth
C           (truncated to 10 - 500 km, as recommended in the paper)
C        Site term is defined for 220 <= VS30 <= 800 m/s
C        Defined for PGV (specT = -1), PGA, and 0.1 - 10 sec
C        Sigma for PGA includes the lambda_PGA term (Table 1, Eq. 10)
c ---------------------------------------------------------------------

      subroutine S02_Allen_2022 ( m, Rhyp, vs, Zhyp, specT, period2, lnY,
     1                            sigma, phiT, tauT, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=22)
      real period(MAXPER), c0(MAXPER), c1(MAXPER), c2(MAXPER), c3(MAXPER)
      real c4(MAXPER), d0(MAXPER), d1(MAXPER), d2(MAXPER), d3(MAXPER)
      real n0(MAXPER), s0(MAXPER), s1(MAXPER)
      real tau(MAXPER), phi(MAXPER), sig(MAXPER)
      integer count1, count2, iflag, nPer, i
      real c0T, c1T, c2T, c3T, c4T, d0T, d1T, d2T, d3T
      real n0T, s0T, s1T, tauT, phiT, sigT
      real specT, period1, period2, lnY, sigma
      real m, Rhyp, vs, Zhyp, Rx, hz
      real fR, fM, fD, fN, fS

      data period / -1.0, 0.0, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4,
     1              0.5, 0.6, 0.75, 1.0, 1.25, 1.5, 2.0, 2.5,
     2              3.0, 4.0, 5.0, 6.0, 7.5, 10.0 /
      data c0 / 5.1394, -4.5508, -5.3000, -5.2557, -5.1597, -5.0958, -4.9242,
     1          -4.2365, -1.6262, 0.1424, 1.8210, 3.4777, 4.7348, 5.6353,
     2          6.0524, 5.7973, 5.4552, 4.8483, 4.1446, 3.4946, 2.8095,
     3          2.0278 /
      data c1 / 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000,
     1          -0.0213, -0.0192, -0.0670, -0.1674, -0.2088, -0.1776, -0.2334,
     2          -0.3545, -0.3619, -0.3484, -0.3590, -0.3549, -0.3247, -0.3034,
     3          -0.3459 /
      data c2 / 1.9713, 1.6129, 1.4672, 1.4638, 1.4603, 1.4502, 1.4988,
     1          1.6047, 1.6542, 1.7420, 1.9003, 1.9943, 2.1150, 2.2245,
     2          2.4366, 2.4976, 2.5378, 2.6446, 2.7324, 2.7833, 2.7654,
     3          2.7722 /
      data c3 / -2.6644, -0.4923, 0.0000, 0.0456, 0.0284, -0.0069, -0.0897,
     1          -0.3716, -1.4501, -2.2296, -3.0099, -3.8107, -4.4348, -4.8971,
     2          -5.2236, -5.2934, -5.3066, -5.2890, -5.1969, -5.1162, -5.0636,
     3          -5.0168 /
      data c4 / -7.336, -10.167, -10.513, -10.580, -10.382, -10.080, -9.920,
     1          -9.728, -8.792, -7.931, -6.912, -6.044, -5.403, -4.781,
     2          -4.348, -4.005, -3.734, -3.656, -3.636, -3.547, -3.453,
     3          -3.368 /
      data d0 / 5.2107, 4.8787, 4.3555, 5.7681, 6.0008, 6.0464, 6.5203,
     1          6.1641, 6.7588, 5.3677, 4.4379, 4.8745, 5.2371, 5.9873,
     2          5.0004, 5.0432, 5.0210, 5.0015, 5.0599, 4.6695, 4.7249,
     3          5.0737 /
      data d1 / -1.8808, -2.5207, -2.7260, -2.9707, -3.0078, -3.0019, -3.0348,
     1          -2.8040, -2.7780, -2.4344, -2.0824, -1.9619, -1.8821, -1.9196,
     2          -1.5450, -1.4210, -1.3028, -1.1239, -1.0292, -0.9160, -0.8061,
     3          -0.7276 /
      data d2 / 9.459, 12.070, 12.699, 14.147, 14.372, 14.338, 14.583,
     1          13.533, 13.581, 11.765, 10.083, 9.702, 9.451, 9.800,
     2          7.960, 7.407, 6.860, 6.057, 5.628, 5.042, 4.540,
     3          4.187 /
      data d3 / -13.949, -16.388, -16.557, -19.169, -19.594, -19.567, -20.160,
     1          -18.818, -19.326, -16.380, -13.980, -13.938, -13.950, -14.879,
     2          -12.217, -11.637, -11.007, -10.102, -9.653, -8.744, -8.204,
     3          -7.966 /
      data n0 / -1.0181, -1.3651, -2.6602, -1.2049, -1.2446, -1.3811, -1.4749,
     1          -1.5512, -1.0315, -1.1632, -1.0232, -1.0307, -0.8072, -0.9129,
     2          -0.6695, -0.6849, -0.6495, -0.4710, -0.5199, -0.5526, -0.5132,
     3          -0.2645 /
      data s0 / -0.5880, -1.0254, -1.0022, -0.9190, -0.8846, -0.8669, -0.7987,
     1          -0.7893, -0.6889, -0.6341, -0.7019, -0.6230, -0.5700, -0.5587,
     2          -0.4756, -0.4486, -0.4330, -0.3915, -0.4377, -0.4253, -0.4327,
     3          -0.4564 /
      data s1 / 0.2536, 0.4008, 0.4258, 0.4239, 0.4066, 0.3822, 0.3619,
     1          0.3403, 0.3084, 0.2919, 0.2781, 0.2654, 0.2509, 0.2342,
     2          0.2230, 0.2183, 0.2095, 0.2042, 0.1985, 0.1910, 0.1820,
     3          0.1736 /
      data tau / 0.6361, 0.7066, 0.7532, 0.7578, 0.7568, 0.7436, 0.7172,
     1           0.7019, 0.7046, 0.6765, 0.6455, 0.6146, 0.6223, 0.6233,
     2           0.5910, 0.5689, 0.5618, 0.6152, 0.6352, 0.6207, 0.5969,
     3           0.5880 /
      data phi / 0.4883, 0.5094, 0.5038, 0.5252, 0.5343, 0.5291, 0.5286,
     1           0.5499, 0.5530, 0.5214, 0.4849, 0.4532, 0.4257, 0.4387,
     2           0.4394, 0.4162, 0.4033, 0.3843, 0.3917, 0.4018, 0.4066,
     3           0.4185 /
      data sig / 0.8019, 0.9118, 0.9061, 0.9220, 0.9264, 0.9127, 0.8910,
     1           0.8917, 0.8957, 0.8541, 0.8073, 0.7636, 0.7540, 0.7622,
     2           0.7365, 0.7049, 0.6915, 0.7254, 0.7463, 0.7394, 0.7222,
     3           0.7217 /

C Find the requested spectral period and corresponding coefficients
      nPer = 22

C First check for the PGV case (i.e., specT=-1.0)
      if (specT .eq. -1.0) then
         period1 = period(1)
         c0T  = c0(1)
         c1T  = c1(1)
         c2T  = c2(1)
         c3T  = c3(1)
         c4T  = c4(1)
         d0T  = d0(1)
         d1T  = d1(1)
         d2T  = d2(1)
         d3T  = d3(1)
         n0T  = n0(1)
         s0T  = s0(1)
         s1T  = s1(1)
         tauT = tau(1)
         phiT = phi(1)
         sigT = sig(1)
         goto 1011

C Check for the PGA case (i.e., specT=0.0)
      elseif (specT .eq. 0.0) then
         period1 = period(2)
         c0T  = c0(2)
         c1T  = c1(2)
         c2T  = c2(2)
         c3T  = c3(2)
         c4T  = c4(2)
         d0T  = d0(2)
         d1T  = d1(2)
         d2T  = d2(2)
         d3T  = d3(2)
         n0T  = n0(2)
         s0T  = s0(2)
         s1T  = s1(2)
         tauT = tau(2)
         phiT = phi(2)
         sigT = sig(2)
         goto 1011

      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=3,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'Allen (2022) Far-field North Australian Craton'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),c0(count1),c0(count2),
     +             specT,c0T,iflag)
      call S24_interp (period(count1),period(count2),c1(count1),c1(count2),
     +             specT,c1T,iflag)
      call S24_interp (period(count1),period(count2),c2(count1),c2(count2),
     +             specT,c2T,iflag)
      call S24_interp (period(count1),period(count2),c3(count1),c3(count2),
     +             specT,c3T,iflag)
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),
     +             specT,c4T,iflag)
      call S24_interp (period(count1),period(count2),d0(count1),d0(count2),
     +             specT,d0T,iflag)
      call S24_interp (period(count1),period(count2),d1(count1),d1(count2),
     +             specT,d1T,iflag)
      call S24_interp (period(count1),period(count2),d2(count1),d2(count2),
     +             specT,d2T,iflag)
      call S24_interp (period(count1),period(count2),d3(count1),d3(count2),
     +             specT,d3T,iflag)
      call S24_interp (period(count1),period(count2),n0(count1),n0(count2),
     +             specT,n0T,iflag)
      call S24_interp (period(count1),period(count2),s0(count1),s0(count2),
     +             specT,s0T,iflag)
      call S24_interp (period(count1),period(count2),s1(count1),s1(count2),
     +             specT,s1T,iflag)
      call S24_interp (period(count1),period(count2),tau(count1),tau(count2),
     +             specT,tauT,iflag)
      call S24_interp (period(count1),period(count2),phi(count1),phi(count2),
     +             specT,phiT,iflag)
      call S24_interp (period(count1),period(count2),sig(count1),sig(count2),
     +             specT,sigT,iflag)

 1011 period1 = specT

C     Check that VS30 is within the range of the site term
      if (vs .lt. 220.0 .or. vs .gt. 800.0) then
         write (*,*)
         write (*,*) 'Allen (2022) Far-field North Australian Craton'
         write (*,*) 'site term is not defined for a VS30 of: '
         write (*,'(a10,f10.2)') ' VS30   = ', vs
         write (*,*) 'The model is defined for 220 <= VS30 <= 800 m/s.'
         write (*,*) 'Please check the input file.'
         write (*,*)
         stop 99
      endif

C     Hinge distance (km)
      Rx = 600.0

C     Distance scaling term (Eq. 2)
      if (Rhyp .le. Rx) then
         fR = c3T*alog10(Rhyp)
      else
         fR = c3T*alog10(Rx) + c4T*(alog10(Rhyp) - alog10(Rx))
      endif

C     Magnitude scaling term (Eq. 3)
      fM = c0T + c1T*(m - 6.0)**2 + c2T*(m - 6.0)

C     Hypocentral depth scaling term (Eq. 4), with the depth truncated to 10 - 500 km
      hz = min(max(Zhyp, 10.0), 500.0)
      fD = d0T + d1T*alog10(hz)**3 + d2T*alog10(hz)**2 + d3T*alog10(hz)

C     Near-source distance correction (Eq. 5)
      if (Rhyp .le. Rx) then
         fN = n0T*(alog10(Rhyp) - alog10(Rx))
      else
         fN = 0.0
      endif

C     Site term (Eq. 6)
      fS = s0T + s1T/(alog10(vs) - alog10(150.0))

C     Median ground motion (Eq. 1), in natural log units of g (cm/s for PGV)
      lnY = fR + fM + fD + fN + fS

C     Total standard deviation (Table 1)
      sigma = sigT

C     Convert ground motion to units of gals.
      lnY = lnY + 6.89

      period2 = period1

      return
      end

c ---------------------------------------------------------------------
C     *** Bayless and Somerville (2024), Australia ***
C   Bayless, J. and P. Somerville (2024). An updated ground motion model for
C     Australia developed using broadband ground motion simulations,
C     Australian Earthquake Engineering Society 2024 National Conference,
C     Adelaide, South Australia
C   The paper does not list the coefficients; they are from the model
C     authors' implementation of the model
C     Notes:
C        Cratonic (CratFlag = 1) and non-Cratonic (CratFlag = 0) models
C        Applicable range: 4.0 <= M <= 8.0, 0 <= Rrup <= 300 km
C        Defined for PGA and 0.01 - 10 sec (PGA is set to the 0.01 sec value)
C        VS30 scaling (linear and nonlinear) and Z1.0 scaling are from
C           Boore et al. (2014), the hanging wall model is from Abrahamson
C           et al. (2014), and the aleatory variability is from Al Atik (2015)
C        Z1.0 scaling is applied only for basinflag = 1; the paper recommends
C           not using it (i.e., using the default Z1.0 for the VS30)
c ---------------------------------------------------------------------

      subroutine S02_BS_2024 ( mag, dip, fltWidth, ztor, rRup, Rx, Ry0,
     1                         vs30, z10, hwflag, basinflag, specT, period2,
     2                         lnY, sigma, phi, tau, iflag, CratFlag )

      implicit none

      real mag, dip, fltWidth, ztor, rRup, Rx, Ry0, vs30, z10
      real specT, period2, lnY, sigma, phi, tau, lnSa, pgar
      integer hwflag, basinflag, iflag, CratFlag

c     Compute the median PGA for the reference site condition (VS30 = 760 m/s),
c     used in the nonlinear site term
      call S02_BS_2024_model ( mag, dip, fltWidth, ztor, rRup, Rx, Ry0,
     1                         760.0, 0.0, z10, hwflag, 0, 0.0,
     2                         lnSa, sigma, phi, tau, iflag, CratFlag )
      pgar = exp(lnSa)

c     Compute the ground motion for the requested spectral period and VS30
      call S02_BS_2024_model ( mag, dip, fltWidth, ztor, rRup, Rx, Ry0,
     1                         vs30, pgar, z10, hwflag, basinflag, specT,
     2                         lnSa, sigma, phi, tau, iflag, CratFlag )

C     Convert ground motion to units of gals.
      lnY = lnSa + 6.89

      period2 = specT

      return
      end

c ----------------------------------------------------------------------
      subroutine S02_BS_2024_model ( mag, dip, fltWidth, ztor, rRup, Rx, Ry0,
     1                         vs30, pgar, z10, hwflag, basinflag, specT,
     2                         lnSa, sigma, phi, tau, iflag, CratFlag )

      implicit none

      integer MAXPER
      parameter (MAXPER=63)
      real period(MAXPER), M1(MAXPER), c4(MAXPER), a2(MAXPER), a3(MAXPER)
      real a4(MAXPER), a5(MAXPER), a6(MAXPER), a7(MAXPER), a8(MAXPER)
      real a13(MAXPER), c(MAXPER), vc(MAXPER), b4(MAXPER), b5(MAXPER)
      real b6(MAXPER), b7(MAXPER), cz1(MAXPER), cz2(MAXPER), cz3(MAXPER)
      real cz4(MAXPER), a1Crat(MAXPER), a17Crat(MAXPER), a1NC(MAXPER)
      real a17NC(MAXPER), PhiA(MAXPER), PhiB(MAXPER)
      real M1T, c4T, a2T, a3T, a4T, a5T, a6T, a7T, a8T, a13T
      real cT, vcT, b4T, b5T, b6T, b7T, cz1T, cz2T, cz3T, cz4T
      real a1CratT, a17CratT, a1NCT, a17NCT, PhiAT, PhiBT, a1T, a17T
      real mag, dip, fltWidth, ztor, rRup, Rx, Ry0, vs30, pgar, z10
      real specT, period1, lnSa, sigma, phi, tau
      integer hwflag, basinflag, iflag, CratFlag, nPer, count1, count2, i
      real M2, c4_mag, R, zx, Vref, b1, b2, b3, flin, fnl, z1_ref, dz1
      real HW_taper1, HW_taper2, HW_taper3, HW_taper4, HW_taper5
      real hw_a2, h1, h2, h3, R1, R2, Ry1
      real tau1, tau2, tau3, tau4
      real f1, f4, f5, f6, f10

      data period / 0.01, 0.011, 0.012, 0.013, 0.015, 0.017, 0.02, 0.022, 0.025, 0.029,
     1              0.032, 0.035, 0.04, 0.045, 0.05, 0.055, 0.06, 0.065, 0.075, 0.085,
     2              0.1, 0.11, 0.12, 0.13, 0.15, 0.17, 0.2, 0.22, 0.24, 0.26,
     3              0.28, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.75,
     4              0.85, 1.0, 1.1, 1.2, 1.3, 1.5, 1.7, 2.0, 2.2, 2.4,
     5              2.6, 2.8, 3.0, 3.5, 4.0, 4.4, 5.0, 5.5, 6.0, 6.5,
     6              7.5, 8.5, 10.0 /
      data M1 / 6.7500, 6.7500, 6.7500, 6.7500, 6.7500, 6.7500, 6.7500, 6.7500, 6.7500, 6.7500,
     1          6.7500, 6.7500, 6.7500, 6.7500, 6.7500, 6.7500, 6.7500, 6.7500, 6.7500, 6.7500,
     2          6.7500, 6.7500, 6.7500, 6.7500, 6.7500, 6.7500, 6.7500, 6.7500, 6.7500, 6.7500,
     3          6.7500, 6.7500, 6.7500, 6.7500, 6.7500, 6.7500, 6.7500, 6.7500, 6.7500, 6.7500,
     4          6.7500, 6.7500, 6.7500, 6.7500, 6.7500, 6.7500, 6.7500, 6.7500, 6.7665, 6.7815,
     5          6.7953, 6.8081, 6.8200, 6.8736, 6.9200, 6.9542, 7.0000, 7.0314, 7.0600, 7.0905,
     6          7.1450, 7.1907, 7.2500 /
      data c4 / 4.5000, 4.5000, 4.5000, 4.5000, 4.5000, 4.5000, 4.5000, 4.5000, 4.5000, 4.5000,
     1          4.5000, 4.5000, 4.5000, 4.5000, 4.5000, 4.5000, 4.5000, 4.5000, 4.5000, 4.5000,
     2          4.5000, 4.5000, 4.5000, 4.5000, 4.5000, 4.5000, 4.5000, 4.5000, 4.5000, 4.5000,
     3          4.5000, 4.5000, 4.5000, 4.5000, 4.5000, 4.5000, 4.5000, 4.5000, 4.5000, 4.5000,
     4          4.5000, 4.5000, 4.5000, 4.5000, 4.5000, 4.5000, 4.5000, 4.5000, 4.5000, 4.5000,
     5          4.5000, 4.5000, 4.5000, 4.5000, 4.5000, 4.5000, 4.5000, 4.5000, 4.5000, 4.5000,
     6          4.5000, 4.5000, 4.5000 /
      data a2 / -0.7627, -0.7627, -0.7627, -0.7627, -0.7627, -0.7627, -0.7627, -0.7627, -0.7627, -0.7627,
     1          -0.7627, -0.7627, -0.7627, -0.7627, -0.7627, -0.7627, -0.7627, -0.7627, -0.7627, -0.7627,
     2          -0.7627, -0.7627, -0.7627, -0.7627, -0.7627, -0.7627, -0.7627, -0.7627, -0.7627, -0.7627,
     3          -0.7627, -0.7627, -0.7627, -0.7627, -0.7627, -0.7627, -0.7627, -0.7627, -0.7627, -0.7627,
     4          -0.7627, -0.7627, -0.7627, -0.7627, -0.7627, -0.7626, -0.7622, -0.7595, -0.7567, -0.7535,
     5          -0.7472, -0.7422, -0.7346, -0.7134, -0.6882, -0.6698, -0.6449, -0.6261, -0.6088, -0.5926,
     6          -0.5633, -0.5371, -0.5025 /
      data a3 / 0.22, 0.22, 0.22, 0.22, 0.22, 0.22, 0.22, 0.22, 0.22, 0.22,
     1          0.22, 0.22, 0.22, 0.22, 0.22, 0.22, 0.22, 0.22, 0.22, 0.22,
     2          0.22, 0.22, 0.22, 0.22, 0.22, 0.22, 0.22, 0.22, 0.22, 0.22,
     3          0.22, 0.22, 0.22, 0.22, 0.22, 0.22, 0.22, 0.22, 0.22, 0.22,
     4          0.22, 0.22, 0.22, 0.22, 0.22, 0.22, 0.22, 0.22, 0.22, 0.22,
     5          0.22, 0.22, 0.22, 0.22, 0.22, 0.22, 0.22, 0.22, 0.22, 0.22,
     6          0.22, 0.22, 0.22 /
      data a4 / -0.0209, -0.0209, -0.0209, -0.0209, -0.0209, -0.0209, -0.0209, -0.0209, -0.0209, -0.0209,
     1          -0.0209, -0.0209, -0.0209, -0.0209, -0.0209, -0.0209, -0.0209, -0.0209, -0.0209, -0.0209,
     2          -0.0209, -0.0209, -0.0209, -0.0209, -0.0209, -0.0209, -0.0209, -0.0209, -0.0209, -0.0209,
     3          -0.0209, -0.0209, -0.0209, -0.0209, -0.0209, -0.0209, -0.0209, -0.0209, -0.0209, -0.0209,
     4          -0.0209, -0.0209, -0.0209, -0.0209, -0.0209, -0.0209, -0.0209, -0.0209, -0.0209, -0.0209,
     5          -0.0209, -0.0209, -0.0209, -0.0209, -0.0209, -0.0209, -0.0209, -0.0209, -0.0209, -0.0209,
     6          -0.0209, -0.0209, -0.0209 /
      data a5 / -0.3309, -0.3309, -0.3309, -0.3309, -0.3309, -0.3309, -0.3309, -0.3309, -0.3309, -0.3309,
     1          -0.3309, -0.3309, -0.3309, -0.3309, -0.3309, -0.3309, -0.3309, -0.3309, -0.3309, -0.3309,
     2          -0.3309, -0.3309, -0.3309, -0.3309, -0.3309, -0.3309, -0.3309, -0.3309, -0.3309, -0.3309,
     3          -0.3309, -0.3309, -0.3309, -0.3309, -0.3309, -0.3309, -0.3309, -0.3309, -0.3309, -0.3309,
     4          -0.3309, -0.3309, -0.3309, -0.3309, -0.3309, -0.3309, -0.3309, -0.3309, -0.3309, -0.3309,
     5          -0.3309, -0.3309, -0.3309, -0.3309, -0.3309, -0.3309, -0.3309, -0.3309, -0.3309, -0.3309,
     6          -0.3309, -0.3309, -0.3309 /
      data a6 / 2.1541, 2.1530, 2.1520, 2.1511, 2.1494, 2.1480, 2.1461, 2.1486, 2.1519, 2.1557,
     1          2.1475, 2.1348, 2.1160, 2.0994, 2.0845, 2.0713, 2.0593, 2.0483, 2.0285, 2.0339,
     2          2.0408, 2.0596, 2.0768, 2.0926, 2.1208, 2.1657, 2.2241, 2.2618, 2.2962, 2.3180,
     3          2.3285, 2.3383, 2.4082, 2.4688, 2.5162, 2.5586, 2.5876, 2.6141, 2.6385, 2.6821,
     4          2.7173, 2.7630, 2.7800, 2.7956, 2.8099, 2.8355, 2.8624, 2.8973, 2.8994, 2.9013,
     5          2.9030, 2.9046, 2.9061, 2.8968, 2.8888, 2.8929, 2.8984, 2.8969, 2.8955, 2.8864,
     6          2.8700, 2.8583, 2.8431 /
      data a7 / 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000,
     1          0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000,
     2          0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000,
     3          0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000,
     4          0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000,
     5          0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000,
     6          0.0000, 0.0000, 0.0000 /
      data a8 / -0.0017, -0.0017, -0.0017, -0.0017, -0.0017, -0.0017, -0.0017, -0.0017, -0.0017, -0.0017,
     1          -0.0017, -0.0017, -0.0017, -0.0017, -0.0017, -0.0017, -0.0017, -0.0017, -0.0017, -0.0017,
     2          -0.0017, -0.0017, -0.0017, -0.0017, -0.0017, -0.0031, -0.0080, -0.0120, -0.0159, -0.0198,
     3          -0.0238, -0.0279, -0.0379, -0.0475, -0.0565, -0.0650, -0.0733, -0.0814, -0.0895, -0.1044,
     4          -0.1180, -0.1360, -0.1465, -0.1559, -0.1644, -0.1792, -0.1914, -0.2062, -0.2143, -0.2211,
     5          -0.2268, -0.2314, -0.2350, -0.2413, -0.2451, -0.2472, -0.2498, -0.2517, -0.2534, -0.2549,
     6          -0.2575, -0.2595, -0.2621 /
      data a13 / 0.6000, 0.6000, 0.6000, 0.6000, 0.6000, 0.6000, 0.6000, 0.6000, 0.6000, 0.6000,
     1           0.6000, 0.6000, 0.6000, 0.6000, 0.6000, 0.6000, 0.6000, 0.6000, 0.6000, 0.6000,
     2           0.6000, 0.6000, 0.6000, 0.6000, 0.6000, 0.6000, 0.6000, 0.6000, 0.6000, 0.6000,
     3           0.6000, 0.6000, 0.5893, 0.5800, 0.5694, 0.5600, 0.5529, 0.5465, 0.5406, 0.5300,
     4           0.5169, 0.5000, 0.4812, 0.4640, 0.4482, 0.4200, 0.3895, 0.3500, 0.3147, 0.2826,
     5           0.2529, 0.2255, 0.2000, 0.0928, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000,
     6           0.0000, 0.0000, 0.0000 /
      data c / -0.6037, -0.5996, -0.5959, -0.5924, -0.5863, -0.5809, -0.5739, -0.5645, -0.5520, -0.5374,
     1         -0.5245, -0.5111, -0.4912, -0.4737, -0.4580, -0.4547, -0.4517, -0.4490, -0.4441, -0.4629,
     2         -0.4872, -0.5089, -0.5287, -0.5470, -0.5796, -0.6266, -0.6876, -0.7236, -0.7564, -0.7868,
     3         -0.8152, -0.8417, -0.8788, -0.9109, -0.9417, -0.9693, -0.9801, -0.9900, -0.9991, -1.0154,
     4         -1.0305, -1.0500, -1.0489, -1.0479, -1.0470, -1.0454, -1.0427, -1.0392, -1.0326, -1.0266,
     5         -1.0211, -1.0160, -1.0112, -0.9888, -0.9694, -0.9481, -0.9195, -0.8859, -0.8552, -0.8270,
     6         -0.7766, -0.7240, -0.6558 /
      data vc / 1500.0, 1500.0, 1500.0, 1500.0, 1500.0, 1500.0, 1500.0, 1501.0, 1502.0, 1503.0,
     1          1503.0, 1502.0, 1502.0, 1502.0, 1501.0, 1500.0, 1498.0, 1497.0, 1494.0, 1488.0,
     2          1479.0, 1471.0, 1463.0, 1456.0, 1443.0, 1421.0, 1393.0, 1377.0, 1363.0, 1346.0,
     3          1327.0, 1308.0, 1279.0, 1253.0, 1227.0, 1204.0, 1191.0, 1179.0, 1167.0, 1148.0,
     4          1131.0, 1110.0, 1101.0, 1093.0, 1086.0, 1072.0, 1045.0, 1009.0, 989.0, 970.0,
     5          953.0, 937.0, 922.0, 881.0, 844.0, 823.0, 793.0, 788.0, 783.0, 779.0,
     6          771.0, 773.0, 775.0 /
      data b4 / -0.1483, -0.1481, -0.1480, -0.1478, -0.1476, -0.1474, -0.1471, -0.1489, -0.1514, -0.1542,
     1          -0.1596, -0.1661, -0.1758, -0.1843, -0.1920, -0.2021, -0.2113, -0.2198, -0.2350, -0.2412,
     2          -0.2492, -0.2511, -0.2528, -0.2543, -0.2571, -0.2525, -0.2466, -0.2419, -0.2377, -0.2321,
     3          -0.2254, -0.2191, -0.2066, -0.1958, -0.1848, -0.1750, -0.1665, -0.1587, -0.1515, -0.1387,
     4          -0.1241, -0.1052, -0.0950, -0.0858, -0.0772, -0.0620, -0.0507, -0.0361, -0.0308, -0.0260,
     5          -0.0215, -0.0174, -0.0136, -0.0080, -0.0032, -0.0020, -0.0003, -0.0003, -0.0002, -0.0002,
     6          -0.0001, -0.0001, 0.0000 /
      data b5 / -0.0070, -0.0070, -0.0071, -0.0071, -0.0072, -0.0072, -0.0073, -0.0073, -0.0073, -0.0073,
     1          -0.0072, -0.0071, -0.0069, -0.0067, -0.0065, -0.0063, -0.0061, -0.0060, -0.0057, -0.0057,
     2          -0.0056, -0.0057, -0.0057, -0.0058, -0.0059, -0.0060, -0.0061, -0.0063, -0.0064, -0.0065,
     3          -0.0066, -0.0067, -0.0069, -0.0071, -0.0073, -0.0074, -0.0076, -0.0077, -0.0079, -0.0081,
     4          -0.0083, -0.0084, -0.0083, -0.0081, -0.0080, -0.0077, -0.0064, -0.0048, -0.0041, -0.0035,
     5          -0.0029, -0.0023, -0.0018, -0.0017, -0.0015, -0.0015, -0.0014, -0.0014, -0.0014, -0.0014,
     6          -0.0014, -0.0014, -0.0014 /
      data b6 / -9.9000, -9.9000, -9.9000, -9.9000, -9.9000, -9.9000, -9.9000, -9.9000, -9.9000, -9.9000,
     1          -9.9000, -9.9000, -9.9000, -9.9000, -9.9000, -9.9000, -9.9000, -9.9000, -9.9000, -9.9000,
     2          -9.9000, -9.9000, -9.9000, -9.9000, -9.9000, -9.9000, -9.9000, -9.9000, -9.9000, -9.9000,
     3          -9.9000, -9.9000, -9.9000, -9.9000, -9.9000, -9.9000, -7.5512, -5.4070, -3.4345, 0.0920,
     4          0.2116, 0.3670, 0.4307, 0.4889, 0.5424, 0.6380, 0.7394, 0.8710, 0.9331, 0.9897,
     5          1.0418, 1.0901, 1.1350, 1.2079, 1.2710, 1.2958, 1.3290, 1.3290, 1.3290, 1.3290,
     6          1.3290, 1.2655, 1.1830 /
      data b7 / -9.9000, -9.9000, -9.9000, -9.9000, -9.9000, -9.9000, -9.9000, -9.9000, -9.9000, -9.9000,
     1          -9.9000, -9.9000, -9.9000, -9.9000, -9.9000, -9.9000, -9.9000, -9.9000, -9.9000, -9.9000,
     2          -9.9000, -9.9000, -9.9000, -9.9000, -9.9000, -9.9000, -9.9000, -9.9000, -9.9000, -9.9000,
     3          -9.9000, -9.9000, -9.9000, -9.9000, -9.9000, -9.9000, -7.5590, -5.4218, -3.4558, 0.0590,
     4          0.1238, 0.2080, 0.2317, 0.2534, 0.2734, 0.3090, 0.3408, 0.3820, 0.4135, 0.4423,
     5          0.4687, 0.4932, 0.5160, 0.5765, 0.6290, 0.6756, 0.7380, 0.7547, 0.7699, 0.7839,
     6          0.8090, 0.7629, 0.7030 /
      data cz1 / 1.325488e-4, 1.325744e-4, 1.325980e-4, 1.326258e-4, 1.326888e-4, 1.327763e-4,
     1           1.329486e-4, 1.330999e-4, 1.334157e-4, 1.338342e-4, 1.341721e-4, 1.347842e-4,
     2           1.361571e-4, 1.374530e-4, 1.381713e-4, 1.384314e-4, 1.386093e-4, 1.389004e-4,
     3           1.402769e-4, 1.410298e-4, 1.417650e-4, 1.418208e-4, 1.420089e-4, 1.414096e-4,
     4           1.405305e-4, 1.399520e-4, 1.379511e-4, 1.361603e-4, 1.341161e-4, 1.336040e-4,
     5           1.341446e-4, 1.342473e-4, 1.326980e-4, 1.301491e-4, 1.260214e-4, 1.208692e-4,
     6           1.158043e-4, 1.131128e-4, 1.098908e-4, 1.016030e-4, 8.817458e-5, 5.224173e-5,
     7           1.806055e-5, -2.608161e-5, -6.492015e-5, -1.243038e-4, -1.648680e-4, -2.048960e-4,
     8           -2.219840e-4, -2.295813e-4, -2.213266e-4, -1.990612e-4, -1.743046e-4, -1.095979e-4,
     9           -4.770469e-5, -7.520799e-6, 3.713649e-5, 7.399659e-5, 1.091787e-4, 1.240822e-4,
     a           1.365910e-4, 1.389766e-4, 1.395908e-4 /
      data cz2 / -6.999603e-3, -7.000727e-3, -7.001761e-3, -7.002955e-3, -7.005638e-3, -7.009275e-3,
     1           -7.016444e-3, -7.022441e-3, -7.034862e-3, -7.054221e-3, -7.071245e-3, -7.099305e-3,
     2           -7.157659e-3, -7.213155e-3, -7.246723e-3, -7.261932e-3, -7.275294e-3, -7.291614e-3,
     3           -7.350952e-3, -7.386078e-3, -7.418449e-3, -7.422057e-3, -7.426520e-3, -7.406264e-3,
     4           -7.373773e-3, -7.349851e-3, -7.261906e-3, -7.190087e-3, -7.112497e-3, -7.090806e-3,
     5           -7.101329e-3, -7.093361e-3, -7.022087e-3, -6.914169e-3, -6.748225e-3, -6.526250e-3,
     6           -6.307257e-3, -6.176351e-3, -6.028595e-3, -5.635651e-3, -4.972511e-3, -3.205426e-3,
     7           -1.570870e-3, 4.902557e-4, 2.311814e-3, 5.044826e-3, 6.925038e-3, 8.879608e-3,
     8           9.813057e-3, 1.033226e-2, 1.012965e-2, 9.333930e-3, 8.436106e-3, 6.053134e-3,
     9           3.710695e-3, 2.118920e-3, 1.462783e-4, -1.532421e-3, -3.118259e-3, -3.825556e-3,
     a           -4.444746e-3, -4.572971e-3, -4.607853e-3 /
      data cz3 / 1.202697e-1, 1.202885e-1, 1.203059e-1, 1.203262e-1, 1.203713e-1, 1.204296e-1,
     1           1.205436e-1, 1.206366e-1, 1.208328e-1, 1.211873e-1, 1.215135e-1, 1.219772e-1,
     2           1.228729e-1, 1.237481e-1, 1.243963e-1, 1.248098e-1, 1.252096e-1, 1.256382e-1,
     3           1.265878e-1, 1.271557e-1, 1.275866e-1, 1.276926e-1, 1.277573e-1, 1.274974e-1,
     4           1.269512e-1, 1.264609e-1, 1.251753e-1, 1.241996e-1, 1.231411e-1, 1.225121e-1,
     5           1.221363e-1, 1.215887e-1, 1.199934e-1, 1.180744e-1, 1.156838e-1, 1.125623e-1,
     6           1.094006e-1, 1.068605e-1, 1.041537e-1, 9.740602e-2, 8.654064e-2, 5.925349e-2,
     7           3.553768e-2, 7.125261e-3, -1.764006e-2, -5.462312e-2, -8.104414e-2, -1.107089e-1,
     8           -1.264466e-1, -1.372143e-1, -1.396491e-1, -1.357284e-1, -1.300570e-1, -1.126552e-1,
     9           -9.329191e-2, -7.891795e-2, -5.795846e-2, -3.886129e-2, -2.035350e-2, -1.058312e-2,
     a           8.984393e-4, 4.286764e-3, 5.952553e-3 /
      data cz4 / -6.464041e-1, -6.465077e-1, -6.466032e-1, -6.467152e-1, -6.469617e-1, -6.472600e-1,
     1           -6.478300e-1, -6.483183e-1, -6.493246e-1, -6.511911e-1, -6.529194e-1, -6.551660e-1,
     2           -6.597074e-1, -6.645417e-1, -6.686218e-1, -6.713644e-1, -6.742963e-1, -6.772978e-1,
     3           -6.819853e-1, -6.845763e-1, -6.862269e-1, -6.871420e-1, -6.877568e-1, -6.870790e-1,
     4           -6.841278e-1, -6.805745e-1, -6.731818e-1, -6.685334e-1, -6.638726e-1, -6.601196e-1,
     5           -6.559912e-1, -6.509111e-1, -6.384821e-1, -6.270608e-1, -6.159586e-1, -6.029356e-1,
     6           -5.890475e-1, -5.754603e-1, -5.611971e-1, -5.268935e-1, -4.739102e-1, -3.505072e-1,
     7           -2.512864e-1, -1.404709e-1, -4.834836e-2, 8.852911e-2, 1.919530e-1, 3.203855e-1,
     8           3.964343e-1, 4.602114e-1, 5.088136e-1, 5.498642e-1, 5.832101e-1, 6.397211e-1,
     9           6.569914e-1, 6.483045e-1, 5.822143e-1, 5.059828e-1, 4.303319e-1, 3.776517e-1,
     a           2.934614e-1, 2.407439e-1, 1.820601e-1 /
      data a1Crat / 1.5042, 1.5171, 1.531, 1.5463, 1.5828, 1.6334,
     1              1.823, 1.9594, 2.0755, 2.1949, 2.2621, 2.3158,
     2              2.3795, 2.4217, 2.4465, 2.4543, 2.4539, 2.4504,
     3              2.4282, 2.4, 2.3424, 2.298, 2.25270739, 2.202328916,
     4              2.11004319, 2.019826037, 1.910225674, 1.845814949, 1.789125471, 1.735130143,
     5              1.686263365, 1.639131679, 1.532469573, 1.446727185, 1.370237685, 1.301333393,
     6              1.242784333, 1.189783298, 1.144214006, 1.044762543, 0.927476116, 0.692771558,
     7              0.499532224, 0.300180783, 0.128554604, -0.142325155, -0.334603062, -0.601027416,
     8              -0.759500809, -0.895100034, -1.0299, -1.1476, -1.2695, -1.5564,
     9              -1.7879, -1.9615, -2.1285, -2.2037, -2.3061, -2.4162,
     a              -2.6446, -2.8563, -3.1836 /
      data a17Crat / -0.005124, -0.005224, -0.005322, -0.005418, -0.005601, -0.00577, -0.006087, -0.006259,
     1               -0.006479, -0.006646, -0.006667, -0.006638, -0.006528, -0.006364, -0.006199, -0.006032,
     2               -0.00587, -0.005716, -0.005441, -0.005206, -0.004904, -0.004734, -0.004582, -0.004444,
     3               -0.004214, -0.004018, -0.003783, -0.003654, -0.003539, -0.003438, -0.003346, -0.003262,
     4               -0.003079, -0.002933, -0.002813, -0.002713, -0.002635, -0.002567, -0.002524, -0.002523,
     5               -0.002584, -0.002658, -0.00263, -0.002568, -0.002495, -0.00216, -0.001818, -0.001338,
     6               -0.001087, -0.000881, -0.00073, -0.000611, -0.000529, -0.000372, -0.000309, -0.000294,
     7               -0.000294, -0.000294, -0.000294, -0.000294, -0.000294, -0.000294, -0.000294 /
      data a1NC / 0.9145, 0.9187, 0.9225, 0.9265, 0.9343, 0.9427,
     1            0.9578, 0.9694, 0.992, 1.0297, 1.061, 1.1006,
     2            1.1711, 1.2462, 1.3193, 1.3879, 1.4512, 1.5067,
     3            1.5979, 1.666, 1.733, 1.7629, 1.7813, 1.788,
     4            1.7895, 1.7849, 1.775110773, 1.767935981, 1.759307226, 1.747363011,
     5            1.733175641, 1.717390936, 1.673622376, 1.623189354, 1.572967167, 1.522866837,
     6            1.472803014, 1.426090575, 1.375845307, 1.272260601, 1.144037719, 0.907519934,
     7            0.735429095, 0.556667389, 0.409175808, 0.177214595, -0.0103, -0.2523,
     8            -0.3982, -0.5317, -0.6623, -0.7976, -0.9319, -1.2426,
     9            -1.5092, -1.689, -1.8888, -2.0204, -2.1417, -2.272,
     a            -2.5146, -2.7476, -3.0604 /
      data a17NC / -0.007517, -0.007525, -0.007533, -0.00754, -0.007555, -0.007568, -0.007603, -0.00764,
     1             -0.0077, -0.007802, -0.007898, -0.008007, -0.008203, -0.008412, -0.008607, -0.008781,
     2             -0.008931, -0.009053, -0.009201, -0.009252, -0.009259, -0.009177, -0.009085, -0.008987,
     3             -0.008729, -0.008484, -0.008129, -0.007908, -0.007703, -0.007512, -0.007335, -0.007174,
     4             -0.006823, -0.006533, -0.00628, -0.006059, -0.005863, -0.005685, -0.005522, -0.005212,
     5             -0.004891, -0.004428, -0.004103, -0.003822, -0.003562, -0.003244, -0.003, -0.002803,
     6             -0.002712, -0.002615, -0.002463, -0.002292, -0.002111, -0.001697, -0.001348, -0.001106,
     7             -0.001106, -0.001106, -0.001106, -0.001106, -0.001106, -0.001106, -0.001106 /
      data PhiA / 0.7131, 0.7130, 0.7130, 0.7129, 0.7128, 0.7127, 0.7126, 0.7137, 0.7151, 0.7167,
     1            0.7194, 0.7226, 0.7274, 0.7407, 0.7526, 0.7614, 0.7694, 0.7767, 0.7899, 0.7932,
     2            0.7974, 0.7927, 0.7884, 0.7844, 0.7773, 0.7646, 0.7481, 0.7370, 0.7268, 0.7174,
     3            0.7086, 0.7005, 0.6831, 0.6680, 0.6556, 0.6446, 0.6354, 0.6270, 0.6193, 0.6055,
     4            0.5943, 0.5797, 0.5717, 0.5644, 0.5577, 0.5457, 0.5362, 0.5239, 0.5180, 0.5125,
     5            0.5075, 0.5029, 0.4986, 0.4917, 0.4857, 0.4828, 0.4789, 0.4773, 0.4758, 0.4745,
     6            0.4721, 0.4712, 0.4700 /
      data PhiB / 0.5770, 0.5771, 0.5772, 0.5773, 0.5774, 0.5775, 0.5777, 0.5793, 0.5814, 0.5838,
     1            0.5875, 0.5918, 0.5982, 0.6147, 0.6295, 0.6405, 0.6505, 0.6597, 0.6761, 0.6809,
     2            0.6872, 0.6833, 0.6797, 0.6765, 0.6706, 0.6607, 0.6478, 0.6396, 0.6321, 0.6257,
     3            0.6203, 0.6152, 0.6069, 0.5998, 0.5943, 0.5893, 0.5854, 0.5819, 0.5786, 0.5728,
     4            0.5669, 0.5593, 0.5545, 0.5501, 0.5460, 0.5388, 0.5324, 0.5240, 0.5180, 0.5126,
     5            0.5076, 0.5029, 0.4986, 0.4917, 0.4857, 0.4828, 0.4789, 0.4773, 0.4758, 0.4744,
     6            0.4720, 0.4711, 0.4700 /

C Find the requested spectral period and corresponding coefficients
      nPer = 63

C First check for the PGA case (i.e., specT=0.0), which uses the 0.01 sec coefficients
      if (specT .eq. 0.0) then
         period1  = period(1)
         M1T      = M1(1)
         c4T      = c4(1)
         a2T      = a2(1)
         a3T      = a3(1)
         a4T      = a4(1)
         a5T      = a5(1)
         a6T      = a6(1)
         a7T      = a7(1)
         a8T      = a8(1)
         a13T     = a13(1)
         cT       = c(1)
         vcT      = vc(1)
         b4T      = b4(1)
         b5T      = b5(1)
         b6T      = b6(1)
         b7T      = b7(1)
         cz1T     = cz1(1)
         cz2T     = cz2(1)
         cz3T     = cz3(1)
         cz4T     = cz4(1)
         a1CratT  = a1Crat(1)
         a17CratT = a17Crat(1)
         a1NCT    = a1NC(1)
         a17NCT   = a17NC(1)
         PhiAT    = PhiA(1)
         PhiBT    = PhiB(1)
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=1,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'Bayless and Somerville (2024) Australia'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),M1(count1),M1(count2),
     +             specT,M1T,iflag)
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),
     +             specT,c4T,iflag)
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +             specT,a2T,iflag)
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +             specT,a3T,iflag)
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +             specT,a4T,iflag)
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +             specT,a5T,iflag)
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +             specT,a6T,iflag)
      call S24_interp (period(count1),period(count2),a7(count1),a7(count2),
     +             specT,a7T,iflag)
      call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +             specT,a8T,iflag)
      call S24_interp (period(count1),period(count2),a13(count1),a13(count2),
     +             specT,a13T,iflag)
      call S24_interp (period(count1),period(count2),c(count1),c(count2),
     +             specT,cT,iflag)
      call S24_interp (period(count1),period(count2),vc(count1),vc(count2),
     +             specT,vcT,iflag)
      call S24_interp (period(count1),period(count2),b4(count1),b4(count2),
     +             specT,b4T,iflag)
      call S24_interp (period(count1),period(count2),b5(count1),b5(count2),
     +             specT,b5T,iflag)
      call S24_interp (period(count1),period(count2),b6(count1),b6(count2),
     +             specT,b6T,iflag)
      call S24_interp (period(count1),period(count2),b7(count1),b7(count2),
     +             specT,b7T,iflag)
      call S24_interp (period(count1),period(count2),cz1(count1),cz1(count2),
     +             specT,cz1T,iflag)
      call S24_interp (period(count1),period(count2),cz2(count1),cz2(count2),
     +             specT,cz2T,iflag)
      call S24_interp (period(count1),period(count2),cz3(count1),cz3(count2),
     +             specT,cz3T,iflag)
      call S24_interp (period(count1),period(count2),cz4(count1),cz4(count2),
     +             specT,cz4T,iflag)
      call S24_interp (period(count1),period(count2),a1Crat(count1),a1Crat(count2),
     +             specT,a1CratT,iflag)
      call S24_interp (period(count1),period(count2),a17Crat(count1),a17Crat(count2),
     +             specT,a17CratT,iflag)
      call S24_interp (period(count1),period(count2),a1NC(count1),a1NC(count2),
     +             specT,a1NCT,iflag)
      call S24_interp (period(count1),period(count2),a17NC(count1),a17NC(count2),
     +             specT,a17NCT,iflag)
      call S24_interp (period(count1),period(count2),PhiA(count1),PhiA(count2),
     +             specT,PhiAT,iflag)
      call S24_interp (period(count1),period(count2),PhiB(count1),PhiB(count2),
     +             specT,PhiBT,iflag)

 1011 period1 = specT

C     Set the region-specific coefficients (cratonic or non-cratonic)
      if ( CratFlag .eq. 1 ) then
         a1T = a1CratT
         a17T = a17CratT
      else
         a1T = a1NCT
         a17T = a17NCT
      endif

C     Magnitude scaling break
      M2 = 5.0

C     Magnitude dependent taper for c4
      if (mag .ge. 5.0) then
         c4_mag = c4T
      elseif (mag .ge. 4.0) then
         c4_mag = c4T - (c4T-2.0)*(5.0-mag)
      else
         c4_mag = 2.0
      endif

      R = sqrt(rRup**2 + c4_mag**2)

C     Magnitude and distance scaling (Eqs. 3 and 4)
      if ( mag .lt. M2 ) then
         f1 = a1T + a4T*(M2-M1T) + a8T*(8.5-M2)**2 + a6T*(mag-M2) + a7T*(mag-M2)**2
     1        + (a2T + a3T*(M2-M1T))*alog(R) + a17T*rRup
      elseif ( mag .lt. M1T ) then
         f1 = a1T + a4T*(mag-M1T) + a8T*(8.5-mag)**2
     1        + (a2T + a3T*(mag-M1T))*alog(R) + a17T*rRup
      else
         f1 = a1T + a5T*(mag-M1T) + a8T*(8.5-mag)**2
     1        + (a2T + a3T*(mag-M1T))*alog(R) + a17T*rRup
      endif

C     Depth to top of rupture scaling (Eq. 5)
      zx = min(ztor, 20.0)
      f6 = cz1T*zx**3 + cz2T*zx**2 + cz3T*zx + cz4T

C     VS30 scaling (Boore et al., 2014), linear and nonlinear terms
      Vref = 760.0
      b1 = 0.0
      b3 = 0.1
      if (vs30 .le. vcT) then
         flin = cT*alog(vs30/Vref)
      else
         flin = cT*alog(vcT/Vref)
      endif
      b2 = b4T*(exp(b5T*(min(vs30,760.0)-360.0)) - exp(b5T*(760.0-360.0)))
      fnl = b1 + b2*alog((pgar+b3)/b3)
      f5 = flin + fnl

C     Z1.0 scaling (Boore et al., 2014), applied for T >= 0.75 sec
      f10 = 0.0
      if (basinflag .eq. 1 .and. specT .ge. 0.75) then
         z1_ref = exp(-7.15/4.0*alog((vs30**4 + 570.94**4)/(1360.0**4 + 570.94**4)))/1000.0
         dz1 = z10 - z1_ref
         if (dz1 .le. b7T/b6T) then
            f10 = b6T*dz1
         else
            f10 = b7T
         endif
      endif

C     Hanging wall model (Abrahamson et al., 2014)
      if ( dip .le. 30. ) then
         HW_taper1 = 60./45.
      else
         HW_taper1 = (90.-dip)/45.
      endif

      hw_a2 = 0.2
      if ( mag .gt. 6.5 ) then
         HW_taper2 = 1. + hw_a2*(mag-6.5)
      elseif ( mag .gt. 5.5 ) then
         HW_taper2 = 1. + hw_a2*(mag-6.5) - (1.0 - hw_a2)*(mag-6.5)**2
      else
         HW_taper2 = 0.
      endif

      h1 = 0.25
      h2 = 1.5
      h3 = -0.75
      R1 = fltWidth*cos(dip*3.1415926/180.)
      R2 = 3.*R1
      if ( Rx .lt. 0.0 ) then
         HW_taper3 = 0.
      elseif ( Rx .le. R1 ) then
         HW_taper3 = h1 + h2*(Rx/R1) + h3*(Rx/R1)**2
      elseif ( Rx .lt. R2 ) then
         HW_taper3 = 1. - (Rx-R1)/(R2-R1)
      else
         HW_taper3 = 0.
      endif

      if ( ztor .lt. 10. ) then
         HW_taper4 = 1. - (ztor**2)/100.
      else
         HW_taper4 = 0.
      endif

      Ry1 = Rx*tan(20.*3.1415926/180.)
      if ( Ry0 .lt. Ry1 ) then
         HW_taper5 = 1.
      elseif ( Ry0-Ry1 .lt. 5. ) then
         HW_taper5 = 1. - (Ry0-Ry1)/5.
      else
         HW_taper5 = 0.
      endif

      if ( hwflag .eq. 1 ) then
         f4 = a13T*HW_taper1*HW_taper2*HW_taper3*HW_taper4*HW_taper5
      else
         f4 = 0.
      endif

C     Median ground motion (Eq. 2), in natural log units of g
      lnSa = f1 + f4 + f5 + f6 + f10

C     Aleatory variability from Al Atik (2015): tau from Table 5.1 and Eq. 5.6,
C     phi from Table 5.16 and Eq. 5.14
      tau1 = 0.4518
      tau2 = 0.4270
      tau3 = 0.3863
      tau4 = 0.3508
      if (mag .gt. 6.5) then
         tau = tau4
      elseif (mag .gt. 5.5) then
         tau = tau3 + (tau4 - tau3)*(mag - 5.5)/(6.5 - 5.5)
      elseif (mag .gt. 5.0) then
         tau = tau2 + (tau3 - tau2)*(mag - 5.0)/(5.5 - 5.0)
      elseif (mag .gt. 4.5) then
         tau = tau1 + (tau2 - tau1)*(mag - 4.5)/(5.0 - 4.5)
      else
         tau = tau1
      endif

      if (mag .le. 5.0) then
         phi = PhiAT
      elseif (mag .gt. 6.5) then
         phi = PhiBT
      else
         phi = PhiAT + (mag - 5.0)*(PhiBT - PhiAT)/1.5
      endif

C     Total standard deviation (Eq. 6)
      sigma = sqrt( phi**2 + tau**2 )

      return
      end

c ---------------------------------------------------------------------
C     *** Drouet (2015), Brazil ***
C   Unpublished stochastic model for Brazil by S. Drouet, developed with the
C     method of Drouet, S. and F. Cotton (2015). Regional stochastic GMPEs in
C     low-seismicity areas: scaling and aleatory variability analysis -
C     application to the French Alps, Bull. Seismol. Soc. Am. 105, 1883-1902
C     (functional form is Eq. 8 of that paper)
C   Coefficients are from the OpenQuake implementation (DrouetBrazil2015)
C     Notes:
C        Distance metric is RJB
C        Simulated range: 3.0 <= M <= 8.0, epicentral distance 1 - 250 km
C        Defined for PGV (specT = -1), PGA, and 0.01 - 3 sec
C        The model is for ground motion in m/s**2 (m/s for PGV)
c ---------------------------------------------------------------------

      subroutine S02_Drouet_2015_Brazil ( m, Rjb, specT, period2, lnY,
     1                                    sigma, phiT, tauT, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=22)
      real period(MAXPER), c1(MAXPER), c2(MAXPER), c3(MAXPER), c4(MAXPER)
      real c5(MAXPER), c6(MAXPER), c7(MAXPER), phi(MAXPER), tau(MAXPER)
      integer count1, count2, iflag, nPer, i
      real c1T, c2T, c3T, c4T, c5T, c6T, c7T, phiT, tauT
      real specT, period1, period2, lnY, sigma, m, Rjb

      data period / -1.0, 0.0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.075, 0.1, 0.15, 0.2,
     1              0.25, 0.3, 0.4, 0.5, 0.75, 1.0, 1.25, 1.5, 1.75, 2.0, 3.0 /
      data c1 / 0.457169, 2.575109, 2.586201, 2.717172, 2.930319, 3.156209,
     1          3.268731, 3.352582, 3.455122, 3.456514, 3.480893, 3.358985,
     2          3.115954, 2.806835, 2.837393, 2.383076, 2.070536, 1.944386,
     3          1.973072, 1.747518, 1.667278, 1.292331 /
      data c2 / 0.230048, -0.243140, -0.242271, -0.261739, -0.236637, -0.266436,
     1          -0.189794, -0.164010, -0.203575, -0.169395, -0.155262, -0.255601,
     2          -0.237559, -0.340296, -0.355473, -0.374649, -0.263869, -0.196142,
     3          -0.160616, -0.129961, -0.083863, 0.312316 /
      data c3 / -0.140795, -0.155164, -0.154703, -0.152580, -0.147729, -0.143032,
     1          -0.140262, -0.140254, -0.148680, -0.160434, -0.168476, -0.194574,
     2          -0.215762, -0.250121, -0.271003, -0.298428, -0.303220, -0.309115,
     3          -0.313180, -0.320672, -0.319818, -0.263539 /
      data c4 / -1.785544, -1.807995, -1.808695, -1.859646, -1.831982, -1.915769,
     1          -1.753521, -1.691894, -1.708867, -1.607720, -1.646459, -1.669187,
     2          -1.451276, -1.418762, -1.453916, -1.472297, -1.410898, -1.408815,
     3          -1.493457, -1.400692, -1.405853, -1.464213 /
      data c5 / 0.162408, 0.156084, 0.156141, 0.160136, 0.155727, 0.162181,
     1          0.144779, 0.140095, 0.139700, 0.131021, 0.132556, 0.134462,
     2          0.114182, 0.114054, 0.111753, 0.117984, 0.117144, 0.116519,
     3          0.122469, 0.116855, 0.114769, 0.130085 /
      data c6 / 7.201555, 7.629410, 7.623170, 7.773640, 7.918015, 8.224381,
     1          7.767181, 7.428414, 7.707583, 7.274064, 7.424609, 7.753731,
     2          7.212529, 6.837724, 7.298391, 7.051676, 6.815268, 6.904435,
     3          7.427893, 7.143261, 7.128404, 6.416692 /
      data c7 / -0.002832, -0.003996, -0.004020, -0.004048, -0.004558, -0.004350,
     1          -0.004910, -0.004756, -0.004261, -0.004025, -0.002871, -0.002732,
     2          -0.003761, -0.004081, -0.003037, -0.002899, -0.003307, -0.003017,
     3          -0.002316, -0.003402, -0.003174, -0.002621 /
      data phi / 0.526166, 0.625075, 0.630187, 0.688591, 0.751663, 0.779379,
     1           0.780218, 0.706350, 0.652456, 0.587130, 0.556933, 0.533650,
     2           0.526336, 0.519411, 0.512892, 0.507442, 0.511352, 0.511909,
     3           0.511871, 0.508641, 0.505025, 0.512370 /
      data tau / 0.393025, 0.481226, 0.481707, 0.486003, 0.490319, 0.498289,
     1           0.497182, 0.493511, 0.490010, 0.480912, 0.462619, 0.458696,
     2           0.452876, 0.440790, 0.427910, 0.405868, 0.384417, 0.376152,
     3           0.370833, 0.361738, 0.353357, 0.344082 /

C Find the requested spectral period and corresponding coefficients
      nPer = 22

C First check for the PGV case (i.e., specT=-1.0)
      if (specT .eq. -1.0) then
         period1 = period(1)
         c1T  = c1(1)
         c2T  = c2(1)
         c3T  = c3(1)
         c4T  = c4(1)
         c5T  = c5(1)
         c6T  = c6(1)
         c7T  = c7(1)
         phiT = phi(1)
         tauT = tau(1)
         goto 1011

C Check for the PGA case (i.e., specT=0.0)
      elseif (specT .eq. 0.0) then
         period1 = period(2)
         c1T  = c1(2)
         c2T  = c2(2)
         c3T  = c3(2)
         c4T  = c4(2)
         c5T  = c5(2)
         c6T  = c6(2)
         c7T  = c7(2)
         phiT = phi(2)
         tauT = tau(2)
         goto 1011

      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=3,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'Drouet (2015) Brazil'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),c1(count1),c1(count2),
     +             specT,c1T,iflag)
      call S24_interp (period(count1),period(count2),c2(count1),c2(count2),
     +             specT,c2T,iflag)
      call S24_interp (period(count1),period(count2),c3(count1),c3(count2),
     +             specT,c3T,iflag)
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),
     +             specT,c4T,iflag)
      call S24_interp (period(count1),period(count2),c5(count1),c5(count2),
     +             specT,c5T,iflag)
      call S24_interp (period(count1),period(count2),c6(count1),c6(count2),
     +             specT,c6T,iflag)
      call S24_interp (period(count1),period(count2),c7(count1),c7(count2),
     +             specT,c7T,iflag)
      call S24_interp (period(count1),period(count2),phi(count1),phi(count2),
     +             specT,phiT,iflag)
      call S24_interp (period(count1),period(count2),tau(count1),tau(count2),
     +             specT,tauT,iflag)

 1011 period1 = specT

C     Median ground motion (Eq. 8 of Drouet and Cotton, 2015), in natural log
C     units of m/s**2 (m/s for PGV)
      lnY = c1T + c2T*(m - 8.0) + c3T*(m - 8.0)**2
     1      + (c4T + c5T*m)*alog(sqrt(Rjb**2 + c6T**2)) + c7T*Rjb

C     Total standard deviation
      sigma = sqrt( phiT**2 + tauT**2 )

C     Convert ground motion to units of gals: from m/s**2 to g (g = 9.80665 m/s**2)
C     for PGA and spectral acceleration, and from m/s to cm/s for PGV
      if (specT .eq. -1.0) then
         lnY = lnY + alog(100.0) + 6.89
      else
         lnY = lnY - alog(9.80665) + 6.89
      endif

      period2 = period1

      return
      end

c ---------------------------------------------------------------------
C     *** Somerville et al. (2009), Australia, Cratonic ***
C   Somerville, P., R. Graves, N. Collins, S. G. Song, S. Ni, and P. Cummins
C     (2009). Source and ground motion models for Australian earthquakes,
C     Proceedings of the 2009 Annual Conference of the Australian Earthquake
C     Engineering Society, Newcastle
C   Coefficients are from Table 4 of Somerville et al. (2009)
C     Notes:
C        This is the Yilgarn Craton model, applicable to the Yilgarn
C           Craton and, to first order, other cratonic regions of Australia
C        Rock site conditions (VS30 = 865 m/s)
C        Distance metric is RJB (0 - 500 km)
C        Developed for 5.0 <= M <= 7.5
C        Defined for PGV (specT = -1), PGA, and 0.01 - 10 sec
c ---------------------------------------------------------------------

      subroutine S02_Somerville_2009_Craton ( m, Rjb, specT, period2, lnY,
     1                                 sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=24)
      real period(MAXPER), c1(MAXPER), c2(MAXPER), c3(MAXPER), c4(MAXPER)
      real c5(MAXPER), c6(MAXPER), c7(MAXPER), c8(MAXPER), sig(MAXPER)
      integer count1, count2, iflag, nPer, i
      real c1T, c2T, c3T, c4T, c5T, c6T, c7T, c8T, sigT
      real specT, period1, period2, lnY, sigma, m, Rjb
      real m1, r1, h, R, Rref

      data period / -1.0, 0.0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.075, 0.1, 0.15,
     1              0.2, 0.25, 0.3003, 0.4, 0.5, 0.75, 1.0, 1.4993, 2.0, 3.0003,
     2              4.0, 5.0, 7.5019, 10.0 /
      data c1 / 5.23440, 1.54560, 1.55510, 2.33800, 2.48090, 2.31450, 2.26860, 1.97070,
     1          1.71030, 1.52310, 1.36830, 1.40180, 1.45000, 1.44150, 1.40380, 1.50840,
     2          2.10630, 2.55790, 2.39600, 0.96040, 0.12190, -0.84240, -1.92260, -2.60330 /
      data c2 / 1.58530, 1.45650, 1.46380, 1.38060, 1.37540, 1.60250, 1.55840, 1.68030,
     1          1.75070, 1.69160, 1.57940, 1.28940, 1.04630, 0.92820, 0.69160, 0.75800,
     2          0.38180, -0.84270, -1.39950, -0.46120, -0.06980, 0.53160, 0.63760, 0.59060 /
      data c3 / -1.01540, -1.11510, -1.11460, -1.22970, -1.17620, -1.12600, -1.07340, -1.01540,
     1          -0.99330, -0.96310, -0.94720, -0.94410, -0.94880, -0.91830, -0.91010, -0.99010,
     2          -1.08680, -0.81810, -0.70440, -0.70450, -0.75910, -0.79600, -0.81900, -0.80940 /
      data c4 / 0.21400, 0.16640, 0.16620, 0.18010, 0.17120, 0.17150, 0.14710, 0.14560,
     1          0.13820, 0.13330, 0.13640, 0.14360, 0.14760, 0.11320, 0.13480, 0.11260,
     2          0.07950, 0.07650, 0.06770, 0.06450, 0.08490, 0.10330, 0.14550, 0.16090 /
      data c5 / -0.00341, -0.00567, -0.00568, -0.00467, -0.00542, -0.00629, -0.00709, -0.00737,
     1          -0.00746, -0.00713, -0.00677, -0.00617, -0.00581, -0.00576, -0.00557, -0.00458,
     2          -0.00406, -0.00220, -0.00366, -0.00429, -0.00374, -0.00180, -0.00066, -0.00106 /
      data c6 / -0.91610, -1.04900, -1.04840, -1.39850, -1.38720, -1.27910, -1.08910, -0.91930,
     1          -0.78140, -0.67330, -0.62690, -0.67070, -0.68700, -0.59520, -0.62390, -0.69040,
     2          -0.90340, -1.35320, -0.90860, -0.51190, -0.41450, -0.62130, -0.75740, -0.68550 /
      data c7 / 1.12980, 1.05530, 1.05850, 0.95990, 0.96930, 1.07040, 1.10750, 1.18290,
     1          1.29390, 1.22430, 1.17760, 1.05610, 0.94040, 0.86280, 0.71230, 0.68590,
     2          0.61850, -0.25440, -0.64320, -0.16430, 0.12350, 0.53680, 0.69020, 0.70350 /
      data c8 / 0.14810, 0.20000, 0.20140, 0.20130, 0.19280, 0.23560, 0.20670, 0.22170,
     1          0.23790, 0.21020, 0.18950, 0.14590, 0.11040, 0.04060, 0.00620, -0.05630,
     2          -0.18250, -0.46660, -0.59600, -0.46310, -0.39250, -0.27570, -0.23290, -0.22910 /
      data sig / 0.6606, 0.5513, 0.5512, 0.5510, 0.5508, 0.5509, 0.5510, 0.5514,
     1           0.5529, 0.5544, 0.5558, 0.5583, 0.5602, 0.5614, 0.5636, 0.5878,
     2           0.6817, 0.8514, 0.8646, 0.8424, 0.8225, 0.8088, 0.7808, 0.7624 /

C Find the requested spectral period and corresponding coefficients
      nPer = 24

C First check for the PGV case (i.e., specT=-1.0)
      if (specT .eq. -1.0) then
         period1 = period(1)
         c1T  = c1(1)
         c2T  = c2(1)
         c3T  = c3(1)
         c4T  = c4(1)
         c5T  = c5(1)
         c6T  = c6(1)
         c7T  = c7(1)
         c8T  = c8(1)
         sigT = sig(1)
         goto 1011

C Check for the PGA case (i.e., specT=0.0)
      elseif (specT .eq. 0.0) then
         period1 = period(2)
         c1T  = c1(2)
         c2T  = c2(2)
         c3T  = c3(2)
         c4T  = c4(2)
         c5T  = c5(2)
         c6T  = c6(2)
         c7T  = c7(2)
         c8T  = c8(2)
         sigT = sig(2)
         goto 1011

      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=3,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'Somerville et al. (2009) Australia, Cratonic'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),c1(count1),c1(count2),
     +             specT,c1T,iflag)
      call S24_interp (period(count1),period(count2),c2(count1),c2(count2),
     +             specT,c2T,iflag)
      call S24_interp (period(count1),period(count2),c3(count1),c3(count2),
     +             specT,c3T,iflag)
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),
     +             specT,c4T,iflag)
      call S24_interp (period(count1),period(count2),c5(count1),c5(count2),
     +             specT,c5T,iflag)
      call S24_interp (period(count1),period(count2),c6(count1),c6(count2),
     +             specT,c6T,iflag)
      call S24_interp (period(count1),period(count2),c7(count1),c7(count2),
     +             specT,c7T,iflag)
      call S24_interp (period(count1),period(count2),c8(count1),c8(count2),
     +             specT,c8T,iflag)
      call S24_interp (period(count1),period(count2),sig(count1),sig(count2),
     +             specT,sigT,iflag)

 1011 period1 = specT

C     Set constants (Table 2)
      m1 = 6.4
      r1 = 50.0
      h = 6.0

      R = sqrt(Rjb**2 + h**2)
      Rref = sqrt(r1**2 + h**2)

C     Median ground motion (Table 2), in natural log units of g (cm/s for PGV)
      lnY = c1T + c4T*(m - m1)*alog(R) + c5T*Rjb + c8T*(8.5 - m)**2

C     Magnitude scaling
      if (m .lt. m1) then
         lnY = lnY + c2T*(m - m1)
      else
         lnY = lnY + c7T*(m - m1)
      endif

C     Distance scaling
      if (Rjb .lt. r1) then
         lnY = lnY + c3T*alog(R)
      else
         lnY = lnY + c3T*alog(Rref) + c6T*(alog(R) - alog(Rref))
      endif

      sigma = sigT

C     Convert ground motion to units of gals.
      lnY = lnY + 6.89

      period2 = period1

      return
      end


c ---------------------------------------------------------------------
C     *** Somerville et al. (2009), Australia, Non-Cratonic ***
C   Somerville, P., R. Graves, N. Collins, S. G. Song, S. Ni, and P. Cummins
C     (2009). Source and ground motion models for Australian earthquakes,
C     Proceedings of the 2009 Annual Conference of the Australian Earthquake
C     Engineering Society, Newcastle
C   Coefficients are from Table 3 of Somerville et al. (2009)
C     Notes:
C        This is the non-cratonic model, applicable, to first order, to
C           non-cratonic regions of Australia
C        Rock site conditions (VS30 = 865 m/s)
C        Distance metric is RJB (0 - 500 km)
C        Developed for 5.0 <= M <= 7.5
C        Defined for PGV (specT = -1), PGA, and 0.01 - 10 sec
c ---------------------------------------------------------------------

      subroutine S02_Somerville_2009_NonCraton ( m, Rjb, specT, period2, lnY,
     1                                 sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=24)
      real period(MAXPER), c1(MAXPER), c2(MAXPER), c3(MAXPER), c4(MAXPER)
      real c5(MAXPER), c6(MAXPER), c7(MAXPER), c8(MAXPER), sig(MAXPER)
      integer count1, count2, iflag, nPer, i
      real c1T, c2T, c3T, c4T, c5T, c6T, c7T, c8T, sigT
      real specT, period1, period2, lnY, sigma, m, Rjb
      real m1, r1, h, R, Rref

      data period / -1.0, 0.0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.075, 0.1, 0.15,
     1              0.2, 0.25, 0.3003, 0.4, 0.5, 0.75, 1.0, 1.4993, 2.0, 3.0003,
     2              4.0, 5.0, 7.5019, 10.0 /
      data c1 / 5.07090, 1.03780, 1.05360, 1.05680, 1.13530, 1.30000, 1.47680, 1.70220,
     1          1.65720, 1.94440, 1.82720, 1.74380, 1.80560, 1.88750, 2.03760, 1.93060,
     2          1.60380, 0.47740, -0.25810, -0.96360, -1.46140, -1.61160, -2.35310, -3.26140 /
      data c2 / 0.52780, -0.03970, -0.04190, -0.03920, -0.04790, -0.07020, -0.09310, -0.05160,
     1          0.15080, -0.09620, -0.06230, -0.02530, -0.27020, -0.37820, -0.79590, -0.80280,
     2          -0.47800, 0.90960, 1.37770, 1.14690, 1.07950, 0.74860, 0.35190, 0.69730 /
      data c3 / -0.85740, -0.79430, -0.79390, -0.79680, -0.80920, -0.83150, -0.83330, -0.80720,
     1          -0.77590, -0.75000, -0.73430, -0.72480, -0.73190, -0.70580, -0.69730, -0.74510,
     2          -0.86950, -1.02440, -1.01000, -0.88530, -0.80490, -0.78100, -0.64340, -0.62760 /
      data c4 / 0.17700, 0.14450, 0.14450, 0.14550, 0.15000, 0.15920, 0.15600, 0.14560,
     1          0.13100, 0.11670, 0.11940, 0.11950, 0.13490, 0.09960, 0.11470, 0.11220,
     2          0.07320, 0.11060, 0.10310, 0.10380, 0.10960, 0.09650, 0.09590, 0.12920 /
      data c5 / -0.00501, -0.00618, -0.00619, -0.00617, -0.00610, -0.00599, -0.00606, -0.00655,
     1          -0.00708, -0.00698, -0.00677, -0.00646, -0.00606, -0.00589, -0.00565, -0.00503,
     2          -0.00569, -0.00652, -0.00539, -0.00478, -0.00395, -0.00307, -0.00138, -0.00155 /
      data c6 / -0.61190, -0.72540, -0.72660, -0.73230, -0.76410, -0.82850, -0.86740, -0.87690,
     1          -0.77830, -0.69490, -0.64380, -0.63740, -0.66440, -0.58770, -0.59990, -0.59460,
     2          -0.41590, -0.19000, -0.27340, -0.40420, -0.46040, -0.46490, -0.68260, -0.61980 /
      data c7 / 0.80660, -0.03590, -0.03940, -0.03930, -0.05710, -0.09810, -0.12740, -0.10970,
     1          0.01690, -0.13320, -0.09570, -0.06250, -0.17470, -0.24420, -0.48670, -0.50120,
     2          0.06360, 1.09610, 1.50330, 1.54130, 1.41960, 1.24090, 0.92880, 1.01050 /
      data c8 / -0.03800, -0.09730, -0.09740, -0.09600, -0.09210, -0.08530, -0.09130, -0.08690,
     1          -0.05980, -0.12530, -0.11920, -0.11650, -0.14340, -0.21890, -0.29690, -0.34990,
     2          -0.33730, -0.10660, -0.04530, -0.11020, -0.14700, -0.22170, -0.31230, -0.24550 /
      data sig / 0.6417, 0.5685, 0.5684, 0.5684, 0.5681, 0.5676, 0.5670, 0.5663,
     1           0.5659, 0.5659, 0.5669, 0.5678, 0.5708, 0.5697, 0.5739, 0.5876,
     2           0.6269, 0.7517, 0.8036, 0.8219, 0.8212, 0.8240, 0.7957, 0.7602 /

C Find the requested spectral period and corresponding coefficients
      nPer = 24

C First check for the PGV case (i.e., specT=-1.0)
      if (specT .eq. -1.0) then
         period1 = period(1)
         c1T  = c1(1)
         c2T  = c2(1)
         c3T  = c3(1)
         c4T  = c4(1)
         c5T  = c5(1)
         c6T  = c6(1)
         c7T  = c7(1)
         c8T  = c8(1)
         sigT = sig(1)
         goto 1011

C Check for the PGA case (i.e., specT=0.0)
      elseif (specT .eq. 0.0) then
         period1 = period(2)
         c1T  = c1(2)
         c2T  = c2(2)
         c3T  = c3(2)
         c4T  = c4(2)
         c5T  = c5(2)
         c6T  = c6(2)
         c7T  = c7(2)
         c8T  = c8(2)
         sigT = sig(2)
         goto 1011

      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=3,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'Somerville et al. (2009) Australia, Non-Cratonic'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),c1(count1),c1(count2),
     +             specT,c1T,iflag)
      call S24_interp (period(count1),period(count2),c2(count1),c2(count2),
     +             specT,c2T,iflag)
      call S24_interp (period(count1),period(count2),c3(count1),c3(count2),
     +             specT,c3T,iflag)
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),
     +             specT,c4T,iflag)
      call S24_interp (period(count1),period(count2),c5(count1),c5(count2),
     +             specT,c5T,iflag)
      call S24_interp (period(count1),period(count2),c6(count1),c6(count2),
     +             specT,c6T,iflag)
      call S24_interp (period(count1),period(count2),c7(count1),c7(count2),
     +             specT,c7T,iflag)
      call S24_interp (period(count1),period(count2),c8(count1),c8(count2),
     +             specT,c8T,iflag)
      call S24_interp (period(count1),period(count2),sig(count1),sig(count2),
     +             specT,sigT,iflag)

 1011 period1 = specT

C     Set constants (Table 2)
      m1 = 6.4
      r1 = 50.0
      h = 6.0

      R = sqrt(Rjb**2 + h**2)
      Rref = sqrt(r1**2 + h**2)

C     Median ground motion (Table 2), in natural log units of g (cm/s for PGV)
      lnY = c1T + c4T*(m - m1)*alog(R) + c5T*Rjb + c8T*(8.5 - m)**2

C     Magnitude scaling
      if (m .lt. m1) then
         lnY = lnY + c2T*(m - m1)
      else
         lnY = lnY + c7T*(m - m1)
      endif

C     Distance scaling
      if (Rjb .lt. r1) then
         lnY = lnY + c3T*alog(R)
      else
         lnY = lnY + c3T*alog(Rref) + c6T*(alog(R) - alog(Rref))
      endif

      sigma = sigT

C     Convert ground motion to units of gals.
      lnY = lnY + 6.89

      period2 = period1

      return
      end


c ---------------------------------------------------------------------
C     *** Allen (2012), Southeastern Australia ***
C   Allen, T. I. (2012). Stochastic ground-motion prediction equations for
C     southeastern Australian earthquakes using updated source and attenuation
C     parameters, Geoscience Australia Record 2012/69
C   Coefficients are from the OpenQuake implementation (Allen2012), which uses
C     the coefficients from the model author's spreadsheet (20120821) instead
C     of Tables 7 and 8 of the report; these reproduce the unit test data in
C     Appendix I of the report
C     Notes:
C        Separate coefficients for shallow (hypocentral depth < 10 km) and
C           deep (hypocentral depth >= 10 km) earthquakes
C        Average southeastern Australia site conditions (VS30 of about 820 m/s)
C        Distance metric is Rrup
C        Applicable range: 4.0 <= M <= 7.5, Rrup < 400 km
C        Defined for PGA and 0.01 - 4 sec (PGA is set to the 0.01 sec value)
c ---------------------------------------------------------------------

      subroutine S02_Allen_2012 ( m, Rrup, hypoDepth, specT, period2, lnY,
     1                            sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=19)
      real period(MAXPER), c0(MAXPER,2), c1(MAXPER,2), c2(MAXPER,2)
      real c3(MAXPER,2), c4(MAXPER,2), c5(MAXPER,2), c6(MAXPER,2)
      real c7(MAXPER,2), c8(MAXPER,2), c9(MAXPER,2), c10(MAXPER,2)
      real c11(MAXPER,2), sig(MAXPER,2)
      integer count1, count2, iflag, nPer, i, idep
      real c0T, c1T, c2T, c3T, c4T, c5T, c6T, c7T, c8T, c9T, c10T, c11T, sigT
      real specT, period1, period2, lnY, sigma, m, Rrup, hypoDepth
      real Mref, r1, r2, g0, g1, g2, logY

      data period / 0.0, 0.01, 0.02, 0.03, 0.05, 0.075, 0.1, 0.15, 0.2, 0.25,
     1              0.3, 0.4, 0.5, 0.75, 1.0, 1.4999, 2.0, 3.0003, 4.0 /

C     Shallow earthquakes (hypocentral depth < 10 km)
      data (c0(i,1),i=1,MAXPER) / 3.2586, 3.2586, 3.3683, 3.5107, 3.5450, 3.5160, 3.4584,
     1                            3.2956, 3.1362, 2.9980, 2.8721, 2.6231, 2.3981, 1.9437,
     2                            1.6160, 1.1925, 0.9286, 0.5738, 0.3395 /
      data (c1(i,1),i=1,MAXPER) / 0.5054, 0.5054, 0.4962, 0.4705, 0.4768, 0.4919, 0.5147,
     1                            0.5804, 0.6416, 0.6920, 0.7358, 0.8177, 0.8888, 1.0108,
     2                            1.0784, 1.1020, 1.0737, 1.0109, 0.9536 /
      data (c2(i,1),i=1,MAXPER) / -0.0693, -0.0693, -0.0620, -0.0599, -0.0628, -0.0679, -0.0728,
     1                            -0.0819, -0.0916, -0.1014, -0.1101, -0.1239, -0.1339, -0.1471,
     2                            -0.1514, -0.1423, -0.1263, -0.0953, -0.0712 /
      data (c3(i,1),i=1,MAXPER) / -1.8386, -1.8386, -1.8078, -1.8523, -1.8368, -1.8063, -1.7739,
     1                            -1.7002, -1.6475, -1.6149, -1.5934, -1.5654, -1.5476, -1.5267,
     2                            -1.5217, -1.5395, -1.5639, -1.5879, -1.6036 /
      data (c4(i,1),i=1,MAXPER) / 0.1580, 0.1580, 0.1363, 0.1423, 0.1403, 0.1424, 0.1423,
     1                            0.1314, 0.1272, 0.1304, 0.1351, 0.1398, 0.1399, 0.1404,
     2                            0.1445, 0.1604, 0.1751, 0.1848, 0.1884 /
      data (c5(i,1),i=1,MAXPER) / 1.2466, 1.2466, 1.2475, 1.4336, 1.4595, 1.4184, 1.3746,
     1                            1.2855, 1.2140, 1.1595, 1.1206, 1.0823, 1.0705, 1.0501,
     2                            1.0252, 1.0087, 1.0164, 1.0479, 1.0706 /
      data (c6(i,1),i=1,MAXPER) / -0.2045, -0.2045, -0.4862, -0.5445, -0.4889, -0.3407, -0.2152,
     1                            -0.0627, 0.0715, 0.2078, 0.3188, 0.4330, 0.4658, 0.4756,
     2                            0.4812, 0.5036, 0.5219, 0.5137, 0.5091 /
      data (c7(i,1),i=1,MAXPER) / -0.0441, -0.0441, 0.0163, 0.0148, -0.0106, -0.0428, -0.0611,
     1                            -0.0672, -0.0804, -0.1046, -0.1244, -0.1271, -0.1056, -0.0924,
     2                            -0.1235, -0.1592, -0.1638, -0.1574, -0.1580 /
      data (c8(i,1),i=1,MAXPER) / -5.1081, -5.1081, -4.7019, -4.9778, -5.1162, -4.9874, -4.8360,
     1                            -4.6640, -4.6829, -4.8054, -4.8545, -4.4665, -3.8200, -3.4220,
     2                            -4.0450, -4.2474, -3.7546, -3.7826, -4.2008 /
      data (c9(i,1),i=1,MAXPER) / -2.8612, -2.8612, -2.8631, -2.8492, -3.0724, -3.2518, -3.2860,
     1                            -3.1221, -2.9347, -2.7890, -2.6803, -2.5395, -2.4577, -2.4062,
     2                            -2.4381, -2.4984, -2.5266, -2.5356, -2.5644 /
      data (c10(i,1),i=1,MAXPER) / 0.2520, 0.2520, 0.2482, 0.2495, 0.3025, 0.3059, 0.2677,
     1                             0.1562, 0.1074, 0.1130, 0.1307, 0.1358, 0.1156, 0.1245,
     2                             0.1887, 0.2558, 0.2664, 0.2682, 0.2860 /
      data (c11(i,1),i=1,MAXPER) / -0.6911, -0.6911, -0.9393, -1.0706, -1.2294, -0.7602, -0.1890,
     1                             0.6327, 1.0339, 1.1911, 1.1665, 0.6517, -0.0632, -1.0127,
     2                             -1.0992, -0.6929, -0.2953, -0.0570, -0.1061 /
      data (sig(i,1),i=1,MAXPER) / 0.4120, 0.4120, 0.4383, 0.4310, 0.3994, 0.3805, 0.3720,
     1                             0.3637, 0.3594, 0.3575, 0.3558, 0.3544, 0.3522, 0.3495,
     2                             0.3487, 0.3492, 0.3484, 0.3467, 0.3457 /

C     Deep earthquakes (hypocentral depth >= 10 km)
      data (c0(i,2),i=1,MAXPER) / 3.3830, 3.3830, 3.5565, 3.7069, 3.7388, 3.6718, 3.5788,
     1                            3.3779, 3.1826, 3.0040, 2.8429, 2.5501, 2.3035, 1.8219,
     2                            1.4789, 1.0706, 0.8407, 0.5344, 0.3202 /
      data (c1(i,2),i=1,MAXPER) / 0.6034, 0.6034, 0.5904, 0.5281, 0.5462, 0.5844, 0.6266,
     1                            0.7276, 0.8205, 0.8987, 0.9634, 1.0648, 1.1393, 1.2480,
     2                            1.2965, 1.2744, 1.2059, 1.0959, 1.0250 /
      data (c2(i,2),i=1,MAXPER) / -0.0905, -0.0905, -0.0856, -0.0843, -0.0835, -0.0893, -0.0972,
     1                            -0.1128, -0.1283, -0.1432, -0.1556, -0.1718, -0.1807, -0.1860,
     2                            -0.1818, -0.1611, -0.1390, -0.1054, -0.0845 /
      data (c3(i,2),i=1,MAXPER) / -1.9289, -1.9289, -1.9394, -1.9729, -1.9369, -1.8879, -1.8478,
     1                            -1.7796, -1.7325, -1.7010, -1.6791, -1.6521, -1.6367, -1.6145,
     2                            -1.6031, -1.6256, -1.6639, -1.7074, -1.7307 /
      data (c4(i,2),i=1,MAXPER) / 0.1754, 0.1754, 0.1621, 0.1855, 0.1650, 0.1578, 0.1573,
     1                            0.1476, 0.1443, 0.1483, 0.1537, 0.1594, 0.1603, 0.1579,
     2                            0.1567, 0.1725, 0.1938, 0.2178, 0.2291 /
      data (c5(i,2),i=1,MAXPER) / 1.1140, 1.1140, 1.1954, 1.1653, 1.4281, 1.5010, 1.5125,
     1                            1.6277, 1.6481, 1.5729, 1.4901, 1.4364, 1.4725, 1.6026,
     2                            1.6826, 1.7006, 1.6575, 1.5980, 1.5703 /
      data (c6(i,2),i=1,MAXPER) / -0.1822, -0.1822, -0.4561, -0.4548, -0.4399, -0.2986, -0.1629,
     1                            -0.0090, 0.1229, 0.2598, 0.3689, 0.4608, 0.4626, 0.4530,
     2                            0.4868, 0.5261, 0.5385, 0.5975, 0.6403 /
      data (c7(i,2),i=1,MAXPER) / -0.0126, -0.0126, 0.0338, 0.0016, 0.0012, -0.0335, -0.0579,
     1                            -0.0451, -0.0430, -0.0632, -0.0843, -0.0955, -0.0833, -0.0765,
     2                            -0.1014, -0.1349, -0.1475, -0.1670, -0.1765 /
      data (c8(i,2),i=1,MAXPER) / -4.6974, -4.6974, -4.8588, -4.6835, -4.3352, -4.1895, -4.0552,
     1                            -3.6274, -3.4854, -3.5975, -3.7509, -3.8563, -3.7902, -3.6309,
     2                            -3.6122, -3.7229, -3.7642, -3.1596, -2.7375 /
      data (c9(i,2),i=1,MAXPER) / -3.1490, -3.1490, -3.0919, -3.2849, -3.4777, -3.5529, -3.5179,
     1                            -3.2850, -3.0590, -2.8838, -2.7493, -2.5597, -2.4365, -2.3807,
     2                            -2.4713, -2.5644, -2.5814, -2.7062, -2.8114 /
      data (c10(i,2),i=1,MAXPER) / 0.3152, 0.3152, 0.3012, 0.3938, 0.4168, 0.3990, 0.3471,
     1                             0.1821, 0.1035, 0.1047, 0.1261, 0.1346, 0.1113, 0.1155,
     2                             0.1820, 0.2480, 0.2597, 0.3113, 0.3526 /
      data (c11(i,2),i=1,MAXPER) / -0.7242, -0.7242, -1.1518, -1.2871, -1.0867, -1.2308, -1.1589,
     1                             -0.1896, 0.1635, -0.1385, -0.5928, -1.1806, -1.4107, -1.4951,
     2                             -1.4247, -0.9947, -0.5854, -0.5753, -0.8547 /
      data (sig(i,2),i=1,MAXPER) / 0.3653, 0.3653, 0.3897, 0.3840, 0.3558, 0.3392, 0.3323,
     1                             0.3271, 0.3247, 0.3246, 0.3245, 0.3248, 0.3225, 0.3188,
     2                             0.3180, 0.3161, 0.3142, 0.3113, 0.3097 /

C Select the coefficients for shallow (idep = 1) or deep (idep = 2) earthquakes
      if (hypoDepth .lt. 10.0) then
         idep = 1
      else
         idep = 2
      endif

C Find the requested spectral period and corresponding coefficients
      nPer = 19

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1 = period(1)
         c0T  = c0(1,idep)
         c1T  = c1(1,idep)
         c2T  = c2(1,idep)
         c3T  = c3(1,idep)
         c4T  = c4(1,idep)
         c5T  = c5(1,idep)
         c6T  = c6(1,idep)
         c7T  = c7(1,idep)
         c8T  = c8(1,idep)
         c9T  = c9(1,idep)
         c10T = c10(1,idep)
         c11T = c11(1,idep)
         sigT = sig(1,idep)
         goto 1011

      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'Allen (2012) Southeastern Australia'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),c0(count1,idep),c0(count2,idep),
     +             specT,c0T,iflag)
      call S24_interp (period(count1),period(count2),c1(count1,idep),c1(count2,idep),
     +             specT,c1T,iflag)
      call S24_interp (period(count1),period(count2),c2(count1,idep),c2(count2,idep),
     +             specT,c2T,iflag)
      call S24_interp (period(count1),period(count2),c3(count1,idep),c3(count2,idep),
     +             specT,c3T,iflag)
      call S24_interp (period(count1),period(count2),c4(count1,idep),c4(count2,idep),
     +             specT,c4T,iflag)
      call S24_interp (period(count1),period(count2),c5(count1,idep),c5(count2,idep),
     +             specT,c5T,iflag)
      call S24_interp (period(count1),period(count2),c6(count1,idep),c6(count2,idep),
     +             specT,c6T,iflag)
      call S24_interp (period(count1),period(count2),c7(count1,idep),c7(count2,idep),
     +             specT,c7T,iflag)
      call S24_interp (period(count1),period(count2),c8(count1,idep),c8(count2,idep),
     +             specT,c8T,iflag)
      call S24_interp (period(count1),period(count2),c9(count1,idep),c9(count2,idep),
     +             specT,c9T,iflag)
      call S24_interp (period(count1),period(count2),c10(count1,idep),c10(count2,idep),
     +             specT,c10T,iflag)
      call S24_interp (period(count1),period(count2),c11(count1,idep),c11(count2,idep),
     +             specT,c11T,iflag)
      call S24_interp (period(count1),period(count2),sig(count1,idep),sig(count2,idep),
     +             specT,sigT,iflag)

 1011 period1 = specT

C     Magnitude-dependent hinge distances (Eq. 19)
      Mref = m - 4.0
      r1 = 90.0 + c8T*Mref
      r2 = 150.0 + c11T*Mref

C     Geometrical attenuation terms (Eq. 19)
      g0 = alog10(sqrt(min(Rrup, r1)**2 + (1.0 + c5T*Mref)**2))
      g1 = max(alog10(Rrup/r1), 0.0)
      g2 = max(alog10(Rrup/r2), 0.0)

C     Median ground motion (Eq. 18), in log10 units of cm/s**2
      logY = c0T + c1T*Mref + c2T*Mref**2 + (c3T + c4T*Mref)*g0
     1       + (c6T + c7T*Mref)*g1 + (c9T + c10T*Mref)*g2

C     Convert from log10(cm/s**2) to natural log units of g (g = 980.665 cm/s**2)
C     and then to units of gals
      lnY = logY*alog(10.0) - alog(980.665) + 6.89

C     Convert sigma from log10 to natural log units
      sigma = sigT*alog(10.0)

      period2 = period1

      return
      end
