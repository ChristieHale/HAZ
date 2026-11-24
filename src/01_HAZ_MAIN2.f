      program haz45_4

c     Probabilisitic Seismic Hazard Program (PSHA)

      implicit none
      include 'pfrisk.h'
      include 'declare1.h'

c     Clustered sequences (source type 8), see 36_Cluster.f:
c       clPass   0 = ordinary source, 1 = probability pass, 2 = contribution pass
c       Pmem     member exceedance probabilities P_k (pass 1)
c       Pclu     cluster probability P_c = 1 - prod_k (1 - P_k), for each
c                segmentation model of the cluster's fault system
c       sMem     each member's factor in pass 2: the allocation factors
c                s = P_c / sum_k P_k of the models that include the member,
c                weighted by the models' weights, over the member's total
c                segmentation weight
c       iClV     number of out1 blocks of cluster curves written for this site
      real*8, allocatable :: Pmem(:,:,:,:,:,:,:), sMem(:,:,:,:,:,:,:),
     1                       Pclu(:,:,:,:,:,:,:)
      integer clPass, iClFirst, iClLast, kMem, nClMem, nClInten,
     1        nClAtten, nClParam, nClFtype, nClWidth, nClSeg, iClV

c     Write Program information to the screen.
      write (*,*) '*********************************'
      write (*,*) '*      Seismic Hazard Code      *'
      write (*,*) '*         Release 45.4          *'
      write (*,*) '*       Under Development       *'
      write (*,*) '*         October 2026          *'
      write (*,*) '*********************************'
      write (*,*)

      write (*,*) 'Enter number of cases to run in batch mode.'
      write (*,*) '         (For a single run enter 0)'
      read (*,*) bnum
      if (bnum .eq. 0) then
          bnumflag = 0
          bnum = 1
      else
          bnumflag = 1
      endif

      if ( bnumflag .eq. 1) then
         write (*,*) 'Enter the batch mode filename.'
         read (*,'(a80)') filebmode
         open (77,file=filebmode,status='old')
      endif

c     initial read flag for GMPE Tables
      do i=1,17
        iflagRead(i) = 0
      enddo

c     Start loop over number of batch mode runs
      do 2000 ibnum=1,bnum
         if (bnumflag .eq. 1) then
            write (*,*) 'Looping over Number of Batch Mode Cases: ', ibnum, bnum
         endif

c     Read Run File
      call S21_RdInput ( nProb, nAttenType, nAtten, jcalc, specT, sigTrunc,
     1               gmScale, dirFlag, PCflag, nInten, testInten, lgTestInten,
     2               psCorFlag, minlat, maxlat, minlong, maxlong, distmax,
     3               nMagBins, magBins, nDistBins, distBins, nepsBins, epsBins,
     4               nXcostBins, xcostBins, soilAmpFlag, gm_wt, sigvaradd,
     5               sCalc, sigfix, bnumflag, cfcoefRrup, cfcoefRjb,
     6               coefcountRrup, coefcountRjb, iMixture, version, starttime )

c     read fault File
      call S29_Rd_Fault_Data ( nFlt, fName, minMag, magStep, xStep,
     1     yStep, segModelWt, rateParam, rateParamWt, beta,
     2     magRecur, magRecurWt, faultWidth, faultWidthWt, maxMag,
     3     maxMagWt, fLong, fLat, fZ, dip, nfp, nMag, ftype,
     4     sourceType, nRupArea, coeff_area, sigArea, nRupWidth,
     5     coeff_width, sigWidth, nParamVar, iCoor, minDepth,
     6     fIndex, probAct, nWidth, mpdf_param, al_segWt, attenType,
     7     sampleStep, grid_a, grid_dlong, grid_dlat, grid_n,
     8     grid_long, grid_lat, grid_top, minlat, maxlat, minlong,
     9     maxlong, scaleRate, fsys, mMagout, mMagoutWt, fltDirect,
     1     synchron, nsyn_Case, synjcalc, synmag, syndistRup,
     2     syndistJB, synDistSeismo, synHypo, synftype, synhwflag,
     3     synwt, RateType, iDepthModel, depthParam, nMaxmag2, segWt1,
     4     faultFlag, nDD, nFtype, ftype_wt,
     5     segModelFlag, nSegModel, segModelWt1, syn_dip,
     6     syn_zTOR, syn_RupWidth, syn_RX, syn_Ry0, magS7, rateS7,
     7     DistS7, DipS7, mechS7, ncountS7, version, VarXstepFlag, VarYstepFlag,
     8     distmaxType, clusterID)

c     Clustered earthquake sequences (source type 8): summary and checks
      call S36_ClusterSetup ( nFlt, clusterID, fName, fsys, probAct,
     1     nParamVar, rateParam, rateParamWt, magRecur, maxMag,
     2     minMag, nProb, specT, PCflag, nSegModel, nWidth, nFtype )

c     Determine actual (run-specific) bounds for tempHaz and allocate it.
c     This replaces the MAXPARAM x MAX_INTEN x MAX_PROB x MAX_ATTEN x MAX_FTYPE
c     compile-time worst case (hundreds of MB of static data, which also had to
c     be cleared for every source and width branch).
      nParamMax = 0
      do iFlt = 1, nFlt
        do iFltWidth = 1, nWidth(iFlt)
          if ( nParamVar(iFlt,iFltWidth) .gt. nParamMax )
     1         nParamMax = nParamVar(iFlt,iFltWidth)
        enddo
      enddo

      nIntenMax = 0
      do iProb = 1, nProb
        if ( nInten(iProb) .gt. nIntenMax ) nIntenMax = nInten(iProb)
      enddo

      nAttenMax = 0
      do iProb = 1, nProb
        do jT = 1, 4
          if ( nAtten(iProb,jT) .gt. nAttenMax )
     1         nAttenMax = nAtten(iProb,jT)
        enddo
      enddo

      if ( allocated(tempHaz) ) deallocate ( tempHaz )
      allocate ( tempHaz(nParamMax,nIntenMax,nProb,nAttenMax,MAX_FTYPE) )

C *** SPEEDUP EDIT (Stage 2): hoist accumulator starts at zero; it is reset
C     to zero entry by entry each time it is applied (end of each magnitude)
      hG = 0.d0
C *** END SPEEDUP EDIT

c     Loop Over Number of Sites
      read (13,*,err=2100) nSite
      do 1000 iSite = 1, nSite

c      Read site coordinates and properties
       read (13,*,err=2101) SiteX, SiteY, vs, depthvs10, depthvs15, D25, vrup, forearc

c      Read site-specific site amplification
       if ( soilAmpFlag .eq. 1 ) then
        call S33_RdSoilAmpModel ( refPeriod, nRefPer, RefGM, nRefGM, RefGM_mag, nRefMag, amp )
       endif

c      Open Output1 file which will contain the individual hazard curves.
       read (13,'( a80)',err=2102) file1
       open (11,file=file1,status='unknown')
       write (11, *) '45.3 Haz45.3 Out1 file - individual hazard curves'

c      Open Output2 file for Probability of Magnitude Density for each parameter combination.
       read (13,'( a80)',err=2104) file2
       open (17,file=file2,status='unknown')
       write (17, *) '45.3 Haz45.3 Out2 file - magnitude recurrence curves for SSC fractile code'
       write (17,'(i15, 3x,''nFlt, nWidth'')') nFlt
       write (17,'( 20i5)') (nWidth(iFlt), iFlt=1,nFlt)

c      Open Output3 file
       read (13,'( a80)',err=2106) file1
       open (12,file=file1,status='unknown')
       write (12, *) '45.3 Haz45.3 Out3 file - mean hazard curves'

c      Open Output4 file
       read (13,'( a80)') file1
       open (14,file=file1,status='unknown')
       write (14, *) '45.3 Haz45.3 Out4 file - deaggregation output'

c      Open Output5 file which will contain the individual source hazard curves averaged over GMPEs.
       read (13,'( a80)',err=2105) file1
       open (27,file=file1,status='unknown')
       write (27, *) '45.3 Haz45.3 Out5 file - SSC tornado output file'

c      Open Output6 file which will contain the individual GMPE hazard curves over SSC models.
       read (13,'( a80)',err=2106) file1
       open (28,file=file1,status='unknown')
       write (28, *) '45.3 Haz45.3 Out6 file - GMC tornado output file'

c      Open Output7 file which will contain deaggregations for each source.
       read (13,'( a80)',err=2107) file1
       open (29,file=file1,status='unknown')
       write (29, *) '45.3 Haz45.3 Out7 file - deaggregation by source'

c      Initialize Haz Arrays to zero
       call S23_InitHaz ( Haz )
       call S23_InitHaz ( magbar1 )
       call S23_InitHazBins ( HazBins )
       call S23_InitHazBinsX ( HazBinsX )

c      Initialize Mean Deagg values for this site
       call S23_InitDeagg ( m_bar, d_bar, e_bar, Xcost_bar )
       call S23_InitDeagg2 ( m_bar_s, rrup_bar_s, rjb_bar_s, rx_bar_s, e_bar_s,
     1                   rSeismo_bar_s, ry0_bar_s, mag_bar_s, ftype_bar_s,
     2                   hypodepth_bar_s, dipavgd_bar_s, ztor_bar_s,
     3                   thetasite_bar_s, rupwidth_bar_s, rHypo_bar_s )

C      Initialize temp hazard array for GM sensitivity
       call S23_Init_tempHaz2 (tempHaz2)

c      Sum Over Number of sources
c      The members of a cluster (source type 8) are processed twice: pass 1
c      computes each member's exceedance probability, and pass 2 adds the
c      members' contributions with the allocation factor (see 36_Cluster.f).
       clPass = 0
       iClV = 0
       iFlt = 0
 890   iFlt = iFlt + 1
       if ( iFlt .gt. nFlt ) goto 901

c       First member of a cluster: start the probability pass
        if ( clusterID(iFlt) .gt. 0 .and. clPass .eq. 0 ) then
          clPass = 1
          iClFirst = iFlt
          iClLast = iFlt
          do while ( iClLast .lt. nFlt )
            if ( clusterID(iClLast+1) .ne. clusterID(iFlt) ) exit
            iClLast = iClLast + 1
          enddo
          call S36_ClusterSize ( iClFirst, iClLast, nProb, nInten,
     1         nAtten, attenType, nParamVar, nFtype, nWidth, nClMem,
     2         nClInten, nClAtten, nClParam, nClFtype, nClWidth )
          nClSeg = nSegModel(iClFirst)
          allocate ( Pmem(nClMem,nClInten,nProb,nClAtten,nClParam,nClFtype,nClWidth),
     1               sMem(nClMem,nClInten,nProb,nClAtten,nClParam,nClFtype,nClWidth),
     2               Pclu(nClInten,nProb,nClAtten,nClParam,nClFtype,nClWidth,nClSeg) )
          Pmem = 0.0d0
        endif
        if ( clPass .gt. 0 ) kMem = iFlt - iClFirst + 1

        write (*,'(2x,''Site = '',i4,'', '',''iFlt ='',4i5)') iSite, iFlt, nFlt, sourceType(iFlt), ibnum

c       Write fault width weights to output2 file
        if ( clPass .ne. 1 )
     1  write (17,'( 20f10.6)') (faultWidthWt(iFlt,iWidth),iWidth=1,nWidth(iFlt))

c       Initialize p1_sum (check of the integration over all source pdfs)
        p1_sum = 0.

c       Initialize closest distance for each distance metric
        call S23_InitMinDis (iFlt, nWidth(iFlt), MinRrup_temp, MinRjb_temp, MinSeismo_temp, SourceDist)

c       Loop over alternative Fault Widths (epistemic)
c       (This changes the geometry of the source)
        do 860 iFltWidth=1,nWidth(iFlt)

C         Initialize temp hazard array for this source
C         (not used in the cluster probability pass)
          if ( clPass .ne. 1 ) then
            call S23_Init_tempHaz ( tempHaz, nParamMax, nIntenMax, nProb,
     1                             nAttenMax, MAX_FTYPE )
            call S23_Init_tempHaz1 ( tempHaz1 )
          endif

c        Set bottom of fault for standard faults (source type 1)
          if ( sourceType(iFlt) .eq. 1. ) then
            call S20_SetFltBottom (iCoor, iFlt, nfp, dip(iFlt,iFltWidth),
     1                         faultWidth(iFlt,iFltWidth), fZ, flat, flong, nDD)
          endif

c        Convert Long, Lat to x,y in km and put into new array (1-D)
           call S20_ConvertCoordinates2 (nfp(iFlt), iFlt, iCoor, grid_n(iFlt),
     1           sourceType(iFlt), nDD(iFlt), siteX, siteY, fLat, fLong, fZ,
     2           grid_lat, grid_long, grid_dlat, grid_dlong, nPts, xFlt, yFlt,
     3           zFlt, grid_x, grid_y, grid_dx, grid_dy, x0, y0, z0)

c        Turn fault into a grid
         if ( sourceType(iFlt) .eq. 1 .or. sourceType(iFlt) .eq. 5 .or. sourceType(iFlt) .eq. 6 ) then
           call S20_calcFltGrid ( xFlt, yFlt, zFlt, nfp(iFlt), nDD(iFlt), fltGrid_x, fltGrid_y,
     1               fltGrid_z, nfltGrid, fltGrid_a, fltGrid_w, x0, y0, z0,
     2               fltGrid_Rrup, fltGrid_Rjb, faultArea, faultLen, aveWidth,
     3               xStep(iFlt), fltGrid_fLen, fltGrid_x1, fltGrid_y1,
     4               fltGrid_z1, fltGrid_x2, fltGrid_y2, fltGrid_z2, fltGrid_x3,
     5               fltGrid_y3, fltGrid_z3, fltGrid_x4, fltGrid_y4, fltGrid_z4 )
         endif
         if ( (sourceType(iFlt) .eq. 1 .or. sourceType(iFlt) .eq. 5) .and. clPass .ne. 1 )
     1      write (18,'( 2x,''fault area (km^2) = '',e12.3)') faultArea

c        Set Sampling of Rupture Area and Rupture Width Distributions
         call S23_initRup ( sigArea, nRupArea, sigMaxArea, areaStep, iFlt)
         call S23_initRup ( sigWidth, nRupWidth, sigMaxWidth, widthStep, iFlt)

c        Compute horizontal distance density function for areal sources (polygons or gridded seismicity)
         if ( sourceType(iFlt) .eq. 2 ) then
           call S14_CalcDistDensity (nPts, xFlt, yFlt, distDensity,
     1         xStep(iFlt), nLocXAS, x0, y0, sampleStep(iFlt), VarXstepFlag(iFlt))
         elseif ( sourceType(iFlt) .eq. 3 ) then
           call S14_CalcDistDensity1 ( iFlt, grid_a, grid_x, grid_y, grid_dx,
     1             grid_dy, grid_n, distDensity, xStep(iFlt), nLocXAS,
     2             x0, y0, sampleStep(iFlt), VarXstepFlag(iFlt))
         elseif ( sourceType(iFlt) .eq. 4 ) then
           call S14_CalcDistDensity2 ( iFlt, grid_a, grid_n, distDensity2 )
         endif

c        Compute activity rate: N(Mmin)
           call S31_Set_Rates ( sourceType(iFlt), nParamVar, MagRecur, rate, beta, minMag,
     1         maxMag, iFlt, iFltWidth, faultArea, coeff_area, RateParam, mpdf_param, magStep,
     2         RateType, charMeanMo, expMeanMo )

c        Intergrate Over Magnitude (from minMag to maxMag) (Aleatory)

c        Set nMag(iFlt) = ncount for Source Type 7 case
         if (sourceType(iFlt) .eq. 7) then
           write (*,*) 'Number of sources for SourceType7 = ', iFlt,nMag(iFlt)
         endif

         do 800 iMag = 1, nMag(iFlt)
           if (sourceType(iFLt) .ne. 7) then
             mag = minMag(iFlt) + (iMag-0.5) * magStep(iFlt)
           elseif (sourceType(iFlt) .eq. 7) then
             mag = magS7(iFlt,iMag)
           endif
           magTotal = mag

c         Set the magnitude bin for deagregating
          call S23_SetBin ( nMagBins, magBins, mag, iMagBin )

c         Compute Probability of mag between mag-magStep/2 and mag+magStep/2
c         using the magnitude pdf for each parameter variation
            call S25_magProb (sourceType(iFlt), mag, maxMag, minMag, magStep, beta,
     1                    iFlt, iFltWidth, pMag, nParamVar, nWidth, MagRecur,
     2                    mpdf_param, ExpMeanMo, CharMeanMo, rup1_flag)

C *** SPEEDUP EDIT (Stage 1, zero-pMag magnitude skip)
C     If pMag is zero for every epistemic branch at this magnitude, every
C     hazard and deaggregation term below is zero. The rupture geometry and
C     distance loops still run (so minimum-distance outputs are unchanged);
C     only the ground-motion and accumulation loops are skipped.
          allZeroMag = .false.
          if (sourceType(iFlt) .ne. 7) then
            allZeroMag = .true.
            do iParam=1,nParamVar(iFlt,iFltWidth)
              if ( pMag(iParam,iFltWidth) .ne. 0. ) allZeroMag = .false.
            enddo
          endif
C *** END SPEEDUP EDIT

C *** SPEEDUP EDIT (Stage 2, recurrence-loop hoist): per-magnitude branch sums
C     The marginal rate in the original inner loop is
C        mHaz = rate(iParam)*pRock*p1*probAct,  p1 = pMag(iParam)*pAleat,
C     where pAleat (rupture area/width, location, hypocenter and synchronous-
C     rupture probabilities) does not depend on iParam. Sums over iParam are
C     therefore formed once per magnitude here, and the inner loop over iParam
C     is replaced by these sums. Source type 7 and PC runs keep the original loop.
          if (sourceType(iFlt) .ne. 7) then
            do iParam=1,nParamVar(iFlt,iFltWidth)
              hRate(iParam) = dble(rate(iParam,iFltWidth))
     1                      * dble(pMag(iParam,iFltWidth)) * dble(probAct(iFlt))
            enddo
            do iFtype=1,nFtype(iFlt)
              hWtRate(iFtype) = 0.d0
              hWtP1(iFtype) = 0.d0
              do iParam=1,nParamVar(iFlt,iFltWidth)
                wt = RateParamWt(iFlt,iParam,iFltWidth)
     1             * magRecurWt(iFlt,iParam,iFltWidth)
     2             * faultWidthWt(iFlt,iFltWidth)
     2             * maxMagWt(iFlt,iParam,iFltWidth)
     2             * ftype_wt(iFlt,iFtype)
                wtout(iFlt,iParam,iFltWidth,iFtype) = wt
                hWtP1(iFtype) = hWtP1(iFtype) + wt*pMag(iParam,iFltWidth)
                hWtRate(iFtype) = hWtRate(iFtype)
     1                + wt*al_segWt(iFlt)*segwt1(iFlt)*hRate(iParam)
              enddo
            enddo
          endif
          hUsed = .false.
C *** END SPEEDUP EDIT

c         Echo magnitude integration step over magnitude to the screen
c         as a check of the programs progress.
          if (sourceType(iFlt) .ne. 7) then
            write (*,'( 2x,2I5,f10.3)') iflt, ifltWidth, mag
            if ( clPass .ne. 1 ) write (18,'( 2x,2I5,f10.3)') iflt, ifltWidth, mag
          elseif (sourceType(iFlt) .eq. 7) then
            nn10000 = (iMag/10000)*10000
            if (nn10000 .eq. iMag) then
              write (*,'( 2x,2I10,f10.3)') iMag, ncountS7(iFlt), mag
            endif
          endif

c         Intergrate Over Rupture Area for this mag (aleatory)
          do 750 iArea = 1, nRupArea(iFlt)

c          Compute Rupture Area and Probability of Rupture Area
           call S27_rupArea ( sourceType(iFlt), mag, coeff_area, sigArea,
     1          areaStep, sigMaxArea, rupArea, pArea, iFlt, iArea )

c          Intergrate Over Rupture Width for this mag (aleatory)
           do 700 iWidth = 1, nRupWidth(iFlt)

c           Compute Rupture Width and Probability of Rupture Width
            call S27_rupWidth ( sourceType(iFlt), mag, rupArea, coeff_width,
     1           sigWidth, widthStep, sigMaxWidth, rupWidth, pWidth, iFlt, iWidth)

        call S28_RupDims (sourcetype(iFlt), rupWidth, aveWidth, rupArea, faultLen,
     1                faultWidth(iFlt,iFltWidth), dip(iFlt,iFltWidth), nLocYST1,
     2                yStep(iFlt), rupLen)

        call S28_nLocXcells (sourceType(iFlt), nLocXAS, grid_n(iFlt), nfltgrid, fltgrid_w,
     1                   rupWidth, fltgrid_a, ruparea, nLocYST1, nLocX, n1AS, n2AS)

c           Integrate Over Rupture Location - along strike (aleatory)
c           This is along strike for faults and epicentral distance for source zones
            iDepthFlag = 0

            do 650 iLocX = 1, nLocX

               call S28_nLocYcells (iLocX, n1AS, sourceType(iFlt), nLocX, distDensity, xStep(iFlt),
     1                          faultWidth(iFlt,iFltWidth), yStep(iFlt), distDensity2, grid_x,
     2                          grid_y, x0, y0, nLocY, pLocX, r_horiz, VarXstepFlag(iFlt),
     3                          VarYstepFlag(iFlt))

                  if ( pLocX .eq. 0. ) then
                    goto 650
                  endif

c            set the probabilities for the depths
             if ( iDepthFlag .eq. 0 ) then
               call S13_CalcDepthProb ( iDepthModel(iFlt), depthParam, iFlt, pLocY,
     1              sourceType(iFlt), nLocY, yStep(iFlt), zFlt(1,1),
     2              faultWidth(iFlt,iFltWidth), rupWidth, dip(iFlt,iFltWidth) )
               if (sourceType(iFlt) .eq. 1 .or. sourceType(iFlt) .eq. 7) then
                 iDepthFlag = 1
               endif
               if (sourceType(iFlt) .eq. 2 .and. VarYstepFlag(iFlt) .eq. 0) then
                 iDepthFlag = 1
               endif
               if (sourceType(iFlt) .eq. 3 .and. VarYstepFlag(iFlt) .eq. 0) then
                 iDepthFlag = 1
               endif
               if (sourceType(iFlt) .eq. 4 .and. VarYstepFlag(iFlt) .eq. 0) then
                 iDepthFlag = 1
               endif
             endif

c            Integrate Over Rupture Location - Down Dip (aleatory)
             do 600 iLocY = 1, nLocY

c            Find the Closest Distances for this rupture
c            Pass along fault grid locations for calculation of HW and Rx values within CalcDist subroutine.
             call S14_CalcDist (sourceType(iFlt), pscorflag, nFltGrid, n1AS, iLocX, iLocY, n2AS,
     1             iFltWidth, iFlt, iMag, ystep(iFlt), grid_top, RupWidth, RupLen, r_horiz, mindepth(iFlt),
     2             fltGrid_x, fltGrid_y, fltGrid_z, fltgrid_x1, fltgrid_y1, fltgrid_z1,
     3             fltgrid_x2, fltgrid_y2, fltgrid_z2, fltgrid_x3, fltgrid_y3, fltgrid_z3,
     4             fltgrid_x4, fltgrid_y4, fltgrid_z4, fltGrid_Rrup, fltGrid_Rjb, dip, dipS7,
     5             distS7, HWFlag, n1, n2, icellRupstrike, icellRupdip, hypoDepth, distJB,
     6             distRup, ZTOR, distSeismo, distepi, disthypo, dipavgd, Rx, Ry, Ry0)

c            Set minimum distances for output files.
             call S14_Set_MinDist (sourceType(iFlt), iFlt, iFltWidth, distRup, distJB, distSeismo,
     1                         SourceDist, MinRrup_temp, MinRjb_temp, MinSeismo_temp)

c             Check if the distance is greater than Max dist for hazard
c             (max distance for this source's max distance type)
              if ( distRup .gt. distmax(distmaxType(iFlt)) .and.
     1             distJB .gt. distmax(distmaxType(iFlt)) ) goto 600

c             Set the distance bin for de-aggregation (using rupture distance)
              call S23_SetBin ( nDistBins, distBins, distRup,iDistBin)

C *** SPEEDUP EDIT (Stage 1, zero-pMag magnitude skip)
              if ( allZeroMag ) goto 600
C *** END SPEEDUP EDIT

c             Loop over ftypes
              do 561 iFtype=1,nFtype(iFlt)

c             Loop Over Number of Problems (e.g. spectral periods)
              do 560 iProb=1,nProb
               jType = attenType(iFlt)
C *** SPEEDUP EDIT (Stage 1, GMM cache): new rupture/ftype/period -> empty cache
               nCacheMed = 0
               nCacheSig = 0
C *** END SPEEDUP EDIT
C *** SPEEDUP EDIT (Stage 2): per-rupture epsilon-independent hazard sum
               do jInten=1,nInten(iProb)
                 hHsum(jInten) = 0.d0
               enddo
C *** END SPEEDUP EDIT
               do 550 iAtten = 1,nAtten(iProb,jType)

c               Check for negative jcalc values which will set the corresponding sigma to
C               either a fixed value or sigma from another model.
                sigflag = 0
                if (jcalc(iProb,jType,iAtten) .lt. 0) then
                   jcalc1 = abs(jcalc(iProb,jType,iAtten) )
                   scalc1 = scalc(iProb,jtype,iAtten)
                   sigfix1 = sigfix(iProb,jType,iAtten)
C                  Check for either fixed sigma value (scalc1<0) or other sigma model
                   if (scalc1 .lt. 0) then
                     sigflag = 2
                   else
                     sigflag = 1
                   endif
                else
                   jcalc1 = jcalc(iProb,jType,iAtten)
                endif

c              Compute the median and sigma of the ground motions
               if (sourceType(iFlt) .ne. 7) then
C *** SPEEDUP EDIT (Stage 1, GMM cache): reuse the median/sigma already computed
C     for this jcalc on this rupture. S11_meanInten results depend only on
C     jcalc and the rupture/site inputs (iAtten, iProb, jType are used only to
C     store period1), so the cached values are identical to a fresh call.
                 do iCache = 1, nCacheMed
                   if ( cacheMedJ(iCache) .eq. jcalc1 ) then
                     lgInten = cacheMedLn(iCache)
                     sigmaY  = cacheMedSg(iCache)
                     goto 5101
                   endif
                 enddo
                 call S11_meanInten ( distRup, distJB, distSeismo,
     1               HWFlag, mag, jcalc1, specT(iProb),
     2               lgInten, sigmaY, ftype(iFlt,iFtype), attenName, period1,
     3               iAtten, iProb, jType, vs, hypodepth, intflag, AR, dipavgd,
     4               disthypo, depthvs10, depthvs15, D25, tau,
     5               zTOR, theta_site, RupWidth, vs30_class, forearc, Rx, phi,
     6               cfcoefrrup, cfcoefrjb, Ry0 )
                 if ( nCacheMed .lt. MAX_GMCACHE ) then
                   nCacheMed = nCacheMed + 1
                   cacheMedJ(nCacheMed)  = jcalc1
                   cacheMedLn(nCacheMed) = lgInten
                   cacheMedSg(nCacheMed) = sigmaY
                 endif
 5101            continue
C *** END SPEEDUP EDIT
               elseif (sourceType(iFlt) .eq. 7) then
                 call S11_meanInten ( distRup, distJB, distSeismo,
     1               HWFlag, mag, jcalc1, specT(iProb),
     2               lgInten, sigmaY, mechS7(iFlt,iMag), attenName, period1,
     3               iAtten, iProb, jType, vs, hypodepth, intflag, AR, dipavgd,
     4               disthypo, depthvs10, depthvs15, D25, tau,
     5               zTOR, theta_site, RupWidth, vs30_class, forearc, Rx, phi,
     6               cfcoefrrup, cfcoefrjb, Ry0 )
              endif

c               Add epistemic uncertainty term (constant shift) to median
                lgInten = lgInten + gmScale(iProb,jType,iAtten)

C               Second Median Call for Complex/Splay Rupture Median
C               Ground Motions will be SRSS with main rupture median ground motions.
                if (synchron(iFlt) .eq. 0) then
                  nSyn_Case(iFlt) = 1
                  synwt(iFlt,1)= 1.0
                endif

c               Temp: set probability of syn rupture to unity
c               Later, this will be an input
                probSyn = 1.

c               Loop over synchronous ruptures (aleatory)
                do 549 isyn=1,nSyn_Case(iFlt)
                 if (synchron(iFlt) .gt. 0 .and. rup1_flag .eq. 1) then
C                   Check to see if same Attenuation model (i.e. -999) is used for synchron rupture
C                   of if different model is defined based on input in fault file.
                    if (synjcalc(iFlt) .ne. -999) then
                       jcalc1 = synjcalc(iFlt)
                    endif
                  call S11_meanInten ( synDistRup(iFlt,isyn), syndistJB(iFlt,isyn),
     1                syndistSeismo(iFlt,isyn),
     2                synhwflag(iFlt,isyn), synmag(iFlt,isyn), jcalc1, specT(iProb),
     3                lgIntenS, temp, synftype(iFlt,isyn), attenName, period1,
     4                iAtten, iProb, jType, vs, synhypo(iflt,1), intflag, AR, syn_dip(iFlt,isyn),
     5                disthypo, depthvs10, depthvs15, D25, tau,
     6                syn_zTOR(iFlt,isyn), theta_site, syn_RupWidth(iFlt,isyn),
     7                vs30_class, forearc, syn_Rx(iFlt,isyn), phi,
     8                cfcoefrrup, cfcoefrjb, syn_Ry0(iFlt,isyn) )
c                 Compute SRSS of median
                  lgInten = 0.5* alog( exp(lgInten)**2 + exp(lgIntenS)**2 )
                endif

C               Second call get GMPE for different sigma model
                if (sigflag .eq. 1) then
                  if (sourceType(iFlt) .ne. 7) then
C *** SPEEDUP EDIT (Stage 1, GMM cache): reuse sigma already computed for this scalc
                    do iCache = 1, nCacheSig
                      if ( cacheSigJ(iCache) .eq. scalc1 ) then
                        sigmaY = cacheSigSg(iCache)
                        goto 5102
                      endif
                    enddo
                    call S11_meanInten ( distRup, distJB, distSeismo,
     1               hwflag, mag, scalc1, specT(iProb),
     2               temp, sigmaY, ftype(iFlt,iFtype), sigmaName, period1,
     3               iAtten, iProb, jType, vs, hypodepth, intflag, AR, dipavgd,
     4               disthypo, depthvs10, depthvs15, D25, tau,
     5               zTOR, theta_site, RupWidth, vs30_class, forearc, Rx, phi,
     6               cfcoefrrup, cfcoefrjb, Ry0 )
                    if ( nCacheSig .lt. MAX_GMCACHE ) then
                      nCacheSig = nCacheSig + 1
                      cacheSigJ(nCacheSig)  = scalc1
                      cacheSigSg(nCacheSig) = sigmaY
                    endif
 5102               continue
C *** END SPEEDUP EDIT
                  elseif (sourceType(iFlt) .eq. 7) then
                    call S11_meanInten ( distRup, distJB, distSeismo,
     1               HWFlag, mag, jcalc1, specT(iProb),
     2               lgInten, sigmaY, mechS7(iFlt,iMag), sigmaName, period1,
     3               iAtten, iProb, jType, vs, hypodepth, intflag, AR, dipavgd,
     4               disthypo, depthvs10, depthvs15, D25, tau,
     5               zTOR, theta_site, RupWidth, vs30_class, forearc, Rx, phi,
     6               cfcoefrrup, cfcoefrjb, Ry0 )
                  endif

c               Check if a constant, user input sigma, is selected
                elseif (sigflag .eq. 2) then
                  sigmaY = sigfix1
                endif

C               Adjust sigma value if sigvaradd .ne. 0.0
                if (sigvaradd(iProb,jType,iAtten) .ne. 0.0) then
                  sigmaY = sqrt(sigmaY*sigmaY + sigvaradd(iProb,jType,iAtten) )
                endif

c               Check that sigma is not less than zero (0.0001)
                if (sigmaY .lt. 0.0001 ) sigmaY = 0.0001

c               Reset SigmaTotal variable
                sigmaTotal = sigmaY

c               For the PC method, get the epistemic uncertainty in the median gm for this scenario
                if (PCflag(iProb) .eq. 1) then
                  call S09_get_sigma_mu (distRup, mag, HWflag, ftype(iFlt,iFtype),
     1                                   specT(iProb), iProb, jType, sigma_mu)
                endif


c               Set values for use with directivity
                lgInten0 = lgInten
                sigma0 = sigmaTotal

c               Set default for no randomization of hypocenters
                nHypoX = 1
                pHypoX = 1.
                nHypoXStep = 1
                nHypoZ = 1
                pHypoZ = 1.
                nHypoZStep = 1

C               Application of Directivity model.
                if ( fltDirect(iFlt) .eq. 1 .and. dirflag(iProb) .ge. 1
     1              .and. mag .gt. 5.6 .and. specT(iProb) .ge. 0.50 ) then
                 dirFlag1 = 1
                 if ( dirflag(iProb) .lt. 100 ) then
                   nHypoX = 9
                   nHypoZ = 9
                   pHypoX = 1./ 9.
                   pHypoZ = 1./ 9.
                 endif
                else
                 dirFlag1 = 0
                endif

c               Loop over hypocenter location along strike (aleatory)
                do 540 iHypoX=1,nHypoX,nHypoXstep
                 fs = float(iHypoX) / (nHypoX + 1.)

c                Loop over hypocenter location down dip (aleatory)
                 do 530 iHypoZ=1,nHypoZ,nHypoZstep
                  fd = float(iHypoZ) / (nHypoZ + 1.)

C                 Call to the rupture directivity Subroutine if applicable
                  if ( dirflag1 .eq. 1) then
                    call S16_Directivity ( dirFlag(iProb), specT(iProb), DistRup, zTOR,
     1                 x0, y0, z0, Rx, Ry, Ry0, mag, ftype(iFlt,iFtype), RupWidth,
     2                 RupLen, dipavgd, HWflag, dirMed, dirSigma, fltgrid_x,
     3                 fltgrid_y, fltgrid_z, n1, n2, fs, fd, dpp_flag,
     4                 iLocX, iLocY)

c                       write (44,'( 6f8.2 )') mag, RupLen, fs, fd, dirMed, dirSigma

c                   Add directivity to median and sigma
                    lgInten = lgInten0 + dirMed
                    if ( dirSigma .lt. 0. ) then
                      t1 = sigma0**2 - dirSigma**2
                    else
                      t1 = sigma0**2 + dirSigma**2
                    endif
                    if ( t1 .lt. 0. ) t1 = 0.01
                    sigmaTotal = sqrt( t1 )
                  endif

C *** SPEEDUP EDIT (Stage 2): aleatory probability of this rupture without pMag
                  hPAleat = dble(pArea)*dble(pWidth)*dble(pLocX)*dble(pLocY(iLocY))
     1                    *dble(phypoX)*dble(phypoZ)*dble(probSyn)*dble(synwt(iFlt,isyn))
C                 Not for a member of a clustered sequence (clPass > 0): its
C                 probability pass and allocation factor depend on the branch,
C                 so the members use the original loop below
                  hHoist = ( sourceType(iFlt) .ne. 7 .and. PCflag(iProb) .eq. 0
     1                       .and. clPass .eq. 0 )
C *** END SPEEDUP EDIT

c                  Loop over test ground motion values
                   do 510 jInten = 1, nInten(iProb)

c                    Check for PC method, calculate the PC coefficients for this scenario and z value
                     if (PCflag(iProb) .eq. 1) then
                       call S09_calc_PC_coeff_fullCorr (nPC, lgInten, sigma_mu, lgTestInten(iProb,jInten),
     1                     sigmaTotal, jInten, sigTrunc(iProb), iMixture(iProb,jType,iAtten), PC_Coef)
                       pRock = PC_Coef(1,jInten)
                     else

c                     Traditional method (not PC method)
c                     Check for mixture model distribution, compute Probability of exceeding test
                      if ( iMixture(iProb,jType,iAtten)  .eq. 0 ) then
                        pRock = pxceed3 (lgInten, lgTestInten, sigmaTotal, iProb,jInten,sigTrunc(iProb))
                      else
                       sigma1 = sigmaTotal*0.8
                       sigma2 = sigmaTotal*1.2
                       pRock = 0.5 * pxceed3 (lgInten, lgTestInten, sigma1, iProb,jInten,sigTrunc(iProb))
     1                        +0.5 * pxceed3 (lgInten, lgTestInten, sigma2, iProb,jInten,sigTrunc(iProb))
                      endif

                     endif

c                   Compute number of standard deviations (epsilon) to reach test level
                    if (sigmatotal .le. 0.0001) then
                      epsilon1 = 0.0
                    else
                      epsilon1 = ( lgtestInten(iProb,jInten)-lgInten )/sigmaTotal
                    endif

c                   Set bin for epsilon deaggregation
                    call S23_SetBin_Eps ( nEpsBins, epsBins, epsilon1, iepsBin)

C *** SPEEDUP EDIT (Stage 2): replacement for the iParam loop (see per-magnitude sums)
                    if ( hHoist ) then
                      hQ = prock*hPAleat
C                     branch-resolved part: applied to tempHaz/tempHaz1 per magnitude
                      hG(jInten,iProb,iAtten,iFtype) = hG(jInten,iProb,iAtten,iFtype) + hQ
                      hUsed = .true.
C                     p1_sum check, same condition as the original loop
                      if ( iAtten .eq. 1 .and. iProb .eq. 1 .and. jInten .eq. 1 ) then
                        p1_sum = p1_sum + hWtP1(iFtype)*hPAleat
                      endif
C                     hH = sum over iParam of mHaz*wt;  hHw = sum of mHaz*wt1
                      hH  = hQ*hWtRate(iFtype)
                      hHw = hH*gm_wt(iProb,jType,iAtten)
C                     GMC tornado sum
                      tempHaz2(jType,jInten,iProb,iAtten) = hH
     1                        + tempHaz2(jType,jInten,iProb,iAtten)
C                     epsilon-dependent deaggregation terms
                      HazBins(iMagBin,iDistBin,iEpsBin,iProb,jInten) =
C *** PRECISION EDIT: add in double precision (was  + real(hHw) )
     1                      HazBins(iMagBin,iDistBin,iEpsBin,iProb,jInten) + hHw
                      e_bar(iProb,jInten) = e_bar(iProb,jInten) + hHw*epsilon1
                      e_bar_s(iFlt,iProb,jInten) = e_bar_s(iFlt,iProb,jInten) + hHw*epsilon1
C                     epsilon-independent terms: applied once per rupture after the GMM loop
                      hHsum(jInten) = hHsum(jInten) + hHw
                      goto 510
                    endif
C *** END SPEEDUP EDIT

c                   Loop over parameter variations (epistemic)
                    do 500 iParam=1,nParamVar(iFlt,iFltWidth)

c                    Set the weight for this set of parameters (epistemic)
                     wt = RateParamWt(iFlt,iParam,iFltWidth)
     1                 * magRecurWt(iFlt,iParam,iFltWidth)
     2                 * faultWidthWt(iFlt,iFltWidth)
     2                 * maxMagWt(iFlt,iParam,iFltWidth)
     2                 * ftype_wt(iFlt,iFtype)

c                    Set up weight array for later output.
                     wtout(iFlt,iParam,iFltWidth,iFtype) = wt

C *** SPEEDUP EDIT (Stage 1, zero-pMag branch skip): p1 = 0 for this branch, so
C     every term accumulated below (Haz, deagg, tempHaz, tempHaz1, tempHaz2,
C     p1_sum) would add exactly zero.
                     if ( sourceType(iFlt) .ne. 7 .and.
     1                    pMag(iParam,iFltWidth) .eq. 0. ) goto 500
C *** END SPEEDUP EDIT

c                    Set probability of this earthquake (w/o gm) - (aleatory)
                     p1 = pMag(iParam,iFltWidth)*pArea*pWidth*pLocX*pLocY(iLocY)
     1                    *phypoX*phypoZ*probSyn*synwt(iFlt,isyn)

c                    Set weights and probabilities for SourceType 7
                     if (sourceType(iFlt) .eq. 7) then
                        p1 = 1.0
                        wt = 1.0
                     endif

c                    Sum up probability (w/o ground motion) as a check
                     if ( iAtten .eq. 1 .and. iProb .eq. 1 .and. jInten .eq. 1) then
                       if (sourcetype(iFlt) .ne. 7) then
                         p1_sum = p1_sum + wt*p1
                       elseif (sourcetype(iFlt) .eq. 7) then
                         p1_sum = p1_sum + wt*p1/ncounts7(iFlt)
                       endif
                     endif

c                    Cluster probability pass: add to the member's exceedance
c                    probability only (no rate, no Prob Activity, no output)
                     if ( clPass .eq. 1 ) then
                       Pmem(kMem,jInten,iProb,iAtten,iParam,iFtype,iFltWidth) =
     1                   Pmem(kMem,jInten,iProb,iAtten,iParam,iFtype,iFltWidth) + p1*prock
                       goto 500
                     endif

c                    Add weight for aleatory rupture segmentation
                     wt = wt * al_segWt(iFlt)

c                    Compute Marginal Rate of Occurance
c                    (cluster member: rate of the sequence times the allocation factor)
                     if (sourcetype(iFlt) .ne. 7) then
                       mHaz = rate(iParam,iFltWidth) * prock * p1 * probAct(iFlt)
                       if ( clPass .eq. 2 ) mHaz = mHaz
     1                     * sMem(kMem,jInten,iProb,iAtten,iParam,iFtype,iFltWidth)
                       wt = wt *segwt1(iFLt)
                     elseif (sourcetype(iFlt) .eq. 7) then
                       mHaz = rateS7(iFlt,iMag) * prock * p1 * probAct(iFlt)
                     endif

c                    Add marginal rate of exceed to total
                     wt1 = wt * gm_wt(iProb,jType,iAtten)
                     Haz(jInten,iProb,iFlt) = Haz(jInten,iProb,iFlt) + mHaz*wt1

                     HazBins(iMagBin,iDistBin,iEpsBin,iProb,jInten) =
C *** PRECISION EDIT: add in double precision (was  + real(mHaz*wt1) )
     1                      HazBins(iMagBin,iDistBin,iEpsBin,iProb,jInten) + mHaz*wt1

c  Note: directivity deaggregation was removed from this version
c                     should wt be modified to wt1?
c                     HazBinsX(iXcost,iProb,jInten) = HazBinsX(iXcost,iProb,jInten) + real(mHaz*wt)

c                    Add to mean deagg
                     m_bar(iProb,jInten) = m_bar(iProb,jInten) + mHaz*wt1*magTotal
                     d_bar(iProb,jInten) = d_bar(iProb,jInten) + mHaz*wt1*distRup
                     e_bar(iProb,jInten) = e_bar(iProb,jInten) + mHaz*wt1*epsilon1
                     Xcost_bar(iProb,jInten) = Xcost_bar(iProb,jInten) + mHaz*wt1*Xcost

c                    Add to source deagg
                     m_bar_s(iFlt,iProb,jInten) = m_bar_s(iFlt,iProb,jInten) + mHaz*wt1*magTotal
                     rrup_bar_s(iFlt,iProb,jInten) = rrup_bar_s(iFlt,iProb,jInten) + mHaz*wt1*distRup
                     rjb_bar_s(iFlt,iProb,jInten) = rjb_bar_s(iFlt,iProb,jInten) + mHaz*wt1*distjb
                     rx_bar_s(iFlt,iProb,jInten) = rx_bar_s(iFlt,iProb,jInten) + mHaz*wt1*Rx
                     e_bar_s(iFlt,iProb,jInten) = e_bar_s(iFlt,iProb,jInten) + mHaz*wt1*epsilon1

                     rSeismo_bar_s(iFlt,iProb,jInten) = rSeismo_bar_s(iFlt,iProb,jInten) + mHaz*wt1*distseismo
                     ry0_bar_s(iFlt,iProb,jInten) = ry0_bar_s(iFlt,iProb,jInten) + mHaz*wt1*ry0
                     rHypo_bar_s(iFlt,iProb,jInten) = rHypo_bar_s(iFlt,iProb,jInten) + mHaz*wt1*disthypo
                     hypodepth_bar_s(iFlt,iProb,jInten) = hypodepth_bar_s(iFlt,iProb,jInten) + mHaz*wt1*hypodepth
                     Ztor_bar_s(iFlt,iProb,jInten) = Ztor_bar_s(iFlt,iProb,jInten) + mHaz*wt1*Ztor
                     thetasite_bar_s(iFlt,iProb,jInten) = thetasite_bar_s(iFlt,iProb,jInten) + mHaz*wt1*theta_site
                     rupwidth_bar_s(iFlt,iProb,jInten) = rupwidth_bar_s(iFlt,iProb,jInten) + mHaz*wt1*rupwidth
                     dipavgd_bar_s(iFlt,iProb,jInten) = dipavgd_bar_s(iFlt,iProb,jInten) + mHaz*wt1*dipavgd
                     ftype_bar_s(iFlt,iProb,jInten) = ftype_bar_s(iFlt,iProb,jInten) + mHaz*wt1*ftype(iFlt,iFtype)

c                    Save Marginal Hazard to temp array for fractile output
c                    This is for the total hazard fractiles
c                    (not for a cluster member: its out1 blocks hold zeros, and the
c                    cluster's curves are written after the last member)
                     if ( clPass .ne. 2 )
     1               tempHaz(iParam,jInten,iProb,iAtten,iFtype) = mHaz
     2                        + tempHaz(iParam,jInten,iProb,iAtten,iFtype)

c                    This is for the SSC hazard fractiles
                     tempHaz1(iParam,jInten,iProb,iFtype) = mHaz*gm_wt(iProb,jType,iAtten)
     1                        + tempHaz1(iParam,jInten,iProb,iFtype)

c                    This is for the GMC hazard fractiles
                     tempHaz2(jType,jInten,iProb,iAtten) = mHaz*wt
     1                        + tempHaz2(jType,jInten,iProb,iAtten)

c                    PC method terms
                     if (PCflag(iProb) .eq. 1) then

c                     This is for the total hazard fractiles
c                     Compute Rate of occurance of this scenario
                      if (sourcetype(iFlt) .ne. 7) then
                        rate1 = rate(iParam,iFltWidth) * p1 * probAct(iFlt)
                      elseif (sourcetype(iFlt) .eq. 7) then
                        rate1 = rateS7(iFlt,iMag) * p1 * probAct(iFlt)
                      endif

c                     Sum up the weighted PC coeff for each PC Coeff
                      do iPC = 1,nPC
                        PC_D(iPC,iParam,jInten,iProb,iFtype) =
     1                      PC_D(iPC,iParam,jInten,iProb,iFtype)
     2                      + PC_coef(iPC,jInten)* rate1
                      enddo
                     endif


 500                continue
 510               continue
 530              continue
 540             continue
 549            continue
 550           continue

C *** SPEEDUP EDIT (Stage 2): epsilon-independent hazard and deaggregation
C     sums for this rupture (all multipliers are fixed for the rupture)
               if ( sourceType(iFlt) .ne. 7 .and. PCflag(iProb) .eq. 0 ) then
                do jInten=1,nInten(iProb)
                 hHw = hHsum(jInten)
                 if ( hHw .ne. 0.d0 ) then
                  Haz(jInten,iProb,iFlt) = Haz(jInten,iProb,iFlt) + hHw
                  m_bar(iProb,jInten) = m_bar(iProb,jInten) + hHw*magTotal
                  d_bar(iProb,jInten) = d_bar(iProb,jInten) + hHw*distRup
                  Xcost_bar(iProb,jInten) = Xcost_bar(iProb,jInten) + hHw*Xcost
                  m_bar_s(iFlt,iProb,jInten) = m_bar_s(iFlt,iProb,jInten) + hHw*magTotal
                  rrup_bar_s(iFlt,iProb,jInten) = rrup_bar_s(iFlt,iProb,jInten) + hHw*distRup
                  rjb_bar_s(iFlt,iProb,jInten) = rjb_bar_s(iFlt,iProb,jInten) + hHw*distjb
                  rx_bar_s(iFlt,iProb,jInten) = rx_bar_s(iFlt,iProb,jInten) + hHw*Rx
                  rSeismo_bar_s(iFlt,iProb,jInten) = rSeismo_bar_s(iFlt,iProb,jInten) + hHw*distseismo
                  ry0_bar_s(iFlt,iProb,jInten) = ry0_bar_s(iFlt,iProb,jInten) + hHw*ry0
                  rHypo_bar_s(iFlt,iProb,jInten) = rHypo_bar_s(iFlt,iProb,jInten) + hHw*disthypo
                  hypodepth_bar_s(iFlt,iProb,jInten) = hypodepth_bar_s(iFlt,iProb,jInten) + hHw*hypodepth
                  Ztor_bar_s(iFlt,iProb,jInten) = Ztor_bar_s(iFlt,iProb,jInten) + hHw*Ztor
                  thetasite_bar_s(iFlt,iProb,jInten) = thetasite_bar_s(iFlt,iProb,jInten) + hHw*theta_site
                  rupwidth_bar_s(iFlt,iProb,jInten) = rupwidth_bar_s(iFlt,iProb,jInten) + hHw*rupwidth
                  dipavgd_bar_s(iFlt,iProb,jInten) = dipavgd_bar_s(iFlt,iProb,jInten) + hHw*dipavgd
                  ftype_bar_s(iFlt,iProb,jInten) = ftype_bar_s(iFlt,iProb,jInten) + hHw*ftype(iFlt,iFtype)
                 endif
                enddo
               endif
C *** END SPEEDUP EDIT
 560          continue
 561          continue
 600         continue
 650        continue
 651        continue
 700       continue
 750      continue

C *** SPEEDUP EDIT (Stage 2): apply this magnitude's sums to the branch-resolved
C     fractile arrays:  tempHaz  += hRate(iParam)*sum(pRock*pAleat)
C                       tempHaz1 += hRate(iParam)*gm_wt*sum(pRock*pAleat)
          if ( hUsed ) then
            jType = attenType(iFlt)
            do iFtype=1,nFtype(iFlt)
             do iProb=1,nProb
              do iAtten=1,nAtten(iProb,jType)
               do jInten=1,nInten(iProb)
                hQ = hG(jInten,iProb,iAtten,iFtype)
                if ( hQ .ne. 0.d0 ) then
                 hHw = hQ*gm_wt(iProb,jType,iAtten)
                 do iParam=1,nParamVar(iFlt,iFltWidth)
                  tempHaz(iParam,jInten,iProb,iAtten,iFtype) = hRate(iParam)*hQ
     1                     + tempHaz(iParam,jInten,iProb,iAtten,iFtype)
                  tempHaz1(iParam,jInten,iProb,iFtype) = hRate(iParam)*hHw
     1                     + tempHaz1(iParam,jInten,iProb,iFtype)
                 enddo
                 hG(jInten,iProb,iAtten,iFtype) = 0.d0
                endif
               enddo
              enddo
             enddo
            enddo
          endif
C *** END SPEEDUP EDIT

c         No out2 output in the cluster probability pass
          if ( clPass .eq. 1 ) goto 800

c         Compute the rate for this magnitude for each epistemic parameter variation
          do iParam=1,nParamVar(iFlt,iFltWidth)
            rout(iParam,iFltWidth) = (rate(iParam,iFltWidth)*pmag(iParam,iFltWidth))
          enddo
          mag = minMag(iFlt) + (iMag-0.5) * magStep(iFlt)

c         Write out magnitude rates of occurrence (out2 file)
c         Not currently implemented for SourceType 7 cases.
c         Write header for the first magnitude only
          if (iMag .eq. 1 ) then
            write (17,'( i5,2x,a80)') iFlt, fname(iflt)
            write (17,'( 2i5,''  nMag, nParamVar'')') nMag(iFlt), nParamVar(iFlt,iFltWidth)
c           Set the weight for this set of parameters (epistemic)
            do iParam=1,nParamVar(iFlt,iFltWidth)
              wt2(iParam) = RateParamWt(iFlt,iParam,iFltWidth)
     1                      * magRecurWt(iFlt,iParam,iFltWidth)
     2                      * maxMagWt(iFlt,iParam,iFltWidth)
            enddo
            write (17,'( 100f10.6)') (wt2(iParam),
     1            iParam=1,nParamVar(iFlt,iFltWidth))
          endif
          write (17,'(2i4,f8.3,500e12.4)') iFlt, iFltwidth, mag,
     1      (rout(iParam,iFltWidth), iParam=1,nParamVar(iFlt,iFltWidth))

 800     continue

 850     MinRrup(iFlt) = MinRrup_temp

c        No out1 or out5 output in the cluster probability pass
         if ( clPass .eq. 1 ) goto 860

c        Cluster: the members' out1 blocks hold zeros (nothing is added to
c        tempHaz in pass 2). The cluster's branch curves are written after the
c        last member, one set for each segmentation model (S36_ClusterWriteOut1).

c        Write temp Haz array to file
         call S21_WriteTempHaz ( PCflag, tempHaz, PC_D, nPC, nParamVar, nInten,
     1                nProb, nAtten, iFlt, attenType(iFlt), nFtype, iFltWidth, nWidth,
     2                nParamMax, nIntenMax, nAttenMax )

         call S21_WriteTempHaz1 ( tempHaz1, nParamVar, nInten, nProb,
     1        nAtten, iFlt, attenType(iFlt), nFtype, iFltWidth, nWidth )

 860    continue

c       Write p1_sum as a check
        if ( clPass .ne. 1 ) then
          write (*,'( 2x,'' Site = '',i5,2x,'' iFlt = '',i5,'' p1sum ='',f10.5, i5)') iSite, iflt, p1_sum, nFLt
          pdfsum(iFlt) = p1_sum
        endif

 900   continue

c       Cluster: after the last member, combine and start the contribution
c       pass at the first member; after the contribution pass, release storage
        if ( clPass .eq. 1 .and. iFlt .eq. iClLast ) then
          call S36_ClusterCombine ( Pmem, sMem, Pclu, iClFirst, nClMem,
     1         nClInten, nProb, nClAtten, nClParam, nClFtype, nClWidth,
     2         nClSeg, segModelFlag, segModelWt1, segWt1 )
          clPass = 2
          iFlt = iClFirst - 1
        elseif ( clPass .eq. 2 .and. iFlt .eq. iClLast ) then
          call S36_ClusterWriteOut1 ( Pclu, iClFirst, nFlt, iClV, nWidth,
     1         nParamVar, nFtype, nAtten, attenType(iClFirst), nInten, nProb,
     2         rate, probAct(iClFirst), nClInten, nClAtten, nClParam,
     3         nClFtype, nClWidth, nClSeg )
          deallocate ( Pmem, sMem, Pclu )
          clPass = 0
        endif
       goto 890
 901   continue

       close (11) !out1
       close (17) !out2

c      Write out the mean Haz
       call S21_output_TotalHaz ( isite, sitex, sitey, testInten, nInten,
     1       nFlt, nProb, Haz, fName, jCalc, sigTrunc, csrflag,
     2       attenName, period1, probAct, nWidth, m_bar, d_bar, e_bar,
     3       HazBins, nMagBins, nDistBins, nEpsBins, magBins, distBins,
     4       epsBins, al_segWt, MinRrup, nAttenType, attenType, pdfsum,
     5       segwt1, dirflag, tapflag,intflag, fsys, SourceDist,
     6       mMagout, hwflagout, ftype, vs, nMaxmag2, mmagoutWt, specT)

c      Write out the deagrregated hazard
       call S21_output_HazBins ( isite, sitex, sitey, testInten, nInten,
     1       nProb, HazBins, nMagBins, nDistBins, nEpsBins, magBins,
     2       distBins, epsBins, m_bar, d_bar, e_bar, Xcost_bar,
     3       nXcostBins, XcostBins, HazBinsX )

       call S21_output_Sourcedeagg ( isite, sitex, sitey, testInten, nInten,
     1           nFlt, haz, fName, m_bar_s, rrup_bar_s, rjb_bar_s,
     2           rx_bar_s, e_bar_s, rSeismo_bar_s, ry0_bar_s, mag_bar_s,
     3           ftype_bar_s, hypodepth_bar_s, dipavgd_bar_s, ztor_bar_s,
     4           thetasite_bar_s, rupwidth_bar_s, rHypo_bar_s, specT, nProb)

       call S21_WriteTempHaz2 ( tempHaz2, nInten, nProb, nAtten, nattenType )

       close (27) !out5
       close (28) !out6

 1000 continue

 2000 continue
      close (77)

      call itime(endtime)
      write (18, 5500 ) starttime
      write (18, 5501 ) endtime
 5500 format (  'Start time ',
     &         i2.2, ':', i2.2, ':', i2.2 )
 5501 format (  'End time ',
     &         i2.2, ':', i2.2, ':', i2.2 )

      close (18) !log file

      write (*,'( 2x,''Normal termination'')')
      stop

 2100 write (*,'( 2x,''input file error: number of sites'')')
      stop 99
 2101 write (*,'( 2x,''input file error: site long lat line'')')
      stop 99
 2102 write (*,'( 2x,''input file error: output file name'')')
      stop 99
 2103 write (*,'( 2x,''input file error: output file name'')')
      stop 99
 2104 write (*,'( 2x,''input file error: output file name'')')
      stop 99
 2105 write (*,'( 2x,''input file error: output file name'')')
      stop 99
 2106 write (*,'( 2x,''input file error: output file name'')')
      stop 99
 2107 write (*,'( 2x,''input file error: output file name'')')
      stop 99

      end
