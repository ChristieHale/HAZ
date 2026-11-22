c ----------------------------------------------------------------------
c     Clustered earthquake sequences (source type 8)
c
c     A cluster is a fault system whose segments are all source type 8.
c     Each segment is one member: an earthquake that occurs every time the
c     sequence occurs. The sequence has one rate, alpha, and on each epistemic
c     branch the cluster's rate of exceedance is
c
c        alpha * ( 1 - prod_k ( 1 - P_k ) )
c
c     where P_k is the probability that member k's earthquake exceeds the
c     test level given that the sequence occurs (USGS OFR 2008-1128,
c     Appendix F). See docs/Cluster_Source_Spec.md.
c ----------------------------------------------------------------------

      subroutine S36_CheckClusterMember ( iFlt, iFltFirst, fName, synchron,
     1           al_segWt, attenType, nWidth, nParamVar, nFtype, RateType,
     2           rateParam, rateParamWt, magRecurWt, maxMagWt )

c     Checks the input of one member of a clustered sequence (source type 8)
c     after its fault segment has been read. iFltFirst is the first member of
c     the same cluster.

      implicit none
      include 'pfrisk.h'

      integer iFlt, iFltFirst, synchron(MAX_FLT), attenType(MAX_FLT),
     1        nWidth(MAX_FLT), nParamVar(MAX_FLT,MAX_WIDTH), nFtype(MAX_FLT)
      real al_segWt(MAX_FLT), RateType(MAX_FLT,MAXPARAM,MAX_WIDTH),
     1     rateParam(MAX_FLT,MAXPARAM,MAX_WIDTH),
     2     rateParamWt(MAX_FLT,MAXPARAM,MAX_WIDTH),
     3     magRecurWt(MAX_FLT,MAXPARAM,MAX_WIDTH),
     4     maxMagWt(MAX_FLT,MAXPARAM,MAX_WIDTH)
      character*80 fName(MAX_FLT)

      integer iWidth, iParam
      real sum

c     Every member occurs in every sequence
      if ( abs(al_segWt(iFlt)-1.0) .gt. 1.0e-6 ) then
        write (*,'( 2x,''Flt file error: the aleatory segment weight of a cluster member (source type 8) must be 1.0'')')
        goto 900
      endif

c     Not combined with synchronous rupture
      if ( synchron(iFlt) .ne. 0 ) then
        write (*,'( 2x,''Flt file error: synchronous rupture is not allowed for a cluster member (source type 8)'')')
        goto 900
      endif

c     The rate is the rate of the sequence, given as activity rates only
      do iWidth=1,nWidth(iFlt)
        do iParam=1,nParamVar(iFlt,iWidth)
          if ( RateType(iFlt,iParam,iWidth) .ne. 2 ) then
            write (*,'( 2x,''Flt file error: a cluster member (source type 8) takes activity rates only'')')
            write (*,'( 2x,''(no slip rates, recurrence intervals or moment rates)'')')
            goto 900
          endif
        enddo
      enddo

c     One SSC branch (thickness and dip, rate and magnitude, mechanism)
      if ( nWidth(iFlt) .ne. 1 .or. nParamVar(iFlt,1) .ne. 1 .or. nFtype(iFlt) .ne. 1 ) then
        write (*,'( 2x,''Flt file error: a cluster member (source type 8) must have a single'')')
        write (*,'( 2x,''thickness, dip, activity rate, magnitude model, reference magnitude and mechanism'')')
        goto 900
      endif

c     The activity rates carry all of the rate weight
      do iWidth=1,nWidth(iFlt)
        sum = 0.
        do iParam=1,nParamVar(iFlt,iWidth)
          sum = sum + rateParamWt(iFlt,iParam,iWidth) * magRecurWt(iFlt,iParam,iWidth)
     1                * maxMagWt(iFlt,iParam,iWidth)
        enddo
        if ( abs(sum-1.0) .gt. 1.0e-3 ) then
          write (*,'( 2x,''Flt file error: the activity-rate branch weight of a cluster member (source type 8)'')')
          write (*,'( 2x,''must be 1.0, and the slip-rate, recurrence-interval and moment-rate weights 0.0'')')
          goto 900
        endif
      enddo

c     All members share the GMMs and the rate of the sequence
      if ( iFlt .gt. iFltFirst ) then
        if ( attenType(iFlt) .ne. attenType(iFltFirst) ) then
          write (*,'( 2x,''Flt file error: all members of a cluster (source type 8) must have the same atten type'')')
          goto 900
        endif
        do iWidth=1,nWidth(iFlt)
          do iParam=1,nParamVar(iFlt,iWidth)
            if ( abs(rateParam(iFlt,iParam,iWidth)-rateParam(iFltFirst,iParam,iWidth)) .gt.
     1               1.0e-5*abs(rateParam(iFltFirst,iParam,iWidth)) .or.
     2           abs(rateParamWt(iFlt,iParam,iWidth)-rateParamWt(iFltFirst,iParam,iWidth)) .gt. 1.0e-5 ) then
              write (*,'( 2x,''Flt file error: all members of a cluster (source type 8) must have the same'')')
              write (*,'( 2x,''activity rates and weights (the rate of the sequence)'')')
              goto 900
            endif
          enddo
        enddo
      endif

      return

 900  write (*,'( 2x,''From fault segment: '',a80)') fName(iFlt)
      stop 99
      end

c ----------------------------------------------------------------------

      subroutine S36_ClusterSetup ( nFlt, clusterID, fName, fsys, probAct,
     1           nParamVar, rateParam, rateParamWt, magRecur, maxMag,
     2           minMag, nProb, specT, PCflag )

c     Writes a summary of the clustered sequences to the screen and log file
c     and checks the run options they do not support.

      implicit none
      include 'pfrisk.h'

      integer nFlt, clusterID(MAX_FLT), fsys(MAX_FLT),
     1        nParamVar(MAX_FLT,MAX_WIDTH), nProb, PCflag(MAX_PROB)
      real probAct(MAX_FLT), rateParam(MAX_FLT,MAXPARAM,MAX_WIDTH),
     1     rateParamWt(MAX_FLT,MAXPARAM,MAX_WIDTH),
     2     magRecur(MAX_FLT,MAXPARAM,MAX_WIDTH),
     3     maxMag(MAX_FLT,MAXPARAM,MAX_WIDTH), minMag(MAX_FLT),
     4     specT(MAX_PROB)
      character*80 fName(MAX_FLT)

      integer iFlt, iProb, iParam, nCluster, iUnit, k

      nCluster = 0
      do iFlt=1,nFlt
        if ( clusterID(iFlt) .gt. nCluster ) nCluster = clusterID(iFlt)
      enddo
      if ( nCluster .eq. 0 ) return

c     The PC method is linear in the rate and does not apply to clusters
      do iProb=1,nProb
        if ( PCflag(iProb) .ne. 0 ) then
          write (*,'( 2x,''Run file error: the PC method is not available with clustered sequences (source type 8)'')')
          write (*,'( 2x,''Period: '',f8.3)') specT(iProb)
          stop 99
        endif
      enddo

c     Summary on the screen (unit 6) and in the log file (unit 18)
      do k=1,2
        iUnit = 6
        if ( k .eq. 2 ) iUnit = 18
        write (iUnit,*)
        write (iUnit,'( 2x,''Clustered earthquake sequences (source type 8): '',i4)') nCluster
        do iFlt=1,nFlt
          if ( clusterID(iFlt) .eq. 0 ) goto 50
          if ( iFlt .gt. 1 ) then
            if ( clusterID(iFlt-1) .eq. clusterID(iFlt) ) goto 40
          endif

c         First member: the cluster's fault system and the rate of the sequence
          write (iUnit,'( 4x,''Cluster '',i4,'', fault system '',i4,'', Prob Activity '',f7.4)')
     1          clusterID(iFlt), fsys(iFlt), probAct(iFlt)
          do iParam=1,nParamVar(iFlt,1)
            write (iUnit,'( 6x,''Rate of the sequence (/yr): '',e12.5,''  weight '',f7.4)')
     1            rateParam(iFlt,iParam,1), rateParamWt(iFlt,iParam,1)
          enddo
          write (iUnit,'( 6x,''Source  Mag model  Min mag  Max mag  Name'')')

 40       write (iUnit,'( 6x,i6,i11,2f9.3,2x,a40)') iFlt, nint(magRecur(iFlt,1,1)),
     1          minMag(iFlt), maxMag(iFlt,1,1), fName(iFlt)
 50       continue
        enddo
        write (iUnit,*)
      enddo

c     The hazard calculation for clusters is not implemented yet
      write (*,'( 2x,''Source type 8 (clustered sequence) is read, but the hazard calculation'')')
      write (*,'( 2x,''for clusters is not implemented yet.'')')
      write (18,'( 2x,''Source type 8 (clustered sequence) is read, but the hazard calculation'')')
      write (18,'( 2x,''for clusters is not implemented yet.'')')
      stop 99

      end
