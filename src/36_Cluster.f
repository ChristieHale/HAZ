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
     2           rateParam, rateParamWt, magRecurWt, maxMagWt, faultWidthWt,
     3           ftype_wt )

c     Checks the input of one member of a clustered sequence (source type 8)
c     after its fault segment has been read. iFltFirst is the first segment of
c     the same fault system.
c
c     The members' epistemic branches are matched by index: on each branch
c     (thickness and dip, rate and magnitude, mechanism, GMM) the cluster
c     combines the members' branch curves with the same index. Every member
c     must therefore have the same numbers of branches with the same weights,
c     and the same rates. The magnitudes may differ by member, so that a branch
c     can hold a joint rupture set.

      implicit none
      include 'pfrisk.h'

      integer iFlt, iFltFirst, synchron(MAX_FLT), attenType(MAX_FLT),
     1        nWidth(MAX_FLT), nParamVar(MAX_FLT,MAX_WIDTH), nFtype(MAX_FLT)
      real al_segWt(MAX_FLT), RateType(MAX_FLT,MAXPARAM,MAX_WIDTH),
     1     rateParam(MAX_FLT,MAXPARAM,MAX_WIDTH),
     2     rateParamWt(MAX_FLT,MAXPARAM,MAX_WIDTH),
     3     magRecurWt(MAX_FLT,MAXPARAM,MAX_WIDTH),
     4     maxMagWt(MAX_FLT,MAXPARAM,MAX_WIDTH),
     5     faultWidthWt(MAX_FLT,MAX_WIDTH), ftype_wt(MAX_FLT,MAX_N1)
      character*80 fName(MAX_FLT)

      integer iWidth, iParam, iFtype, k
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

c     All members share the GMMs, the rate of the sequence and the epistemic
c     branches (matched by index)
      if ( iFlt .gt. iFltFirst ) then
        if ( attenType(iFlt) .ne. attenType(iFltFirst) ) then
          write (*,'( 2x,''Flt file error: all members of a cluster (source type 8) must have the same atten type'')')
          goto 900
        endif
        k = 0
        if ( nWidth(iFlt) .ne. nWidth(iFltFirst) ) k = 1
        if ( nFtype(iFlt) .ne. nFtype(iFltFirst) ) k = 1
        if ( k .eq. 0 ) then
          do iWidth=1,nWidth(iFlt)
            if ( abs(faultWidthWt(iFlt,iWidth)-faultWidthWt(iFltFirst,iWidth)) .gt. 1.0e-5 ) k = 1
            if ( nParamVar(iFlt,iWidth) .ne. nParamVar(iFltFirst,iWidth) ) k = 1
          enddo
          do iFtype=1,nFtype(iFlt)
            if ( abs(ftype_wt(iFlt,iFtype)-ftype_wt(iFltFirst,iFtype)) .gt. 1.0e-5 ) k = 1
          enddo
        endif
        if ( k .ne. 0 ) then
          write (*,'( 2x,''Flt file error: all members of a cluster (source type 8) must have the same numbers'')')
          write (*,'( 2x,''and weights of thickness and dip, rate and magnitude, and mechanism branches'')')
          goto 900
        endif
        do iWidth=1,nWidth(iFlt)
          do iParam=1,nParamVar(iFlt,iWidth)
            if ( abs(magRecurWt(iFlt,iParam,iWidth)*maxMagWt(iFlt,iParam,iWidth)
     1         - magRecurWt(iFltFirst,iParam,iWidth)*maxMagWt(iFltFirst,iParam,iWidth)) .gt. 1.0e-5 ) then
              write (*,'( 2x,''Flt file error: all members of a cluster (source type 8) must have the same numbers'')')
              write (*,'( 2x,''and weights of thickness and dip, rate and magnitude, and mechanism branches'')')
              goto 900
            endif
          enddo
        enddo
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
     2           minMag, nProb, specT, PCflag, nSegModel, nWidth, nFtype )

c     Writes a summary of the clustered sequences to the screen and log file
c     and checks the run options they do not support.

      implicit none
      include 'pfrisk.h'

      integer nFlt, clusterID(MAX_FLT), fsys(MAX_FLT),
     1        nParamVar(MAX_FLT,MAX_WIDTH), nProb, PCflag(MAX_PROB),
     2        nSegModel(MAX_FLT), nWidth(MAX_FLT), nFtype(MAX_FLT)
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
          write (iUnit,'( 6x,''Segmentation models: '',i4)') nSegModel(iFlt)
          write (iUnit,'( 6x,''Branches (matched in every member): width'',i4,'', rate and magnitude'',i4,
     1          '', mechanism'',i4)') nWidth(iFlt), nParamVar(iFlt,1), nFtype(iFlt)
c         Each rate of the sequence once (a rate is repeated for each magnitude branch)
          do iParam=1,nParamVar(iFlt,1)
            if ( iParam .gt. 1 ) then
              if ( rateParam(iFlt,iParam,1) .eq. rateParam(iFlt,iParam-1,1) .and.
     1             rateParamWt(iFlt,iParam,1) .eq. rateParamWt(iFlt,iParam-1,1) ) goto 30
            endif
            write (iUnit,'( 6x,''Rate of the sequence (/yr): '',e12.5,''  weight '',f7.4)')
     1            rateParam(iFlt,iParam,1), rateParamWt(iFlt,iParam,1)
 30         continue
          enddo
          write (iUnit,'( 6x,''Source  Mag model  Min mag  Max mag  Name'')')

 40       write (iUnit,'( 6x,i6,i11,2f9.3,2x,a40)') iFlt, nint(magRecur(iFlt,1,1)),
     1          minMag(iFlt), maxMag(iFlt,1,1), fName(iFlt)
 50       continue
        enddo
        write (iUnit,*)
      enddo

      return
      end

c ----------------------------------------------------------------------

      subroutine S36_ClusterSize ( iClFirst, iClLast, nProb, nInten,
     1           nAtten, attenType, nParamVar, nFtype, nWidth, nClMem,
     2           nClInten, nClAtten, nClParam, nClFtype, nClWidth )

c     Dimensions of the storage for one cluster (members iClFirst to
c     iClLast): numbers of members, test levels, GMMs and epistemic branches.

      implicit none
      include 'pfrisk.h'

      integer iClFirst, iClLast, nProb, nInten(MAX_PROB), nAtten(MAX_PROB,4),
     1        attenType(MAX_FLT), nParamVar(MAX_FLT,MAX_WIDTH),
     2        nFtype(MAX_FLT), nWidth(MAX_FLT), nClMem, nClInten, nClAtten,
     3        nClParam, nClFtype, nClWidth

      integer iFlt, iProb, iWidth

      nClMem = iClLast - iClFirst + 1
      nClInten = 1
      nClAtten = 1
      do iProb=1,nProb
        nClInten = max( nClInten, nInten(iProb) )
        nClAtten = max( nClAtten, nAtten(iProb,attenType(iClFirst)) )
      enddo
      nClParam = 1
      nClFtype = 1
      nClWidth = 1
      do iFlt=iClFirst,iClLast
        nClFtype = max( nClFtype, nFtype(iFlt) )
        nClWidth = max( nClWidth, nWidth(iFlt) )
        do iWidth=1,nWidth(iFlt)
          nClParam = max( nClParam, nParamVar(iFlt,iWidth) )
        enddo
      enddo

      return
      end


c ----------------------------------------------------------------------

      subroutine S36_ClusterCombine ( Pmem, sMem, Pclu, iClFirst, nClMem,
     1           nClInten, nProb, nClAtten, nClParam, nClFtype, nClWidth,
     2           nClSeg, segModelFlag, segModelWt1, segWt1 )

c     Combines the members' exceedance probabilities on each epistemic
c     branch (matched by index), at each test level, and for each
c     segmentation model s of the cluster's fault system, using the members
c     that s includes:
c
c        P_c,s = 1 - prod_k ( 1 - P_k )
c        s_s   = P_c,s / sum_k P_k          (s_s = 1 when sum_k P_k = 0)
c
c     P_c,s is built up one member at a time as P_c + P_k (1 - P_c). This
c     form only adds non-negative terms, so it keeps probabilities far
c     below the round-off of 1 - P_k, and it tends to 1 as the P_k do.
c
c     Member k's factor in pass 2 is
c
c        sMem_k = sum over the models s that include k of w_s s_s / segWt1_k
c
c     where w_s is the weight of model s and segWt1_k = sum of those w_s.
c     Pass 2 multiplies each contribution of member k by sMem_k and, as for
c     any segment, by segWt1_k, so on every branch the members'
c     contributions add up to alpha * ProbAct * sum_s w_s P_c,s.

      implicit none
      include 'pfrisk.h'

      integer iClFirst, nClMem, nClInten, nProb, nClAtten, nClParam,
     1        nClFtype, nClWidth, nClSeg, segModelFlag(MAX_FLT,100)
      real segModelWt1(MAX_FLT,100), segWt1(MAX_FLT)
      real*8 Pmem(nClMem,nClInten,nProb,nClAtten,nClParam,nClFtype,nClWidth),
     1       sMem(nClMem,nClInten,nProb,nClAtten,nClParam,nClFtype,nClWidth),
     2       Pclu(nClInten,nProb,nClAtten,nClParam,nClFtype,nClWidth,nClSeg)

      integer k, j, iProb, iAtten, iParam, iFtype, iWidth, iSeg
      real*8 Pk, Pc, sumP, s

      do iWidth=1,nClWidth
       do iFtype=1,nClFtype
        do iParam=1,nClParam
         do iAtten=1,nClAtten
          do iProb=1,nProb
           do j=1,nClInten
             do k=1,nClMem
               sMem(k,j,iProb,iAtten,iParam,iFtype,iWidth) = 0.0d0
             enddo
             do iSeg=1,nClSeg
               Pc = 0.0d0
               sumP = 0.0d0
               do k=1,nClMem
                 if ( segModelFlag(iClFirst+k-1,iSeg) .eq. 1 ) then
                   Pk = min( max( Pmem(k,j,iProb,iAtten,iParam,iFtype,iWidth), 0.0d0 ), 1.0d0 )
                   sumP = sumP + Pk
                   Pc = Pc + Pk * ( 1.0d0 - Pc )
                 endif
               enddo
               Pclu(j,iProb,iAtten,iParam,iFtype,iWidth,iSeg) = Pc
               if ( sumP .gt. 0.0d0 ) then
                 s = Pc / sumP
               else
                 s = 1.0d0
               endif
               do k=1,nClMem
                 if ( segModelFlag(iClFirst+k-1,iSeg) .eq. 1 ) then
                   sMem(k,j,iProb,iAtten,iParam,iFtype,iWidth) =
     1               sMem(k,j,iProb,iAtten,iParam,iFtype,iWidth)
     2               + dble(segModelWt1(iClFirst+k-1,iSeg)) * s
                 endif
               enddo
             enddo
             do k=1,nClMem
               sMem(k,j,iProb,iAtten,iParam,iFtype,iWidth) =
     1           sMem(k,j,iProb,iAtten,iParam,iFtype,iWidth) / dble(segWt1(iClFirst+k-1))
             enddo
           enddo
          enddo
         enddo
        enddo
       enddo
      enddo

      return
      end

c ----------------------------------------------------------------------

      subroutine S36_ClusterWriteOut1 ( Pclu, iClFirst, nFlt, iClV, nWidth,
     1           nParamVar, nFtype, nAtten, jType, nInten, nProb, rate,
     2           probAct1, nClInten, nClAtten, nClParam, nClFtype, nClWidth,
     3           nClSeg )

c     Writes a cluster's branch curves to out1 (unit 11), after its members'
c     blocks, which hold zeros. There is one set of blocks for each
c     segmentation model s of the cluster's fault system, in the same layout
c     as a source's blocks (one block for each width branch and period). The
c     source number in the block header is nFlt + the number of such sets
c     written so far for this site, so it does not clash with a source. The
c     curves are alpha * ProbAct * P_c,s on each branch.
c
c     The fractile program draws a segmentation model for each fault system;
c     for a cluster it then draws one branch and adds that model's curve, so
c     the cluster is sampled as one source (docs/Cluster_Source_Spec.md, A7).

      implicit none
      include 'pfrisk.h'

      integer iClFirst, nFlt, iClV, nWidth(MAX_FLT),
     1        nParamVar(MAX_FLT,MAX_WIDTH), nFtype(MAX_FLT), nAtten(MAX_PROB,4),
     2        jType, nInten(MAX_PROB), nProb, nClInten, nClAtten, nClParam,
     3        nClFtype, nClWidth, nClSeg
      real rate(MAXPARAM,MAX_WIDTH), probAct1
      real*8 Pclu(nClInten,nProb,nClAtten,nClParam,nClFtype,nClWidth,nClSeg)

      integer iSeg, iWidth, iProb, iAtten, iFtype, iParam, j

      do iSeg=1,nClSeg
        iClV = iClV + 1
        do iWidth=1,nWidth(iClFirst)
          do iProb=1,nProb
            write (11,'( 20i5)') nFlt+iClV, iWidth, iProb, nAtten(iProb,jType),
     1          nWidth(iClFirst), nFtype(iClFirst), nParamVar(iClFirst,iWidth), nInten(iProb)
            do iAtten=1,nAtten(iProb,jType)
              do iFtype=1,nFtype(iClFirst)
                do iParam=1,nParamVar(iClFirst,iWidth)
                  write (11,'( 20e15.6 )') ( rate(iParam,iWidth) * probAct1
     1              * Pclu(j,iProb,iAtten,iParam,iFtype,iWidth,iSeg), j=1,nInten(iProb) )
                enddo
              enddo
            enddo
          enddo
        enddo
      enddo

      return
      end
