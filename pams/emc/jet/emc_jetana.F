CC:>-----------------------------------------------------------------
CC: FILE:       emc_jetana.F.template
CC: HISTORY:    Created July97 OGAWA,Akio
CC:             00jan96-v000a-hpl- Created by stic Version
CC:  Id: idl.y,v 1.8 1996/10/15 18:33:35 ward Exp  
CC:<-----------------------------------------------------------------
      INTEGER*4 FUNCTION EMC_JETANA(
     1        ems_jetlist_h,       ems_jetlist,
     2      ideal_jetlist_h,     ideal_jetlist,
     3       emc_jet_eval_h,      emc_jet_eval) 
      IMPLICIT NONE
#include "emc_jetana.inc"
CC:>-----------------------------------------------------------------
CC: ROUTINE:    EMC_JETANA
CC: DESCRIPTION: Jet analysis code. 
CC:             Compare Jets from full simulation and event genarator
CC: AUTHOR:     OGAWA, Akio (akio@bnl.gov)
CC: ARGUMENTS:
CC:          IN:
CC:      ems_jetlist     - Jet from simulated track/EMC
CC:    ems_jetlist_h     - 
CC:    ideal_jetlist     - Jet from event genarator tracks
CC:    ideal_jetlist_h   - 
CC:       INOUT:
CC:         OUT:
CC:     emc_jet_eval     - Results
CC:     emc_jet_eval_h   - 
CC: RETURNS:    STAF Condition Value
CC:>-----------------------------------------------------------------
      integer MAX
      parameter(MAX=4)
      integer i,j,k,l,n,nj1,nj2,iflag(MAX),jflag(MAX),index(MAX*MAX)
      real dr(MAX,MAX),de(MAX,MAX),dp(MAX,MAX),PI
      parameter(PI=3.141592654)
      EMC_JETANA = STAFCV_BAD
      
      n=emc_jet_eval_h.nok+1
      if(n.ge.emc_jet_eval_h.maxlen)then
         write(*,*) 'EMC_JETANA:Increase memory for JET'
         return
      endif

      if(nj1.gt.MAX)then
         write(*,*) 'Too many jet1 found'
      endif
      if(nj2.gt.MAX)then
         write(*,*) 'Too many jet2 found'
      endif
      nj1=MIN(ideal_jetlist_h.nok,MAX)
      nj2=MIN(ems_jetlist_h.nok,MAX)
      emc_jet_eval(n).njet1=nj1
      emc_jet_eval(n).njet2=nj2
      do i=1,nj1
         emc_jet_eval(n).flag1(i)=1
         emc_jet_eval(n).np1(i)  =ideal_jetlist(i).n_cells
         emc_jet_eval(n).et1(i)  =ideal_jetlist(i).e_j
         emc_jet_eval(n).eta1(i) =ideal_jetlist(i).eta_w
         emc_jet_eval(n).phi1(i) =ideal_jetlist(i).phi_w
      enddo
      if(nj1.lt.MAX) then
         do i=nj1+1,MAX
            emc_jet_eval(n).flag1(i)=-1
            emc_jet_eval(n).np1(i)  =-10.0
            emc_jet_eval(n).et1(i)  =-10.0
            emc_jet_eval(n).eta1(i) =-10.0
            emc_jet_eval(n).phi1(i) =-10.0
         enddo
      endif
c     
c***  find a jet cloest to a initial partons
c           
      call vzero(iflag,MAX)
      call vzero(jflag,MAX)
      do i=1,4
         do j=1,4
            dr(i,j)=100.0
         enddo
      enddo
      do i=1,nj1                  ! find closest jet for each parton
         do j=1,nj2
            de(i,j)=ems_jetlist(j).eta_w-emc_jet_eval(n).eta1(i)
            dp(i,j)=ems_jetlist(j).phi_w-emc_jet_eval(n).phi1(i)
            if(dp(i,j).lt.-PI) dp(i,j)=dp(i,j)+2*PI    
            if(dp(i,j).gt.PI)  dp(i,j)=dp(i,j)-2*PI
            dr(i,j)=sqrt(de(i,j)*de(i,j)+dp(i,j)*dp(i,j))
         enddo
      enddo
      call sortzv(dr,index,MAX*MAX,-1,0,0)
      k=1
      j=(index(k)-1)/MAX+1
      i=mod(index(k)-1,MAX)+1
      do while (dr(i,j). le. 1.0)
         emc_jet_eval(n).flag1(i)=2
         emc_jet_eval(n).flag2(i)=2
         emc_jet_eval(n).np2(i)  =ems_jetlist(j).n_cells
         emc_jet_eval(n).et2(i)  =ems_jetlist(j).e_j   
         emc_jet_eval(n).eta2(i) =ems_jetlist(j).eta_w 
         emc_jet_eval(n).phi2(i) =ems_jetlist(j).phi_w 
         emc_jet_eval(n).det(i)  =emc_jet_eval(n).et2(i)
     +                           -emc_jet_eval(n).et1(i)
         emc_jet_eval(n).deta(i) =de(i,j)
         emc_jet_eval(n).dphi(i) =dp(i,j)
         iflag(i)=1
         jflag(j)=1
         do l=1,MAX
            dr(i,l)=100.0
            dr(l,j)=100.0
         enddo        
         k=k+1
         j=(index(k)-1)/MAX+1
         i=mod(index(k)-1,MAX)+1
      enddo
      do i=1,MAX
         if(iflag(i) .eq. 0) then
            emc_jet_eval(n).det(i)  =-1000.0
            emc_jet_eval(n).deta(i) =-10.0
            emc_jet_eval(n).dphi(i) =-10.0
         endif
      enddo
      do j=1,nj2
         if(jflag(j) .eq. 0) then
            do i=1,MAX
               if (iflag(i) .eq. 0) then
                  iflag(i)=1
                  emc_jet_eval(n).flag2(i)=1
                  emc_jet_eval(n).np2(i)  =ems_jetlist(j).n_cells
                  emc_jet_eval(n).et2(i)  =ems_jetlist(j).e_j   
                  emc_jet_eval(n).eta2(i) =ems_jetlist(j).eta_w 
                  emc_jet_eval(n).phi2(i) =ems_jetlist(j).phi_w 
                  goto 200
               endif
            enddo
 200        continue
         endif
      enddo
      do i=1,MAX
         if(iflag(i).eq.0) then
            emc_jet_eval(n).flag2(i)=-1
            emc_jet_eval(n).np2(i)  =-10.0
            emc_jet_eval(n).et2(i)  =-10.0
            emc_jet_eval(n).eta2(i) =-10.0
            emc_jet_eval(n).phi2(i) =-10.0
         endif
      enddo
      emc_jet_eval_h.nok=emc_jet_eval_h.nok+1
      EMC_JETANA=STAFCV_OK
      RETURN
      END
  
