* $Id: g2t_version.g,v 1.78 2015/01/08 21:24:18 jwebb Exp $
* $Log: g2t_version.g,v $
********************************************************************
      function g2t_version(Csys)

 Replace [;ASSERT(#) ! #;] with [;if (.not.#1) { stop 'g2t_version.g: line __LINE__' };];


*
* Modification history:                                            *
* PN 28.12.99: use structure control access to avoid warnings      *
* PN 28.12.99: make decision on CALB 2/3 level numbering based on  *
*              CALB_Nmodule(1) and (2), not on RICH presence !     *
********************************************************************
      implicit none
      real  g2t_version
+CDE,gcunit.
* 
      Character*(*)    Csys
      Integer          itpc/0/,ibtf/0/,ical/0/,ivpd/0/,ieem/0/,isvt/0/,istb/0/
      Integer          ifpd/0/,ifms/0/,ifpdmgeo/0/,ifsc/0/,imtd/0/
      Integer          istVersion/0/,istLayer/0/
 
      Structure  SVTG  {version}
      Structure  TPCG  {version, tpadconfig }
      Structure  VPDG  {version}
      Structure  BTOG  {version, int choice, posit1(2), posit2, posit3}
      Structure  CALG  {version, int Nmodule(2), int NetaT, int MaxModule, 
                                 int Nsub, int NetaSMDp, int NPhistr,
      	                         int Netfirst, int Netsecon} 
*         
      Structure  EMCG { Version, int Onoff, int fillMode}
      Structure  ISMG { Layer, Rin,            Rout,        TotalLength, code}

      Structure  FMCG { Version }         ! FMS/FPD++/FPD geometry
      Structure  FPDG { Version }         ! FPD geometry
      Structure  FSCG { Version }         ! FSC geometry
      Structure  MTDG { Version, Config } ! MTD geometry


      logical    first/.true./
      logical    printOnce/.true./
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
*
      if (first) then
	  first = .false.
          call RBPUSHD

*         in simulations done in MDC1 (1998) btog_posit1 was not saved
          btog_posit1 = {32,33}
          USE  /DETM/SVTT/SVTG  stat=isvt
          USE  /DETM/TPCE/TPCG  stat=itpc
          USE  /DETM/BTOF/BTOG  stat=ibtf
          USE  /DETM/CALB/CALG  stat=ical
          USE  /DETM/VPDD/VPDG  stat=ivpd
          USE  /DETM/ECAL/EMCG  stat=ieem
          USE  /DETM/ISTB/ISMG  stat=istb

          USE  /DETM/FPDM/FPDG  stat=ifpd
          USE  /DETM/FPDM/FMCG  stat=ifms
          USE  /DETM/FSCM/FSCG  stat=ifsc
          USE  /DETM/MUTD/MTDG  stat=imtd

          call RBPOPD
          if (itpc>=0) print *,' g2t_version: TPC version =',tpcg_version
          if (ivpd>=0) print *,'              : VPD version =',vpdg_version
          if (ibtf>=0) print *,'              : TOF version =',btog_version,
                               ' choice  =',btog_choice
          if (ical>=0) print *,'              : CALB patch  =',calg_nmodule
          if (ieem>=0) print *,'              : ECAL version=',emcg_version, 
                               ' onoff   =',emcg_onoff,emcg_FillMode
          if (istb>=0) then
*             print *,'              : ISTB version of code=', ismg_code
             istVersion=ismg_code
          endif
      endif	
*
      g2t_version = 0
      if (csys == 'svtg_version')    g2t_version = svtg_version
      if (csys == 'tpcg_version')    g2t_version = tpcg_version
      if (csys == 'tpcg_tpadconfig') g2t_version = tpcg_tpadconfig
      if (csys == 'vpdg_version')    g2t_version = vpdg_version
      if (csys == 'btog_version')    g2t_version = btog_version
      if (csys == 'btog_choice')     g2t_version = btog_choice
      if (csys == 'btog_posit1(1)')  g2t_version = btog_posit1(1)
      if (csys == 'btog_posit1(2)')  g2t_version = btog_posit1(2)
      if (csys == 'btog_posit2')     g2t_version = btog_posit2
      if (csys == 'btog_posit3')     g2t_version = btog_posit3
      if (csys == 'btog_version')    g2t_version = btog_version
      if (csys == 'calg_version')    g2t_version = calg_version
      if (csys == 'calg_nmodule(1)') g2t_version = calg_nmodule(1)
      if (csys == 'calg_nmodule(2)') g2t_version = calg_nmodule(2)
      if (csys == 'calg_netaT')      g2t_version = calg_netaT
      if (csys == 'calg_maxmodule')  g2t_version = calg_maxmodule
      if (csys == 'calg_nsub')       g2t_version = calg_nsub
      if (csys == 'calg_netasmdp')   g2t_version = calg_netasmdp
      if (csys == 'calg_nphistr')    g2t_version = calg_nphistr
      if (csys == 'calg_netfirst')   g2t_version = calg_netfirst
      if (csys == 'calg_netsecon')   g2t_version = calg_netsecon
      if (csys == 'emcg_version')    g2t_version = emcg_version
      if (csys == 'emcg_onoff')      g2t_version = emcg_onoff
      if (csys == 'emcg_fillmode')   g2t_version = emcg_fillmode
      if (csys == 'emcg_version')    g2t_version = emcg_version
      if (csys == 'ismg_layer')      g2t_version = ismg_layer
      if (csys == 'ismg_rin')        g2t_version = ismg_rin
      if (csys == 'ismg_rout')       g2t_version = ismg_rout
      if (csys == 'ismg_totallength')g2t_version = ismg_totallength
      if (csys == 'ismg_code')       g2t_version = ismg_code

      if (csys == 'fmcg_version')    g2t_version = fmcg_version
      if (csys == 'fpdg_version')    g2t_version = fpdg_version
      if (csys == 'fscg_version')    g2t_version = fscg_version
      if (csys == 'mtdg_version')    g2t_version = mtdg_version
      end      
