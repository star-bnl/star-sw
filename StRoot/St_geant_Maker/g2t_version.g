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
      integer  g2t_version
+CDE,gcunit.
* 
      Integer          Csys
      Integer          itpc/0/,ibtf/0/,ical/0/,ivpd/0/,ieem/0/,isvt/0/,istb/0/
      Integer          ifpd/0/,ifms/0/,ifpdmgeo/0/,ifsc/0/,imtd/0/
      Integer          istVersion/0/,istLayer/0/
      Integer          j	
*     FPD
      Integer          n1,n2,ew,nstb,ch,sl,quad,layr,slat
*
*    this is an internal agfhit/digi information - need a better access.
      integer          idigi
      common /AgCdigi/ idigi(15)
      Integer          Iprin,Nvb
      Character*4                   cs,cd
      COMMON /AGCHITV/ Iprin,Nvb(8),cs,cd
      integer nbpads , maxpads, npadi, npado, npada
      parameter (maxpads=100) "Maximum number of TPADs"
      integer tpads(maxpads) "/ 1, 1, 1, 2, 2, 2, 3, 3, 3, 4,"
			    "  4, 4, 5, 5, 5, 6, 6, 6, 7, 7,"
			    "  7, 8, 8, 8, 9, 9, 9,10,10,10,"
			    " 11,11,11,12,12,12,13,13,13,14,"
			    " 14,15,16,17,18,19,20,21,22,23,"
			    " 24,25,26,27,28,29,30,31,32,33,"
			    " 34,35,36,37,38,39,40,41,42,43,"
			    " 44,45,45/;                    "

      integer isdets(maxpads)"/1, 0, 2, 1, 0, 2, 1, 0, 2, 1,"
			    " 0, 2, 1, 0, 2, 1, 0, 2, 1, 0,"
			    " 2, 1, 0, 2, 1, 0, 2, 1, 0, 2,"
			    " 1, 0, 2, 1, 0, 2, 1, 0, 2, 1,"
			    " 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,"
			    " 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,"
			    " 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,"
			    " 0, 0, 2/;                    "



      Integer hcal_tower  "HCAL towers 6x6"
      Integer hcal_cell   "HCAL cells  3x3"
      Integer hcal_fiber  "HCAL fibers 15x15 or 16x16"
 
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
*
      g2t_version = 0
      end      
