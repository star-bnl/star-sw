

* $Id: g2t_volume_id.g,v 1.81 2016/11/03 13:49:01 jwebb Exp $
* $Log: g2t_volume_id.g,v $
* Revision 1.81  2016/11/03 13:49:01  jwebb
* Integrate EPD into framework.
*
* Revision 1.80  2015/10/12 20:46:57  jwebb
* Hit definition and starsim to root interface for FTS.
*
* Revision 1.79  2015/10/06 19:38:52  jwebb
* g2t updates to readout preshower in HCAL
*
* Revision 1.78  2015/01/08 21:24:18  jwebb
* Support FMS preshower detector in HCAL.
*
* Revision 1.77  2015/01/07 17:34:27  jwebb
* Remove print statements which Akio snuck by me...
*
* Revision 1.76  2015/01/06 21:47:34  jwebb
* Updated g2t volume ID for FMS preshower
*
* Revision 1.75  2014/08/04 14:11:30  jwebb
* Updates to HCAL geometry and volume IDs (affects y2014b,dev15b).
*
* Revision 1.74  2014/05/14 20:01:23  jwebb
* More support for HCAL.  Also note... last checkin of g2t_volume_id was to support FMS preshower.
*
* Revision 1.73  2014/05/14 19:54:18  jwebb
* Support for HCAL prototype readout.
*
* Revision 1.72  2013/07/10 20:34:20  jwebb
* Detects tpadconfig from TPCG structure instead of relying on versioning of
* the TPC... of course, the best way would be to just use teh appropriate
* TPAD structure and detect number of padrows, but this should work.
*
* Revision 1.71  2013/01/16 16:51:31  jwebb
* Update BTOF volume id's to support GMT in y2013.
*
* Revision 1.70  2012/03/20 20:45:06  jwebb
* Changes to g2t_volume_id to support reconfiguration of the inner TPAD
* volumes for inner TPC upgrade studies.
*
* Revision 1.69  2012/01/24 03:32:32  perev
* Add Etr
*
* Revision 1.68  2011/09/23 21:54:32  perev
* FGT two numbers id now
*
* Revision 1.67  2011/08/03 20:11:53  jwebb
* Add MTD to the g2t hit tables.
*
* Revision 1.66  2011/07/20 20:43:41  perev
* Fsc added
*
* Revision 1.65  2011/01/31 18:05:43  jwebb
* Added code to detect which version of the fpd geometry module is in use
* so that we can decode geant volume Ids and associate with g2t volume id
* properly.
*
* Revision 1.64  2011/01/26 19:21:17  perev
* FPD ==> STAR Soft
*
* Revision 1.63  2010/08/03 22:14:49  geurts
* Fix unknown TOFr choice for year2007 (btog_choice=10)  [bug ticket #1715]
* Fix wrong TOFr tray position ID for run 5 (btog_choice=8)
*
* Revision 1.62  2009/11/10 19:54:54  fisyak
* pams Cleanup
*
* Revision 1.61  2009/10/26 19:44:03  perev
* (1-73) normal TPC hits, (74-146) prompt hits
*
* Revision 1.60  2009/09/25 18:09:20  perev
* Monor improvements by F.Geurts
*
* Revision 1.59  2009/09/24 02:54:23  perev
* BugFix in TOF g2t_volume_id F.Geurts
*
* Revision 1.58  2009/07/25 02:07:17  perev
* Prompt hits added
*
* Revision 1.57  2007/03/27 23:29:56  potekhin
* Commented out an annoying left over print statement
*
* Revision 1.56  2006/12/18 02:39:18  potekhin
* Need to instroduce versioning for the multiple
* IST configurations
*
* Revision 1.55  2006/10/14 19:41:45  potekhin
* Implemented a temporary solution for the IST volume numbering,
* the coding scheme being final and some logic to be added shortly.
* To be used in conjunction with istbgeo2.g
*
* Revision 1.54  2006/10/11 18:01:39  potekhin
* Added a clause for HPD (Sevil)
*
* Revision 1.53  2005/07/01 15:10:01  potekhin
* Adding the GEM barrel tracker
*
* Revision 1.52  2005/04/15 14:50:46  potekhin
* Volume enumeration added for FGT (GEM) detector
*
* Revision 1.51  2005/03/31 18:10:38  potekhin
* Correction of the SSD volume numbering, as discussed with
* Lilian and Kai. +7000 to the number.
*
* Revision 1.50  2005/03/23 21:59:23  potekhin
* Added the numbering for the IST and FST, based on Kai's code
*
* Revision 1.49  2004/09/13 23:14:00  potekhin
* New material to support SSD, by Kai
*
* Revision 1.48  2004/03/19 01:32:22  geurts
* TOFp/r changes for y2004: accomodate choice=7 (see geometry/btofgeo/btofgeo2.g)
*
* Revision 1.47  2003/11/12 22:42:37  potekhin
* add the pixel detector volume encoding
*
* Revision 1.46  2003/10/09 16:33:02  potekhin
* Introduced a small hash table for the GEANT-> reco mapping
* of the FTPC sector numbers, in accordance with
* http://wwwstar.mppmu.mpg.de/ftpc_calibration_page/calibration/calibration.html
* This is clean and flexible, and should be easy to correct very quickly
* if necessary
*
* Revision 1.45  2003/10/02 23:17:24  potekhin
* Added a missing dollar mark which so far prevented
* the inclusion of CVS log into the source
*
*
* Revision 1.38  2002/10/16 19:12:44  kopytin
* Volume ID for BBC elements added. this Affects StBbcSimulationMaker.
*
* Revision ecal1.2  2002/10/03 15:48:55  zolni
* updated g2t_volume_id.g from Ole
*
* Revision 1.37  2001/09/06 00:22:00  nevski
* new svt geometry numbering intrroduced
*
* Revision 1.36  2001/07/05 17:00:36  nevski
* forward pion detector added
*
* Revision 1.35  2001/07/03 23:15:57  nevski
* forward pion detector added
*
* Revision 1.34  2001/07/03 15:59:01  nevski
* phmd added
*
* Revision 1.33  2001/04/06 18:13:11  akio
* Modifications for FPD
*
* Revision 1.32  2001/02/13 02:29:47  nevski
* USE BTOG bug fix
*
* Revision 1.31  2000/12/01 22:48:00  nevski
* phmd stuff added
*
* Revision 1.30  2000/11/19 21:27:13  nevski
* comments updated
*
* Revision 1.29  2000/11/17 02:55:21  nevski
* TOF geometry 3 - tof tray moved to east
*
* Revision 1.28  2000/08/14 20:43:49  nevski
* bug corrected
*
* Revision 1.27  2000/08/10 22:10:57  nevski
* vpd versionning introduced
*
* Revision 1.26  2000/05/24 15:02:50  nevski
* rich volume numbering done in g2t_volume ONLY
*
* Revision 1.25  2000/05/23 20:02:09  fisyak
* Attempt to merge all correction together (PN)
*
********************************************************************




      real function g2t_version(Csys)




*
* Modification history:                                            *
* PN 28.12.99: use structure control access to avoid warnings      *
* PN 28.12.99: make decision on CALB 2/3 level numbering based on  *
*              CALB_Nmodule(1) and (2), not on RICH presence !     *
********************************************************************
      implicit none
*
* $Id: gcunit.inc,v 1.1.1.1 2004/01/15 00:12:24 potekhin Exp $
*
* $Log: gcunit.inc,v $
* Revision 1.1.1.1  2004/01/15 00:12:24  potekhin
*
*
* Revision 1.1.1.1  1997/11/03 15:29:49  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:20:33  cernlib
* Geant
*
*
*
*
* gcunit.inc
*
      COMMON/GCUNIT/LIN,LOUT,NUNITS,LUNITS(5)
      INTEGER LIN,LOUT,NUNITS,LUNITS
      COMMON/GCMAIL/CHMAIL
      CHARACTER*132 CHMAIL
C
*
* gcunit.inc
*

* 
      Character*(*)    Csys
      Integer          NUMBV(15)
      Integer          i,iWheel,zsubsect,zsublayer,eemc_depth
      Integer          sector_hash(6,2) / 4, 5, 6, 7, 8, 9, 
                                         10,11,12, 1, 2, 3/
      Integer          ftpc_hash(6,2)   / 1, 6, 5, 4, 3, 2, 
                                          6, 1, 2, 3, 4, 5/

      Integer          innout,sector,sub_sector,volume_id
      Integer          rileft,eta,phi,phi_sub,superl,forw_back,strip
      Integer          ftpv,padrow,ftpc_sector,innour,lnumber,wafer,lsub,phi_30d
      Integer          section,tpgv,tpss,tpad,isdet,ladder,is,nladder,nwafer
      Integer          module,layer,nch
      Integer          nEndcap,nFpd,depth,shift,nv
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
      Integer hcal_sl     "HCAL short long cell, 1 short,2 long"
 
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
      save
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
*
      if (First) then
          first=.false.
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
          if (itpc>=0) print *,' g2t_volume_id: TPC version =',tpcg_version
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
      if (csys == 'isvt')            g2t_version = isvt
      if (csys == 'itpc')            g2t_version = itpc
      if (csys == 'ibtf')            g2t_version = ibtf
      if (csys == 'ical')            g2t_version = ical
      if (csys == 'ivpd')            g2t_version = ivpd
      if (csys == 'ieem')            g2t_version = ieem
      if (csys == 'istb')            g2t_version = istb
      if (csys == 'ifpd')            g2t_version = ifpd
      if (csys == 'ifms')            g2t_version = ifms
      if (csys == 'ifsc')            g2t_version = ifsc
      if (csys == 'imtd')            g2t_version = imtd

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
