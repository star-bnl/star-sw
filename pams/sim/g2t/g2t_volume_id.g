* $Id: g2t_volume_id.g,v 1.98 2020/11/20 14:45:06 jwebb Exp $
* $Log: g2t_volume_id.g,v $
* Revision 1.98  2020/11/20 14:45:06  jwebb
* Reduce verbosity
*
* Revision 1.97  2020/02/25 22:53:08  jwebb
* Add FCS preshower volume ID
*
* Revision 1.96  2020/02/05 15:18:16  jwebb
* Fix g2t_volume_id for forward silicon
*
* Revision 1.95  2020/02/04 17:47:50  jwebb
*
* Changes to forward silicon geometry, and associated volume ID changes.
*
* Revision 1.94  2019/10/30 20:29:21  jwebb
* Final (?) fix for zfatal bug.  Missing volume id in fast simulator because the fpdmgeo4 module was not properly detected / configured in g2t_volume_id.
*
* Revision 1.93  2019/10/25 17:13:09  jwebb
* Deprecate old numbering scheme for hcal
*
* Revision 1.92  2019/09/30 14:13:49  jwebb
*
* Integrate HITS for forward tracking and forward calorimeter.
*
* n.b. deprecates the legacy HcalGeo RnD detector.
*
* Revision 1.91  2019/09/03 20:12:55  jwebb
* Proposed fix for ticket 3399--
* https://www.star.bnl.gov/rt3/Ticket/Display.html?id=3399&results=8b3b60b210def723798e28371b984406
*
* Relies on changes to the geometry which flag the mixed iTPC/TPC mode.
* -- StarGeo.xml v1.29 / TpceConfig.xml v1.7 / TpceGeo3a.xml v1.20
*
* When the mixed iTPC/TPC mode is detected, the outer padrows in sector
* 20 are renumbered to match the iTPC numbering scheme (41 through 72).
*
* The inner padrows keep the old TPC numbering scheme (1 through 13) with 3
* pseudopadrows per real padrow.  To hopefully avoid confusion, we place these
* in a fake sector 99.  TpcRS should skip these in the hit processing loop.
* We place them in a fake sector 99.  TpcRS should skip these in the hit processing
* loop.
*
* Code has been minimally tested as per the recipe in the trouble ticket.
* Sector 20 shows reasonable (as many as 32) matched hits in the small sample
* generated.
*
* Revision 1.90  2018/12/03 21:08:53  jwebb
* g2t_volume_id for TPC is split out into a separate subroutine.
*
* Code is added to recognize iTPC era geometry, and setup the appropriate
* TPAD volume mappings.
*
* (Support for iTPC R&D models retired).
*
* Code was tested on simulations from y2003 to 2018.  Output of fz2root is
* identical on a hit-by-hit basis on the first production quality geometry
* from each year.  y2019 simulations of single muons show the expected
* number of hits on tracks.  note-- prompt hit region has not been
* vetted, but no issues expected as we do not change the prompt-hit logic.
*
* Revision 1.89  2018/07/24 18:46:14  jwebb
* Updates to EPD geometry and numbering from Prashanth.
*
* Revision 1.88  2018/02/01 22:52:38  jwebb
* Updated g2t volume ID for epd.
*
* Revision 1.87  2018/01/26 18:09:07  jwebb
* Reduce unneeded output
*
* Revision 1.86  2017/12/28 18:11:29  jwebb
*
* Add FMS postshower hits interface to C++...
*
* Revision 1.85  2017/12/19 16:36:29  jwebb
* Updated EPD volume id
*
* Revision 1.84  2017/10/24 22:03:32  jwebb
* Shut the framework up.
*
* Revision 1.83  2017/10/18 14:33:39  jwebb
* Modifications of g2t_volume_id to support alternate paths to pxl, ist, sst
* active sensors, while maintaining the same absolute volume ID.
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
      function g2t_volume_id(Csys,numbv)

 Replace [;ASSERT(#) ! #;] with [;if (.not.#1) { stop 'g2t_volume_id.g: line __LINE__' };];


*
* Modification history:                                            *
* PN 28.12.99: use structure control access to avoid warnings      *
* PN 28.12.99: make decision on CALB 2/3 level numbering based on  *
*              CALB_Nmodule(1) and (2), not on RICH presence !     *
********************************************************************
      implicit none

      integer :: g2t_volume_id
      integer :: g2t_tpc_volume_id, g2t_fst_volume_id, g2t_stg_volume_id
      integer :: g2t_wca_volume_id, g2t_hca_volume_id, g2t_pre_volume_id

+CDE,gcunit.
* 
      Character*3      Csys
      Integer          NUMBV(15)
      Integer          i,iWheel,zsubsect,zsublayer,eemc_depth
      Integer          sector_hash(6,2) / 4, 5, 6, 7, 8, 9, 
                                         10,11,12, 1, 2, 3/
      Integer          ftpc_hash(6,2)   / 1, 6, 5, 4, 3, 2, 
                                          6, 1, 2, 3, 4, 5/

      Integer          innout,sector,sub_sector,volume_id,sensor,unknown
      Integer          rileft,eta,phi,phi_sub,superl,forw_back,strip
      Integer          ftpv,padrow,ftpc_sector,innour,lnumber,wafer,lsub,phi_30d
      Integer          section,tpgv,tpss,tpad,isdet,ladder,is,nladder,nwafer
      Integer          module,layer,nch
      Integer          nEndcap,nFpd,depth,shift,nv
      Integer          itpc/0/,ibtf/0/,ical/0/,ivpd/0/,ieem/0/,isvt/0/,istb/0/
      Integer          ifpd/0/,ifms/0/,ifpdmgeo/0/,ifsc/0/,imtd/0/,ipxl/0/
      Integer          iist/0/,isst/0/, ifst/0/
      Integer          istVersion/0/,istLayer/0/
*     FPD
      Integer          n1,n2,ew,nstb,ch,sl,quad,layr,slat
*
*    this is an internal agfhit/digi information - need a better access.
      integer          idigi
      common /AgCdigi/ idigi(15)
      Integer          Iprin,Nvb
      Character*4                   cs,cd
      COMMON /AGCHITV/ Iprin,Nvb(8),cs,cd
      integer nbpads , maxpads
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
      Integer epd_epdm, epd_epss, epd_epdt

      Integer etof_sector, etof_plane, etof_counter, etof_gap, etof_cell
      Integer pixl_alignment/0/ "Pixel alignment level"
 
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

      Structure  PIXL { Version, int sector, int ladder, int sensor, phiNum(10), int on(1), int misalign }
      Structure  ISTC { version, int misalign }
      Structure  SSDP { version, int contig, int placement, int misalign }

      Structure  FSTG { Version }

      Structure  GDAT {real mfscale, char gtag(2)}

      logical    first/.true./
      logical    printOnce/.true./
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

          USE  /DETM/PIXL/PIXL  stat=ipxl
          USE  /DETM/ISTD/ISTC  stat=iist
          USE  /DETM/SISD/SSDP  stat=isst
          USE  /DETM/FSTM/FSTG  stat=ifst
        
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






   """This is a hack.  For some reason that I have yet to determine, the PIXL"""
   """structure is not being loaded... maybe the name PIXL gets confused with"""
   """the data struct associated w/ pixlgeo?  Maybe reasonas?"""
             pixl_alignment = 1
             pixl_misalign  = 1




          """ Intialize TPADs based on TPC version """
          IF TPCG_tpadconfig==1 .or. _
             TPCG_tpadconfig==4 .or. _
             TPCG_tpadconfig==6 "Inner TPC upgrade 32 pads" {

             nbpads = 32 "inner" + 32 "outer" + 4 "edge/fake"
 
             tpads = { 1, 1, 2, 3, 4, 5, 6, 7, 8, 9,
                      10,11,12,13,14,15,16,17,18,19,
                      20,21,22,23,24,25,26,27,28,29,
                      30,31,32,32,33,33,34,35,36,37,
                      38,39,40,41,42,43,44,45,46,47,
                      48,49,50,51,52,53,54,55,56,57,
                      58,59,60,61,62,63,64,64 };

              isdets = { 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                         0, 0, 0, 2, 1, 0, 0, 0, 0, 0,
                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                         0, 0, 0, 0, 0, 0, 0, 2 };

          } else {           

             nbpads = 73
             tpads = { 1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 
                       4, 4, 5, 5, 5, 6, 6, 6, 7, 7, 
                       7, 8, 8, 8, 9, 9, 9,10,10,10, 
                      11,11,11,12,12,12,13,13,13,14, 
                      14,15,16,17,18,19,20,21,22,23, 
                      24,25,26,27,28,29,30,31,32,33, 
                      34,35,36,37,38,39,40,41,42,43, 
                      44,45,45};

             isdets = { 1, 0, 2, 1, 0, 2, 1, 0, 2, 1,
	                0, 2, 1, 0, 2, 1, 0, 2, 1, 0,
		        2, 1, 0, 2, 1, 0, 2, 1, 0, 2,
		        1, 0, 2, 1, 0, 2, 1, 0, 2, 1,
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                        0, 0, 2 };

          }



          !! IFPDMGEO indicates which major version of the FPD/FMS module is present
          !! IFPD    indicates which geometry version is in place (to allow changes
          !!         w/in each major version.)
          if ( ifpd > 0 ) { 
                         ifpdmgeo=0      !! fpdmgeo.g
                         ifpd   =fpdg_version
          } 
          if ( ifms > 0 ) { 
             ifms = fmcg_version
             if (ifms=6) ifpdmgeo=1      !! fpdmgeo1.g
             if (ifms=7) ifpdmgeo=2      !! fpdmgeo2.g
             if (ifms=8) ifpdmgeo=3      !! fpdmgeo3.g                                       
             if (ifms=9) ifpdmgeo=4      !! fpdmgeo4.g "!!!! missed this when FpdmGeo4 was integrated!!!!"
          }
		  if (ifsc>=0) print *,' g2t_volume_id: FSC version =',fscg_version

      endif
*
      volume_id = 0	
      g2t_volume_id = volume_id
      
      nv        = nvb(1)  ! number of real volume levels in NUMBV
*
*  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 


**      write (*,*) 'g2t volume id: csys=', csys, ' numbv=', numbv


      If (Csys=='svt') then
*1*                                          Ken Wilson
** Helen altered SVT volume IDs so agrees with hardware defs.
        If  (Cd=='SVTD') then
           lnumber    = numbv(1)
           ladder     = numbv(2)
           wafer      = numbv(3)
        
           If ( ladder .eq. 0) then
* This is the year 1 ladder
                nladder = 1
                wafer   = lnumber
                ladder  = 12
                lnumber = 4
* Set First barrel ids
           else if (lnumber.le.2) then
                nladder = 8
		nwafer  = 4
*               wafer   = 5-wafer 
* Set 2nd barrel ids
           else if (lnumber.le.4) then
                nladder  = 12
		nwafer   = 6	
*               wafer   = 7-wafer   
* Set 3rd barrel ids
           else if (lnumber.le.6) then
                nladder  = 16
		nwafer   = 7
*               wafer   = 8-wafer
           else
             print *,' G2T warning: layer number ',lnumber,
     >               '     in svt hits not found' 
           endif

* PN: change geant numbering (CCW) to STAR numbering(CW):
           if (nladder>1) then
*             inner sub-layer - 0, outer - 1:
              lsub    = mod(lnumber-1,2)
              if (svtg_version==1) then 
*             OLD: 3 o'clock is geant's first and STAR N/4 element:
                ladder=nladder/4-(ladder-1)*2-lsub
                ladder=mod(nladder+ladder-1,nladder)+1
              else
*             NEW: 12 o'clock is geant's first and STAR last element:
                ladder=nladder-(ladder-1)*2-lsub
              endif
           endif
	if (ladder < 1 | ladder > nladder | wafer < 1 | wafer > nwafer) return
        volume_id  = 1000*lnumber+100*wafer+ladder
        else if (Cd=='SFSD') then
           volume_id =  7000+100*numbv(2)+numbv(1)
        endif
        
      else if (Csys=='ssd') then

        if ( isst.lt.0 ) then

        volume_id = 7000+100*numbv(2)+numbv(1)

        else if ( ssdp_misalign .eq. 0 ) then

        ladder = numbv(1)
        sensor = numbv(2)
        volume_id = 7000+100*numbv(2)+numbv(1)

        else if( ssdp_misalign.eq.1 ) then

        ladder = numbv(1) - 1
        sensor = mod( ladder , 16 ) + 1
        ladder =      ladder / 16   + 1

        volume_id = 7000 + 100*sensor + ladder

        endif


* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      else if (Csys=='tpc') then

        volume_id = g2t_tpc_volume_id( numbv )

      else if (Csys=='mwc') then
*3*
        rileft    = numbv(1)
        sector    = numbv(2) 
        innout    = numbv(3)
        innour    = numbv(4)
        volume_id = 1000*rileft+100*innout+10*innour+sector
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      else if (Csys=='tof') then
*4*                                             Frank Geurts
        If (btog_version==1) then
           rileft     = numbv(1)
           sector     = numbv(2)
           sub_sector = numbv(3) 
           innout     = numbv(4)
           volume_id  = 100000*rileft+1000*innout+10*sector+sub_sector   
        else if (btog_version==2) then
*          simulations done in 2000 - one tof tray on west side
           if (btog_choice==4) then
              rileft     = 1
              sector     = btog_posit1(1)
              sub_sector = numbv(1) 
              innout     = numbv(2)
              volume_id  = 100000*rileft+1000*innout+10*sector+sub_sector   
           else
              print *,' g2t_volume_id : choice not coded yet '
           endif        
        else if (btog_version>=3) then
*          For simulations after 28-sep-00, before it was version 2
           if (btog_choice==2) then      ! Full TOF
             rileft     = numbv(1)       !     west(1)/east(2)
             sector     = numbv(2)       !     tray(1-60)
             innout     = numbv(3)       !     4wide(1)/5wide(2) sections
             sub_sector = numbv(4)       !     theta-tray(4w:1-4, 5w:1-5)
             section    = numbv(5)       !     phi-tray(4w:1-9,5w:1)
           else if (btog_choice==3) then  ! ~25% TOF (only on east side)
             rileft     = 2              !     east (pre-set)
             sector     = numbv(1)       !     tray
             innout     = numbv(2)       !     4wide/5wide section
             sub_sector = numbv(3)       !     theta-tray
             section    = numbv(4)       !     phi-tray
           else if (btog_choice<=7) then  !  TOFp (single tray)
             rileft     = 2              !     east (pre-set)
             if (btog_choice!=7) then    !
              sector    = btog_posit1(1) !     tray (pre-set)
             else                        !
              sector    = btog_posit1(2) !
             endif                       !
             innout     = numbv(1)       !     4wide/5wide section
             sub_sector = numbv(2)       !     theta-tray
             section    = numbv(3)       !     phi-tray
           else
             print *,' g2t_volume_id: unknown TOF choice.'
             print *,' g2t_volume_id: btog_choice=',btog_choice
           endif

*  -------- sanity check ---------
           if ((rileft.lt.1).OR.(rileft.gt. 2).or.
     +        (sector.lt.1).OR.(sector.gt.60).or.
     +        (innout.lt.1).OR.(innout.gt. 2).or.
     +        (sub_sector.lt.1).or.
     +         ((innout==1).AND.(sub_sector.gt.4)).or.
     +         ((innout==2).AND.(sub_sector.gt.5)).or.
     +        (section.lt.1).OR.(section.gt.9).or.
     +         ((innout.eq.2).AND.(section.ne.1))) then
          print *,' g2t_volume_id: TOF sanity check failed.',
     +              rileft,sector,innout,sub_sector,section
	  return
	endif

*  -------- combine 4wide and 5wide sections ---------
          if (innout==1) then
             section = section+1  !  phi-tray (4wide-sections)
          endif

*  -------- encode volume_id ---------
          volume_id = 100000*rileft+1000*sector+100*sub_sector+section
        else
          print *,' g2t_volume_id : TOF version not coded yet'
          print *,' g2t_volume_id : btog_version=',btog_version
        endif

* ------- TOFr detector (single tray) --------------
      else if (Csys=='tfr') then   ! TOFr
         if (btog_choice==5 .or. btog_choice==7) then      !  single tray
            rileft     = 2               !  east (pre-set)
            sector     = btog_posit2     !  tray (pre-set)
            module     = numbv(1)        !  module (eta)
            layer      = numbv(2)        !  layer (1-6, gap in module)
	 else if (btog_choice==8 .or. btog_choice==9 .or. btog_choice==10) then  !  single tray (different location)
           rileft     = 2               !  east (pre-set)
           sector     = btog_posit3     !  tray (pre-set)
           module     = numbv(1)        !  module (eta)
           layer      = numbv(2)        !  layer (1-6, gap in module)
         else if (btog_choice==11) then                     ! Run 8
            rileft     = 2               !  east (pre-set)
            sector     = numbv(1)        !  tray(1-60)
            module     = numbv(2)        !  module (eta)
            layer      = numbv(3)        !  layer (1-6, gap in module)
         else if (btog_choice==6 .or. btog_choice==12) then ! full TOF or Run 9
            rileft     = numbv(1)        !  west(1)/east(2)
            sector     = numbv(2)        !  tray(1-60)
            module     = numbv(3)        !  module (eta)
            layer      = numbv(4)        !  layer (1-6, gap in module)
         else if (btog_choice==13) then  ! full TOF with GMT trays (Run 13+)
            rileft     = numbv(1)        !  west(1)/east(2)
            sector     = numbv(2)        !  tray(1-60)
            module     = numbv(3)        !  module (eta)
            layer      = numbv(4)        !  layer (1-6, gap in module)
*           GMT replacement only affects trays 8 (W8), 23 (W23), 93 (E33), and 108 (E48)
            if ((rileft==1 .and. (sector== 8 .or. sector==23)) .or.
     +          (rileft==2 .and. (sector==33 .or. sector==48))) then 
            module     = module + 4    ! account for 4 modules that have replaced with GMT
            endif
         else
            print *,' g2t_volume_id: unknown TOFr choice.'
            print *,' g2t_volume_id: btog_choice=',btog_choice
         endif
         volume_id = layer +10*(module +100*(sector+100*rileft) )

      else if (Csys=='ctb') then
*5*
        volume_id = 1000*numbv(1)+100*numbv(3)+numbv(2)
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
*   ------------------  calorimetry  ---------------------

      else if (Csys=='emc') then
*6*                                barrel calorimeter - K.Shester
        if (CALG_Nmodule(1)*CALG_Nmodule(2)>0) then
*          both left and right barrels:
           rileft = numbv(1)
           phi    = numbv(2)
           superl = numbv(3)
	else                   
*          only one barrel - left or write 
	   if(CALG_Nmodule(1)>0) then
              rileft=1
           else
              rileft=2
	   endif
           phi    = numbv(1)
           superl = numbv(2)
        endif
*
        eta=idigi(1)+1
        phi_sub=idigi(2)
        If (rileft==1) then
          phi=60-phi+1
          If (phi_sub==0) phi_sub=2
        else
          phi=60+phi
          phi_sub=phi_sub+1
        endif

	if(rileft<1 .or. rileft>2) then            
	  print *,'**ERROR at g2t_volume_id: emc rl ',rileft
*	else if(eta<1 .or. eta>CALG_NetaT)  then                 
*	  print *,'**ERROR at g2t_volume_id: emc eta ',eta
*	else if(phi<1 .or. phi>CALG_MaxModule)  then            
*	  print *,'**ERROR at g2t_volume_id: emc phi ',phi
*	else if(superl<1 .or. superl>CALG_NSub) then            
*	  print *,'**ERROR at g2t_volume_id: emc superl ',superl
	else 
	  volume_id=10000000*rileft+100000*eta+100*phi+
     +	              +10*phi_sub+superl
	endif

      else if (Csys=='smd') then
*7*
        if (CALG_Nmodule(1)*CALG_Nmodule(2)>0) then
           rileft   =numbv(1)
           phi      =numbv(2)
           forw_back=numbv(3)
        else
           if (CALG_Nmodule(1)>0) then
              rileft=1
           else
              rileft=2
           endif
           phi      =numbv(1)
           forw_back=numbv(2)
        endif

        eta  =idigi(2)+1
        strip=idigi(3)+1

        If (forw_back==4) forw_back=3
        If (rileft==1) then
          phi=60-phi+1
        else
          phi=60+phi
        endif     

	if(rileft<1 .or. rileft>2)  then                        
	  print *,'**ERROR at g2t_volume_id: smd rl ',rileft
*	else if(eta<1 .or. eta>calg_NetaSMDp) then                  
*	  print *,'**ERROR at g2t_volume_id: smd eta ',eta
*	else if(phi<1 .or. phi>CALG_MaxModule) then            
*	  print *,'**ERROR at g2t_volume_id: smd phi ',phi
	else if(forw_back<1 .or. forw_back>3) then            
	  print *,'**ERROR at g2t_volume_id: smd forw_back ',forw_back
	else if(strip<1) then            
	  print *,'**ERROR at g2t_volume_id: smd strip ',strip
	else if(forw_back=1.and.strip>calg_Netfirst) then            
	  print *,'**ERROR at g2t_volume_id: smd strip ',strip, forw_back
	else if(forw_back=2.and.strip>calg_Netsecon)  then            
	  print *,'**ERROR at g2t_volume_id: smd strip ',strip, forw_back
	else if(forw_back=3.and.strip>calg_NPhistr) then            
	  print *,'**ERROR at g2t_volume_id: smd strip ',strip, forw_back
	else 
          volume_id=100000000*rileft+1000000*eta+1000*phi+
     +              100*forw_back+strip
	endif
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      else if (Csys=='eem') then
*8*
* PN, MAX:
*      OnOff    = (0..3)  -  East-West config:  0-none,            1 - west,     2-east,   3-both
*      FillMode = (1..3)  -  Sectors fill mode: 1- one 3rd filled, 2 - one half, 3 - full
*
 
	if (cd=='ESCI') then

          if (emcg_onoff < 3) then
            rileft    = emcg_onoff
            shift     = 0
          else
            rileft    = numbv(1)
            shift     = 1
          endif
* we may have versions later than 5, that's why this has been changed to >=
	  if (emcg_version >= 5) then ! need to look at fillmode:
            if (emcg_FillMode <= 2 ) then
                iWheel = 1
            else
                iWheel = numbv(1+shift) 
                shift  += 1 
            endif

            section   = numbv(1+shift)                        ! ECVO
            phi_30d   = sector_hash(numbv(2+shift),iWheel)    ! EMOD
            zsubsect  = numbv(3+shift)                        ! ESEC (no readout)
            zsublayer = numbv(4+shift)                        ! EMGT (no readout)
            phi       = numbv(5+shift)                        ! EPER (5 fingers)
            eta       = numbv(6+shift)                        ! ETAR (radial division)

* we signal this once
            if(printOnce) then
               if (6+shift != nv) print *,' G2T_VOL_ID: new inconsistency in ECAL
               printOnce=.false.
            endif

            eemc_depth = zsubsect + 3*(section-1)

            volume_id = 100000*rileft + 1000*(5*(phi_30d-1)+phi) + 10*eta + eemc_depth
	  else ! version other than 5
            shift     = 1
            rileft    = numbv(0+shift)
            phi_30d   = numbv(1+shift)
            section   = numbv(2+shift)
            phi       = numbv(3+shift)
            eta       = numbv(5+shift)

            if(printOnce) then
               if (5+shift != nv) print *,' G2T_VOL_ID: old inconsistency in ECAL'
               printOnce=.false.
            endif

            volume_id = 100000*rileft+1000*(5*(phi_30d-1)+phi)+10*eta+section
          endif

	endif ! cd==ESCI

		  
      else if (Csys=='esm') then
*9* 
          if (emcg_onoff < 3) then
            rileft    = emcg_onoff
            shift     = 0
          else
            rileft    = numbv(1)
            shift     = 1
          endif
* see comment above about >=
	  if (emcg_version >= 5) then

            if (emcg_FillMode <= 2 ) then
                iWheel = 1
            else
                iWheel = numbv(1+shift) 
                shift  += 1 
            endif

          depth     = numbv(1+shift)
*         phi       = numbv(2+shift) 
          phi_30d   = sector_hash(numbv(2+shift),iWheel)
          strip     = numbv(3+shift) 

        else

* version before 5
          rileft    = numbv(1)
          depth     = numbv(2)
          phi       = numbv(3)
          strip     = numbv(4) 

	endif       
	volume_id = 1000000*rileft+10000*phi_30d+1000*depth+strip
 
*   ------------------ forward region ---------------------

      else if (Csys=='ftp') then
*10*
* ftpv=1 for west, 2 for east part of the FTPC
* ftpc_sector is the phi division of the gas layer
* the numbering scheme below designed by Janet Seyboth,
* but I'm adding the correct mapping between GEANT id's
* and the ones found on:
* http://wwwstar.mppmu.mpg.de/ftpc_calibration_page/calibration/calibration.html
* I use a hash table for a unique and clean way to number sectors
* --max--
        ftpv       = numbv(1)
        padrow     = numbv(2)
	ftpc_sector= ftpc_hash(numbv(3),ftpv)
        volume_id  = (100*ftpv+padrow)*10 + ftpc_sector
*   ---------------------
      else if (Csys=='vpd') then
*11*    Vertex position detector - Frank Geurts <geurts@rice.edu>

        if (vpdg_version == 1) then
          rileft    = numbv(1)
          innout    = numbv(2)
          sector    = numbv(3)
        else
          rileft    = numbv(1)
          innout    = 0
          sector    = numbv(2)
        endif
        volume_id  =  1000*rileft + 100*innout + sector
*
      else if (Csys=='pgc') then
*12*
      else if (Csys=='psc') then
*13*
      else if (Csys=='rch') then
        is=0
        if       (cd=='RGAP') {is=1}
        else if  (cd=='RCSI') {is=2} 
        else if  (cd=='QUAR') {is=3} 
        else if  (cd=='FREO') {is=4} 
        else if  (cd=='OQUA') {is=5} 

        volume_id = numbv(1) + Is*1000
*14*
      else if (Csys=='zdc') then
        volume_id = numbv(1)*1000+numbv(2)

*15*                                 pmd,     Bedanga
      else if (Csys=='pmd') then
        volume_id = numbv(1)*1000000 + numbv(2)*100000 + numbv(3)*10000 _
                                     + numbv(4)*100    + numbv(5)

*16*                          Mikhail Kopytine for the BBC group
      else if (Csys=='bbc') then
*         
*       BBC has 4 levels: west/east, annulus, triple module, single module
        volume_id = numbv(1)*1000 + numbv(2)*100 + numbv(3)*10 + numbv(4)    
*17*                                 Kai Schweda
      else if (Csys=='pix') then

c$$$    write (*,*) numbv

        """ Old numbering scheme """
        if ( pixl_alignment .eq. 0 ) then
        write (*,*) 'old numbering'
        sector = numbv(1)
        ladder = numbv(2)
        sensor = numbv(3)
        unknown = numbv(4)
        endif

        """ New numbering scheme (ladders in pxmo)"""
        if ( pixl_alignment .eq. 1 ) then

        sector =    ( numbv(1)-1)/4   + 1
        ladder = mod( numbv(1)-1, 4 ) + 1
        sensor = numbv(2)

        endif


        if ( pixl_misalign .eq. 1 ) then

        sensor = numbv(1) - 1     ! 0 ... 399
        sector = sensor / 40 + 1
        ladder = mod ( sensor / 10, 4 ) + 1
        sensor = mod ( sensor, 10 ) + 1

        endif

        "volume_id = numbv(1)*1000000 + numbv(2)*10000 + numbv(3)*100  + numbv(4)"
        volume_id = 1000000*sector + 10000*ladder + 100*sensor

*18* 
      else if ( Csys=='ist' .and. iist>= 0 ) then

          " The extra offset on the ladder number looks odd here...      "
          " But it was used in the legacy encoding (ver != 3 or 4 below) "
          " which we end up falling through to in the ist.               "

          if      ( ISTC_misalign .eq. 1 ) then

              sensor = numbv(1) - 1         ! 0 to 143
              ladder = sensor / 6 + 1 + 1   ! extra +1 as per legacy below...
              sensor = mod( sensor, 6 ) + 1 !


          else if ( ISTC_misalign .eq. 0 ) then
              ladder = numbv(1) + 1 ! IBAM
              sensor = numbv(2)     ! IBLM

          else
              STOP "IST misalignment in ambiguous state / test mode"
          endif
          volume_id = ladder * 1000000  +   
                      sensor *   10000  



      else if (Csys=='ist') then
        if(istVersion.ne.3.and.istVersion.ne.4) then
            istLayer=numbv(1)+1
            volume_id = istLayer*1000000 + numbv(2)*10000 + 100*numbv(3)  + numbv(4)
        endif
        if(istVersion.eq.3) then
            istLayer=3
            volume_id = istLayer*1000000 + numbv(1)*10000 + 100*numbv(2)  + numbv(3)
        endif
        if(istVersion.eq.4) then
            istLayer=2
            volume_id = istLayer*1000000 + numbv(1)*10000 + 100*numbv(2)  + numbv(3)
        endif
*19*                                 Kai Schweda
***   else if (Csys=='fst') then
***   09/24/2019 JCW Drop support for forward silicon rnd circa 2005
***     volume_id = numbv(1)*1000000 + numbv(2)*10000 + numbv(3)*100  + numbv(4)
*20*                                 Kai Schweda
      else if (Csys=='fgt') then
        volume_id = numbv(1)*100 + numbv(2)

*21*                                 Gerrit van Nieuwenhuizen
      else if (Csys=='igt') then
        volume_id = numbv(1)*1000000 + numbv(2)*10000 + numbv(3)*100  + numbv(4)
*22*                                 Sevil Salur
      else if (Csys=='hpd') then
        volume_id = numbv(1)*1000000 + numbv(2)*10000 + numbv(3)*100  + numbv(4)
*23*                                 Pibero Djawotho
      else if (Csys=='fpd') then

        if (ifpdmgeo>=3 ) {                                                    !! FMS Geometry
         n1 = numbv(1); n2 = numbv(2); sl = -999
         if(cd=='FLGR') sl=1
         if(cd=='FLXF') sl=2; 
         if(cd=='FPSC') sl=3; 
         if(cd=='FOSC') sl=4; 
         assert(sl.gt.0)        ! Wrong sensitive detector in FPD/FMS
         if(sl.le.2) then !fms or fpd
          ew=(n1-1)/2+1
          nstb = -999
          if(n1.eq.1) nstb=1; if(n1.eq.2) nstb=2
          if(n1.eq.3 .and. sl.eq.2) nstb=1
          if(n1.eq.4 .and. sl.eq.2) nstb=2
          if(n1.eq.3 .and. sl.eq.1) nstb=3
          if(n1.eq.4 .and. sl.eq.1) nstb=4 ; assert(nstb.gt.0) ! Wrong nstb in FPD/FMS
          ch=n2
          if(ew.eq.1) then
           if(ch.gt.49 .and. ch.le.56) then
              ch=ch-49
              nstb=nstb+4
           endif
           if(nstb.eq.1 .or. nstb.eq.5) then
              ch=ch + 6 - 2*mod(ch-1,7)
           else if(nstb.eq.3 .or. nstb.eq.4) then
              ch=ch + 4 - 2*mod(ch-1,5)
           endif
          else if(ew.eq.2) then
           if(nstb.le.2) then  
            if(n2.ge.11  .and. n2.le.21 )  ch=n2 +  7
	    if(n2.ge.22  .and. n2.le.33 )  ch=n2 + 13
	    if(n2.ge.34  .and. n2.le.46 )  ch=n2 + 18
	    if(n2.ge.47  .and. n2.le.60 )  ch=n2 + 22
	    if(n2.ge.61  .and. n2.le.75 )  ch=n2 + 25
	    if(n2.ge.76  .and. n2.le.91 )  ch=n2 + 27
	    if(n2.ge.92  .and. n2.le.125)  ch=n2 + 28
	    if(n2.ge.126 .and. n2.le.269)  ch=n2 + 36 + 8*((n2-126)/9)
	    if(n2.ge.270 .and. n2.le.319)  ch=n2 +156
	    if(n2.ge.320 .and. n2.le.334)  ch=n2 +157
	    if(n2.ge.335 .and. n2.le.348)  ch=n2 +159
	    if(n2.ge.349 .and. n2.le.361)  ch=n2 +162
	    if(n2.ge.362 .and. n2.le.373)  ch=n2 +166
	    if(n2.ge.374 .and. n2.le.384)  ch=n2 +171
	    if(n2.ge.385 .and. n2.le.394)  ch=n2 +177   
           else
            if(n2.ge. 85 .and. n2.le.154)  ch=n2 +  5 + 5*((n2-85)/7)
	    if(n2.ge.155 .and. n2.le.238)  ch=n2 + 50 
           endif
          endif
          volume_id=ew*10000+nstb*1000+ch       
       else if(sl.eq.3) then    ! FPS (FMS-Preshower)
          ew=2
          layr=n1
          if(layr.eq.4) layr=3
          if(n2.le.21) then
             quad=1
             slat=n2
          else if(n2.le.21+19) then
             quad=2
             slat=n2-21
          else if(n2.le.21+19+21) then
             quad=3
             slat=n2-21-19
          else
             quad=4
             slat=n2-21-19-21
          endif
          volume_id=100000+ew*10000+quad*1000+layr*100+slat
       else                     ! FPOST (FMS-Postshower)
          ew=2          
          if(n1.le.5) then
             quad=1
             if(n1.le.2) then
                layr=n1
             else
                layr=n1+1
             endif
          else
             quad=2
             layr=n1-3
          endif
          slat=n2
          volume_id=200000+ew*10000+quad*1000+layr*100+slat  
       endif
      }
*24*                                 Dmitry Arkhipkin
      else if (Csys=='fsc') then
        volume_id = numbv(1);

*25*                                 Frank Geurts
      else if (Csys=='mtd') then
        sector = numbv(1)
        module = numbv(2)
        layer  = numbv(3)
        volume_id = 1000*sector + 100*module + layer

*******************************************************************************************
       elseif (Csys=='etr') then

          sector = MOD( (numbv(1)-1), 12 );   "Sectors count from 0 - 11"
          layer  =      (numbv(1)-1)/ 12;     "Layers  count from 0 - 2"
          section= numbv(2) - 1

          """ sector  from 0 - 11  (phi division)      """
          """ layer   from 0 - 2   (z-division)        """
          """ section from 0 - 29  (division of layer) """

          volume_id = section + 100*layer + 10000*sector

          <W> numbv(1), numbv(2), sector, layer, section, volume_id; 
    ('numbv =',I4,2X,I4,' sector=',I4,' layer=',I4,' section=',I4,' vid=', I7);


*******************************************************************************************
*26* 				Prashanth Shanmuganathan(HCAL)
      elseif (Csys=='@@@'  ) then """ Used to be HCA, but we have deprecated this version """
      """  Volume_Id """
      """ HCAL             1 000  """
      """ FPSC           100 000  """
      """ FTB             10 000  """
      """ PreShower 2014     100  """

          " Hcal lead scintilationg fiber cells"
	  if (cd=='HCEL' .or. cd=='HCES') then
	      hcal_tower = numbv(1)
	      hcal_cell  = numbv(2)

	      " Calorimeter cell volume ID "
	      if (cd=='HCEL') hcal_sl = 2 
	      if (cd=='HCES') hcal_sl = 1 

	      volume_id  =    100 * hcal_tower + 10 * hcal_sl + hcal_cell 
	  endif

	  " Preshower volume ID dev16"
	  if (cd=='FPSC') then
	     n1 = numbv(1)
	     n2 = numbv(2)
	     ew = 2
	     layr = n1 
	     if ( layr==4 ) layr = 3
	     if ( n2 <= 21 ) then
		quad = 1
		slat = n2
	     else if ( n2 <= 21+19 ) then
		quad = 2
		slat = n2 - 21
	     else if ( n2 <= 21+19+21 ) then
		quad = 3 
		slat = n2 - 21 - 19
	     else
		quad = 4
		slat = n2 - 21 - 19 - 21
	     endif

	  volume_id = 100000+ew*10000+quad*1000+layr*100+slat
	  endif

	  " FTBF setup hctest"
	  if (cd=='BBCF') volume_id = 10001
	  if (cd=='BBCB') volume_id = 10002
	  if (cd=='LEDG') volume_id = 10003

	  "Preshower for y2014b"
	  if (cd=='HSTP') then
	     if(numbv(1) == 1) volume_id = 21 - numbv(2)
	     if(numbv(1) == 2) volume_id = 41 - numbv(2)
	  endif


*******************************************************************************************
** 27                                                                            Jason Webb
      ELSE IF (CSYS=='fts') THEN         

           volume_id = g2t_fst_volume_id( numbv )
c          write (*,*) csys, volume_id

      ELSE IF (CSYS=='fst') THEN "Forward silicon tracker" 

           volume_id = g2t_fst_volume_id( numbv )
c          write (*,*) csys, volume_id

      ELSE IF (CSYS=='stg') THEN "Small thin gap chambers"
           volume_id = g2t_stg_volume_id( numbv )
      ELSE IF (CSYS=='wca') THEN "FCS EM calorimeter"
           volume_id = g2t_wca_volume_id( numbv )
      ELSE IF (CSYS=='hca') THEN "FCS Hadronic calorimeter"
           volume_id = g2t_hca_volume_id( numbv )
      ELSE IF (CSYS=='pre') THEN "FCS Preshower"
           volume_id = g2t_pre_volume_id( numbv )

*******************************************************************************************
** 28                                                                           Prashanth S 
      ELSE IF (CSYS=='epd') THEN
         
           epd_epdm = numbv(1) "1 for east, 2 for west mother wheel"  
           epd_epss = numbv(2) "1 for PP1, 2 for PP2, PP-postion 1'o,2'o clock etc"
           epd_epdt = numbv(3) "1:T1 trap, 2:T1 Triangular, 3:T2 Thin, 4:T3 Thick"

	   volume_id = 100000 * epd_epdm                          +
	                 1000 * epd_epss                          +
		           10 * (mod(epd_epdt,2) + (epd_epdt/2) ) +
			    1 * (mod(epd_epdt,2)) 

     " EPD volume_id " 
     " 100,000 : east or west "
     "   1,000 : Position clock wise, 1 to 12 "
     "      10 : Tile number 1 to 31, refer EPD Drupal page"
     "       1 : 1 T1 trap or T2 thin; 0 T1 triangular or T2 thick
	               

*write (*,*) 'g2t volume id: epd=', epd_epdm,'  ',epd_epss,'  ',epd_epdt,'  ',
*    (mod(epd_epdt,2) + (epd_epdt/2) ),' ',mod(epd_epdt,2) ,'  ',volume_id
   


*******************************************************************************************
** 29                                                                           Jason Webb
      ELSE IF (CSYS=='eto') THEN

         """Endcap TIME OF FLIGHT"""

         etof_plane    = numbv(1)/100          "1 closest to IP, 3 furthest from IP"
         etof_sector   = mod( numbv(1), 100 )  "matches TPC scheme 13 to 24"
         etof_counter  = numbv(2)              "3 counters per gas volume"
         etof_gap      = numbv(3)              "12 gaps between glass"
         etof_cell     = numbv(4)              "32 cells per gap"

        

         volume_id = etof_cell               + 
                     100      * etof_gap     +
                     10000    * etof_counter + 
                     100000   * etof_sector  +   
                     10000000 * etof_plane

         """ Note: this last part could just be 100000*numbv(1).  We break it """
         """ into plane and sector just to make explicit the numbering scheme """
                     
      else
          print *,' G2T warning: volume  ',Csys,'  not found '  
      endif


    g2t_volume_id = volume_id

    end

!//______________________________________________________________________________________      
    Integer function g2t_tpc_volume_id ( numbv )
      Integer, intent(in) :: numbv(15)

      Logical :: first = .true.
      
      Structure  TPCC  { version,misalign  }      
      Structure  TPCG  { version,tpadconfig  }
      Structure  TPRS  { sec,nrow }


      Integer ::       Iprin,Nvb
      Character(len=4) ::           cs,cd
      COMMON /AGCHITV/ Iprin,Nvb(8),cs,cd

      Integer :: npadi = 0, npado = 0, npad = 0
      Integer,Parameter :: fpad = 1
      Integer,Parameter :: fsec = 100

      Integer :: tpads(156,24), isdet(156,24), i, j, n, m

      Integer :: tpgv, tpss, pad, det, prompt, sector

      


      if ( first ) then

         call rbpushd  "save current zebra directory"
         first = .false.
         USE /DETM/TPCE/TPCC       stat=itpcc
         USE /DETM/TPCE/TPCG       stat=itpcg
         USE /DETM/TPCE/TPRS       stat=itprs
         npadi = TPRS_nrow
         USE /DETM/TPCE/TPRS       stat=itprs 
         npado = TPRS_nrow
         call rbpopd   "restore original zebra directory"


        !write (*,*) 'We are in FIRST', itpcg, itprs
        !write (*,*) 'We have TPCC  version    = ', TPCC_version
        !write (*,*) 'We have TPCG  version    = ', TPCG_version
        !write (*,*) 'We have TPCG  tpadconfig = ', TPCG_tpadconfig
        !write (*,*) 'We have TPRS(sec=1) nrow = ', npadi
        !write (*,*) 'We have TPRS(sec=2) nrow = ', npado


         ! zero out both arrays to start
         tpads(1:156,:) = 0
         isdet(1:156,:) = 0  

         ! Default will be the TPC pre iTPC upgrade



         npad = 73 """13 inner + 2x13 fake + 32 outer + 2 fake """

       DO I=1,24
         tpads(1:73,I) = (/     1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 
                              4, 4, 5, 5, 5, 6, 6, 6, 7, 7, 
                              7, 8, 8, 8, 9, 9, 9,10,10,10, 
                             11,11,11,12,12,12,13,13,13,14, 
                             14,15,16,17,18,19,20,21,22,23, 
                             24,25,26,27,28,29,30,31,32,33, 
                             34,35,36,37,38,39,40,41,42,43, 
                             44,45,45   /)

         isdet(1:73,I) = (/ 1, 0, 2, 1, 0, 2, 1, 0, 2, 1,
                          0, 2, 1, 0, 2, 1, 0, 2, 1, 0,
                          2, 1, 0, 2, 1, 0, 2, 1, 0, 2,
	                  1, 0, 2, 1, 0, 2, 1, 0, 2, 1,
	                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		          0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		          0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                          0, 0, 2 /)
        ENDDO

         ! Otherwise handle the iTPC upgrade inner = 40 rows outer = 32
         if (TPCC_version.ge.6) then

         write (*,*) 'Info :: g2t_volume_id :: iTPC geometry will be decoded'

         npad = 76 """ 40 inner + 2 fake + 32 outer + 2 fake """

        DO I=1,24
         tpads(1:76,I) = (/   1,  1,  2,  3,  4,  5,  6,  7,  8,  9,
                           10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
                           20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
                           30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
                           40, 40, 41, 41, 42, 43, 44, 45, 46, 47, 48, 49,
                           50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
                           60, 61, 62, 63, 64, 65, 66, 67, 68, 69,
                           70, 71, 72, 72 /)
                      
         isdet(1:76,I) = (/   1,  0,  0,  0,  0,  0,  0,  0,  0,  0,
                            0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
                            0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
                            0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
                            0,  2,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,
                            0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
                            0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
                            0,  0,  0,  2 /)
        ENDDO
                      
         endif


      ! Detect and handle the mixed scenario
      IF ( tpcc_version .eq. 3.2 ) THEN
         I = 20
         tpads(1:73,I) = (/   1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 
                              4, 4, 5, 5, 5, 6, 6, 6, 7, 7, 
                              7, 8, 8, 8, 9, 9, 9,10,10,10, 
                             11,11,11,12,12,12,13,13,13,41, 
                             41,42,43,44,45,46,47,48,49,50, 
                             51,52,53,54,55,56,57,58,59,60, 
                             61,62,63,64,65,66,67,68,69,70, 
                             71,72,72   /)
         isdet(1:73,I) = (/ 1, 0, 2, 1, 0, 2, 1, 0, 2, 1,
                          0, 2, 1, 0, 2, 1, 0, 2, 1, 0,
                          2, 1, 0, 2, 1, 0, 2, 1, 0, 2,
	                  1, 0, 2, 1, 0, 2, 1, 0, 2, 1,
	                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		          0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		          0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                          0, 0, 2 /)

      ENDIF

      endif!first           

      tpgv = numbv(1)
      tpss = numbv(2)
      sector  = tpss + 12*(tpgv - 1)    
      pad  = numbv(3)
      det  = 0

      If  (tpcg_version==1) then

          If (cd=='TPAI')  det=1
          If (cd=='TPAO')  det=2
*PN:      outermost pseudopadrow:
          If (cd=='TPAO' & pad==14) pad=45

      else

          if (pad .gt. npad) pad = pad - npad

          det = isdet(pad,sector); ! flag as fake or not
          pad = tpads(pad,sector); ! remap fake padrows onto nearest real one
          IF ( tpcc_version .eq. 3.2 .and. sector.eq.20 .and. pad .lt.14 ) THEN
             sector = 99 "Effectively blind TpcRS to these hits"
          ENDIF
        endif      

      g2t_tpc_volume_id = 100000*det + 100*sector + pad

      RETURN
    end
!//______________________________________________________________________________________      
    Integer function g2t_fst_volume_id ( numbv )
      Integer, intent(in) :: numbv(15)
      Integer             :: disk, wedge, sensor
      Integer,parameter   :: mapping(3) = (/ 2, 3, 1 /)

      Structure  FSTG { Version }
      Integer    ifst
      Logical    first /.true./

      Integer          Iprin,Nvb
      Character(len=4)              cs,cd
      COMMON /AGCHITV/ Iprin,Nvb(8),cs,cd

      IF ( first ) then
         USE  /DETM/FSTM/FSTG  stat=ifst
      ENDIF 


      IF (cd=='FTUS' ) then     

      disk   = numbv(1)         " disk is 4,5,6 for historical reasons"
      wedge  = numbv(2)         " 12 wedges"   
      sensor = numbv(3)         " sensor number"
      sensor = mapping(sensor)  " remap sensor 1=inner, 2,3=outer "

      ENDIF

      "Inner wedge assembly (deprecated geometry model)"
      IF (cd=='FTIS') then

      disk  = numbv(1) "There are 3 silicon disks"
      wedge = numbv(2) "There are 12 wedges"
      sensor= 1        "There are 2+1 sensors" 
                       "1=inner wedge, 2,3=outer wedge"

      ENDIF

      "Outer wedge assembly (deprecated geometry model)"
      IF (cd=='FTOS') then

      disk  = numbv(1) "There are 3 silicon disks"
      wedge = numbv(2) "There are 12 wedges"
      sensor= numbv(3) "There are 2+1 sensors" 
                       "1=inner wedge, 2,3=outer wedge"

      ENDIF


     IF ( istat>=0 .and. fstg_version .ge. 1.2 ) then "misaligned geometry"

         ! loop over 3 disks
         ! loop over 12 wedges
         ! place 3 instances of FTUS

         disk = ( (numbv(1)-1)  / 36 ) + 4
         wedge = mod((( numbv(1)-1 ) / 3 ), 12 ) + 1
         sensor = mod((numbv(1)-1),3) + 1
         
      ENDIF

     g2t_fst_volume_id = 1000*disk + 10*wedge + sensor   
   



      
!$$$  write (*,*) cd, numbv(1:5), g2t_fst_volume_id

    End Function g2t_fst_volume_id

!//______________________________________________________________________________________      
    Integer function g2t_stg_volume_id ( numbv )
      Integer, intent(in) :: numbv(15)
      
      g2t_stg_volume_id = numbv(1)

    End Function g2t_stg_volume_id
!//______________________________________________________________________________________      
     Integer function g2t_wca_volume_id( numbv ) 
       Integer, intent(in) :: numbv(15)
       Integer             :: mod, tow

       mod = numbv(1)
       tow = numbv(2) "nx = 22 ny = 34, nx*ny = 748"

       g2t_wca_volume_id = mod*1000 + tow

     End Function g2t_wca_volume_id
!//______________________________________________________________________________________      
     Integer function g2t_hca_volume_id( numbv ) 
       Integer, intent(in) :: numbv(15)
       Integer             :: mod, tow

       mod = numbv(1)
       tow = numbv(2) "nx = 13 ny = 20, nx*ny = 260"

       g2t_hca_volume_id = mod*1000 + tow

     End Function g2t_hca_volume_id
!//______________________________________________________________________________________      
     Integer function g2t_pre_volume_id( numbv ) 
       Integer, intent(in) :: numbv(15)
       Integer             :: layer, slat

       layer = numbv(1)
       slat = numbv(2)

       g2t_pre_volume_id = layer*1000 + slat

     End Function g2t_pre_volume_id
!//______________________________________________________________________________________      
