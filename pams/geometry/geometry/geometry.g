* $Id: geometry.g,v 1.290 2015/10/12 18:36:35 jwebb Exp $
* $Log: geometry.g,v $
* Revision 1.290  2015/10/12 18:36:35  jwebb
* Initial version of dev2020 geometry tag including forward tracking system.
*
* Revision 1.289  2015/07/14 21:12:53  jwebb
* Added y2015a production geometry, initial release
*
* Revision 1.288  2015/06/19 13:48:25  jwebb
* Added HCAL test configuration (hctest)
*
* Revision 1.287  2015/06/08 18:10:38  jwebb
* Enable secondary tracking (hit association) for pixel (and FGT) -- AgSFlag('SIMU',2).
*
* Revision 1.286  2015/05/19 19:29:20  jwebb
* Associate hits on 2ndary tracks to the track, not the primary track which initiates decay/shower.  https://www.star.bnl.gov/rt3/Ticket/Display.html?id=3092
*
* Revision 1.285  2015/04/17 19:14:51  jwebb
* ... also need to be careful to call the right geometry module.  Needed an extra flag in geometry.g to handle this.
*
* Revision 1.284  2015/04/17 14:54:59  jwebb
* Corrected configuration error in VPD in y2015/agml/agstar geometry.
*
* Revision 1.283  2015/01/06 19:45:16  jwebb
* Correct version of VPD in 2015
*
* Revision 1.282  2015/01/06 19:06:46  jwebb
* Added FGT to HCAL dev2016 geometry
*
* Revision 1.281  2015/01/06 15:58:40  jwebb
* Add HCAL to dev2016
*
* Revision 1.280  2014/12/22 22:21:22  jwebb
* dev2016 geometry implemented in starsim
*
* Revision 1.279  2014/08/18 18:50:38  jwebb
* Updated y2014 tag w/ MTD y2014 configuration, not available for fast offline production.  Change required so that MTD match maker can run on all y2014 tags.  MTD does not participate in tracking and is outside of the acceptance of the tracker, so this modification does not affect the reproducability of the fast offline period.
*
* Revision 1.278  2014/05/14 20:02:49  jwebb
* Support for FMS preshower in dev15a.  Support for HCAL test in y2014b.  Support for HCAL proposal in dev15b.
*
* Revision 1.277  2014/04/02 21:03:16  jwebb
* y2014x geometry is moved to y2014a production.
*
* Revision 1.276  2014/03/24 19:58:30  jwebb
* Setup y2014a candidate as y2014a.
*
* Revision 1.275  2014/03/17 21:30:18  jwebb
* Fix bug in y2013 definitions eliminating pixel support tube.
*
* Revision 1.274  2014/03/11 18:24:15  jwebb
* Defined y2013_1a, _1b, _1c, _2a, _2b, _2c geometry tags.
*
* y2013_1x and y2013_2x were mistakenly used in the y2013 data production, using
* library SL14a.  The tags y2013_1c and y2013_2c freeze the y2013_1x and y2013_2x
* geometry tags ase they exist in SL14a.  They are equivalent to the geometries
* used in y2013 data production.
*
* Added y2010x asymptotic tag for STV testing.
*
* Revision 1.273  2014/02/11 21:42:40  jwebb
* kOnly --> konly.
*
* Revision 1.272  2014/01/24 19:45:13  jwebb
* Revert to CaveGeo for y2014 geometry.
*
* Revision 1.271  2014/01/23 23:04:39  jwebb
* Make sure pixl support shows up in y2014.
*
* Revision 1.270  2014/01/23 17:04:34  jwebb
* Added y2013b production geometries with extra HFT dead material near east
* poletip.  Modified y2014 first cut.
*
* Revision 1.269  2013/12/09 14:45:58  jwebb
* Changes to pixel detector for y2014.
*
* Revision 1.268  2013/12/04 20:01:33  jwebb
* Removes FGT cables from IDSM in y2014.
*
* Revision 1.267  2013/12/03 16:54:36  jwebb
* Added y2014 first cut geometry.
*
* Revision 1.266  2013/09/30 16:12:09  jwebb
* Fixes for IDSM / PIPE.
*
* Revision 1.265  2013/09/12 18:09:21  jwebb
* Moved IDS construction before beam pipe.
*
* Revision 1.264  2013/09/11 14:27:21  jwebb
* Added case statements to enable y2013a geometries.
*
* Revision 1.263  2013/09/10 18:59:04  jwebb
* Definitions for y2013a, new beam pipe module and new ist module.
*
* Revision 1.262  2013/08/13 18:55:54  jwebb
* Defined a CONSTRUCT keyword in geometry.g.  CONSTRUCT calls a geometry
* module using comis.  If the subroutine is not linked with the program,
* it will not be called and a warning will be issued.  This warning can
* be caught and parsed for dependency analysis.
*
* For the case where one or more moduels are missing, a fortran STOP will
* be issued to prevent invalid geometries from being used.
*
* Revision 1.261  2013/07/10 21:19:08  jwebb
* Correction to eStar2 definition.
*
* Revision 1.260  2013/07/09 18:37:00  jwebb
* Additions to support eStar2 model.
*
* Revision 1.259  2013/05/22 14:39:15  jwebb
* Added new version of the CAVE as CAVE05.  Better dimensions, walls, platform,
* crates.  The y2013x (asymptotic) STAR uses CAVE05.  First cut geometry remains
* at CAVE04.
*
* Revision 1.258  2013/02/26 15:25:32  jwebb
* Updates to the Y2013 first cut and asymptotic tags.  Pixel support tube
* remains in place when pixel detector is removed.
*
* Revision 1.257  2013/02/21 22:51:24  jwebb
* Defined pixel detector in and out geometries.
*
* Revision 1.256  2013/02/06 21:58:09  jwebb
* Corrections to y2013 geometry tag.  Addition of y2012b geometry tag to
* properly include the MTD.
*
* Revision 1.255  2013/02/05 21:26:31  jwebb
* Corrected double placement of volume in PixlGeo5
*
* Revision 1.254  2013/02/05 16:58:36  jwebb
* Definition of the y2013 and y2013x geometry tags.
*
* Revision 1.253  2013/01/22 18:27:10  jwebb
* Defined Y2013x geometry.
*
* Revision 1.252  2013/01/17 21:04:52  jwebb
* Support for improved magnet model, improved trim coil description, in y2013x
* geometry tag.
*
* Revision 1.251  2012/12/19 14:48:39  jwebb
* Updates to support y2013 version of MTD.
*
* Revision 1.250  2012/12/14 16:13:56  jwebb
* Updates for y2013 geometry.
*
* Revision 1.249  2012/11/02 15:03:21  jwebb
* Fix format statement for SL4.4.
*
* Revision 1.248  2012/11/01 20:49:16  jwebb
* Add TOF_MAX configuration option.
*
* Revision 1.247  2012/08/27 17:50:28  jwebb
* Added estar1 and tpciv1 geometry tags.
*
* Revision 1.246  2012/08/27 14:56:12  jwebb
* Added vfgt to definition of the estar development geometry.
*
* Revision 1.245  2012/07/02 18:50:36  jwebb
* Few more modifications for dev14 geometry.  Add VPD and PXST to dev14.
*
* Revision 1.244  2012/06/29 15:10:51  jwebb
* Added DEV14 geometry tag.
*
* Revision 1.243  2012/05/31 20:57:28  jwebb
* Added y2012a production tag
*
* Revision 1.242  2012/05/07 21:44:06  jwebb
* Added "devT" geometry tag for TPC upgrade studies.
*
* Revision 1.241  2012/03/28 15:14:17  jwebb
* Switched pipe12 (old beam pipe) to pipev1 (new narrow beam pipe) for dev13
* geometry with the HFT.
*
* Revision 1.240  2011/11/22 15:09:19  jwebb
* Added "devE" tag for eStar development.
*
* Revision 1.239  2011/10/13 18:23:58  jwebb
* Added production geometry tag y2011a.  Tag y2011a is consistent with the y2011
* geometry tag, as it exists in the SL11c and SL11d libraries.  y2011a should be
* used for any reproduction of production series PL11ic and PL11id.
*
* Revision 1.238  2011/10/07 19:44:45  jwebb
* Switched versions of the PIXL detector.
*
* Revision 1.237  2011/10/06 20:38:22  jwebb
* Fixed FGT setup.
*
* Revision 1.236  2011/10/06 19:54:48  jwebb
* Moved IDSM earlier in the call sequence.
*
* Revision 1.235  2011/10/06 15:52:05  jwebb
* Added dev13 tag.
*
* Revision 1.234  2011/10/06 14:54:00  jwebb
* Added DEV13 geometry.  Removed pmd from y2012.  Added pixl to complete.
*
* Revision 1.233  2011/10/03 22:03:06  jwebb
* Redefined the "complete" geometry for use in anticipated simulations with
* FGT.  Did not add the HFT as we do not yet have a geometry for that.
*
* Revision 1.232  2011/08/19 16:44:14  jwebb
* Definition of Y2012 geometry tag (1st cut).
*
* Revision 1.231  2011/07/20 20:23:47  jwebb
* Upgr23 tag defined with FSC geometry and FMS in open position.
*
* Revision 1.230  2011/07/18 15:53:12  jwebb
* Reverted to single FGT "upgr2012" geometry.
*
* Revision 1.229  2011/07/06 17:39:01  jwebb
* Defined upgr2012a: 2-disk FGT
*
* Revision 1.228  2011/05/12 19:42:42  jwebb
* Definition of upgr2012 geometry now includes IdsmGeo1 and FgtdGeo3 by default.
*
* Revision 1.227  2011/05/02 20:22:16  jwebb
* Added code to pass configuration of the MTD to the mutdgeo4 module.
*
* Revision 1.226  2011/04/25 18:27:00  jwebb
* Added Y2008e tag, which implements the LOW_EM central calorimeter cuts in
* the y2008 geometry.
*
* Revision 1.225  2011/04/11 17:37:56  jwebb
* Introduce UPGR2012 geometry as y2011 with inner detectors (FGT, SSD) and
* supports removed.
*
* Revision 1.224  2011/03/11 00:05:18  jwebb
* Added Y2008d, Y2009d, Y2010c and updated Y2011 geometry tags.  These tags
* now contain an improved model of the SVT support cone... specifically the
* support rods.  Previous geometry assumed solid carbon.  Now we assume a
* carbon-fiber nomex sandwich.
*
* Revision 1.223  2010/12/22 00:13:00  jwebb
* Correction to the documentation bank in the fzd file for y2008c geometry.
*
* Revision 1.222  2010/12/21 17:21:31  jwebb
* Added Y2008c tag.
* Added Y2009c tag.
* Added y2010b tag.
*
* These three tags represent the current best model of the STAR detector,
* including TOF geometry, for Y2008 - Y2010.  Previous models used a TPC
* envelope which was too large and overlapped with 'kOnly' volumes in the
* TOF, causing 'odd' hit distributions.
*
* Revision 1.221  2010/12/17 20:01:24  jwebb
*
* Defined TPCE04r (reduced TPC envelope radius) and BTOF67 (btof sensitive
* volume size fix) and incorporated them into Y2011 tag.
*
* Revision 1.220  2010/11/12 20:04:14  jwebb
* Added y2008b geometry tag with most recent models of the TPC, endcap
* and barrel.
*
* Revision 1.219  2010/10/31 16:27:32  jwebb
* Switch PHMD on.  Modified configuration of MUTD at request of Bill Llope.
*
* Revision 1.218  2010/07/30 18:31:29  jwebb
* Added development / baseline y2011 geometry tag and reduced the number
* of write statements.
*
* Revision 1.217  2010/07/12 18:47:20  jwebb
* Added y2005i tag to provide up-to-date version of ecal in y2005 geometry
* and to provide 10 keV transport cuts in calorimeters.
*
* Revision 1.216  2010/06/01 18:57:41  jwebb
* Modified geometry.g so that seperate particle transport cuts can be
* used in the BEMC and EEMC.  This is needed for the spin/dijet simulation
* request summarized here:
*
* http://drupal.star.bnl.gov/STAR/starsimrequests/2010/may/26/filtered-dijet-simulation
*
* The simulation request applies filters which select out barrel- and
* adjacent-jetpatch triggers.  Thus, the endcap is only important for
* trigger response.  Requestors have noted a x2 increase in speed when
* EEMC is run with higher tracking cuts than in the BEMC.
*
* We define the Y2009b tag to apply the 10keV default tracking cuts in
* the BEMC, and revert the EEMC to the 80kev/1MeV (photon/electron) cuts.
*
* Revision 1.215  2010/05/25 21:21:50  jwebb
* y2010a geometry tag commit.  Tag is identical to y2010, except dependency
* on y2009a is removed.
*
* Revision 1.214  2010/04/19 16:16:03  jwebb
* Restored code to save geometry tag and field setting in the FZD file.
*
* Revision 1.213  2010/04/13 21:47:52  jwebb
* Added PHMD to y2010 geometry in preparation for y2010 simulations.
*
* Revision 1.212  2009/12/23 21:39:23  jwebb
* (1) Code modified to use 10 keV e- and gamma transport cuts in the barrel
*     and endcap calorimeters.   Note that this only takes effect in geometries
*     using the ecalgeo6 and calbgeo2 geometries.
*
* (2) Explicitly disabled effects of (1) for existing geometry tags (y2000-
*     y2008).
*
* (3) Side effects possible in upgrade geometries (i.e. calorimeter cuts may
*     be effected depending on specific geometry options).
*
* Revision 1.211  2009/12/22 13:42:16  jwebb
* Added options to set geant tracking cuts for electrons and photons in the
* B/EEMC to 10, 30, 100 or 1000 keV.
*
* Revision 1.210  2009/11/24 23:49:28  jwebb
* Changed to the TPCE04 model of the TPC in y2006h.  This is the current best
* geometry of the TPC.  TPCE04 is present in tags from y2005 to present.
*
* Revision 1.209  2009/11/19 18:24:15  perev
* y2009a and inherited from it y2010
*
* Revision 1.208  2009/11/18 20:46:34  jwebb
* Y2009A production tag added:
*
* (1) Y2009A includes the new model of the EEMC
* (2) Revert to old model of EEMC in Y2009 tag for compat w/ W preproduction.
* (3) Removed the y2006dev tag
* (4) Added y2006 h tag, which is y2006g + new model of the endcap.
*
* Revision 1.207  2009/11/17 16:18:47  jwebb
* Added y2006dev with new endcap model.  y2006dev (and future y200[3-8]dev)
* will be for testing purposes only.
*
* Revision 1.206  2009/11/16 22:37:48  jwebb
* Added logic to support multiple ecalgeoX files (subroutines).  ecalgeo6
* (version 6.1) set as default version for y2009 tag.
*
* Revision 1.205  2009/10/29 00:00:21  perev
* y2010=y2009+Full BTOF
*
* Revision 1.204  2009/09/25 18:02:16  perev
* Comment corrected
*
* Revision 1.203  2009/09/24 00:36:57  perev
* BTOFc6 <== BTOFc7 in y2009 F.Geurts
*
* Revision 1.202  2009/09/23 23:28:51  perev
* BugFix. pipe14==>pipe12 in y2008 & y2009
*
* Revision 1.201  2009/08/28 16:50:12  perev
* CleanUp of write(*,*)
*
* Revision 1.200  2009/08/26 20:13:15  perev
* JanB pixel off for upgr16
*
* Revision 1.199  2009/08/21 18:33:01  perev
* PMD off for y2009
*
* Revision 1.198  2009/08/19 22:48:11  perev
* Jan: thinner beam pipe for upgr16
*
* Revision 1.197  2009/08/18 17:29:13  perev
* F.Geurts TOF for run 9
*
* Revision 1.196  2009/08/14 22:38:06  perev
* Remove Cone fr upgr16 (Jan request)
*
* Revision 1.195  2009/07/14 01:02:59  perev
* Increase interaction/decay volume
*
* Revision 1.194  2009/06/22 22:21:44  perev
* Remove redundant messages
*
* Revision 1.193  2009/04/20 23:14:22  perev
* upgr22 fhcmgeo
*
* Revision 1.192  2009/03/20 02:57:12  perev
* upgr16a == upgr16 +tpc2009
*
* Revision 1.191  2009/03/13 21:08:27  perev
* y2005h, y2007h added
*
* Revision 1.190  2009/03/07 01:04:00  perev
* SSD shield fix + cleanup
*
* Revision 1.189  2009/02/22 21:36:23  perev
* Y2009 born
*
* Revision 1.188  2009/02/20 21:35:05  perev
* upgr15 full tof. Jonathan/Spiros
*
* Revision 1.187  2009/02/19 00:27:22  perev
* Upgr15==>macros
*
* Revision 1.186  2009/02/13 19:20:34  perev
* back BTOF for 2008. Again itof=6 bTofConfig=11
*
* Revision 1.185  2009/01/13 03:19:12  perev
* Mag field nou controlled from starsim. BugFix
*
* Revision 1.184  2009/01/12 00:31:44  perev
* Bug fix in ON logic(VP)
*
* Revision 1.183  2009/01/08 20:16:46  perev
* Fix y2008a and y2009 btof
*
* Revision 1.182  2009/01/06 04:05:48  perev
* For y2008a,y2009 elliptic rods
*
* Revision 1.181  2009/01/03 23:03:36  perev
* BtofConfig=6 in 2008a,2009
*
* Revision 1.180  2008/12/30 19:41:09  perev
* 1st version of y2009
*
* Revision 1.179  2008/12/15 01:03:56  perev
* CleanUp
*
* Revision 1.178  2008/12/12 20:45:13  perev
* upgr16/17 btofConfig=6
*
* Revision 1.177  2008/12/08 23:02:20  perev
* C++ style comment removed
*
* Revision 1.176  2008/12/08 19:28:29  didenko
* fixed typo
*
* Revision 1.175  2008/12/05 23:46:25  perev
* y2008 bTofConfig=6 now(jan)
*
* Revision 1.174  2008/12/01 23:45:10  perev
* ubgr16 last vers BTOF
*
* Revision 1.173  2008/11/30 01:30:53  perev
* modifs for extending alpha,theta,phi,ort commandas
*
* Revision 1.172  2008/11/19 04:08:25  perev
*  updates to the corrected(vp) starsim
*
* Revision 1.171  2008/10/13 03:21:35  perev
* upgr17 added Wei(MingZhang)
*
* Revision 1.170  2008/10/13 00:22:19  perev
* upgr16 pipe changed to provisional
*
* Revision 1.168  2008/09/25 03:05:58  perev
* upgr16 (Jan)
*
* Revision 1.167  2008/06/03 22:27:16  fisyak
* Add y2005g and y2007g geometries for SVT with latest Renes corrections
*
* Revision 1.166  2008/04/23 22:00:29  perev
* tofZ0=0.00 ==> tofZ0=-0.50 /xin
*
* Revision 1.165  2008/03/20 18:45:28  perev
* Simplest.gerrit upgr15 added
*
* Revision 1.164  2008/01/21 01:11:02  perev
* TOF weight corrected
*
* Revision 1.163  2007/11/13 21:38:08  perev
* pipeFlag and nSvtLayer==7 added
*
* Revision 1.162  2007/11/07 21:25:41  perev
* btofgeo6 added by X.Dong
*
* Revision 1.161  2007/11/06 01:19:35  perev
* y2008 geo
*
* Revision 1.160  2007/10/13 01:27:27  perev
* u2007 ==> upgr20
*
* Revision 1.159  2007/09/28 18:54:08  perev
* dongx/TOFr/y8update
*
* Revision 1.158  2007/09/25 19:56:14  perev
* U2007A added
*
* Revision 1.157  2007/09/21 20:30:08  perev
* Add U2007 geometry
*
* Revision 1.156  2007/08/15 18:06:22  potekhin
* Seec omment at
* http://drupal.star.bnl.gov/STAR/comp/simu/geometry0/changes-beampipe-support
* about the newest round of the SVT corrections (Carbon used to construct water
* channels as opposed to Beryllium). The difference of 0.4 to 0.6% of rad length
* is big enough to warrant the creation of a new tag, in this case Y2007A.
* The SVT code version activated in this tag is 10.
*
* Revision 1.155  2007/07/12 20:16:47  potekhin
* Added the following geometry tags:
* a) Y2008   -- first cut, will be improved
* b) UPGR14  -- UPGR13 sans IST
* c) DEV2007 -- sandbox for what-if studies, non-production grade
*
* Revision 1.154  2007/04/13 17:54:58  potekhin
* Based on a comment by Akio, remove the PHMD (photon
* multiplicity detector) from the Y2006 configuration, to
* reflect the actual setup for that year. The new tag is Y2006C
*
* Revision 1.153  2007/03/21 21:08:05  potekhin
* A cleaner version of managing the HFT (pixlgeo) versions
*
* Revision 1.152  2007/03/15 19:56:16  potekhin
* Provide versioning for the thicker active Si layer
* in pixlgeo3, via setting the structure elements
*
* Revision 1.151  2007/03/09 21:40:48  potekhin
* UPGR13 modifications: (a) FSTD is out (b) modified SSD with carbon parts,
* for R&D purposes (c) modified IST with single sided inner layer for the
* April proposal
*
* Revision 1.150  2007/02/23 21:45:40  potekhin
* a) re-instated the calls to IGT codes to keep a degree
* of backward compatibility (for early UPGRXX tags)
* b) deleted the previosuly commented out IST1 tag (as
* advertised earlier, it was officially retired)
* c) put in the dead material version of SSD into UPGR13 (SSD5)
*
* Revision 1.149  2007/02/23 21:20:02  potekhin
* In the R and D tag UPGR13, removed the prototype support
* cone on the East side on Gerrit  request. Corrected the
* steering logic for the new FGT (former IGT) to integrate
* the newer code obtained form Gerrit.
*
* Revision 1.148  2007/02/22 22:37:39  potekhin
* Correcting a simple typo (accidentally hit delete)
*
* Revision 1.147  2007/02/22 22:21:18  potekhin
* As stated in the S&C meeting, the recent correction
* for the dead material in the SSD should be propagated
* into earlier model-years, therefore creating new tags.
* Only two such tags were necessary and have been created:
* Y2005F and Y2006B. In addition to the SSD configuration,
* they also feature a newer version of the CALB code,
* which they share with Y2007.
*
* Revision 1.146  2007/02/16 22:57:50  potekhin
* As per Xins communications, the correct logic for year 2007
* in the upVPD code is triggered when the config flag is set to 7.
* I make this tweak in steering for Y2007.
*
* Revision 1.145  2007/02/13 20:42:29  potekhin
* Along the lines previously discussed, replace the IGT
* by the FGT in the UPGRXX tags; in this case, by creating
* a new tag UPGR13, which is the continuation of the UPGR07
* line and will be further tuned.
*
* Revision 1.144  2007/02/09 22:04:37  potekhin
* a) added steering for new code and settings for TOF, upVPD and FPD/FMS
* b) retired IST1 (commented out, to be deleted later)
*
* Revision 1.143  2007/02/02 18:20:46  potekhin
* The updated FMS code (fpdgeo) needs more space at the
* end of the cave, so we need to add some. We will reflect
* the more precise dimensions in cavegeo.g - here we just
* add the requisite configuration flag for Y2007
*
* Revision 1.142  2007/02/02 17:18:40  potekhin
* Added logic to include the updated SSD code
*
* Revision 1.141  2006/12/21 23:06:17  potekhin
* Previous versions of UPGRxx geometries were deficient
* is that there was a clash between the IGT disks and
* other elements of the central tracker system. This lead
* to a loss of hits in the IGT, a problem which was mitigated
* in private versions of the code. To avoid prolifirations
* of the latter, we need to introduce a corrected tag,
* which is UPGR12, based on UPGR05 but with different
* disk radii.
*
* Revision 1.140  2006/12/18 23:28:33  potekhin
* Introduced geometry tags UPGR10 and UPGR11 (as discussed
* in appropriate fora) which utilize source files istbgeo4 and 5.
* Made some cosmetic changes to the code layout.
*
* Revision 1.139  2006/12/14 21:36:25  potekhin
* Add tag UPGR09 with sttering logic
* for the ISTB with only the outer layer...
*
* Revision 1.138  2006/12/12 22:32:19  potekhin
* a) enable a cleaner barrel EMC code in Y2007
* b) re-instate UPGR06 for the upcoming simulation
*
* Revision 1.137  2006/12/01 19:26:58  potekhin
* Removing the SSD from the R&D geometry UPGR05, resulting
* in a new configuration, UPGR08
*
* Revision 1.136  2006/11/28 00:02:09  potekhin
* Added Y2007 and set it up to include the new FMS (FPD)
*
* Revision 1.135  2006/11/22 17:41:49  potekhin
* Added a tag which will be used exclusively for the material
* balance effect study, i.e. it won`t contains any realistic
* detectors in the center of STAR, and feature a variable
* thickness cylinder instead.
*
* Revision 1.134  2006/11/18 02:56:17  potekhin
* Rewrote UPGR01 to better conform with
* other UPGR0X tags in terms of actual code
* (same version of SSD, identical flags in most
* places). UPGR01 is HFT+SSD in central tracking,
* and nothing else.
*
* Revision 1.133  2006/11/14 00:21:03  potekhin
* Improved steering for the IGTD (gem disks), in order to
* provide the possibility of a proper versioning. This is
* done now via the IgtdConfig variable
*
* Revision 1.132  2006/11/01 00:21:09  potekhin
* As discussed in appropriate fora, we need to introduce a HPD-less
* tag for our TUP study. Let there be UPGR07.
*
* Revision 1.131  2006/10/21 18:14:21  potekhin
* a) Added steering for the TUP support structure
* b) optionally change the radius of the FSTD (to better fit with
* the rest of TUP
* c) using a more precise version of SSD code in UPGR05
*
* Revision 1.129  2006/10/09 16:19:17  potekhin
* Due to the ongoing SSD studies, we need to refine the 2005 tag,
* so as to include Lilian code that were checked into CVS in
* early 2006 but were valid in 2005 as well. We have therefore created
* the tag Y2005E, which is an improvement over Y2005D (more precise SSD),
* bigger SVT shield (to accomodate the SSD) and a full barrel calorimeter.
*
* Revision 1.128  2006/10/02 21:37:03  potekhin
* Added steering logic for the new tag UPGR05, which
* includes the HFT (former pixel), HPD, IST and SSD,
* but no SVT. GEM detectors are also excluded.
*
* Revision 1.127  2006/09/15 19:56:31  potekhin
* Due to ongoing development, we need to create a new tag,
* UPGR04, and steering logic for the new detector HPDT
*
* Revision 1.126  2006/07/07 17:41:28  potekhin
* Fixing a very old and inconsequential typo in the
* assignment of a variable for the "minimum Si layer"
* (which I doubt was previously used)
*
* Revision 1.125  2006/06/12 18:34:28  potekhin
* Created the tag Y2006A, which will allow for the all-new
* FPD to be properly included, as well as otherpotential changes
* to be implemented in mid-year.
*
* Revision 1.124  2006/06/08 19:36:36  potekhin
* By an unfortunate slip of the wrist, I deleted year2000
* and year2001 during the previous check-in. Now they are restored.
*
* Revision 1.123  2006/06/02 17:34:37  potekhin
* a) removed the PIX1 tag that was reliably
* confirmed as obsolete
* b) added the SISD_OFF flag that facilitates
* creation of test geometries in which both the SVT
* and the SSD are taken out. Needed for R&D.
*
* Revision 1.122  2006/05/05 17:38:41  potekhinconfig
* Just rename the IST2 to UPGR03 to stivk with
* previously chosen naming convention.
*
* Revision 1.121  2006/05/05 17:24:58  potekhin
* Need a new R&D tag, IST2, to properly manage
* the configuration of an alternative tracking upgrade project.
* Other changes -- in DEV2005 -- are due to the SVT study, and
* since this is not a production tag, are immaterial.
*
* Revision 1.120  2006/03/21 23:51:41  potekhin
* Fairly significant additions:
* (a) add steering for the muon trigger system, "MUTD"
* (b) specify a complete barrel calorimeter for Y2006
* (c) add steering for the corrected SSD (sisdgeo3)
* (d) add steering for a small modifications in the SVT shield in Y2006
*
* Revision 1.119  2006/01/18 23:06:13  potekhin
* Introducing the baseline year 2006 geometry, which is "the best"
* Y2005 geo plus the bugfix in the TPC backplane (not too important).
* Pending a better definition of the SSD from Lilian which we`ll have
* to version and maybe cut new tags of 2004-5 geometries if there is
* a need.
*
* As agreed, created UPGR01 and UPGR02 (note cautious use of namespace),
* which are basically SSD+HFT and IST+HFT. Note that NONE of engineering
* detail is available in either case, and less so for the integration
* structural elements. We expect to do a lot of development in this area
* as new facts and engineering data are provided. This cut will allow us
* to proceed with tracking studies in the near term.
*
* Fixed a comment and added a couple more.
*
* Revision 1.118  2005/10/20 20:17:47  potekhin
* a) added a few parameters to the "low_em" setting to
* better simulate soft EM processes, if needed
* b) added logic for calling the latest svttgeo6
* c) added Y2003C, Y2004D and Y2005D to take advantage of this
* new SVT geometry file. Added same to DEV2005 for development
* purposes
*
* Revision 1.117  2005/10/06 17:54:57  potekhin
* a) in DEV2005, provide a way to use a highly customised version of the SVT
* b) create a key that enables the user to lower the electromagnetic processes
* GEANT cut to 10 keV, from the KUMAC script without the need to recompile.
*
* Revision 1.116  2005/09/26 21:44:18  potekhin
* We need a convenient way to optionally remove the SVT from
* the simulation, to facilitate conversion and brems studies.
* To this end, add  "SVTT_OFF" to the list of options
*
* Revision 1.115  2005/09/02 18:20:35  potekhin
* Added separate config variables for the Quad section
* (which includes D0) -- way upstream area
*
* Revision 1.114  2005/08/16 00:56:13  potekhin
* Modified the development tag DEV2005 to configure the
* geometry for the shielding studies. Added steering for
* the shield and renamed a variable to avoid naming clash.
* Also, removed ZCAL from this tag, because it would take
* time to reconsile its geometry with the shield, and it`s
* not too important for teh shielding study.
*
* Revision 1.113  2005/07/14 22:13:32  potekhin
* In the tag PIX1:
* Need to actuate a thinner pipe as well as an updated
* pixel detector geometry, to include an exoskeleton
* for the beampipe (R&D request from Kai et al.)
*
* Revision 1.112  2005/06/28 16:18:22  potekhin
* Add config variables and steering for the GEM
* barrel tracker -- only inlfated in the tag IST1.
*
* Revision 1.111  2005/06/03 15:54:43  potekhin
* As agreed with all parties, we would like to run the 2004 simulation
* with  improved geometry. In particular, the pp physics groups are calling
* for an updated version of Y2003X (full calorimeter). We have no other
* option but to create a new tag, Y2004Y, to reflect that.
*
* Revision 1.110  2005/05/26 16:03:31  potekhin
* a) As advertised before, removed the various Year 1 tags,
* as they were taking space and weren't used anymore
* b) Included the updated TPC backplane into the tag Y2004C,
* along with FTRO. This makes sense as (1) it wasn't used
* in production yet (2) similar updates were done in the
* latest 2005 tag.
* c) Improved formatting and comments in select places
*
* Revision 1.109  2005/04/15 15:28:23  potekhin
* a) Corrected a comment that could surreptitiously break the Mortan
* parsing and cause a bug, of the type : !----- your text here ----
* b) Improved the formatting of comments and fixed a typo
* c) As agreed with Jerome, added a special development tag DEV2005,
* which will allows as to better insure consistency and backward
* compatibility of the code when working on the improvements in the
* current tag. Production with such a development tag will be
* prohibited and effectively disabled, as implied in its designation.
*
* Revision 1.108  2005/04/11 17:47:09  potekhin
* Add the tag Y2005C, as authorized by Jerome, in order to activate
* the latest TOF upgrades from Xin
*
* Revision 1.107  2005/04/07 19:52:30  potekhin
* As per Janet`s note, update the FTPC config
* in Y2004C (Ar+CO2)
*
* Revision 1.106  2005/04/04 22:13:37  potekhin
* Creating a new Y2004C per the request from Jamie and A.Wetzler.
* It contains a better version of the SSD (unavailable back in 2004),
* the correction for the hybrid chip assembly length in the SVT
* and extra copper in the cones, i.e. the components that make
* difference in conversion studies
*
* Revision 1.105  2005/03/25 17:28:24  potekhin
* Added the corrected SSD ladder positions (as per Lilian
* communication) to the tag y2005b
*
* Revision 1.104  2005/03/25 02:13:59  potekhin
* A very significant set of code changes, related to
* versioning: The CorrNum variable turned out to be
* unwieldy, dur to combinatorially large number of various
* individual detector correctionds. Accordingly, it has
* been retired.
*
* I have extended the set of "Config" variables
* to describe the parameters previously wrapped into
* CorrNum. This appears to work nicely.
*
* The tag y2005b will contain the latest improvements,
* done in this new scheme of versioning:
* a) SVT elelctronics mother volume length bug fix
* b) Ar+C02 mix in the FTPC
* c) SSD ladder radius corection (to be double-checked with Lilian)
*
* Better diagnostic printout and code simplification.
*
* Revision 1.103  2005/03/08 01:05:35  potekhin
* Created a new tag, y2005b, which is necessary to optionally
* activate the updated version of the TPC geometry. Introduced
* simple logic to handle same, similar to the versioning of
* other detectors (TpceConfig)
*
* Revision 1.102  2005/02/19 05:26:29  potekhin
* a) Corrected the comment for tag IST1
* b) Added the FGTD activation in same
*
* Revision 1.101  2005/02/02 00:16:09  potekhin
* We now have a new estimate of the copper cable mass, for
* the cables feeding the SVT and residing on the support cones.
* Included in the code now are the switches and logic to allow
* the updated configuration to be created.
*
* Revision 1.100  2005/01/19 16:40:53  potekhin
* We extended the y2005x tag from y2004x and made
* a mistake of creating only a half barrel of the SSD,
* which in fact has now been completed. I fixed that
* in y2205x and y2005 now.
*
* Revision 1.99  2005/01/03 22:11:23  potekhin
* Need to update the experimental IST1 tag to better
* refelct the needs of the new tracking group. Took
* out the FTPC, put in the Pixel and SSD, and made
* provisions for the latter to work w/o the SVT
* volumes.
*
* Revision 1.98  2004/12/07 00:46:04  potekhin
* We need to steer the newly added FSTD (forward tracker).
* For now I add it to the experimental tag IST1, which is
* used for development only.
*
* Revision 1.97  2004/11/02 19:00:55  potekhin
* Added the Y2005 tag which we need for testing and
* for general consistency. It will be subject to possible
* changes, and for the time being is same as the latest
* cut of the year 2004 (Y2004B).
*
* Revision 1.96  2004/10/28 22:05:53  potekhin
* Changed the coding convention for the SSD geometry
* "levels", which specify which file to load.
*
* Revision 1.95  2004/10/26 21:46:23  potekhin
* 1) Cleaned out the remaining test code from Y2004B
* 2) Created Y2005X which is same as Y2004B except for
* the full barrel calorimeter as per Thomas` request
*
* Revision 1.94  2004/10/26 21:11:00  potekhin
* 1) Moved filling of GDAT to the end of code after a consultation
* with Pavel -- this is less error prone as by this time the main
* Zebra bank is properly populated
* 2) Chaged the Y2004B to activate the sisdgeo1
*
* Revision 1.93  2004/09/11 01:16:32  potekhin
* Two additions:
* (a) Geo tag Y2004B will include the most recent enhancements
* such as the FTPC readout cage. In this cut, this is a modified
* geo and work in progress
* (b) Geo tag PIX1, based on the request from the pixel group.
* The inner layer of SVT has been removed such that it can coexist with
* the innder pixel based tracker
*
* Revision 1.92  2004/07/15 16:30:08  potekhin
* Since the "MITT" detector was updated and became ISTB,
* this needs to be reflected in the main geometry steering.
* The new tag replaces MITT1 and is called IST1
*
* Revision 1.91  2004/06/28 22:53:45  potekhin
* The emergence of two new detectors, the Pixel and
* the other dubbed MITT, necessitates the creation
* of dedicated geometries that can be used for R&D
* for both. The previous COMPLETE geometry was not
* well suited for this at all, and didn`t allow
* proper versioning. Hence, the new MITT1 tag has
* been created, on same footing as COMPLETE but
* with a different structure.
*
* Revision 1.90  2004/05/10 21:49:38  potekhin
* For consistency, the SSD config in the y2004a tag should
* be equal 2 (even if the SSD data won`t be used)
*
* Revision 1.89  2004/04/28 23:30:37  potekhin
* Deleted an unnecessary line setting the
* PHMD version. It will be done throught the
* structure PMVR anyway.
*
* Revision 1.88  2004/04/28 00:35:40  potekhin
* Extra detail in steering the PHMD geo, see the PHMD code
*
* Revision 1.87  2004/04/14 20:54:27  potekhin
* Changed the Y2004A to Y2004X to emphasize the fact
* that this is a variation of asymptotic geometry
* and not a corecction of the actual one. This
* complies with the naming scheme we used in 2003.
*
* Revision 1.86  2004/04/14 19:02:12  potekhin
* Introducing the geometry Y2004A, which is same as
* Y2004 but with full Barrel Calorimeter, a-la Y2003X,
* as per requests of PWG`s. Subject to final approval.
*
* Revision 1.85  2004/03/31 16:37:51  potekhin
* Added version control for the FPD,
* via the variable FpdmConfig
*
* Revision 1.84  2004/03/24 23:33:48  potekhin
* Added proper VPD versioning as discussed with the team.
* No numerical data here, just config flag.
*
* Revision 1.83  2004/03/10 20:11:34  potekhin
* In Y2004, set the TOF config to 7 as requested by B.Llope,
* to reflect the current configuration.
*
* Revision 1.82  2004/03/04 02:38:38  potekhin
* Added modifications COMPLETE, to exclude SISD
* as per Kai request -- won`t affect anybody else
*
* Revision 1.81  2004/02/10 00:27:57  potekhin
* The SVT group wanted the correction in the SVT geometry,
* which we discovered that we needed earlier this year, to
* be applied RETROACTIVELY to year2001 geometry tag.
* This breaks compatibility of the simulated SVT data
* between earlier simulation runs and the ones to
* follow, however this has been signed off by Helen
* and Jerome as an acceptable compromise. The CorrNum
* for year2001 is now set to 1.
*
* Revision 1.80  2004/01/29 20:46:45  potekhin
* Disabled the long since obsoleted version of TOF,
* because this piece of code would need to be rewritten
* to be compiled with a variety of compiler options.
*
* Revision 1.79  2004/01/22 00:21:32  potekhin
* Provide a facility to position the SVT with the MANY option,
* which we`ll likely need due to overlap of the PIXL (embedded
* in SVT) and the mother volume of the beampipe
*
* Revision 1.78  2004/01/19 22:53:27  potekhin
* A small additional piece of logic to steer the
* construction of the barrel calorimeter, better
* code layout and comments
*
* Revision 1.77  2003/12/17 22:15:18  potekhin
* a) In accordance with recent modifications, we also
* introduce the configuration variable for the beam
* pipe, instead of keeping its actual parameters here
* b) corrected the COMPLETE geometry with the newest
* version of the svtt code (with ssd separated out)
* c) removed obsolete variables and logic, streamlined
* the code
* d) better comments and formatting in a few places
*
* Revision 1.76  2003/12/03 19:53:04  potekhin
* a) Corrected a small but annoying bug in
* propagating the geo tag to reco:
* one of the characters was copied twice
* because of the indexing error
*
* b) Added the "shift" angle for the second half
* barrel in Y2004
*
* Revision 1.75  2003/11/20 02:58:10  potekhin
* Changed the correction number scheme, such that it
* allows for a new layout of the SVT to be implemented --
* the one without the nested SSD. This is achieved by
* calling the new code, svttgeo3.
*
* Changed the SSD config number in y2004 to "2": 10 ladders,
* the "1" being one ladder installed previosuly and "3"
* being the complete 20 ladder config.
*
* Made smallchanges in the barrel config flag for y2004,
* improved comments and cleaned out unused variables.
*
* Revision 1.74  2003/11/14 22:56:19  potekhin
* We are about to redo a sim run with y2003x,
* and it seems that me might put in some of the
* prior corrections as well. Therefore, I`m changing
* the correctin level to 2.
*
* Revision 1.73  2003/11/13 00:54:50  potekhin
* Create a facility to modify the TPC
* gas density programmatically
*
* Revision 1.72  2003/11/13 00:21:42  potekhin
* The modification flag we introduced earlier
* to reflect variations in the dimensions of
* certain support structured of SVT takes a borader
* meaning than just the shield, hence we rename
* the variable to SupportVer
*
* Revision 1.71  2003/11/12 18:45:09  potekhin
* As per Fabrice, change the number of layer in the
* SVT to 7 to ensure that the ssd in its current
* version is also included
*
* Revision 1.70  2003/10/30 00:15:42  potekhin
* To perfect our already sophisticated taxonomy of
* the geometry tags, we rename Y2003C into Y2004,
* because the run for which it is meant will start
* in early 2004 anyway. Anyone to confuse 2003
* and 2004, from now on, will be jailed and
* possibly deported.
*
* Revision 1.69  2003/10/29 22:07:30  potekhin
* Two changes:
* 1) As agreed, I swap the tags y2003(b,c) to arrange
* them chronologically for better mneumonics
* 2) Introduced variable for steering of the Silicon Strip
* Detector code (which needs to be written)
*
* Revision 1.68  2003/10/28 00:01:59  potekhin
* As agreed with Jerome, we shall prevent
* proliferation of custom geometries to reduce
* the various dependencies between simu and reco
* and databases. Therefore, the experimental
* geometry "ASYMPT1" has been removed and has
* taken place of the "COMPLETE", which is our
* official sandbox. The Pixel Detector is
* defined in it right now.
*
* Revision 1.67  2003/10/15 23:19:35  potekhin
* Due to an apparent need to have the "most precise"
* geometry describing the STAR configuration in the
* spring 03 run, we introduce the tag Y2003C. It
* includes all the corrections first introduced in
* Y2003A, but also has the extra material in the
* SVT that we recently added in the GEANT model
* and which had been missing before.
*
* Revision 1.66  2003/10/10 23:59:18  potekhin
* Per Jerome`s suggestion, which I agree with, we should
* semantically separate our current geometry tags from the
* future development ones. Also, it makes sense to have a more
* complete (as compared to 2003) geometry, for the pixel
* studies. Therefore, the pixel testbed tag has been
* renamed and the STAR detector more fully populated.
* It`s now ASYMPT1, and the indexing may continue.
*
* Revision 1.65  2003/10/10 23:15:35  potekhin
* In previous check-n: forgot to mention the new CorrNum=3
* which actually programmatically modifies the inner radius of
* the shield in the SVT -- otherwise the pixel detector won`t
* fit. Plus, put in extra flags PIXL_ON and PIPE_OFF to facilitate
* experimentation.
*
* Revision 1.64  2003/10/10 23:12:56  potekhin
* The fact is, we will need a suitable specialized geometry
* for the pixel detector development, as it will require
* a different beampipe and other modifications. I hereby
* create the tag y2003c which will serve this purpose.
* Right now it disables the old beampipe w/o offering anything
* in its place -- this is subject to change as we assimilate
* the new pipe design.
*
* Improved comments and structure in a few places
*
* Revision 1.63  2003/10/01 23:44:17  potekhin
* Code modifications related to persisting the vital
* geometry/version data, for now the magnetic field
* scale and the geometry tag
*
* 1) Change the semantics of the variable Geom, which was hacky
* anyway, and put the mwx=1 in the individual year_1 branches
* themselves (a lot cleaner than a prior obscure "if")
*
* 2) Store the complete geometry tag in the variable Geom,
* which is 8 characters long
*
* 3) Change the subroutine "geometry" into a "module",
* which process instruments it (via Mortran) to access
* and manipulate ZEBRA banks as structures
*
* 4) Introduce the bank GDAT, as a sub-bank of GEOM,
* which for now contains the field scale and the tag.
*
* Revision 1.62  2003/09/29 19:48:41  potekhin
* 1) Fixed typos in comments
*
* 2) Created a few options that allow the user to selectively include
* certain detectors such as svtt, ECAL, CALB into the minimal geometry,
* thus facilitating the creation of custom geometries on the fly --
* useful for debugging and detector exploration
*
* 3) Improved the PHMD logic
*
* 4) last but not least -- the shift variable for CALB was changed from
* {75,75} (incorrect) to {75,105} in geometry y2003x (full).
*
* Revision 1.61  2003/09/18 22:09:34  potekhin
* Corrected a small comment typo and added the full
* endcap wheel to the  new flagship geometry, y2003b.
* This is done with fill mode 3.
*
* Revision 1.60  2003/09/17 23:10:42  potekhin
* Small improvements to the Correction Level
* logic.
*
* Revision 1.59  2003/08/21 20:29:27  potekhin
* As per discussion with Jerome, I`m introducing
* a cleaner versioning of the 2003 geometries:
*
* y2003a is now really the corrected year2003,
* without any additions -- the SVT layer positions
* have been updated and the old supogeo bug fixed.
*
* y2003b has same corrections as y2003a but will also
* include extra material in the SVT, new ECAL configuration
* as well as a new FPD, plus the Photon Multiplicity Detector.
* Other changes will be done as needed. So for practical
* purposes, this will be the actual Fall`03 geometry.
*
* Revision 1.58  2003/08/05 23:37:09  potekhin
* Continued to use the CorrNum "correction level" to
* correct older geometry bugs in a controlled and versioned
* manner. Keep the newer version of the FTPC support pieces,
* and add a call the new SVTT module, which will
* include a number of corrections.
*
* Revision 1.57  2003/07/03 04:45:20  potekhin
* Added the "special" variation of the year 2003 geometry, which has
* a complete set of the barrel and endcap calorimeter elements. This
* is needed primarily for heavy flavor studies and other rare signals.
* The tag is y2003x.
*
* Also, fixed a subtle bug in the configuration of the endcap calorimeter.
* It did not affect previous simulations, but would manifest itself
* in a complete configurations such as this one.
*
* Revision 1.56  2003/05/01 23:00:16  potekhin
* Photon Multiplicity Detector is now part of the
* GEANT geometry version "y2003a", with proper
* versioning of its position
*
* Revision 1.55  2003/04/29 21:04:55  potekhin
* To keep the consistency of current simulation runs,
* the geometry "year2003" is frozen. All the corrections
* will go into "y2003a", and there will be a number of those.
* Such geometry has been added to this present source file.
* In the current cut of y2003a, to be tested, we corrected
* the supogeo and the offset of the ECAL phi position.
* We are also awaiting further corrections from the SVT group.
*
* Revision 1.55  2003/04/29 16:57:00  potekhin
* New geometry y2003a -- corrected
*
* Revision 1.54  2002/12/10 01:48:25  potekhin
* Important: the hadronic interactions are now indeed actuated in GCALOR
*
* Revision 1.53  2002/12/05 23:28:41  potekhin
* Streamlined the BTOF config logic
*
* Revision 1.52  2002/11/27 21:53:14  potekhin
* code improvement for readability etc -- moved bbcmgeo call
*
* Revision 1.51  2002/11/03 02:16:10  nevski
* geometry up to 2003 introduced
*
* Revision 1.50  2002/10/28 15:49:35  nevski
* fpd as a separate detector added
*
* Revision 1.49  2002/10/28 15:42:29  nevski
* introducing 2002 version
*
* Revision 1.48  2001/09/10 17:39:34  nevski
* do not set MFLD datacards without a DETP GEOM
*
* Revision 1.47  2001/05/22 17:40:47  nevski
* field find tuning
*
* Revision 1.46  2001/05/21 21:07:05  nevski
* Steves field map added
*
* Revision 1.45  2001/04/09 15:31:35  nevski
* second version of cerenkov light properties introduced
*
* Revision 1.44  2001/03/16 22:09:13  nevski
* some clean-up
*
* Revision 1.43  2001/03/16 00:32:06  nevski
* switch on/off cooling water
*
* Revision 1.42  2001/03/15 01:24:47  nevski
* default BTOF forced to no TOF tray
*
* Revision 1.41  2001/03/15 01:14:20  nevski
* first approach to forward pion detector
*
* Revision 1.40  2001/03/13 20:56:31  nevski
* variable RICH position taken from DB
*
* Revision 1.39  2001/03/12 01:01:30  nevski
* mwc x-hits corrected
*
* Revision 1.38  2001/02/13 02:28:52  nevski
* Y2B: extend CALB patch, add VPD
*
* Revision 1.37  2001/02/07 02:09:09  nevski
* 6 silicon layers in y_2b geometry
*
* Revision 1.36  2000/11/22 17:51:41  nevski
* tof geometry versions 1/2 preserved in btofgeo1, version 3 goes in btofgeo2
***************************************************************************

"""                                                                               """
""" Define a CONSTRUCT command to check for the presence of modules using CsADDR. """
""" If they are not found, they are not loaded and an error message will be made  """
""" at the end.  This breaks the dependency on the geometry modules, and allows   """
""" us to run geometry.g standalone to, e.g., print a list of the dependencies.   """
"""                                                                               """
REPLACE [CONSTRUCT #;] with [
   address = CsADDR('#1'); 
   IF address>0 { Write (*,*) '---> Construct: ',  '#1';          nloaded+=1;  
                  CALL CsJCAL (address,0, 0,0,0,0,0, 0,0,0,0,0);               }
   ELSE {         Write (*,*) '---> Unresolved: ', '#1';          nfailed+=1;  }
];
REPLACE [CONSTRUCT # !#;] with [!#2
   address = CsADDR('#1'); 
   IF address>0 { Write (*,*) '---> Construct: ',  '#1';          nloaded+=1;  
                  CALL CsJCAL (address,0, 0,0,0,0,0, 0,0,0,0,0);               }
   ELSE {         Write (*,*) '---> Unresolved: ', '#1';          nfailed+=1;  }
];
REPLACE [CONSTRUCT #(#);] with [
   address = CsADDR('#1');
   IF address>0 { Write (*,*) '---> Construct: ',  '#1';              nloaded+=1; 
                  CALL CsJCAL (address,1, #2,0,0,0,0, 0,0,0,0,0);               }
   ELSE {         Write (*,*) '---> Unresolved: ', '#1';              nfailed+=1; }
];



********* Detector definitions*********************************************

replace [exe BBCMon;] with [;BBCM=on;]

replace [exe MAGPv1;] with [;MagpConfig = 2; """Improved magnet model"""; ]

replace [exe CALBof;] with [;CALB=off;]
replace [exe CALB00;] with [;"Full barrel in 2007"; CALB=on;
                     CalbConfig = 0;
                     emsEdit=on ; nmod={12,0}; shift={87,0};]
replace [exe CALBa0;] with [;"Full barrel in 2007"; CALB=on;
                     CalbConfig = 0;
                     emsEdit=on ; nmod={24,0}; shift={21,0};]
replace [exe CALBb0;] with [;"Full barrel in 2007"; CALB=on;
                     CalbConfig = 0;
                     emsEdit=on ; nmod={60,0}; shift={0,0};]
replace [exe CALBc0;] with [; CALB=on;
                     CalbConfig = 0;
                     emsEdit=on ; nmod={60,60}; shift={75,105};]
replace [exe CALBd0;] with [" 60 sectors "; CALB=on;
                     CalbConfig = 0;
                     emsEdit=on ; nmod={60,0}; shift={75,0};]
replace [exe CALBe0;] with [" 60 sectors "; CALB=on;
                     CalbConfig = 0;
                     emsEdit=on ; nmod={60,60}; shift={75,105}; " 60 sectors on both sides";]

replace [exe CALB01;] with [;CALB=on;
                     CalbConfig = 1;
                     emsEdit=on ; nmod={60,60}; shift={75,105}; " 60 sectors on both sides";]
replace [exe CALB02;] with [;CALB=on;
                     CalbConfig = 2;
                     emsEdit=on ; nmod={60,60}; shift={75,105}; " 60 sectors on both sides";]

replace [exe CAVE03;] with [ "We need an even bigger Cave";   CaveConfig = 3;]
replace [exe CAVE04;] with [ "We need an even bigger Cave";   CaveConfig = 4;]
replace [exe CAVE05;] with [ "Extended Cave and tunnel"; CaveConfig=5;]

*                                                                                   Endcap Calorimeter 
replace [exe ECALof;] with [;ECAL=off;]
replace [exe ECAL31;] with [;"ECAL31"; ECAL=on;
                             ecalFill=3; "all sectors filled " 
                             EcalConfig=1; " one ECAL patch, west ";
                             EcalGeometry=5; "old version of the geometry file";
                            ]

replace [exe ECAL11;] with [;"ECAL11"; ECAL=on;
                             ecalFill=1; 
                             EcalConfig=1;  
                             EcalGeometry=5; "old version of the geometry file";
                            ]
replace [exe ECAL31;] with [;"ECAL31"; 
                             ECAL=on;
                             ecalFill=3; 
                             EcalConfig=1;   " one ECAL patch, west ";
                             EcalGeometry=5; "old version of the geometry file";
                            ]
replace [exe ECAL33;] with [;"ECAL33"; ECAL=on;
                             ecalFill=3 "all sectors filled "; EcalConfig=3; "both wheels"  
                             EcalGeometry=5; "old version of the geometry file";
                            ;]

replace [exe ECALv6;] with[;"ECAL version 6.1 (or higher)"
                           ;ECAL=on;
                           ;EcalFill=3;     "all sectors filled";
                           ;EcalConfig=1;   "EEMC on west poletip only";
                           ;EcalGeometry=6; "Version 6.1 and higher";
                           ]

replace [exe EMCUTS(#,#);] with [ "Set eemc / bemc cuts to #1";
                                #1CutConfig=#2;
                              ]


*                                                                                      FPD Calorimeter 

replace [exe FPDMof;] with [; "FMS/FPD off"; FPDM=off; FpdmConfig=0; ]
replace [exe FPDM00;] with [; "forward pion detector "; FPDM=on; FpdmConfig  = 0;]
replace [exe FPDM01;] with [; "forward pion detector "; FPDM=on; FpdmConfig  = 1;]
replace [exe FPDM02;] with [; "forward pion detector "; FPDM=on; FpdmConfig  = 2;]
replace [exe FPDM03;] with [; "forward pion detector "; FPDM=on; FpdmConfig  = 3;]
replace [exe FPDM04;] with [; "forward pion detector "; FPDM=on; FpdmConfig  = 4;]

replace [exe HCALof;] with [; "HCAL off"; HCAL=off; HcalConfig=0; ]
replace [exe HCALv0;] with [; "HCAL on "; HCAL=on;  HcalConfig=0; ]
replace [exe HCALv1;] with [; "HCAL on "; HCAL=on;  HcalConfig=1; ]
replace [exe HCALvF;] with [; "HCAL on "; HCAL=on;  HcalConfig=15;]

replace [exe FTSDof;] with [; "FTSD off"; FTSD=off; FtsdConfig=0; ]
replace [exe FTSDv0;] with [; "FTSD on" ; FTSD=on ; FtsdConfig=0; ]

*                                                                                          Forward TPC 

replace [exe FTPCof;] with ["ftpc configuration"; FTPC=off;]
replace [exe FTPC00;] with ["ftpc configuration"; FTPC=on;
                            ;FtpcConfig = 0;"FTPC Support";SupoConfig = 1;]
replace [exe FTPC01;] with ["ftpc configuration"; FTPC=on;
                            ;FtpcConfig = 1;"FTPC Support";SupoConfig = 1;]

replace [exe FTRO01;] with ["FTPC Readout barrel "; FTRO = on; FtroConfig = 1;]

replace [exe MFLDof;] with [ MFLD=off;]
replace [exe MFLD23;] with [ MFLD=on; magField = 2.5; MfldConfig=3;]
replace [exe MFLD53;] with [ MFLD=on; magField = 5.0; MfldConfig=3;]
replace [exe MFLD54;] with [ MFLD=on; magField = 5.0; MfldConfig=4;]

*                                                                                       Muon Telescope 

Replace [exe MUTD01;] with [ "Muon Trigger System";                   MUTD=on;   MutdConfig= 1;]
Replace [exe MUTD03;] with [ "Muon Trigger System";                   MUTD=on;   MutdConfig= 3;]
Replace [exe MUTD04;] with [ "MTD Run 11 - single backleg, 3 trays";  MUTD=on;   MutdConfig= 4;]
Replace [exe MUTD05;] with [ "MTD Run 28 backlegs, 118 trays";        MUTD=on;   MutdConfig= 5;]
Replace [exe MUTD12;] with [ "MTD Run 12 - 3  backlegs, 13 trays";    MUTD=on;   MutdConfig=12;]
Replace [exe MUTD13;] with [ "MTD Run 13 - 15 backlegs, 75 trays";    MUTD=on;   MutdConfig=13;]
Replace [exe MUTD14;] with [ "MTD Run 13 - 15 backlegs, 75 trays";    MUTD=on;   MutdConfig=14;]



*                                                                         Photon Multiplicity Detector   

replace [exe PHMDof;] with ["Photon Multiplicity Detector Version ";PHMD=off; PhmdConfig = 0;]
replace [exe PHMD01;] with ["Photon Multiplicity Detector Version ";PHMD=on;  PhmdConfig = 1;]
replace [exe PHMD02;] with ["Photon Multiplicity Detector Version ";PHMD=on;  PhmdConfig = 2;]

*                                                                                            Beam Pipe   

replace [exe PIPE00;] with [ "Simplest.Gerrit"; PipeConfig = -1;PipeFlag   = -1;]
replace [exe PIPE12;] with [ "Default pipe"; PipeConfig = 2 ; PipeFlag   = 1;]
replace [exe PIPE04;] with [ "The new pipe according to Kai"; PipeConfig = 4;
                             "pipe wrap only" ;               PipeFlag   = 0;]
replace [exe PIPE14;] with [ "The new pipe according to Kai"; PipeConfig = 4;
                             "pipe wrap only" ;               PipeFlag   = 1;]

replace [exe PIPEv1;] with [ "The beam pipe for the HFT";     PipeConfig = 10;]
replace [exe PIPEv2;] with [ "The beam pipe for the HFT";     PipeConfig = 20;]
replace [exe PIPEv3;] with [ "The beam pipe for the HFT";     PipeConfig = 30;]


*                                                                                       Pixel Detector

replace [exe PIXL00;] with [ "Simplest.Gerrit" PIXL=on; PixlConfig=-1;]
replace [exe PIXL01;] with [ "Put the pixel detector in" PIXL=on; PixlConfig=1;]
replace [exe PIXL02;] with [ "Add the pixle detector to the IDSM"; PIXL=on; PixlConfig=6; ]
replace [exe PIXL05;] with [ "Add pixel detector to the IDSM"; PIXL=on;     PixlConfig=50; ]
replace [exe PIXL06;] with [ "Add pixel detector to the IDSM"; PIXL=on;     PixlConfig=60; ]
replace [exe PIXL62;] with [ "Add pixel detector to the IDSM"; PIXL=on;     PixlConfig=62; ]

replace [exe DTUB01;] with [ "Add DTUB (no op)"; ]

replace [exe ISTD01;] with [ "Add the ist detector to the IDSM"; ISTD=on; IstdConfig=1; ]
replace [exe ISTD02;] with [ "Add the ist detector to the IDSM"; ISTD=on; IstdConfig=2; ]

replace [exe PXST01;] with [ "Add the PST to the IDSM"; PXST=on; PxstConfig=0; ]
replace [exe PSUPof;] with [ "Add the pixel supports to the IDSM"; PSUP=off; ]
replace [exe PSUP01;] with [ "Add the pixel supports to the IDSM"; PSUP=on; ]

replace [exe RICHof;] with [;RICH=off;]
replace [exe RICH02;] with [;RICH=on; richPos=2; richConfig=2;]

replace [exe SCON02;] with [;SCON = off; ConeConfig=2 " new cable weight estimate ";]

replace [exe SCON12;] with [;SCON = on ; ConeConfig=2 " new cable weight estimate ";]
replace [exe SCON13;] with [;SCON = on ; ConeConfig=3 " new cable weight estimate ";]
replace [exe SCON14;] with [;SCON = on ; ConeConfig=4 " new cable weight estimate better SROD";]


*                                                                               Silicon Strip Detector

replace [exe SISDof;] with ["Silicon Strip Detector off "; SISD=off;]
replace [exe SISD02;] with ["Silicon Strip Detector on  "; SISD=on ; SisdConfig= 2;]
replace [exe SISD12;] with ["Silicon Strip Detector on  "; SISD=on ; SisdConfig=12;]
replace [exe SISD22;] with ["Silicon Strip Detector on  "; SISD=on ; SisdConfig=22;]
replace [exe SISD23;] with ["Silicon Strip Detector on  "; SISD=on ; SisdConfig=23;]
replace [exe SISD24;] with ["Silicon Strip Detector on  "; SISD=on ; SisdConfig=24;]
replace [exe SISD35;] with ["Silicon Strip Detector on  "; SISD=on ; SisdConfig=35;]
replace [exe SISD55;] with ["Silicon Strip Detector on  "; SISD=on ; SisdConfig=55;]
replace [exe SISD65;] with ["Silicon Strip Detector on  "; SISD=on ; SisdConfig=65;]
replace [exe SISD75;] with ["Silicon Strip Detector on  "; SISD=on ; SisdConfig=75;]
replace [exe SISD85;] with ["Silicon Strip Detector on  "; SISD=on ; SisdConfig=85;]

replace [exe SVTTof;] with ["SVTT version"; SVTT=off; SvttConfig = -1;]
replace [exe SVTT00;] with ["SVTT version"; SVTT=on;
                              SvttConfig = 0; svtWater=off; nSvtVafer=7; nSvtLayer=6;]
replace [exe SVT100;] with ["SVTT version"; SVTT=on;
                              SvttConfig = 0; svtWater=on; nSvtVafer=0; nSvtLayer=-3; svtWaferDim=0;]
replace [exe SVT101;] with ["SVTT version"; SVTT=on;
                              SvttConfig = 1; svtWater=on ; nSvtLayer=6;]
replace [exe SVT102;] with ["SVTT version"; SVTT=on;
                              SvttConfig = 2; svtWater=on; nSvtVafer=0; nSvtLayer=6; svtWaferDim=0;
                              SvshConfig = 0; "No SVT shield";]
replace [exe SVT103;] with ["SVTT version"; SVTT=on;
                              SvttConfig = 3; svtWater=on; nSvtVafer=0; nSvtLayer=6; svtWaferDim=0;
                              SvshConfig = 0; "No SVT shield";]
replace [exe SVT106;] with ["SVTT version"; SVTT=on;
                              SvttConfig = 6; svtWater=on; nSvtVafer=0; nSvtLayer=6; svtWaferDim=0;
                              SvshConfig = 0; "No SVT shield";]
replace [exe SVT203;] with ["SVTT version"; SVTT=on;
                              SvttConfig = 3; svtWater=on; nSvtVafer=0; nSvtLayer=6; svtWaferDim=0;
                              SvshConfig = 2; "SVT shield";]
replace [exe SVT204;] with ["SVTT version"; SVTT=on;
                              SvttConfig = 4; svtWater=on; nSvtVafer=0; nSvtLayer=6; svtWaferDim=0;
                              SvshConfig = 2; "SVT shield";]
replace [exe SVT304;] with ["SVTT version"; SVTT=on;
                              SvttConfig = 4; svtWater=on; nSvtVafer=0; nSvtLayer=6; svtWaferDim=0;
                              SvshConfig = 3; "SVT shield";]
replace [exe SVT206;] with ["SVTT version"; SVTT=on;
                              SvttConfig = 6; svtWater=on; nSvtVafer=0; nSvtLayer=6; svtWaferDim=0;
                              SvshConfig = 2; "SVT shield";]

replace [exe SVT306;] with ["SVTT version"; SVTT=on;
                              SvttConfig =  6; svtWater=on; nSvtVafer=0; nSvtLayer=6; svtWaferDim=0;
                              SvshConfig = 3; "SVT shield";]
replace [exe SVT306x;] with ["SVTT version"; SVTT=on;
                              SvttConfig =  6; svtWater=on; nSvtVafer=0; nSvtLayer=7; svtWaferDim=0;
                              SvshConfig = 3; "SVT shield";]
replace [exe SVT310x;] with ["SVTT version"; SVTT=on;
                              SvttConfig = 10; svtWater=on; nSvtVafer=0; nSvtLayer=7; svtWaferDim=0;
                              SvshConfig = 3; "SVT shield";]
replace [exe SVT211;] with ["SVTT version"; SVTT=on;
                              SvttConfig = 11; svtWater=on; nSvtVafer=0; nSvtLayer=6; svtWaferDim=0;
                              SvshConfig = 2; "SVT shield";]
replace [exe SVT311;] with ["SVTT version"; SVTT=on;
                              SvttConfig = 11; svtWater=on; nSvtVafer=0; nSvtLayer=6; svtWaferDim=0;
                              SvshConfig = 3; "SVT shield";]
replace [exe SVT312;] with ["SVTT version"; exe SVT311; SvttConfig = 12;]

replace [exe SVT312x;] with ["SVTT version"; SVTT=on;
                              SvttConfig = 12; svtWater=on; nSvtVafer=0; nSvtLayer=7; svtWaferDim=0;
                              SvshConfig = 3; "SVT shield";]

replace [exe BTOF42;] with [;BTOF=on; BtofConfig= 4;Itof=2 " call btofgeo2 ";]
replace [exe BTOF52;] with [;BTOF=on; BtofConfig= 5;Itof=2 " call btofgeo2 ";]
replace [exe BTOF72;] with [;BTOF=on; BtofConfig= 7;Itof=2 " call btofgeo2 ";]
replace [exe BTOF84;] with [;BTOF=on; BtofConfig= 8;Itof=4 " call btofgeo4 ";]
replace [exe BTOFa5;] with [;BTOF=on; BtofConfig=10;Itof=5 " call btofgeo5 ";]

replace [exe BTOF16;] with [;" X.Dong";BTOF=on;
                            BtofConfig=1; Itof=6 " call btofgeo6 ";
                            tofX0= 0.00; tofZ0=-0.50;]
replace [exe BTOF66;] with [;" X.Dong";BTOF=on;
                            BtofConfig=6; Itof=6 " call btofgeo6 ";
                            tofX0= 0.00; tofZ0=0;]

replace [exe BTOF67;] with [;"F.Geurts fixes to sensitive volumes";
                            BtofConfig=6; Itof=7 "call btofgeo7";
                            tofX0=0.00; tofZ0=0.00;]

replace [exe BTOFb6;] with [;" X.Dong";BTOF=on;
                            BtofConfig=11; Itof=6 " call btofgeo6 ";
                            tofX0= 0.00; tofZ0=-0.50;]

replace [exe BTOFb7;] with [;" X.Dong";BTOF=on;
                            BtofConfig=11; Itof=7 " call btofgeo7 ";
                            tofX0= 0.00; tofZ0=-0.50;]

replace [exe BTOFc6;] with [;" F.Geurts";BTOF=on; BtofConfig=12; Itof=6 " call btofgeo6 ";]
replace [exe BTOFc7;] with [;" F.Geurts";BTOF=on; BtofConfig=12; Itof=7 " call btofgeo7 ";]

replace [exe BTOFv8;] with [;BTOF=on; BtofConfig=13; Itof=8 "call btofgeo8";]
""" ----------------------------------------------------------------------- """
""" TPC Configurations                                                      """
"""     Naming convention is offset by 1 from the corresponding module.     """
"""                                                                         """
""" TPCE00  Module: tpcegeo                                                 """
""" TPCE01  Module: tpcegeo                                                 """
""" TPCE02  Module: tpcegeo1                                                """
""" TPCE03  Module: tpcegeo2                                                """
""" TPCE04  Module: tpcegeo3                                                """
""" TPCE04r Module: tpcegeo3 with reduced rmax                              """
"""                                                                         """
""" TPCE31 revises this scheme.  It corresponds to the version number (3)   """
"""        and subversion number (1). TPCE30 == TPCE04r.                    """
""" TPCE51 revises this scheme.  It corresponds to the version number (5)   """


replace [exe TPCE00;] with [;"New version of the TPC backplane "; TpceConfig = 1;]
replace [exe TPCE01;] with [;"New version of the TPC backplane "; TpceConfig = 1;
                             "gas density correction";            DensConfig = 1;]
replace [exe TPCE02;] with [;"New version of the TPC backplane "; TpceConfig = 2;
                             "gas density correction";            DensConfig = 1;]
replace [exe TPCE03;] with [;"New version of the TPC backplane "; TpceConfig = 3;
                             "gas density correction";            DensConfig = 1;]
replace [exe TPCE04;] with [;"New version of the TPC backplane "; TpceConfig = 4;
                             "gas density correction";            DensConfig = 1;]
replace [exe TPCE04r;] with [;"New version of the TPC backplane "; TpceConfig = 4;
                              "gas density correction";            DensConfig = 1;
                              "radius correction";                 RmaxConfig = 1;]
replace [exe TPCE30;] with [;"Equivalent to TPCE04r"; exe TPCE30; ]
replace [exe TPCE31;] with [;"                     "; TpceConfig = 31; ]
replace [exe TPCE05r;] with [;"New version of the TPC backplane "; TpceConfig = 51;
                              "gas density correction";            DensConfig = 1;
                              "radius correction";                 RmaxConfig = 1;]
replace [exe TPCE05rA;] with [;"New version of the TPC backplane "; TpceConfig = 51;
                              "gas density correction";            DensConfig = 1;
                              "radius correction";                 RmaxConfig = 1;
	                      ;"Set 1st TPAD config"; TpadConfig=1;] 
replace [exe TPCE05rB;] with [;"New version of the TPC backplane "; TpceConfig = 51;
                              "gas density correction";            DensConfig = 1;
                              "radius correction";                 RmaxConfig = 1;
	                      ;"Set 2nd TPAD config"; TpadConfig=2;]
replace [exe TPCE05rC;] with [;"New version of the TPC backplane "; TpceConfig = 51;
                              "gas density correction";            DensConfig = 1;
                              "radius correction";                 RmaxConfig = 1;
	                      ;"Set 3rd TPAD config"; TpadConfig=3;]
replace [exe TPCE05rD;] with [;"New version of the TPC backplane "; TpceConfig = 51;
                              "gas density correction";            DensConfig = 1;
                              "radius correction";                 RmaxConfig = 1;
	                      ;"Set 4th TPAD config"; TpadConfig=4;]
replace [exe TPCE05rE;] with [;"New version of the TPC backplane "; TpceConfig = 51;
                              "gas density correction";            DensConfig = 1;
                              "radius correction";                 RmaxConfig = 1;
	                      ;"Set 5th TPAD config"; TpadConfig=5;]

replace [exe TPCE05rF;] with [;"New version of the TPC backplane "; TpceConfig = 51;
                              "gas density correction";            DensConfig = 1;
                              "radius correction";                 RmaxConfig = 1;
	                      ;"Set 5th TPAD config"; TpadConfig=6;]

replace [exe TPCE05rX;] with [;"New version of the TPC backplane "; TpceConfig = 51;
                              "gas density correction";            DensConfig = 1;
                              "radius correction";                 RmaxConfig = 1;
	                      ;"Set the final TPAD config"; TpadConfig=7;]
replace [exe TPCE05rY;] with [;"New version of the TPC backplane "; TpceConfig = 51;
                              "gas density correction";            DensConfig = 1;
                              "radius correction";                 RmaxConfig = 1;
	                      ;"Set the final TPAD config"; TpadConfig=8;]
replace [exe TPCE51;] with [;"                     "; TpceConfig = 51; ]



replace [exe tpcx10;] with [;"TPC test version";    TpcxConfig=1;
                            ;"Disable old TPC";     TpceConfig=0;
                            ;"Set 1st TPAD config"; TpadConfig=0;]
replace [exe tpcx11;] with [;"TPC test version";    TpcxConfig=1;
                            ;"Disable old TPC";     TpceConfig=0;
                            ;"Set 2nd TPAD config"; TpadConfig=1;]
replace [exe tpcx16;] with ["TPC test version";     TpcxConfig=2;
                            "Disable old TPC";      TpceConfig=0;
                            ;"Set 6th TPAD config"; TpadConfig=6;]
                            

replace [exe ISTB00;] with [;ISTB=on;IstbConfig=-1;]

replace [exe VPDDof;] with [;VPDD=off;]
replace [exe VPDD02;] with  [;"pseudo Vertex Position Detector";VPDD=on;VpddConfig=2; VpddModule=0; ]
replace [exe VPDD03;] with  [;"pseudo Vertex Position Detector";VPDD=on;VpddConfig=3; VpddModule=0; ]
replace [exe VPDD04;] with  [;"pseudo Vertex Position Detector";VPDD=on;VpddConfig=4; VpddModule=0; ]
replace [exe VPDD07;] with  [;"pseudo Vertex Position Detector";VPDD=on;VpddConfig=7; VpddModule=2; ]
replace [exe VPDD08;] with  [;"pseudo Vertex Position Detector";VPDD=on;VpddConfig=7; VpddModule=3; ]


replace [exe FGTDof;] with  [;FGTD=off;FgtdConfig=0; "FGT off";]
replace [exe FGTD02;] with  [;FGTD=on;FgtdConfig=2;  "GEM forward tracker"]
replace [exe FGTDv31;] with [;FGTD=on;FgtdConfig=31; "FGT v3 5 half plus one full disk"]
replace [exe FGTDv32;] with [;FGTD=on;FgtdConfig=32; "FGT v3 6 disks"]
replace [exe FGTDv55;] with [;FGTD=on;FgtdConfig=55; "FGT very forward upgrade w/ 12 disks"]
replace [exe FGTDv56;] with [;FGTD=on;FgtdConfig=56; "FGT very forward w/ 6 disks";]

replace [exe IDSM01;] with [;IDSM=on;IdsmConfig=1; "Inner Detector Support"]
replace [exe IDSM02;] with [;IDSM=on;IdsmConfig=2; "Inner Detector Support"]
replace [exe IDSM14;] with [;IDSM=on;IdsmConfig=14; "Y2014 version of IDSM"]

replace [exe FSTDof;] with  [;FSTD=off;]
replace [exe ITSPof;] with  [;ITSP=off;] "prototype of the Inner Tracker SuPport structure"

replace [exe FHCM01;] with  [;FhcmConfig=1;] 


replace [exe EIDDv01;] with [;EiddConfig=1; EIDD=on; ]


********* Geometry definitions *******************************************************
*********   y2000   ***
replace [exe y2000;] with [;"corrected: MWC readout, RICH reconstructed position, no TOF ";
                            "actual 2000:  TPC+CTB+RICH+caloPatch+svtLadder"
        exe TPCE00; 
        exe VPDDof;
        exe ECALof;
        exe FTPCof; 
        exe SVTT00;
        exe CALB00;
        exe MFLD23;
        ]

*********   y2001   ***
replace [exe y2001;] with ["2001 geometry - TPC+CTB+FTPC+RICH+CaloPatch+SVT+FPD";
" 02/09/2004  Jerome signed off on changing, retroactively, the"
" position of the wafers in year2001, which was incorrectly offset"
" by 250 um insterad of 150 um."
" -- Obsoleted CorrNum = 1;"
        exe TPCE00; 
        exe SVT101;
        exe FTPC00;
        exe BTOF42; 
        exe RICH02;
        exe ECAL31; 
        exe CALBa0; 
        exe MFLD53;
        ]

*********   y2002   ***
replace [exe y2002;] with ["january 2002 geometry - TPC+CTB+FTPC+CaloPatch2+Rich+SVT3+BBC+FPD";                           
        exe TPCE00; 
        exe SVT100; 
        exe RICH02; 
        exe BTOF42; 
        exe CALBa0; 
        exe ECALof;
        exe BBCMon; 
        exe FPDM00; 
        exe VPDD02; 
        exe MFLD54;
        ]

*========   y2003   y2003   y2003   y2003   =y2003  y2003    y2003   ============================
*********   y2003   ***
replace [exe y2003;] with ["draft 2003 geometry - TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL";
         exe TPCE00; 
         exe SVT100; 
         exe RICHof; 
         exe BTOF52; 
         exe CALBb0; 
         exe ECAL11;
         exe BBCMon; 
         exe FPDM00; 
         exe VPDD03; 
         exe MFLD54;
         ]


*********   y2003a   ***
replace [exe y2003a;] with [
***********************************************************************
* In y2003a:
*    removed serious bugs from SUPOGEO (incorrect positioning inside the SVT,
*    where is doesn't belong)
*    corrected CALB -- the shift variable (was 0,0 so the barrel wasn't tilted right)
*    corrected SVT  -- the layer radii (from 250 to 150 microns, see the svt code)
****************************************************************************************
        exe y2003; 
        exe CALBd0; 
        exe FTPC00; 
        exe SVT101
        ]



*********   y2003b   ***
replace [exe y2003b;] with [
***********************************************************************
* y2003b is y2003a, but with the extra material in the SVT
* This is actually an important case (i.e. the "most precise" geometry
* approximation for the early 2003 run) which we were lacking so far.
* This is achieved by setting CorrNum to 2.
* The endcap EMC has one third of one wheel, as before
* For more info on the extra material in SVT -- see web page
***********************************************************************
        exe y2003a; 
        exe SVT102;
        ]



*********   y2003c   ***
replace [exe y2003c;] with [ "Better SVT model on top of 2003B: TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL";
        exe y2003b; 
        exe SVT106;
        ]



*********   y2003x   ***
replace [exe y2003x;] with [ "same as y2003b but with full calorimeters and PHMD";
        exe y2003b; 
        exe CALBc0; 
        exe ECAL33;  
        exe PHMD01;
]



*********  (in)complete   ***
!$$$    [exe COMPLETE;] with ["New Complete+correction 3 in 2003 geometry:";
!$$$                          "TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL+PHMD";
!$$$                          "(complete tag has been out of date since 2003)x";
!$$$     exe SVT204;
!$$$     exe BTOF52;
!$$$     exe CALBc0;
!$$$     exe ECAL33;
!$$$     exe BBCMon;
!$$$     exe FPDM00;
!$$$     exe TPCE01;
!$$$     exe FTPC00;
!$$$     exe PHMD01;
!$$$     exe SISDof;
!$$$     exe PIPE04; 
!$$$     exe PIXL01;
!$$$     ]



*========   y2004   y2004  y2004  y2004  y2004  y2004  y2004  y2004  =================================
*********   y2004   ***
* baseline 2004 geometry: TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL+PHMD with low cuts GSTPAR in PHMD;
replace [exe y2004;] with [
        exe TPCE01;
        exe SVT103; 
        exe BTOF72; 
        exe CALB01; 
        exe ECAL31; 
        exe BBCMon; 
        exe FPDM01;
        exe VPDD04; 
        exe MFLD54; 
        exe FTPC00; 
        exe PHMD01; 
        exe SISD02;
        ]



*********   y2004a   ***
replace [exe y2004a;] with [ exe y2004; exe PHMD02; ]

*********   y2004b   ***
replace [exe y2004b;] with [ exe y2004a; exe SISD12; ]

*********   y2004c   ***
replace [exe y2004c;] with [ exe y2004b; exe TPCE02; exe SVT204; exe SCON02; ]

*********   y2004d   ***
replace [exe y2004d;] with [
                exe SVT206; exe BTOF72; exe CALB01; exe ECAL31; exe BBCMon; exe FPDM01; exe SCON02;
                exe VPDD04; exe MFLD54; exe FTPC01; exe PHMD02; exe SISD22; exe FTRO01; exe TPCE02;
]
*********   y2004x   ***
replace [exe y2004x;] with [
                exe SVT203; exe BTOF72; exe CALBe0; exe ECAL31; exe BBCMon; exe FPDM01;
                exe VPDD04; exe MFLD54; exe FTPC01; exe PHMD02; exe SISD02; exe TPCE01;
]
*********   y2004y   ***
replace [exe y2004y;] with [
                exe SVT204; exe SCON02; exe BTOF72; exe CALBe0; exe ECAL31; exe BBCMon; exe FPDM01;
                exe VPDD04; exe MFLD54; exe FTPC01; exe FTRO01; exe PHMD02; exe SISD22; exe TPCE02;
]






*______________________________________________________________________________
*========   y2005   y2005  y2005  y2005  y2005  y2005  y2005  y2005  =================================



*********   y2005   ***
replace [exe y2005;] with [
                exe SVT203; exe SCON02; exe BTOF72; exe CALB01; exe ECAL31; exe BBCMon; exe FPDM01;
                exe VPDD04; exe MFLD54; exe FTPC01; exe PHMD02; exe FTRO01; exe SISD23; exe TPCE01;
]

*********   y2005b   ***
replace [exe y2005b;] with [
                exe SVT204; exe SCON02; exe BTOF72; exe CALB01; exe ECAL31; exe BBCMon; exe FPDM01;
                exe VPDD04; exe MFLD54; exe FTPC01; exe PHMD02; exe FTRO01; exe SISD24; exe TPCE02;
]

*********   y2005c   ***
replace [exe y2005c;] with [ exe y2005b;  exe BTOF84;]

*********   y2005d   ***
replace [exe y2005d;] with [ exe y2005c;  exe SVT206;]

*********   y2005e   ***
replace [exe y2005e;] with [
                exe SVT306; exe SCON02; exe BTOF84; exe CALB01; exe ECAL31; exe BBCMon; exe FPDM01;
                exe VPDD04; exe MFLD54; exe FTPC01; exe PHMD02; exe FTRO01; exe SISD35; exe TPCE02;
]



replace [exe y2005f;] with [ 
         exe y2005e; 
         exe CALB02;      
         exe EMCUTS(bemc,0); "disable 10 keV cuts";
         exe EMCUTS(eemc,0); "disable 10 keV cuts";
         exe SISD55;
         ]

replace [exe y2005g;] with [ 
         exe y2005f; 
         exe svt312; 
         exe sisd75;
         ]

replace [exe y2005h;] with [ 
        exe y2005g; 
        exe TPCE04;
        ]

replace [exe y2005i;] with [
        exe y2005h; 
        exe ECALv6;         "Latest version of the EEMC geometry";
        exe EMCUTS(bemc,1); "10 keV transport cuts in the BEMC";
        exe EMCUTS(eemc,1); "10 keV transport cuts in the EEMC";
        ]



!//______________________________________________________________________________
*********   y2006   ***

replace [exe y2006;] with [ "y2006 baseline which is Y2005D+fixed TPC backplane+New SSD"
                exe SVT306; 
                exe SCON02; 
                exe BTOF84; 
                exe CALB01; 
                exe ECAL31;
	        exe BBCMon; 
                exe FPDM01; 
                exe VPDD04; 
                exe MFLD54; 
                exe FTPC01;
		exe PHMD02; 
                exe FTRO01; 
                exe SISD35; 
                exe TPCE03; 
                exe mutd01; 
                exe cave03;
                ]
                 

*********   y2006a   ***
replace [exe y2006a;] with ["Y2006 baseline which is Y2005D+fixed TPC backplane+New SSD"
         exe y2006; 
         exe FPDM02;
         ]

*********   y2006b   ***
replace [exe y2006b;] with ["Y2006A + improved SSD with dead area + improved CALB"
         exe y2006; 
         exe CALB02; 
         exe EMCUTS(bemc,0); "disable 10 keV cuts";
         exe EMCUTS(eemc,0); "disable 10 keV cuts";
         exe FPDM02;
         exe SISD55;
         ]




*********   y2006c   ***
replace [exe y2006c;] with ["Y2006B without the PHMD"
         exe y2006b; 
         exe PHMDof;
         ]

*********   y2006g   ***
replace [exe y2006g;] with ["Y2006C new SVT dead material"
         exe y2006c; 
         exe SVT312;
         exe SISD75;
         ]

*                                                                    == y2006h ==
* Development geometry, not yet cleared for production.  May be altered / removed 
* at any time.
*
replace [exe y2006h;] with ["y2006g + new BEMC, new EEMC";
        exe y2006g;    "Y2006h modifies Y2006g geometry"; 
        exe TPCe04;    "Latest model of the TPC, with additional mass";  
        exe CALB02;    "Latest model of the BEMC, with additional volumes";
        exe ECALv6;    "Latest model of the EEMC, with additional volumes and bug fixes";
        exe EMCUTS(eemc,1); "10 keV cuts in b/emc calorimeter volumes";
        exe EMCUTS(bemc,1); "10 keV cuts in b/emc calorimeter volumes";
        ]

!//______________________________________________________________________________
*********   y2007   ***
replace [exe y2007;] with ["y2006 baseline which is Y2006+FMS"
                exe SVT306x;
                exe SCON02; 
                exe BTOFa5; 
                exe CALB02; 
                exe ECAL31;
                exe EMCUTS(eemc,0);   "disable 10 keV calorimeter cuts";
                exe EMCUTS(bemc,0);   "disable 10 keV calorimeter cuts";
                exe BBCMon; 
                exe FPDM03; 
                exe VPDD07; 
                exe MFLD54; 
                exe FTPC01;		
                exe PHMD02; 
                exe FTRO01; 
                exe SISD55; 
                exe TPCE03; 
                exe mutd01; 
                exe cave04;
        ]

*********   y2007a   ***
replace [exe y2007a;] with ["y2007 but corrected SVT,carbon instead of Be water channels";
         exe y2007; 
         exe SVT310x;
        ]


*********   y2007g   ***
replace [exe y2007g;] with ["y2007A + dead material from Rene"
         exe y2007a; 
         exe SVT312x;
         exe SISD75;
         ]

*********   y2007h   ***
replace [exe y2007h;] with ["y2007g + TPC y2009"
         exe y2007g; 
         exe TPCE04;
         ]

""" ================================================================================= """
""" NOTE: Y2007 and earlier need to have the SROD modification applied to the support """
"""       rods in the SVT.  This involves correcting 10 different geometry files in   """
"""       order to propagate this fix backwards.                                      """
""" ================================================================================= """


!//______________________________________________________________________________
*********   y2008   ***
replace [exe y2008;] with [;
{ "y2008 baseline: no SVT,  cones,beam support,FTPC in CAVE now"
    exe SCON02;
    exe TPCE03;
    exe BTOFb6;
    exe CALB02;
    exe ECAL31;
    exe EMCUTS(eemc,0);   "disable 10 keV calorimeter cuts";
    exe EMCUTS(bemc,0);   "disable 10 keV calorimeter cuts";
    exe BBCMon;
    exe FPDM03;
    exe VPDD07;
    exe FTPC01;
    exe SVTTof;
    exe PHMDof;
    exe SISDof;
    exe FTRO01;
    exe MUTD03;
    exe CAVE04;
    exe PIPE12;
};]

*********   y2008a   ***
replace [exe y2008a;] with [;exe y2008; exe SCON13;]

replace [exe y2008b;] with [;exe y2008a; 
        exe TPCE04;
        exe CALB02;
        exe ECALv6;]


replace [exe y2008c;] with ["Y2008 production tag C: Fixes TOF response " ; 
        exe y2008b ; "Inherit everything from y2008b";
        exe TPCE04r; "Reduce the TPC envelope raidus";
        exe BTOFb7;           "Fixed TOF sensitve volumes";
        ]

replace [exe y2008d;] with [
   "Y2008 production tag D: Improved SROD description in support cone";
   exe y2008c;
   exe scon14;
]

replace [exe y2008e;] with [
   "Y2008 production tag E: Same as D but with lowered EM cuts in the EEMC and BEMC";
   exe y2008d;
   exe EMCUTS(eemc,1); "Enable 10 keV cuts in EEMC";
   exe EMCUTS(bemc,1); "Enable 10 keV cuts in BEMC";
   exe CALB02;
   exe ECALv6;
]
 

!//______________________________________________________________________________
*                                                                           Y2009
replace [exe y2009;] with [;
{   "y2009 baseline: much more detailed TPC (thnx YF)"
    exe SCON13;
    exe TPCE04;
    exe BTOFc6;
    exe CALB02;
    exe ECAL31;   
    exe EMCUTS(eemc,0); "disable 10 keV cuts";
    exe EMCUTS(bemc,0); "disable 10 keV cuts";
    exe BBCMon;
    exe FPDM03;
    exe VPDD07;
    exe FTPC01;
    exe SVTTof;
    exe PHMDof;
    exe SISDof;
    exe FTRO01;
    exe MUTD03;
    exe CAVE04;
    exe PIPE12;    
};]

replace [exe y2009a;] with [;
{   "y2009a baseline: much more detailed TPC (thnx YF), version 6.1 of the endcap geometry"
    exe SCON13;      "support cone without SVT and new cable weight estimates";
    exe TPCE04;      "agstar version of yf model";
    exe BTOFc6;      "time of flight";
    exe CALB02;      "updated bemc model";
    exe ECALv6;      "several bugfixes in eemc geometry";
    exe EMCUTS(eemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
    exe EMCUTS(bemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
    exe BBCMon;      "beam beam counters";
    exe FPDM03;      "";
    exe VPDD07;      "";
    exe FTPC01;      "";
    exe SVTTof;      "";
    exe PHMDof;      "Photon mult detector out of 2009a";
    exe SISDof;
    exe FTRO01;
    exe MUTD03;
    exe CAVE04;
    exe PIPE12;
};]


replace [exe y2009b;] with [;
{   "y2009b production tag B: Y2009A tag with the old tracking cuts in the EEMC.";
    "This tag is not appropriate for EEMC simulations.";
    exe Y2009A;           "Y2009A configugration";
    exe EMCUTS(eemc,0);   "10 keV EM thresholds in barrel and endcap calorimeters";
    exe EMCUTS(bemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
};]

replace [exe y2009c;] with [;
   "y2009b production tag C: Y2009A tag with fixed TOF resonse";
   exe Y2009A;           "Y2009A configugration";
   exe TPCE04r;          "Reduced TPC envelope radius";
   exe BTOFc7;           "Fixed TOF sensitve volumes";
;]

replace [exe y2009d;] with [;
   "y2009d production tag D: Improved SROD description in support cone";
   exe Y2009C;           "Y2009C configugration";
   exe SCON14;           "Improved SROD";
;]



!//______________________________________________________________________________
*********   y2010   ***

replace [exe y2010;] with [;
{ "y2010 baseline: y2009a+full tof+phmd, blessed 04/13 jcw"
  exe y2009a; 
  exe BTOF66;
  exe PHMD02;
};]

replace [exe y2010a;] with [;
 "y2010a: production tag A"
  exe SCON13;      "support cone without SVT and new cable weight estimates";
  exe TPCE04;      "agstar version of yf model";
  exe BTOF66;      "time of flight";
  exe CALB02;      "updated bemc model";
  exe ECALv6;      "several bugfixes in eemc geometry";
  exe EMCUTS(eemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
  exe EMCUTS(bemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
  exe BBCMon;      "beam beam counters";
  exe FPDM03;      "";
  exe VPDD07;      "";
  exe FTPC01;      "";
  exe SVTTof;      "";
  exe PHMD02;      "Photon mult detector";
  exe SISDof;
  exe FTRO01;
  exe MUTD03;
  exe CAVE04;
  exe PIPE12;
;]

replace [exe y2010b;] with ["Y2010 production tag B: Based on A, with TOF fixes";
   exe y2010a;           "Inherit from y2010a";
   exe TPCE04r;          "reduced TPC envelope raidus";
   exe BTOF67;           "fixes to TOF sensitive volume dimensions";
   ]
replace [exe y2010c;] with ["Y2010 production tag C: Improved SROD description in support cone";
   exe y2010b;           "Inherit from y2010a";
   exe scon14;           "Support cone"
   ] 

c ======================================================================= y2011 =
REPLACE [exe y2011;] with ["y2011 baseline: Essentially Y2010a with fixes to TPC envelope radius and TOF";
    exe SCON14;      "support cone without SVT and new cable weight estimates and SROD fix";
    exe TPCE04r;     "agstar version of yf model with reduced Rmax";
    exe BTOF67;      "time of flight";
    exe CALB02;      "updated bemc model";
    exe ECALv6;      "several bugfixes in eemc geometry";
    exe EMCUTS(eemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
    exe EMCUTS(bemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
    exe BBCMon;      "beam beam counters";
    exe FPDM03;      "Latest version of FPD";
    exe VPDD07;      "Latest version of VPD";
    exe FTPC01;      "FTPC";
    exe SVTTof;      "No SVT";
    exe PHMD02;      "Photon mult detector on";
    exe SISDof;      "No sisd";
    exe FTRO01;      "FTPC readout";
    exe MUTD04;      "Muon telescope detector";
    exe CAVE04;      "Cave and tunnel";
    exe PIPE12;      "The beam pipe";
]

REPLACE [exe y2011a;] with ["y2011a: Pro.  consistent with production series P11ic and P11id";
    exe SCON14;      "support cone without SVT and new cable weight estimates and SROD fix";
    exe TPCE04r;     "agstar version of yf model with reduced Rmax";
    exe BTOF67;      "time of flight";
    exe CALB02;      "updated bemc model";
    exe ECALv6;      "several bugfixes in eemc geometry";
    exe EMCUTS(eemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
    exe EMCUTS(bemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
    exe BBCMon;      "beam beam counters";
    exe FPDM03;      "Latest version of FPD";
    exe VPDD07;      "Latest version of VPD";
    exe FTPC01;      "FTPC";
    exe SVTTof;      "No SVT";
    exe PHMD02;      "Photon mult detector on";
    exe SISDof;      "No sisd";
    exe FTRO01;      "FTPC readout";
    exe MUTD04;      "Muon telescope detector";
    exe CAVE04;      "Cave and tunnel";
    exe PIPE12;      "The beam pipe";
]

REPLACE [exe y2011b;] with ["y2011b: ";
    exe SCON14;      "support cone without SVT and new cable weight estimates and SROD fix";
    exe TPCE05r;     "agstar version of yf model with reduced Rmax, new rows arrangement";
    exe BTOF67;      "time of flight";
    exe CALB02;      "updated bemc model";
    exe ECALv6;      "several bugfixes in eemc geometry";
    exe EMCUTS(eemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
    exe EMCUTS(bemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
    exe BBCMon;      "beam beam counters";
    exe FPDM03;      "Latest version of FPD";
    exe VPDD07;      "Latest version of VPD";
    exe FTPC01;      "FTPC";
    exe SVTTof;      "No SVT";
    exe PHMD02;      "Photon mult detector on";
    exe SISDof;      "No sisd";
    exe FTRO01;      "FTPC readout";
    exe MUTD04;      "Muon telescope detector";
    exe CAVE05;      "Extended Cave and tunnel";
    exe PIPE12;      "The beam pipe";
]
c ===============================================================================
c ===============================================================================
c ===============================================================================

REPLACE [exe y2012;] with ["y2012 FGT upgrade studies";
    exe TPCE04r;     "agstar version of yf model with reduced Rmax";
    exe BTOF67;      "time of flight";
    exe CALB02;      "updated bemc model";
    exe ECALv6;      "several bugfixes in eemc geometry";
    exe EMCUTS(eemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
    exe EMCUTS(bemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
    exe BBCMon;      "beam beam counters";
    exe FPDM03;      "Latest version of FPD";
    exe VPDD07;      "Latest version of VPD";
    exe FTPCof;      "FTPC";
    exe SVTTof;      "No SVT";
    exe PHMDof;      "Photon mult detector on";
    exe SISDof;      "No sisd";
   "exe MUTD12; executable code was not present when tag defined... no effect"
    exe CAVE04;      "Cave and tunnel";
    exe PIPE12;      "The beam pipe";

    exe IDSM01;      "Inner detector support";
    exe FGTDv31;     "FGT v3 5 half + 1 full disks";
]

REPLACE [exe y2012a;] with ["y2012a production geometry tag";
    exe TPCE04r;     "agstar version of yf model with reduced Rmax";
    exe BTOF67;      "time of flight";
    exe CALB02;      "updated bemc model";
    exe ECALv6;      "several bugfixes in eemc geometry";
    exe EMCUTS(eemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
    exe EMCUTS(bemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
    exe BBCMon;      "beam beam counters";
    exe FPDM03;      "Latest version of FPD";
    exe VPDD07;      "Latest version of VPD";
    exe FTPCof;      "FTPC";
    exe SVTTof;      "No SVT";
    exe PHMDof;      "Photon mult detector on";
    exe SISDof;      "No sisd";
   "exe MUTD12; executable code not present when tag defined ... no effect"
    exe CAVE04;      "Cave and tunnel";
    exe PIPE12;      "The beam pipe";
    exe IDSM01;      "Inner detector support";
    exe FGTDv31;     "FGT v3 5 half + 1 full disks";
]

REPLACE [exe y2012b;] with ["y2012b production geometry tag";
    exe y2012a; "Y2012a baseline";
    exe MUTD12; "Adds in code to activate MTD";
]

REPLACE [exe y2012c;] with ["y2012c production geometry tag";
    exe y2012b; "Y2012b baseline";
    exe TPCE05r;      "agstar version of yf model with reduced Rmax";
    exe CAVE05;       "Extended Cave and tunnel";
]
c ===============================================================================
c ===============================================================================
c ===============================================================================

REPLACE [exe dev13;] with ["DEV13 upgrade geometry";
    exe TPCE04r;     "agstar version of yf model with reduced Rmax";
    exe BTOF67;      "time of flight";
    exe CALB02;      "updated bemc model";
    exe ECALv6;      "several bugfixes in eemc geometry";
    exe EMCUTS(eemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
    exe EMCUTS(bemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";

    exe BBCMon;      "beam beam counters";
    exe FPDM03;      "Latest version of FPD";
    exe VPDD07;      "Latest version of VPD";
    exe FTPCof;      "FTPC";
    exe SVTTof;      "No SVT";
    exe PHMDof;      "Photon mult detector on";
    exe SISDof;      "No sisd";
    exe MUTD13;      "Muon telescope detector";
    exe CAVE04;      "Cave and tunnel";
    exe PIPEv1;      "The beam pipe";

    exe IDSM02;      "Inner detector support";
    exe FGTDv32;     "FGT v3 6 disks";

    exe PXST01;      "PIXEL detector support version 1";
    exe PIXL02;      "Development version of the pixl detector";
]

REPLACE [exe y2013;] with ["Y2013 first cut geometry";
    exe CAVE04;      "Cave version 4";
    exe MAGPv1;      "New magnet with H20";
    exe TPCE04r;     "agstar version of yf model with reduced Rmax";
    exe BTOFv8;      "time of flight";
    exe CALB02;      "updated bemc model";
    exe ECALv6;      "several bugfixes in eemc geometry";
    exe EMCUTS(eemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
    exe EMCUTS(bemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";

    exe BBCMon;      "beam beam counters";
    exe FPDM03;      "Latest version of FPD";
    exe VPDD07;      "Latest version of VPD";
    exe FTPCof;      "FTPC";
    exe SVTTof;      "No SVT";
    exe PHMDof;      "Photon mult detector on";
    exe SISDof;      "No sisd";
    exe MUTD13;      "Muon telescope detector";
    exe PIPEv2;      "The beam pipe";

    exe IDSM02;      "Inner detector support";
    exe FGTDv32;     "FGT v3 6 disks";

    exe PXST01;      "PIXEL detector support version 1";
    exe PIXL05;      "Production version of the pixl detector";
    exe DTUB01;      "DTUB";
    exe PSUPof;      "Switch off pixel support";
]

""" Configuration 1 baseline, 2 without pixl """
REPLACE [exe y2013_1;] with ["Y2013 first cut"          ; exe Y2013; ];
REPLACE [exe y2013_2;] with ["Y2013 first cut sans PIXL"; exe Y2013;  PIXL=off; PXST=on; ];


REPLACE [exe y2013a;] with ["Y2013a first production geometry";
    exe CAVE05;       "Extended Cave and tunnel";
    exe MAGPv1;       "New magnet geometry";
    exe TPCE04r;      "agstar version of yf model with reduced Rmax";
    exe BTOFv8;       "time of flight";
    exe CALB02;       "updated bemc model";
    exe ECALv6;       "several bugfixes in eemc geometry";
    exe EMCUTS(eemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
    exe EMCUTS(bemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";

    exe BBCMon;      "beam beam counters";
    exe FPDM03;      "Latest version of FPD";
    exe VPDD07;      "Latest version of VPD";
    exe FTPCof;      "FTPC";
    exe SVTTof;      "No SVT";
    exe PHMDof;      "Photon mult detector on";
    exe SISDof;      "No sisd";
    exe MUTD13;      "Muon telescope detector";
    exe PIPEv3;      "Improved beam pipe";

    exe IDSM02;      "Inner detector support";
    exe FGTDv32;     "FGT v3 6 disks";

    exe PXST01;      "PIXEL detector support version 1";
    exe PIXL05;      "Production version of the pixl detector";
    exe DTUB01;      "DTUB";
    exe PSUPof;      "Switch off pixel support";
]

""" Configuration 1 baseline, 2 without pixl """
REPLACE [exe y2013_1a;] with ["Y2013 first cut"          ; exe Y2013a; ];
REPLACE [exe y2013_2a;] with ["Y2013 first cut sans PIXL"; exe Y2013a;  PIXL=off; PXST=on; ];

REPLACE [exe y2013b;]   with ["Y2013 production b";           exe y2013a;   exe PSUP01; ];
REPLACE [exe y2013_1b;] with ["Y2013 production b";           exe y2013_1a; exe PSUP01; ];
REPLACE [exe y2013_2b;] with ["Y2013 production b sans PIXL"; exe y2013_2a; exe PSUP01; PIXL=off; PSUP=off; ];

REPLACE [exe y2013c;]   with ["Y2013c   = y2013x   in SL14a"; exe y2013b;   exe CAVE05; exe TPCE31;];
REPLACE [exe y2013_1c;] with ["Y2013_1c = y2013_1x in SL14a"; exe y2013_1b; exe CAVE05; exe TPCE31;];
REPLACE [exe y2013_2c;] with ["Y2013_2c = y2013_2x in SL14a"; exe y2013_2b; exe CAVE05; exe TPCE31;];

REPLACE [exe y2013d;]   with ["Y2013d";   exe y2013c; exe TPCE05r;];
REPLACE [exe y2013_1d;] with ["Y2013_1d"; exe y2013d;];
REPLACE [exe y2013_2d;] with ["Y2013_2d, sans pixel"; exe y2013d; exe PSUP01; PIXL=off; PSUP=off; ];

REPLACE [exe y2013x;] with [                                      "Y2013 asymptotic";
    EXE y2013a;   "First production geometry";
    EXE TPCE31;   "version 3.1 of the TPC (increase deadzone and integration time)";
]
REPLACE [exe y2013_1x;] with [ "Y2013 asymptotic"; 
    EXE Y2013x; 
]
REPLACE [exe y2013_2x;] with [ "Y2013 asymptotic sans PIXL"; 
    EXE y2013x; PIXL=off; 
]


c ===============================================================================
c ===============================================================================
c ===============================================================================
REPLACE [exe y2014;] with ["Y2014 first cut geometry";
    exe FGTDof;      "switch off FGT";
    exe TPCE04r;     "agstar version of yf model with reduced Rmax";
    exe BTOFv8;      "time of flight";
    exe CALB02;      "updated bemc model";
    exe ECALv6;      "several bugfixes in eemc geometry";
    exe EMCUTS(eemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
    exe EMCUTS(bemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
    exe BBCMon;      "beam beam counters";
    exe FPDMof;      "Latest version of FPD";
    exe VPDD07;      "Latest version of VPD";
    exe FTPCof;      "no FTPC";
    exe SVTTof;      "No SVT";
    exe PHMDof;      "Photon mult detector off";
    exe MUTD14;      "Muon telescope detector";
    exe CAVE05;      "Extended Cave and tunnel";
    exe IDSM14;      "Inner detector support";
    exe SISD85;      "SSD version 7"
    exe PIPEv3;      "The small diameter beam pipe";
    exe ISTD02;      "IST version 2";
    exe PXST01;      "PIXEL detector support version 1";
    exe PIXL06;      "Full config of the pixl detector";
    exe DTUB01;      "DTUB";
    exe PSUP01;      "1st version of pixl supports";
]

REPLACE [exe y2014a;] with ["Y2014 first cut geometry";
    exe FGTDof;      "switch off FGT";
    exe TPCE31;      "agstar version of yf model with reduced Rmax";
    exe BTOFv8;      "time of flight";
    exe CALB02;      "updated bemc model";
    exe ECALv6;      "several bugfixes in eemc geometry";
    exe EMCUTS(eemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
    exe EMCUTS(bemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
    exe BBCMon;      "beam beam counters";
    exe FPDMof;      "Latest version of FPD";
    exe VPDD07;      "Latest version of VPD";
    exe FTPCof;      "no FTPC";
    exe SVTTof;      "No SVT";
    exe PHMDof;      "Photon mult detector off";
    exe MUTD14;      "Muon telescope detector";
    exe CAVE05;      "Extened Cave and tunnel";
    exe IDSM14;      "Inner detector support";
    exe SISD85;      "SSD version 7"
    exe PIPEv3;      "The small diameter beam pipe";
    exe ISTD02;      "IST version 2";
    exe PXST01;      "PIXEL detector support version 1";
    exe PIXL06;      "Full config of the pixl detector";
    exe DTUB01;      "DTUB";
    exe PSUP01;      "1st version of pixl supports";
]

REPLACE [exe y2014b;] with ["Y2014 production plus hcal prototype";
    exe y2014a;      "y2014a baseline";
    exe hcalv0;      "Prototype hcal";
]

REPLACE [exe y2014c;] with ["Y2014 production plus hcal prototype";
    exe y2014a;      "y2014a baseline";
    exe TPCE05r;      "agstar version of yf model with reduced Rmax";
    exe hcalv0;      "Prototype hcal";
]

REPLACE [exe y2015;] with ["Y2015 first cut geometry";
    exe FGTDof;      "switch off FGT";
    exe TPCE31;      "agstar version of yf model with reduced Rmax";
    exe BTOFv8;      "time of flight";
    exe CALB02;      "updated bemc model";
    exe ECALv6;      "several bugfixes in eemc geometry";
    exe EMCUTS(eemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
    exe EMCUTS(bemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
    exe BBCMon;      "beam beam counters";

    exe VPDD08;      "Latest version of VPD";
    exe FTPCof;      "no FTPC";
    exe SVTTof;      "No SVT";
    exe PHMDof;      "Photon mult detector off";
    exe MUTD14;      "Muon telescope detector";
    exe CAVE05;      "Cave and tunnel";
    exe IDSM14;      "Inner detector support";
    exe SISD85;      "SSD version 7"
    exe PIPEv3;      "The small diameter beam pipe";
    exe ISTD02;      "IST version 2";
    exe PXST01;      "PIXEL detector support version 1";
    exe PIXL06;      "Full config of the pixl detector";
    exe DTUB01;      "DTUB";
    exe PSUP01;      "1st version of pixl supports";
    exe FPDM04;      "FMS plus preshower";
]

REPLACE [exe y2015a;] with ["Y2015 production geometry";
    exe FGTDof;      "switch off FGT";
    exe TPCE31;      "agstar version of yf model with reduced Rmax";
    exe BTOFv8;      "time of flight";
    exe CALB02;      "updated bemc model";
    exe ECALv6;      "several bugfixes in eemc geometry";
    exe EMCUTS(eemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
    exe EMCUTS(bemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
    exe BBCMon;      "beam beam counters";

    exe VPDD08;      "Latest version of VPD";
    exe FTPCof;      "no FTPC";
    exe SVTTof;      "No SVT";
    exe PHMDof;      "Photon mult detector off";
    exe MUTD14;      "Muon telescope detector";
    exe CAVE05;      "Cave and tunnel";
    exe IDSM14;      "Inner detector support";
    exe SISD85;      "SSD version 7"
    exe PIPEv3;      "The small diameter beam pipe";
    exe ISTD02;      "IST version 2";
    exe PXST01;      "PIXEL detector support version 1";
    exe PIXL62;      "Full config of the pixl detector with Al cables";
    exe DTUB01;      "DTUB";
    exe PSUP01;      "1st version of pixl supports";
    exe FPDM04;      "FMS plus preshower";
]


REPLACE [exe dev2016;] with ["Y2016 development tag";

    exe TPCE31;      "agstar version of yf model with reduced Rmax";
    exe BTOFv8;      "time of flight";
    exe CALB02;      "updated bemc model";
    exe ECALv6;      "several bugfixes in eemc geometry";
    exe EMCUTS(eemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
    exe EMCUTS(bemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
    exe BBCMon;      "beam beam counters";
    exe FPDMof;      "FMS off";

    exe VPDD07;      "Latest version of VPD";
    exe FTPCof;      "no FTPC";
    exe SVTTof;      "No SVT";
    exe PHMDof;      "Photon mult detector off";
    exe MUTD14;      "Muon telescope detector";
    exe CAVE05;      "Cave and tunnel";
    exe IDSM14;      "Inner detector support";
    exe SISD85;      "SSD version 7"
    exe PIPEv3;      "The small diameter beam pipe";
    exe ISTD02;      "IST version 2";
    exe PXST01;      "PIXEL detector support version 1";
    exe PIXL06;      "Full config of the pixl detector";
    exe DTUB01;      "DTUB";
    exe PSUP01;      "1st version of pixl supports";
    exe FGTDv56;     "12 disk FGT";
    exe HCALv1;      "HCAL prototype";

];    

REPLACE [exe dev15a;] with ["Y2014 first cut geometry";
    exe y2014a;      "Baseline is y2014a";
    exe FPDM04;      "FMS plus preshower";
]

REPLACE [exe dev15b;] with ["Y2014 first cut geometry";
    exe y2014a;      "Baseline is y2014a";
    exe HCALv1;      "Naked HCAL";
]

REPLACE [exe dev2018;] with ["Y2018 development tag";

    exe TPCE31;      "agstar version of yf model with reduced Rmax";
    exe BTOFv8;      "time of flight";
    exe CALB02;      "updated bemc model";
    exe ECALv6;      "several bugfixes in eemc geometry";
    exe EMCUTS(eemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
    exe EMCUTS(bemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
    exe BBCMon;      "beam beam counters";
    exe FPDM03;      "Latest version of VPD";
    exe VPDD07;      "Latest version of VPD";
    exe FTPCof;      "no FTPC";
    exe SVTTof;      "No SVT";
    exe SISDof;      "No sisd";
    exe PHMDof;      "Photon mult detector off";
    exe MUTD14;      "Muon telescope detector";
    exe CAVE05;      "Cave and tunnel";
    exe PIPE12;      "The beam pipe";
*   exe IDSM14;      "Inner detector support";
*    exe SISD85;      "SSD version 7"
*    exe PIPEv3;      "The small diameter beam pipe";
*    exe ISTD02;      "IST version 2";
*    exe PXST01;      "PIXEL detector support version 1";
*    exe PIXL06;      "Full config of the pixl detector";
*    exe DTUB01;      "DTUB";
*    exe PSUP01;      "1st version of pixl supports";
*    exe FGTDv56;     "12 disk FGT";
    exe HCALv1;      "HCAL prototype";

];    

REPLACE [exe dev2020;] with ["DEV 2020 first cut";
     exe y2015a;
     exe FTSDv0;
];


REPLACE [exe COMPLETE;] with [ "Extrapolation of geometry to y2014."
    exe TPCE05r;     "agstar version of yf model with reduced Rmax";
    exe BTOF67;      "time of flight";
    exe CALB02;      "updated bemc model";
    exe ECALv6;      "several bugfixes in eemc geometry";
    exe EMCUTS(eemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
    exe EMCUTS(bemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
    exe BBCMon;      "beam beam counters";
    exe FPDM03;      "Latest version of FPD";
    exe VPDD07;      "Latest version of VPD";
    exe FTPCof;      "no FTPC";
    exe SVTTof;      "No SVT";
    exe PHMDof;      "Photon mult detector off";
    exe MUTD13;      "Muon telescope detector";
    exe CAVE05;      "Extened Cave and tunnel";
    exe IDSM02;      "Inner detector support";
    exe SISD85;      "SSD version 7"
    exe PIPEv3;      "The small diameter beam pipe";
    exe ISTD02;      "IST version 2";
    exe PXST01;      "PIXEL detector support version 1";
    exe PIXL06;      "Full config of the pixl detector";
    exe DTUB01;      "DTUB";
]



REPLACE [exe devE;] with ["DEVE eSTAR upgrade geometry";
    """Deprecated detectors"""
    exe FTPCof;      "FTPC";
    exe SVTTof;      "No SVT";
    exe PHMDof;      "Photon mult detector on";
    exe SISDof;      "No sisd";

    exe TPCE04r;     "agstar version of yf model with reduced Rmax";
    exe BTOF67;      "time of flight";
    exe CALB02;      "updated bemc model";
    exe ECALv6;      "several bugfixes in eemc geometry";
    exe EMCUTS(eemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
    exe EMCUTS(bemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
    exe BBCMon;      "beam beam counters";
    exe FPDM03;      "Latest version of FPD";
    exe VPDD07;      "Latest version of VPD";

    exe MUTD05;      "Muon telescope detector";
    exe CAVE04;      "Cave and tunnel";
    exe PIPE12;      "The beam pipe";

    exe IDSM02;      "Inner detector support";
    exe FGTDv55;     "Very forward FGT, 12 disks";

    exe PIXL02;      "Development version of the pixl detector";

    """Move the FMS N and S modules to an open position"""
       FpdmPosition=1;
       
    """Switch on and configure the FSC geometry"""
       FSCE=on;
       FsceConfig=1;

    """Activate the EIDD geometry"""
       EIDD=on; 
       EiddConfig=1; ]


Replace [exe devT;] with ["DEVT TPC Upgrade Geometry";
       exe devE;    "eSTAR baseline";
       exe Tpcx10;  "TPC upgrade geometry w/ default pads"; ]

Replace [exe devTA;] with ["DEVTA TPC Upgrade Geometry";
       exe y2014a;    "Y2014A upgrade geometry";
       exe TPCE05rA;   "A: TPC upgrade studies 32 inner pad rows 0.67 x 2.0"; ]
Replace [exe devTB;] with ["DEVTB TPC Upgrade Geometry";
       exe y2014a;    "Y2014A upgrade geometry";
       exe TPCE05rB;   "B : TPC upgrade studies 40 inner pad rows 0.67 x 1.6"; ]
Replace [exe devTC;] with ["DEVTC TPC Upgrade Geometry";
       exe y2014a;    "Y2014A upgrade geometry";
       exe TPCE05rC;   "C : TPC upgrade studies 40 inner pad rows 0.50 x 1.6"; ]
Replace [exe devTD;] with ["DEVTD TPC Upgrade Geometry";
       exe y2014a;    "Y2014A upgrade geometry";
       exe TPCE05rD;   "D : TPC upgrade studies 32 inner pad rows 0.50 x 2.0"; ]
Replace [exe devTE;] with ["DEVTE TPC Upgrade Geometry";
       exe y2014a;    "Y2014A upgrade geometry";
       exe TPCE05rE;   "E : TPC upgrade studies 50 inner pad rows 0.335 x 1.28"; ]
Replace [exe devTF;] with ["DEVTF TPC Upgrade Geometry";
       exe y2014a;    "Y2014A upgrade geometry";
       exe TPCE05rF;   "F : TPC upgrade studies 32 inner pad rows 0.400 x 2.00"; ]
Replace [exe devTX;] with ["DEVTX TPC Upgrade Geometry";
       exe y2014a;    "Y2014A upgrade geometry";
       exe TPCE05rX;   "X : TPC upgrade studies 40 inner pad rows 0.500 x 1.60"; ]
Replace [exe devTY;] with ["DEVTX TPC Upgrade Geometry";
       exe y2014a;    "Y2014A upgrade geometry";
       exe TPCE05rY;   "Y : TPC upgrade studies 40 inner pad rows 0.500 x 1.60"; ]

Replace [exe devTZ;] with ["DEVTZ TPC Upgrade Geometry: devTY with no HFT";
       exe dev2018;    "dev2018 upgrade geometry";
       exe TPCE05rY;   "Y : TPC upgrade studies 40 inner pad rows 0.500 x 1.60"; ]

** Aliases to devE and devT geometries to work around the field bug **
Replace [exe estar1;] with [exe devE;]
Replace [exe TPCIv1;] with [exe devT;]


""" Setup eStar2 geometry """
Replace [exe eStar2;] with [
    """Deprecated detectors"""
    exe FTPCof;      "FTPC";
    exe SVTTof;      "No SVT";
    exe PHMDof;      "Photon mult detector on";
    exe SISDof;      "No sisd";

    exe TPCX16; 
    exe BTOF67;      "time of flight";
    exe CALB02;      "updated bemc model";
    exe ECALv6;      "several bugfixes in eemc geometry";
    exe EMCUTS(eemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
    exe EMCUTS(bemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
    exe BBCMon;      "beam beam counters";
    exe FPDM03;      "Latest version of FPD";
    exe VPDD07;      "Latest version of VPD";

    exe MUTD13;      "Muon telescope detector";
    exe CAVE04;      "Cave and tunnel";
    exe PIPE12;      "The beam pipe";

    exe IDSM02;      "Inner detector support";
    exe FGTDv55;     "Very forward FGT, 12 disks";

    """Move the FMS N and S modules to an open position"""
       FpdmPosition=1;
       
    """Switch on and configure the FSC geometry"""
       FSCE=on;
       FsceConfig=1;

    """Activate the EIDD geometry"""
       EIDD=on; 
       EiddConfig=1; ]



!//______________________________________________________________________________
replace [exe UPGR15;] with ["New Tracking: HFT+IST+TPC+SSD-SVT"
                            exe SVTTof; exe FTPCof; exe BTOF66; exe CALBc0; exe ECAL31;
                            exe BBCMon; exe FPDM00; exe MFLD54;
                            DensConfig = 1; "gas density correction"
                            SupoConfig = 1; "FTPC Support"
                            exe PHMD01; exe SISD65; exe PIPE00; exe PIXL00; exe ISTB00;
                            exe FSTDof; exe FGTD02; 
"* On Gerrit request, we disable the cone:"
                            exe ITSPof; "prototype of the Inner Tracker SuPport structure"]

!//______________________________________________________________________________
replace [exe UPGR16;] with ["New Tracking: HFT+IST+TPC+SSD-SVT"
                     SVTT=off; "no SVT  at all in this configuration"
                     ftpc=off; "no FTPC at all in this configuration"
                     SCON=off;
                     ConeConfig=2 " new cable weight estimate ";

* X.Dong
                 "ctb: central trigger barrer             ";
                     Itof=6 " CONSTRUCT btofgeo6 ";
* NEW CONFIG!
                     tofX0= 0.00;
                     tofZ0=-0.50;
                     BtofConfig=6;

                  "CALB"
                     emsEdit=on
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                     CalbConfig = 2
                  "ECAL"
                     EcalConfig=1   " west wheel "
                     ecalFill=3     " all sectors filled "
                  "beam-beam counter "
                     BBCM=on
                  "forward pion detector "
                     FPDM=on
                  "field version "
                     MfldConfig=4;      "tabulated field, with correction "

                     SvshConfig = 0; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 0;

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 1;
                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 65;
* careful! Achtung!
                   PipeConfig=6;   " thinner pipe"
                   PipeFlag = 0;    "no wrap,no shild"

                   PIXL=off;        " put the pixel detector in"
                   PixlConfig=-1;   " Simplest.Gerrit"

                   ISTB=on;  "IST barrel"
                   IstbConfig=-1;

                   FSTD=off;  "no pixel based forward tracker in this tag"
                   FstdConfig=0;

* Forward STAR tracker disk
                   FGTD=on;  "GEM forward tracker"
                   FgtdConfig=3;
* On Gerrit request, we disable the cone:
                   ITSP=off; "prototype of the Inner Tracker SuPport structure"
                  "New version of the TPC backplane "
                     TpceConfig = 3;
                  "We need an even bigger Cave"
                     CaveConfig = 4;
]
!//______________________________________________________________________________
replace [exe UPGR16a;] with ["upgr16 +tpc2009"
			      exe upgr16;exe TPCE04;]

!//______________________________________________________________________________
replace [exe UPGR22;] with ["upgr16a + fhcm01"
			      exe upgr16a;exe FHCM01;]



*********HELP***HELP***HELP***HELP***HELP***HELP***HELP***HELP***HELP***
* ORT = XYZ ! (90,  0, 90, 90,  0,  0)
* ORT = YZX ! (90, 90,  0,  0, 90,  0)
* ORT = ZXY ! ( 0,  0, 90,  0, 90, 90)
*********HELP***HELP***HELP***HELP***HELP***HELP***HELP***HELP***HELP***
***************************************************************************
   module geometry is the main steering module for the STAR geometry
   author  Pavel Nevski
   Created August 1998
*                                                                         *
*  Update history:                                                        *
*  08/19/98, PN: tof is not part of year_2a                               *
*  12/04/98, PN: RICH  + upstream part + zero degree calo                 *
*  09/26/99, E.Cains: 1H geometry added - one svt ladder at layer 3       *
*  01/27/99, PN: RICH in 1H geometry is simulated with hits is quartz & fr*
*  05/22/01, PN: starting with tag y2000 field is version 3 (direct map)  *
*  09/30/03, MP: see the many new CVS comments about recent check-ins     *
*  09/30/03, MP: converted the sub into a MODULE to allow for ZEBRA access*
***************************************************************************

   Integer stop_on_fail / 1 / ! Full stop if missing modules

   Structure  GDAT {real mfscale, char gtag(2)}

   Integer CsADDR, address/0/, nloaded/0/, nfailed/0/

* list of system on/off switches:
   Logical    CAVE,PIPE,SVTT,SISD,TPCE,FTPC,
              BTOF,VPDD,MAGP,CALB,ECAL,UPST,
              RICH,ZCAL,MFLD,BBCM,FPDM,PHMD,
              PIXL,ISTB,GEMB,FSTD,FTRO,FGTD,
              SHLD,QUAD,MUTD,IGTD,HPDT,ITSP,
              DUMM,SCON,IDSM,FSCE,EIDD,ISTD,
              PXST,PSUP,HCAL,FTSD


* Qualifiers:  TPC        TOF         etc
   Logical    emsEdit,svtWater,
              on/.true./,off/.false./

   Logical    verbose/.false./


*  Codes:
*  1 - full ctb,         2 - full TOFp based tof,   3 - partial TOFp based tof,
*  4 - single TOFp tray, 5 - one TOFp and one TOFr, 6 - full TOFr based tof.
* X.Dong - global parameters for TOF trays
   real       tofX0, tofZ0

   real       Par(1000),myArg,magField,dcay(5),shift(2),svtWaferDim

   Integer    LENOCC,ICFNBL,ICFMUL,JL,JR,LL,
              IPRIN,nSvtLayer,nSvt1stLayer,i,jGotCom,l,
              kgeom,nmod(2),nonf(3),ecalFill,
              sisd_level,
              Nleft,Mleft,richConfig,richPos,nSvtVafer,Itof,mwx

   Real       magX(3) /0,0,0/,magB(3)
***************** historical note: *********************8
* CorrNum allows us to control incremental bug fixes in a more
* organized manner -- ! Obsoleted 20050324 maxim! --


* The following are the versioning flags:

   Integer    DensConfig, SvttConfig, BtofConfig, VpddConfig, FpdmConfig, SisdConfig, PipeConfig,
              CalbConfig, PixlConfig, IstbConfig, GembConfig, FstdConfig, FtroConfig, ConeConfig,
              FgtdConfig, TpceConfig, PhmdConfig, SvshConfig, SupoConfig, FtpcConfig, CaveConfig,
              ShldConfig, QuadConfig, MutdConfig, HpdtConfig, IgtdConfig, MfldConfig, EcalConfig,
              FhcmConfig, RmaxConfig, IdsmConfig, FsceConfig, EiddConfig, TpcxConfig, TpadConfig,
              IstdConfig, PxstConfig, MagpConfig, HcalConfig, FtsdConfig

   Integer                                        VpddModule/0/

   Integer    FpdmPosition / 0 /

* The following flags select different base geometry files for the endcap
   Integer    EcalGeometry / 6 /            ! defaults to version 5

* Selects the configuration for EM cuts in the EEMC (ecalgeo6) and BEMC (calbgeo2)
*
*   0 = cuts defined in geometry files
*   1 =  10 keV cuts
*   2 =  30 keV cuts
*   3 = 100 keV cuts
*   4 =   1 MeV cuts

   Integer    BEmcCutConfig / 1 /             ! defaults to 10 keV cuts
   Integer    EEmcCutConfig / 1 /



   Integer    pipeFlag

*             DensConfig, ! TPC gas density correction
*             RmaxConfig, ! TPC envelope max radius in tpcgeo3
*             SvttConfig, ! SVTT version
*             BtofConfig, ! BTOF trays
*             VpddConfig, ! VPDD
*             FpdmConfig, ! Forfward Pion Mult detectoe
*             SisdConfig, ! SSD
*             PipeConfig, ! Beam Pipe
*             CalbConfig, ! Barrel EMC
*             PixlConfig, ! Inner Pixel detector
*             HpdtConfig, ! Heavy Flavor Tracker
*             IstbConfig, ! Integrated Silicon Tracker
*             PxstConfig, ! PST
*             IstdConfig, ! Integrated Silicon Tracker
*             GembConfig, ! Inner GEM barrel tracker
*             IgtdConfig, ! GEM disks
*             FstdConfig, ! Forward Silicon tracker Disks
*             FtroConfig, ! FTPC Readout Electronics
*             ConeConfig, ! SVTT support cones and cables
*             FgtdConfig, ! Forward GEM tracker
*             TpceConfig, ! TPC
*             PhmdConfig  ! Photon Multiplicity Detector
*             SvshConfig  ! SVT Shield
*             SupoConfig  ! FTPC support
*             FtpcConfig  ! FTPC
*             ShldConfig  ! Beam shield
*             QuadConfig  ! All magnets from D0 and up
*             MutdConfig  ! Muon Trigger System
*	      FhcmConfig  ! Forward Hadron Detector
* Note that SisdConfig can take values in the tens, for example 20
* We do this to not proliferate additional version flags -- there has
* been a correction which resulted in new code.. We check the value
* and divide by 10 if necessary.

   character  Commands*4000
   character  Geom    *8
   data       Geom/'unknown '/


* - - - - - - - - - - - - - - - - -
+CDE,GCBANK,GCUNIT,GCPHYS,GCCUTS,GCFLAG,AGCKINE,QUEST.
*  temporarely until GCTLIT is not part of GCTMED:
   Integer        Thrind   ,Jmin,ItCkov,ImCkov,NpCkov
   common/GCTLIT/ Thrind(4),Jmin,ItCkov,ImCkov,NpCkov
* - - - - - - - - - - - - - - - - -

replace[;Case#{#;] with [
  IF ((JL.LT.JR) .and. (Commands(JL:JR) .eq. '#1')) {;
    myArg=0; jGotCom=2009;
    if (Commands(JR+1:JR+1).eq.'=') then;
      i = ((JR+1+3+4)/4); myArg = par(i+1);
      Commands(JR+1:i*4)=' ';
    endif
    JL = ICFNBL(Commands,JR+1,LL);
    JR = ICFMUL(' =',Commands,JL,LL)-1;
    <W>; (' #1: #2');
;]

* If geometry was already built, the local DB will be dropped completely now
* but the request for the next geometry should be saved in a temp. par arrray
   call ASLGETBA ('GEOM','DETP',1000,LL,Par); LL = (LL-1)*4;
   If (JVOLUM>0) call AGDROP ('*')

* -------------------- set GSTAR absolute default ------------------------
* before parsing the request, set some default values:
   IPRIN    = IDEBUG
   NtrSubEv = 1000     " automatic !"


   BtofConfig  = 1 ! ctb only
   CalbConfig  = 0 ! really make use of it starting in y2004
   CaveConfig  = 1 ! custom for shielding studies=2, wider for muon detector=3, and longer=4
   ConeConfig  = 1 ! 1 (def) old version, 2=more copper
   DensConfig  = 0 ! gas density correction
   RmaxConfig  = 0 ! tpcegeo3 rmax
   FgtdConfig  = 306 ! version
   IdsmConfig  = 1 ! version
   FpdmConfig  = 0 ! 0 means the original source code
   HcalConfig  = 0 ! 0 means HcalGeo, 1 HcalGeo1, etc...
   FtsdConfig  = 0 ! 0 means FtsdGeo
   FstdConfig  = 0 ! 0=no, >1=version
   FtroConfig  = 0 ! 0=no, >1=version
   FtpcConfig  = 0 ! 0  version, 1=gas correction
   HpdtConfig  = 0 ! 0=no, >1=version
   IstbConfig  = 0 ! 0=no, >1=version
   IstdConfig  = 0 ! 0=no, >1=version
   IgtdConfig  = 1 ! 1=old radii etc, 2=new ones
   GembConfig  = 0 ! 0=no, >1=version
   MutdConfig  = 0 ! same
   PhmdConfig  = 0 ! No Photon multiplicity detectorby default
   PipeConfig  = 2 ! Default, Be pipe used in most of the runs =<2003
   PixlConfig  = 0 ! 0=no, 1=inside the SVT, 2=inside CAVE, 3=with pipe support
   QuadConfig  = 0 ! No D0 and quads by default
   ShldConfig  = 0 ! No Beam Shield by default
   SisdConfig  = 0 ! No Silicon strip by default
   SupoConfig  = 0 ! 0 (def) old buggy version, 1=correction
   SvshConfig  = 0 ! SVTT shield version
   SvttConfig  = 0 ! SVTT version
   TpceConfig  = 1 ! 1 (def) old version, 2=more structures in the backplane
   VpddConfig  = 1 ! vpd...
   FhcmConfig  = 0 ! Forward Hadron Detector off by default
   FsceConfig  = 0 ! Forward Sphagettoni Calorimeter off by default
   EiddConfig  = 0 ! EAst side, Trd, Tof and Calo for eSTAR studies
   TpcxConfig  = 0 ! Tpc Experimental 
   TpadConfig  = 0 ! Tpad configuration
   MagpConfig  = 1 ! Default magnet configuration

   pipeFlag = 3 ! pipe wrap + svt shield

* Set only flags for the main configuration (everthing on, except for tof),
* but no actual parameters (CUTS,Processes,MODES) are set or modified here.
* If an empty or no DETP GEOM was issued, geometry is defined externally.

   magField=5               " default"

* "Canonical" detectors are all ON by default,

   {CAVE,PIPE,SVTT,TPCE,FTPC,BTOF,VPDD,CALB,ECAL,MAGP,MFLD,UPST,ZCAL} = on;
* whereas some newer stuff is considered optional:
   {BBCM,FPDM,PHMD,PIXL,
    ISTB,GEMB,FSTD,SISD,
    FTRO,FGTD,SHLD,QUAD,
    MUTD,IGTD,HPDT,ITSP,
    DUMM,SCON,IDSM,FSCE,
    EIDD,ISTD,PXST,HCAL,
    FTSD}=off;

   {emsEdit,RICH}=off        " TimeOfFlight, EM calorimeter Sector            "
   nSvtLayer=7; nSvtVafer=0;  svtWaferDim=0; " SVT+SSD, wafer number and width as in code     "
   nSvt1stLayer=1;           " the innermost layer of SVT                     "
   svtWater=on               " water+water manifold in svt, off for Y2000 only"
   mwx=2                 " for Year_1? mwx=1 limites x in mwc hits (<Y2K) "
   Itof=2                " use btofgeo2 - default starting from Y2000     "
   richConfig=2          " add non-sensitive hits to RICH system          "
   richPos=2             " real RICH position and spectra inst.of nominal "
   nonf={1,2,2}          " ECAL on right side, FPD parts on left side     "
   EcalConfig=0          " Ecal: east, west or both                       "
   EcalFill=3            " full wheel by default                          "
   MfldConfig=2                  " default field - symmetrical, as fitted by Bill "
   Commands=' ';


* -------------------- select USERS configuration ------------------------
* On a non-empty DETP GEOM every keyword results in an action and is erased.
*
* Actions consist here of selecting the appropriate parameteres and flags.
* This flags are used in the next section to create subsystems and
* to communicate DETP commands with parameters to them.
*

If LL>0
{ Call AGSFLAG  ('GEOM',1)
* convert input line into a string of upprecase characters
  CALL UHTOC(PAR(2),4,Commands,LL);  Call CLTOU(Commands);
  do JL=1,LL {if (ichar(Commands(JL:JL)).lt.ichar(' ')) Commands(JL:JL)=' ';}
  JL = ICFNBL(Commands, 1,LL);
  JR = ICFMUL(' =',Commands,JL,LL)-1;


  if (CUTNEU .gt. 0.009) then ! BIG
* set geant processes and cuts only if any detp geometry was issued:
  {CUTGAM,CUTELE,CUTNEU,CUTHAD,CUTMUO,BCUTE,BCUTM,DCUTE,DCUTM,PPCUTM} =.001;
  {IDCAY,IANNI,IBREM,ICOMP,IHADR,IMUNU,IPAIR,IPHOT,ILOSS,IDRAY,IMULS} = 1;
  {IRAYL,ISTRA} = 0;
  TOFMAX        = 1.e-4
  endif
*
  for(jGotCom=1;jGotCom>0;) { jGotCom=0;
  Case HELP       { you may select the following (incomplete) list of keywords: ;
                  <W>;('---------------:----------------------------- ');
                  <W>;('Configurations : complete,tpc_only,field_only ');
                  <W>;('               : year_2a                      ');
                  <W>;('               : year2000, year2001,year2002  ');
                  <W>;('               : year2003, y2003a             ');
                  <W>;('Gcalor         : Gcalor_on, Gcalor_off        ');
                  <W>;('Geant Physics  : Hadr_on, Hadr_off            ');
                  <W>;('Geant Physics  : Phys_off, Decay_Only         ');
                  <W>;('Geometry Detail: mwc_off, pse_off, 4th_off    ');
                  <W>;('Magnetic Field : Field_on/off, field=value    ');
                  <W>;('Auxillary keys : Debug_on/off, Split_on/off   ');
                  <W>;('--------------------------------------------- ');
                  <W>;('Default: complete STAR with hadr_on,auto-split');
                  <W>;('--------------------------------------------- ');
                  <W>;('EEMC/BEMC Cuts : EMC_10keV,  EMC_30keV,       ');
                  <W>;('               : EMC_100keV, EMC_1MeV         ');
                  <W>;('--------------------------------------------- ');
                }

  Case YEAR_2B    { old 2001 geometry first guess - TPC+CTB+FTPC+RICH+CaloPatch+SVT;
                  Geom='YEAR_2B ';
                  BtofConfig=4;
                  {RICH,emsEdit}=on;  nmod={24,0}; shift={21,0};
                  nonf={0,2,2};  Itof=2;  richConfig=2;                        nSvtLayer=6; }

  Case YEAR_2A    { old asymptotic STAR;    Itof=1; mwx=1;  BBCM=on;  Geom='YEAR_2A ';      }

*************************************************************************************************************
* as complete as Y2003X below but with all corrections AND pixel detector
*************************************************************************************************************
  Case COMPLETE  { New Complete + correction 3 in 2003 geometry: TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL+PHMD;
                 exe complete;
                 geom = 'complete';
               }
****************************************************************************************
* corrected: MWC readout, RICH reconstructed position, no TOF
  Case YEAR2000   { actual 2000:  TPC+CTB+RICH+caloPatch+svtLadder;
                  exe y2000; }
  Case Y2000      { actual 2000:  TPC+CTB+RICH+caloPatch+svtLadder;
                  Geom = 'Y2000   ';
                  exe y2000; }

  Case YEAR2001   { 2001 geometry - TPC+CTB+FTPC+RICH+CaloPatch+SVT+FPD;
                  Geom = 'Y2001   ';
                  exe y2001; }
  Case Y2001      { 2001 geometry - TPC+CTB+FTPC+RICH+CaloPatch+SVT+FPD;
                  Geom = 'Y2001   ';
                  exe y2001; }

****************************************************************************************
  Case YEAR2002   { january 2002 geometry - TPC+CTB+FTPC+CaloPatch2+Rich+SVT3+BBC+FPD;
                  Geom = 'Y2002   ';
                  exe y2002;
                }
  Case Y2002      { january 2002 geometry - TPC+CTB+FTPC+CaloPatch2+Rich+SVT3+BBC+FPD;
                  Geom = 'Y2002   ';
                  exe y2002;
                }
****************************************************************************************
  Case YEAR2003   { draft 2003 geometry - TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL;
                  Geom = 'Y2003   ';
                  exe y2003;
                }

****************************************************************************************
  Case Y2003      { draft 2003 geometry - TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL;
                  Geom = 'Y2003   ';
                  exe y2003;
                }

***********************************************************************
* In y2003a:
*    removed serious bugs from SUPOGEO (incorrect positioning inside the SVT,
*    where is doesn't belong)
*    corrected CALB -- the shift variable (was 0,0 so the barrel wasn't tilted right)
*    corrected SVT  -- the layer radii (from 250 to 150 microns, see the svt code)
****************************************************************************************
  Case Y2003A    { correction 1 in 2003 geometry - TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL;
                  Geom = 'Y2003a  ';
                 exe y2003a; }
***********************************************************************
* y2003b is y2003a, but with the extra material in the SVT
* This is actually an important case (i.e. the "most precise" geometry
* approximation for the early 2003 run) which we were lacking so far.
* This is achieved by setting CorrNum to 2.
* The endcap EMC has one third of one wheel, as before
* For more info on the extra material in SVT -- see web page
****************************************************************************************
  Case Y2003B    { correction 2 in 2003 geometry: TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL;
                  Geom = 'Y2003b  ';
                 exe y2003b;
               }
****************************************************************************************
  Case Y2003C    { Better SVT model on top of 2003B: TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL;
                  Geom = 'Y2003c  ';
                 exe y2003c;
               }

****************************************************************************************
  Case Y2003X    { same as y2003b but with full calorimeters and PHMD
                  Geom = 'Y2003x  ';
                 exe y2003x;
               }

*
*
**********************************************************************
* Corrections and enhancements in y2004:
*    added the Photon Multiplicity Detector (PHMD)
*    The endcap EMC has one complete wheel in the west
*    To be done: 3/4 of the second half of the barrel!
*
*                >>>THIS IS THE MASTER GEOMETRY FOR THE SPRING 04<<<
*
****************************************************************************************
  Case Y2004     { baseline 2004 geometry: TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL+PHMD with low cuts GSTPAR in PHMD;
                  Geom = 'Y2004x  ';
                 exe y2004;
               }

****************************************************************************************
  Case Y2004A    { baseline 2004 geometry: TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL+PHMD with standard GSTPAR in PHMD;
                  Geom = 'Y2004a  ';
                 exe y2004a; }

*
****************************************************************************************
  Case Y2004B    { corrected 2004 geometry: TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL+PHMD with standard GSTPAR in PHMD;
                  Geom = 'Y2004b  ';
                 exe y2004b; }

****************************************************************************************
  Case Y2004C    { same as Y2004B but with the SVT chip correction+cone+better SSD+TPC backplane+FTRO
                  Geom = 'Y2004c  ';
                 exe y2004c; }
****************************************************************************************
  Case Y2004D    { Better SVT on top of Y2004B
                  Geom = 'Y2004d  ';
                 exe y2004d; }
****************************************************************************************

  Case Y2004X    { hypothetical 2004 geometry: full barrel. Standard cuts in PHMD.;
                  Geom = 'Y2004x  ';
                 exe y2004x;
               }

****************************************************************************************
  Case Y2004Y    { same as Y2004X but with the SVT chip correction+cone+better SSD+TPC backplane+FTRO
                  Geom = 'Y2004y  ';
                 exe y2004y;
                }

****************************************************************************************
  Case Y2005    { first cut of 2005 geometry: TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL+PHMD_FTRO;
                  Geom = 'Y2005   ';
                exe y2005;
              }
****************************************************************************************
  Case Y2005X    { first cut of full CALB 2005 geometry: TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL+PHMD_FTRO;
                  "svt: 3 layers ";
                     nSvtLayer=6  " 3 bi-plane layers, nSvtLayer<=7 ";
                     nSvtVafer=0  " numbering is in the code   ";
                     svtWaferDim=0  " width is in the code      ";

                     ConeConfig=2 " new cable weight estimate ";

                  "ctb: central trigger barrer             ";
                     Itof=2 " CONSTRUCT btofgeo2 ";
* note the upgrade with respect to previous years:
                     BtofConfig=7;

* note the full barrel same as in y2003x:
                  "CALB"
                     emsEdit=on ;
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"

                  "ECAL"
                     EcalConfig=1   " one ECAL patch, west "
                     ecalFill=3     " all sectors filled "

                  "beam-beam counter "
                     BBCM=on

                  "forward pion detector "
                     FPDM=on
                     FpdmConfig  = 1 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     VPDD=on;
                     VpddConfig=4;

                  "field version "
                     MfldConfig=4;      "tabulated field, with correction "

*                    -- Obsoleted CorrNum = 3;
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 3; "SVTT version"
                     DensConfig = 1; "gas density correction"

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 23; "second version, full barrel"


                  "FTPC Readout barrel "
                     FTRO=on;
                     FtroConfig = 1;

                  Geom = 'Y2005x  ';

                }

****************************************************************************************
  Case Y2005B    { TPC,FTPC,SVT and SSD correction of 2005 geometry
                  Geom = 'Y2005b  ';
                exe y2005b;
               }

****************************************************************************************
  Case Y2005C    { TPC,FTPC,SVT and SSD correction of 2005 geometry
                  Geom = 'Y2005c  ';
                exe y2005c;
               }

****************************************************************************************
  Case Y2005D    { Better SVT on top of Y2005C
                  Geom = 'Y2005d  ';
                exe y2005d;
               }

****************************************************************************************
  Case Y2005E    { Better SVT, bigger shield and SSD on top of Y2005C, and full barrel calorimeter
                  Geom = 'Y2005e  ';
                 exe y2005e;
               }

****************************************************************************************
  Case Y2005F    { Y2005E + corrected SSD with gaps and dead area
                  Geom = 'Y2005f  ';
                 exe y2005f;
	       }
****************************************************************************************
  Case Y2005G    { Y2005F + corrected corrected SVT dead volumes from Rene
                  Geom = 'Y2005g  ';
                 exe y2005g;
               }
****************************************************************************************
  Case Y2005h    { Y2005g + TPC from y2009
                  Geom = 'Y2005h  ';
                 exe y2005h;
               }

  Case y2005i { Y2005h + updated endcap and lowered thresholds in E/BEMC
                Geom = 'Y2005i  ';
                exe y2005i;
              }
****************************************************************************************
****************************************************************************************
  Case Y2006    { y2006 baseline which is Y2005D+fixed TPC backplane+New SSD
                  Geom = 'Y2006   ';
                exe y2006;
              }

  Case Y2006A   { y2006 baseline which is Y2005D+fixed TPC backplane+New SSD
                  Geom = 'Y2006a  ';
                exe y2006a;
              }

  Case Y2006B   { Y2006A + improved SSD with dead area + improved CALB
                  Geom = 'Y2006b  ';
                  exe y2006b;
                }


  Case Y2006C   { Y2006B without the PHMD
                  Geom = 'Y2006c  ';
                  exe y2006c;
                }

  Case Y2006G   { Y2006C + new SVT dead material
                  Geom = 'Y2006g  ';
                  exe y2006g;
                }

  Case y2006h { Y2006h development: y2006g + version 6.1 of the endcap (not yet ready for production)
                  Geom = 'Y2006h  ';
                  exe y2006h;
                }

****************************************************************************************
****************************************************************************************
  Case Y2007    { y2006 baseline which is Y2006+FMS
                  Geom = 'Y2007   ';
                  exe y2007;
	        }
****************************************************************************************
  Case Y2007A   { y2007 (see below) but with corrected SVT (carbon instead of Be water channels)
                  Geom = 'Y2007a  ';
                  exe y2007a;
                }
****************************************************************************************
  Case Y2007G   { y2007A + dead material from Rene
                  Geom = 'Y2007g  ';
                  exe y2007g;
                }
****************************************************************************************
  Case Y2007H   { y2007g + TPC 
                  Geom = 'Y2007h  ';
                  exe y2007h;
                }
****************************************************************************************
****************************************************************************************
  Case Y2008    { y2008 baseline: no SVT,  cones,beam support,FTPC in CAVE now
                  Geom = 'Y2008   ';
                  exe y2008; 
		}
****************************************************************************************
  Case Y2008a   { y2008 + SCONE
                  Geom = 'Y2008a  ';
                exe y2008a;
	        }

  Case Y2008b   { y2008 + SCONE + calorimeter fixes
                  Geom = 'Y2008b  ';
                exe y2008b;
	        }

  Case Y2008c   { "2008c production: y2008b geometry with fixes for TOF response.";
                  Geom = 'Y2008c  ';
                  exe y2008c;}

  Case Y2008d   { "Y2008 production tag D: Improved SROD description in support cone";
                  Geom = 'Y2008d  ';
                  exe y2008d;}

  Case Y2008e   {"Y2008 production tag E: Same as D but with lowered EM cuts in the EEMC and BEMC";
                  Geom = 'Y2008e  ';
                  exe y2008e;}

****************************************************************************************
  Case Y2009   { y2009 initial geometry: more detailed TPC
                 Geom = 'Y2009   ';
                 exe y2009;}

  Case Y2009a   { y2009a baseline: more detailed TPC, version 6.1 of the endcap geometry
                  Geom = 'Y2009a  ';
                  exe y2009a;}

  Case Y2009b   { "2009b production: y2009a geometry with 'old' tracking thresholds in the EEMC.  Not suitable for EEMC simulations.";
                  Geom = 'Y2009b  ';
                  exe y2009b;}

  Case Y2009c   { "2009c production: y2009a geometry with fixes for TOF response.";
                  Geom = 'Y2009c  ';
                  exe y2009c;}

  Case Y2009d    {   "y2009d production tag D: Improved SROD description in support cone";
                   Geom = 'Y2009d';
                   exe y2009d;
                 }

****************************************************************************************
  Case Y2010   { y2010: baseline
                 Geom = 'Y2010   ';
                 exe y2010;         }

  Case Y2010A  { Y2010a: production tag A
                 Geom = 'Y2010A  ';
                 exe y2010a;       }

  Case Y2010b  { "Y2010a: production tag B with fixes for TOF response";
                 Geom = 'Y2010b  ';
                 exe y2010b;       }

  Case Y2010c  { "Y2010 production tag C: Improved SROD description in support cone";
                 Geom = 'Y2010c  ';
                 exe y2010c;       }

****************************************************************************************

  Case y2011   { Y2011: baseline y2011 geometry, placeholder added 07/30/2010
                 Geom = 'Y2011   ';
                 exe y2011; }

  Case y2011a   { Y2011a: Production quality tag
                 Geom = 'Y2011a  ';
                 exe y2011a; }

  Case y2011b   { Y2011b: Production quality tag
                 Geom = 'Y2011b  ';
                 exe y2011b; }


  Case upgr2012 { y2012 : Y2012 geometry first cut;
                 Geom = 'y2012   ';
                 exe y2012; }

  Case y2012 { y2012 : Y2012 geometry first cut;
                 Geom = 'y2012   ';
                 exe y2012; }

  Case y2012a { y2012a : Y2012a production geometry tag;
                 Geom = 'y2012a  ';
                 exe y2012a; }
  Case y2012b { y2012b : Y2012b production geometry tag;
                 Geom = 'y2012b  ';
                 exe y2012b; }

  Case y2012c { y2012c : Y2012c production geometry tag;
                 Geom = 'y2012c  ';
                 exe y2012c; }

  Case dev13 { dev13 : y2013 studies;
                 Geom = 'dev13   ';
                 exe dev13; }

  Case y2013 { y2013 : y2013 first cut geometry;
                 Geom = 'y2013   ';
                 exe y2013; }

  Case y2013x { y2013x : y2013 asymptotic;
                  Geom = 'y2013x  ';
                  exe y2013x; }

  Case Y2013_1 { Y2013_1 : Y2013 baseline; 
                  Geom = 'y2013_1 ';
                  exe y2013_1; }

  Case Y2013_2 { Y2013_2 : Y2013 baseline sans PIXL; 
                  Geom = 'y2013_2; ';
                  exe y2013_2; }

  Case Y2013_1x { Y2013_1x : Y2013 asymptotic; 
                  Geom = 'y2013_1x';
                  exe y2013_1x; }

  Case Y2013_2x { Y2013_2x : Y2013 asymptotic sans PIXL; 
                  Geom = 'y2013_2x  '; 
                  exe y2013_2x; }

  Case Y2013a   { Y2013a   : Y2013 1st prod geometry w/  PIXL; 
                  Geom = 'y2013a    '; 
                  exe y2013a;   }
  Case Y2013_1a { Y2013_1a : Y2013 1st prod geometry w/  PIXL; 
                  Geom = 'y2013_1a  '; 
                  exe y2013_1a;   }
  Case Y2013_2a { Y2013_2a : Y2013 1st prod geometry w/o PIXL; 
                  Geom = 'y2013_2a  '; 
                  exe y2013_2a;   }
  Case Y2013b   { Y2013b   : Y2013 2nd prod geometry w/  PIXL; 
                  Geom = 'y2013b    '; 
                  exe y2013b;   }
  Case Y2013_1b { Y2013_1b : Y2013 2nd prod geometry w/  PIXL; 
                  Geom = 'y2013_1b  '; 
                  exe y2013_1b;   }
  Case Y2013_2b { Y2013_2b : Y2013 2nd prod geometry w/o PIXL; 
                  Geom = 'y2013_2b  '; 
                  exe y2013_2b;   }

  Case Y2013c   { Y2013c equals Y2013x in library SL14a;                 
                  Geom = 'y2013c    ';
                  exe y2013c; }

  Case Y2013_1c { Y2013_1c equals Y2013_1x in library SL14a;
                  Geom = 'y2013_1c   ';
                  exe y2013_1c; }

  Case Y2013_2c { Y2013_2c equals Y2013_2x in library SL14a;
                  Geom = 'y2013_2c   ';
                  exe y2013_2c; }
                                      
  Case Y2013d   { Y2013d equals Y2013x in library SL14a;                 
                  Geom = 'y2013d    ';
                  exe y2013d; }

  Case Y2013_1d { Y2013_1d;
                  Geom = 'y2013_1d   ';
                  exe y2013_1d; }

  Case Y2013_2d { Y2013_2d;
                  Geom = 'y2013_2d   ';
                  exe y2013_2d; }
                                      

  Case y2014    { y2014 : y2014 first cut;                     
                  Geom = 'y2014     '; exe y2014; }

  Case y2014a   { y2014a : y2014 production level;              
                  Geom = 'y2014a    '; exe y2014a; }

  Case y2014b   { y2014b : y2014a plus hcal prototype;          
                  Geom = 'y2014b    '; exe y2014b; }

  Case y2014c   { y2014c : y2014a plus hcal prototype;          
                  Geom = 'y2014c    '; exe y2014c; }

  Case y2015    { y2015  : y2015 baseline, is y2014a plus FMS preshower;
                  Geom = 'y2015     '; exe y2015;  }

  Case y2015a   { y2015a : y2015a production baseline, is y2015 with pixel Al cables;
                  Geom = 'y2015a    '; exe y2015a;  }

  Case dev2016  { dev2016 : y2016 baseline, is y2014a plus FMS preshower;
                  Geom = 'dev2016   '; exe dev2016; }

  Case dev2018  { dev2018 : y2018 baseline, is y2012 lus FMS preshower;
                  Geom = 'dev2018   '; exe dev2018; }

  Case dev15a   { dev15a : y2014 a plus FMS preshower;          
                  Geom = 'dev15a    '; exe dev15a; }

  Case dev15b   { dev15a : y2014 a plus a bare naked hadron calo;
                  Geom = 'dev15b    '; exe dev15b; }

  Case dev2020  { dev2020: y2015a base plus FTSD;
                  Geom = 'dev2020   '; exe dev2020;}


  Case devE  { devE : eSTAR development geometry;
                 Geom = 'devE    ';
                 exe devE; }

  Case DevT  { devT : TPC upgrade studies;
               Geom = 'devT';
               exe devT; }
  Case DevTA  { devTA : TPC upgrade studies 32 inner pad rows 0.67 x 2.0;
               Geom = 'devTA';
               exe devTA; }
  Case DevTB  { devTB : TPC upgrade studies 40 inner pad rows 0.67 x 1.6;
               Geom = 'devTB';
               exe devTB; }
  Case DevTC  { devTC : TPC upgrade studies 40 inner pad rows 0.50 x 1.6;
               Geom = 'devTC';
               exe devTC; }
  Case DevTD  { devTD : TPC upgrade studies 32 inner pad rows 0.50 x 2.0;
               Geom = 'devTD';
               exe devTD; }
  Case DevTE  { devTE : TPC upgrade studies 50 inner pad rows 0.335 x 1.28;
               Geom = 'devTE';
               exe devTE; }
  Case DevTF  { devTF : TPC upgrade studies 32 inner pad rows 0.400 x 2.00;
               Geom = 'devTF';
               exe devTF; }
  Case DevTX  { devTX : TPC upgrade studies 40 inner pad rows 0.500 x 1.60;
               Geom = 'devTX';
               exe devTX; }
  Case DevTY  { devty : TPC upgrade studies 40 inner pad rows 0.500 x 1.60;
               Geom = 'devTY';
               exe devty; }
  Case DevTZ  { devty : TPC upgrade studies 40 inner pad rows 0.500 x 1.60 no HFT;
               Geom = 'devTZ';
               exe devTZ; }

  Case estar1  { eStar1 : eStar development geometry;     Geom = 'estar1  ';  exe estar1; }
  Case TPCIv1  { TPCIv1 : TPC inner pad upgrade geometry; Geom = 'TPCIv1  ';  exe TPCIv1; }
  Case eStar2  { eStar2 : eStar development geometry;     Geom = 'eStar2  ';  exe eStar2; }


  Case hctest  {

  "Switch off all detectors" 
  {PIPE,SVTT,SISD,TPCE,FTPC,BTOF,VPDD,MAGP,CALB,ECAL,UPST,
   RICH,ZCAL,MFLD,BBCM,FPDM,PHMD,PIXL,ISTB,GEMB,FSTD,FTRO,FGTD,
   SHLD,QUAD,MUTD,IGTD,HPDT,ITSP,DUMM,SCON,IDSM,FSCE,EIDD,ISTD, 
   PXST,PSUP,HCAL}=off;

   MFLD = on;
   exe CAVE04
   exe HCALvF



  }
****************************************************************************************
****************************************************************************************
****************************************************************************************
  Case DUMM01   { R and D geometry: TPC+DUMM

                     SVTT=off; "no SVT  at all in this configuration"
                     ftpc=off; "no FTPC at all in this configuration"
                  "ctb: central trigger barrer             ";
                     Itof=2 " CONSTRUCT btofgeo2 ";
                     BtofConfig=5;
                  "CALB"
                     emsEdit=on
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ECAL"
                     EcalConfig=1   " west wheel "
                     ecalFill=3     " all sectors filled "
                  "beam-beam counter "
                     BBCM=on
                  "forward pion detector "
                     FPDM=on
                  "field version "
                     MfldConfig=4;      "tabulated field, with correction "

                     SvshConfig = 0; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 0;

                  "Photon Multiplicity Detector Version "
                     PHMD=off;
                     PhmdConfig = 0;
                  "Silicon Strip Detector Version "
                     SISD=off;
                     SisdConfig = 0;
* careful! Achtung!
                   PipeConfig=4;   " provisional"
                   pipeFlag=0; ! No wrap no svt shield
                   DUMM=on;        " put the dummy in"

                }
****************************************************************************************
  Case UPGR01   { "R and D geometry: TPC+SSD+HFT-SVT"

                  exe EMCUTS(eemc,0);
                  exe EMCUTS(bemc,0);

                     SVTT=off; "no SVT  at all in this configuration"
                     ftpc=off; "no FTPC at all in this configuration"
                  "ctb: central trigger barrer             ";
                     Itof=2 " CONSTRUCT btofgeo2 ";
                     BtofConfig=5;
                  "CALB"
                     emsEdit=on
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ECAL"
                     EcalConfig=1   " west wheel "
                     ecalFill=3     " all sectors filled "
                  "beam-beam counter "
                     BBCM=on
                  "forward pion detector "
                     FPDM=on
                  "field version "
                     MfldConfig=4;      "tabulated field, with correction "

                     SvshConfig = 0; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 0;

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 1;
                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 45;
* careful! Achtung!
                   PipeConfig=4;   " provisional"
                   pipeFlag=0; ! No wrap no svt shield
                   PIXL=on;        " put the pixel detector in"
                   PixlConfig=4;   " newest version by Andrew Rose"
* HPD, IST off
* Forward STAR tracker disk
                   FSTD=on;  "new pixel based forward tracker"
                   FstdConfig=2;
* Forward STAR tracker disk
                   FGTD=off;  "GEM forward tracker"
                   FgtdConfig=0;
* Forward GEM disks in this tag
                   IGTD=on;
* prototype of the Inner Tracker SuPport structure
                   ITSP=on;



                }
****************************************************************************************
  Case UPGR02    { R and D geometry: TPC+IST+HFT-SVT
                  "svt: 3 layers ";

                  exe EMCUTS(eemc,0);
                  exe EMCUTS(bemc,0);

                     nSvtLayer=6  " 3 bi-plane layers, nSvtLayer<=7 ";
                     nSvtVafer=0  " numbering is in the code   ";
                     svtWaferDim=0  " width is in the code      ";

                     ConeConfig=2 " new cable weight estimate ";

                  "ctb: central trigger barrer             ";
                     Itof=4 " CONSTRUCT btofgeo4 ";
* NEW CONFIG!
                     BtofConfig=8;

                  "CALB"
                     emsEdit=on
                     CalbConfig = 1
* remember that with this config, the following parameters have
* a different meaning because we have to (unfortunately) switch
* from divisions to copies and introduce a map, which DOES
* control the configuration
                     nmod={60,60}; shift={75,105}; " 60 sectors West plus 30 East split between 2 halves"

                  "ECAL"
                     EcalConfig=1   " one ECAL patch, west "
                     ecalFill=3     " all sectors filled "

                  "beam-beam counter "
                     BBCM=on

                  "forward pion detector "
                     FPDM=on
                     FpdmConfig  = 1 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     VPDD=on;
                     VpddConfig=4;

                  "field version "
                     MfldConfig=4;      "tabulated field, with correction "

* important: (1) new SVT version (2) FTPC gas correction tp Ar+C02 mix (3) SSD ladders raddi correction

                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 5; "SVTT version"
                     DensConfig = 1; "gas density correction"
                     FtpcConfig = 1; "ftpc configuration"

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     SISD=off;
                     SisdConfig = 0; "no ssd here"


                  "FTPC Readout barrel "
                     FTRO=on;
                     FtroConfig = 1;

* Inner STAR tracker barrel
                   ISTB=on;  "new pixel based inner tracker"
                   IstbConfig=1;

* careful! Achtung!
                   PipeConfig=5;   " thinner pipe "
                   pipeFlag=0; ! No wrap no svt shield
                   PIXL=on;        " activate "
                   PixlConfig=3;   " source version "

                  "New version of the TPC backplane "
                     TpceConfig = 3;

                }
*************************************************************************************************************
  Case UPGR03   { New Tracking: IST+IGT+HFT-SVT

                  exe EMCUTS(eemc,0);
                  exe EMCUTS(bemc,0);

                     SVTT=off; "no SVT  at all in this configuration"
                     ftpc=off; "no FTPC at all in this configuration"
                  "ctb: central trigger barrer             ";
                     Itof=2 " CONSTRUCT btofgeo2 ";
                     BtofConfig=5;
                  "CALB"
                     emsEdit=on
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ECAL"
                     EcalConfig=1   " west wheel "
                     ecalFill=3     " all sectors filled "
                  "beam-beam counter "
                     BBCM=on
                  "forward pion detector "
                     FPDM=on
                  "field version "
                     MfldConfig=4;      "tabulated field, with correction "
*                    -- Obsoleted: CorrNum = 4;
                     SvshConfig = 2; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 4;

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 1;
                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 23;
* careful! Achtung!
                   PipeConfig=4;   " provisional"
                   pipeFlag=0; ! No wrap no svt shield
                   PIXL=on;        " put the pixel detector in"
                   PixlConfig=2;   " newer version decoupled from SVT"
* Inner STAR tracker barrel
                   ISTB=on;  "new pixel based inner tracker"
                   IstbConfig=1;
* Inner STAR GEM barrel
                   GEMB=off;
                   GembConfig=0;
* Forward STAR tracker disk
                   FSTD=on;  "new pixel based forward tracker"
                   FstdConfig=1;
* Forward STAR tracker disk
                   FGTD=off;  "GEM forward tracker"
                   FgtdConfig=0;
* the forward GEM disks
                   IGTD=on;
                }
*************************************************************************************************************
  Case UPGR04   { New Tracking: HPD

                  exe EMCUTS(eemc,0);
                  exe EMCUTS(bemc,0);

                     SVTT=off; "no SVT  at all in this configuration"
                     ftpc=off; "no FTPC at all in this configuration"

                  "ctb: central trigger barrer             ";
                     Itof=2 " CONSTRUCT btofgeo2 ";
                     BtofConfig=5;
                  "CALB"
                     emsEdit=on
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ECAL"
                     EcalConfig=1   " west wheel "
                     ecalFill=3     " all sectors filled "
                  "beam-beam counter "
                     BBCM=on
                  "forward pion detector "
                     FPDM=on
                  "field version "
                     MfldConfig=4;      "tabulated field, with correction "
*                    -- Obsoleted: CorrNum = 4;
                     SvshConfig = 2; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 4;

                  "Photon Multiplicity Detector Version "
                     PHMD=off;
                     PhmdConfig = 0;
                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 35;
* careful! Achtung!
                   PipeConfig=4;   " provisional"
                   pipeFlag=0; ! No wrap no svt shield
                   HPDT=on;        " put the detector in"
                   HpdtConfig=1;   " base version"
                }
****************************************************************************************
  Case UPGR05   { New Tracking: HFT+HPD+IST+TPC-SVT

                  exe EMCUTS(eemc,0);
                  exe EMCUTS(bemc,0);

                     SVTT=off; "no SVT  at all in this configuration"
                     ftpc=off; "no FTPC at all in this configuration"
                  "ctb: central trigger barrer             ";
                     Itof=2 " CONSTRUCT btofgeo2 ";
                     BtofConfig=5;
                  "CALB"
                     emsEdit=on
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ECAL"
                     EcalConfig=1   " west wheel "
                     ecalFill=3     " all sectors filled "
                  "beam-beam counter "
                     BBCM=on
                  "forward pion detector "
                     FPDM=on
                  "field version "
                     MfldConfig=4;      "tabulated field, with correction "

                     SvshConfig = 0; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 0;

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 1;
                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 45;
* careful! Achtung!
                   PipeConfig=4;   " provisional"
                   pipeFlag=0; ! No wrap no svt shield
                   PIXL=on;        " put the pixel detector in"
                   PixlConfig=4;   " newest version by Andrew Rose"

                   HPDT=on;        " put the Hybrid Pixel detector in"
                   HpdtConfig=1;   " base version"
* Inner STAR tracker barrel
                   ISTB=on;  "new pixel based inner tracker"
                   IstbConfig=3;
* Inner STAR GEM barrel
                   GEMB=off;
                   GembConfig=0;
* Forward STAR tracker disk
                   FSTD=on;  "new pixel based forward tracker"
                   FstdConfig=2;
* Forward STAR tracker disk
                   FGTD=off;  "GEM forward tracker"
                   FgtdConfig=0;
* Forward GEM disks in this tag
                   IGTD=on;
* prototype of the Inner Tracker SuPport structure
                   ITSP=on;
                }
****************************************************************************************
  Case UPGR06   { New Tracking: HFT+HPD+SSD

                  exe EMCUTS(eemc,0);
                  exe EMCUTS(bemc,0);

                     SVTT=off; "no SVT  at all in this configuration"
                     ftpc=off; "no FTPC at all in this configuration"
                  "ctb: central trigger barrer             ";
                     Itof=2 " CONSTRUCT btofgeo2 ";
                     BtofConfig=5;
                  "CALB"
                     emsEdit=on
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ECAL"
                     EcalConfig=1   " west wheel "
                     ecalFill=3     " all sectors filled "
                  "beam-beam counter "
                     BBCM=on
                  "forward pion detector "
                     FPDM=on
                  "field version "
                     MfldConfig=4;      "tabulated field, with correction "

                     SvshConfig = 0; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 0;

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 1;
                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 45;
* careful! Achtung!
                   PipeConfig=4;   " provisional"
                   pipeFlag=0; ! No wrap no svt shield
                   PIXL=on;        " put the pixel detector in"
                   PixlConfig=4;   " newest version by Andrew Rose"

                   HPDT=on;        " put the Hybrid Pixel detector in"
                   HpdtConfig=1;   " base version"
* Forward STAR tracker disk
                   FSTD=on;  "new pixel based forward tracker"
                   FstdConfig=2;
* Forward STAR tracker disk
                   FGTD=off;  "GEM forward tracker"
                   FgtdConfig=0;
* Forward GEM disks in this tag
                   IGTD=on;
* prototype of the Inner Tracker SuPport structure
                   ITSP=on;
                }
****************************************************************************************
  Case UPGR07   { New Tracking: HFT+IST+TPC+SSD-SVT

                  exe EMCUTS(eemc,0);
                  exe EMCUTS(bemc,0);

                     SVTT=off; "no SVT  at all in this configuration"
                     ftpc=off; "no FTPC at all in this configuration"
                  "ctb: central trigger barrer             ";
                     Itof=2 " CONSTRUCT btofgeo2 ";
                     BtofConfig=5;
                  "CALB"
                     emsEdit=on
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ECAL"
                     EcalConfig=1   " west wheel "
                     ecalFill=3     " all sectors filled "
                  "beam-beam counter "
                     BBCM=on
                  "forward pion detector "
                     FPDM=on
                  "field version "
                     MfldConfig=4;      "tabulated field, with correction "

                     SvshConfig = 0; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 0;

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 1;
                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 45;
* careful! Achtung!
                   PipeConfig=4;   " provisional"
                   pipeFlag=0; ! No wrap no svt shield
                   PIXL=on;        " put the pixel detector in"
                   PixlConfig=4;   " newest version by Andrew Rose"

                   ISTB=on;  "IST barrel"
                   IstbConfig=3;

                   FSTD=on;  "new pixel based forward tracker disk"
                   FstdConfig=2;

                   IGTD=on; "Forward GEM disks in this tag"
                   ITSP=on; "prototype of the Inner Tracker SuPport structure"
                }
****************************************************************************************
  Case UPGR08   { New Tracking: HFT+HPD+IST+TPC-SVT-SSD

                  exe EMCUTS(eemc,0);
                  exe EMCUTS(bemc,0);

                     SVTT=off; "no SVT  at all in this configuration"
                     ftpc=off; "no FTPC at all in this configuration"
                  "ctb: central trigger barrer             ";
                     Itof=2 " CONSTRUCT btofgeo2 ";
                     BtofConfig=5;
                  "CALB"
                     emsEdit=on
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ECAL"
                     EcalConfig=1   " west wheel "
                     ecalFill=3     " all sectors filled "
                  "beam-beam counter "
                     BBCM=on
                  "forward pion detector "
                     FPDM=on
                  "field version "
                     MfldConfig=4;      "tabulated field, with correction "

                     SvshConfig = 0; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 0;

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 1;
                  "Silicon Strip Detector Version "
                     SISD=off;
                     SisdConfig = 0;
* careful! Achtung!
                   PipeConfig=4;   " provisional"
                   pipeFlag=0; ! No wrap no svt shield
                   PIXL=on;        " put the pixel detector in"
                   PixlConfig=4;   " newest version by Andrew Rose"

                   HPDT=on;        " put the Hybrid Pixel detector in"
                   HpdtConfig=1;   " base version"
* Inner STAR tracker barrel
                   ISTB=on;  "new pixel based inner tracker"
                   IstbConfig=3;
* Inner STAR GEM barrel
                   GEMB=off;
                   GembConfig=0;
* Forward STAR tracker disk
                   FSTD=on;  "new pixel based forward tracker"
                   FstdConfig=2;
* Forward STAR tracker disk
                   FGTD=off;  "GEM forward tracker"
                   FgtdConfig=0;
* Forward GEM disks in this tag
                   IGTD=on;
* prototype of the Inner Tracker SuPport structure
                   ITSP=on;
                }
****************************************************************************************
  Case UPGR09   { New Tracking: HFT+HPD+IST*outer+TPC-SVT-SSD

                  exe EMCUTS(eemc,0);
                  exe EMCUTS(bemc,0);

                     SVTT=off; "no SVT  at all in this configuration"
                     ftpc=off; "no FTPC at all in this configuration"
                  "ctb: central trigger barrer             ";
                     Itof=2 " CONSTRUCT btofgeo2 ";
                     BtofConfig=5;
                  "CALB"
                     emsEdit=on
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ECAL"
                     EcalConfig=1   " west wheel "
                     ecalFill=3     " all sectors filled "
                  "beam-beam counter "
                     BBCM=on
                  "forward pion detector "
                     FPDM=on
                  "field version "
                     MfldConfig=4;      "tabulated field, with correction "

                     SvshConfig = 0; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 0;

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 1;
                  "Silicon Strip Detector Version "
                     SISD=off;
                     SisdConfig = 0;
* careful! Achtung!
                   PipeConfig=4;   " provisional"
                   PIXL=on;        " put the pixel detector in"
                   pipeFlag=0; ! No wrap no svt shield
                   PixlConfig=4;   " newest version by Andrew Rose"

                   HPDT=on;        " put the Hybrid Pixel detector in"
                   HpdtConfig=1;   " base version"
* Inner STAR tracker barrel
                   ISTB=on;  "new pixel based inner tracker"
                   IstbConfig=4;
* Inner STAR GEM barrel
                   GEMB=off;
                   GembConfig=0;
* Forward STAR tracker disk
                   FSTD=on;  "new pixel based forward tracker"
                   FstdConfig=2;
* Forward STAR tracker disk
                   FGTD=off;  "GEM forward tracker"
                   FgtdConfig=0;
* Forward GEM disks in this tag
                   IGTD=on;
* prototype of the Inner Tracker SuPport structure
                   ITSP=on;
                }
****************************************************************************************
  Case UPGR10   { New Tracking: HFT+innerLayerIST+TPC-SVT+SSD

                  exe EMCUTS(eemc,0);
                  exe EMCUTS(bemc,0);

                     SVTT=off; "no SVT  at all in this configuration"
                     ftpc=off; "no FTPC at all in this configuration"
                  "ctb: central trigger barrer             ";
                     Itof=2 " CONSTRUCT btofgeo2 ";
                     BtofConfig=5;
                  "CALB"
                     emsEdit=on
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ECAL"
                     EcalConfig=1   " west wheel "
                     ecalFill=3     " all sectors filled "
                  "beam-beam counter "
                     BBCM=on
                  "forward pion detector "
                     FPDM=on
                  "field version "
                     MfldConfig=4;      "tabulated field, with correction "

                     SvshConfig = 0; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 0;

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 1;
                  "Silicon Strip Detector Version "
                     SISD=off;
                     SisdConfig = 0;
* careful! Achtung!
                   PipeConfig=4;   " provisional"
                   pipeFlag=0; ! No wrap no svt shield
                   PIXL=on;        " put the pixel detector in"
                   PixlConfig=4;   " newest version by Andrew Rose"

                  "Silicon Strip Detector Version "
                   SISD=on;
                   SisdConfig = 45;
* Inner STAR tracker barrel
                   ISTB=on;  "new pixel based inner tracker"
                   IstbConfig=5;
* Inner STAR GEM barrel
                   GEMB=off;
                   GembConfig=0;
* Forward STAR tracker disk
                   FSTD=on;  "new pixel based forward tracker"
                   FstdConfig=2;
* Forward STAR tracker disk
                   FGTD=off;  "GEM forward tracker"
                   FgtdConfig=0;
* Forward GEM disks in this tag
                   IGTD=on;
* prototype of the Inner Tracker SuPport structure
                   ITSP=on;
                }
****************************************************************************************
  Case UPGR11   { New Tracking: HFT+2LayerIST+TPC-SVT+SSD

                  exe EMCUTS(eemc,0);
                  exe EMCUTS(bemc,0);

                     SVTT=off; "no SVT  at all in this configuration"
                     ftpc=off; "no FTPC at all in this configuration"
                  "ctb: central trigger barrer             ";
                     Itof=2 " CONSTRUCT btofgeo2 ";
                     BtofConfig=5;
                  "CALB"
                     emsEdit=on
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ECAL"
                     EcalConfig=1   " west wheel "
                     ecalFill=3     " all sectors filled "
                  "beam-beam counter "
                     BBCM=on
                  "forward pion detector "
                     FPDM=on
                  "field version "
                     MfldConfig=4;      "tabulated field, with correction "

                     SvshConfig = 0; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 0;

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 1;
* careful! Achtung!
                   PipeConfig=4;   " provisional"
                   pipeFlag=0; ! No wrap no svt shield
                   PIXL=on;        " put the pixel detector in"
                   PixlConfig=4;   " newest version by Andrew Rose"

                  "Silicon Strip Detector Version "
                   SISD=on;
                   SisdConfig = 45;
* Inner STAR tracker barrel
                   ISTB=on;  "new pixel based inner tracker"
                   IstbConfig=6;
* Inner STAR GEM barrel
                   GEMB=off;
                   GembConfig=0;
* Forward STAR tracker disk
                   FSTD=on;  "new pixel based forward tracker"
                   FstdConfig=2;
* Forward STAR tracker disk
                   FGTD=off;  "GEM forward tracker"
                   FgtdConfig=0;
* Forward GEM disks in this tag
                   IGTD=on;
* prototype of the Inner Tracker SuPport structure
                   ITSP=on;
                }
*************    ***********************************************************************
  Case UPGR12   { New Tracking: HFT+HPD+IST+TPC+IGT*newRadii

                  exe EMCUTS(eemc,0);
                  exe EMCUTS(bemc,0);

                     SVTT=off; "no SVT  at all in this configuration"
                     ftpc=off; "no FTPC at all in this configuration"
                  "ctb: central trigger barrer             ";
                     Itof=2 " CONSTRUCT btofgeo2 ";
                     BtofConfig=5;
                  "CALB"
                     emsEdit=on
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ECAL"
                     EcalConfig=1   " west wheel "
                     ecalFill=3     " all sectors filled "
                  "beam-beam counter "
                     BBCM=on
                  "forward pion detector "
                     FPDM=on
                  "field version "
                     MfldConfig=4;      "tabulated field, with correction "

                     SvshConfig = 0; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 0;

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 1;
                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 45;
* careful! Achtung!
                   PipeConfig=4;   " provisional"
                   pipeFlag=0; ! No wrap no svt shield
                   PIXL=on;        " put the pixel detector in"
                   PixlConfig=4;   " newest version by Andrew Rose"

                   HPDT=on;        " put the Hybrid Pixel detector in"
                   HpdtConfig=1;   " base version"
* Inner STAR tracker barrel
                   ISTB=on;  "new pixel based inner tracker"
                   IstbConfig=3;
* Inner STAR GEM barrel
                   GEMB=off;
                   GembConfig=0;
* Forward STAR tracker disk
                   FSTD=on;  "new pixel based forward tracker"
                   FstdConfig=2;
* Forward STAR tracker disk
                   FGTD=off;  "GEM forward tracker"
                   FgtdConfig=0;
* Forward GEM disks in this tag
                   IGTD=on;
                   IgtdConfig=2;
* prototype of the Inner Tracker SuPport structure
                   ITSP=on;
                }
****************************************************************************************
  Case UPGR13   { New Tracking: HFT+IST+TPC+SSD-SVT

                  exe EMCUTS(eemc,0);
                  exe EMCUTS(bemc,0);

                     SVTT=off; "no SVT  at all in this configuration"
                     ftpc=off; "no FTPC at all in this configuration"
                  "ctb: central trigger barrer             ";
                     Itof=2 " CONSTRUCT btofgeo2 ";
                     BtofConfig=5;
                  "CALB"
                     emsEdit=on
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ECAL"
                     EcalConfig=1   " west wheel "
                     ecalFill=3     " all sectors filled "
                  "beam-beam counter "
                     BBCM=on
                  "forward pion detector "
                     FPDM=on
                  "field version "
                     MfldConfig=4;      "tabulated field, with correction "

                     SvshConfig = 0; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 0;

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 1;
                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 65;
* careful! Achtung!
                   PipeConfig=4;   " provisional"
                   pipeFlag=0; ! No wrap no svt shield
                   PIXL=on;        " put the pixel detector in"
                   PixlConfig=5;   " newest version, thicker active Si"

                   ISTB=on;  "IST barrel"
                   IstbConfig=7;

                   FSTD=off;  "no pixel based forward tracker in this tag"
                   FstdConfig=0;

* Forward STAR tracker disk
                   FGTD=on;  "GEM forward tracker"
                   FgtdConfig=2;
* On Gerrit  request, we disable the cone:
                   ITSP=off; "prototype of the Inner Tracker SuPport structure"
                }
****************************************************************************************
  Case UPGR14   { TUP sans IST: HFT+TPC+SSD-SVT

                  exe EMCUTS(eemc,0);
                  exe EMCUTS(bemc,0);

                     SVTT=off; "no SVT  at all in this configuration"
                     ftpc=off; "no FTPC at all in this configuration"
                  "ctb: central trigger barrer             ";
                     Itof=2 " CONSTRUCT btofgeo2 ";
                     BtofConfig=5;
                  "CALB"
                     emsEdit=on
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ECAL"
                     EcalConfig=1   " west wheel "
                     ecalFill=3     " all sectors filled "
                  "beam-beam counter "
                     BBCM=on
                  "forward pion detector "
                     FPDM=on
                  "field version "
                     MfldConfig=4;      "tabulated field, with correction "

                     SvshConfig = 0; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 0;

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 1;
                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 65;
* careful! Achtung!
                   PipeConfig=4;   " provisional"
                   pipeFlag=0; ! No wrap no svt shield
                   PIXL=on;        " put the pixel detector in"
                   PixlConfig=5;   " newest version, thicker active Si"

                   ISTB=off;       "no IST barrel"
                   IstbConfig=0;   "ditto"

                   FSTD=off;  "no pixel based forward tracker in this tag"
                   FstdConfig=0;

* Forward STAR tracker disk
                   FGTD=on;  "GEM forward tracker"
                   FgtdConfig=2;
* On Gerrit  request, we disable the cone:
                   ITSP=off; "prototype of the Inner Tracker SuPport structure"
                }
****************************************************************************************
*The reason for naming these three file xxxxgeo00.g is to indicate that these
*are the most simple geometries that can be made. As such they Do not fit in
*with the existing naming scheme where the increasing numbers indicate a
*next step of evolution, better understanding of material, better design, etc.
*I explicitly wanted to break with this scheme to make it clear that these
*designs are just for testing purposes. Gerrit

  Case UPGR15   { New Tracking: HFT+IST+TPC+SSD-SVT

                  exe EMCUTS(eemc,0);
                  exe EMCUTS(bemc,0);

                exe UPGR15; }
****************************************************************************************
  Case UPGR16   { New Tracking: HFT+IST+TPC+SSD-SVT
                  exe EMCUTS(eemc,0);
                  exe EMCUTS(bemc,0);
                  exe  UPGR16; }
****************************************************************************************
  Case UPGR16a   { upgr16 + tpc2009
                  exe EMCUTS(eemc,0);
                  exe EMCUTS(bemc,0);
                  exe  UPGR16a; }
****************************************************************************************
  Case UPGR17   { UPGR16 - FGTD + FTPC  request Wei-Ming-Zhang

                  exe EMCUTS(eemc,0);
                  exe EMCUTS(bemc,0);
                     SVTT=off; "no SVT  at all in this configuration"
                     ftpc=off; "no FTPC at all in this configuration"
                     SCON=on;
                     ConeConfig=2 " new cable weight estimate ";

                  "ctb: central trigger barrer             ";
                     Itof=2 " CONSTRUCT btofgeo2 ";
                     BtofConfig=6;
                  "CALB"
                     emsEdit=on
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ECAL"
                     EcalConfig=1   " west wheel "
                     ecalFill=3     " all sectors filled "
                  "beam-beam counter "
                     BBCM=on
                  "forward pion detector "
                     FPDM=on
                  "field version "
                     MfldConfig=4;      "tabulated field, with correction "

                     SvshConfig = 0; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     SvttConfig = 0;

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 1;
                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 65;
* careful! Achtung!
                   PipeConfig=4;   " provisional"
                   pipeFlag=-1; !   " Simplest.Gerrit"
                   PipeFlag = 1;    "pipe wrap only"

                   PIXL=on;         " put the pixel detector in"
                   PixlConfig=-1;   " Simplest.Gerrit"

                   ISTB=on;  "IST barrel"
                   IstbConfig=-1;

                   FSTD=off;  "no pixel based forward tracker in this tag"
                   FstdConfig=0;

* No Forward STAR tracker disk
                   FGTD=off;  "GEM forward tracker"
* On Gerrit  request, we disable the cone:
                   ITSP=off; "prototype of the Inner Tracker SuPport structure"
                  "New version of the TPC backplane "
                     TpceConfig = 3;
                  "We need an even bigger Cave"
                     CaveConfig = 4;
* FTPC from y2008
                     SupoConfig = 1;            "FTPC Support"
                     FtpcConfig = 1;            "ftpc configuration"
                  "FTPC Readout barrel "
                     FTRO=on;
                     FtroConfig = 1;

                }
****************************************************************************************
  Case DEV2005    { THIS TAG IS RESERVED FOR THE 2005 DEVELOPMENT ONLY
                  "svt: 3 layers ";
                     nSvtLayer=6  " 3 bi-plane layers, nSvtLayer<=7 ";
                     nSvtVafer=0  " numbering is in the code   ";
                     svtWaferDim=0  " width is in the code      ";

                     ConeConfig=2 " new cable weight estimate ";

                  "ctb: central trigger barrer             ";
                     Itof=4 " CONSTRUCT btofgeo4 ";
* NEW CONFIG!
                     BtofConfig=8;

                  "CALB"
                     emsEdit=on
                     CalbConfig = 1
* remember that with this config, the following parameters have
* a different meaning because we have to (unfortunately) switch
* from divisions to copies and introduce a map, which DOES
* control the configuration
                     nmod={60,60}; shift={75,105}; " 60 sectors West plus 30 East split between 2 halves"

                  "ECAL"
                     EcalConfig=1   " one ECAL patch, west "
                     ecalFill=3     " all sectors filled "

                  "beam-beam counter "
                     BBCM=on

                  "forward pion detector "
                     FPDM=on
                     FpdmConfig  = 1 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     VPDD=on;
                     VpddConfig=4;

                  "field version "
                     MfldConfig=4;      "tabulated field, with correction "

* important: (1) new SVT version (2) FTPC gas correction tp Ar+C02 mix (3) SSD ladders raddi correction

                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 7; "SVTT version"
                     DensConfig = 1; "gas density correction"
                     FtpcConfig = 1; "ftpc configuration"

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 24; "second version, full barrel with corrected radii"


                  "FTPC Readout barrel "
                     FTRO=on;
                     FtroConfig = 1;

                  "New version of the TPC backplane "
                     TpceConfig = 2;
                }

****************************************************************************************
  Case DEV2007    { THIS TAG IS RESERVED FOR THE 2007 DEVELOPMENT ONLY
                  "svt: 3 layers ";
                     nSvtLayer=6  " 3 bi-plane layers, nSvtLayer<=7 ";
                     nSvtVafer=0  " numbering is in the code   ";
                     svtWaferDim=0  " width is in the code      ";

                     ConeConfig=2 " new cable weight estimate ";

                  "ctb: central trigger barrer             ";
                     Itof=5 " CONSTRUCT btofgeo5 ";
* NEW CONFIG!
                     BtofConfig=10;

* Full barrel in 2007
                  "CALB"
                     emsEdit=on ;
* important:
                     CalbConfig = 2
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ECAL"
                     EcalConfig=1   " one ECAL patch, west "
                     ecalFill=3     " all sectors filled "

                  "beam-beam counter "
                     BBCM=on

                  "forward pion detector "
                     FPDM=on
                     FpdmConfig  = 3 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     VPDD=on;
                     VpddConfig=7;

                  "field version "
                     MfldConfig=4;      "tabulated field, with correction "

* important: (1) new SVT version (2) FTPC gas correction tp Ar+C02 mix (3) SSD ladders raddi correction

                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 6; "SVTT version"
                     SvshConfig = 3; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     FtpcConfig = 1; "ftpc configuration"

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 55; "fifth version, corrected radii, gaps, dead material"


                  "FTPC Readout barrel "
                     FTRO=on;
                     FtroConfig = 1;

                  "New version of the TPC backplane "
                     TpceConfig = 3;
                  "Muon Trigger System"
                     MUTD = on;
                     MutdConfig = 1;
                  "We need an even bigger Cave"
                     CaveConfig = 4;
                }

****************************************************************************************
****************************************************************************************
  Case UPGR21    { Year UPGR20 + full tof;


                  exe EMCUTS(eemc,0);
                  exe EMCUTS(bemc,0);

                     nSvtLayer=6  " 3 bi-plane layers, nSvtLayer<=7 ";
                     nSvtVafer=0  " numbering is in the code   ";
                     svtWaferDim=0  " width is in the code      ";

                     ConeConfig=2 " new cable weight estimate ";

                  "ctb: central trigger barrer             ";
                     Itof=5 " CONSTRUCT btofgeo5 ";
* NEW CONFIG!
                     BtofConfig=6;

* Full barrel in 2007
                  "CALB"
                     emsEdit=on ;
* important:
                     CalbConfig = 2
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ECAL"
                     EcalConfig=1   " one ECAL patch, west "
                     ecalFill=3     " all sectors filled "

                  "beam-beam counter "
                     BBCM=on

                  "forward pion detector "
                     FPDM=on
                     FpdmConfig  = 3 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     VPDD=on;
                     VpddConfig=7;

                  "field version "
                     MfldConfig=4;      "tabulated field, with correction "

* important: (1) new SVT version (2) FTPC gas correction tp Ar+C02 mix (3) SSD ladders raddi correction

                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 6; "SVTT version"
                     SvshConfig = 3; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     FtpcConfig = 1; "ftpc configuration"

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 55; "fifth version, corrected radii, gaps, dead material"


                  "FTPC Readout barrel "
                     FTRO=on;
                     FtroConfig = 1;

                  "New version of the TPC backplane "
                     TpceConfig = 3;
                  "Muon Trigger System"
                     MUTD = on;
                     MutdConfig = 2;
                  "We need an even bigger Cave"
                     CaveConfig = 4;
                }
****************************************************************************************
  Case UPGR20    { y2007 + one TOF


                  exe EMCUTS(eemc,0);
                  exe EMCUTS(bemc,0);

                  "svt: 3 layers ";
                     nSvtLayer=6  " 3 bi-plane layers, nSvtLayer<=7 ";
                     nSvtVafer=0  " numbering is in the code   ";
                     svtWaferDim=0  " width is in the code      ";

                     ConeConfig=2 " new cable weight estimate ";

                  "ctb: central trigger barrer             ";
                     Itof=5 " CONSTRUCT btofgeo5 ";
* NEW CONFIG!
                     BtofConfig=10;

* Full barrel in 2007
                  "CALB"
                     emsEdit=on ;
* important:
                     CalbConfig = 2
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ECAL"
                     EcalConfig=1   " one ECAL patch, west "
                     ecalFill=3     " all sectors filled "

                  "beam-beam counter "
                     BBCM=on

                  "forward pion detector "
                     FPDM=on
                     FpdmConfig  = 3 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     VPDD=on;
                     VpddConfig=7;

                  "field version "
                     MfldConfig=4;      "tabulated field, with correction "

* important: (1) new SVT version (2) FTPC gas correction tp Ar+C02 mix (3) SSD ladders raddi correction

                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 6; "SVTT version"
                     SvshConfig = 3; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     FtpcConfig = 1; "ftpc configuration"

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 55; "fifth version, corrected radii, gaps, dead material"


                  "FTPC Readout barrel "
                     FTRO=on;
                     FtroConfig = 1;

                  "New version of the TPC backplane "
                     TpceConfig = 3;
                  "Muon Trigger System"
                     MUTD = on;
                     MutdConfig = 2;
                  "We need an even bigger Cave"
                     CaveConfig = 4;
                }
****************************************************************************************
  Case UPGR22    { first FHMC version;

                  exe EMCUTS(eemc,0);
                  exe EMCUTS(bemc,0);

                 exe UPGR22;
               }

****************************************************************************************
****************************************************************************************
****************************************************************************************
  Case Upgr23 { The Forward Spaghettini Calorimeter

       """Use y2010a as the baseline"""
       EXE y2010a;

       """Move the FMS N and S modules to an open position"""
       FpdmPosition=1;
       
       """Switch on and configure the FSC geometry"""
       FSCE=on;
       FsceConfig=1;

  }


  Case HADR_ON    { all Geant Physics On;                                       }
  Case HADR_OFF   { all Geant Physics on, except for hadronic interactions;
                                                                       IHADR=0}
  Case GCALOR_ON { setting hadr 6 to activate hadronic showers;
                              IHADR=6;}

  Case PHYS_OFF   { No Physics: only energy loss;
      {IDCAY,IANNI,IBREM,ICOMP,IHADR,IMUNU,IPAIR,IPHOT,IDRAY,IMULS}=0; Iloss=2}
  Case DECAY_ONLY { Some Physics: decays, mult.scat and energy loss;
                  {IANNI,IBREM,ICOMP,IHADR,IMUNU,IPAIR,IPHOT,IDRAY}=0; Iloss=2}
  Case NO_BREM    { No bremmstrahlung;
                  IBREM=0;}

  Case LOW_EM     { Low cuts on ElectroMagnetic processes;
                  CUTGAM=0.00001;
                  CUTELE=0.00001;
                  BCUTE =0.00001;
                  BCUTM =0.00001;
                  DCUTE =0.00001;
                  DCUTM =0.00001;
                }

  Case EMC_10keV    { 10 keV cuts on Electromagnetic processes in the barrel and endcap;
                    BEmcCutConfig = 1;
                    EEmcCutConfig = 1;
                    }
  Case EMC_30keV    { 30 keV cuts on Electromagnetic processes in the barrel and endcap;
                    BEmcCutConfig = 2;
                    EEmcCutConfig = 2;
                    }
  Case EMC_100keV   { 100 keV cuts on Electromagnetic processes in the barrel and endcap;
                    BEmcCutConfig = 3;
                    EEmcCutConfig = 3;
                    }
  Case EMC_1MeV     { 1 MeV cuts on Electromagnetic processes in the barrel and endcap;
                    BEmcCutConfig = 4;
                    EEmcCutConfig = 4;
                    }

  Case TPC_ONLY   { Minimal geometry - only TPC;
                  {PIPE,SVTT,ftpc,BTOF,VPDD,CALB,ECAL,MAGP,UPST,ZCAL,PHMD,FPDM,BBCM,SISD,FTRO}=off; }
  Case TPC_AND_SVTT  { Only TPC and SVT;
                  {PIPE,ftpc,BTOF,VPDD,CALB,ECAL,MAGP,UPST,ZCAL,PHMD,FPDM,BBCM,FTRO}=off; }
  Case SVTT_ON    { Optional SVTT added on top of the minimal geo;
                     SVTT=on; }
  Case SVTT_OFF   { Optionally remove the SVTT;
                     SVTT=off; }
  Case SISD_OFF   { Optionally remove the SISD sisd;
                     SISD=off; }
  Case ONLY_SVTT   { Only SVTT;
                  {PIPE,TPCE,ftpc,BTOF,VPDD,CALB,ECAL,MAGP,UPST,ZCAL,PHMD,FPDM,BBCM,FTRO}=off; }
*
  Case PIPE_ON    { Optional PIPE added on top of the minimal geo;
                     PIPE=on; }
  Case PIPE_OFF   { Pipe optionally removed;
                     PIPE=off; }
*
  Case FTPC_ON    { Optional FTPC added on top of the minimal geo;
                     ftpc=on; }
  Case BTOF_ON    { Optional BTOF added on top of the minimal geo;
                     BTOF=on; }
  Case ECAL_ON    { Optional ECAL added on top of the minimal geo;
                     ECAL=on; }
  Case CALB_ON    { Optional CALB added on top of the minimal geo;
                     CALB=on; }

  Case SHIELD_OFF { Can switch the shield off in the DEV geom;
                     SHLD=off; }

  Case PIXL_ON    { Optional PIXL added on top of the minimal geo;
                     PIXL=on; }
  Case ISTD_ON    { Optional PIXL added on top of the minimal geo;
                     ISTD=on; }

  Case FIELD_ONLY { No geometry - only magnetic field;
      NtrSubEv=0;
      {CAVE,PIPE,SVTT,TPCE,ftpc,BTOF,VPDD,MAGP,CALB,ECAL,RICH,UPST,ZCAL}=off; }
  Case FIELD_OFF  { no magnetic field;                
                  magField=0;                  }
  Case FIELD_ON   { Standard (5 KGs) field on;        
                  magField=5;                  }
  Case FIELD      { defined mag field;
                  magField=myArg; }

  Case STOP       { Stop processor on failure to load; stop_on_fail = myArg; }

  Case 4TH_OFF    { SVT fourth layer off;
		nSvtLayer=min(nSvtLayer,6);           }
  Case SPLIT_OFF  { events will not be split into subevents;
		NtrSubEv=0;    }
  Case SPLIT_ON   { events will be split into subevents;
           	NtrSubEv=1000; }
  Case DEBUG_ON   { verbose mode, some graphics;
		Idebug=max(Idebug,1); Itest=1; }
  Case DEBUG_OFF  { standard debug mode;
           	{Idebug,Itest}=0;              }

  Case TOF_MAX { Configure maximum time of flight for particles in this run;
                <W> TOFMAX, myArg; ('Change TOFMAX from ',E8.1,' to ', E8.1);
                TOFMAX = myArg; }

  } !// end of main moop

* sanity check - if something left in commands (unknown keyword), we stop!
  if (JL .le. JR) {
     print *,' Unknown command left => ', commands(JL:JR), ' ',JL,JR
     if (IPRIN==0) stop 'You better stop here to avoid problems'
  }
}


* -------------------- setup selected configuration ------------------------
* Now when all parameters and flags are ready, make gstar work as usually
* ie put a MODE or/and DETP command and executing them for selected systems.
*
* - to save secondaries AFTER all decays:      DETP TRAC DCAY 210 210 0.1 0.01
* - where 210 = max Rxy, 210=max abs(Z). 
* - 0.1 min energy 0.01 min kin of out tracks of secondary(decay)

!// here Rxy & aZ for all STAR sensetive volumes
!// when it is too much, use card like "DETP TRAC DCAY 210 210 0.1 0.01"
   dcay={450,2000,0.1,0.01}

   If LL>0 { call AgDETP new ('Trac'); call AgDETP add ('TracDCAY',dcay,4) }

   if ( verbose ) {
   write(*,*) '****** ATTENTION ACHTUNG ATTENZIONE VNIMANIE UVAGA WEI ******'
   write(*,*) '******* THESE FLAGS ARE USED TO GENERATE THE GEOMETRY *******'
   write(*,*) '                 BtofConfig: ',BtofConfig
   write(*,*) '                 CaveConfig: ',CaveConfig
   write(*,*) '                 CalbConfig: ',CalbConfig
   write(*,*) '                 ConeConfig: ',ConeConfig
   write(*,*) '                 DensConfig: ',DensConfig
   write(*,*) '                 FgtdConfig: ',FgtdConfig
   write(*,*) '                 FpdmConfig: ',FpdmConfig
   write(*,*) '                 FstdConfig: ',FstdConfig
   write(*,*) '                 FtpcConfig: ',FtpcConfig
   write(*,*) '                 FtroConfig: ',FtroConfig
   write(*,*) '                 HpdtConfig: ',HpdtConfig
   write(*,*) '                 IstbConfig: ',IstbConfig
   write(*,*) '                 MutdConfig: ',MutdConfig
   write(*,*) '                 GembConfig: ',GembConfig
   write(*,*) '                 PhmdConfig: ',PhmdConfig
   write(*,*) '                 PipeConfig: ',PipeConfig,'  PipeFlag: ',PipeFlag
   write(*,*) '                 PixlConfig: ',PixlConfig
   write(*,*) '                 SvshConfig: ',SvshConfig
   write(*,*) '                 SisdConfig: ',SisdConfig
   write(*,*) '                 SupoConfig: ',SupoConfig
   write(*,*) '                 SvttConfig: ',SvttConfig
   write(*,*) '                 TpceConfig: ',TpceConfig
   write(*,*) '                 VpddConfig: ',VpddConfig
   write(*,*) '***** FOR EXPERTS ONLY: LOOK UP GEOMETRY.G FOR DETAIL *******'
   }


   if (RICH) ItCKOV = 1

   IF CAVE {

      WRITE (*,*) 'CAVE: caveconfig = ', caveconfig
 
      call AgDETP new ('CAVE')
      call AgDETP add ('CVCF.config=',CaveConfig,1)
      IF      caveconfig < 5 {       CONSTRUCT cavegeo;  }
      ELSE IF caveconfig ==5 {       CONSTRUCT cavegeo2; }
 
   }


   """ Support cone needs to be constructed before the v3 beam pipe """
   IF IDSM { "Inner detector support module" 
      Call AgDETP new ('IDSM')
      Call AgDETP add ('IDSC.version=',IdsmConfig,1)
      IF IdsmConfig==1  { CONSTRUCT IdsmGeo1; }
      IF IdsmConfig==2  { CONSTRUCT IdsmGeo2; }
      IF IdsmConfig==14 { CONSTRUCT IdsmGeo2; }
   }

* Pipe:

   IF PIPE  {

     Call AgDETP new ('PIPE')
     IF   PipeConfig < 10 {
          Call AgDETP add ('pipv.PipeConfig=', PipeConfig,1);
          Call AgDETP add ('pipv.PipeFlag=',   PipeFlag,  1);
     }

     IF   PipeConfig == 30 {
          Call AgDETP add ('pipv.PipeConfig=', PipeConfig,1);
          Call AgDETP add ('pipv.PipeFlag=',   PipeFlag,  1);
     }

     IF      PipeConfig == -1 { CONSTRUCT pipegeo00; "Simple beam pipe"   }
     ELSE IF PipeConfig  < 10 { CONSTRUCT pipegeo;   "Standard beam pipe" }
     ELSE IF PipeConfig == 10 { CONSTRUCT pipegeo1;  "HFT era beam pipe"  }
     ELSE IF PipeConfig == 20 { CONSTRUCT pipegeo2;  "HFT era beam pipe as built" }
     ELSE IF PipeConfig == 30 { CONSTRUCT pipegeo3;  "HFT era beam pipe as built improved" }

   }

* Upstream (DX), shield, and D0+Q1+Q2+Q3
   if (UPST)        {
      CONSTRUCT upstgeo;
   }
   if (SHLD)        {
      CONSTRUCT shldgeo;
   }
   if (QUAD)        {
      CONSTRUCT quadgeo;
   }

* ---
   Call AGSFLAG('SIMU',2)

* - to switch off the fourth svt layer:        DETP SVTT SVTG.nlayer=6
   if (SCON) {
     call AgDETP new ('SCON')
     call AgDETP add ('svtg.ConeVer=',ConeConfig ,1) ! could have more copper on the cone
     CONSTRUCT scongeo
   }

   If (SVTT) { 
     call AgDETP new ('SVTT')
     if (nSvtLayer < 7)     call AgDETP add ('svtg.nlayer=',   nSvtLayer,1)
     if (nSvt1stLayer > 1)  call AgDETP add ('svtg.nmin=',     nSvt1stLayer,1)
     if (PipeConfig >= 4)   call AgDETP add ('svtg.ifMany=',     1,1)
     if (nSvtVafer > 0)     call AgDETP add ('svtl(3).nwafer=',nSvtVafer,1)
     if (svtWaferDim > 0)           call AgDETP add ('swca.WaferWid=', svtWaferDim,1)
     if (svtWaferDim > 0)           call AgDETP add ('swca.WaferLen=', svtWaferDim,1)
     if (.not.svtWater)     call AgDETP add ('swam.Len=',       0, 1)

* Take care of the correction level and call the appropriate constructor:

*   This applies to the newer versions of the svt code:
*   we can now switch to a better description of the cone
*   material (copper cables) thanks to a new measurement by
*   Dave Lynn

    call AgDETP add ('svtg.ConeVer=',ConeConfig ,1) ! could have more copper on the cone

* Optionally, switch to a larger inner shield, AND smaller beampipe support
* Or, pick a shield that is slighly bigger outside according to Lilian observation
    if(SvshConfig>0) call AgDETP add ('svtg.SupportVer=',SvshConfig ,1)
    call AgDETP add ('svtg.Config=',SvttConfig ,1)

* Ugly, but I Do not want to hash function pointers in Fortran:

    IF SvttConfig== 0 {  CONSTRUCT svttgeo;   }
    IF SvttConfig== 1 {  CONSTRUCT svttgeo1;  }
    IF SvttConfig== 2 {  CONSTRUCT svttgeo2;  }
    IF SvttConfig== 3 {  CONSTRUCT svttgeo3;  }
    IF SvttConfig== 4 {  CONSTRUCT svttgeo4;  }
    IF SvttConfig== 5 {  CONSTRUCT svttgeo5;  }
    IF SvttConfig== 6 {  CONSTRUCT svttgeo6;  }
    IF SvttConfig== 7 {  CONSTRUCT svttgeo7;  }
    IF SvttConfig== 9 {  CONSTRUCT svttgeo9;  }
    IF SvttConfig==10 {  CONSTRUCT svttgeo10; }
    IF SvttConfig>=11 {  CONSTRUCT svttgeo11; } 

  }!!end SVTT

* Set the proper configuration of the Silicon Strip Detector
* See note on top about using MOD(10) to encode the geometry
* cut, as opposed to configuration of the detector:

* Back in July 2003 Yuri has discovered the discrepancy
* in the gas density. The patch for this is activated here: (was: if(CorrNum>=3) )



   if (TPCE)  {
* Back in July 2003 Yuri has discovered the discrepancy
* in the gas density. The patch for this is activated here: (was: if(CorrNum>=3) )

     call AgDETP new('TPCE');  

     if (DensConfig >0) {        Call AgDETP add ('tpcg.gasCorr=',2 ,1);     }

     if (TpceConfig==1) {        CONSTRUCT tpcegeo;}

     if (TpceConfig==2) {        CONSTRUCT tpcegeo1; }
     if (TpceConfig==3) {        CONSTRUCT tpcegeo2; }
     if (TpceConfig==4) {
     if ( RmaxConfig>0) {        Call AgDetp add ('tpcg.rmax=',207.77,1); }
                                 CONSTRUCT tpcegeo3
                        }


     IF TpcxConfig==1   {                                                    CONSTRUCT TpcxGeo1;  }
     IF TpcxConfig==2   {                                                    CONSTRUCT TpcxGeo2;  }
     IF TpceConfig==31 {         Call AgDETP add('tpcc.version=', 3.1, 1 );  CONSTRUCT TpceGeo3a; }
     IF TpceConfig==51 {         Call AgDETP add('tpcc.version=', 5.1, 1 );  
                                 Call AgDetp add('tpcg.TpadConfig=',TpadConfig,1);
                                                                             CONSTRUCT TpceGeo5a; }

   }
   if (ftpc) then
        if(FtpcConfig==0) {CONSTRUCT ftpcgeo;}
        if(FtpcConfig==1) {CONSTRUCT ftpcgeo1;}
*       and look at the support pieces, was: if(CorrNum==0)
        if(SupoConfig==0)  {CONSTRUCT supogeo;}
        if(SupoConfig==1)  {CONSTRUCT supogeo1;}
   endif

* FTPC readout electronics barrel
   if (FTRO) {CONSTRUCT ftrogeo;}

* - tof system should be on (for year 2):      DETP BTOF BTOG.choice=2
   If (BTOF) { 
c    write(*,*) 'BTOF'
     call AgDETP new ('BTOF')
     call AgDETP add ('btog.choice=',BtofConfig,1)
* X.Dong
     if(Itof>5) {
         call AgDETP add ('btog.X0=',tofX0,1)
         call AgDETP add ('btog.Z0=',tofZ0,1)
     }
* X.Dong.end
      IF (Itof=1)  write(*,*) '*****  ATTENTION : OLD VERSION OF BTOF NOT IMPLEMENTED - NO TOF CREATED *****'
      IF Itof=2 {  CONSTRUCT btofgeo2; }
      IF Itof=4 {  CONSTRUCT btofgeo4; }
      IF Itof=5 {  CONSTRUCT btofgeo5; }
      IF Itof=6 {  CONSTRUCT btofgeo6; }     !X.Dong + F.Geurts
      IF Itof=7 {  CONSTRUCT btofgeo7; }     !F.Geurts fixes to sensitive volumes
      IF Itof=8 {  CONSTRUCT btofgeo8; }    
   } 

   Call AGSFLAG('SIMU',1)


********************* Vertex Position Detector *******************
   If (LL>0 & VPDD) then
     call AgDETP new ('VPDD')
     call AgDETP add ('vpdv.vpdConfig=',VpddConfig,1);
!    if(VpddConfig <7) { CONSTRUCT vpddgeo; }
!    if(VpddConfig==7) { CONSTRUCT vpddgeo2;}
!    if(VpddConfig==8) { CONSTRUCT vpddgeo3;}
     if VpddModule==0  { CONSTRUCT vpddgeo; }
     if VpddModule==2  { CONSTRUCT vpddgeo2;}
     if VpddModule==3  { CONSTRUCT vpddgeo3;}
   endif

********************** BARREL CALORIMETER ************************
*  - Set up the parameters for the barrel calorimeter
   If (CALB) {
     call AgDETP new ('CALB')
     if (emsEdit)  call AgDETP add ('calg.nmodule=',Nmod, 2)
     if (emsEdit)  call AgDETP add ('calg.shift=',  shift,2)

       if(CalbConfig==0) then
c          write(*,*) '************** Creating the 1996-2003 version of the Barrel Calorimeter'
           CONSTRUCT calbgeo
       endif

       if(CalbConfig==1) then
c          write(*,*) '************** Creating the 2004-2006 version of the Barrel Calorimeter'
           CONSTRUCT calbgeo1
       endif

       if(CalbConfig==2) then
c          write(*,*) '************** Creating the 2007-     version of the Barrel Calorimeter'
           Call AgDetp add ('ccut.absorber=',  BEmcCutConfig, 1)
           Call AgDetp add ('ccut.sensitive=', BEmcCutConfig, 1)
           CONSTRUCT calbgeo2
       endif

   }
******************************************************************
*  - Set up the parameters for the RICH counter
   if (LL>0 & RICH) then
      call AgDETP new ('Rich')
      if (richConfig>0) call AgDETP add ('Rich.Version=', richConfig,1)
      if (richPos>0) call AgDETP add ('Rich.Position=',richPos,1)
      if (richPos>0) call AgDETP add ('Rich.Cversion=',richPos,1)
   endif
   if (RICH) {CONSTRUCT richgeo;}

******************************************************************
*  - Set up the parameters for the endcap calorimeter
   If (ECAL) then
      call AgDETP new ('ECAL')
      call AgDETP add ('emcg.OnOff='   ,EcalConfig,1)
      call AgDETP add ('emcg.FillMode=',ecalFill,1)
      IF ( EcalGeometry .lt. 6 ) { CONSTRUCT ecalgeo; }           ! version 5
      IF ( EcalGeometry .eq. 6 ) THEN 
                                 Call AgDetp add ('ecut.absorber=',  EEmcCutConfig, 1)
                                 Call AgDetp add ('ecut.sensitive=', EEmcCutConfig, 1)
                                 CONSTRUCT ecalgeo6;           ! version 6
      ENDIF
   endif

******************************************************************
* The rest of steering:

   if (BBCM)                   { 
      CONSTRUCT bbcmgeo
   }

   if (FPDM){

     IF FpdmPosition > 0 {
        """Move the FMS N/S modules to an outward position"""
        Call AgDetp NEW ( 'FPDM' );
        Call AgDetp ADD ( 'FPOS(imod=3).x=', -50.3, 1 );
        Call AgDetp ADD ( 'FPOS(imod=4).x=', +50.3, 1 );
     }

     if (FpdmConfig==0) {CONSTRUCT fpdmgeo; }
     if (FpdmConfig==1) {CONSTRUCT fpdmgeo1;}
     if (FpdmConfig==2) {CONSTRUCT fpdmgeo2;}    
     if (FpdmConfig==3) {CONSTRUCT fpdmgeo3;}
     if (FpdmConfig==4) {CONSTRUCT fpdmgeo4;}

   }

   IF (HCAL)   {
      IF HcalConfig==0  { CONSTRUCT HcalGeo; }
      IF HcalConfig==1  { CONSTRUCT HcalGeo1; }
      IF HcalConfig==15 { CONSTRUCT HcalGeoF; }
   }


   if (ZCAL)   { CONSTRUCT zcalgeo;}
   if (MAGP)   {
        call AgDetp NEW ('MAGP')
        call AgDetp ADD ('magg.version=', magpConfig, 1 );
        CONSTRUCT MagpGeo
   }


  Call AgSFlag('SIMU',2) ! Save hits from all secondaries in SSD, PXL, IST 

  if(SISD) {
       sisd_level=0
       call AgDETP new ('SISD')

* if SVT is present, position the SSD in it, otherwise need to position in CAVE (default)
       if(IDSM) { call AgDETP add ('ssdp.Placement=',1 ,1) };

* In the following, level means the version of the ssd geo code to be loaded
* It is the most important decimal place of the SisdConfig, and we just check
* for it here:

       if (SisdConfig>10) then
         sisd_level=SisdConfig/10
         if (sisd_level <= 5) SisdConfig=mod(SisdConfig,10)

         call AgDETP add ('ssdp.Config=',SisdConfig ,1)
         if     (sisd_level.eq.1) then
            CONSTRUCT sisdgeo1
         elseif (sisd_level.eq.2) then
            CONSTRUCT sisdgeo2
         elseif (sisd_level.eq.3) then
            CONSTRUCT sisdgeo3
         elseif (sisd_level.eq.4) then
            CONSTRUCT sisdgeo4
         elseif (sisd_level.eq.8) then
            CONSTRUCT sisdgeo7
         else
            CONSTRUCT sisdgeo6	!//only sisdgeo6 is used from now
         endif

       else
*        The original version (pretty much obsolete)
         call AgDETP add ('ssdp.Config=',SisdConfig ,1)
         CONSTRUCT sisdgeo
       endif

*       write(*,*) '*** Silicon Strip Detector Config and Code Level: ',SisdConfig, ' ',sisd_level

  } !!end SISD

   IF ISTD {
     IF IstdConfig==1 { CONSTRUCT istdgeo0; }
     IF IstdConfig==2 { CONSTRUCT istdgeo1; }
   }


   IF  PXST {
     IF PxstConfig==0 { CONSTRUCT pxstgeo1; }
   }


  Call AgSFlag('SIMU',1) ! Return to association of 2ndary hits on primary tracks 

   IF MUTD { 
     Call AgDetp NEW ('MUTD')
     IF MutdConfig=1 { CONSTRUCT mutdgeo; }
     IF MutdConfig=2 { CONSTRUCT mutdgeo2;}
     IF MutdConfig=3 { CONSTRUCT mutdgeo3;}
     IF MutdConfig=4 | MutdConfig=5 | MutdConfig=12 | MutdConfig=13 
     { 
         Call AgDetp ADD( 'MTDG.config=', MutdConfig, 1);   
         CONSTRUCT mutdgeo4;
     } 
     IF MutdConfig=14 {
         Call AgDetp ADD( 'MTDG.config=', MutdConfig, 1);   
         CONSTRUCT mutdgeo5;
     }

   }

   Call AgSFlag('SIMU',2) ! Save hits from all secondaries in SSD, PXL, IST 

   IF PIXL {
     IF PixlConfig==-1 { CONSTRUCT pixlgeo00; }
     IF PixlConfig==1  { CONSTRUCT pixlgeo;   }
     IF PixlConfig==2  { CONSTRUCT pixlgeo1;  }
     IF PixlConfig==3  { CONSTRUCT pixlgeo2;  }
     IF PixlConfig==4  { CONSTRUCT pixlgeo3;  }
     IF PixlConfig==5  {
           call AgDETP new ('PIXL')
           call AgDETP add ('PXLV.LadVer=',2.0,1)
           CONSTRUCT pixlgeo3;
     }
     IF PixlConfig==6 {
           call AgDetp new ('PIXL')
           call AgDetp add ('PXLV.LadVer=',2.0,1)
           call AgDetp add ('PXLV.location=',2.0,1)
           CONSTRUCT pixlgeo4;
     }


     IF PixlConfig==50 {               "Y2013 Pixel Configuration"
           call AgDetp new ('PIXL')
           call AgDetp add ('PXLW.LadrConfig=',   1.0, 1);
           call AgDetp add ('PXLW.SecVersion=',   7.0, 1); 
           CONSTRUCT PixlGeo5   """ Pixl Detector """
           CONSTRUCT DtubGeo1   """ Electronics etc... """  
IF (PSUP){ CONSTRUCT PsupGeo;}    """ Insertion structures """
     }
     IF PixlConfig==60 {               "Dev14 Pixel Configuration"
           call AgDetp new ('PIXL')
           call AgDetp add ('PXLW.SecVersion=',   1.0, 1); 
           call AgDetp add ('PXLW.LadrConfig=',   1.0, 1);
           CONSTRUCT PixlGeo6   """ Pixl Detector """
           CONSTRUCT DtubGeo1   """ Electronics etc... """
IF (PSUP){ CONSTRUCT PsupGeo;}    """ Insertion structures """
     }
     IF PixlConfig==62 {               "Dev14 Pixel Configuration"
           call AgDetp new ('PIXL')
           call AgDetp add ('PXLW.SecVersion=',   1.0, 1); 
           call AgDetp add ('PXLW.LadrConfig=',   2.0, 1);
           CONSTRUCT PixlGeo6   """ Pixl Detector """
           CONSTRUCT DtubGeo1   """ Electronics etc... """
IF (PSUP){ CONSTRUCT PsupGeo;}    """ Insertion structures """
     }


   }

   IF FTSD {
        CONSTRUCT FtsdGeo
   }


   IF ISTB {

     IF IstbConfig==-1 {CONSTRUCT istbgeo00;}
     IF IstbConfig== 1 {CONSTRUCT istbgeo;}
     IF IstbConfig== 2 {CONSTRUCT istbgeo1;}
     IF IstbConfig== 3 {CONSTRUCT istbgeo2;}
     IF IstbConfig== 4 {CONSTRUCT istbgeo3;}
     IF IstbConfig== 5 {CONSTRUCT istbgeo4;}
     IF IstbConfig== 6 {CONSTRUCT istbgeo5;}
     IF IstbConfig== 7 {CONSTRUCT istbgeo6;}
   }

   IF (GEMB.and.GembConfig>0)  {
      CONSTRUCT gembgeo;
   }

   if (FSTD.and.FstdConfig>0)  then
      if(FstdConfig==2) then
         call AgDETP new ('FSTD')
         call AgDETP add ('fstg.Rmax=',22.3,1)
      endif
      CONSTRUCT fstdgeo
   endif


   IF (FGTD) THEN                                            

     Call AgDETP new ('FGTD')                                     """Establish the interface to the geometry module"""

     if (FgtdConfig==1) {  CONSTRUCT fgtdgeo;  }! old, decomissioned
     if (FgtdConfig==2) {  CONSTRUCT fgtdgeo1; }
     if (FgtdConfig==3) {  CONSTRUCT fgtdgeo2; }

     IF FgtdConfig>30 {                                           """Apply FGT configuration and construct geometry"""
        IF FgtdConfig==31 { Call AgDETP add ( 'FGGG.FgstConfig=', 1.0, 1 );         CONSTRUCT FgtdGeo3; }
        IF FgtdConfig==32 { Call AgDetp add ( 'FGGG.FgstConfig=', 2.0, 1 );         CONSTRUCT FgtdGeo3; }
        IF FgtdConfig==55 {                                               ;         CONSTRUCT FgtdGeoV; }
        IF FgtdConfig==56 { Call AgDetp add ( 'FGGG.Fgstconfig=', 4.0, 1 );         CONSTRUCT FgtdGeoV; }
     }


   ENDIF

   Call AgSFlag('SIMU',1) ! Return to association of 2ndary hits on primary tracks 

   """The Foward Spaghetti Calorimeter"""    
   IF FSCE {  CONSTRUCT fscegeo;  }


   """The EIDD Geometry (TOF, TRD, East calo)"""
   IF EIDD {  CONSTRUCT eiddgeo;  }

   if (IGTD) then

     if(IgtdConfig==2) then
         call AgDETP new ('IGTD')
         call AgDETP add ('igtv.Config=',IgtdConfig ,1)
     endif
     CONSTRUCT igtdgeo
   endif

   if (HPDT.and.HpdtConfig>0) { write(*,*) 'HPDT';CONSTRUCT hpdtgeo;}
   if (ITSP)                  { write(*,*) 'ITSP';CONSTRUCT itspgeo;}

******************************************************************
* If PHMD is present and a non-zero version of the Photon Multiplicity Detector
* is defined, pass the version number to its constructor
* and create it:

   if  (PHMD.and.PhmdConfig>0) then
c     write(*,*) 'PHMD'
      call AgDETP new ('PHMD')
      call AgDETP add ('PMVR.Config=', PhmdConfig,1)
      CONSTRUCT phmdgeo
   endif


********************************************************************
   if(DUMM) then
      CONSTRUCT dummgeo
   endif
********************************************************************
   If (FhcmConfig .ne.0) then
c     write(*,*) 'FHCM'
      call AgDETP new ('FHCM')
      call AgDETP add ('fhcg.Version='   ,FhcmConfig,1)
      CONSTRUCT fhcmgeo
   endif
****************  Magnetic Field  ********************************
*
* - reset magnetic field value (default is 5): DETP MFLD MFLG.Bfield=5
   If (MFLD) {
!//       call AgDETP new ('MFLD')
!//       if (MFLD & magField!=5)   call AgDETP add ('MFLG(1).Bfield=' ,magField  ,1)
!//       if (MFLD & MfldConfig!=0) call AgDETP add ('MFLG(1).version=',MfldConfig,1)
!//       Call mfldgeo;
      CONSTRUCT mfldgeo(magField);
      call gufld(magX,magB);
      write(*,*) 'MFLD magField,Bz = ',magField,magB(3)
   }
*
   if JVOLUM>0
   { Call ggclos
     If IDEBUG>0 { CALL ICLRWK(0,1); Call GDRAWC('CAVE',1,.2,10.,10.,.03,.03)}
   }
   IDEBUG = IPRIN
   ITEST  = min(IPRIN,1)
   Call agphysi
*                      automatic subevent size selection
   If NtrSubev > 0
   { Call MZNEED(IXDIV,1000,'G')
     NLEFT    = max(10,IQUEST(11)/1200)
     MLEFT    = 10**Int(Alog10(Float(Nleft))-1)
     NtrSubEv = MLEFT*(NLEFT/MLEFT)
     Prin1 NtrSubEv; (' Ntrack per subevent = ',i6)
   }

c  ==================================================================================================
c  ==
c  == Save configuration data used in reconstruction
c  ==
c  ==================================================================================================
   Fill GDAT                     ! GEANT run data
      mfscale=magfield/5.0       ! magnetic field scale (nominal)
      gtag={geom(1:4),geom(5:8)} ! geometry tag 
   EndFill

   IF nfailed>0 {
      WRITE (*,*);      WRITE (*,*);       WRITE (*,*)
      WRITE (*,*) 'WARNING: (x)geometry.so was not built properly or not loaded.'
      WRITE (*,*) '         ', nloaded, ' modules loaded'
      WRITE (*,*) '         ', nfailed, ' modules failed to load'
      IF stop_on_fail > 0 { STOP; }
   }

   END
