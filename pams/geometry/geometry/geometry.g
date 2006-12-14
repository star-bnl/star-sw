* $Id: geometry.g,v 1.139 2006/12/14 21:36:25 potekhin Exp $
* $Log: geometry.g,v $
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
* balance effect study, i.e. it won't contains any realistic
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
* so as to include Lilian's code that were checked into CVS in
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
* Revision 1.122  2006/05/05 17:38:41  potekhin
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
* (a) add steering for the muon trigger system, "mutd"
* (b) specify a complete barrel calorimeter for Y2006
* (c) add steering for the corrected SSD (sisdgeo3)
* (d) add steering for a small modifications in the SVT shield in Y2006
*
* Revision 1.119  2006/01/18 23:06:13  potekhin
* Introducing the baseline year 2006 geometry, which is "the best"
* Y2005 geo plus the bugfix in the TPC backplane (not too important).
* Pending a better definition of the SSD from Lilian which we'll have
* to version and maybe cut new tags of 2004-5 geometries if there is
* a need.
*
* As agreed, created UPGR01 and UPGR02 (note cautious use of namespace),
* which are basically SSD+HFT and IST+HFT. Note that NONE of engineering
* detail is available in either case, and less so for the integration
* structural elements. We expect to do a lot of development in this area
* as new facts and engineering data are provided. This cut will allow us
* to proceed  with tracking studies in the near term.
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
* time to reconsile its geometry with the shield, and it's
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
* As per Janet's note, update the FTPC config
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
* Added the corrected SSD ladder positions (as per Lilian's
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
* the full barrel calorimeter as per Thomas' request
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
* well suited for this at all, and didn't allow
* proper versioning. Hence, the new MITT1 tag has
* been created, on same footing as COMPLETE but
* with a different structure.
*
* Revision 1.90  2004/05/10 21:49:38  potekhin
* For consistency, the SSD config in the y2004a tag should
* be equal 2 (even if the SSD data won't be used)
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
* as per requests of PWG's. Subject to final approval.
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
* as per Kai request -- won't affect anybody else
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
* which we'll likely need due to overlap of the PIXL (embedded
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
* prior corrections as well. Therefore, I'm changing
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
* spring'03 run, we introduce the tag Y2003C. It
* includes all the corrections first introduced in
* Y2003A, but also has the extra material in the
* SVT that we recently added in the GEANT model
* and which had been missing before.
*
* Revision 1.66  2003/10/10 23:59:18  potekhin
* Per Jerome's suggestion, which I agree with, we should
* semantically separate our current geometry tags from the
* future development ones. Also, it makes sense to have a more
* complete (as compared to 2003) geometry, for the pixel
* studies. Therefore, the pixel testbed tag has been
* renamed and the STAR detector more fully populated.
* It's now ASYMPT1, and the indexing may continue.
*
* Revision 1.65  2003/10/10 23:15:35  potekhin
* In previous check-n: forgot to mention the new CorrNum=3
* which actually programmatically modifies the inner radius of
* the shield in the SVT -- otherwise the pixel detector won't
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
* scale and the geometry tag"
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
* certain detectors such as svtt, ecal, calb into the minimal geometry,
* thus facilitating the creation of custom geometries on the fly --
* useful for debugging and detector exploration
*
* 3) Improved the phmd logic
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
* As per discussion with Jerome, I'm introducing
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
* purposes, this will be the actual Fall'03 geometry.
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
* Streamlined the btof config logic
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
* default btof forced to no TOF tray
*
* Revision 1.41  2001/03/15 01:14:20  nevski
* first approach to forward pion detector
*
* Revision 1.40  2001/03/13 20:56:31  nevski
* variable rich position taken from DB
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
*
***************************************************************************
   module geometry is the main steering module for the STAR geometry
   author  Pavel Nevski
   Created August 1998
*                                                                         *
*  Update history:                                                        *
*  08/19/98, PN: tof is not part of year_2a                               *
*  12/04/98, PN: rich  + upstream part + zero degree calo                 *
*  09/26/99, E.Cains: 1H geometry added - one svt ladder at layer 3       *
*  01/27/99, PN: rich in 1H geometry is simulated with hits is quartz & fr*
*  05/22/01, PN: starting with tag y2000 field is version 3 (direct map)  *
*  09/30/03, MP: see the many new CVS comments about recent check-ins     *
*  09/30/03, MP: converted the sub into a MODULE to allow for ZEBRA access*
***************************************************************************

   Structure  GDAT {real mfscale, char gtag(2)}

* list of system on/off switches:
   Logical    cave,pipe,svtt,sisd,tpce,ftpc,
              btof,vpdd,magp,calb,ecal,upst,
              rich,zcal,mfld,bbcm,fpdm,phmd,
              pixl,istb,gemb,fstd,ftro,fgtd,
              shld,quad,mutd,igtd,hpdt,itsp,dumm

* Qualifiers:  TPC        TOF         etc
   Logical    mwc,pse,ems,svtw,
              on/.true./,off/.false./




*  Codes:
*  1 - full ctb,         2 - full TOFp based tof,   3 - partial TOFp based tof,
*  4 - single TOFp tray, 5 - one TOFp and one TOFr, 6 - full TOFr based tof.

   real       Par(1000),field,dcay(5),shift(2),wdm

   Integer    LENOCC,LL,IPRIN,Nsi,NsiMin,i,j,l,kgeom,nmod(2),nonf(3),
              ecal_config, ecal_fill,
              sisd_level,
              Nleft,Mleft,Rv,Rp,Wfr,Itof,mwx,mf

***************** historical note: *********************8
* CorrNum allows us to control incremental bug fixes in a more
* organized manner -- ! Obsoleted 20050324 maxim! --


* The following are the versioning flags:

   Integer    DensConfig, SvttConfig, BtofConfig, VpddConfig, FpdmConfig, SisdConfig, PipeConfig,
              CalbConfig, PixlConfig, IstbConfig, GembConfig, FstdConfig, FtroConfig, ConeConfig,
              FgtdConfig, TpceConfig, PhmdConfig, SvshConfig, SupoConfig, FtpcConfig, CaveConfig,
              ShldConfig, QuadConfig, MutdConfig, HpdtConfig, IgtdConfig

*             DensConfig, ! TPC gas density correction
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

* Note that SisdConfig can take values in the tens, for example 20
* We do this to not proliferate additional version flags -- there has
* been a correction which resulted in new code.. We check the value
* and divide by 10 if necessary.

   character  Commands*4000,Geom*8

* - - - - - - - - - - - - - - - - -
+CDE,GCBANK,GCUNIT,GCPHYS,GCCUTS,GCFLAG,AGCKINE,QUEST.
*  temporarely until GCTLIT is not part of GCTMED:
   Integer        Thrind   ,Jmin,ItCkov,ImCkov,NpCkov
   common/GCTLIT/ Thrind(4),Jmin,ItCkov,ImCkov,NpCkov
* - - - - - - - - - - - - - - - - -

replace[;ON#{#;] with [
  IF Index(Commands,'#1')>0 
  { j=Index(Commands,'#1');  l=j+Lenocc('#1')-1; 
    if (Commands(j:j+3)=='YEAR') Geom=Commands(j:l); 
    if (Commands(j:j)  =='Y')    Geom=Commands(j:l); 
    Commands(j:l)=' ';  <W>; (' #1: #2');
]

* If geometry was already built, the local DB will be dropped completely now
* but the request for the next geometry should be saved in a temp. par arrray
   call ASLGETBA ('GEOM','DETP',1000,LL,Par)
   If (JVOLUM>0) call AGDROP ('*')

* -------------------- set GSTAR absolute default ------------------------
* before parsing the request, set some default values:
   IPRIN    = IDEBUG
   NtrSubEv = 1000     " automatic !"


   BtofConfig  = 1 ! ctb only
   CalbConfig  = 0 ! really make use of it starting in y2004
   CaveConfig  = 1 ! also -  a modified cave for shielding studies (2), or wider cave for muon detector (3)
   ConeConfig  = 1 ! 1 (def) old version, 2=more copper
   DensConfig  = 0 ! gas density correction
   FgtdConfig  = 0 ! 0=no, >1=version
   FpdmConfig  = 0 ! 0 means the original source code
   FstdConfig  = 0 ! 0=no, >1=version
   FtroConfig  = 0 ! 0=no, >1=version
   FtpcConfig  = 0 ! 0  version, 1=gas correction
   HpdtConfig  = 0 ! 0=no, >1=version
   IstbConfig  = 0 ! 0=no, >1=version
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

* Set only flags for the main configuration (everthing on, except for tof),
* but no actual parameters (CUTS,Processes,MODES) are set or modified here. 
* If an empty or no DETP GEOM was issued, geometry is defined externally.

   field=5               " default"

* "Canonical" detectors are all ON by default,
   {cave,pipe,svtt,tpce,ftpc,btof,vpdd,calb,ecal,magp,mfld,upst,zcal} = on;
* whereas some newer stuff is considered optional:
   {bbcm,fpdm,phmd,pixl,istb,gemb,fstd,sisd,ftro,fgtd,shld,quad,mutd,igtd,hpdt,itsp,dumm} = off;

   {mwc,pse}=on          " MultiWire Chambers, pseudopadrows              "
   {ems,rich}=off        " TimeOfFlight, EM calorimeter Sector            "
   Nsi=7; Wfr=0;  Wdm=0; " SVT+SSD, wafer number and width as in code     "
   NsiMin=1;             " the innermost layer of SVT                     "
   svtw=on               " water+water manifold in svt, off for Y2000 only"
   mwx=2                 " for Year_1? mwx=1 limites x in mwc hits (<Y2K) "
   Itof=2                " use btofgeo2 - default starting from Y2000     "
   Rv=2                  " add non-sensitive hits to RICH system          "
   Rp=2                  " real RICH position and spectra inst.of nominal "
   nonf={1,2,2}          " ecal on right side, FPD parts on left side     "
   ecal_config=0         " Ecal: east, west or both                       "
   ecal_fill=3           " full wheel by default                          "
   mf=2                  " default field - symmetrical, as fitted by Bill "
   Commands=' ';

   do kgeom=1,8
      Geom(kgeom:kgeom)='\0'; ! Initialize the string with NULLs for strcpy safety
   enddo;

* -------------------- select USERS configuration ------------------------
* On a non-empty DETP GEOM every keyword results in an action and is erased.
*
* Actions consist here of selecting the appropriate parameteres and flags.
* This flags are used in the next section to create subsystems and 
* to communicate DETP commands with parameters to them.
* 

If LL>1   
{ Call AGSFLAG  ('GEOM',1)
* convert input line into a string of upprecase characters
  CALL UHTOC(PAR(2),4,Commands,LL*4-4);  Call CLTOU(Commands);


* set geant processes and cuts only if any detp geometry was issued:
  {CUTGAM,CUTELE,CUTNEU,CUTHAD,CUTMUO,BCUTE,BCUTM,DCUTE,DCUTM,PPCUTM} =.001;
  {IDCAY,IANNI,IBREM,ICOMP,IHADR,IMUNU,IPAIR,IPHOT,ILOSS,IDRAY,IMULS} = 1;
  {IRAYL,ISTRA} = 0; 
  TOFMAX        = 1.e-4 
*
  for(j=1;j>0;) { j=0;
  on HELP       { you may select the following keywords: ;
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
                }  

  on YEAR_2B    { old 2001 geometry first guess - TPC+CTB+FTPC+RICH+CaloPatch+SVT;
                  BtofConfig=4;
                  {rich,ems}=on;  nmod={24,0}; shift={21,0};  
                  nonf={0,2,2};  Itof=2;  Rv=2;                        Nsi=6; }

  on YEAR_2A    { old asymptotic STAR;    Itof=1; mwx=1;  bbcm=on;            }

*************************************************************************************************************
* as complete as Y2003X below but with all corrections AND pixel detector
*************************************************************************************************************
  on COMPLETE  { New Complete + correction 3 in 2003 geometry: TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL+PHMD;
                  "svt: 3 layers ";
                     nsi=7  " 3 bi-plane layers + ssd ";
                     wfr=0  " numbering is in the code   ";
                     wdm=0  " width is in the code      ";
                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";
                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
                     BtofConfig=5;
                  "calb" 
                     ems=on
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ecal"
                     ecal_config=3   "both wheels"
                     ecal_fill=3     "all sectors filled "
                  "beam-beam counter "
                     bbcm=on
                  "forward pion detector "
                     fpdm=on
                  "field version "
                     Mf=4;      "tabulated field, with correction "

*                    -- obsoleted 20050324 maxim -- CorrNum = 4;
                     SvshConfig = 1; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 4;

                  "Photon Multiplicity Detector Version "
                     phmd=on;
                     PhmdConfig = 1;
                  "Silicon Strip Detector Version "
                     sisd=off;
                     SisdConfig = 1;
* careful! Achtung!
                   pipeConfig=4;   " provisional"
                   pixl=on;    " put the pixel detector in"
                   PixlConfig=1;
                }

*************************************************************************************************************
  on IST1   { New Tracking: TPC+CTB+CaloPatch2+SVT3+BBC+FPD+ECAL+PHMD+ISTB+FSTD+FGTD;

                     svtt=off; "no SVT  at all in this configuration"
                     ftpc=off; "no FTPC at all in this configuration"
                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";
                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
                     BtofConfig=5;
                  "calb" 
                     ems=on
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ecal"
                     ecal_config=1   " west wheel "
                     ecal_fill=3     " all sectors filled "
                  "beam-beam counter "
                     bbcm=on
                  "forward pion detector "
                     fpdm=on
                  "field version "
                     Mf=4;      "tabulated field, with correction "
*                    -- Obsoleted: CorrNum = 4;
                     SvshConfig = 1; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 4;

                  "Photon Multiplicity Detector Version "
                     phmd=on;
                     PhmdConfig = 1;
                  "Silicon Strip Detector Version "
                     sisd=on;
                     SisdConfig = 23;
* careful! Achtung!
                   pipeConfig=4;   " provisional"
                   pixl=on;        " put the pixel detector in"
                   PixlConfig=2;   " newer version decoupled from SVT"
* Inner STAR tracker barrel
                   istb=on;  "new pixel based inner tracker"
                   IstbConfig=1;
* Inner STAR GEM barrel
                   gemb=on;  
                   GembConfig=1;
* Forward STAR tracker disk
                   fstd=on;  "new pixel based forward tracker"
                   FstdConfig=1;
* Forward STAR tracker disk
                   fgtd=on;  "GEM forward tracker"
                   FgtdConfig=1;
                }


* corrected: MWC readout, RICH reconstructed position, no TOF 
  on YEAR2000   { actual 2000:  TPC+CTB+RICH+caloPatch+svtLadder; 
                  {vpdd,ecal,ftpc,svtw}=off; {rich,ems}=on; Field=2.5; 
                  nmod={12,0}; shift={87,0}; Rp=2; Rv=2; Wfr=7; Mf=3;  Nsi=-3;}

  on YEAR2001   { 2001 geometry - TPC+CTB+FTPC+RICH+CaloPatch+SVT+FPD;

* 02/09/2004  Jerome signed off on changing, retroactively, the
* position of the wafers in year2001, which was incorrectly offset
* by 250 um insterad of 150 um.

*                    -- Obsoleted CorrNum = 1;
                     SvttConfig = 1; "SVTT version"
                     SupoConfig = 1; "FTPC Support"

                  BtofConfig=4;
                  {rich,ems}=on;

* a newer way to steer ecal:
                  ecal_config=1   " one ecal patch, west "

* this was put here in recent versions (as of 1.50) and I believe this is wrong as
* it destroys compatibility with earlier code: --max--
*    ecal=off;  
                  nmod={24,0}; shift={21,0}; Itof=2; Rv=2; Mf=3;       Nsi=6; }  
                
****************************************************************************************
  on YEAR2002   { january 2002 geometry - TPC+CTB+FTPC+CaloPatch2+Rich+SVT3+BBC+FPD;
                  "svt: 3 layers ";
                     nsi=6        " 3 bi-plane layers, nsi<=7 ";
                     wfr=0        " numbring is in the code   ";
                     wdm=0        " width is in the code      ";
                  "tpc: standard, i.e.  "
                     mwc=on       " Wultiwire chambers are read-out ";
                     pse=on       " inner sector has pseudo padrows ";
                  "rich"
                     rich=on      " have rich ";
                     Rv=2;        " save additional (fake) hits "; 
                  "ctb: central trigger barrer ";
                     Itof=2       " call btofgeo2 ";
                     BtofConfig=4;
                  "calb: barrel calorimeter "
                     ems=on       " sector version "
                     nmod={24,0}  " 24 sectors ";
                     shift={21,0} " starting from 21         "; 
                  "ecal "
                     ecal=off
                  "beam-beam counter "
                     bbcm=on
                  "forward pion detector "
                     fpdm=on
                  "pseudo Vertex Position Detector"
                     vpdd=on;
                     VpddConfig=2;
                  "field version "
                     Mf=4;      " tabulated field, with correction ";
                }
****************************************************************************************
  on YEAR2003   { draft 2003 geometry - TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL;
                  "svt: 3 layers ";
                     nsi=6  " 3 bi-plane layers, nsi<=7 ";
                     wfr=0  " numbering is in the code   ";
                     wdm=0  " width is in the code      ";
                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";
                  "ctb: central trigger barrer             ";
                     Itof=2 "  btofgeo2  ";
                     BtofConfig=5;
                  "calb" 
                     ems=on   "endcap "
                     nmod={60,0}; shift={0,0}; " 60 sectors "
                  "ecal" 
                     ecal_config=1   "one ecal patch, west "
                     ecal_fill=1     " sectors 2-5 filled "
                  "beam-beam counter "
                     bbcm=on
                  "forward pion detector "
                     fpdm=on
                  "pseudo Vertex Position Detector"
                     vpdd=on;
                     VpddConfig=3;
                  "field version "
                     Mf=4;      "tabulated field, with correction "
                }


***********************************************************************
* In y2003a:
*    removed serious bugs from SUPOGEO (incorrect positioning inside the SVT,
*    where is doesn't belong)
*    corrected CALB -- the shift variable (was 0,0 so the barrel wasn't tilted right)
*    corrected SVT  -- the layer radii (from 250 to 150 microns, see the svt code)
****************************************************************************************
  on Y2003A    { correction 1 in 2003 geometry - TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL;
                  "svt: 3 layers ";
                     nsi=6  " 3 bi-plane layers, nsi<=7 ";
                     wfr=0  " numbering is in the code   ";
                     wdm=0  " width is in the code      ";
                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";
                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
                     BtofConfig=5;
                  "calb" 
                     ems=on
                     nmod={60,0}; shift={75,0}; " 60 sectors " 
                  "ecal"
                     ecal_config=1   " one ecal patch, west "
                     ecal_fill=1     " sectors 2-5 filled "
                  "beam-beam counter "
                     bbcm=on
                  "forward pion detector "
                     fpdm=on
                  "pseudo Vertex Position Detector"
                     vpdd=on;
                     VpddConfig=3;
                  "field version "
                     Mf=4;      "tabulated field, with correction "
*                    -- Obsoleted CorrNum = 1;
                     SvttConfig = 1; "SVTT version"
                     SupoConfig = 1; "FTPC Support"
                }
***********************************************************************
* y2003b is y2003a, but with the extra material in the SVT
* This is actually an important case (i.e. the "most precise" geometry
* approximation for the early 2003 run) which we were lacking so far.
* This is achieved by setting CorrNum to 2.
* The endcap EMC has one third of one wheel, as before
* For more info on the extra material in SVT -- see web page
****************************************************************************************
  on Y2003B    { correction 2 in 2003 geometry: TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL;
                  "svt: 3 layers ";
                     nsi=6  " 3 bi-plane layers, nsi<=7 ";
                     wfr=0  " numbering is in the code   ";
                     wdm=0  " width is in the code      ";
                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";
                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
                     BtofConfig=5;
                  "calb" 
                     ems=on
                     nmod={60,0}; shift={75,0}; " 60 sectors " 
                  "ecal"
                     ecal_config=1   " one ecal patch, west "
                     ecal_fill=1     " sectors 2-5 filled "
                  "beam-beam counter "
                     bbcm=on
                  "forward pion detector "
                     fpdm=on
                  "pseudo Vertex Position Detector"
                     vpdd=on;
                     VpddConfig=3;
                  "field version "
                     Mf=4;      "tabulated field, with correction "
*                    -- Obsoleted CorrNum = 2;
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 2; "SVTT version"
                }
****************************************************************************************
  on Y2003C    { Better SVT model on top of 2003B: TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL;
                  "svt: 3 layers ";
                     nsi=6  " 3 bi-plane layers, nsi<=7 ";
                     wfr=0  " numbering is in the code   ";
                     wdm=0  " width is in the code      ";
                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";
                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
                     BtofConfig=5;
                  "calb" 
                     ems=on
                     nmod={60,0}; shift={75,0}; " 60 sectors " 
                  "ecal"
                     ecal_config=1   " one ecal patch, west "
                     ecal_fill=1     " sectors 2-5 filled "
                  "beam-beam counter "
                     bbcm=on
                  "forward pion detector "
                     fpdm=on
                  "pseudo Vertex Position Detector"
                     vpdd=on;
                     VpddConfig=3;
                  "field version "
                     Mf=4;      "tabulated field, with correction "
*                    -- Obsoleted CorrNum = 2;
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 6; "SVTT version"
                }

****************************************************************************************
  on Y2003X    { same as y2003b but with full calorimeters and phmd
                  "svt: 3 layers ";
                     nsi=6  " 3 bi-plane layers, nsi<=7 ";
                     wfr=0  " numbering is in the code   ";
                     wdm=0  " width is in the code      ";
                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";
                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
                     BtofConfig=5;
                  "calb" 
                     ems=on ;
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides" 
                  "ecal" 
                     ecal_config=3   "both wheels"
                     ecal_fill=3     "all sectors filled "
                  "beam-beam counter "
                     bbcm=on
                  "forward pion detector "
                     fpdm=on
                  "pseudo Vertex Position Detector"
                     vpdd=on;
                     VpddConfig=3;
                  "field version "
                     Mf=4;      "tabulated field, with correction "
*                    -- Obsoleted CorrNum = 2;
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 2; "SVTT version"

                  "Photon Multiplicity Detector Version "
                     phmd=on;
                     PhmdConfig = 1;
                }

****************************************************************************************
* NOTE:  this geometry, y2004x, should logically follow the baseline y2004, as a block of code,
* however the parser we used isn't too good and it grabs the y2004 and then
* stumbles, hence the order of tags is sometimes important and we have to list
* the y2004x prior to y2004

  on Y2004X    { hypothetical 2004 geometry: full barrel. Standard cuts in PHMD.;
                  "svt: 3 layers ";
                     nsi=6  " 3 bi-plane layers, nsi<=7 ";
                     wfr=0  " numbering is in the code   ";
                     wdm=0  " width is in the code      ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
* note the upgrade with respect to previous years:
                     BtofConfig=7;

* note the full barrel same as in y2003x:
                  "calb" 
                     ems=on ;
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides" 

                  "ecal"
                     ecal_config=1   " one ecal patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     bbcm=on

                  "forward pion detector "
                     fpdm=on
                     FpdmConfig  = 1 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     vpdd=on;
                     VpddConfig=4;

                  "field version "
                     Mf=4;      "tabulated field, with correction "

*                    -- Obsoleted CorrNum = 3;
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 3; "SVTT version"
                     DensConfig = 1; "gas density correction"

                  "Photon Multiplicity Detector Version "
                     phmd=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     sisd=on;
                     SisdConfig = 2;
                }

****************************************************************************************
  on Y2004Y    { same as Y2004X but with the SVT chip correction+cone+better SSD+TPC backplane+FTRO
                  "svt: 3 layers ";
                     nsi=6  " 3 bi-plane layers, nsi<=7 ";
                     wfr=0  " numbering is in the code   ";
                     wdm=0  " width is in the code      ";

                     ConeConfig=2 " new cable weight estimate ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
* note the upgrade with respect to previous years:
                     BtofConfig=7;

* note the full barrel same as in y2003x:
                  "calb" 
                     ems=on ;
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides" 

                  "ecal"
                     ecal_config=1   " one ecal patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     bbcm=on

                  "forward pion detector "
                     fpdm=on
                     FpdmConfig  = 1 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     vpdd=on;
                     VpddConfig=4;

                  "field version "
                     Mf=4;      "tabulated field, with correction "

                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 4; "SVTT version"
                     DensConfig = 1; "gas density correction"
                     FtpcConfig = 1; "ftpc configuration"
* Above:  Ar+C02 in ftpc

                  "Photon Multiplicity Detector Version "
                     phmd=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     sisd=on;
                     SisdConfig = 22;

                  "FTPC Readout barrel "
                     ftro=on;
                     FtroConfig = 1;

                  "New version of the TPC backplane "
                     TpceConfig = 2;

                }

****************************************************************************************
  on Y2004A    { baseline 2004 geometry: TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL+PHMD with standard GSTPAR in PHMD; 
                  "svt: 3 layers ";
                     nsi=6  " 3 bi-plane layers, nsi<=7 ";
                     wfr=0  " numbering is in the code   ";
                     wdm=0  " width is in the code      ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
* note the upgrade with respect to previous years:
                     BtofConfig=7;

                  "calb" 
                     ems=on
                     CalbConfig = 1
* remember that with this config, the following parameters have
* a different meaning because we have to (unfortunately) switch
* from divisions to copies and introduce a map, which DOES
* control the configuration
                     nmod={60,60}; shift={75,105}; " 60 sectors West plus 30 East split between 2 halves"

                  "ecal"
                     ecal_config=1   " one ecal patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     bbcm=on

                  "forward pion detector "
                     fpdm=on
                     FpdmConfig  = 1 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     vpdd=on;
                     VpddConfig=4;

                  "field version "
                     Mf=4;      "tabulated field, with correction "

*                    -- Obsoleted CorrNum = 3;
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 3; "SVTT version"
                     DensConfig = 1; "gas density correction"

                  "Photon Multiplicity Detector Version "
                     phmd=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     sisd=on;
                     SisdConfig = 2;
                }

*
****************************************************************************************
  on Y2004B    { corrected 2004 geometry: TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL+PHMD with standard GSTPAR in PHMD;
                  "svt: 3 layers ";
                     nsi=6  " 3 bi-plane layers, nsi<=7 ";
                     wfr=0  " numbering is in the code   ";
                     wdm=0  " width is in the code      ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
* note the upgrade with respect to previous years:
                     BtofConfig=7;

                  "calb" 
                     CalbConfig = 1  " Please see note in Y2004A"
                     nmod={60,60}; shift={75,105}; " 60 sectors West plus 30 East split between 2 halves"

                  "ecal"
                     ecal_config=1   " one ecal patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     bbcm=on

                  "forward pion detector "
                     fpdm=on
                     FpdmConfig  = 1 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     vpdd=on;
                     VpddConfig=4;

                  "field version "
                     Mf=4;      "tabulated field, with correction "

*                    -- Obsoleted CorrNum = 3;
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 3; "SVTT version"
                     DensConfig = 1; "gas density correction"

                  "Photon Multiplicity Detector Version "
                     phmd=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     sisd=on;
                     SisdConfig = 12;

                }

****************************************************************************************
  on Y2004C    { same as Y2004B but with the SVT chip correction+cone+better SSD+TPC backplane+FTRO
                  "svt: 3 layers ";
                     nsi=6  " 3 bi-plane layers, nsi<=7 ";
                     wfr=0  " numbering is in the code   ";
                     wdm=0  " width is in the code      ";

                     ConeConfig=2 " new cable weight estimate ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
* note the upgrade with respect to previous years:
                     BtofConfig=7;

                  "calb" 
                     CalbConfig = 1  " Please see note in Y2004A"
                     nmod={60,60}; shift={75,105}; " 60 sectors West plus 30 East split between 2 halves"

                  "ecal"
                     ecal_config=1   " one ecal patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     bbcm=on

                  "forward pion detector "
                     fpdm=on
                     FpdmConfig  = 1 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     vpdd=on;
                     VpddConfig=4;

                  "field version "
                     Mf=4;      "tabulated field, with correction "


                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 4; "SVTT version"
                     DensConfig = 1; "gas density correction"
                     FtpcConfig = 1; "ftpc configuration"
* Above:  Ar+C02 in ftpc


                  "Photon Multiplicity Detector Version "
                     phmd=on;
                     PhmdConfig = 2;

* second file version, 10 ladders
                  "Silicon Strip Detector Version "
                     sisd=on;
                     SisdConfig = 22;

                  "FTPC Readout barrel "
                     ftro=on;
                     FtroConfig = 1;

                  "New version of the TPC backplane "
                     TpceConfig = 2;
                }

****************************************************************************************
  on Y2004D    { Better SVT on top of Y2004B
                  "svt: 3 layers ";
                     nsi=6  " 3 bi-plane layers, nsi<=7 ";
                     wfr=0  " numbering is in the code   ";
                     wdm=0  " width is in the code      ";

                     ConeConfig=2 " new cable weight estimate ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
* note the upgrade with respect to previous years:
                     BtofConfig=7;

                  "calb" 
                     CalbConfig = 1  " Please see note in Y2004A"
                     nmod={60,60}; shift={75,105}; " 60 sectors West plus 30 East split between 2 halves"

                  "ecal"
                     ecal_config=1   " one ecal patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     bbcm=on

                  "forward pion detector "
                     fpdm=on
                     FpdmConfig  = 1 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     vpdd=on;
                     VpddConfig=4;

                  "field version "
                     Mf=4;      "tabulated field, with correction "


                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 6; "SVTT version"
                     DensConfig = 1; "gas density correction"
                     FtpcConfig = 1; "ftpc configuration"
* Above:  Ar+C02 in ftpc


                  "Photon Multiplicity Detector Version "
                     phmd=on;
                     PhmdConfig = 2;

* second file version, 10 ladders
                  "Silicon Strip Detector Version "
                     sisd=on;
                     SisdConfig = 22;

                  "FTPC Readout barrel "
                     ftro=on;
                     FtroConfig = 1;

                  "New version of the TPC backplane "
                     TpceConfig = 2;
                }

*
*
**********************************************************************
* Corrections and enhancements in y2004:
*    added the Photon Multiplicity Detector (PHMD)
*    The endcap EMC has one complete wheel in the west
*    To be done: 3/4 of the second half of the barrel!
*
*                >>>THIS IS THE MASTER GEOMETRY FOR THE SPRING'04<<<
*
****************************************************************************************
  on Y2004     { baseline 2004 geometry: TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL+PHMD with low cuts GSTPAR in PHMD; ; 
                  "svt: 3 layers ";
                     nsi=6  " 3 bi-plane layers, nsi<=7 ";
                     wfr=0  " numbering is in the code   ";
                     wdm=0  " width is in the code      ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
* note the upgrade with respect to previous years:
                     BtofConfig=7;

                  "calb" 
                     ems=on
                     CalbConfig = 1
* remember that with this config, the following parameters have
* a different meaning because we have to (unfortunately) switch
* from divisions to copies and introduce a map, which DOES
* control the configuration
                     nmod={60,60}; shift={75,105}; " 60 sectors West plus 30 East split between 2 halves"


                  "ecal"
                     ecal_config=1   " one ecal patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     bbcm=on

                  "forward pion detector "
                     fpdm=on
                     FpdmConfig  = 1 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     vpdd=on;
                     VpddConfig=4;

                  "field version "
                     Mf=4;      "tabulated field, with correction "

*                    -- Obsoleted CorrNum = 3;
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 3; "SVTT version"
                     DensConfig = 1; "gas density correction"

                  "Photon Multiplicity Detector Version "
                     phmd=on;
                     PhmdConfig = 1;

                  "Silicon Strip Detector Version "
                     sisd=on;
                     SisdConfig = 2;
                }

****************************************************************************************
  on Y2005X    { first cut of full CALB 2005 geometry: TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL+PHMD_FTRO;
                  "svt: 3 layers ";
                     nsi=6  " 3 bi-plane layers, nsi<=7 ";
                     wfr=0  " numbering is in the code   ";
                     wdm=0  " width is in the code      ";

                     ConeConfig=2 " new cable weight estimate ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
* note the upgrade with respect to previous years:
                     BtofConfig=7;

* note the full barrel same as in y2003x:
                  "calb" 
                     ems=on ;
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides" 

                  "ecal"
                     ecal_config=1   " one ecal patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     bbcm=on

                  "forward pion detector "
                     fpdm=on
                     FpdmConfig  = 1 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     vpdd=on;
                     VpddConfig=4;

                  "field version "
                     Mf=4;      "tabulated field, with correction "

*                    -- Obsoleted CorrNum = 3;
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 3; "SVTT version"
                     DensConfig = 1; "gas density correction"

                  "Photon Multiplicity Detector Version "
                     phmd=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     sisd=on;
                     SisdConfig = 23; "second version, full barrel"


                  "FTPC Readout barrel "
                     ftro=on;
                     FtroConfig = 1;

                }

****************************************************************************************
  on Y2005B    { TPC,FTPC,SVT and SSD correction of 2005 geometry
                  "svt: 3 layers ";
                     nsi=6  " 3 bi-plane layers, nsi<=7 ";
                     wfr=0  " numbering is in the code   ";
                     wdm=0  " width is in the code      ";

                     ConeConfig=2 " new cable weight estimate ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
* note the upgrade with respect to previous years:
                     BtofConfig=7;

                  "calb" 
                     ems=on
                     CalbConfig = 1
* remember that with this config, the following parameters have
* a different meaning because we have to (unfortunately) switch
* from divisions to copies and introduce a map, which DOES
* control the configuration
                     nmod={60,60}; shift={75,105}; " 60 sectors West plus 30 East split between 2 halves"

                  "ecal"
                     ecal_config=1   " one ecal patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     bbcm=on

                  "forward pion detector "
                     fpdm=on
                     FpdmConfig  = 1 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     vpdd=on;
                     VpddConfig=4;

                  "field version "
                     Mf=4;      "tabulated field, with correction "

* important: (1) new SVT version (2) FTPC gas correction tp Ar+C02 mix (3) SSD ladders raddi correction

                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 4; "SVTT version"
                     DensConfig = 1; "gas density correction"
                     FtpcConfig = 1; "ftpc configuration"

                  "Photon Multiplicity Detector Version "
                     phmd=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     sisd=on;
                     SisdConfig = 24; "second version, full barrel with corrected radii"


                  "FTPC Readout barrel "
                     ftro=on;
                     FtroConfig = 1;

                  "New version of the TPC backplane "
                     TpceConfig = 2;

                }

****************************************************************************************
  on Y2005C    { TPC,FTPC,SVT and SSD correction of 2005 geometry
                  "svt: 3 layers ";
                     nsi=6  " 3 bi-plane layers, nsi<=7 ";
                     wfr=0  " numbering is in the code   ";
                     wdm=0  " width is in the code      ";

                     ConeConfig=2 " new cable weight estimate ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=4 " call btofgeo4 ";
* NEW CONFIG!
                     BtofConfig=8;

                  "calb" 
                     ems=on
                     CalbConfig = 1
* remember that with this config, the following parameters have
* a different meaning because we have to (unfortunately) switch
* from divisions to copies and introduce a map, which DOES
* control the configuration
                     nmod={60,60}; shift={75,105}; " 60 sectors West plus 30 East split between 2 halves"

                  "ecal"
                     ecal_config=1   " one ecal patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     bbcm=on

                  "forward pion detector "
                     fpdm=on
                     FpdmConfig  = 1 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     vpdd=on;
                     VpddConfig=4;

                  "field version "
                     Mf=4;      "tabulated field, with correction "

* important: (1) new SVT version (2) FTPC gas correction tp Ar+C02 mix (3) SSD ladders raddi correction

                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 4; "SVTT version"
                     DensConfig = 1; "gas density correction"
                     FtpcConfig = 1; "ftpc configuration"

                  "Photon Multiplicity Detector Version "
                     phmd=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     sisd=on;
                     SisdConfig = 24; "second version, full barrel with corrected radii"


                  "FTPC Readout barrel "
                     ftro=on;
                     FtroConfig = 1;

                  "New version of the TPC backplane "
                     TpceConfig = 2;

                }

****************************************************************************************
  on Y2005D    { Better SVT on top of Y2005C
                  "svt: 3 layers ";
                     nsi=6  " 3 bi-plane layers, nsi<=7 ";
                     wfr=0  " numbering is in the code   ";
                     wdm=0  " width is in the code      ";

                     ConeConfig=2 " new cable weight estimate ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=4 " call btofgeo4 ";
* NEW CONFIG!
                     BtofConfig=8;

                  "calb" 
                     ems=on
                     CalbConfig = 1
* remember that with this config, the following parameters have
* a different meaning because we have to (unfortunately) switch
* from divisions to copies and introduce a map, which DOES
* control the configuration
                     nmod={60,60}; shift={75,105}; " 60 sectors West plus 30 East split between 2 halves"

                  "ecal"
                     ecal_config=1   " one ecal patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     bbcm=on

                  "forward pion detector "
                     fpdm=on
                     FpdmConfig  = 1 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     vpdd=on;
                     VpddConfig=4;

                  "field version "
                     Mf=4;      "tabulated field, with correction "

* important: (1) new SVT version (2) FTPC gas correction tp Ar+C02 mix (3) SSD ladders raddi correction

                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 6; "SVTT version"
                     DensConfig = 1; "gas density correction"
                     FtpcConfig = 1; "ftpc configuration"

                  "Photon Multiplicity Detector Version "
                     phmd=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     sisd=on;
                     SisdConfig = 24; "second version, full barrel with corrected radii"


                  "FTPC Readout barrel "
                     ftro=on;
                     FtroConfig = 1;

                  "New version of the TPC backplane "
                     TpceConfig = 2;

                }

****************************************************************************************
  on Y2005E    { Better SVT, bigger shield and SSD on top of Y2005C, and full barrel calorimeter
                  "svt: 3 layers ";
                     nsi=6  " 3 bi-plane layers, nsi<=7 ";
                     wfr=0  " numbering is in the code   ";
                     wdm=0  " width is in the code      ";

                     ConeConfig=2 " new cable weight estimate ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=4 " call btofgeo4 ";
* NEW CONFIG!
                     BtofConfig=8;

* note the full barrel same as in y2003x:
                  "calb" 
                     ems=on ;
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides" 

                  "ecal"
                     ecal_config=1   " one ecal patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     bbcm=on

                  "forward pion detector "
                     fpdm=on
                     FpdmConfig  = 1 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     vpdd=on;
                     VpddConfig=4;

                  "field version "
                     Mf=4;      "tabulated field, with correction "

* important: (1) new SVT version (2) FTPC gas correction tp Ar+C02 mix (3) SSD ladders raddi correction

                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 6; "SVTT version"
                     SvshConfig = 2; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     FtpcConfig = 1; "ftpc configuration"

                  "Photon Multiplicity Detector Version "
                     phmd=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     sisd=on;
                     SisdConfig = 35; "second version, full barrel with corrected radii"


                  "FTPC Readout barrel "
                     ftro=on;
                     FtroConfig = 1;

                  "New version of the TPC backplane "
                     TpceConfig = 2;

                }

****************************************************************************************
  on Y2005    { first cut of 2005 geometry: TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL+PHMD_FTRO;
                  "svt: 3 layers ";
                     nsi=6  " 3 bi-plane layers, nsi<=7 ";
                     wfr=0  " numbering is in the code   ";
                     wdm=0  " width is in the code      ";

                     ConeConfig=2 " new cable weight estimate ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
* note the upgrade with respect to previous years:
                     BtofConfig=7;

                  "calb" 
                     ems=on
                     CalbConfig = 1
* remember that with this config, the following parameters have
* a different meaning because we have to (unfortunately) switch
* from divisions to copies and introduce a map, which DOES
* control the configuration
                     nmod={60,60}; shift={75,105}; " 60 sectors West plus 30 East split between 2 halves"

                  "ecal"
                     ecal_config=1   " one ecal patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     bbcm=on

                  "forward pion detector "
                     fpdm=on
                     FpdmConfig  = 1 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     vpdd=on;
                     VpddConfig=4;

                  "field version "
                     Mf=4;      "tabulated field, with correction "

*                    -- Obsoleted CorrNum = 3;
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 3; "SVTT version"
                     DensConfig = 1; "gas density correction"

                  "Photon Multiplicity Detector Version "
                     phmd=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     sisd=on;
                     SisdConfig = 23; "second version, full barrel"


                  "FTPC Readout barrel "
                     ftro=on;
                     FtroConfig = 1;

                }

****************************************************************************************
  on Y2006A   { Year 2006 baseline which is Y2005D+fixed TPC backplane+New SSD
                  "svt: 3 layers ";
                     nsi=6  " 3 bi-plane layers, nsi<=7 ";
                     wfr=0  " numbering is in the code   ";
                     wdm=0  " width is in the code      ";

                     ConeConfig=2 " new cable weight estimate ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=4 " call btofgeo4 ";
* NEW CONFIG!
                     BtofConfig=8;

* Full barrel in 2006
                  "calb" 
                     ems=on ;
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides" 


                  "ecal"
                     ecal_config=1   " one ecal patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     bbcm=on

                  "forward pion detector "
                     fpdm=on
                     FpdmConfig  = 2 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     vpdd=on;
                     VpddConfig=4;

                  "field version "
                     Mf=4;      "tabulated field, with correction "

* important: (1) new SVT version (2) FTPC gas correction tp Ar+C02 mix (3) SSD ladders raddi correction

                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 6; "SVTT version"
                     SvshConfig = 2; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     FtpcConfig = 1; "ftpc configuration"

                  "Photon Multiplicity Detector Version "
                     phmd=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     sisd=on;
                     SisdConfig = 35; "third version, full barrel newly corrected radii"


                  "FTPC Readout barrel "
                     ftro=on;
                     FtroConfig = 1;

                  "New version of the TPC backplane "
                     TpceConfig = 3;
                  "Muon Trigger System"
                     mutd = on;
                     MutdConfig = 1;
                  "We need a bigger Cave"
                     CaveConfig = 3;
                }
****************************************************************************************
  on Y2006    { Year 2006 baseline which is Y2005D+fixed TPC backplane+New SSD
                  "svt: 3 layers ";
                     nsi=6  " 3 bi-plane layers, nsi<=7 ";
                     wfr=0  " numbering is in the code   ";
                     wdm=0  " width is in the code      ";

                     ConeConfig=2 " new cable weight estimate ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=4 " call btofgeo4 ";
* NEW CONFIG!
                     BtofConfig=8;

* Full barrel in 2006
                  "calb" 
                     ems=on ;
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides" 


                  "ecal"
                     ecal_config=1   " one ecal patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     bbcm=on

                  "forward pion detector "
                     fpdm=on
                     FpdmConfig  = 1 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     vpdd=on;
                     VpddConfig=4;

                  "field version "
                     Mf=4;      "tabulated field, with correction "

* important: (1) new SVT version (2) FTPC gas correction tp Ar+C02 mix (3) SSD ladders raddi correction

                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 6; "SVTT version"
                     SvshConfig = 2; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     FtpcConfig = 1; "ftpc configuration"

                  "Photon Multiplicity Detector Version "
                     phmd=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     sisd=on;
                     SisdConfig = 35; "third version, full barrel newly corrected radii"


                  "FTPC Readout barrel "
                     ftro=on;
                     FtroConfig = 1;

                  "New version of the TPC backplane "
                     TpceConfig = 3;
                  "Muon Trigger System"
                     mutd = on;
                     MutdConfig = 1;
                  "We need a bigger Cave"
                     CaveConfig = 3;
                }
****************************************************************************************
  on Y2007    { Year 2006 baseline which is Y2006+FMS
                  "svt: 3 layers ";
                     nsi=6  " 3 bi-plane layers, nsi<=7 ";
                     wfr=0  " numbering is in the code   ";
                     wdm=0  " width is in the code      ";

                     ConeConfig=2 " new cable weight estimate ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=4 " call btofgeo4 ";
* NEW CONFIG!
                     BtofConfig=8;

* Full barrel in 2006
                  "calb" 
                     ems=on ;
                     CalbConfig = 2
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides" 
                  "ecal"
                     ecal_config=1   " one ecal patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     bbcm=on

                  "forward pion detector "
                     fpdm=on
                     FpdmConfig  = 3 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     vpdd=on;
                     VpddConfig=4;

                  "field version "
                     Mf=4;      "tabulated field, with correction "

* important: (1) new SVT version (2) FTPC gas correction tp Ar+C02 mix (3) SSD ladders raddi correction

                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 6; "SVTT version"
                     SvshConfig = 2; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     FtpcConfig = 1; "ftpc configuration"

                  "Photon Multiplicity Detector Version "
                     phmd=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     sisd=on;
                     SisdConfig = 35; "third version, full barrel newly corrected radii"


                  "FTPC Readout barrel "
                     ftro=on;
                     FtroConfig = 1;

                  "New version of the TPC backplane "
                     TpceConfig = 3;
                  "Muon Trigger System"
                     mutd = on;
                     MutdConfig = 1;
                  "We need a bigger Cave"
                     CaveConfig = 3;
                }
****************************************************************************************
  on DUMM01   { R and D geometry: TPC+DUMM

                     svtt=off; "no SVT  at all in this configuration"
                     ftpc=off; "no FTPC at all in this configuration"
                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";
                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
                     BtofConfig=5;
                  "calb" 
                     ems=on
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ecal"
                     ecal_config=1   " west wheel "
                     ecal_fill=3     " all sectors filled "
                  "beam-beam counter "
                     bbcm=on
                  "forward pion detector "
                     fpdm=on
                  "field version "
                     Mf=4;      "tabulated field, with correction "

                     SvshConfig = 0; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 0;

                  "Photon Multiplicity Detector Version "
                     phmd=off;
                     PhmdConfig = 0;
                  "Silicon Strip Detector Version "
                     sisd=off;
                     SisdConfig = 0;
* careful! Achtung!
                   pipeConfig=4;   " provisional"
                   dumm=on;        " put the dummy in"
                }
****************************************************************************************
  on UPGR01   { R and D geometry: TPC+SSD+HFT-SVT

                     svtt=off; "no SVT  at all in this configuration"
                     ftpc=off; "no FTPC at all in this configuration"
                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";
                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
                     BtofConfig=5;
                  "calb" 
                     ems=on
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ecal"
                     ecal_config=1   " west wheel "
                     ecal_fill=3     " all sectors filled "
                  "beam-beam counter "
                     bbcm=on
                  "forward pion detector "
                     fpdm=on
                  "field version "
                     Mf=4;      "tabulated field, with correction "

                     SvshConfig = 0; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 0;

                  "Photon Multiplicity Detector Version "
                     phmd=on;
                     PhmdConfig = 1;
                  "Silicon Strip Detector Version "
                     sisd=on;
                     SisdConfig = 45;
* careful! Achtung!
                   pipeConfig=4;   " provisional"
                   pixl=on;        " put the pixel detector in"
                   PixlConfig=4;   " newest version by Andrew Rose"
* HPD, IST off
* Forward STAR tracker disk
                   fstd=on;  "new pixel based forward tracker"
                   FstdConfig=2;
* Forward STAR tracker disk
                   fgtd=off;  "GEM forward tracker"
                   FgtdConfig=0;
* Forward GEM disks in this tag
                   igtd=on;
* prototype of the Inner Tracker SuPport structure
                   itsp=on;
                }
****************************************************************************************
  on UPGR02    { R and D geometry: TPC+IST+HFT-SVT
                  "svt: 3 layers ";
                     nsi=6  " 3 bi-plane layers, nsi<=7 ";
                     wfr=0  " numbering is in the code   ";
                     wdm=0  " width is in the code      ";

                     ConeConfig=2 " new cable weight estimate ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=4 " call btofgeo4 ";
* NEW CONFIG!
                     BtofConfig=8;

                  "calb" 
                     ems=on
                     CalbConfig = 1
* remember that with this config, the following parameters have
* a different meaning because we have to (unfortunately) switch
* from divisions to copies and introduce a map, which DOES
* control the configuration
                     nmod={60,60}; shift={75,105}; " 60 sectors West plus 30 East split between 2 halves"

                  "ecal"
                     ecal_config=1   " one ecal patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     bbcm=on

                  "forward pion detector "
                     fpdm=on
                     FpdmConfig  = 1 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     vpdd=on;
                     VpddConfig=4;

                  "field version "
                     Mf=4;      "tabulated field, with correction "

* important: (1) new SVT version (2) FTPC gas correction tp Ar+C02 mix (3) SSD ladders raddi correction

                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 5; "SVTT version"
                     DensConfig = 1; "gas density correction"
                     FtpcConfig = 1; "ftpc configuration"

                  "Photon Multiplicity Detector Version "
                     phmd=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     sisd=off;
                     SisdConfig = 0; "no ssd here"


                  "FTPC Readout barrel "
                     ftro=on;
                     FtroConfig = 1;

* Inner STAR tracker barrel
                   istb=on;  "new pixel based inner tracker"
                   IstbConfig=1;

* careful! Achtung!
                   pipeConfig=5;   " thinner pipe "
                   pixl=on;        " activate "
                   PixlConfig=3;   " source version "

                  "New version of the TPC backplane "
                     TpceConfig = 3;

                }
*************************************************************************************************************
  on UPGR03   { New Tracking: IST+IGT+HFT-SVT

                     svtt=off; "no SVT  at all in this configuration"
                     ftpc=off; "no FTPC at all in this configuration"
                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";
                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
                     BtofConfig=5;
                  "calb" 
                     ems=on
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ecal"
                     ecal_config=1   " west wheel "
                     ecal_fill=3     " all sectors filled "
                  "beam-beam counter "
                     bbcm=on
                  "forward pion detector "
                     fpdm=on
                  "field version "
                     Mf=4;      "tabulated field, with correction "
*                    -- Obsoleted: CorrNum = 4;
                     SvshConfig = 1; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 4;

                  "Photon Multiplicity Detector Version "
                     phmd=on;
                     PhmdConfig = 1;
                  "Silicon Strip Detector Version "
                     sisd=on;
                     SisdConfig = 23;
* careful! Achtung!
                   pipeConfig=4;   " provisional"
                   pixl=on;        " put the pixel detector in"
                   PixlConfig=2;   " newer version decoupled from SVT"
* Inner STAR tracker barrel
                   istb=on;  "new pixel based inner tracker"
                   IstbConfig=1;
* Inner STAR GEM barrel
                   gemb=off;  
                   GembConfig=0;
* Forward STAR tracker disk
                   fstd=on;  "new pixel based forward tracker"
                   FstdConfig=1;
* Forward STAR tracker disk
                   fgtd=off;  "GEM forward tracker"
                   FgtdConfig=0;
* the forward GEM disks
                   igtd=on;
                }
*************************************************************************************************************
  on UPGR04   { New Tracking: HPD

                     svtt=off; "no SVT  at all in this configuration"
                     ftpc=off; "no FTPC at all in this configuration"

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";
                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
                     BtofConfig=5;
                  "calb" 
                     ems=on
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ecal"
                     ecal_config=1   " west wheel "
                     ecal_fill=3     " all sectors filled "
                  "beam-beam counter "
                     bbcm=on
                  "forward pion detector "
                     fpdm=on
                  "field version "
                     Mf=4;      "tabulated field, with correction "
*                    -- Obsoleted: CorrNum = 4;
                     SvshConfig = 1; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 4;

                  "Photon Multiplicity Detector Version "
                     phmd=off;
                     PhmdConfig = 0;
                  "Silicon Strip Detector Version "
                     sisd=on;
                     SisdConfig = 35;
* careful! Achtung!
                   pipeConfig=4;   " provisional"
                   hpdt=on;        " put the detector in"
                   HpdtConfig=1;   " base version"
                }
****************************************************************************************
  on UPGR05   { New Tracking: HFT+HPD+IST+TPC-SVT

                     svtt=off; "no SVT  at all in this configuration"
                     ftpc=off; "no FTPC at all in this configuration"
                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";
                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
                     BtofConfig=5;
                  "calb" 
                     ems=on
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ecal"
                     ecal_config=1   " west wheel "
                     ecal_fill=3     " all sectors filled "
                  "beam-beam counter "
                     bbcm=on
                  "forward pion detector "
                     fpdm=on
                  "field version "
                     Mf=4;      "tabulated field, with correction "

                     SvshConfig = 0; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 0;

                  "Photon Multiplicity Detector Version "
                     phmd=on;
                     PhmdConfig = 1;
                  "Silicon Strip Detector Version "
                     sisd=on;
                     SisdConfig = 45;
* careful! Achtung!
                   pipeConfig=4;   " provisional"
                   pixl=on;        " put the pixel detector in"
                   PixlConfig=4;   " newest version by Andrew Rose"

                   hpdt=on;        " put the Hybrid Pixel detector in"
                   HpdtConfig=1;   " base version"
* Inner STAR tracker barrel
                   istb=on;  "new pixel based inner tracker"
                   IstbConfig=3;
* Inner STAR GEM barrel
                   gemb=off;  
                   GembConfig=0;
* Forward STAR tracker disk
                   fstd=on;  "new pixel based forward tracker"
                   FstdConfig=2;
* Forward STAR tracker disk
                   fgtd=off;  "GEM forward tracker"
                   FgtdConfig=0;
* Forward GEM disks in this tag
                   igtd=on;
* prototype of the Inner Tracker SuPport structure
                   itsp=on;
                }
****************************************************************************************
  on UPGR06   { New Tracking: HFT+HPD+SSD

                     svtt=off; "no SVT  at all in this configuration"
                     ftpc=off; "no FTPC at all in this configuration"
                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";
                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
                     BtofConfig=5;
                  "calb" 
                     ems=on
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ecal"
                     ecal_config=1   " west wheel "
                     ecal_fill=3     " all sectors filled "
                  "beam-beam counter "
                     bbcm=on
                  "forward pion detector "
                     fpdm=on
                  "field version "
                     Mf=4;      "tabulated field, with correction "

                     SvshConfig = 0; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 0;

                  "Photon Multiplicity Detector Version "
                     phmd=on;
                     PhmdConfig = 1;
                  "Silicon Strip Detector Version "
                     sisd=on;
                     SisdConfig = 45;
* careful! Achtung!
                   pipeConfig=4;   " provisional"
                   pixl=on;        " put the pixel detector in"
                   PixlConfig=4;   " newest version by Andrew Rose"

                   hpdt=on;        " put the Hybrid Pixel detector in"
                   HpdtConfig=1;   " base version"
* Forward STAR tracker disk
                   fstd=on;  "new pixel based forward tracker"
                   FstdConfig=2;
* Forward STAR tracker disk
                   fgtd=off;  "GEM forward tracker"
                   FgtdConfig=0;
* Forward GEM disks in this tag
                   igtd=on;
* prototype of the Inner Tracker SuPport structure
                   itsp=on;
                }
****************************************************************************************
  on UPGR07   { New Tracking: HFT+IST+TPC+SSD-SVT

                     svtt=off; "no SVT  at all in this configuration"
                     ftpc=off; "no FTPC at all in this configuration"
                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";
                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
                     BtofConfig=5;
                  "calb" 
                     ems=on
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ecal"
                     ecal_config=1   " west wheel "
                     ecal_fill=3     " all sectors filled "
                  "beam-beam counter "
                     bbcm=on
                  "forward pion detector "
                     fpdm=on
                  "field version "
                     Mf=4;      "tabulated field, with correction "

                     SvshConfig = 0; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 0;

                  "Photon Multiplicity Detector Version "
                     phmd=on;
                     PhmdConfig = 1;
                  "Silicon Strip Detector Version "
                     sisd=on;
                     SisdConfig = 45;
* careful! Achtung!
                   pipeConfig=4;   " provisional"
                   pixl=on;        " put the pixel detector in"
                   PixlConfig=4;   " newest version by Andrew Rose"

                   istb=on;  "IST barrel"
                   IstbConfig=3;

                   fstd=on;  "new pixel based forward tracker disk"
                   FstdConfig=2;

                   igtd=on; "Forward GEM disks in this tag"
                   itsp=on; "prototype of the Inner Tracker SuPport structure"
                }
****************************************************************************************
  on UPGR08   { New Tracking: HFT+HPD+IST+TPC-SVT-SSD

                     svtt=off; "no SVT  at all in this configuration"
                     ftpc=off; "no FTPC at all in this configuration"
                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";
                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
                     BtofConfig=5;
                  "calb" 
                     ems=on
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ecal"
                     ecal_config=1   " west wheel "
                     ecal_fill=3     " all sectors filled "
                  "beam-beam counter "
                     bbcm=on
                  "forward pion detector "
                     fpdm=on
                  "field version "
                     Mf=4;      "tabulated field, with correction "

                     SvshConfig = 0; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 0;

                  "Photon Multiplicity Detector Version "
                     phmd=on;
                     PhmdConfig = 1;
                  "Silicon Strip Detector Version "
                     sisd=off;
                     SisdConfig = 0;
* careful! Achtung!
                   pipeConfig=4;   " provisional"
                   pixl=on;        " put the pixel detector in"
                   PixlConfig=4;   " newest version by Andrew Rose"

                   hpdt=on;        " put the Hybrid Pixel detector in"
                   HpdtConfig=1;   " base version"
* Inner STAR tracker barrel
                   istb=on;  "new pixel based inner tracker"
                   IstbConfig=3;
* Inner STAR GEM barrel
                   gemb=off;  
                   GembConfig=0;
* Forward STAR tracker disk
                   fstd=on;  "new pixel based forward tracker"
                   FstdConfig=2;
* Forward STAR tracker disk
                   fgtd=off;  "GEM forward tracker"
                   FgtdConfig=0;
* Forward GEM disks in this tag
                   igtd=on;
* prototype of the Inner Tracker SuPport structure
                   itsp=on;
                }
****************************************************************************************
  on UPGR09   { New Tracking: HFT+HPD+IST*outer+TPC-SVT-SSD

                     svtt=off; "no SVT  at all in this configuration"
                     ftpc=off; "no FTPC at all in this configuration"
                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";
                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
                     BtofConfig=5;
                  "calb" 
                     ems=on
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ecal"
                     ecal_config=1   " west wheel "
                     ecal_fill=3     " all sectors filled "
                  "beam-beam counter "
                     bbcm=on
                  "forward pion detector "
                     fpdm=on
                  "field version "
                     Mf=4;      "tabulated field, with correction "

                     SvshConfig = 0; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 0;

                  "Photon Multiplicity Detector Version "
                     phmd=on;
                     PhmdConfig = 1;
                  "Silicon Strip Detector Version "
                     sisd=off;
                     SisdConfig = 0;
* careful! Achtung!
                   pipeConfig=4;   " provisional"
                   pixl=on;        " put the pixel detector in"
                   PixlConfig=4;   " newest version by Andrew Rose"

                   hpdt=on;        " put the Hybrid Pixel detector in"
                   HpdtConfig=1;   " base version"
* Inner STAR tracker barrel
                   istb=on;  "new pixel based inner tracker"
                   IstbConfig=4;
* Inner STAR GEM barrel
                   gemb=off;  
                   GembConfig=0;
* Forward STAR tracker disk
                   fstd=on;  "new pixel based forward tracker"
                   FstdConfig=2;
* Forward STAR tracker disk
                   fgtd=off;  "GEM forward tracker"
                   FgtdConfig=0;
* Forward GEM disks in this tag
                   igtd=on;
* prototype of the Inner Tracker SuPport structure
                   itsp=on;
                }
****************************************************************************************
  on DEV2005    { THIS TAG IS RESERVED FOR THE 2005 DEVELOPMENT ONLY
                  "svt: 3 layers ";
                     nsi=6  " 3 bi-plane layers, nsi<=7 ";
                     wfr=0  " numbering is in the code   ";
                     wdm=0  " width is in the code      ";

                     ConeConfig=2 " new cable weight estimate ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=4 " call btofgeo4 ";
* NEW CONFIG!
                     BtofConfig=8;

                  "calb" 
                     ems=on
                     CalbConfig = 1
* remember that with this config, the following parameters have
* a different meaning because we have to (unfortunately) switch
* from divisions to copies and introduce a map, which DOES
* control the configuration
                     nmod={60,60}; shift={75,105}; " 60 sectors West plus 30 East split between 2 halves"

                  "ecal"
                     ecal_config=1   " one ecal patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     bbcm=on

                  "forward pion detector "
                     fpdm=on
                     FpdmConfig  = 1 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     vpdd=on;
                     VpddConfig=4;

                  "field version "
                     Mf=4;      "tabulated field, with correction "

* important: (1) new SVT version (2) FTPC gas correction tp Ar+C02 mix (3) SSD ladders raddi correction

                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 7; "SVTT version"
                     DensConfig = 1; "gas density correction"
                     FtpcConfig = 1; "ftpc configuration"

                  "Photon Multiplicity Detector Version "
                     phmd=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     sisd=on;
                     SisdConfig = 24; "second version, full barrel with corrected radii"


                  "FTPC Readout barrel "
                     ftro=on;
                     FtroConfig = 1;

                  "New version of the TPC backplane "
                     TpceConfig = 2;
                }

****************************************************************************************
  on HADR_ON    { all Geant Physics On;                                       }
  on HADR_OFF   { all Geant Physics on, except for hadronic interactions; 
                                                                       IHADR=0}
  on GCALOR_ON { setting hadr 6 to activate hadronic showers;
                              IHADR=6;}

  on PHYS_OFF   { No Physics: only energy loss;
      {IDCAY,IANNI,IBREM,ICOMP,IHADR,IMUNU,IPAIR,IPHOT,IDRAY,IMULS}=0; Iloss=2}
  on DECAY_ONLY { Some Physics: decays, mult.scat and energy loss;
                  {IANNI,IBREM,ICOMP,IHADR,IMUNU,IPAIR,IPHOT,IDRAY}=0; Iloss=2}
  on NO_BREM    { No bremmstrahlung;
                  IBREM=0;}

  on LOW_EM     { Low cuts on ElectroMagnetic processes;
                  CUTGAM=0.00001;
                  CUTELE=0.00001;
                  BCUTE =0.00001;
                  BCUTM =0.00001;
                  DCUTE =0.00001;
                  DCUTM =0.00001;
                }

  on TPC_ONLY   { Minimal geometry - only TPC;
                  {pipe,svtt,ftpc,btof,vpdd,calb,ecal,magp,upst,zcal,phmd,fpdm,bbcm,sisd,ftro}=off; }
  on TPC_AND_SVTT  { Only TPC and SVT;
                  {pipe,ftpc,btof,vpdd,calb,ecal,magp,upst,zcal,phmd,fpdm,bbcm,ftro}=off; }
  on SVTT_ON    { Optional SVTT added on top of the minimal geo;
                     svtt=on; }
  on SVTT_OFF   { Optionally remove the SVTT;
                     svtt=off; }
  on SISD_OFF   { Optionally remove the SISD ssd;
                     sisd=off; }
  on ONLY_SVTT   { Only SVTT;
                  {pipe,tpce,ftpc,btof,vpdd,calb,ecal,magp,upst,zcal,phmd,fpdm,bbcm,ftro}=off; }
*
  on PIPE_ON    { Optional pipe added on top of the minimal geo;
                     pipe=on; }
  on PIPE_OFF   { Pipe optionally removed;
                     pipe=off; }
*
  on FTPC_ON    { Optional FTPC added on top of the minimal geo;
                     ftpc=on; }
  on BTOF_ON    { Optional BTOF added on top of the minimal geo;
                     btof=on; }
  on ECAL_ON    { Optional ECAL added on top of the minimal geo;
                     ecal=on; }
  on CALB_ON    { Optional CALB added on top of the minimal geo;
                     calb=on; }

  on SHIELD_OFF { Can switch the shield off in the DEV geom;
                     shld=off; }

  on PIXL_ON    { Optional PIXL added on top of the minimal geo;
                     pixl=on; }

  on FIELD_ONLY { No geometry - only magnetic field;              NtrSubEv=0;
      {cave,pipe,svtt,tpce,ftpc,btof,vpdd,magp,calb,ecal,rich,upst,zcal}=off; }
  on FIELD_OFF  { no magnetic field;                field=0;                  }
  on FIELD_ON   { Standard (5 KGs) field on;        field=5;                  }

  i=Index(Commands,'FIELD=')
  if i>0        { j=i/4+3; field=Par(1+j);  Commands(i:j*4)=' ';
                  <W> field; (' Modified field value =',F6.2,' KGS');         }
  on MWC_OFF    { Trigger Multy-wire readout off;   mwc=off;                  }
  on PSE_OFF    { No TPC pseudo-padrow generated;   pse=off;                  }
  on 4TH_OFF    { SVT fourth layer off;             Nsi=min(Nsi,6);           }
  on SPLIT_OFF  { events will not be split into subevents;     NtrSubEv=0;    }
  on SPLIT_ON   { events will be split into subevents;         NtrSubEv=1000; }
  on DEBUG_ON   { verbose mode, some graphics; Idebug=max(Idebug,1); Itest=1; }
  on DEBUG_OFF  { standard debug mode;         {Idebug,Itest}=0;              }
  }

* sanity check - if something left in commands (unknown keyword), we stop!
  l=LENOCC(commands); if l>0
  {  print *,' Unknown command left => ', commands(1:l), ' <= ',l
     if (IPRIN==0) stop 'You better stop here to avoid problems'     
  }
}


* -------------------- setup selected configuration ------------------------
* Now when all parameters and flags are ready, make gstar work as usually
* ie put a MODE or/and DETP command and executing them for selected systems.
*
* - to save secondaries AFTER all decays:      DETP TRAC DCAY 210 210 0.1 0.01
   dcay={210,210,0.1,0.01}
   If LL>1 { call AgDETP new ('Trac'); call AgDETP add ('TracDCAY',dcay,4) }


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
   write(*,*) '                 PipeConfig: ',PipeConfig
   write(*,*) '                 PixlConfig: ',PixlConfig
   write(*,*) '                 SvshConfig: ',SvshConfig
   write(*,*) '                 SisdConfig: ',SisdConfig
   write(*,*) '                 SupoConfig: ',SupoConfig
   write(*,*) '                 SvttConfig: ',SvttConfig
   write(*,*) '                 TpceConfig: ',TpceConfig
   write(*,*) '                 VpddConfig: ',VpddConfig
   write(*,*) '***** FOR EXPERTS ONLY: LOOK UP GEOMETRY.G FOR DETAIL *******'


   if (rich) ItCKOV = 1

   if (cave) then
      call AgDETP new ('CAVE')
      call AgDETP add ('CVCF.config=',CaveConfig,1)
      call cavegeo
   endif

* Pipe:
   If (LL>1)        call AgDETP new ('PIPE')
   call AgDETP add ('pipv.pipeConfig=',pipeConfig,2);
   if (pipe)        Call pipegeo

* Upstream (DX), shield, and D0+Q1+Q2+Q3
   if (upst)        Call upstgeo
   if (shld)        Call shldgeo
   if (quad)        Call quadgeo

* --- 
   Call AGSFLAG('SIMU',2)

* - to switch off the fourth svt layer:        DETP SVTT SVTG.nlayer=6 

   If (LL>1 & svtt) then
     call AgDETP new ('SVTT')
     if (Nsi < 7)           call AgDETP add ('svtg.nlayer=',   Nsi,1)
     if (NsiMin > 1)        call AgDETP add ('svtg.nmin=',   NsiMin,1)
     if (pipeConfig >= 4)   call AgDETP add ('svtg.ifMany=',     1,1)
     if (Wfr > 0)           call AgDETP add ('svtl(3).nwafer=',wfr,1)
     if (wdm > 0)           call AgDETP add ('swca.WaferWid=', wdm,1)
     if (wdm > 0)           call AgDETP add ('swca.WaferLen=', wdm,1)
     if (.not.svtw)         call AgDETP add ('swam.Len=',       0, 1)
   endif

******************************************************************
* Take care of the correction level and call the appropriate constructor:
  if(svtt) then

*   This applies to the newer versions of the svt code:
*   we can now switch to a better description of the cone
*   material (copper cables) thanks to a new measurement by
*   Dave Lynn

    call AgDETP add ('svtg.ConeVer=',ConeConfig ,1) ! could have more copper on the cone

* Optionally, switch to a larger inner shield, AND smaller beampipe support 
    if(SvshConfig==1) call AgDETP add ('svtg.SupportVer=',2 ,1)
* Or, pick a shield that is slighly bigger outside according to Lilian's observation
    if(SvshConfig==2) call AgDETP add ('svtg.SupportVer=',3 ,1)

* Ugly, but I don't want to hash function pointers in Fortran:

    if(SvttConfig==0) call svttgeo
    if(SvttConfig==1) call svttgeo1
    if(SvttConfig==2) call svttgeo2
    if(SvttConfig==3) call svttgeo3
    if(SvttConfig==4) call svttgeo4
    if(SvttConfig==5) call svttgeo5
    if(SvttConfig==6) call svttgeo6
    if(SvttConfig==7) call svttgeo7

  endif

* Set the proper configuration of the Silicon Strip Detector
* See note on top about using MOD(10) to encode the geometry
* cut, as opposed to configuration of the detector:

  if(sisd) then
       sisd_level=0
       call AgDETP new ('SISD')

* if SVT is present, position the SSD in it, otherwise need to position in CAVE (default)
       if(svtt) { call AgDETP add ('ssdp.Placement=',1 ,1) };

* In the following, level means the version of the ssd geo code to be loaded
* It is the most important decimal place of the SisdConfig, and we just check
* for it here:

       if (SisdConfig>10) then
         sisd_level=SisdConfig/10
         SisdConfig=SisdConfig-sisd_level*10

         call AgDETP add ('ssdp.Config=',SisdConfig ,1)
         if     (sisd_level.eq.1) then
            call sisdgeo1
         elseif (sisd_level.eq.2) then
            call sisdgeo2
         elseif (sisd_level.eq.3) then
            call sisdgeo3
         elseif (sisd_level.eq.4) then
            call sisdgeo4
         else ! Unimplemented level
            write(*,*) '******************* ERROR IN PARSING THE SSD GEOMETRY LEVEL! ******************'
            if (IPRIN==0) stop 'You better stop here to avoid problems'     
         endif

       else
*        The original version (pretty much obsolete)
         call AgDETP add ('ssdp.Config=',SisdConfig ,1)
         call sisdgeo
       endif

*       write(*,*) '*** Silicon Strip Detector Config and Code Level: ',SisdConfig, ' ',sisd_level

  endif


* - MWC or pseudo padrows needed ? DETP TPCE TPCG(1).MWCread=0 TPRS(1).super=1
*   CRAY does not accept construction: IF (mwc==off) ... I do it differntly:
* - for year_1 X in mwc hits was limited, keep this (mwx=1)

   If (LL>1 & tpce) then
     call AgDETP new ('TPCE')
* Attention -- this line below was effectively moved into individual year 1 declarations:
*     If (Geom(1:2)='_1') mwx=1
* Since we don't need the GEOM variable anymore in this context, we use it differently:
* to simply contains the whole geometry tag such as year_1S or y2003a
     If (  .not. mwc   ) mwx=0
     If ( mwx <2 )  call AgDETP add ('tpcg(1).MWCread=',mwx,1)
     If (.not.pse)  call AgDETP add ('tprs(1).super='  , 1, 1) 
   endif 

* Back in July 2003 Yuri has discovered the discrepancy
* in the gas density. The patch for this is activated here: (was: if(CorrNum>=3) )

   if(DensConfig>0) call AgDETP add ('tpcg.gasCorr=',2 ,1)

   write(*,*) 'TPC'
   if (tpce.and.TpceConfig==1) Call tpcegeo
   if (tpce.and.TpceConfig==2) Call tpcegeo1
   if (tpce.and.TpceConfig==3) Call tpcegeo2

   write(*,*) 'FTPC'
   if (ftpc) then
	if(FtpcConfig==0) Call ftpcgeo
	if(FtpcConfig==1) Call ftpcgeo1
*       and look at the support pieces, was: if(CorrNum==0)
	if(SupoConfig==0)  Call supogeo
	if(SupoConfig==1)  Call supogeo1
   endif

* FTPC readout electronics barrel
   if (ftro) Call ftrogeo

   write(*,*) 'BTOF'
* - tof system should be on (for year 2):      DETP BTOF BTOG.choice=2
   If (LL>1 & btof) then
     call AgDETP new ('BTOF')
     call AgDETP add ('btog.choice=',BtofConfig,1)
   endif

   if(btof) then
             if(Itof.eq.1) then
                write(*,*) '***********  ATTENTION : OLD VERSION OF BTOF IS NO LONGER IMPLEMENTED **********'
                write(*,*) '***********  NO BTOF WILL BE INSTANTIATED **************************************'
             endif

             if(Itof.eq.2) then
                call btofgeo2
             endif

             if(Itof.eq.4) then
                call btofgeo4
             endif
   endif
     
   Call AGSFLAG('SIMU',1)


********************* Vertex Position Detector *******************
   If (LL>1 & vpdd) then
     call AgDETP new ('VPDD')
     call AgDETP add ('vpdv.vpdConfig=',VpddConfig,1);
     call vpddgeo
   endif

********************** BARREL CALORIMETER ************************
*  - Set up the parameters for the barrel calorimeter
   If (LL>1 & calb) then
     call AgDETP new ('CALB')
     if (ems)  call AgDETP add ('calg.nmodule=',Nmod, 2)
     if (ems)  call AgDETP add ('calg.shift=',  shift,2)
   endif


   if (calb) then ! Pick the version:

       if(CalbConfig==0) then
           write(*,*) '************** Creating the 1996-2003 version of the Barrel Calorimeter'
           Call calbgeo
       endif

       if(CalbConfig==1) then
           write(*,*) '************** Creating the 2004-2006 version of the Barrel Calorimeter'
           Call calbgeo1
       endif

       if(CalbConfig==2) then
           write(*,*) '************** Creating the 2007-     version of the Barrel Calorimeter'
           Call calbgeo2
       endif

  endif
******************************************************************
*  - Set up the parameters for the RICH counter
   if (LL>1 & rich) then
      call AgDETP new ('Rich')
      if (Rv>0) call AgDETP add ('Rich.Version=', Rv,1) 
      if (Rp>0) call AgDETP add ('Rich.Position=',Rp,1)
      if (Rp>0) call AgDETP add ('Rich.Cversion=',Rp,1)
   endif
   if (rich) Call richgeo

******************************************************************
*  - Set up the parameters for the endcap calorimeter
   If (LL>1 & ecal) then
      call AgDETP new ('ECAL')
      call AgDETP add ('emcg.OnOff='   ,ecal_config,1)
      call AgDETP add ('emcg.FillMode=',ecal_fill,1)
   endif

******************************************************************
* The rest of steering:

   if (ecal) Call ecalgeo
   if (bbcm) Call bbcmgeo

   if (fpdm.and.FpdmConfig==0) Call fpdmgeo
   if (fpdm.and.FpdmConfig==1) Call fpdmgeo1
   if (fpdm.and.FpdmConfig==2) Call fpdmgeo2
   if (fpdm.and.FpdmConfig==3) Call fpdmgeo3

   if (zcal) Call zcalgeo
   if (magp) Call magpgeo

   if (mutd.and.MutdConfig==1) Call mutdgeo

   if (pixl.and.PixlConfig==1) Call pixlgeo
   if (pixl.and.PixlConfig==2) Call pixlgeo1
   if (pixl.and.PixlConfig==3) Call pixlgeo2
   if (pixl.and.PixlConfig==4) Call pixlgeo3

   if (istb.and.IstbConfig==1)  Call istbgeo
   if (istb.and.IstbConfig==2)  Call istbgeo1
   if (istb.and.IstbConfig==3)  Call istbgeo2
   if (istb.and.IstbConfig==4)  Call istbgeo3


   if (gemb.and.GembConfig>0)  Call gembgeo

   if (fstd.and.FstdConfig>0)  then
      if(FstdConfig==2) then
         call AgDETP new ('FSTD')
         call AgDETP add ('fstg.Rmax=',22.3,1)
      endif
        Call fstdgeo
   endif

   if (fgtd.and.FgtdConfig>0)  Call fgtdgeo

   if (igtd) then
       if(IgtdConfig==2) then
           call AgDETP new ('IGTD')
           call AgDETP add ('igtv.Config=',IgtdConfig ,1)
       endif
       Call igtdgeo
   endif

   if (hpdt.and.HpdtConfig>0)  Call hpdtgeo

   if (itsp)                   Call itspgeo
******************************************************************
* If PHMD is present and a non-zero version of the Photon Multiplicity Detector
* is defined, pass the version number to its constructor
* and create it:

   if  (phmd.and.PhmdConfig>0) then
      call AgDETP new ('PHMD')
      call AgDETP add ('PMVR.Config=', PhmdConfig,1)
      call phmdgeo
   endif


********************************************************************
   if(dumm) then
      call dummgeo
   endif
****************  Magnetic Field  ********************************
*
* - reset magnetic field value (default is 5): DETP MFLD MFLG.Bfield=5
   If (LL>1) then
      call AgDETP new ('MFLD')
      if (mfld & field!=5) call AgDETP add ('MFLG(1).Bfield=',field,1)
      if (mfld & mf!=0)    call AgDETP add ('MFLG(1).version=',mf,1)
*     if (mfld & mf>=4)    call AgDETP add ('MFLG(1).nrp=',200,1)
*     if (mfld & mf>=4)    call AgDETP add ('MFLG(1).nzp=',800,1)

   endif
   if (mfld) Call mfldgeo
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
*
* -------------------- persist certain global parameters -------------------

   Fill GDAT                     ! GEANT run data
      mfscale=field/5.0          ! magnetic field scale (nominal)
      gtag={geom(1:4),geom(5:8)} ! geometry tag 
   EndFill
*
   end

