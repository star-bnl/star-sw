// $Id: St_geant_Maker.cxx,v 1.153 2015/06/23 20:27:53 jwebb Exp $
// $Log: St_geant_Maker.cxx,v $
// Revision 1.153  2015/06/23 20:27:53  jwebb
// Added hits for FTBF version of HCAL
//
// Revision 1.152  2015/01/13 19:02:47  jwebb
// Read out sensitive layer from FgtdGeoV.xml
//
// Revision 1.151  2015/01/08 21:24:18  jwebb
// Support FMS preshower detector in HCAL.
//
// Revision 1.150  2015/01/06 15:59:20  jwebb
// Add FMS preshower to data readout
//
// Revision 1.149  2014/07/29 17:51:29  jwebb
// Read in HCAL hits.
//
// Revision 1.148  2014/03/24 19:57:15  jwebb
// Added code to support reading new MTD active layers.
//
// Revision 1.147  2013/06/26 18:55:05  jwebb
// Updated MTD sensitive volume.
//
// Revision 1.146  2013/02/06 22:04:45  fisyak
// Add attribute hadr_off
//
// Revision 1.145  2013/01/17 15:10:55  fisyak
// Add handle for setting runG, be more careful with setting mag.field
//
// Revision 1.144  2012/11/26 18:45:55  jwebb
// Restoring to previous version of St_geant_Maker and adding in changes needed
// for new generator framework (i.e. exposing TGiant3 instance).
//
// Revision 1.142  2012/11/14 00:02:12  fisyak
// Add flux histograms, use Attributes intead of m_Mode
//
// Revision 1.141  2012/01/24 03:13:29  perev
// Etr added
//
// Revision 1.140  2011/09/11 20:57:12  fisyak
// Add kinematics definition via MuDst, Clean up
//
// Revision 1.139  2011/08/03 20:12:24  jwebb
// Add mtd to the geant maker.
//
// Revision 1.138  2011/07/20 17:37:12  perev
// Fsc added
//
// Revision 1.137  2011/04/19 15:52:05  fisyak
// Restore handle for pile-up events
//
// Revision 1.136  2011/01/26 19:42:37  perev
// FPD ==> STAR Soft
//
// Revision 1.135  2010/08/10 16:35:33  fisyak
// Add initialization of starsim parameter tables after opening zebra-file
//
// Revision 1.134  2010/05/27 13:36:14  fisyak
// 3rd attemp to synchronize mag.field. Now take care that the maker can be not Active and do InitRun in Work
//
// Revision 1.132  2010/05/24 15:38:40  fisyak
// Move geometry and mag.field initialization from Init into InitRun in order to allow mag. field settings from StMagFMaker::InitRun
//
// Revision 1.130  2010/05/10 14:19:52  fisyak
// move geometry load from Init to InitRun in order to allow MagF maker to set mag.field
//
// Revision 1.129  2010/04/23 23:19:26  perev
// Remove not needed and expencive call AgstHits
//
// Revision 1.128  2010/03/31 19:15:45  fine
// RT #1890 Fix the side effect introduced  yesterdays
//
// Revision 1.127  2010/03/29 20:37:54  fine
// RT #1890 Fix the geometry leak that entails the warning message
//
// Revision 1.126  2010/02/22 15:02:18  fisyak
// Clean up, fix bug #1860 by replacing skip => trig
//
// Revision 1.125  2010/01/07 19:16:24  perev
// mArgs for agvolume is 9 now
//
// Revision 1.124  2009/12/31 00:02:59  perev
// Add the material name to the volume name
//
// Revision 1.123  2008/10/28 22:28:34  perev
// FGZD hits added
//
// Revision 1.122  2008/10/21 18:02:50  perev
// FGSC==>FGZC its division, Wei-Ming
//
// Revision 1.121  2008/07/30 15:04:35  fisyak
// Remove custom SetDebug, fix bug #1252
//
// Revision 1.120  2008/06/12 20:35:27  fisyak
// Move creation of TGiant from ctor to Init
//
// Revision 1.119  2007/07/12 20:34:02  fisyak
// use StarLogger
//
// Revision 1.118  2007/05/10 19:15:29  potekhin
// Improved handling of the Pythia event header translation,
// replaced a print statement with a logger call, plus
// a few cosmetic changes
//
// Revision 1.117  2007/04/28 17:56:12  perev
// Redundant StChain.h removed
//
// Revision 1.116  2007/04/26 15:51:31  fisyak
// Move creation of TGiant3 in ctor (fix byg 942)
//
// Revision 1.115  2007/04/26 04:18:27  perev
// Remove StBFChain dependency
//
// Revision 1.114  2007/03/07 16:46:04  fine
// Add the warning
//
// Revision 1.113  2007/03/03 02:30:50  fine
// fix the geometry file name. Thanks P.Nebski
//
// Revision 1.112  2007/03/03 00:35:42  fine
// Fix the leak of the ROOT objects and introduce the method to return the source code filename  for the arbitrary geometry node
//
// Revision 1.111  2006/10/31 23:54:13  potekhin
// Corrected a typo in the GEMB hit readout -- credit goes to Ross Corliss
//
// Revision 1.110  2006/10/17 19:24:30  fisyak
// Mode initialization after open input fz-file
//
// Revision 1.109  2006/09/22 21:27:51  potekhin
// Added readout of hits for two R&D detectors,
// GEM and HPD
//
// Revision 1.108  2006/06/19 23:26:03  potekhin
// Need the correct handle on the IDT hits,
// the previous tags were obsolete
//
// Revision 1.107  2005/11/22 23:13:24  fisyak
// Add default kinematics if there is no input fiels and if maker is active
//
// Revision 1.106  2005/10/12 22:58:56  fisyak
// SetDateTime from fz-file if it was not set before
//
// Revision 1.105  2005/10/06 19:23:07  fisyak
// Add set date/time from fz-file
//
// Revision 1.104  2005/08/29 21:47:09  fisyak
// Changes for VMC
//
// Revision 1.103  2005/06/30 22:47:40  potekhin
// Adding the previosuly missing g2t_igt_hit header.
//
// Revision 1.102  2005/06/30 22:32:56  potekhin
// Included the newly developed Inner GEM tracker
// (fresh R&D by Gerrit and Nikolai), and also
// improved debug print statemetns in a few
// newer detectors
//
// Revision 1.101  2005/04/26 23:40:18  potekhin
// As Lilian pointed out, we need to treat the SSD hits separately
// from the SVT in the newer versions of the geometry (we decoupled
// those a while ago).
//
// To this end, we use two separate variables for the number of
// detected hits, and place them into corresponding tables.
//
// Revision 1.100  2005/04/18 23:27:18  potekhin
// Correct the name for the FGT (GEM) hits
//
// Revision 1.99  2005/04/15 14:52:37  potekhin
// Adding an interface for reading the FGT (GEM) hits
//
// Revision 1.98  2005/04/13 22:27:11  fisyak
// Add Hit description extractor (AgstHits)
//
// Revision 1.97  2005/03/23 21:56:30  potekhin
// Added the latest Kai's code for reading hits
// from the IST and FST tables
//
// Revision 1.96  2005/02/07 21:09:20  fisyak
// rename antique TGeant3 to TGiant3
//
// Revision 1.95  2004/11/24 04:09:42  jeromel
// Loss of one GtHash object per call fixed
//
// Revision 1.94  2004/08/05 16:40:12  potekhin
// Propagating Pythia event characterization data
//
// Revision 1.93  2004/07/30 00:29:54  potekhin
// Fixed an old indexing typo
//
// Revision 1.92  2004/03/16 18:37:49  potekhin
// Corrected a typo that caused an out-of-bounds
// array error.
//
// Revision 1.91  2004/03/01 17:29:54  fisyak
// switch to starsim
//
// Revision 1.89  2004/02/25 17:55:05  fine
// An extra protection against of crash with gGeometry == 0
//
// Revision 1.88  2004/02/24 17:16:44  fisyak
// remove creation of empty fVolume
//
// Revision 1.87  2004/02/10 23:16:34  potekhin
// First version of Ag2Geom
//
// Revision 1.86  2003/11/12 22:44:26  potekhin
// Kill a stray debug print statement
//
// Revision 1.85  2003/10/31 23:12:13  potekhin
// Added a piece of code to handle the pixel detector hits.
// Reformatted a few lines and changed some comments.
//
// Revision 1.84  2003/10/02 00:13:03  potekhin
// Added the handling of the gdat structure, for now being
// written into runco are. may want to later augment this
// so that it gets into geant.root file.
//
// Revision 1.83  2003/09/02 17:59:29  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.82  2003/05/14 22:54:23  potekhin
// Fixing the incompatibilities gradually accumulated, in the
// part that reads and propagates the event header info.
// In particular, the pseudoparticle codes were incorrect.
// FIll the header based on the parsed event record.
//
// Revision 1.81  2003/05/01 20:48:56  jeromel
// This one is ugly ... But needed for root transition again.
//
// Revision 1.80  2003/04/30 20:39:19  perev
// Warnings cleanup. Modified lines marked VP
//
// Revision 1.79  2003/04/18 15:53:26  geurts
// Code added for TOFr (tfr) tables.
//
// Revision 1.78  2002/11/28 02:35:53  jeromel
// Minor correction
//
// Revision 1.77  2002/11/27 21:54:32  potekhin
// Added new naming convention in ESM
//
// Revision 1.76  2002/11/01 03:17:41  fine
// the previous version has been restored. No need of the special flag
//
// Revision 1.74  2002/10/16 20:39:23  kopytin
// Added code to read out BBC GSTAR tables. Needed by StBbcSimulationMaker
//
// Revision 1.72  2002/06/17 16:12:43  perev
// fix wrong geant time
//
// Revision 1.71  2002/04/14 21:57:09  perev
// Obsolete StBroadcast
//
// Revision 1.70  2002/03/12 21:22:38  fisyak
// Set only one StEvtHddr as default option (due to Embedding)
//
// Revision 1.69  2001/11/18 00:58:14  perev
//
// Revision 1.68  2001/07/06 17:34:24  nevski
// type fixed
//
// Revision 1.67  2001/07/03 23:37:25  nevski
// forward pion detector added
//
// Revision 1.66  2001/07/03 15:51:48  nevski
// phmd added
//
// Revision 1.65  2001/06/01 03:03:57  perev
// overloaded GetDataSet -> FindDataSet
//
// Revision 1.64  2000/12/19 18:35:11  fisyak
// make proper allocation for cgnam
//
// Revision 1.63  2000/07/19 16:57:34  fisyak
// Protection against double resetting the same run no. (Sasha)
//
// Revision 1.62  2000/06/23 16:52:12  fisyak
// Add filling of Run/Event no./Type from geant
//
// Revision 1.61  2000/03/26 02:43:22  fine
//  adjusted to ROOT 2.24
//
// Revision 1.60  2000/03/03 22:00:53  nevski
// protection against bad track number
//
// Revision 1.59  2000/03/03 20:53:48  nevski
// add protection against corrupted Itrac
//
// Revision 1.58  2000/02/29 22:25:52  lasiuk
// added FREO and QUAR to Rich hits
//
// Revision 1.57  2000/02/03 19:34:40  fisyak
// Clean up St_geant_Maker::Init, move its parameters to ctor
//
// Revision 1.56  2000/02/03 16:14:39  fisyak
// Add Kathy's histograms
//
// Revision 1.55  2000/02/02 21:21:19  fisyak
// Hack for CC5
//
// Revision 1.54  2000/01/23 19:20:53  nevski
// pseudo-doc
//
// Revision 1.53  2000/01/14 23:43:54  fisyak
// Add missing defines
//
// Revision 1.52  2000/01/04 21:51:11  fisyak
// Move TGiant3 to root4star
//
// Revision 1.51  1999/12/07 15:44:25  fisyak
// Add geane, new TGiant3 from Alice
//
// Revision 1.50  1999/11/13 17:30:05  fine
// scope for i within for loop fixed
//
// Revision 1.49  1999/11/13 02:40:55  fisyak
// Add gclose
//
// Revision 1.48  1999/11/11 05:16:30  fine
// GetDataSet method has been introduced to build GEANT geometry on fly
//
// Revision 1.47  1999/11/06 23:05:01  fisyak
// fix chars
//
// Revision 1.46  1999/10/20 19:18:17  nevski
// g2t_event table filled
//
// Revision 1.45  1999/09/24 01:23:42  fisyak
// Reduced Include Path
//
// Revision 1.44  1999/07/14 16:47:44  fisyak
// Set protection against empty event
//
// Revision 1.43  1999/07/09 02:18:03  fisyak
// Add Skip
//
// Revision 1.42  1999/07/09 01:15:48  fisyak
// Remove non printing character from generator type
//
// Revision 1.41  1999/07/03 22:40:11  fine
// St_geant_Maker::Work - workaround of LINUX compiler problem
//
// Revision 1.40  1999/04/30 15:17:03  perev
// SetOutput added to announce Geometry exists
//
// Revision 1.39  1999/04/29 19:29:27  nevski
// SetInputFile returns status
//
// Revision 1.38  1999/04/20 21:40:17  nevski
// all shapes are going via Victors hash
//
// Revision 1.37  1999/04/19 06:29:30  nevski
// update of user parameter extraction
//
// Revision 1.36  1999/04/19 06:25:35  nevski
// update of user parameter extraction
//
// Revision 1.35  1999/04/15 20:36:40  fine
// St_geant::Work() was void becomes TVolume *
//
// Revision 1.34  1999/04/12 23:17:11  fine
// Unique postion ID has been introduced
//
// Revision 1.33  1999/04/09 23:52:48  nevski
// checking 3 volume parameters now
//
// Revision 1.32  1999/04/08 00:39:08  fine
// Work metod - workaround for ROOT bug PCON definition
//
// Revision 1.31  1999/04/07 12:59:45  fine
// Fixed bug for PCON and PGON shapes
//
// Revision 1.30  1999/04/06 19:40:08  nevski
// variable size volumes
//
// Revision 1.29  1999/03/22 14:45:23  nevski
// geometry tree corrected
//
// Revision 1.28  1999/03/20 22:43:05  perev
// Do(trig)
//
// Revision 1.27  1999/03/11 00:15:22  perev
// St_geant_Maker in new maker schema
//
// Revision 1.26  1999/03/04 19:32:16  nevski
// esm/eem corrected
//
// Revision 1.25  1999/02/24 17:12:27  fine
// St_Table::New has been activated
//
// Revision 1.24  1999/02/23 18:59:50  nevski
// SVT 4th layer added to svt hit table
//
// Revision 1.23  1999/02/22 23:55:57  fine
// St_geant_Maker::rootmaptable is prepaed to use St_TableNew(), not activated yet
//
// Revision 1.22  1999/02/22 20:51:25  fisyak
// Mismatch between ctb/tof
//
// Revision 1.21  1999/02/22 19:27:20  fisyak
// add gtrigi and gtigc
//
// Revision 1.20  1999/02/20 20:23:45  fisyak
// Fix Aeast
//
// Revision 1.18  1999/02/19 14:41:00  fisyak
// Set kIsNotOwn Bit for geometry tables
//
// Revision 1.17  1999/02/18 15:44:47  fisyak
// Cleanup warinings
//
// Revision 1.16  1999/02/17 22:42:07  fisyak
// fix Linux parameters
//
// Revision 1.15  1999/02/17 15:55:38  fisyak
// Add GSTAR to ROOT geometry tables transformation
//
// Revision 1.14  1999/02/16 18:15:45  fisyak
// Check in the latest updates to fix them
//
// Revision 1.13  1999/02/12 17:57:01  nevski
// particle table
//
// Revision 1.12  1999/02/12 14:18:27  nevski
// merging 2 mods
//
// Revision 1.5  1999/01/10 20:37:31  fisyak
// Give access to Zebra
//
// Revision 1.4  1999/01/05 01:37:02  fisyak
// Intermeidate version with TVolume
//
// Revision 1.3  1999/01/03 20:56:35  fisyak
// Remove St_geom_Maker
//
// Revision 1.7  1998/12/25 21:02:13  nevski
// Add Set/Get method
//
// Revision 1.6  1998/12/17 14:38:00  fisyak
// Change default to no Higz window
//
// Revision 1.5  1998/12/16 20:56:24  fisyak
// Add gstar to ROOT
//
// Revision 1.4  1998/12/12 00:21:15  fisyak
// Remove gstar for the moment
//
// Revision 1.3  1998/12/12 00:18:00  fisyak
// Remove gstar for the moment
//
// Revision 1.2  1998/12/04 19:36:47  fisyak
// Add Pavel/Ruben gstar interface
//
// Revision 1.1  1998/10/31 00:28:31  fisyak
// Makers take care about branches
//
// Revision 1.6  1998/10/06 18:00:29  perev
// cleanup
//
// Revision 1.5  1998/10/02 13:46:08  fine
// DataSet->DataSetIter
//
// Revision 1.4  1998/08/14 15:25:58  fisyak
// add options
//
// Revision 1.3  1998/08/10 02:32:07  fisyak
// Clean up
//
// Revision 1.2  1998/07/20 15:08:15  fisyak
// Add tcl and tpt
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
//               St_geant_Maker class for Makers                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include "St_geant_Maker.h"
#include "TDataSetIter.h"
#include "TTable.h"
#include "Stiostream.h"
#include <stdio.h>
#include <string.h>
#include <string>
#include "TSystem.h"
#include "GtHash.h"
#include "TGeometry.h"
#include "TMaterial.h"
#include "TMixture.h"
#include "TString.h"
#include "TRegexp.h"
#include "TInterpreter.h"
#include "TClassTable.h"    
#include "TVolume.h"
#include "TFile.h"
#include "TFileSet.h"
#include "TMath.h"
#include "TBRIK.h"
#include "TTRD1.h"
#include "TTRD2.h"
#include "TTRAP.h"
#include "TTUBE.h"
#include "TTUBS.h"
#include "TCONE.h"
#include "TCONS.h"
#include "TSPHE.h"
#include "TPARA.h"
#include "TPGON.h"
#include "TPCON.h"
#include "TELTU.h"
//     #include "THYPE.h"
#include "TGTRA.h"
#include "TCTUB.h"
//  new Geometry
#include "TGeoMaterial.h"
#include "TGeoMatrix.h"
#include "TGeoNode.h"
#include "TGeoManager.h"
#include "TGeoVolume.h"
#include "TGeoPcon.h"
#include "TGeoPgon.h"
#include "TObjString.h"
#include "TGraph.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "StarMagField.h"
//#include "tables/St_g2t_run_Table.h"
#include "tables/St_g2t_event_Table.h"
#include "tables/St_g2t_pythia_Table.h"
#include "tables/St_g2t_gepart_Table.h"
#include "tables/St_g2t_vertex_Table.h"
#include "tables/St_g2t_track_Table.h"
#include "tables/St_geom_gdat_Table.h"
#include "StDetectorDbMaker/St_MagFactorC.h"
#include "tables/St_det_user_Table.h"
#include "tables/St_det_hit_Table.h"
#include "tables/St_det_path_Table.h"
#include "tables/St_mfld_mflg_Table.h"
#include "TDataSetIter.h"
#include "TTreeIter.h"
// event header:
#include "g2t/St_g2t_get_event_Module.h"
// Pythia-specific header
#include "g2t/St_g2t_get_pythia_Module.h"
//
#include "g2t/St_g2t_get_kine_Module.h"
#include "g2t/St_g2t_particle_Module.h"
// Subsystems:
#include "g2t/St_g2t_svt_Module.h"
#include "g2t/St_g2t_ssd_Module.h"
#include "g2t/St_g2t_pix_Module.h"
#include "g2t/St_g2t_hpd_Module.h"
#include "g2t/St_g2t_ist_Module.h"
#include "g2t/St_g2t_igt_Module.h"
#include "g2t/St_g2t_gem_Module.h"
#include "g2t/St_g2t_fst_Module.h"
#include "g2t/St_g2t_fgt_Module.h"
#include "g2t/St_g2t_tpc_Module.h"
#include "g2t/St_g2t_mwc_Module.h"
#include "g2t/St_g2t_ftp_Module.h"
#include "g2t/St_g2t_ctb_Module.h"
#include "g2t/St_g2t_tof_Module.h"
#include "g2t/St_g2t_tfr_Module.h" 
#include "g2t/St_g2t_rch_Module.h"
#include "g2t/St_g2t_emc_Module.h"
#include "g2t/St_g2t_smd_Module.h"
#include "g2t/St_g2t_eem_Module.h"
#include "g2t/St_g2t_esm_Module.h"
#include "g2t/St_g2t_zdc_Module.h"
#include "g2t/St_g2t_vpd_Module.h"
#include "g2t/St_g2t_pmd_Module.h"
#include "g2t/St_g2t_bbc_Module.h"
#include "g2t/St_g2t_fpd_Module.h"
#include "g2t/St_g2t_fsc_Module.h"
#include "g2t/St_g2t_mtd_Module.h"
#include "g2t/St_g2t_etr_Module.h"
#include "g2t/St_g2t_hca_Module.h"

#include "St_db_Maker/St_db_Maker.h"
#include "TUnixTime.h"
#include "StarCallf77.h" 
#include "StMagF.h"
#include "StMessMgr.h"
#ifdef DetectorIndex
#include "StarDetectorMap.h"
#endif
#include "StDetectorDbMaker/St_vertexSeedC.h"
#ifdef F77_NAME
#define    geometry	 F77_NAME(geometry,GEOMETRY)
#define    agstroot	 F77_NAME(agstroot,AGSTROOT)
#define    csaddr	 F77_NAME(csaddr,CSADDR)
#define    csjcal	 F77_NAME(csjcal,CSJCAL)
#define    g2t_volume_id F77_NAME(g2t_volume_id,G2T_VOLUME_ID)
#define    g2r_get_sys F77_NAME(g2r_get_sys,G2R_GET_SYS)
#define    gfrotm	 F77_NAME(gfrotm,GFROTM)
#define    gfxzrm	 F77_NAME(gfxzrm,GFXZRM)
#define    dzddiv	 F77_NAME(dzddiv,DZDDIV)
#define    agnzgete	 F77_NAME(agnzgete,AGNZGETE)
#define    rootmaptable  F77_NAME(rootmaptable,ROOTMAPTABLE)
#define    agvolume      F77_NAME(agvolume,AGVOLUME)
#define    agvoluma      F77_NAME(agvoluma,AGVOLUMA)
#define    uhtoc         F77_NAME(uhtoc,UHTOC)
#define    agfpath       F77_NAME(agfpath,UHTOC)
#if 0
#define    mfldgeo       F77_NAME(mfldgeo,MFLDGEO)
#endif
#define    agfdig0       F77_NAME(agfdig0,AGFDIG0)
#define    agfdpar       F77_NAME(agfdpar,AGFDPAR)
#if 0
#define    acfromr       F77_NAME(acfromr,ACFROMR)
#endif
#define    dstkine       F77_NAME(dstkine,DSTKINE)
#define    g2t_version   F77_NAME(g2t_version,G2T_VERSION)
#endif
typedef long int (*addrfun)(); 
R__EXTERN "C" {
  void type_of_call geometry();
  Int_t type_of_call agstroot();
  void type_of_call *csaddr(char *name, int l77name=0);
  long int type_of_call csjcal(
			       addrfun *fun, /* addres of external routine */
			       int  *narg,   /* number   of arguments      */
			       ...);         /* other narg arguments       */
  
  Int_t type_of_call g2t_volume_id (DEFCHARD, int* DEFCHARL);
  void type_of_call g2r_get_sys (DEFCHARD, DEFCHARD, int&, int& DEFCHARL DEFCHARL);
  void type_of_call gfrotm   (Int_t&,Float_t&,Float_t&,Float_t&,Float_t&,Float_t&,Float_t&);
  void type_of_call gfxzrm   (Int_t &NLEV_0,Float_t &X,Float_t &Y,Float_t &Z,
			      Float_t &TET1,Float_t &PHI1,
			      Float_t &TET2,Float_t &PHI2,
			      Float_t &TET3,Float_t &PHI3,Float_t &TYPE);
  void type_of_call agnzgete (Int_t &ILK,Int_t &IDE,
			      Int_t &NPART,Int_t &IRUN,Int_t &IEVT,DEFCHARD CGNAM,
			      Float_t *VERT,Int_t &IWTFL,Float_t &WEIGH DEFCHARL);
  void type_of_call dzddiv   (Int_t &,Int_t &,DEFCHARD,DEFCHARD,
			      Int_t &,Int_t &,Int_t &,Int_t & DEFCHARL DEFCHARL);
  /*
   * Input : ILK   - Link number  : 1 = primary, 2 = secondary (obsolete)    *
   *         IDE   - ID of event in gate ( ZEBRA IDN)                        *
   * Output: NPART - Number of particles in event record                     *
   *         IRUN  - run number as recorded by generator                     *
   *         IEVT  - event number as recorded by generator                   *
   *         CGNAM - generator name                                          *
   *         VERT(4)- x,y,z,t of event (metres,seconds or mm,mm/c)           *
   *         IWTFL - weight flag                                             *
   *         WEIGH - event weight                                            *
   */
  void type_of_call rootmaptable_(DEFCHARD,DEFCHARD,DEFCHARD, Int_t&,Char_t * 
				  DEFCHARL DEFCHARL DEFCHARL);
  Int_t type_of_call agvolume(TVolume*&, Float_t*&, Float_t*&, Float_t*&, Int_t&
                             ,Int_t&,    Float_t*&, Int_t&,    int *);
  Int_t type_of_call agvoluma(void*,void*,void*,void*,void*,void*,void*,void*,void*,void*);
  void type_of_call uhtoc(Int_t&,Int_t &,DEFCHARD,Int_t& DEFCHARL);
  int  type_of_call agfdig0 (const char*,const char*,int,int);
  void type_of_call agfdpar (float &hits,const char *chit, float &alim, float &blim, float &bin, int);
  void type_of_call agfpath(Int_t *);
  void type_of_call dstkine();
  Float_t type_of_call g2t_version(DEFCHARD DEFCHARL);
}
Char_t type_of_call *acfromr(Float_t r=8009359);

Quest_t  *St_geant_Maker::cquest; 
Gclink_t *St_geant_Maker::clink; 
Gcflag_t *St_geant_Maker::cflag; 
Gcvolu_t *St_geant_Maker::cvolu; 
Gcnum_t  *St_geant_Maker::cnum; 
Int_t    *St_geant_Maker::z_iq, *St_geant_Maker::z_lq; 
Float_t  *St_geant_Maker::z_q; 
Gcsets_t *St_geant_Maker::csets;
Gckine_t *St_geant_Maker::ckine;
Gcking_t *St_geant_Maker::cking;
Gctrak_t *St_geant_Maker::ctrak;
Gcmate_t *St_geant_Maker::cmate;
Gccuts_t *St_geant_Maker::ccuts;
Gcphys_t *St_geant_Maker::cphys;
Int_t     St_geant_Maker::nlev;
static Int_t irot = 0;
static TVolume *topnode=0;
typedef struct {
  Float_t par[50];
} params;
typedef struct {
  Float_t lseen, lstyle, lwidth, lcolor, lfill;
} attributes;

static Int_t ifz = 0;
ClassImp(St_geant_Maker)
  
TDataSet *St_geant_Maker::fgGeom = 0;
TGiant3  *St_geant_Maker::geant3 = 0;
St_geant_Maker *St_geant_Maker::fgGeantMk = 0;
static TTreeIter *MuDstIter = 0;
TString   St_geant_Maker::fgVertex("");
TString   St_geant_Maker::fgSpread("");
Bool_t    St_geant_Maker::fInitRunDone = kFALSE;

//_____________________________________________________________________________
St_geant_Maker::St_geant_Maker(const Char_t *name,Int_t nwgeant,Int_t nwpaw, Int_t iwtype):
  StMaker(name), 
   fNwGeant(nwgeant), fNwPaw(nwpaw), fIwType(iwtype),
   fVolume(0), fTopGeoVolume(0), 
  fInputFile(""),fGeoDirectory(0), fEvtHddr(0), 
  m_geom_gdat(0),mInitialization(""), mFieldOpt("")
{
  fgGeantMk = this;
  fgGeom  = new TDataSet("geom");  
  m_ConstSet->Add(fgGeom);
  SetOutput(fgGeom);	//Declare this "geom" for output
}
//_____________________________________________________________________________
TDataSet  *St_geant_Maker::FindDataSet (const char* logInput,const StMaker *uppMk,
                                        const StMaker *dowMk) const 
{
  bool lookupHall   = !strcmp(logInput,"HALL");
  bool lookupGeoDir = !strcmp(logInput,"GeometryDirectory");

  TDataSet *ds = 0;
  if ( !(lookupHall || lookupGeoDir) ) {
     ds = StMaker::FindDataSet(logInput,uppMk,dowMk); 
  } else {
     if (lookupHall) {
        ds = fVolume;
        if (!fVolume) {
           ((St_geant_Maker *)this)->Work();
           ds = fVolume;
           if (fVolume) {
              if (gGeometry) {
                 TList *listOfVolume = gGeometry->GetListOfNodes();

                 // Remove hall from the list of ROOT nodes to make it free of ROOT control
                 listOfVolume->Remove(fVolume);
                 listOfVolume->Remove(fVolume);
              }
              // Add "hall" into ".const" area of this maker
              ((St_geant_Maker *)this)->AddConst(fVolume);
              if (Debug()) fVolume->ls(3);
           }
        }
    } else if (lookupGeoDir) {
        if (!fGeoDirectory) {
           TString file("pams/geometry");
           // Check the local path first
           TFileSet *geoDir = new TFileSet(file.Data());
           if (!geoDir->FindByName("geometry.g")) {
              // Try the global one
              delete geoDir;
              TString starRoot = "$STAR/" + file;
              geoDir = new TFileSet(starRoot.Data());
              if (!geoDir->FindByName("geometry.g")) {
                  LOG_DEBUG << "NO STAR geometry source directory has been found" << endm;
                  delete geoDir; geoDir = 0;
              } else {
                 TString star("$STAR/pams");
                 gSystem->ExpandPathName(star);
                 geoDir->SetTitle(star.Data()); 
              }
           } else {
             TString wd = gSystem->WorkingDirectory();
             wd += "/pams";
             geoDir->SetTitle(wd.Data()); 
           }
           if (geoDir) {
              ((St_geant_Maker *)this)->fGeoDirectory = geoDir;              
               TDataSet *container = new TDataSet("GeometryDirectory");
               container->Add(geoDir);
               ds = fGeoDirectory;
              ((St_geant_Maker *)this)->AddConst(container);
              if (Debug()) fGeoDirectory->ls(3);
           }
        }
        ds = fGeoDirectory;
     }
  }
  return ds;  
}
//_____________________________________________________________________________
Int_t St_geant_Maker::Init(){
  // Initialize GEANT
  if (  geant3) return kStOK;
  PrintInfo();
  geant3 = new TGiant3("C++ Interface to Geant3",fNwGeant,fNwPaw,fIwType);
  assert(geant3);
  cquest = (Quest_t  *) geant3->Quest();
  clink  = (Gclink_t *) geant3->Gclink();
  cflag  = (Gcflag_t *) geant3->Gcflag();
  cvolu  = (Gcvolu_t *) geant3->Gcvolu();
  cnum   = (Gcnum_t  *) geant3->Gcnum();
  z_iq   = (Int_t    *) geant3->Iq();
  z_lq   = (Int_t    *) geant3->Lq();
  z_q    = (Float_t  *) geant3->Q();
  csets  = (Gcsets_t *) geant3->Gcsets();
  ckine  = (Gckine_t *) geant3->Gckine();
  cking  = (Gcking_t *) geant3->Gcking();
  ctrak  = (Gctrak_t *) geant3->Gctrak();
  cmate  = (Gcmate_t *) geant3->Gcmate();
  ccuts  = (Gccuts_t *) geant3->Gccuts();
  cphys  = (Gcphys_t *) geant3->Gcphys();
  Do("kuip/s/filecase KEEP");
  TString InputFile(fInputFile);
  if (fInputFile != "") {//check that first word contains .fz then add "gfile p" 
    //                                       -"-          .nt then add "user/input user" 
    if (Debug()) LOG_INFO << "St_geant_Maker::Init File " << fInputFile.Data() << endm;
    TString kuip("");
    if      (InputFile.Contains(".fz"))    {ifz = 1; kuip = "gfile p ";         kuip += InputFile;}
    else if (InputFile.Contains(".nt"))    {
      kuip = "user/input user "; 
      kuip += InputFile; 
      SetAttr("user/input",kTRUE);
      if (fgVertex != "") {Do(fgVertex.Data());  LOG_INFO << "St_geant_Maker::" << fgVertex.Data() << endm;}
      if (fgSpread != "") {Do(fgSpread.Data());  LOG_INFO << "St_geant_Maker::" << fgSpread.Data() << endm;}
    }
    else if (InputFile.Contains(".MuDst")) {
      if (! MuDstIter) MuDstIter = new TTreeIter();
      MuDstIter->AddFile(InputFile); 
      kuip = "user/input please MuDst.Dst";
      //      m_Mode = 10*(m_Mode/10);
      //      m_Mode += 1; // take time stamp from MuDst
      SetAttr("Don'tTouchTimeStamp",kTRUE);
      SetDatimeFromMuDst();
    }
    if (kuip != "") {
      if (Debug()) LOG_INFO << "St_geant_Maker::Init kuip " << kuip.Data() << endm;
      Do(kuip.Data()); 
      if (cquest->iquest[0] > kStOK) {
	LOG_INFO << "St_geant_Maker::Init File " << InputFile.Data() << " cannot be opened. Exit!" << endm;
	gSystem->Exit(1);
      }
      InputFile = "";
    }
  } else {  
    if (IsActive()) {// default
      Do("subevent 0;");
      TString CintF(SAttr("GeneratorFile"));
      if (CintF != "") {
	static const Char_t *path  = ".:./StarDb/Generators:$STAR/StarDb/Generators";
	Char_t *file = gSystem->Which(path,CintF,kReadPermission);
	if (! file) Fatal("St_geant_Maker::Init","File %s has not been found in path %s",CintF.Data(),path);
	else        Warning("St_geant_Maker::Init","File %s has been found as %s",CintF.Data(),file);
	TString command(Form(".L %s",file));
	TInterpreter::EErrorCode ee;
	gInterpreter->ProcessLine(command,&ee);
	assert(!ee);
	TDataSet *d = (TDataSet *) gInterpreter->Calc("CreateTable()",&ee);
	assert(!ee);
	AddConst(d);
	command.ReplaceAll(".L ",".U ");
	gInterpreter->ProcessLine(command,&ee);
	assert(!ee);
      } else {
	// gkine #particles partid ptrange yrange phirange vertexrange
	Do("gkine        20      6    1. 1. -1. 1. 0 6.28      0. 0.;");
      }
      Do("mode g2tm prin 1;");
      //  Do("next;");
      //  Do("dcut cave z 1 10 10 0.03 0.03;");
      //      if ((m_Mode/1000)%10 == 1) {// phys_off
      if (Debug() > 1) {
	Do("debug on;");
	Do("swit 2 2;");
      }
    }
  }
  return kStOK;
}
//________________________________________________________________________________
Int_t St_geant_Maker::InitRun(Int_t run){
  if (fInitRunDone) return kStOK;
  fInitRunDone = kTRUE;
  if (! ifz) {
    // Default cuts
    //  CUTS   CUTGAM CUTELE CUTHAD CUTNEU CUTMUO BCUTE BCUTM DCUTE DCUTM PPCUTM TOFMAX GCUTS[5]
    Do("CUTS     1e-3   1e-3   1e-3   1e-3   1e-3  1e-3  1e-3  1e-3  1e-3   1e-3 50.e-6");
    Do("LOSS 1"); // restricted energy loss fluctuations, instead of 2 => Full fluctuations without delta electrons
    if (IAttr("phys_off")) 	{
      LOG_INFO << "St_geant_Maker::Init switch off physics" << endm;
      Do("DCAY 0");
      Do("ANNI 0");
      Do("BREM 0");
      Do("COMP 0");
      Do("HADR 0");
      Do("MUNU 0");
      Do("PAIR 0");
      Do("PFIS 0");
      Do("PHOT 0");
      Do("RAYL 0");
      Do("LOSS 4"); // no fluctuations 
      //      Do("LOSS 1"); // with delta electron above dcute
      Do("DRAY 0");
      Do("MULS 0");
      Do("STRA 0");
    } else if (IAttr("hadr_off")) 	{
      LOG_INFO << "St_geant_Maker::Init switch off hadron interactions" << endm;
      Do("DCAY 1");
      Do("ANNI 0");
      Do("BREM 0");
      Do("COMP 0");
      Do("HADR 0");
      Do("MUNU 0");
      Do("PAIR 0");
      Do("PFIS 0");
      Do("PHOT 0");
      Do("RAYL 0");
      Do("LOSS 2"); 
      Do("DRAY 0");
      Do("MULS 1");
      Do("STRA 0");
    } else if (IAttr("flux")) {
      Do("call agustep");
      Do("HADR 6"); // gcalor
      //  CUTS   CUTGAM CUTELE CUTHAD CUTNEU CUTMUO BCUTE BCUTM DCUTE DCUTM PPCUTM TOFMAX GCUTS[5]
      //      Do("CUTS     1e-5   1e-5   1e-3  1e-14   1e-3  1e-3  1e-3  1e-3  1e-3   1e-3     10");
      //      Do("CUTS     1e-5   1e-5   1e-3  1e-14   1e-3  1e-3  1e-3  1e-3  1e-3   1e-3     1e-3");
      Do("CUTS     1e-5   1e-5   1e-3  1e-14   1e-3  1e-3  1e-3  1e-3  1e-3   1e-3     1e3");
    }
    LOG_INFO << "St_geant_Maker::InitRun -- Do geometry initialization" << endm;
    LOG_INFO << "St_geant_Maker::InitRun -- with " << mInitialization.Data() << endm;
    if (mInitialization != "") {Do(mInitialization.Data()); mInitialization = "";}
    Geometry();
    Do("physi");
    Do("gclose all");
  }
  Agstroot();
  if (IAttr("RunG")) {
    LOG_INFO << "St_geant_Maker::InitRun -- Set RunG/rndm " << IAttr("RunG") << endl;
    //    Do(Form("rung %d %d",gSystem->GetPid(),IAttr("RunG")));
    Do(Form("ranlux %d",IAttr("RunG")));
    Do("rndm");
  }
  LOG_INFO << "St_geant_Maker::InitRun -- Do mag.field initialization" << endm;
  m_geom_gdat = (St_geom_gdat *) Find(".runco/geom/geom_gdat");
  if (! m_geom_gdat) {
    St_geom_gdat *geom_gdat = (St_geom_gdat *) Find(".const/geom/geom_gdat");
    if ( geom_gdat)  {
      m_geom_gdat = new St_geom_gdat(*geom_gdat);
      AddRunco(m_geom_gdat);
    }
  }
  //  if (m_Mode%10 != 1 && IsActive() ) {// Mixer mode == 1 or reco - do not modify EvtHddr and MagF
  if (! IAttr("Don'tTouchTimeStamp") && IsActive() ) {// Mixer mode == 1 or reco - do not modify EvtHddr and MagF
    fEvtHddr = (StEvtHddr*) GetTopChain()->GetDataSet("EvtHddr");
    if (!fEvtHddr) {                            // Standalone run
      fEvtHddr = new StEvtHddr(m_ConstSet);
      fEvtHddr->SetRunNumber(0);                // to have run positive and < 1000000 (to avoid mess with RunLog)
      SetOutput(fEvtHddr);	              //Declare this "EvtHddr" for output
    }
    if (! ifz ) {
      // if Simulation is read from zebra file set Scale to value got from the file
      // if Simulation is done on fly use mFieldOpt field option set by StBFChain
      // if data use Scale for Db unless it has been reset by StBFChain field option (in IsActive)
      if (! m_geom_gdat) {
	m_geom_gdat = new St_geom_gdat("geom_gdat",1);
	AddRunco(m_geom_gdat);
	geom_gdat_st row = {{0,0}, 1, "gstar"};
	m_geom_gdat->AddAt(&row);
      }
    } else {
      // set mag. field from already simulated data, only 5 option allowed
      if (MuDstIter) {
	// set mag field
	if (StarMagField::Instance() && StarMagField::Instance()->IsLocked()) {
	  // Passive mode, do not change scale factor
	  LOG_INFO << "St_geant_Maker::InitRun passive mode. Don't update Mag.Field from DB" << endm;
	} else {
	  Float_t  fScale = St_MagFactorC::instance()->ScaleFactor();
	  if (TMath::Abs(fScale) < 1e-3) fScale = 1e-3;
	  LOG_INFO << "St_geant_Maker::InitRun active mode ";
	  if (! StarMagField::Instance()) {
	    new StarMagField ( StarMagField::kMapped, fScale);
	    LOG_INFO << "Initialize STAR magnetic field with scale factor " << fScale << endm;
	  }  else {
	    StarMagField::Instance()->SetFactor(fScale);
	    LOG_INFO << "Reset STAR magnetic field with scale factor " << fScale << endm;
	  }
	}
      } else {
	Float_t mfscale = 1; 
	if (m_geom_gdat)  {
	  geom_gdat_st *gdat = m_geom_gdat->GetTable();
	  mfscale = gdat->mfscale;
	  LOG_INFO << "St_geant_Maker::Init geom_gdata is found in fz-file ! ";
	} else {
	  St_mfld_mflg *mfld_mflg = (St_mfld_mflg *) Find(".const/geom/mfld_mflg");
	  if (mfld_mflg) {
	    LOG_INFO << "St_geant_Maker::Init mfld_mflg is found in fz-file ! ";
	    mfld_mflg_st *s = mfld_mflg->GetTable();
	    mfscale = s->bfield/5.0;
	  } else {
	    LOG_INFO << "St_geant_Maker::Init geom_gdata is missing in fz-file ! Use default mag.field scale factor ";
	  }
	}
	LOG_INFO  << "St_geant_Maker::Init mfscale = " << mfscale		       << endm;
	struct Field_t {
	  const Char_t *name;
	  Float_t scale;
	};
	const Field_t FieldOptions[5] = {
	  {"FullMagFNegative", -1.0},
	  {"FullMagFPositive",  1.0},
	  {"HalfMagFNegative", -0.5},
	  {"HalfMagFPositive",  0.5},
	  {"ZeroMagF",          0.0}
	};
	TString FieldOption("");
	for (Int_t i = 0; i < 5; i++) {
	  if (TMath::Abs(mfscale - FieldOptions[i].scale) < 2.e-2) {
	    FieldOption = FieldOptions[i].name;
	    break;
	  }
	}
	if (FieldOption != "") {
	  SetFlavor(FieldOption.Data(),        "MagFactor");
	  LOG_INFO << "St_geant_Maker::Init  SetFlavor(\"" << FieldOption.Data() 
			     << "\",\"MagFactor\")" << endm;
	}
	delete StarMagField::Instance();
	if (! StarMagField::Instance()) {
	  new StarMagField ( StarMagField::kMapped, mfscale, kTRUE);
	  LOG_INFO << "St_geant_Maker::Init  Create StarMagField and lock it"
			     << endm;
	}
	else {
	  StarMagField::Instance()->SetFactor(mfscale);
	  StarMagField::Instance()->SetLock();
	  LOG_INFO << "St_geant_Maker::Init  Reset StarMagField and lock it"
			     << endm;
	}
      }
    }
    if (! ifz && IAttr("beamLine")) {
      St_vertexSeedC* vSeed = St_vertexSeedC::instance();
      if (vSeed) {
	Double_t x0   = vSeed->x0()  ; Double_t err_x0   = vSeed->err_x0();
	Double_t y0   = vSeed->y0()  ; Double_t err_y0   = vSeed->err_y0();
	Double_t z0   = 0            ; Double_t err_z0   = 60; 
	Double_t dxdz = vSeed->dxdz();
	Double_t dydz = vSeed->dydz(); 
	Do(Form("gvertex   %f %f %f",x0,y0,z0)); // ** setup the vertex
	Do(Form("gspread %f %f %f",err_x0,err_y0,err_z0));
	Do(Form("gslope  %f %f", dxdz, dydz));
      }
    }
  }
  if (IAttr("fzout") && GetChain()->GetTFile()) {
    TString  fzname("gfile o ");
    fzname += GetChain()->GetTFile()->GetName();
    fzname.ReplaceAll(".root",".fz");
    Do(fzname);
  }
  return kStOK;
}
//_____________________________________________________________________________
Int_t St_geant_Maker::Make() {
  Int_t    nhits,nhit1,nhit2,nhit3,nhit4,link=1,ide=1,npart,irun,ievt,iwtfl;
  Float_t  vert[4],weigh;
  if (GetDebug()) { Do("debug on;"); } else {Do("debug off;"); }
  int iRes = 0; if(iRes) {/*touch*/};
  Do("trig");
  
  // check EoF
  if (cquest->iquest[0]) {return kStEOF;}
  Int_t Nwhead,Ihead[100];
  Int_t Nwbuf;
  Float_t Ubuf[100];
  
  // prepare an empty g2t_event
  St_g2t_event *g2t_event = new St_g2t_event("g2t_event",1);  
  m_DataSet->Add(g2t_event);
  
  Char_t   cgnam[21] = "                   \0";                               
  Agnzgete(link,ide,npart,irun,ievt,cgnam,vert,iwtfl,weigh);
  geant3->Gfhead(Nwhead,Ihead,Nwbuf,Ubuf);
  //  if (m_Mode%10 != 1) {
  if (! IAttr("Don'tTouchTimeStamp")) {
    if (fEvtHddr) {
      if (clink->jhead) {
	if (fEvtHddr->GetRunNumber() != *(z_iq+clink->jhead+1)) 
	  fEvtHddr->SetRunNumber(*(z_iq+clink->jhead+1));
	fEvtHddr->SetEventNumber(*(z_iq+clink->jhead+2));
      }
      if (fInputFile != "") fEvtHddr->SetEventType(TString(gSystem->BaseName(fInputFile.Data()),7));
      fEvtHddr->SetProdDateTime();
#if 1
      SetDateTime();
#endif
    }
  }
  if (npart>0) {  
    St_particle  *particle   = new St_particle("particle",npart);
    m_DataSet->Add(particle);  iRes = g2t_particle(particle);
    //    =======================
    if (Debug() > 1) particle->Print(0,10);
    particle_st *p = particle->GetTable();
    
    // 20030508 --max-- found a bug: 9999999
    // "istat==10" on the following line, changing to >=11
    // This "if should now work with both "old" and "new" ntuple conventions
    //    if (m_Mode%10 != 1) {
    if (! IAttr("Don'tTouchTimeStamp")) {
      if ( (p->isthep == 10 && p->idhep  == 9999999 && fEvtHddr) ||
	   (p->isthep >= 11 && p->idhep  == 999998  && fEvtHddr)) {
	
	fEvtHddr->SetBImpact  (p->phep[0]);
	fEvtHddr->SetPhImpact (p->phep[1]);
	fEvtHddr->SetCenterOfMassEnergy(p->phep[2]);
	
	// Obsoleted: --max--
	// 	fEvtHddr->SetGenerType((int)p->phep[2]);
	// 	Int_t west = (int)p->phep[4];
	// 	Int_t east = (int)(1000.*p->phep[4]-1000.*((float)west));
	// 	fEvtHddr->SetAWest(west);
	// 	fEvtHddr->SetAEast(east);
	
	// Update the run number, if necessary
	//	if ( m_Mode%100 != 1 && 
	if (! IAttr("KeepRunNumber") && 
	     p->vhep[0] > 0 && p->vhep[0] < 10000 && 
	     fEvtHddr->GetRunNumber() != p->vhep[0]) {
	  fEvtHddr->SetRunNumber((int)p->vhep[0]);
	  
	  fEvtHddr->SetEventNumber((int)p->vhep[1]);
	  Int_t id = p->jdahep[0];
	  Int_t it = p->jdahep[1];
	  
	  if (id <=        0) id = 19991231;
	  if (id <= 19000000) id +=19000000;
	  if (id >= 20500000) id = 19991231;
	  if (it <         0) it = 235959;
	  if (it >    246060) it = 235959;
	  fEvtHddr->SetDateTime(id,it);
	}
      }
    }
  }
  
  if (!cnum->nvertx || !cnum->ntrack) return kStErr;
  St_g2t_vertex  *g2t_vertex  = new St_g2t_vertex("g2t_vertex",cnum->nvertx);
  m_DataSet->Add(g2t_vertex); 
  St_g2t_track   *g2t_track   = new St_g2t_track ("g2t_track",cnum->ntrack);
  m_DataSet->Add(g2t_track);
  
  iRes = g2t_get_kine (g2t_vertex,g2t_track);
  if (Debug() > 1) {
    g2t_vertex->Print(0,10);
    g2t_track->Print(0,10);
  }

  iRes = g2t_get_event(g2t_event);
  if (Debug() > 1) {
    g2t_event->Print(0,10);
  }

  if(iRes>=10) { // means there was Pythia information detected in the input, see g2t_get_event code
    St_g2t_pythia *g2t_pythia = new St_g2t_pythia("g2t_pythia",1); // prepare an empty g2t_pythia
    m_DataSet->Add(g2t_pythia);
    LOG_INFO << "Pythia event header captured" << endm;
    iRes = g2t_get_pythia(g2t_pythia);
  }
  
  // --max-- Filling the event header, addition due to the new coding
  //  if (m_Mode%10 != 1) {
  if (! IAttr("Don'tTouchTimeStamp") ) {
    if(fEvtHddr) {
      fEvtHddr->SetAEast((*g2t_event)[0].n_wounded_east);
      fEvtHddr->SetAWest((*g2t_event)[0].n_wounded_west);
    }
  }
  //---------------------- inner part -------------------------//

  // Note that we treat the SSD (or SISD as we call it in geo) on
  // the same basis as the SVT, which applies since we decoupled
  // the two detectors
  nhits = 0;
  geant3->Gfnhit("SVTH","SVTD", nhits);
  if (nhits>0) { 
    St_g2t_svt_hit *g2t_svt_hit = new St_g2t_svt_hit("g2t_svt_hit",nhits);
    m_DataSet->Add(g2t_svt_hit); 
    
    iRes = g2t_svt(g2t_track,g2t_svt_hit); if (Debug() > 1) g2t_svt_hit->Print(0,10);
    //	     ===============================
  }

  nhits = 0;
  geant3->Gfnhit("SISH","SFSD", nhits);
  if (nhits>0) { 
    St_g2t_ssd_hit *g2t_ssd_hit = new St_g2t_ssd_hit("g2t_ssd_hit",nhits);
    m_DataSet->Add(g2t_ssd_hit);
    
    iRes = g2t_ssd(g2t_track,g2t_ssd_hit); if (Debug() > 1) g2t_ssd_hit->Print(0,10);
    //	     ===============================
  }

  
  nhits = 0;
  geant3->Gfnhit("PIXH","PLAC", nhits);
  
  if (nhits>0) { 
    St_g2t_pix_hit *g2t_pix_hit = new St_g2t_pix_hit("g2t_pix_hit",nhits);
    m_DataSet->Add(g2t_pix_hit);
    
    iRes = g2t_pix(g2t_track,g2t_pix_hit); if (Debug() > 1) g2t_pix_hit->Print(0,10);
    //	     ===============================
  }
  
  nhits = 0;
  geant3->Gfnhit("HPDH","YPLA", nhits);
  
  if (nhits>0) { 
    St_g2t_hpd_hit *g2t_hpd_hit = new St_g2t_hpd_hit("g2t_hpd_hit",nhits);
    m_DataSet->Add(g2t_hpd_hit);
    
    iRes = g2t_hpd(g2t_track,g2t_hpd_hit); if (Debug() > 1) g2t_hpd_hit->Print(0,10);
    //	     ===============================
  }

  nhits = 0;
  geant3->Gfnhit("ISTH","IBSS", nhits);
  
  if (nhits>0) { 
    St_g2t_ist_hit *g2t_ist_hit = new St_g2t_ist_hit("g2t_ist_hit",nhits);
    m_DataSet->Add(g2t_ist_hit);
    
    iRes = g2t_ist(g2t_track,g2t_ist_hit); if (Debug() > 1) g2t_ist_hit->Print(0,10);
    //	     ===============================
  }

  nhits = 0;
  geant3->Gfnhit("GEMH","GMDI", nhits);
  
  if (nhits>0) { 
    St_g2t_gem_hit *g2t_gem_hit = new St_g2t_gem_hit("g2t_gem_hit",nhits);
    m_DataSet->Add(g2t_gem_hit);
    
    iRes = g2t_gem(g2t_track,g2t_gem_hit); if (Debug() > 1) g2t_gem_hit->Print(0,10);
    //	     ===============================
  }

  nhits = 0;
  geant3->Gfnhit("IGTH","IGAL", nhits);
  
  if (nhits>0) { 
    St_g2t_igt_hit *g2t_igt_hit = new St_g2t_igt_hit("g2t_igt_hit",nhits);
    m_DataSet->Add(g2t_igt_hit);
    
    iRes = g2t_igt(g2t_track,g2t_igt_hit); if (Debug() > 1) g2t_igt_hit->Print(0,10);
    //	     ===============================
  }

  nhits = 0;
  geant3->Gfnhit("FSTH","FDSW", nhits);
  
  if (nhits>0) { 
    St_g2t_fst_hit *g2t_fst_hit = new St_g2t_fst_hit("g2t_fst_hit",nhits);
    m_DataSet->Add(g2t_fst_hit);
    
    iRes = g2t_fst(g2t_track,g2t_fst_hit); if (Debug() > 1) g2t_fst_hit->Print(0,10);
    //	     ===============================
  }

  int myNhits=0;
  geant3->Gfnhit("FGTH","FGZC", myNhits);
  nhits = myNhits;
  geant3->Gfnhit("FGTH","FGZD", myNhits);
  nhits+= myNhits;
  geant3->Gfnhit("FGTH","FZCB", myNhits); // from FgtmGeoV.xml
  nhits+= myNhits;
  if (nhits>0) { 
    St_g2t_fgt_hit *g2t_fgt_hit = new St_g2t_fgt_hit("g2t_fgt_hit",nhits);
    m_DataSet->Add(g2t_fgt_hit);
    
    iRes = g2t_fgt(g2t_track,g2t_fgt_hit); if (Debug() > 1) g2t_fgt_hit->Print(0,10);
    //	     ===============================
  }

  geant3->Gfnhit("TPCH","TPAD", nhits);
  if (nhits>0){ 
    St_g2t_tpc_hit *g2t_tpc_hit = new St_g2t_tpc_hit("g2t_tpc_hit",nhits);
    m_DataSet->Add(g2t_tpc_hit);
    
    iRes = g2t_tpc(g2t_track,g2t_tpc_hit); if (Debug() > 1) g2t_tpc_hit->Print(0,10);
    //	     ==============================
  }
  
  
  nhits = 0;
  geant3->Gfnhit("TPCH","TMSE", nhits);
  if (nhits>0) { 
    St_g2t_mwc_hit *g2t_mwc_hit = new St_g2t_mwc_hit("g2t_mwc_hit",nhits);
    m_DataSet->Add(g2t_mwc_hit);
    iRes = g2t_mwc(g2t_track,g2t_mwc_hit);if (Debug() > 1) g2t_mwc_hit->Print(0,10);
    //	     ==============================
  }
  
  nhits = 0;
  geant3->Gfnhit("FTPH","FSEC", nhits);
  if (nhits>0){
    St_g2t_ftp_hit *g2t_ftp_hit = new St_g2t_ftp_hit("g2t_ftp_hit",nhits);
    m_DataSet->Add(g2t_ftp_hit);
    iRes = g2t_ftp(g2t_track,g2t_ftp_hit);
    //           ===============================
  }
  
  nhits = 0;
  geant3->Gfnhit("BTOH","BXSA", nhits);
  if (nhits>0) { 
    St_g2t_ctf_hit *g2t_ctb_hit = new St_g2t_ctf_hit("g2t_ctb_hit",nhits);
    m_DataSet->Add(g2t_ctb_hit);
    iRes = g2t_ctb(g2t_track,g2t_ctb_hit); if (Debug() > 1) g2t_ctb_hit->Print(0,10);
    //           ==============================
  }
  
  nhits = 0;
  geant3->Gfnhit("BTOH","BCSB", nhits);
  if (nhits>0) {
    St_g2t_ctf_hit *g2t_tof_hit = new St_g2t_ctf_hit("g2t_tof_hit",nhits);
    m_DataSet->Add(g2t_tof_hit);
    iRes = g2t_tof(g2t_track,g2t_tof_hit); if (Debug() > 1) g2t_tof_hit->Print(0,10);
    //           ==============================
  }
  
  nhits = 0;
  geant3->Gfnhit("BTOH","BRSG", nhits);
  if (nhits>0) {
    St_g2t_ctf_hit *g2t_tfr_hit = new St_g2t_ctf_hit("g2t_tfr_hit",nhits);
    m_DataSet->Add(g2t_tfr_hit);
    iRes = g2t_tfr(g2t_track,g2t_tfr_hit); if (Debug() > 1) g2t_tfr_hit->Print(0,10);
    //           ==============================
  }
  
  
  nhit1 = nhit2 = nhit3 = nhit4 = 0;
  geant3->Gfnhit("RICH","RGAP", nhit1);
  geant3->Gfnhit("RICH","RCSI", nhit2);
  geant3->Gfnhit("RICH","FREO", nhit3);
  geant3->Gfnhit("RICH","QUAR", nhit4);
  //  LOG_INFO  << nhit1 << " " << nhit2 << " " << nhit3 << " " << nhit4 << endm;
  nhits=nhit1+nhit2+nhit3+nhit4;
  if (nhits>0) {
    St_g2t_rch_hit *g2t_rch_hit = new St_g2t_rch_hit("g2t_rch_hit",nhits);
    m_DataSet->Add(g2t_rch_hit);
    iRes = g2t_rch(g2t_track,g2t_rch_hit); if (Debug() > 1) g2t_rch_hit->Print(0,10);
    //           ==============================
  }
  
  //---------------------- calorimeters -------------------------//
  nhits = 0;
  geant3->Gfnhit("CALH","CSUP", nhits);
  if (nhits>0) {
    St_g2t_emc_hit *g2t_emc_hit = new St_g2t_emc_hit("g2t_emc_hit",nhits);
    m_DataSet->Add(g2t_emc_hit);
    iRes = g2t_emc(g2t_track,g2t_emc_hit); if (Debug() > 1) g2t_emc_hit->Print(0,10);
    //           ==============================
  }
  
  nhits = 0;
  geant3->Gfnhit("CALH","CSDA", nhits);
  if (nhits>0) {
    St_g2t_emc_hit *g2t_smd_hit = new St_g2t_emc_hit("g2t_smd_hit",nhits);
    m_DataSet->Add(g2t_smd_hit);
    iRes = g2t_smd(g2t_track,g2t_smd_hit); if (Debug() > 1) g2t_smd_hit->Print(0,10);
    //           ==============================
  }
  
  nhit1 = nhit2 = nhit3 = 0;
  geant3->Gfnhit("ECAH","ESCI", nhit1);
  geant3->Gfnhit("ECAH","ELGR", nhit2);
  geant3->Gfnhit("ECAH","EPCT", nhit3);
  nhits = nhit1+nhit2+nhit3; 
  if (nhits>0) {
    St_g2t_emc_hit *g2t_eem_hit = new St_g2t_emc_hit("g2t_eem_hit",nhits);
    m_DataSet->Add(g2t_eem_hit);
    iRes = g2t_eem(g2t_track,g2t_eem_hit); if (Debug() > 1) g2t_eem_hit->Print(0,10);
    //           ==============================
  }
  nhit1 = nhit2 = 0;
  geant3->Gfnhit("ECAH","EXSE", nhit1);
  geant3->Gfnhit("ECAH","EHMS", nhit2);
  nhits = nhit1+nhit2;
  if (nhits>0) {
    St_g2t_emc_hit *g2t_esm_hit = new St_g2t_emc_hit("g2t_esm_hit",nhits);
    m_DataSet->Add(g2t_esm_hit);
    iRes = g2t_esm(g2t_track,g2t_esm_hit); if (Debug() > 1) g2t_esm_hit->Print(0,10);
    //           ==============================
  }
  
  nhits = 0;
  geant3->Gfnhit("VPDH","VRAD", nhits);
  if (nhits>0) {
    St_g2t_vpd_hit *g2t_vpd_hit = new St_g2t_vpd_hit("g2t_vpd_hit",nhits);
    m_DataSet->Add(g2t_vpd_hit);
    iRes = g2t_vpd(g2t_track,g2t_vpd_hit); if (Debug() > 1) g2t_vpd_hit->Print(0,10);
    //           ==============================
  }
  
  nhits = 0;
  geant3->Gfnhit("PHMH","PDGS", nhits);
  if (nhits>0) {
    St_g2t_pmd_hit *g2t_pmd_hit = new St_g2t_pmd_hit("g2t_pmd_hit",nhits);
    m_DataSet->Add(g2t_pmd_hit);
    iRes = g2t_pmd(g2t_track,g2t_pmd_hit); if (Debug() > 1) g2t_pmd_hit->Print(0,10);
    //           ==============================
  }
  
  nhits = 0;
  geant3->Gfnhit("ZCAH","QSCI", nhits);
  if (nhits>0) {
    St_g2t_emc_hit *g2t_zdc_hit = new St_g2t_emc_hit("g2t_zdc_hit",nhits);
    m_DataSet->Add(g2t_zdc_hit);
    iRes = g2t_zdc(g2t_track,g2t_zdc_hit); if (Debug() > 1) g2t_zdc_hit->Print(0,10);
    //           ==============================
  }
  
  
  nhits = 0;
  geant3->Gfnhit("BBCH","BPOL", nhits);
  if (nhits>0) 
    {
      St_g2t_ctf_hit *g2t_bbc_hit = new St_g2t_ctf_hit("g2t_bbc_hit",nhits);
      m_DataSet->Add(g2t_bbc_hit);
      iRes = g2t_bbc(g2t_track,g2t_bbc_hit); if (Debug() > 1) g2t_bbc_hit->Print(0,10);
      //           ==============================
    }

  nhit1 = nhit2 = nhit3 = 0;
  geant3->Gfnhit("FPDH","FLGR",nhit1);
  geant3->Gfnhit("FPDH","FLXF",nhit2);
  geant3->Gfnhit("FPDH","FPSC",nhit3);
  nhits = nhit1+nhit2+nhit3;
  if (nhits>0) {
    St_g2t_emc_hit* g2t_fpd_hit = new St_g2t_emc_hit("g2t_fpd_hit",nhits);
    m_DataSet->Add(g2t_fpd_hit);
    iRes = g2t_fpd(g2t_track,g2t_fpd_hit); if (Debug() > 1) g2t_fpd_hit->Print(0,10);
    //           ==============================
  }

  nhits = 0;
  geant3->Gfnhit("FSCH","FSCT", nhits);
  if (nhits>0) {
    St_g2t_emc_hit *g2t_fsc_hit = new St_g2t_emc_hit("g2t_fsc_hit",nhits);
    m_DataSet->Add(g2t_fsc_hit);
    iRes = g2t_fsc(g2t_track,g2t_fsc_hit); if (Debug() > 1) g2t_fsc_hit->Print(0,10);
    //           ==============================
  }

  nhits = 0; nhit1 = 0; nhit2 = 0; nhit3 = 0;
  geant3->Gfnhit("MUTH","MIGG", nhit1);
  geant3->Gfnhit("MUTH","MTTT", nhit2);
  geant3->Gfnhit("MUTH","MTTF", nhit3); 
  nhits=nhit1+nhit2+nhit3;
  if (nhits>0) {
    St_g2t_mtd_hit *g2t_mtd_hit = new St_g2t_mtd_hit("g2t_mtd_hit",nhits);
    m_DataSet->Add(g2t_mtd_hit);
    iRes = g2t_mtd(g2t_track,g2t_mtd_hit); if (Debug() > 1) g2t_mtd_hit->Print(0,10);
    //           ==============================
  }
 
  nhits = 0;
  geant3 -> Gfnhit("EIDH","TABD", nhits);
  if ( nhits > 0 ) 
    {
      St_g2t_etr_hit *g2t_etr_hit = new St_g2t_etr_hit("g2t_etr_hit",nhits);
      m_DataSet->Add(g2t_etr_hit);
      iRes = g2t_etr( g2t_track, g2t_etr_hit); 
      if ( Debug() > 1 ) g2t_etr_hit->Print(0,10);
    }
  


  nhits = 0; nhit1=nhit2=0;  
  geant3 -> Gfnhit("HCAH","HCEL", nhit1); nhits+=nhit1;
  geant3 -> Gfnhit("HCAH","HCES", nhit1); nhits+=nhit1;
  geant3 -> Gfnhit("HCAH","FPSC", nhit1); nhits+=nhit1;
  geant3 -> Gfnhit("HCAH","BBCF", nhit1); nhits+=nhit1;
  geant3 -> Gfnhit("HCAH","BBCB", nhit1); nhits+=nhit1;
  geant3 -> Gfnhit("HCAH","LEDG", nhit1); nhits+=nhit1; 
  if ( nhits > 0 ) 
     {
        St_g2t_emc_hit *g2t_hca_hit = new St_g2t_emc_hit("g2t_hca_hit",nhits);
        m_DataSet->Add(g2t_hca_hit);
        iRes = g2t_hca( g2t_track, g2t_hca_hit);
     } 



  //------------------------all bloody detectors done--------------------//
#if 0
  Char_t *g2t = "g2t_";
  Int_t  narg = 0;
  addrfun address  = (addrfun ) csaddr(g2t);
  if (address) csjcal(&address,&narg);
#endif
  
  if (cflag->ieorun) return kStEOF; 
  if (cflag->ieotri) return kStErr; 
  if (IAttr("flux")) {
    static TH1D *Vx = 0, *Vy = 0, *Vz = 0;
    static TH2D *BbcC = 0;
    if (! Vx) {
      if (GetChain()->GetTFile()) GetChain()->GetTFile()->cd();
      Vx = new TH1D("Vx","Geant primary vertex X",50,-5.0,5.0);
      Vy = new TH1D("Vy","Geant primary vertex Y",50,-5.0,5.0);
      Vz = new TH1D("Vz","Geant primary vertex Z",50,-100,100);
    }
    St_g2t_vertex *geantVertex=(St_g2t_vertex *) GetDataSet("g2t_vertex"); 
    g2t_vertex_st *gvt=geantVertex->GetTable();
    Vx->Fill(gvt->ge_x[0]);
    Vy->Fill(gvt->ge_x[1]);
    Vz->Fill(gvt->ge_x[2]);
    if (! BbcC) {
      BbcC = new TH2D("BbcC","BBC East versus BBC West",100,-1,9,100,-1,9);
    }
    Double_t BbcW = 0, BbcE = 0;
    St_g2t_ctf_hit *g2t_bbc_hit = (St_g2t_ctf_hit *) GetDataSet("g2t_bbc_hit");
    Int_t N = g2t_bbc_hit->GetNRows();
    g2t_ctf_hit_st *bbc = g2t_bbc_hit->GetTable();
    for (Int_t i = 0; i < N; i++, bbc++) {
      if (bbc->tof > 100e-9) continue;
      if (bbc->volume_id < 2000) BbcW++;
      else                       BbcE++;
    }
    if (BbcW  <= 0) BbcW = 0.1;
    if (BbcE  <= 0) BbcE = 0.1;
    BbcC->Fill(TMath::Log10(BbcW),TMath::Log10(BbcE));
  }
  return kStOK;
}
//_____________________________________________________________________________
void St_geant_Maker::LoadGeometry(const Char_t *option){
  mInitialization = option; 
}
//_____________________________________________________________________________
void St_geant_Maker::Draw(const char* opt)
{ 
  Int_t two = 2;
  Int_t zero = 0;
  Int_t one = 1;
  const char *path = " ";
  Dzddiv (two,zero,path,opt,one,zero,one,one);
}
//_____________________________________________________________________________
void St_geant_Maker::Do(const Char_t *job)
{  
  Int_t l=strlen(job);
  if (l) {
    LOG_INFO << "St_geant_Maker::Do(\"" << job << "\")" << endm;
    geant3->Kuexel(job);
  }
}
//_____________________________________________________________________________

//_____________________________________________________________________________
TVolume *St_geant_Maker::MakeVolume(TString *name, Int_t ivo, Int_t Nlevel, Int_t *Names, Int_t *Numbers){
  TVolume *node = 0;
  Int_t   jvolum  = clink->jvolum;
  Int_t jvo = z_lq[jvolum-ivo];
  if (jvo) {
    node = (TVolume *) topnode->FindObject(name->Data());
    if (! node) {
      TShape *shape = (TShape *) gGeometry->GetListOfShapes()->FindObject(name->Data());
      if (!shape ) {shape = MakeShape(name,ivo);}
      node = new TVolume(name->Data(), name->Data(), shape);
    }
    
    Int_t nin =(Int_t) z_q[jvo+3];
    if (nin > 0) 
      {
	Nlevel++;
	for (Int_t in=1; in<= nin; in++) 
	  {
	    Int_t jin   =        z_lq[jvo-in];
	    Int_t ivom  = (Int_t) z_q[jin+2];
	    Int_t nuser = (Int_t) z_q[jin+3];
	    TString  namem((const Char_t *) &(z_iq[jvolum+ivom]), 4);
	    
	    Names[Nlevel] = z_iq[jvolum+ivom];
	    Numbers[Nlevel] = nuser;
	    Int_t   nlevv = Nlevel+1;
	    Int_t   Ierr;
	    Float_t xx[3], theta1,phi1, theta2,phi2, theta3,phi3, type;
	    
	    Ierr = geant3->Glvolu(nlevv, Names, Numbers);
	    
	    Gfxzrm(Nlevel, xx[0],xx[1],xx[2], 
		   theta1,phi1, theta2,phi2, theta3,phi3, type);
	    TVolume *newnode = (TVolume *) topnode->FindObject(namem.Data());
	    
	    if (!newnode) 
	      {  newnode = MakeVolume(&namem, ivom, nlevv, Names, Numbers);  }
	    
	    irot++;
	    Char_t ss[12];
	    sprintf(ss,"rotm%i",irot);
	    TRotMatrix *rotm = new TRotMatrix(ss,ss, 
					      theta1,phi1, theta2,phi2, theta3,phi3);
	    node->Add(newnode,xx[0],xx[1],xx[2],rotm);
	  }
      }
    if (nin < 0) {
      Nlevel++;
    }
    if (nin == 0) {Nlevel--;}
  }
  return node;
}
//_____________________________________________________________________________
TShape *St_geant_Maker::MakeShape(TString *name, Int_t ivo){
  // make geant3 volume
  typedef enum {BOX=1,TRD1,TRD2,TRAP,TUBE,TUBS,CONE,CONS,SPHE,PARA,
		PGON,PCON,ELTU,HYPE,GTRA=28,CTUB} shapes;
  Int_t jvolum  = clink->jvolum;
  Int_t jvo = z_lq[jvolum-ivo];
  TShape*  t;
  shapes   shape  = (shapes) z_q[jvo+2];
  Int_t    numed  = (Int_t)  z_q[jvo+4];
  Int_t    npar   = (Int_t)  z_q[jvo+5];
  params  *p      = (params *)&z_q[jvo+7];
  attributes *att = (attributes *)(&z_q[jvo+7] + npar);
  Int_t    jtmed  = clink->jtmed;
  Int_t    jtm    =          z_lq[jtmed-numed];
  Int_t    nmat   =          (int)z_q[jtm+6];
  Int_t    jmate  = clink->jmate;
  Int_t    jma    =          z_lq[jmate-nmat];
  Int_t    nmixt  = (Int_t)  z_q[jma+11];
  Int_t    nm     = TMath::Abs(nmixt);
  
  Char_t   astring[20];
  if (nm <= 1)  sprintf (astring,"mat%i",nmat); 
  else          sprintf (astring,"mix%i",nmat);   
  
  TString Astring(astring);
  t = (TShape *) gGeometry->GetListOfShapes()->FindObject(name->Data());
  if (!t) {
    switch (shape) {
    case BOX:  t = new TBRIK((Char_t *) name->Data(),"BRIK",(Char_t *) Astring.Data(),
			     p->par[0],p->par[1],p->par[2]);      
    break;
    case TRD1: t = new TTRD1((Char_t *) name->Data(),"TRD1",(Char_t *) Astring.Data(),
			     p->par[0],p->par[1],p->par[2],p->par[3]);
    break;
    case TRD2: t = new TTRD2((Char_t *) name->Data(),"TRD2",(Char_t *) Astring.Data(),
			     p->par[0],p->par[1],p->par[2],p->par[3],p->par[4]);
    break;
    case TRAP: t = new TTRAP((Char_t *) name->Data(),"TRAP",(Char_t *) Astring.Data(),
			     p->par[0],p->par[1],p->par[2],p->par[3],p->par[4],
			     p->par[5],p->par[6],p->par[7],p->par[8],p->par[9],
			     p->par[10]);
    break;
    case TUBE: t = new TTUBE((Char_t *) name->Data(),"TUBE",(Char_t *) Astring.Data(),
			     p->par[0],p->par[1],p->par[2]); 
    break;
    case TUBS: t = new TTUBS((Char_t *) name->Data(),"TUBS",(Char_t *) Astring.Data(),
			     p->par[0],p->par[1],p->par[2],p->par[3],p->par[4]);
    break;
    case CONE: t = new TCONE((Char_t *) name->Data(),"CONE",(Char_t *) Astring.Data(),
			     p->par[0],p->par[1],p->par[2],p->par[3],p->par[4]);
    break;
    case CONS: t = new TCONS((Char_t *) name->Data(),"CONS",(Char_t *) Astring.Data(),
			     p->par[0],p->par[1],p->par[2],p->par[3],p->par[4],
			     p->par[5],p->par[6]);
    break;
    case SPHE: t = new TSPHE((Char_t *) name->Data(),"SPHE",(Char_t *) Astring.Data(),
			     p->par[0],p->par[1],p->par[2],p->par[3],p->par[4],
			     p->par[5]);
    break;
    case PARA: t = new TPARA((Char_t *) name->Data(),"PARA",(Char_t *) Astring.Data(),
			     p->par[0],p->par[1],p->par[2],p->par[3],p->par[4],
			     p->par[5]);
    break;
    case PGON: t = new TPGON((Char_t *) name->Data(),"PGON",(Char_t *) Astring.Data(),
			     p->par[0],p->par[1],(int)p->par[2],(int)p->par[3]);
    break;
    case PCON: t = new TPCON((Char_t *) name->Data(),"PCON",(Char_t *) Astring.Data(),
			     p->par[0],p->par[1],(int)p->par[2]);
    break;
    case ELTU: t = new TELTU((Char_t *) name->Data(),"ELTU",(Char_t *) Astring.Data(),
			     p->par[0],p->par[1],p->par[2]);
    break;
    //      case HYPE: t = new THYPE((Char_t *) name->Data(),"HYPE",(Char_t *) Astring.Data(),
    //			       p->par[0],p->par[1],p->par[2],p->par[3]);
    //      break;
    case GTRA: t = new TGTRA((Char_t *) name->Data(),"GTRA",(Char_t *) Astring.Data(),
			     p->par[0],p->par[1],p->par[2],p->par[3],p->par[4],
			     p->par[5],p->par[6],p->par[7],p->par[8],p->par[9],
			     p->par[10],p->par[11]); 
    break;
    case CTUB: t = new TCTUB((Char_t *) name->Data(),"CTUB",(Char_t *) Astring.Data(),
			     p->par[0],p->par[1],p->par[2],p->par[3],p->par[4],
			     p->par[5],p->par[6],p->par[7],p->par[8],p->par[9],
			     p->par[10]);
    break;
    //      default:   t = new TBRIK((Char_t *) name->Data(),"BRIK",(Char_t *) Astring.Data(),
    //			       p->par[0],p->par[1],p->par[2]);
    //      break;
    
    default: assert(0);
      
    } 
    if (att->lseen  != 1) t->SetVisibility((int)att->lseen);
    if (att->lstyle != 1) t->SetLineStyle ((int)att->lstyle);
    if (att->lwidth != 1) t->SetLineWidth ((int)att->lwidth);
    if (att->lcolor != 1) t->SetLineColor ((int)att->lcolor);
    if (att->lfill  != 1) t->SetFillStyle ((int)att->lfill);
  }
  return t;
}
//_____________________________________________________________________________
void St_geant_Maker::Call(const Char_t *name)
{  
  Int_t  narg = 0;
  addrfun *address  = (addrfun *) csaddr_((Char_t *)name, strlen(name));
  if (address) csjcal_(address, &narg);
}
//_____________________________________________________________________________
void St_geant_Maker::ClearRootGeoms()
{
  // Move the becoming obsolete ROOT representations if any
  if (fVolume) {
      TDataSet *dataSet = FindByName(".data");
      fVolume->Shunt(dataSet);
      fVolume = 0;
   }
   if (fTopGeoVolume) {
      LOG_ERROR << "Fix me we !!!. Seg fault danger !!!" <<endm;
      delete fTopGeoVolume;
      fTopGeoVolume = 0;
   }
}
//_____________________________________________________________________________
TDataSet *St_geant_Maker::Work()
{  
  if (! fInitRunDone ) {
    InitRun(-1);
  }
  struct  Medium 
  { Char_t name[20]; Int_t nmat, isvol, ifield; Float_t fieldm; };
  struct  Volume
  { Char_t name[4],nick[4]; Int_t npar; Float_t par[50]; };
  char matName[24];  
  //  Int_t node = 0;
  //  TVolume   *volume=0;
  TVolume   *node=0;
  
  Float_t   *volu=0, *position=0, *mother=0, *p=0;
  Int_t     who=0, copy=0, npar=0;
  Int_t     nvol=cnum->nvolum;
  Float_t   theta1,phi1, theta2,phi2, theta3,phi3, type;
  TObjArray nodes(nvol+1);
  
  if (!gGeometry) new TGeometry("STAR","nash STAR");
  GtHash *H = new GtHash;
  
  printf(" looping on agvolume \n");
  //   ===============================================================
  //  while(agvolume(node,volu,position,mother,who,copy,p,npar)) {
    //  while(agvolume(&node,&volu,&position,&mother,&who,&copy,&p,&npar)) {
    while (Agvolume(node,volu,position,mother,who,copy,p,npar,matName)) 
    { // ===============================================================
      
      typedef enum {BOX=1,TRD1,TRD2,TRAP,TUBE,TUBS,CONE,CONS,SPHE,PARA,
		    PGON,PCON,ELTU,HYPE,GTRA=28,CTUB} shapes;
      TShape*  t;
      shapes   shape   = (shapes) volu[1];
      Int_t    nin     = 0;
      //   Int_t    medium  = (Int_t)  volu[3]; 
      Int_t    np      = (Int_t)  volu[4];
      Float_t* p0      = volu+6;
      Float_t* att     = p0+np;
      Char_t   name[]  = {0,0,0,0,0};
      Char_t   nick[]  = {0,0,0,0,0};
      float    xx[3]   = {0.,0.,0.};
      TVolume *newVolume = 0;
      if (mother)  nin = (Int_t) mother[2];
      TVolume *Hp      = 0;
      
      strncpy(nick,(const Char_t*)&cvolu->names[cvolu->nlevel-1],4);
      strncpy(name,(const Char_t*)(volu-5),4);
      
      Hp = (TVolume *) H->GetPointer(p,npar+1);
      if (Hp)  newVolume = Hp; 
      else
	{ // printf(" creating object %s  %f  %f  %f %s \n", name,p[0],p[1],p[2], );
	  switch (shape) 
	    { case BOX:  t=new TBRIK(nick,"BRIK",matName,
				     p[0],p[1],p[2]);                         break;
	    case TRD1: t=new TTRD1(nick,"TRD1",matName,
				   p[0],p[1],p[2],p[3]);                    break;
	    case TRD2: t=new TTRD2(nick,"TRD2",matName,
				   p[0],p[1],p[2],p[3],p[4]);               break;
	    case TRAP: t=new TTRAP(nick,"TRAP",matName,
				   p[0],p[1],p[2],p[3],p[4],p[5],
				   p[6],p[7],p[8],p[9],p[10]);              break;
	    case TUBE: t=new TTUBE(nick,"TUBE",matName,
				   p[0],p[1],p[2]);                         break;
	    case TUBS: t=new TTUBS(nick,"TUBS",matName,
				   p[0],p[1],p[2],p[3],p[4]);               break;
	    case CONE: t=new TCONE(nick,"CONE",matName,
				   p[0],p[1],p[2],p[3],p[4]);               break;
	    case CONS: t=new TCONS(nick,"CONS",matName,    // take care !
				   p[0],p[1],p[2],p[3],p[4],p[5],p[6]);     break;
	    //                         p[1],p[2],p[3],p[4],p[0],p[5],p[6]);     break;
	    case SPHE: t=new TSPHE(nick,"SPHE",matName,
				   p[0],p[1],p[2],p[3],p[4],p[5]);          break;
	    case PARA: t=new TPARA(nick,"PARA",matName,
				   p[0],p[1],p[2],p[3],p[4],p[5]);          break;
	    case PGON: t=new TPGON(nick,"PGON",matName,p[0],p[1],(int)p[2],(int)p[3]);  
	      { Float_t *pp = p+4;
	      for (Int_t i=0; i<p[3]; i++) {
		Float_t z    = *pp++;
		Float_t rmin = *pp++;
		Float_t rmax = *pp++;
		((TPCON *)t)->DefineSection(i,z,rmin,rmax);
		// this is because of a compiler bug on Linux (VF 030699)
		//                         (( TPGON*)t)->DefineSection(i,*pp++,*pp++,*pp++);
	      }
	      }                                              break;
	    case PCON: t=new TPCON(nick,"PCON",matName,p[0],p[1],(int)p[2]);
	      { Float_t *pp = p+3;
	      for (Int_t i=0; i<p[2]; i++) {
		Float_t z    = *pp++;
		Float_t rmin = *pp++;
		Float_t rmax = *pp++;
		((TPCON *)t)->DefineSection(i,z,rmin,rmax);
		// this is because of a compiler bug on Linux (VF 030699)
		//                         ((TPCON *)t)->DefineSection(i,*pp++,*pp++,*pp++);
	      }
	      }                                              break;
	    case ELTU: t=new TELTU(nick,"ELTU",matName,
				   p[0],p[1],p[2]);                         break;
	    //      case HYPE: t=new THYPE(nick,"HYPE",matName,
	    //                       p[0],p[1],p[2],p[3]);                    break;
	    case GTRA: t=new TGTRA(nick,"GTRA",matName,
				   p[0],p[1],p[2],p[3],p[4],p[5],
				   p[6],p[7],p[8],p[9],p[10],p[11]);        break;
	    case CTUB: t=new TCTUB(nick,"CTUB",matName,
				   p[0],p[1],p[2],p[3],p[4],p[5],
				   p[6],p[7],p[8],p[9],p[10]);              break;
	    default:   t=new TBRIK(nick,"BRIK",matName,
				   p[0],p[1],p[2]);                         break;
	    };
	  t->SetLineColor((int)att[4]);
	  
	  // to build a compressed tree, name should be checked for repetition
	  std::string nickMat = Form("%s(%s)", nick,matName);
	  newVolume = new TVolume(name,nickMat.c_str(),t);
	  //      newVolume -> SetVisibility(ENodeSEEN(MapGEANT2StNodeVis(att[1])));
	  newVolume -> SetVisibility((TVolume::ENodeSEEN)TVolume::MapGEANT2StNodeVis((int)att[1]));
	  H->SetPointer(newVolume);
	}
      
      if (node)
	{  Gfxzrm(nlev, xx[0],xx[1],xx[2], theta1,phi1, 
		  theta2,phi2, theta3,phi3, type);
	TRotMatrix *matrix=GetMatrix(theta1,phi1,theta2,phi2,theta3,phi3);
	node->Add(newVolume,xx[0],xx[1],xx[2],matrix,UInt_t(copy));
	}
      //    volume = newVolume;
      node = newVolume;
    };
  
    //  fVolume=volume;
    //  gGeometry->GetListOfNodes()->Add(volume);
    delete H;
    fVolume=node;
    gGeometry->GetListOfNodes()->Add(node);
    return GetVolume();
}
//_____________________________________________________________________________
void St_geant_Maker::Mark(TVolume *topvol) {
  Int_t JSET = clink->jset;
  if (JSET <= 0) return;
  Int_t  NSET=z_iq[JSET-1];
  Char_t Uset[5], Udet[5], Uvol[5];
  memset (Uset, 0, 5);
  memset (Udet, 0, 5);
  memset (Uvol, 0, 5);
  for (Int_t ISET=1;ISET<=NSET;ISET++) {
    Int_t JS=z_lq[JSET-ISET];
    if (JS <= 0) continue;
    Int_t NDET=z_iq[JS-1];
    memcpy (Uset, &z_iq[JSET+ISET], 4);
    for (Int_t IDET=1;IDET<=NDET;IDET++) {
      Int_t JD=z_lq[JS-IDET];
      if (JD <=0) continue;
      Int_t NV=z_iq[JD+2];
      Int_t NWHI=z_iq[JD+7];
      Int_t NWDI=z_iq[JD+8];
      memcpy (Udet, &z_iq[JS+IDET], 4);
      LOG_INFO  << "  Set " << Uset << " Detector " << Udet
	   << "  NV " << NV << " NWHI " << NWHI << " NWDI " << NWDI << endm;
      Int_t JDU = z_lq[JD-3];
      if (JDU > 0) {
	Int_t i1 = (int)z_q[JDU+3], i2 = (int)z_q[JDU+5];
	LOG_INFO  << " Volume/Bits :" << i1 << "/" << i2 <<  endm;
	for (Int_t i=i1;i<i2;i += 3) {
	  Int_t j   = JDU+i;
	  Int_t iv  = (int)z_q[j+1];
	  Int_t Nmx = (int)z_q[j+2];
	  Int_t Nam = (int)z_iq[clink->jvolum+iv];
	  Int_t Nb  = (int)z_q[j+3];
	  memcpy (Uvol, &Nam, 4);
	  LOG_INFO  << "\t" << Uvol << "\t" << Nmx << "\t" << Nb << endm;
	}
      }
      else {
	if (NV > 0) {
	  LOG_INFO  << " Volume/Bits ";
	  for (Int_t I=1; I<=NV; I++) {
	    memcpy (Uvol, &z_iq[JD+2*I+9], 4);
	    LOG_INFO  << "\t" << Uvol << "/\t" << z_iq[JD+2*I+10];
	  }
	  LOG_INFO  << endm;
	}
      }
    }
  }
#if 0
  geant3->Gfinds();
  if (csets->iset && csets->idet) {
    LOG_INFO  << "Set/Det \t" << csets->iset << "/" << csets->idet 
	 << "\tidtype = \t" << csets->idtype
	 << "\tnvname = \t" << csets->nvname << endm; 
    Int_t nLev, lNam[15], lNum[15];
    Char_t Name[4];
    geant3->Gfpath(csets->iset,csets->idet,csets->numbv, nLev, lNam, lNum);
    Int_t four = 4;
    for (Int_t i=0; i< nLev; i++) {
      uhtoc(lNam[i],four,PASSCHARD(Name),four PASSCHARL(Name));
      LOG_INFO  << "\t" << Name << "\t" << lNum[i];
    }
    LOG_INFO  << endm;
  }
#endif
}
//_____________________________________________________________________________
static Bool_t CompareMatrix(TRotMatrix &a,TRotMatrix &b)
{  double *pa=a.GetMatrix(); double *pb=b.GetMatrix();
 for (int i=0; i<9; i++)  if (pa[i]!=pb[i]) return kFALSE;
 return kTRUE;
}
//_____________________________________________________________________________
TRotMatrix *St_geant_Maker::GetMatrix(float thet1, float phii1,
                                      float thet2, float phii2,
                                      float thet3, float phii3)
{  char mname[20];
 THashList *list = gGeometry->GetListOfMatrices();
 int n=list->GetSize(); sprintf(mname,"matrix%d",n+1);
 TRotMatrix *pattern=new TRotMatrix(mname,mname,
				    thet1,phii1,thet2,phii2,thet3,phii3);
 
 TRotMatrix *matrix=0; TIter nextmatrix(list);
 while ((matrix=(TRotMatrix *) nextmatrix())) 
   { if (matrix!=pattern) 
     { if (CompareMatrix(*matrix,*pattern)) 
       { list->Remove(pattern); delete pattern; return matrix; }
     } }
 return pattern;
}
//_____________________________________________________________________________
TString St_geant_Maker::GetVolumeSrcFile(const char *volumeName) const
{
  // Find the source code defining the "volumeName" GEANT volume
  TDataSet *found = 0;
  TString vName = volumeName;
  vName.ToUpper();
  if (fVolume && volumeName && volumeName[0]) {
     const TDataSet *myVolume = fVolume->FindByName(vName.Data());
     TFileSet *geoSrc = dynamic_cast<TFileSet*>(GetDataSet("GeometryDirectory"));
     if (geoSrc && myVolume ) {
        do {
           // construct the source code pattern
           TString pattern = myVolume->GetName();
           pattern.ToLower();
           pattern += "geo.g";
           found = geoSrc->FindByName(pattern.Data());
        } while (!found && (myVolume = myVolume->GetParent()) );
     }    
     if (found) {
        // make the path up
        TString path =  found->Path();
        Ssiz_t pos = path.Index("/geometry/");
        TString topDir = geoSrc->GetTitle();
        path.Replace(0,pos,topDir);
        return path;
     }
  }
  return "";
}
//_____________________________________________________________________________
Int_t St_geant_Maker::SetInputFile(const char *file)
{
  fInputFile = file;
  return kStOK;
}
//_____________________________________________________________________________
Int_t St_geant_Maker::Skip(Int_t Nskip)
{
  if (Nskip >= 0) {
    TString kuip;
    if (IAttr("user/input")) {
      kuip = "user/skip ";
    } else {
      kuip = "trig ";
    }
    kuip += Nskip;
    LOG_INFO << "St_geant_Maker::Skip(" << Nskip << ") by " << kuip.Data() << endm;
    Do(kuip);
    
    if (cquest->iquest[0]) {return kStEOF;}
  }
  return kStOK;
}
//_____________________________________________________________________________
void type_of_call rootmaptable_(const Char_t* cdest,const Char_t* table , const Char_t* spec, 
				Int_t &k, Char_t *iq, 
				const int lCdest,const int lTable, const int lSpec)
{ 
  Char_t *Cdest = new char[(lCdest+1)]; strncpy(Cdest,cdest,lCdest); Cdest[lCdest] = 0;
  Char_t *Table = new char[(lTable+1)]; strncpy(Table,table,lTable); Table[lTable] = 0;
  Char_t *Spec  = new char[(lSpec+1)];  strncpy(Spec,spec,lSpec);    Spec[lSpec]   = 0;
  St_geant_Maker::RootMapTable(Cdest,Table,Spec, k, iq); 
  delete [] Cdest;
  delete [] Table;
  delete [] Spec;
}
//_____________________________________________________________________________
void St_geant_Maker::RootMapTable(Char_t *Cdest,Char_t *Table, Char_t* Spec, 
				  Int_t &k, Char_t *iq)
{
  TString TableName(Table); 
  TString t = TableName.Strip();
  t.ToLower();
  
  // Use St_Table::New(...)  when it is available as follows:
  St_Table *table =  St_Table::New(t.Data(),t.Data(),iq,k);
#ifndef __CINT__
#if ROOT_VERSION_CODE >= ROOT_VERSION(3,05,04)
  if (table) {fgGeom->Add(table); table->SetBit(TTable::kIsNotOwn);}
#else
  if (table) {fgGeom->Add(table); table->SetBit(kIsNotOwn);}
#endif
#endif
  if (fgGeantMk->Debug() > 1) {
    if (table) {
      Int_t N = table->GetNRows(); 
      if (N > 10) N = 10; table->Print(0,N);
    }
    else LOG_DEBUG << "St_geant_Maker::Dictionary for table :" << t.Data() 
			   << " has not been defined yet. Skip it" 
			   << endm;
  }
}
//_____________________________________________________________________________
Int_t St_geant_Maker::G2t_volume_id(const Char_t *name, Int_t *numbv){
  return g2t_volume_id(PASSCHARD(name),numbv PASSCHARL(name));
}
//_____________________________________________________________________________
Int_t St_geant_Maker::Agvolume(TVolume *&node, Float_t *&par, Float_t *&pos
                              ,Float_t *&mot,  Int_t &who,    Int_t &copy
			      ,Float_t *&par1, Int_t &npar,   char matName[24])
{

  int ans = agvolume(node,par,pos,mot,who,copy,par1,npar,(int*)matName);
  matName[20]=0; char *cc = strstr(matName," "); if (cc) *cc=0;
  return ans;

}

//_____________________________________________________________________________
void St_geant_Maker::Agnzgete (Int_t &ILK,Int_t &IDE,
			       Int_t &NPART,Int_t &IRUN,Int_t &IEVT,const Char_t *CGNAM,
			       Float_t *VERT,Int_t &IWTFL,Float_t &WEIGH){
  agnzgete (ILK,IDE,NPART,IRUN,IEVT,PASSCHARD(CGNAM),VERT,IWTFL,WEIGH
	    PASSCHARL(CGNAM));
}
//______________________________________________________________________________
void St_geant_Maker::Geometry() {
  // Move the becoming obsolete ROOT representations if any
  ClearRootGeoms();
  GetDataBase("StarDb/starsim/geometry");
  geometry();
}
//______________________________________________________________________________
Int_t St_geant_Maker::Agstroot() {
//VP not used  AgstHits();
  return agstroot();
}
//_____________________________________________________________________________
void St_geant_Maker::Gfxzrm(Int_t & Nlevel, 
			    Float_t &x, Float_t &y, Float_t &z,
			    Float_t &Theta1, Float_t & Phi1,
			    Float_t &Theta2, Float_t & Phi2,
			    Float_t &Theta3, Float_t & Phi3,
			    Float_t &Type){
  gfxzrm(Nlevel, x, y, z,
	 Theta1, Phi1, 
	 Theta2, Phi2, 
	 Theta3, Phi3, Type);
} 
//_____________________________________________________________________________
void St_geant_Maker::Dzddiv(Int_t& idiv ,Int_t &Ldummy,const Char_t* path,const Char_t* opt,
			    Int_t& one,Int_t &two,Int_t &three,Int_t& iw){
  dzddiv (idiv,Ldummy,PASSCHARD(path),PASSCHARD(opt),
	  one,two,three,iw PASSCHARL(path) PASSCHARL(opt));
}
//________________________________________________________________________________
void St_geant_Maker::SetDateTime(Int_t idat, Int_t itime) {
  //  if ( m_Mode%100 == 1 || ! fEvtHddr ) return;
  if (IAttr("KeepRunNumber") || ! fEvtHddr ) return;
  if (! m_geom_gdat) { // taken from starsim/agzio/agfinfo.age
    LOG_INFO << "St_geant_Maker:: geom_gdat table is missing. Try to get it from GEANT." << endm;
    Int_t jrung = clink->jrung;
    if (jrung > 0 && z_iq[jrung-1]>=10) {
      Int_t jrunh = z_lq[jrung-1];
      if (jrunh > 0) {
	Int_t l = z_iq[jrunh-1];
	Char_t *buf = new Char_t[4*l+1];
	memcpy (buf,  &z_iq[jrunh+1], 4*l);
	buf[4*l] = '\0';
	LOG_INFO << "St_geant_Maker::SetDateTime runh buffer: " << buf << endm;
	TString C(buf);
	delete [] buf;
	Ssiz_t begin, index;
	begin = index = 0;
	TString version;
	Float_t mfscale = 5;
	//	  TRegexp separator(";");
	while ( ( begin < C.Length()) && (index != kNPOS) ) {
	  //	    index = C.Index(separator,&end, begin);
	  index = C.Index(';',1, begin,TString::kExact);
	  if (index > begin) {
	    TString line(C(begin,index-begin));
	    line.ToLower();
	    if (Debug()) LOG_INFO  << line << endm;
	    if (line.Contains("detp")) {
	      Int_t indx = line.Index("year");
	      if (indx) {
		Int_t end = line.Index(" ",1,indx,TString::kExact);
		if (end > indx) {
		  version = TString(line(indx,end-indx));
		}
	      }
	      indx = line.Index("field");
	      if (indx) {
		Int_t eq = line.Index("=",indx+4,TString::kExact);
		sscanf(line.Data()+eq+1,"%f",&mfscale);
	      }
	    }
	  }
	  begin = index + 1;
	}
	if (version.Length()) {
	  m_geom_gdat = new St_geom_gdat("geom_gdat",1);
	  AddRunco(m_geom_gdat);
	  geom_gdat_st gdat;
	  gdat.system[0] = 0;
	  gdat.system[1] = 0;
	  gdat.mfscale   = mfscale/5.;
	  memset (&gdat.gtag[0], 0, 8);
	  strncpy(&gdat.gtag[0], version.Data(), 8);
	  m_geom_gdat->AddAt(&gdat);
	  if (Debug()) m_geom_gdat->Print(0,1);
	  if (StarMagField::Instance()) StarMagField::Instance()->SetFactor(gdat.mfscale);
	}
      } 
    }
  }
  if (m_geom_gdat) {
    geom_gdat_st *gdat = m_geom_gdat->GetTable();
    TString version(&gdat->gtag[0],8);
    version.Strip();
    version.ToLower();
    if (version != "") {
      Int_t id = St_db_Maker::AliasDate(version.Data());
      Int_t it = St_db_Maker::AliasTime(version.Data());
      if (id &&  GetDate() >= 20330101) {
	LOG_INFO << "St_geant_Maker::SetDateTime Date/Time = " 
			 << id << "/" << it << "\tas " << version << endm;
	fEvtHddr->SetDateTime(id,it);
      }
    }
  }  
}
//________________________________________________________________________________
Char_t *acfromr(Float_t r) {// 'TYPE'
  const Char_t *S=" 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz ";
  Char_t *charm = new Char_t[5];
  memset (&charm[0], 0, 5);
  Int_t k = (int) r;
  for (int i = 3; i >= 0; i--) {
    int j = 077 & k; k = k >> 6; charm[i] = S[j];
    //    LOG_INFO  << "i\t" << i << "\tj\t" << j << "\tk\t" << k << "\t" << charm[i] << endm;
  }
  //  LOG_INFO  << charm << endm;
  return charm;
}
//________________________________________________________________________________
Int_t St_geant_Maker::AgstHits() 
{
  if (! geant3) return kStErr;
  Int_t JSET = clink->jset;
  if (JSET <= 0) return kStErr;
  Int_t  NSET=z_iq[JSET-1];
  Char_t Uset[8], Udet[8], Uvol[8];
  memset (Uset, 0, 8);
  memset (Udet, 0, 8);
  memset (Uvol, 0, 8);
  TDataSet *m_Detectors = new TDataSet("Detectors"); AddConst(m_Detectors);
  for (Int_t ISET=1;ISET<=NSET;ISET++) {
    Int_t JS=z_lq[JSET-ISET];
    if (JS <= 0) continue;
    Int_t NDET=z_iq[JS-1];
    memcpy (Uset, &z_iq[JSET+ISET], 4);
    TDataSet *set = new TDataSet(Uset);
    m_Detectors->Add(set);
    for (Int_t IDET=1;IDET<=NDET;IDET++) {
      Int_t JD=z_lq[JS-IDET];
      if (JD <=0) continue;
      Int_t NV=z_iq[JD+2];
      Int_t NWHI=z_iq[JD+7];
      Int_t NWDI=z_iq[JD+8];
      memcpy (Udet, &z_iq[JS+IDET], 4);
      if (Debug()) {
	LOG_INFO  << "  Set " << Uset << " Detector " << Udet
	     << "  NV " << NV << " NWHI " << NWHI << " NWDI " << NWDI << endm;
      }
      Int_t JDU = z_lq[JD-3];
      Int_t ivd = 0;
      if (JDU > 0) {
	TDataSet *det = new TDataSet(Udet);
	set->Add(det);
	St_det_user *detu = new St_det_user("User",1); det->Add(detu);
	det_user_st rowU;
	Int_t i;
	rowU.i0      = (int) z_q[JDU+1]; 
	rowU.N       = (int) z_q[JDU+2]; 
	rowU.i1      = (int) z_q[JDU+3]; 
	rowU.Nva     = (int) z_q[JDU+4]; 
	rowU.i2      = (int) z_q[JDU+5]; 
	rowU.Nvb     = (int) z_q[JDU+6]; 
	rowU.Goption = (int) z_q[JDU+7]; 
	rowU.Serial  = (int) z_q[JDU+8]; 
	rowU.IdType  = (int) z_q[JDU+9]; 
	rowU.Iprin   = (int) z_q[JDU+10];
	detu->AddAt(&rowU);
	if (Debug()) {
	  LOG_INFO  << " displacement for hit description part    = 10                " << rowU.i0 << endm;     
	  LOG_INFO  << " Number of all hit descriptors (both in non- and cum. parts)  " << rowU.N  << endm;     
	  LOG_INFO  << " displacement for volume description part=10+10*Nh            " << rowU.i1 << endm;     
	  LOG_INFO  << " Number of all volume descriptors (branching or not)          " << rowU.Nva << endm;    
	  LOG_INFO  << " displacement for the free space   = 10+10*Nh+3*Nv            " << rowU.i2 << endm;     
	  LOG_INFO  << " number of real volume branchings for NUMBV                   " << rowU.Nvb << endm;    
	  LOG_INFO  << " Hit option: 1 - single step, 4 - Calorimetry                 " << rowU.Goption << endm;
	  LOG_INFO  << " Valid serial number for this subset                          " << rowU.Serial << endm; 
	  LOG_INFO  << " USER detector number                                         " << rowU.IdType << endm; 
	  LOG_INFO  << " current print flag both for HITS and DIGI                    " << rowU.Iprin << endm;  
	}
	St_det_path *detuV = new St_det_path("Path",rowU.Nva);
	St_det_hit  *detuH = new St_det_hit("Hit",rowU.N);
	det->Add(detuV);
	det->Add(detuH);
	det_path_st rowV;
	det_hit_st rowH;
	agfdig0(Uset,Udet,strlen(Uset),strlen(Udet));
	float    hits[15],alim[15],blim[15],bin[15];
	memset (&hits[0],0,15*sizeof(float));
	memset (&alim[0],0,15*sizeof(float));
	memset (&blim[0],0,15*sizeof(float));
	memset (&bin[0] ,0,15*sizeof(float));
	const char chit[60]="";
	agfdpar(hits[0],chit,alim[0],blim[0],bin[0],4);	
	for (i = 0; i < rowU.N; i++) {
	  memset(&rowH,0,detuH->GetRowSize());
	  Int_t j = JDU + rowU.i0 + 10*i;
	  //	  Int_t Nam   = (int) z_q[j+ 1];//     encoded hit name in Display code
	  //	  memcpy (&rowH.hit[0], &chit[4*i], 4);
	  Char_t *HitName = acfromr(z_q[j+ 1]);
	  memcpy (&rowH.hit[0], HitName, 4);
	  delete [] HitName;
	  rowH.option = (int) z_q[j+ 2];//     encoded hit option (R-rounding,S-single step)
	  rowH.Nb     = (int) z_q[j+ 3];//     number of bit requested
	  rowH.Fmin   =       z_q[j+ 4];//     hit low limit
	  rowH.Fmax   =       z_q[j+ 5];//     hit upper limit
	  rowH.Origin =       z_q[j+ 6];//     Geant hit origin (-Fmin)
	  rowH.Factor =       z_q[j+ 7];//     Geant packing factor
	  rowH.Nbit   = (int) z_q[j+ 8];//     number of bit allocated
	  rowH.Iext   = (int) z_q[j+ 9];//     address of the Geant user step routine
	  rowH.Ifun   = (int) z_q[j+10];//     hit function code (1-18 at present)
//  Case  IC of ( X  Y  Z   R    RR   PHI  THET ETA  TDR  CP    _
//                U  V  W   ETOT ELOS BIRK STEP LGAM TOF  USER  _
//                XX YY ZZ  PX   PY   PZ   SLEN PTOT LPTO rese )
	  if (Debug()) {
	    if (! i) 
	    LOG_INFO  << "\thit \toption \tNb \tFmin \tFmax \tOrigin \tFactor \tNbit \tIext \tIfun" << endm;
	    LOG_INFO  << "\t"  << setw(4) << rowH.hit 
		 << "\t"  << rowH.option
		 << "\t"  << rowH.Nb    
		 << "\t"  << rowH.Fmin  //<< "/" << alim[i]
		 << "\t"  << rowH.Fmax  //<< "/" << blim[i]
		 << "\t"  << rowH.Origin
		 << "\t"  << rowH.Factor //<< "/" << bin[i]
		 << "\t"  << rowH.Nbit  
		 << "\t"  << rowH.Iext  
		 << "\t"  << rowH.Ifun  
		 << endm;
	  }
	  detuH->AddAt(&rowH);
	}
	if (Debug()) detuH->Print(0,rowU.N);
	for (i = rowU.i1; i < rowU.i2; i += 3) {
	  memset(&rowV,0,detuV->GetRowSize());
	  Int_t j    = JDU+i;
	  Int_t iv   = (int) z_q[j+1];
	  rowV.Ncopy = (int) z_q[j+2];
	  Int_t Nam  = (int) z_iq[clink->jvolum+iv];
	  rowV.Nb    = (int) z_q[j+3];
	  memcpy (&rowV.VName[0], &Nam, 4);
	  Char_t Udvol[] = "     ";
          if (rowV.Nb > 0) {
	    Int_t Namd = (int) z_iq[JD+2*ivd+11]; ivd++;
	    memcpy (Udvol, &Namd, 4);
	  }
	  if (Debug()) {
	    LOG_INFO  << "\t" << setw(4) << rowV.VName <<  "/" << Udvol 
		 << "\t" << rowV.Ncopy << "\t" << rowV.Nb << endm;
	  }
	  detuV->AddAt(&rowV);
	}
	if (Debug()) {
	  for (; ivd<NV; ivd++) {
	    Int_t Namd = (int) z_iq[JD+2*ivd+11]; ivd++;
	    Char_t Udvol[] = "     ";
	    memcpy (Udvol, &Namd, 4);
	    LOG_INFO  << "\t" << "    " <<  "/" << Udvol << endm;
	  }
	  Int_t n = detuV->GetNRows();
	  detuV->Print(0,n);
	}
      }
    }
  }
  return kStOK;
}
#ifdef DetectorIndex
//________________________________________________________________________________
void St_geant_Maker::DetSetIndex() {
  TString vers = mInitialization;
  vers.ReplaceAll("detp geometry ","");
  LOG_INFO  << "St_geant_Maker::DetSetIndex for geometry version " << vers << endm;
  Int_t JSET = clink->jset;
  if (JSET <= 0) return;
  Int_t  NSET=z_iq[JSET-1];
  Char_t Uset[5], Udet[5], Uvol[5];
  memset (Uset, 0, 5);
  memset (Udet, 0, 5);
  memset (Uvol, 0, 5);
  for (Int_t ISET=1;ISET<=NSET;ISET++) {
    Int_t JS=z_lq[JSET-ISET];
    if (JS <= 0) continue;
    Int_t NDET=z_iq[JS-1];
    memcpy (Uset, &z_iq[JSET+ISET], 4);
    TString set(Uset);
    set.ToLower();
    for (Int_t IDET=1;IDET<=NDET;IDET++) {
      Int_t JD=z_lq[JS-IDET];
      if (JD <=0) continue;
      memcpy (Udet, &z_iq[JS+IDET], 4);
      agfdig0(Uset,Udet,strlen(Uset),strlen(Udet));
      Int_t JDU = z_lq[JD-3];
      if (JDU > 0) {
	Int_t i1      = (int) z_q[JDU+3];
	Int_t i2      = (int) z_q[JDU+5];
	Int_t Nva     = (int) z_q[JDU+4]; 
	Int_t Nvb     = (int) z_q[JDU+6]; 
	LOG_INFO  << "  Set " << Uset << " Detector " << Udet << "\tNva = " << Nva << "\tNvb = " << Nvb << endm;
	TArrayI NVmax(Nvb);
	Int_t ivv = 0;
	TString fmt("");
	for (Int_t i = i1; i < i2; i += 3) {
	  Int_t j     = JDU+i;
	  Int_t iv    = (int) z_q[j+1];
	  Int_t Ncopy = (int) z_q[j+2];
	  Int_t Nam   = (int) z_iq[clink->jvolum+iv];
	  Int_t Nb    = (int) z_q[j+3];
	  memcpy (&Uvol[0], &Nam, 4);
	  //	  LOG_INFO  <<  Uvol << " copy " << Ncopy << " bits " << Nb << endm;  
	  fmt += "/";
	  fmt += Uvol;
	  if (Nb <= 0) fmt += "_1";
	  else {NVmax[ivv] = Ncopy; ivv++; fmt += "_%d";}
	}
	TString CSYS("");
	TString Vol(Uvol);
	for (Int_t i = 0; i < NoDetectors; i++) 
	  if (TString(Detectors[i].det) == Vol && TString(Detectors[i].set) != "") {
	    CSYS = Detectors[i].Csys;
	    break;
	  }
	if (CSYS == "") {
	  TArrayI Ids0;
	  DumpIndex(Uvol, vers, fmt, NVmax, Ids0);
	  continue;
	}
        Int_t Nelem = 1;
	LOG_INFO  << "format: " << fmt << endm;
	LOG_INFO  << "NVmax";
	for (Int_t i = 0; i < Nvb; i++) {Nelem *= NVmax[i]; LOG_INFO  << "[" << NVmax[i] << "]";}
	LOG_INFO  << endm;
	Int_t numbv[15];
	memset (numbv, 0, 15*sizeof(Int_t));
	TArrayI Ids(Nelem);
	for (Int_t elem = 0; elem < Nelem; elem++) {
	  Int_t e = elem;
	  for (Int_t i = Nvb-1; i >= 0; i--) {
	    numbv[i] = e%NVmax[i] + 1;
	    e = e/NVmax[i];
	  }
	  //	  Int_t volid = G2t_volume_id(set.Data(), numbv);
	  Int_t volid = G2t_volume_id(CSYS.Data(), numbv);
	  if (volid < 0) volid = 0;
	  Ids[elem] = volid;
	}
	DumpIndex(Uvol, vers, fmt, NVmax, Ids);
      }
    }
  }
  return;
}
//________________________________________________________________________________
void St_geant_Maker::DumpIndex(const Char_t *name, const Char_t *vers, const Char_t *fmt, TArrayI &NVmax, TArrayI &Ids) {

  TString fOut(name);
  fOut += ".";
  fOut += vers;
  fOut += ".C";
  ofstream out;
  LOG_INFO  << "Create " << fOut << endm;
  out.open(fOut.Data());
  out << "TDataSet *CreateTable() {" << endl;
  out << "  if (!gROOT->GetClass(\"StarVMCDetector\")) return 0;" << endl;
  Int_t NV = NVmax.GetSize();
  Int_t Nelem = Ids.GetSize();
  if (NV > 0) {
    out << "  Int_t NVmax[" << NV << "] = {";
    for (Int_t i = 0; i < NV; i++) {
      out << NVmax[i];
      if (i < NV - 1) out << ",";
      else           out << "};";
    }
    out << endl;
    
    if (Nelem > 0) {
      out << "  Int_t Ids[" << Nelem << "] = {" << endl;
      out << "\t";
      Int_t nn = 20;
      if (Ids[0] > 0 && TMath::Log10(Ids[0]) > 7) nn = 10;
      Int_t nvl = NVmax[NV-1];
      if (nvl > 5 && nvl < nn) nn = NVmax[NV-1];
      if (nn >= nvl) nvl = Nelem;
      Int_t j = 0;
      for (Int_t i = 0; i < Nelem; i++) {
	out << Ids[i];
	j++;
	if (i < Nelem - 1) {
	  out << ",";
	  if (j % nn  == 0 || (i+1) % nvl == 0) {out << endl; out << "\t"; j = 0;}
	}
	else               out << "};";
      }
      out << endl;
    }
  }
  out << "  StarVMCDetector *Set = new StarVMCDetector(\"" << name << "\");" << endl; 
  if (NV > 0) {
    out << "  Set->SetNVmax(" << NV << ", NVmax);" << endl;
    if (Nelem > 0) out << "  Set->SetIds(" << Nelem << ", Ids);" << endl;
    else           out << "  Set->SetIds();" << endl;
  }
  out << "  Set->SetFMT(\"" << fmt << "\");" << endl;
  out << "  return (TDataSet *)Set;" << endl;
  out << "}" << endl;
  out.close(); 
}
#endif
//________________________________________________________________________________
void dstkine() {
  St_geant_Maker::instance()->KinematicsFromMuDst();
}
//________________________________________________________________________________
void St_geant_Maker::Version(ostream& os) {
  const Char_t *names[] = {
    "svtg_version",  
    "tpcg_version", "tpcg_tpadconfig",    
    "vpdg_version",  
    "btog_version", "btog_choice",     "btog_posit1(1)",   "btog_posit1(2)",   "btog_posit2",     "btog_posit3",     "btog_version",  
    "calg_version", "calg_nmodule(1)", "calg_nmodule(2)",  
    "calg_netaT",    "calg_maxmodule",  "calg_nsub",       "calg_netasmdp",    "calg_nphistr",    "calg_netfirst",    "calg_netsecon",  
    "emcg_version", "emcg_onoff",     "emcg_fillmode",  
    "ismg_layer",   "ismg_rin",        "ismg_rout",       "ismg_totallength",  "ismg_code",   
    "fmcg_version",  
    "fpdg_version",  
    "fscg_version",  
    "mtdg_version", 0};
  Int_t i = 0;
  while (names[i]) {
    Float_t version = g2t_version(names[i] PASSCHARL(names[i]));
    if (version) {
      os << "  gEnv->SetValue(\"" << names[i] << "\"," << version << ");" << endl;
    }
    i++;
  }
}
//________________________________________________________________________________
Bool_t St_geant_Maker::GeometryVersion() {
  if (! fInitRunDone) {
    LOG_ERROR << "GeometryVersion: geometry has not been initialized" << endm;
    return kFALSE;
  }
  Version(cout);
  return kTRUE;
}
//_____________________________________________________________________________
Int_t St_geant_Maker::SetDatimeFromMuDst() {
  KinematicsFromMuDst(1);
  return kStOK;
}
//_____________________________________________________________________________
Int_t St_geant_Maker::KinematicsFromMuDst(Int_t flag) {
  TTreeIter &muDstIter = *MuDstIter;
  static const Int_t*&      MuEvent_mEventInfo_mRunId                = muDstIter("MuEvent.mEventInfo.mRunId");
  static const Int_t*&      MuEvent_mEventInfo_mId                   = muDstIter("MuEvent.mEventInfo.mId");
  static const Int_t*&      MuEvent_mEventInfo_mTime                 = muDstIter("MuEvent.mEventInfo.mTime"); 
  static const Int_t*&      MuEvent_mEventSummary_mNumberOfTracks    = muDstIter("MuEvent.mEventSummary.mNumberOfTracks");
  static const Int_t&       NoPrimaryVertices                        = muDstIter("PrimaryVertices");
  static const Float_t*&    PrimaryVertices_mPosition_mX1            = muDstIter("PrimaryVertices.mPosition.mX1");
  static const Float_t*&    PrimaryVertices_mPosition_mX2            = muDstIter("PrimaryVertices.mPosition.mX2");
  static const Float_t*&    PrimaryVertices_mPosition_mX3            = muDstIter("PrimaryVertices.mPosition.mX3");
  static const Int_t&       NoPrimaryTracks                          = muDstIter("PrimaryTracks");
  static const Int_t*&      PrimaryTracks_mIndex2Global              = muDstIter("PrimaryTracks.mIndex2Global");
  static const Int_t*&      PrimaryTracks_mVertexIndex               = muDstIter("PrimaryTracks.mVertexIndex");
  static const Float_t*&    PrimaryTracks_mP_mX1                     = muDstIter("PrimaryTracks.mP.mX1");
  static const Float_t*&    PrimaryTracks_mP_mX2                     = muDstIter("PrimaryTracks.mP.mX2");
  static const Float_t*&    PrimaryTracks_mP_mX3                     = muDstIter("PrimaryTracks.mP.mX3");
  static const Short_t*&    PrimaryTracks_mHelix_mQ                  = muDstIter("PrimaryTracks.mHelix.mQ");
  static const Int_t*&      PrimaryTracks_mNSigmaElectron            = muDstIter("PrimaryTracks.mNSigmaElectron");
  static const Int_t*&      PrimaryTracks_mNSigmaPion                = muDstIter("PrimaryTracks.mNSigmaPion");
  static const Int_t*&      PrimaryTracks_mNSigmaKaon                = muDstIter("PrimaryTracks.mNSigmaKaon");
  static const Int_t*&      PrimaryTracks_mNSigmaProton              = muDstIter("PrimaryTracks.mNSigmaProton");
  static const Short_t*&    GlobalTracks_mFlag                       = muDstIter("GlobalTracks.mFlag");
  //#define __USE_GLOBAL__
#ifdef __USE_GLOBAL__
  static const Int_t&       NoGlobalTracks                           = muDstIter("GlobalTracks");
  static const Float_t*&    GlobalTracks_mP_mX1                      = muDstIter("GlobalTracks.mP.mX1");
  static const Float_t*&    GlobalTracks_mP_mX2                      = muDstIter("GlobalTracks.mP.mX2");
  static const Float_t*&    GlobalTracks_mP_mX3                      = muDstIter("GlobalTracks.mP.mX3");
  static const Short_t*&    GlobalTracks_mHelix_mQ                   = muDstIter("GlobalTracks.mHelix.mQ");
  static const Float_t*&    GlobalTracks_mFirstPoint_mX1             = muDstIter("GlobalTracks.mFirstPoint.mX1");
  static const Float_t*&    GlobalTracks_mFirstPoint_mX2             = muDstIter("GlobalTracks.mFirstPoint.mX2");
  static const Float_t*&    GlobalTracks_mFirstPoint_mX3             = muDstIter("GlobalTracks.mFirstPoint.mX3");
  static const Int_t*&      GlobalTracks_mNSigmaElectron             = muDstIter("GlobalTracks.mNSigmaElectron");
  static const Int_t*&      GlobalTracks_mNSigmaPion                 = muDstIter("GlobalTracks.mNSigmaPion");
  static const Int_t*&      GlobalTracks_mNSigmaKaon                 = muDstIter("GlobalTracks.mNSigmaKaon");
  static const Int_t*&      GlobalTracks_mNSigmaProton               = muDstIter("GlobalTracks.mNSigmaProton");
#endif
  static const Double_t __SIGMA_SCALE__ = 1000.;
  static Int_t flagS = -1;
  if (flagS != 1) {
    flagS = flag;
    do {
      if (! MuDstIter->Next()) {return kStEOF;}
      Int_t id, it;
      TUnixTime ut(MuEvent_mEventInfo_mTime[0]); ut.GetGTime(id,it);
      fEvtHddr = (StEvtHddr*)GetDataSet("EvtHddr");
      if (!fEvtHddr) {                            // Standalone run
	fEvtHddr = new StEvtHddr(m_ConstSet);
	SetOutput(fEvtHddr);	              //Declare this "EvtHddr" for output
      }
      fEvtHddr->SetDateTime(id,it);
      fEvtHddr->SetRunNumber(MuEvent_mEventInfo_mRunId[0]);
      fEvtHddr->SetEventNumber(MuEvent_mEventInfo_mId[0]);
      if (! MuEvent_mEventSummary_mNumberOfTracks[0]) continue;
      break;
    }  while (1); 
    if (flagS == 1) return kStOK;
  }
  flagS = flag;
  Float_t v[3];
  Float_t plab[3];
  Int_t nvtx;
  Int_t ipart;
  static Int_t pId[4][2] = {
    { 2, 3}, // e+, e-
    {11,12}, // K+, K-
    {14,15}, // p, pbar
    { 8, 9}  // pi+, pi-
  };
  for (Int_t l = 0; l < NoPrimaryVertices; l++) {
    v[0] = PrimaryVertices_mPosition_mX1[l];
    v[1] = PrimaryVertices_mPosition_mX2[l];
    v[2] = PrimaryVertices_mPosition_mX3[l];
    nvtx = geant3->Gsvert(v, 0, 0);
    assert(nvtx == l+1);
    for (Int_t k = 0; k < NoPrimaryTracks; k++) {
      if (l != PrimaryTracks_mVertexIndex[k]) continue;
      Int_t kg = PrimaryTracks_mIndex2Global[k];
      if (GlobalTracks_mFlag[kg] < 100) continue;
      plab[0] = PrimaryTracks_mP_mX1[k];
      plab[1] = PrimaryTracks_mP_mX2[k];
      plab[2] = PrimaryTracks_mP_mX3[k];
#ifndef __MUONS__
      Double_t nSigma[4] = {
	PrimaryTracks_mNSigmaElectron[k]/__SIGMA_SCALE__,
	PrimaryTracks_mNSigmaKaon[k]/__SIGMA_SCALE__,
	PrimaryTracks_mNSigmaProton[k]/__SIGMA_SCALE__,
	PrimaryTracks_mNSigmaPion[k]/__SIGMA_SCALE__};
      Int_t s = 0;
      if (PrimaryTracks_mHelix_mQ[k] < 0) s = 1;
      ipart = pId[3][s];
      Double_t nSigmaMin = 1e9;
      for (Int_t i = 0; i < 4; i++) {
	if (TMath::Abs(nSigma[i]) <  nSigmaMin) {
	  nSigmaMin = TMath::Abs(nSigma[i]); 
	  ipart = pId[i][s];
	}
      }
      if (nSigmaMin > 2) ipart = pId[3][s];
#else
      ipart = 5; // muon+
      if (PrimaryTracks_mHelix_mQ[k] < 0) ipart = 6; // muon-
#endif
      geant3->Gskine(plab, ipart, nvtx); 
    }
  }
#ifdef __USE_GLOBAL__
  for (Int_t kg = 0; kg < NoGlobalTracks; kg++) {
    if (GlobalTracks_mFlag[kg] < 100) continue;
    Double_t nSigmaMin = 1e9;
    Double_t nSigma[4] = {
      GlobalTracks_mNSigmaElectron[kg]/__SIGMA_SCALE__,
      GlobalTracks_mNSigmaKaon[kg]/__SIGMA_SCALE__,
      GlobalTracks_mNSigmaProton[kg]/__SIGMA_SCALE__,
      GlobalTracks_mNSigmaPion[kg]/__SIGMA_SCALE__};
    Int_t s = 0;
    if (GlobalTracks_mHelix_mQ[kg] < 0) s = 1;
    for (Int_t k = 0; k < NoPrimaryTracks; k++) {
      if (kg == PrimaryTracks_mIndex2Global[k]) {
	goto NEXTGL;
      }
    }
    v[0] = GlobalTracks_mFirstPoint_mX1[kg];
    v[1] = GlobalTracks_mFirstPoint_mX2[kg];
    v[2] = GlobalTracks_mFirstPoint_mX3[kg];
    nvtx = geant3->Gsvert(v, 0, 0);
    plab[0] = GlobalTracks_mP_mX1[kg];
    plab[1] = GlobalTracks_mP_mX2[kg];
    plab[2] = GlobalTracks_mP_mX3[kg];
    ipart = pId[3][s];
    for (Int_t i = 0; i < 4; i++) {
      if (TMath::Abs(nSigma[i]) <  nSigmaMin) {
	nSigmaMin = TMath::Abs(nSigma[i]); 
	ipart = pId[i][s];
      }
    }
    if (nSigmaMin > 2) ipart = pId[3][s];
    geant3->Gskine(plab, ipart, nvtx); 
  NEXTGL: continue;
  }
#endif
  if (Debug()) {
    Do("gprint vert");
    Do("gprint kine");
  }
  return kStOK;
}
//________________________________________________________________________________
Int_t St_geant_Maker::ipartx(Int_t id) {
  Int_t                            ipartxf = -1;
  if      (id == 1)                ipartxf = 1;  // gamma
  else if (id == 2 || id == 3)     ipartxf = 2;  // e+/-
  else if (id == 5 || id == 6)     ipartxf = 3;  // mu+/-
  else if (id == 13)               ipartxf = 4;  // neutron
#if 0
  else if ((id >=  8 && id <=  9) ||
	   (id >= 11 && id <= 15)) ipartxf = 0;  // all other charged particles
#endif
  else                             ipartxf = 0;  // all other particles
  return ipartxf;
}
//________________________________________________________________________________
Float_t St_geant_Maker::dose(Float_t Z) {
  /*                                                                      *
   *  Function    : Interpolation of gamma dose rate                      *
   *                                                                      *
   *  Arguments   : Z   Atom number                                       */
  static TGraph *graph = 0;
  if (! graph) {
    Int_t n = 43;
    Double_t x[] = {11., 12., 13., 14., 15., 17., 18., 19., 20., 22.,
		    23., 24., 25., 26., 27., 28., 29., 30., 32., 33.,
		    34., 35., 38., 40., 41., 42., 43., 46., 47., 48.,
		    49., 50., 51., 53., 57., 66., 73., 74., 78., 79.,
		    80., 82., 83.}; // Z
    Double_t y[] = {8.8033795E-02, 0.1175197    , 0.1043398    , 8.3658338E-02,
		    7.6843806E-02, 5.7563554E-02, 4.0977564E-02, 4.6153713E-02,
		    4.0977564E-02, 0.1257856    , 0.1797267    , 0.1679161    ,
		    0.1650867    , 0.1828069    , 0.1828069    , 0.1956649    ,
		    0.2094273    , 0.1923680    , 0.1923680    , 0.2612006    ,
		    0.2795725    , 0.3095814    , 0.4499212    , 0.6880791    ,
		    0.7240669    , 0.6213810    , 0.5          , 0.5          ,
		    0.5          , 0.3546627    , 0.3796084    , 0.3428115    ,
		    0.2941946    , 0.3370352    , 0.3          , 0.2702305    ,
		    0.2941946    , 0.2567994    , 0.3607410    , 0.3607410    ,
		    0.3428115    , 0.3          , 0.4}; // Dose
    graph = new TGraph(n,x,y);
  }
  return 13.1286072899874E-6*TMath::Max(0., TMath::Min(0.5, graph->Eval(Z)));
}
//________________________________________________________________________________
void St_geant_Maker::usflux() {
  Int_t        id;
  Float_t      OmegaN;
  Int_t        i;
  Float_t      ZZ, RR;
  Int_t        NstepB;
  Float_t      stepF, destepF, XYZ[3];
  Float_t      RADIUS;
  Int_t        p;
  enum {Nregions = 1, Nparts = 5, NH1T = 3, NH1TE = NH1T + 1, NH2T = 9};
  const Char_t *NameV[Nregions] = {""}; 
  static TH1F *histV1[Nregions][NH1TE][Nparts];
  //#define __3DPLOTS__
#ifdef __3DPLOTS__
  static TH3F *histV2[NH2T][Nparts];
#else
  static TH2F *histV2[NH2T][Nparts];
#endif
  static TH2F *tofg = 0;
  static Bool_t first = kTRUE;
  if (first) {
    assert(GetChain()->GetTFile());
    GetChain()->GetTFile()->cd();
    first = kFALSE;
    memset(histV1, 0, sizeof(histV1));
    memset(histV2, 0, sizeof(histV2));
    Double_t xstep =  5;
    Double_t ystep =  2;
    Double_t xmax  = 2000, xmin = - xmax;
    Double_t ymax  = 1500, ymin =      0;
    Int_t nx = (xmax - xmin)/xstep;
    Int_t ny = (ymax - ymin)/ystep;
    struct Name_t {
      const Char_t *Name;
      const Char_t *Title;
    };
    struct NameX_t {
      Name_t name;
      Int_t nX;
      Double_t xMin, xMax;
      Int_t nY;
      Double_t yMin, yMax;
    };
    tofg = new TH2F("tofg","log_{10} (tof [nsec]) @ step versus particle type",140,-1,13,51,0.5,51.5);
    Name_t Particles[Nparts] = {
      {"", "#pi/K/p and others"}, // 0
      {"g","#gamma"},             // 1
      {"e","e^{#pm}"},            // 2
      {"m","#mu^{#pm}"},          // 3
      {"n","neutron"}             // 4
    };
    NameX_t Types1[NH1TE] = {
      {{"Ekin10"    ,"Log_{10}(GEKIN) for %s for %s"                 }, 340, -14., 3.0, 0, 0, 0},  //5 -> 0 300
      {{"Ekin10s"   ,"Log_{10}(GEKIN) for %s at step for %s weighted with L"   }, 340, -14., 3.0, 0, 0, 0},  //6 -> 1 320
      {{"Ekin10V"   ,"Log_{10}(GEKIN) for %s at production Vx for %s"}, 340, -14., 3.0, 0, 0, 0},   //7 -> 2 400
      {{"Ekin10overV","Log_{10}(GEKIN) for %s at step weight with L/v for %s"}, 340, -14., 3.0, 0, 0, 0} 
    };
    NameX_t Types2[NH2T] = {
      {{"flux"      ,"flux from %s * step "                    }, nx, xmin, xmax, ny, ymin, ymax},  //0 100
      {{"flux100keV","flux from %s * step E_{kin} > 100 keV "  }, nx, xmin, xmax, ny, ymin, ymax},  //1 800
      {{"flux250meV","flux from %s * step E_{kin} < 250 meV "  }, nx, xmin, xmax, ny, ymin, ymax},  //2 500
      {{"entries"   ,"entries from %s "                        }, nx, xmin, xmax, ny, ymin, ymax},  //3 900
      {{"VxProd"    ,"Vertex Production of %s "                }, nx, xmin, xmax, ny, ymin, ymax},  //4 200
      {{"dose"      ,"dose from %s "                           }, nx, xmin, xmax, ny, ymin, ymax},  //8 ->5  600
      {{"star"      ,"star density  from %s "                  }, nx, xmin, xmax, ny, ymin, ymax},  //9 ->6 700
      {{"RD"        ,"Residual Dose  from %s "                 }, nx, xmin, xmax, ny, ymin, ymax},  //0 ->7 701
      {{"DepEnergy" ,"Deposited energy at step (keV)  from %s "}, nx, xmin, xmax, ny, ymin, ymax}
    };
    for (p = 0; p < Nparts; p++) {
      TH1::SetDefaultSumw2(kTRUE);
      for (Int_t r = 0; r < Nregions; r++) {
	for (Int_t t = 0; t < NH1TE; t++) {
	  //	  if (p != Nparts - 1 && t > NH1T) continue;
	  TString Name(Types1[t].name.Name); Name += Particles[p].Name; Name += NameV[r];
	  TString Title(Form(Types1[t].name.Title,Particles[p].Title,NameV[r]));
	  histV1[r][t][p] = 
	    new TH1F(Name,Title,Types1[t].nX,Types1[t].xMin,Types1[t].xMax);
	}
      }
      TH1::SetDefaultSumw2(kFALSE);
      for (Int_t t = 0; t < NH2T; t++) {
	if (p == 3 && t > 1) continue;
	TString Name(Types2[t].name.Name); Name += Particles[p].Name; 
	TString Title(Form(Types2[t].name.Title,Particles[p].Title));
	histV2[t][p] = 
#ifdef __3DPLOTS__
	  new TH3F(Name,Title,Types2[t].nX,Types2[t].xMin,Types2[t].xMax,Types2[t].nY,Types2[t].yMin,Types2[t].yMax,24,-180,180);
#else
	new TH2F(Name,Title,Types2[t].nX,Types2[t].xMin,Types2[t].xMax,Types2[t].nY,Types2[t].yMin,Types2[t].yMax);//24,-180,180);
#endif
      }
    }
    return;
  }
  // Fill histograms
  Int_t Ipart = ckine->ipart%100;
  p = ipartx (Ipart);
  Double_t tofg10 = - 1;
  if (ctrak->tofg > 0) tofg10 = TMath::Log10(1e9*ctrak->tofg);
  tofg->Fill(tofg10,Ipart);
  if (Ipart == 13 && ctrak->gekin <= 0.25E-9) tofg->Fill(tofg10,51);
  if (p < 0) return;
  Double_t Log10gekin = -20;
#if 0
  if (ctrak->gekin <= 0.0)       {
    ctrak->istop = 2;
    return;
  }
#else
  if (ctrak->gekin > 0) Log10gekin = TMath::Log10(ctrak->gekin);
#endif
  if (ctrak->upwght < 1) ctrak->upwght = 1;
  Int_t r = 0;
  RR = TMath::Sqrt(ctrak->vect[0]*ctrak->vect[0] + ctrak->vect[1]*ctrak->vect[1]);
  ZZ = ctrak->vect[2];
  Double_t Phi = TMath::RadToDeg()*TMath::ATan2(ctrak->vect[1],ctrak->vect[0]);
  if (Phi < -180) Phi += 360;
  if (Phi >  180) Phi -= 360;
  // calculate particle flux in sensitive volumes
  if (! (ckine->charge == 0 && p < 0)) {
    /*
     * *** step cannot be bigger then 10 cm => suppose stright line in R/Z
     */
    if (ctrak->step > 0.0)        {
      NstepB = ctrak->step + 0.5;
      NstepB = TMath::Max (1, NstepB);
      stepF  = ctrak->step/NstepB;
      destepF  = 1e6*ctrak->destep/NstepB;
      for (i = 1; i <= NstepB; i++) {
	XYZ[0] = ctrak->vect[0] + ctrak->vect[3]*stepF*(0.5 - i);
	XYZ[1] = ctrak->vect[1] + ctrak->vect[4]*stepF*(0.5 - i);
	XYZ[2] = ctrak->vect[2] + ctrak->vect[5]*stepF*(0.5 - i);
	RADIUS = TMath::Sqrt(XYZ[0]*XYZ[0] + XYZ[1]*XYZ[1]);
	Double_t phi = TMath::RadToDeg()*TMath::ATan2(XYZ[1],XYZ[0]);
	if (phi < -180) phi += 360;
	if (phi >  180) phi -= 360;
#ifdef __3DPLOTS__
	if (histV2[0][p])                             histV2[0][p]->Fill(XYZ[2], RADIUS, Phi, stepF);
	if (ctrak->gekin >= 1.E-4 && histV2[1][p])    histV2[1][p]->Fill(XYZ[2], RADIUS, Phi, stepF);
	if (ctrak->gekin <= 0.25E-9 && histV2[2][p])  histV2[2][p]->Fill(XYZ[2], RADIUS, Phi, stepF);
	if (cmate->dens > 2e-5 && ctrak->destep > 0.0 && histV2[5][p]) 
 	  histV2[5][p]->Fill(XYZ[2], RADIUS, Phi, destepF/cmate->dens);
	if (histV2[8][p])                             histV2[8][p]->Fill(XYZ[2], RADIUS, Phi, destepF);
#else
	if (histV2[0][p])                             histV2[0][p]->Fill(XYZ[2], RADIUS, stepF);
	if (ctrak->gekin >= 1.E-4 && histV2[1][p])    histV2[1][p]->Fill(XYZ[2], RADIUS, stepF);
	if (ctrak->gekin <= 0.25E-9 && histV2[2][p])  histV2[2][p]->Fill(XYZ[2], RADIUS, stepF);
	if (cmate->dens > 2e-5 && ctrak->destep > 0.0 && histV2[5][p]) 
	  histV2[5][p]->Fill(XYZ[2], RADIUS, destepF/cmate->dens);
	if (histV2[8][p])                             histV2[8][p]->Fill(XYZ[2], RADIUS, destepF);
#endif
      }
      histV1[0][0][p]->Fill(Log10gekin);
      histV1[0][1][p]->Fill(Log10gekin,ctrak->step);
      if (r > 0) {
	histV1[r][0][p]->Fill(Log10gekin);
	histV1[r][1][p]->Fill(Log10gekin,ctrak->step);
      }
      if (p == 4 && ctrak->gekin < 1e-3) { // neutrons < 1 MeV
	static Double_t EkinThermal = 25.8e-12; // meV
	static Double_t MassNeutron = 0.9396;
	static Double_t velTherm = TMath::Sqrt(2*EkinThermal/MassNeutron);
	Double_t vel = TMath::Sqrt(2*ctrak->gekin/MassNeutron);
	histV1[0][3][p]->Fill(Log10gekin,velTherm/vel*ctrak->step);
	if (r > 0) histV1[r][3][p]->Fill(Log10gekin,velTherm/vel*ctrak->step);
      }
      if (ctrak->inwvol == 1) {
#ifdef __3DPLOTS__
	if(histV2[3][p]) histV2[3][p]->Fill(ZZ, RR, Phi);
#else
	if(histV2[3][p]) histV2[3][p]->Fill(ZZ, RR);
#endif
      }
      for (Int_t i = 0; i < cking->ngkine; i++) {
	id = ((Int_t)cking->gkin[i][4])%100;
	p = ipartx (id);
	if (p >= 0) {
	  Char_t name[12];
	  Int_t itrtyp;
	  Float_t mass, charge, tlife;
	  TGiant3::Geant3()->Gfpart(id,name,itrtyp,mass,charge,tlife);
#ifdef __3DPLOTS__
	  if (histV2[4][p]) histV2[4][p]->Fill(ZZ, RR, Phi);
#else
	  if (histV2[4][p]) histV2[4][p]->Fill(ZZ, RR);
#endif
	  Double_t ekin = 
	    TMath::Sqrt(cking->gkin[i][0]*cking->gkin[i][0] +
			cking->gkin[i][1]*cking->gkin[i][1] +
			cking->gkin[i][2]*cking->gkin[i][2] + mass*mass) - mass;
	  ekin = TMath::Max (1e-14, ekin);
	  histV1[0][2][p]->Fill(TMath::Log10(ekin));
	  if (r > 0)  histV1[r][2][p]->Fill(TMath::Log10(ekin));
	}
      }
    }
    // Star density
    if (cking->ngkine > 0)        {
      if (ctrak->vect[6] > 0.300 && p <= 0) {
	for (Int_t i = 0; i < ctrak->nmec; i++) {
	  if (ctrak->lmec[i] >= 12 && ctrak->lmec[i] <= 20) {
	    OmegaN = dose(cmate->z);
#ifdef __3DPLOTS__
	    if (histV2[6][p]) histV2[6][p]->Fill(ZZ, RR, Phi); 
	    if (histV2[7][p]) histV2[7][p]->Fill(ZZ, RR, Phi, OmegaN ); 
#else
	    if (histV2[6][p]) histV2[6][p]->Fill(ZZ, RR); 
	    if (histV2[7][p]) histV2[7][p]->Fill(ZZ, RR, OmegaN ); 
#endif
	    break;
	  }
	}
      }
    }
  }
#undef __3DPLOTS__
}
