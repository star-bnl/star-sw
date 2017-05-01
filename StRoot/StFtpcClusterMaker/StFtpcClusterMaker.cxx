// $Log: StFtpcClusterMaker.cxx,v $
// Revision 1.109  2017/04/26 19:47:29  perev
// Hide m_DataSet
//
// Revision 1.108  2013/03/26 15:56:00  genevb
// Replace agufld(x,b) with direct call to StarMagField::Instance()->BField(x,b)
//
// Revision 1.107  2013/02/18 16:30:42  fisyak
// gufld => agufld
//
// Revision 1.106  2011/08/31 16:39:37  jcs
// output ampSlope contents to LOG_INFO
//
// Revision 1.105  2010/12/17 14:54:56  jcs
// For embedding, read raw data from StFtpcMixerMaker tables
//
// Revision 1.104  2010/04/08 16:46:16  jcs
// swap data for RDO6,RDO7 FTPC East when Calibrations_ftpc/ftpcElectronics->swapRDO6RDO7East=1
//
// Revision 1.103  2009/11/25 19:50:16  jcs
// remove all references to StFtpcSoftwareMonitor
//
// Revision 1.102  2009/11/18 12:10:02  jcs
// add USE_LOCAL_DRIFTMAP instructions
//
// Revision 1.101  2009/10/14 15:52:43  jcs
// write out all gas temperature, air pressure info to Run branch of FTPC debug root file
//
// Revision 1.100  2009/08/04 08:37:28  jcs
// When the flaser option is included in the bfc, the 'perfect' gain table and
// adjustAverageWest = adjustAverageEast = 0.0, will be used for cluster finding
//
// Revision 1.99  2008/10/02 16:20:43  jcs
// standardize m_Mode LOG_INFO messages
//
// Revision 1.98  2008/07/30 14:47:29  jcs
// if microsecondsPerTimebin calculated from RHIC clock, write the new value for mMicrosecondsPerTimebin back into
// Calibrations_ftpc/ftpcElectronics table
//
// Revision 1.97  2008/07/17 18:43:14  jcs
// insure that the correct microsecondsPerTimebin is used for every event, not just for the first event
//
// Revision 1.96  2008/01/07 14:46:10  jcs
// create and fill the special set of Ftpc point histograms used to evaluate
// the Ftpc gain scan runs when bfc option fgain is in the chain
//
// Revision 1.95  2008/01/02 11:29:09  jcs
// bfc option fdbg selected if m_Mode==2
//
// Revision 1.94  2007/12/13 10:37:46  jcs
// standardize looger messages
//
// Revision 1.93  2007/12/12 13:24:45  jcs
// replace asserts with LOG_ERROR message and kStErr return code
//
// Revision 1.92  2007/04/28 17:56:09  perev
// Redundant StChain.h removed
//
// Revision 1.91  2007/04/26 11:06:11  jcs
// Use the bfc option "debug" to create and fill the FTPC cluster tuning histograms
//
// Revision 1.90  2007/02/01 11:57:04  jcs
// move unessential output from INFO to DEBUG
//
// Revision 1.89  2007/01/15 07:49:22  jcs
// replace printf, cout and gMesMgr with Logger
//
// Revision 1.88  2006/11/09 13:55:34  jcs
// bfc option fdbg selected if m_Mode>=2
//
// Revision 1.87  2006/08/23 09:25:36  jcs
// Return with kStWarn if error occurs accessing Calibrations_ftpc/ftpcVoltageStatus
// StDetectorState is set only for events with FTPC data
//
// Revision 1.86  2006/08/22 11:42:21  jcs
// Set StDetectorState for Ftpc West/East depending on ftpcVoltageStatus
//
// Revision 1.85  2006/03/19 19:29:45  jcs
// Move cluster struct definitions to StFtpcClustersStructures.hh
// Create DEBUGFILE with bfc option 'fdbg'
//
// Revision 1.84  2006/03/13 19:40:32  jcs
// save microsecondsPerTimebin and temperature/pressure corrections in DEBUGFILE run tree
//
// Revision 1.83  2006/03/01 17:24:47  jcs
// move all database initialization to InitRun
//
// Revision 1.82  2006/01/13 12:35:40  jcs
// Calculate mMicrosecondsPerTimebin from RHIC clock frequency for each event
//
// Revision 1.81  2005/12/12 13:42:32  jcs
// if StFtpcDbRead not constructed exit with kStWarn
//
// Revision 1.80  2005/10/26 14:01:09  jcs
// Set gas temperature to default values when running fss so that database
// values are printed out only once
//
// Revision 1.79  2005/10/14 07:29:01  jcs
// Calculate microsecondsPerTimebin from RHIC ClockFrequency
// If RHIC ClockFrequency = 0, use default value from database
//
// Revision 1.78  2005/03/23 14:32:28  jcs
// additional changes for using body + extra temperatures starting with y2005
//
// Revision 1.77  2005/03/14 22:56:12  jcs
// use the body + the extra ftpc temperature reading starting with y2005
//
// Revision 1.76  2004/11/16 11:35:34  jcs
// label the x-axis for the charge step plots
//
// Revision 1.75  2004/09/27 12:54:28  jcs
// pad vs. time histograms moved to St_QA_Maker
// set radial step histogram line color; red = FTPC East, blue=FTPC West
//
// Revision 1.74  2004/09/08 16:03:26  jcs
// correct binning error in fcl_flags histogram
//
// Revision 1.73  2004/09/07 14:09:07  jcs
// use the IAttr(".histos") to control histogrammingZ
//
// Revision 1.72  2004/09/03 20:35:03  perev
// Big LeakOff + mem optimisation
//
// Revision 1.71  2004/07/18 14:15:08  jcs
// include runNumber in call to StFtpcGasUtilities
//
// Revision 1.70  2004/06/18 12:06:18  jcs
// replace #ifdef...#elif...#endif conditional compiler directives with #ifdef...#endif #ifdef...#endif
//
// Revision 1.69  2004/06/18 09:04:40  jcs
// replace obsolete DEBUGFILE code with code to write out a root file for cluster/laser analysis
//
// Revision 1.68  2004/05/25 07:10:04  jcs
// initialize StFtpcSoftwareMonitor*ftpcMon
//
// Revision 1.67  2004/05/24 13:35:32  jcs
// create a new StFtpcSoftwareMonitor if none exists
//
// Revision 1.66  2004/05/19 17:44:46  oldi
// For simulated data the ftpcHitCollection inside StEvent doesn't exist yet.
// We have to create it on our own. Additional check for ftpcHitCollection
// introduced in StFtpcTrackMaker.
//
// Revision 1.65  2004/04/19 22:00:46  oldi
// Minor changes.
//
// Revision 1.64  2004/04/06 18:36:51  oldi
// New data mebers for pad and time position and pad and time sigma filled.
//
// Revision 1.63  2004/02/12 19:38:46  oldi
// Removal of intermediate tables.
//
// Revision 1.62  2004/01/28 02:04:43  jcs
// replace all instances of StFtpcReducedPoint and StFtpcPoint with StFtpcConfMapPoint
//
// Revision 1.61  2004/01/28 01:41:15  jeromel
// Change OST to OS everywhere since defaultoption is now not to print
// the date.
//
// Revision 1.60  2003/11/13 14:12:17  jcs
// move pressure and gas corrections from StFtpcClusterMaker.cxx to StFtpcGasUtilities
//
// Revision 1.59  2003/10/11 08:52:46  jcs
// protect against divide by zero while calculating average body temperatures
//
// Revision 1.58  2003/10/11 04:07:48  perev
// assert for number of east temperatures added
//
// Revision 1.57  2003/09/02 17:58:14  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.56  2003/08/21 14:27:24  jcs
// remove temporary fix to prevent segmentation violation which occurred when  more than one run per job
//
// Revision 1.55  2003/07/18 18:31:47  perev
// test for nonexistance of XXXReader added
//
// Revision 1.54  2003/07/15 09:35:41  jcs
// do not re-flavor FTPC drift maps if already flavored to avoid creating
// memory leak.
// meomory leak will occur if flavor (i.e. magnetic field) changes within one
// *.fz file (only possible for MC data)
//
// Revision 1.53  2003/07/04 00:35:53  perev
// Defence against luch of DB info added
//
// Revision 1.52  2003/06/13 10:57:28  jcs
// remove debug statement
//
// Revision 1.51  2003/06/12 10:01:25  jcs
// renamed ftpcClusterGeometry database table to ftpcClusterGeom
// (name was too long)
//
// Revision 1.50  2003/06/11 12:06:03  jcs
// get inner cathode and cluster geometry parameters from database
//
// Revision 1.49  2003/06/10 13:10:05  jcs
// get min,max gas temperature and pressure limits from database instead of from
// parameters
//
// Revision 1.48  2003/05/07 15:08:56  putschke
// improvements for cathode offset corretions
//
// Revision 1.47  2003/04/15 11:34:41  putschke
// Include corrections for inner cathode offset and move some parameter to database
//
// Revision 1.46  2003/02/27 22:56:58  jcs
// use the ftpc body temperature readings to make temperature/pressure corrections
//
// Revision 1.44  2003/02/25 04:45:22  jcs
// comment out body4East temperature for AuAu central production
//
// Revision 1.43  2003/02/19 14:52:44  jcs
// calculate ftpc temperature from body temperatures
//
// Revision 1.42  2003/02/08 03:45:22  jcs
// for dAu production ONLY: hardcode gas temperatures
//
// Revision 1.41  2003/01/14 12:58:01  jcs
// use Geometry_ftpc/ftpcAsicMap to control corrections for error in Y2001-2002
// FTPC asic mapping
//
// Revision 1.40  2002/08/02 11:24:29  oldi
// Used database values are printed to screen, now.
//
// Revision 1.39  2002/07/15 13:31:09  jcs
// incorporate charge step histos into cluster finder and remove StFtpcChargeStep
//
// Revision 1.38  2002/03/22 08:52:52  jcs
// correct memory leaks found by Insure
//
// Revision 1.37  2002/03/20 11:44:35  jcs
// forgot to uncomment delete step when reactivating StFtpcChargeStep - caused
// memory leak
//
// Revision 1.36  2002/03/14 10:53:42  jcs
// turn ftpc timebin charge step histograms back on
//
// Revision 1.35  2002/03/01 14:22:20  jcs
// add additional histograms to monitor cluster finding
//
// Revision 1.34  2002/02/10 21:15:38  jcs
// create separate radial chargestep histograms for Ftpc west and east
//
// Revision 1.33  2002/01/22 02:53:09  jcs
// remove extra test from if statement
//
// Revision 1.32  2002/01/21 22:10:38  jcs
// calculate FTPC gas temperature adjusted air pressure using online db
//
// Revision 1.31  2001/12/12 16:05:28  jcs
// Move GetDataBase.. statements from Init to Make to ensure selection of
// correct flavor (Thank you Jeff)
//
// Revision 1.30  2001/11/21 12:44:44  jcs
// make ftpcGas database table available to FTPC cluster maker
//
// Revision 1.29  2001/10/29 12:53:23  jcs
// select FTPC drift maps according to flavor of magnetic field
//
// Revision 1.28  2001/10/19 09:41:22  jcs
// tZero now in data base in ftpcElectronics
//
// Revision 1.27  2001/10/12 14:33:08  jcs
// create and fill charge step histograms for FTPC East and West
//
// Revision 1.26  2001/07/26 13:53:19  oldi
// Check if FTPC data is available.
//
// Revision 1.25  2001/07/12 10:35:14  jcs
// create and fill FTPC cluster radial position histogram
//
// Revision 1.24  2001/06/16 12:59:47  jcs
// delete ftpcReader only if it is the FTPC slow simulator reader
//
// Revision 1.23  2001/06/13 14:37:54  jcs
// change StDaqReader to StDAQReader
//
// Revision 1.22  2001/04/23 19:57:51  oldi
// The chargestep histogram is only filled but the evaluated value of the
// normalized pressure is not used for a correction. This was done due to the
// fact that StFtpcChargeStep is not stable enough to determine the actual
// charge step.
// Output is sent to StMessMgr now.
//
// Revision 1.21  2001/04/04 17:08:42  jcs
// remove references to StFtpcParamReader from StFtpcDbReader
//
// Revision 1.20  2001/04/02 12:10:18  jcs
// get FTPC calibrations,geometry from MySQL database and code parameters
// from StarDb/ftpc
//
// Revision 1.19  2001/03/19 15:52:47  jcs
// use ftpcDimensions from database
//
// Revision 1.18  2001/03/06 23:33:51  jcs
// use database instead of params
//
// Revision 1.17  2001/01/25 15:25:35  oldi
// Fix of several bugs which caused memory leaks:
//  - Some arrays were not allocated and/or deleted properly.
//  - TClonesArray seems to have a problem (it could be that I used it in a
//    wrong way in StFtpcTrackMaker form where Holm cut and pasted it).
//    I changed all occurences to TObjArray which makes the program slightly
//    slower but much more save (in terms of memory usage).
//
// Revision 1.16  2000/11/24 15:02:33  hummler
// commit changes omitted in last commit
//
// Revision 1.14  2000/11/20 11:39:12  jcs
// remove remaining traces of fspar table
//
// Revision 1.13  2000/11/14 13:08:16  hummler
// add charge step calculation, minor cleanup
//
// Revision 1.12  2000/09/18 14:26:46  hummler
// expand StFtpcParamReader to supply data for slow simulator as well
// introduce StFtpcGeantReader to separate g2t tables from simulator code
// implement StFtpcGeantReader in StFtpcFastSimu
//
// Revision 1.11  2000/08/03 14:39:00  hummler
// Create param reader to keep parameter tables away from cluster finder and
// fast simulator. StFtpcClusterFinder now knows nothing about tables anymore!
//
// Revision 1.8  2000/04/13 18:08:21  fine
// Adjusted for ROOT 2.24
//
// Revision 1.7  2000/02/24 15:18:42  jcs
// inactivate histograms for MDC3
//
// Revision 1.6  2000/02/04 13:49:36  hummler
// upgrade ffs:
// -remove unused fspar table
// -make hit smearing gaussian with decent parameters and static rand engine
// -separate hit smearing from cluster width calculation
//
// Revision 1.5  2000/02/02 15:20:33  hummler
// correct acceptance at sector boundaries,
// take values from fcl_det
//
// Revision 1.4  2000/01/27 09:47:18  hummler
// implement raw data reader, remove type ambiguities that bothered kcc
//
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StFtpcClusterMaker class for Makers                                  //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include <Stiostream.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/fcntl.h>
//VP#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDAQMaker/StFTPCReader.h"

#include "StMessMgr.h"
#include "StFtpcClusterMaker.h"
#include "StFtpcParamReader.hh"
#include "StFtpcDbReader.hh"
#include "StFtpcGeantReader.hh"
#include "StFtpcClusterFinder.hh"
#include "StFtpcGasUtilities.hh"
#include "StFtpcTrackMaker/StFtpcConfMapPoint.hh"
#include "StFtpcGeantPoint.hh"
#include "StFtpcFastSimu.hh"
#include "St_DataSetIter.h"
#include "TH1.h"
#include "TH2.h"
#include "TObjArray.h"
#include "TObjectSet.h"
#include "PhysicalConstants.h"

#include "tables/St_fcl_ftpcsqndx_Table.h"
#include "tables/St_fcl_ftpcadc_Table.h"

#include "tables/St_g2t_vertex_Table.h"
#include "tables/St_g2t_track_Table.h"
#include "tables/St_g2t_ftp_hit_Table.h"
#include "tables/St_ffs_gepoint_Table.h"

#include "StDetectorDbMaker/StDetectorDbClock.h"
#include "StDetectorDbMaker/StDetectorDbFTPCGas.h"
#include "StDetectorDbMaker/StDetectorDbFTPCVoltageStatus.h"

#include "St_db_Maker/St_db_Maker.h"

#include "StEvent.h"
#include "StFtpcHitCollection.h"
#include "StDetectorState.h"

ClassImp(StFtpcClusterMaker)

  //_____________________________________________________________________________
StFtpcClusterMaker::StFtpcClusterMaker(const char *name):
StMaker(name),
    m_clusterpars(0),
    m_fastsimgas(0),
    m_fastsimpars(0),
    m_dimensions(0),
    m_padrow_z(0),
    m_asicmap(0),
    m_efield(0),
    m_vdrift(0),
    m_deflection(0),
    m_dvdriftdp(0),
    m_ddeflectiondp(0),
    m_ampslope(0),
    m_ampoffset(0),
    m_timeoffset(0),
    m_driftfield(0),
    m_gas(0),
    m_electronics(0),
    m_cathode(0),
    m_clustergeo(0),
    m_temps(0)
{
  mCurrentEvent=0;   //!
  mFtpcHitColl=0;    //!
  mHitArray=0;       //!
  memset(m_ThBeg,0,m_ThEnd-m_ThBeg+1);
  drawinit=kFALSE;
}
//_____________________________________________________________________________
StFtpcClusterMaker::~StFtpcClusterMaker(){
}
//_____________________________________________________________________________
Int_t StFtpcClusterMaker::InitRun(int runnumber){
  Float_t x[3] = {0,0,0};
  Float_t b[3];
  StarMagField::Instance()->BField(x,b);
  Double_t gFactor = b[2]/4.980;

  mDbMaker     = (St_db_Maker*)GetMaker("db");
  Int_t dbDate = mDbMaker->GetDateTime().GetDate();
  Int_t dbTime = mDbMaker->GetDateTime().GetTime();
  LOG_INFO<<"dbDate = "<<dbDate<<" dbTime = "<<dbTime<<" Run Number = "<<GetRunNumber()<<" gFactor = "<<gFactor<<endm;
  
  // Load the correct FTPC drift maps depending on magnetic field

  // Full Field Positive ?
  if ( gFactor > 0.8 ) {
     SetFlavor("ffp10kv","ftpcVDrift");
     SetFlavor("ffp10kv","ftpcdVDriftdP");
     SetFlavor("ffp10kv","ftpcDeflection");
     SetFlavor("ffp10kv","ftpcdDeflectiondP");
     LOG_DEBUG << "Ftpc drift map flavor set to ffp10kv"<<endm;
  }
  else if ( gFactor > 0.2 ) {
     SetFlavor("hfp10kv","ftpcVDrift");
     SetFlavor("hfp10kv","ftpcdVDriftdP");
     SetFlavor("hfp10kv","ftpcDeflection");
     SetFlavor("hfp10kv","ftpcdDeflectiondP");
     LOG_DEBUG << "Ftpc drift map flavor set to hfp10kv"<<endm;
  }
  else if ( gFactor > -0.2 ) {
     SetFlavor("zf10kv","ftpcVDrift");
     SetFlavor("zf10kv","ftpcdVDriftdP");
     SetFlavor("zf10kv","ftpcDeflection");
     SetFlavor("zf10kv","ftpcdDeflectiondP");
     LOG_DEBUG << "Ftpc drift map flavor set to zf10kv"<<endm;
  }
  else if ( gFactor > -0.8 ) {
     SetFlavor("hfn10kv","ftpcVDrift");
     SetFlavor("hfn10kv","ftpcdVDriftdP");
     SetFlavor("hfn10kv","ftpcDeflection");
     SetFlavor("hfn10kv","ftpcdDeflectiondP");
     LOG_DEBUG << "Ftpc drift map flavor set to hfn10kv"<<endm;
  }
  else {
     SetFlavor("ffn10kv","ftpcVDrift");
     SetFlavor("ffn10kv","ftpcdVDriftdP");
     SetFlavor("ffn10kv","ftpcDeflection");
     SetFlavor("ffn10kv","ftpcdDeflectiondP");
     LOG_DEBUG << "Ftpc drift map flavor set to ffn10kv"<<endm;
  }     

  // calculate microsecondsPerTimebin from RHIC clock frequency for current run 
  // if not available, use default values from offline database

  StDetectorDbClock* dbclock = StDetectorDbClock::instance();
  double freq = dbclock->getCurrentFrequency()/1000000.0;
  if ( freq != 0) 
      microsecondsPerTimebin = 1./(freq/2.);
  else
      microsecondsPerTimebin = 0.;

  // Geometry/ftpc offline database tables

  St_DataSet *ftpc_geometry_db = GetDataBase("Geometry/ftpc");
  if ( !ftpc_geometry_db ){
     LOG_ERROR << "InitRun  - error Getting FTPC offline database Geometry/ftpc"<<endm;
     return kStErr;
  }

  St_DataSetIter       dblocal_geometry(ftpc_geometry_db);

  m_dimensions = (St_ftpcDimensions *)dblocal_geometry("ftpcDimensions");
  m_padrow_z   = (St_ftpcPadrowZ *)dblocal_geometry("ftpcPadrowZ");
  m_asicmap    = (St_ftpcAsicMap *)dblocal_geometry("ftpcAsicMap");
  m_clustergeo = (St_ftpcClusterGeom *)dblocal_geometry("ftpcClusterGeom");
  m_cathode      = (St_ftpcInnerCathode *)dblocal_geometry("ftpcInnerCathode");
 
  // Calibrations/ftpc offline database tables
  
  St_DataSet *ftpc_calibrations_db = GetDataBase("Calibrations/ftpc");
  if ( !ftpc_calibrations_db ){
     LOG_ERROR <<"InitRun - error getting FTPC offline database Calibrations/ftpc"<<endm;
     return kStErr;
  }

  St_DataSetIter       dblocal_calibrations(ftpc_calibrations_db);

  m_efield     = (St_ftpcEField *)dblocal_calibrations("ftpcEField" );

  // USE_LOCAL_DRIFTMAP:
  //                    To use the FTPC drift map tables in $PWD/StarDb instead of those
  //                    in the MySQL offline database, comment out the following 4 lines of code
  //                    Then go to USE_LOCAL_DRIFTMAP: in Int_t StFtpcClusterMaker::Init()
  //                    and follow the instructions
  m_vdrift     = (St_ftpcVDrift *)dblocal_calibrations("ftpcVDrift" );
  m_deflection = (St_ftpcDeflection *)dblocal_calibrations("ftpcDeflection" );
  m_dvdriftdp     = (St_ftpcdVDriftdP *)dblocal_calibrations("ftpcdVDriftdP" );
  m_ddeflectiondp = (St_ftpcdDeflectiondP *)dblocal_calibrations("ftpcdDeflectiondP" );

  m_ampslope = (St_ftpcAmpSlope *)dblocal_calibrations("ftpcAmpSlope" );
  m_ampoffset = (St_ftpcAmpOffset *)dblocal_calibrations("ftpcAmpOffset");
  m_timeoffset = (St_ftpcTimeOffset *)dblocal_calibrations("ftpcTimeOffset");
  m_driftfield = (St_ftpcDriftField *)dblocal_calibrations("ftpcDriftField");
  m_gas        = (St_ftpcGas *)dblocal_calibrations("ftpcGas");
  m_electronics = (St_ftpcElectronics *)dblocal_calibrations("ftpcElectronics");
  m_temps = (St_ftpcTemps *)dblocal_calibrations("ftpcTemps");

  return kStOK;
}
//_____________________________________________________________________________
Int_t StFtpcClusterMaker::Init(){

  // m_Mode is used to pass bfc option information to the Maker
  //        m_Mode            bfc option         Comments
  //          0                                  normal setting for production runs
  //          2               fdbg               open special Ftpc root file and fill with cluster information 
  //                                             for analysis in StFtpcCalibMaker
  //          3               fdbg + flaser      open special Ftpc root file for laser run and fill with cluster information 
  //                                             for analysis in StFtpcCalibMaker
  //          4               fgain              initialize and fill the special set of Ftpc cluster histograms
  //                                             used to evaluate the Ftpc gain scan runs

  LOG_INFO << "StFtpcClusterMaker entered with m_Mode = "<< m_Mode <<endm;
  
  if (m_Mode == 2) {
    LOG_INFO << "StFtpcClusterMaker running with fdbg option selected"<<endm;
  }
  
  if (m_Mode == 3) {
    LOG_INFO << "StFtpcClusterMaker writing to DEBUGFILE (fdbg option selected) for laser run (flaser option selected)"<<endm;
    laserRun = kTRUE;
  }
  else {laserRun = kFALSE;}

  if (m_Mode == 4) {
    LOG_INFO << "StFtpcClusterMaker running with fgain option selected"<<endm;
  }

  St_DataSet *ftpc = GetDataBase("ftpc");
  if (!ftpc) {
     LOG_ERROR << "StFtpcClusterMaker exiting - run parameter database StarDb/ftpc not found"<<endm;
     return kStErr;
  }
  St_DataSetIter       local(ftpc);

  m_clusterpars  = (St_ftpcClusterPars *)local("ftpcClusterPars");
  m_fastsimgas   = (St_ftpcFastSimGas  *)local("ftpcFastSimGas");
  m_fastsimpars  = (St_ftpcFastSimPars *)local("ftpcFastSimPars");


  // USE_LOCAL_DRIFTMAP:
  //                    To use the FTPC drift map tables in $PWD/StarDb instead of those
  //                    in the MySQL offline database, uncomment the following 4 lines of code
  //m_vdrift     = (St_ftpcVDrift *)local("ftpcVDrift" );
  //m_deflection = (St_ftpcDeflection *)local("ftpcDeflection" );
  //m_dvdriftdp     = (St_ftpcdVDriftdP *)local("ftpcdVDriftdP" );
  //m_ddeflectiondp = (St_ftpcdDeflectiondP *)local("ftpcdDeflectiondP" );

  // 		Create Histograms
  m_chargestep_West = new TH1F("fcl_chargestepW","FTPC West chargestep",260, -0.5, 259.5);
  m_chargestep_West->SetXTitle("timebin");
  m_chargestep_East = new TH1F("fcl_chargestepE","FTPC East chargestep",260, -0.5, 259.5);
  m_chargestep_East->SetXTitle("timebin");
  m_cluster_radial_West = new TH1F("fcl_radialW","FTPCW cluster radial position",700,0.,35.);
  m_cluster_radial_West->SetLineColor(kBlue);
  m_cluster_radial_East = new TH1F("fcl_radialE","FTPCE cluster radial position",700,0.,35.);
  m_cluster_radial_East->SetLineColor(kRed);

  m_csteps     = NULL;
  m_hitsvspad  = NULL;
  m_hitsvstime = NULL;

  if (IAttr(".histos")) {
     if (m_Mode == 4) {
        m_pnt_xyFW    = new TH2F("PointXYFtpcW","point: x-y distribution of hits, ftpcW",70,-35,35,70,-35,35);
        m_pnt_xyFE    = new TH2F("PointXYFtpcE","point: x-y distribution of hits, ftpcE",70,-35,35,70,-35,35);
        m_pnt_planeF  = new TH1F("PointPlaneF","point: plane distribution of hits, ftpc",20,0.5,20.5);
        m_pnt_padtimeFW    = new TH2F("PointPadTimeFtpcW","point: #pads vs #timebins of hits, ftpcW",12,0.5,12.5,10,0.5,10.5);
        m_pnt_padtimeFW->SetXTitle("#timebins");
        m_pnt_padtimeFW->SetYTitle("#pads");
        m_pnt_padtimeFE    = new TH2F("PointPadTimeFtpcE","point: #pads vs #timebins of hits, ftpcE",12,0.5,12.5,10,0.5,10.5);
        m_pnt_padtimeFE->SetXTitle("#timebins");
        m_pnt_padtimeFE->SetYTitle("#pads");
     }
     m_maxadc_West = new TH1F("fcl_maxadcW","FTPCW MaxAdc",50,0.5,50.5);
     m_maxadc_East = new TH1F("fcl_maxadcE","FTPCE MaxAdc",50,0.5,50.5);
     m_charge_West = new TH1F("fcl_chargeW","FTPCW charge",50,0.5,500.5);
     m_charge_East = new TH1F("fcl_chargeE","FTPCE charge",50,0.5,500.5);
     if (Debug()){
        m_flags      = new TH1F("fcl_flags"	,"FTPC cluster finder flags"	,8,0.,8.);
        m_row        = new TH1F("fcl_row"	,"FTPC rows"			,20,1.,21.);
        m_sector     = new TH1F("fcl_sector"	,"FTPC sectors"			,6,1.,7.);
        m_row_sector = new TH2F("fcl_row_sector","FTPC(fcl) row vs. sector"	,20,1.,21.,6,1.,7.);
        //m_pads       = new TH1F("fcl_pads"	,"FTPC pads"			,80,1.,161.);
        //m_timebins   = new TH1F("fcl_timebins","FTPC timebins"		,100,1.,257.);
        //m_npad_nbin  = new TH2F("fcl_pad_bin"	,"FTPC(fcl) pad vs. timebin"	,80,1.,161.,100,1.,257.);
        //m_csteps      = new TH2F("fcl_csteps"	,"FTPC charge steps by sector"	,60,-0.5,59.5, 260, -0.5, 259.5);
        m_hitsvspad = new TH2F("fcl_hitsvspad","#hits vs. padlength",10,0.5,10.5,11,0.5,11.5);
        m_hitsvstime = new TH2F("fcl_hitsvstime","#hits vs. timelength",12,0.5,12.5,11,0.5,11.5);
     }
  }   

  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StFtpcClusterMaker::Make()
{

  int using_FTPC_slow_simulator = 0;

  mCurrentEvent = (StEvent*) GetInputDS("StEvent");
  if (mCurrentEvent) {
    if (!(mFtpcHitColl = mCurrentEvent->ftpcHitCollection())) {
      mFtpcHitColl = new StFtpcHitCollection;
      mCurrentEvent->setFtpcHitCollection(mFtpcHitColl);
    }
  } else mFtpcHitColl = 0;

  // create parameter reader
  StFtpcParamReader paramReader(m_clusterpars,m_fastsimgas,m_fastsimpars);

  if ( paramReader.returnCode != kStOK ) {
     LOG_ERROR << "Exiting - error constructing StFtpcParamReader (paramReader.returnCode = "<<paramReader.returnCode<<")"<<endm;
     return kStERR;
  }
  
  // create FTPC data base reader
  StFtpcDbReader dbReader(m_dimensions,
                          m_padrow_z,
			  m_asicmap,
                          m_efield,
                          m_vdrift,
                          m_deflection,
                          m_dvdriftdp,
                          m_ddeflectiondp,
                          m_ampslope,
                          m_ampoffset,
                          m_timeoffset,
                          m_driftfield,
                          m_gas,
                          m_electronics,
			  m_cathode,
			  m_clustergeo);	

  if ( dbReader.returnCode != kStOK ) {
     LOG_ERROR << "Exiting - error constructing StFtpcDbReader (dbReader.returnCode = "<<dbReader.returnCode<<")"<<endm;
     return kStERR;
  }

  if ( paramReader.gasTemperatureWest() == 0 && paramReader.gasTemperatureEast() == 0) {
     dbReader.setLaserRun(laserRun);
     LOG_INFO<<"Using the following values from database:"<<endm;
     if (microsecondsPerTimebin > 0.0 ) {
        dbReader.setMicrosecondsPerTimebin(microsecondsPerTimebin);
        LOG_INFO<<"          microsecondsPerTimebin    = "<<dbReader.microsecondsPerTimebin()<<" (calculated from RHIC Clock Frequency)"<<endm;
     } else {
        LOG_INFO<<"          microsecondsPerTimebin    = "<<dbReader.microsecondsPerTimebin()<<" (default value from database)"<<endm;
     }
     LOG_INFO<<"          SwapRDO6RDO7East          = "<<dbReader.SwapRDO6RDO7East()<<endm;
     LOG_INFO<<"          EastIsInverted            = "<<dbReader.EastIsInverted()<<endm;
     LOG_INFO<<"          Asic2EastNotInverted      = "<<dbReader.Asic2EastNotInverted()<<endm;
     LOG_INFO<<"          tzero                     = "<<dbReader.tZero()<<endm;
     LOG_INFO<<"          temperatureDifference     = "<<dbReader.temperatureDifference()<<endm;
     LOG_INFO<<"          defaultTemperatureWest    = "<<dbReader.defaultTemperatureWest()<<endm;
     LOG_INFO<<"          defaultTemperatureEast    = "<<dbReader.defaultTemperatureEast()<<endm;
     LOG_INFO<<"          adjustAverageWest         = "<<dbReader.adjustAverageWest()<<endm;
     LOG_INFO<<"          adjustAverageEast         = "<<dbReader.adjustAverageEast()<<endm;
     LOG_INFO<<"          magboltzVDrift(0,0)       = "<<dbReader.magboltzVDrift(0,0)<<endm;
     LOG_INFO<<"          magboltzDeflection(0,0)   = "<<dbReader.magboltzDeflection(0,0)<<endm;
     LOG_INFO<<"          offsetCathodeWest         = "<<dbReader.offsetCathodeWest()<<endm;
     LOG_INFO<<"          offsetCathodeEast         = "<<dbReader.offsetCathodeEast()<<endm;
     LOG_INFO<<"          angleOffsetWest           = "<<dbReader.angleOffsetWest()<<endm;
     LOG_INFO<<"          angleOffsetEast           = "<<dbReader.angleOffsetEast()<<endm;
     LOG_INFO<<"          minChargeWindow           = "<<dbReader.minChargeWindow()<<endm;
     LOG_INFO << "        using gain table: amplitudeSlope(1,0) = "<<dbReader.amplitudeSlope(1,0)<<",  amplitudeSlope(1,1) = "<<dbReader.amplitudeSlope(1,1)<< endm;
  }
     LOG_DEBUG<<" Using microsecondsPerTimebin = "<<dbReader.microsecondsPerTimebin()<<" for this event"<<endm;

  // Test if input data is real data (DAQ)

  St_DataSet *daqDataset;
  StDAQReader *daqReader;
  StFTPCReader *ftpcReader=NULL;
  daqDataset=GetDataSet("StDAQReader");
  if(daqDataset)
    {
      LOG_DEBUG << "Using StDAQReader to get StFTPCReader" << endm;
      if (!daqDataset) {
         LOG_ERROR << "Exiting - daqDataset not found" << endm;
         return kStErr;
      }
      daqReader=(StDAQReader *)(daqDataset->GetObject());
      if (!daqReader) {
         LOG_ERROR << "Exiting - daqReader not found" << endm;
         return kStErr;
      }
      ftpcReader=daqReader->getFTPCReader();

      if (!ftpcReader || !ftpcReader->checkForData()) {
	LOG_WARN << "No FTPC data available!" << endm;
	return kStWarn;
      }

      // test if pressure and gas temperature available from offline DB
       
      StDetectorDbFTPCGas *gas = StDetectorDbFTPCGas::instance();
      if ( !gas ){
          LOG_ERROR << "Error getting FTPC Offline database: Calibrations_ftpc/ftpcGasOut"<<endm;
          return kStErr;
      }	      

      Int_t  returnCode;

      // use available pressure and gas temperature from offline DB to adjust 
      // the barometric pressure depending on the FTPC gas temperature
         
      StFtpcGasUtilities gasUtils(&paramReader,
		                  &dbReader,
				  gas,
                                  m_temps);

      returnCode = gasUtils.barometricPressure();

      // Calculate FTPC gas temperature from body temperatures

      //mDbMaker     = (St_db_Maker*)GetMaker("db");
      Int_t dbDate = mDbMaker->GetDateTime().GetDate();

      // For FTPC West
      
      returnCode = gasUtils.averageTemperatureWest(dbDate,GetRunNumber());
      
      // test if averageBodyTemperature for FTPC West found for first event
      if (paramReader.gasTemperatureWest() == 0) {
	 // no value found in Calibrations_ftpc/ftpcGasOut for first event
         // initialize FTPC gas temperatures to default values 
            // default values change depending on SVT high voltage on/off
            // currently using daqReader->SVTPresent() to test but may need
            // access to Conditions_svt/svtInterLocks
         LOG_DEBUG << "daqReader->SVTPresent() = " << daqReader->SVTPresent() << endm;
	 returnCode = gasUtils.defaultTemperatureWest(dbDate,daqReader->SVTPresent());
      }	 

      // For FTPC East
      
      returnCode = gasUtils.averageTemperatureEast(dbDate,GetRunNumber());
      
      // test if averageBodyTemperature for FTPC East found for first event
      if (paramReader.gasTemperatureEast() == 0 ) {
	 // no value found in Calibrations_ftpc/ftpcGasOut for first event
         // initialize FTPC gas temperatures to default values 
            // default values change depending on SVT high voltage on/off
            // currently using daqReader->SVTPresent() to test but may need
            // access to Conditions_svt/svtInterLocks
         LOG_DEBUG << "daqReader->SVTPresent() = " << daqReader->SVTPresent() << endm;
	 returnCode = gasUtils.defaultTemperatureEast(dbDate,daqReader->SVTPresent());
      }	 


 
       // Calculate and set adjustedAirPressureWest

       paramReader.setAdjustedAirPressureWest(paramReader.normalizedNowPressure()*((dbReader.baseTemperature()+STP_Temperature)/(paramReader.gasTemperatureWest()+STP_Temperature)));


       // Calculate and set adjustedAirPressureEast

      paramReader.setAdjustedAirPressureEast(paramReader.normalizedNowPressure()*((dbReader.baseTemperature()+STP_Temperature)/(paramReader.gasTemperatureEast()+STP_Temperature)));

      LOG_INFO << " Using normalizedNowPressure = "<<paramReader.normalizedNowPressure()<<" gasTemperatureWest = "<<paramReader.gasTemperatureWest()<<" gasTemperatureEast = "<<paramReader.gasTemperatureEast()<< " to calculate and set adjustedAirPressureWest = "<<paramReader.adjustedAirPressureWest()<<" and adjustedAirPressureEast = "<<paramReader.adjustedAirPressureEast()<<endm;

    }

  mHitArray = new TObjArray(10000);
  mHitArray->SetOwner(kTRUE); // make sure that delete calls ->Delete() as well
  AddData(new TObjectSet("ftpcClusters", mHitArray));

  // ghitarray will only be used if fast simulator is active
  TObjArray ghitarray(10000);  
  ghitarray.SetOwner();
 
  // Test if input data is embedding data (StFtpcMixerMaker)

  St_DataSet *mix = NULL;
  mix = GetDataSet("FtpcMixer");
  if (mix) {
     LOG_INFO <<" DataSet FtpcMixer found" << endm;  
     //mix->ls(0);

    // Create an iterator
    St_DataSetIter ftpc_raw(mix);

   //Get the tables
   St_fcl_ftpcadc   *adc = (St_fcl_ftpcadc *) ftpc_raw.Find("fcl_ftpcadc");
   St_fcl_ftpcsqndx *sqndx = (St_fcl_ftpcsqndx *) ftpc_raw.Find("fcl_ftpcsqndx");
   if (adc && sqndx) {

      ftpcReader=new StFTPCReader((short unsigned int *) sqndx->GetTable(),
				  sqndx->GetNRows(),
				  (char *) adc->GetTable(),
				  adc->GetNRows());

      LOG_INFO << "Created StFTPCReader from StFtpcMixerMaker(Embedding) tables #fcl_ftpcsqndx rows = "<<sqndx->GetNRows()<<" #fcl_ftpcadc rows = "<<adc->GetNRows() << endm;
      using_FTPC_slow_simulator = 1;
  
      // Set gas temperature to default values so that database values printed only once
      paramReader.setGasTemperatureWest(dbReader.defaultTemperatureWest());
      paramReader.setGasTemperatureEast(dbReader.defaultTemperatureEast());
      LOG_INFO << "Found StFtpcMixerMaker sequences with #fcl_ftpcsqndx rows = "<<sqndx->GetNRows()<<" #fcl_ftpcadc rows = "<<adc->GetNRows() <<endm;
   }
    else {
      LOG_WARN << "FTPC Embedding Tables are not found:" 
               << " fcl_ftpcsqndx = " << sqndx 
	       << " fcl_ftpcadc   = " << adc << endm;
      return kStWarn;  
    }
  }

  // Test if input data is simulated data (StFtpcSlowSimMaker)

  St_DataSet *raw = GetDataSet("ftpc_raw");
  if (raw && !mix) {
    //			FCL
    St_DataSetIter get(raw);
    
    St_fcl_ftpcsqndx *fcl_ftpcsqndx = (St_fcl_ftpcsqndx*)get("fcl_ftpcsqndx");
    St_fcl_ftpcadc   *fcl_ftpcadc   = (St_fcl_ftpcadc*  )get("fcl_ftpcadc");

    if (fcl_ftpcsqndx&&fcl_ftpcadc) { 

      ftpcReader=new StFTPCReader((short unsigned int *) fcl_ftpcsqndx->GetTable(),
				  fcl_ftpcsqndx->GetNRows(),
				  (char *) fcl_ftpcadc->GetTable(),
				  fcl_ftpcadc->GetNRows());

      LOG_INFO << "Created StFTPCReader from FTPC Slow Simulator tables #fcl_ftpcsqndx rows = "<<fcl_ftpcsqndx->GetNRows()<<" #fcl_ftpcadc rows = "<<fcl_ftpcadc->GetNRows() << endm;
      using_FTPC_slow_simulator = 1;
  
      // Set gas temperature to default values so that database values printed only once
      paramReader.setGasTemperatureWest(dbReader.defaultTemperatureWest());
      paramReader.setGasTemperatureEast(dbReader.defaultTemperatureEast());
    }
    else {
      LOG_WARN << "FTPC Slow Simulator Tables are not found:" 
               << " fcl_ftpcsqndx = " << fcl_ftpcsqndx 
	       << " fcl_ftpcadc   = " << fcl_ftpcadc << endm;
      return kStWarn;  
    }
  }

  if(ftpcReader) {

    if(Debug()) {
       LOG_DEBUG << "start running StFtpcClusterFinder" << endm;
    }
      
    Int_t searchResult = kStOK;

    if (m_Mode == 2 || m_Mode==3) {
       StFtpcClusterDebug cldebug((int) GetRunNumber(),(int) GetEventNumber());
       cldebug.fillRun((int) GetRunNumber(), (int) mDbMaker->GetDateTime().GetDate(), (int) mDbMaker->GetDateTime().GetTime(), dbReader.microsecondsPerTimebin(), paramReader.normalizedNowPressure(), paramReader.standardPressure(),dbReader.baseTemperature(), paramReader.gasTemperatureWest(), paramReader.gasTemperatureEast(),paramReader.adjustedAirPressureWest()-paramReader.standardPressure(), paramReader.adjustedAirPressureEast()-paramReader.standardPressure());

       StFtpcClusterFinder fcl(                        ftpcReader, 
						       &paramReader, 
                                                       &dbReader,
						       mHitArray,
						       m_hitsvspad,
						       m_hitsvstime,
                                                       m_csteps,
                                                       m_chargestep_West,
                                                       m_chargestep_East,
						       &cldebug);
          if (Debug()) fcl.DebugOn=kTRUE;
          else fcl.DebugOn=kFALSE;
          searchResult = fcl.search();
    }

    else {
          StFtpcClusterFinder fcl(          ftpcReader, 
	 				    &paramReader, 
                                            &dbReader,
					    mHitArray,
					    m_hitsvspad,
					    m_hitsvstime,
                                            m_csteps,
                                            m_chargestep_West,
                                            m_chargestep_East);
          if (Debug()) fcl.DebugOn=kTRUE;
          else fcl.DebugOn=kFALSE;
          searchResult = fcl.search();
    }
    
    if (searchResult == kStERR) return kStERR;

    if (using_FTPC_slow_simulator) delete ftpcReader;
  }
  else {     
    //                      FFS
    St_DataSet *gea = GetDataSet("geant");
    St_DataSetIter geant(gea);
    St_g2t_vertex  *g2t_vertex  = (St_g2t_vertex *) geant("g2t_vertex");
    St_g2t_track   *g2t_track   = (St_g2t_track *)   geant("g2t_track");
    St_g2t_ftp_hit *g2t_ftp_hit = (St_g2t_ftp_hit *) geant("g2t_ftp_hit");
    if (g2t_vertex && g2t_track && g2t_ftp_hit){
      StFtpcGeantReader geantReader                         (g2t_vertex,
							     g2t_track,
							     g2t_ftp_hit);

      if(Debug()) {
        LOG_DEBUG << "NO RAW DATA AVAILABLE - start running StFtpcFastSimu" << endm;
      }
      
      StFtpcFastSimu  ffs                     (&geantReader,
					       &paramReader,
                                               &dbReader,
					       mHitArray,
					       &ghitarray);
      if(Debug()) {
        LOG_DEBUG << "finished running StFtpcFastSimu" << endm;
      }
    }
  }

  Int_t num_points = mHitArray->GetEntriesFast();
  if(num_points>0 && mFtpcHitColl) { // points found and hitCollection exists
    // Write hits to StEvent 
    StFtpcPoint *point;
    
    for (Int_t i=0; i<num_points; i++) {
      point = (StFtpcPoint *)mHitArray->At(i);
      point->ToStEvent(mFtpcHitColl); 
    }	
  }

       
  StDetectorDbFTPCVoltageStatus *voltageStatus = StDetectorDbFTPCVoltageStatus::instance();
  if ( !voltageStatus) {
      LOG_ERROR << "Error getting FTPC Offline database: Calibrations_ftpc/ftpcVoltageStatus"<<endm;
      return kStErr;
   }
  // if mVoltageStatus  and StEvent exists, set StDetectorState
  // (StDetectorState only set for events with FTPC data)
  if (voltageStatus && mCurrentEvent) {
      mCurrentEvent->addDetectorState(new StDetectorState(kFtpcEastId,voltageStatus->getStatusFTPCEast()));
      mCurrentEvent->addDetectorState(new StDetectorState(kFtpcWestId,voltageStatus->getStatusFTPCWest()));
  }


  Int_t num_gpoints = ghitarray.GetEntriesFast();
  if(num_gpoints>0)
    {
      St_ffs_gepoint *ffs_gepoint = new St_ffs_gepoint("ffs_fgepoint",num_gpoints);
      AddData(ffs_gepoint);
      
      ffs_gepoint_st *gpointTable= ffs_gepoint->GetTable();
      
      StFtpcGeantPoint *gpoint;
      
      for (Int_t i=0; i<num_gpoints; i++) 
	{
	  gpoint = (StFtpcGeantPoint *)ghitarray.At(i);
	  gpoint->ToTable(&(gpointTable[i]));    
	}
      
      ffs_gepoint->SetNRows(num_gpoints);
    }
  
  // mHitArray and its contents will be deleted by StMaker::Clear() since it is sitting in a TDataSet
    
  // Fill FTPC cluster maker histograms
  MakeHistograms(); 

  return kStOK;
}



//_____________________________________________________________________________
void StFtpcClusterMaker::MakeHistograms() 
{
  if (!mHitArray) return;

  //LOG_DEBUG<<"*** NOW MAKING HISTOGRAMS ***"<<endm;

  for (Int_t i=0; i<mHitArray->GetEntriesFast();i++) {
    StFtpcPoint *hit = (StFtpcPoint*)mHitArray->At(i);
  
    if (m_Mode == 4) m_pnt_planeF->Fill(hit->GetPadRow());
    //  created here because x,y still in FTPC internal coordinate system
    Float_t rpos = ::sqrt(hit->GetX()*hit->GetX() + hit->GetY()*hit->GetY());
    if (hit->GetPadRow() <=10 ) {
       m_cluster_radial_West->Fill(rpos);
       if (IAttr(".histos")) {
          if (m_Mode == 4) {
             m_pnt_xyFW->Fill(hit->GetX(),hit->GetY());
             m_pnt_padtimeFW->Fill(hit->GetNumberBins(),hit->GetNumberPads());
          }
          m_maxadc_West->Fill(hit->GetMaxADC());
          m_charge_West->Fill(hit->GetCharge());	 
       }	  
    } //end if hit->GetPadRow() <=10
    else if (hit->GetPadRow() >=11 ) {
       m_cluster_radial_East->Fill(rpos);
       if (IAttr(".histos")) {
          if (m_Mode == 4) {
             m_pnt_xyFE->Fill(hit->GetX(),hit->GetY());
             m_pnt_padtimeFE->Fill(hit->GetNumberBins(),hit->GetNumberPads());
          }
          m_maxadc_East->Fill(hit->GetMaxADC());
          m_charge_East->Fill(hit->GetCharge());
       }	  
    } //end if hit->GetPadRow() >=11

    if (IAttr(".histos") && Debug()) {
       Int_t flag = hit->GetFlags();
       if (flag > 0) {
          Int_t bin = 7;
          for (Int_t twofac=128; twofac>0; twofac=twofac/2,bin--) {
	     Int_t nbit = flag/twofac;
             if (nbit != 1) 	continue;
             m_flags->Fill((float)bin);
	     flag = flag - nbit*twofac;        
          } //end loop twofac
       } //endif flag

       Float_t nrow = hit->GetPadRow();
       m_row->Fill(nrow);
       Float_t nsec = hit->GetSector();
       m_sector->Fill(nsec);
       m_row_sector->Fill(nrow,nsec);

   //  Float_t npad = r->n_pads;
   //  m_pads->Fill(npad);
   //  Float_t nbin = r->n_bins;
   //  m_timebins->Fill(nbin);
   //  m_npad_nbin->Fill(npad,nbin);
    }  //end if IAttr
   
  } //end for mHitArray->GetEntriesFast()

}
