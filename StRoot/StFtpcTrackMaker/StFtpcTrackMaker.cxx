// $Id: StFtpcTrackMaker.cxx,v 1.93 2012/11/07 23:30:18 fisyak Exp $
// $Log: StFtpcTrackMaker.cxx,v $
// Revision 1.93  2012/11/07 23:30:18  fisyak
// Supress warnings
//
// Revision 1.92  2009/11/25 19:50:21  jcs
// remove all references to StFtpcSoftwareMonitor
//
// Revision 1.91  2009/11/24 11:54:01  jcs
// Remove dst_vertex_st code commented out by Yuri
//
// Revision 1.90  2009/11/23 16:38:11  fisyak
// Remove dependence on dst_vertex_st
//
// Revision 1.89  2008/10/02 16:21:01  jcs
// standardize m_Mode LOG_INFO messages
//
// Revision 1.88  2008/05/13 12:20:58  jcs
// only write FTPC calibration vertices with successful fit to StEvent
//
// Revision 1.87  2008/01/07 14:45:06  jcs
// create and fill the special set of Ftpc track histograms used to evaluate
// the Ftpc gain scan runs when the bfc option fgain is in the chain
//
// Revision 1.86  2007/12/12 12:49:34  jcs
// remove assert() and replace LOG_WARN with LOG_ERROR messages
//
// Revision 1.85  2007/12/11 09:36:39  jcs
// Remove m_nrec_track histogram (never used)
//
// Revision 1.84  2007/11/30 09:19:54  jcs
// Use the first primary vertex if any primary vertex exists
// Multiple primary vertices have been ordered in StEvent
//
// Revision 1.83  2007/05/08 09:02:35  jcs
// move database initialization from Init to InitRun as requested by Victor and Yuri
// redefine m_nrec_track histogram and only fill if debug option on
//
// Revision 1.82  2007/04/27 17:04:27  jcs
// forgot to check if bfc option debug is on before filling vertex by sector histograms
//
// Revision 1.81  2007/04/27 15:39:28  jcs
// Removed obsolete histogram (fpt_theta)
// Only create and fill FTPC vertex by sector histograms if bfc debug option is on
//
// Revision 1.80  2007/02/28 13:31:27  jcs
// temporary fix: add explanation why accessing ftpcDimensions and ftpcPadRowZ
// in Init doesn't cause problems
//
// Revision 1.79  2007/01/15 08:23:02  jcs
// replace printf, cout and gMesMgr with Logger commands
//
// Revision 1.78  2006/11/09 13:58:37  jcs
// bfc option fdbg selected if m_Mode>=2
//
// Revision 1.77  2006/09/27 11:00:31  jcs
// comment out ftpc vs. tpc vertex histogram definitions, they are defined and filled in St_QA_Maker
//
// Revision 1.76  2006/08/25 07:57:33  jcs
// remove debug printout
//
// Revision 1.75  2006/03/19 19:25:06  jcs
// Select LASERTRACKING with bfc option 'flaser' (otherwise TWOCYCLETRACKING is used)
// Create DEBUGFILE with bfc option 'fdbg'
//
// Revision 1.74  2006/03/13 19:46:35  jcs
// make changes necessary fot DoT0Calib
//
// Revision 1.73  2005/06/30 09:21:48  jcs
// extend histogram limits to make gasGain determination easier
//
// Revision 1.72  2005/03/01 23:02:11  jcs
// Correct error: TWOCYCLETRACKING should be standard, not LASERTRACKING
//
// Revision 1.71  2004/12/10 23:07:37  jcs
// Only fill FTPC software monitor if it exists
//
// Revision 1.70  2004/11/23 19:18:16  jcs
// Store FTPC vertices (east and west) in StEvent/StCalibrationVertex
// Comment out (should eventually be removed) Ftpc vertex - Tpc vertex histograms,
// they are now filled in StEventQAMaker.cxx
//
// Revision 1.69  2004/09/27 14:17:03  jcs
// pad vs. time histograms moved to St_QA_Maker
//
// Revision 1.68  2004/09/07 14:06:19  jcs
// use the IAttr(".histos") to control histogramming
//
// Revision 1.67  2004/09/03 20:36:22  perev
// Big LeakOff + mem optimisation
//
// Revision 1.66  2004/08/10 12:42:10  jcs
// move DEBUGFILE and tracking method define statements from .h to .cxx
//
// Revision 1.65  2004/08/10 12:21:42  jcs
// remove histograms which are also created in St_QA_Maker
//
// Revision 1.64  2004/08/09 15:08:14  jcs
// remove unused histogram
//
// Revision 1.63  2004/06/18 12:07:41  jcs
// replace #ifdef...#elif...#endif conditional compiler directives with #ifdef...#endif #ifdef...#endif
//
// Revision 1.62  2004/06/18 09:07:03  jcs
// add code to write out a root file for calibration
//
// Revision 1.61  2004/06/04 11:04:15  jcs
// replaced StarDb/ftpc/fdepars/fdepar with StarDb/ftpc/ftpcdEdxPars
//
// Revision 1.60  2004/05/25 07:25:47  jcs
// initialize StFtpcSoftwareMonitor*ftpcMon
//
// Revision 1.59  2004/05/24 13:46:39  jcs
// fill StFtpcSoftwareMonitor not dst_mon_soft_ftpc
//
// Revision 1.58  2004/05/19 17:45:06  oldi
// *** empty log message ***
//
// Revision 1.57  2004/05/07 15:02:18  oldi
// Tracks are written to StEvent directly, now.
// Primary Vertex is read from StEvent.
// MonSoftDst table is filled here now (was filled in the now obsolete StFtpcGlobalMaker before).
//
// Revision 1.56  2004/04/26 09:53:45  jcs
// comment out delete StFtpcTrackingParams::Instance() in FinishRun as a temorary
// fix for Bug #372
//
// Revision 1.55  2004/03/22 16:02:03  oldi
// Moved destruction of the instance of StFtpcTrackingParams from Finish() to FinishRun().
//
// Revision 1.54  2004/02/13 21:12:19  oldi
// Protection against missing FTPC DAQ data added.
//
// Revision 1.53  2004/02/12 19:37:11  oldi
// *** empty log message ***
//
// Revision 1.52  2004/02/05 00:24:54  oldi
// Eliminating a bug concerning a test of the wrong pointer to a vertex.
//
// Revision 1.51  2004/01/28 01:41:32  jeromel
// *** empty log message ***
//
// Revision 1.50  2003/10/07 14:08:33  jcs
// remove previous fix for determining magnetic field
//
// Revision 1.49  2003/09/26 06:08:55  oldi
// Check if the magentic field was reversed 'by hand' with a chain option.
// If yes, multiply the scaleFactor of the field with -1.
//
// Revision 1.48  2003/09/02 17:58:16  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.47  2003/07/07 20:29:30  oldi
// Pointer to geant taken out (no actual code change).
//
// Revision 1.46  2003/07/04 14:09:31  fsimon
// SlowSimulator now rotates hits: Check for simulated hits before rotation
// commented out.
//
// Revision 1.45  2003/05/20 18:34:57  oldi
// Cuts for vertex estimation introduced (globDca < 1 cm, multiplicity >= 200).
//
// Revision 1.44  2003/02/21 01:14:03  oldi
// Unnecessary call of database "Geometry/tpc" removed.
//
// Revision 1.43  2003/01/20 13:11:56  oldi
// Floats converted to ints to avoid warnings on linux machines.
//
// Revision 1.42  2003/01/16 18:04:34  oldi
// Bugs eliminated. Now it compiles on Solaris again.
// Split residuals for global and primary fit.
//
// Revision 1.41  2002/11/06 13:46:46  oldi
// Vertex handling simplifed.
// Global/primary fit handling simplified.
// Vertex estimations only calculated if a vertex was used for tracking.
// Code clean ups.
//
// Revision 1.40  2002/10/31 13:40:52  oldi
// dE/dx parameters read from database.
// Calibration parameters read from database.
// Vertex estimation for different sectors added.
// Vertex estimation switched off for events with no tracks.
//
// Revision 1.39  2002/10/11 15:45:31  oldi
// Get FTPC geometry and dimensions from database.
// No field fit activated: Returns momentum = 0 but fits a helix.
// Bug in TrackMaker fixed (events with z_vertex > outer_ftpc_radius were cut).
// QA histograms corrected (0 was supressed).
// Code cleanup (several lines of code changed due to *params -> Instance()).
// cout -> gMessMgr.
//
// Revision 1.38  2002/10/03 10:34:01  oldi
// Usage of gufld removed.
// Magnetic field is read by StMagUtilities, now.
//
// Revision 1.37  2002/09/07 21:30:52  jeromel
// Syntax correct ")" or "(" at the begining of a line seems to make gcc crash
// in optimize flags.
//
// Revision 1.36  2002/08/02 11:15:21  oldi
// Tracking is performed even if no primary vertex is found. In this case
// (0., 0., 0.) is used as vertex position.
// Minor cosmetics.
//
// Revision 1.35  2002/06/06 15:59:18  oldi
// Local -> global transformation is only done if the event isn't simulated.
//
// Revision 1.34  2002/06/04 13:39:26  oldi
// After tracking local coordinates are transformed to global coordinates.
// Points are written to the table again. This causes a loss of symmetry which
// was used while the tracking was done. Due to the fact that the main vertex
// is measured in global coordinates, the subsequent momentum fit is done
// correctly.
//
// Revision 1.33  2002/04/29 15:50:10  oldi
// All tracking parameters moved to StFtpcTrackingParameters.cc/hh.
// In a future version the actual values should be moved to an .idl file (the
// interface should be kept as is in StFtpcTrackingParameters.cc/hh).
//
// Revision 1.32  2002/04/09 16:10:13  oldi
// Method to get the magentic field factor moved to StFormulary. It works for
// simulation as well, now.
//
// Revision 1.31  2002/04/08 15:38:04  oldi
// Switch for magnetic field factor installed.
// Minor corrections/improvements.
//
// Revision 1.30  2002/04/05 16:51:00  oldi
// Cleanup of MomentumFit (StFtpcMomentumFit is now part of StFtpcTrack).
// Each Track inherits from StHelix, now.
// Therefore it is possible to calculate, now:
//  - residuals
//  - vertex estimations obtained by back extrapolations of FTPC tracks
// Chi2 was fixed.
// Many additional minor (and major) changes.
//
// Revision 1.29  2002/03/25 12:50:56  oldi
// Customization of Warnings.
//
// Revision 1.28  2002/03/25 09:52:55  jcs
// exit with warning if primary vertex calculation returns nan
//
// Revision 1.27  2002/03/15 10:04:41  oldi
// Adjust eta segments not only to z-position of vertex but to x,y as well.
// Avoid tracking if vertex position is outside of the inner radius of the Ftpc.
//
// Revision 1.26  2002/03/01 14:21:21  jcs
// add additional histograms to monitor cluster finding
//
// Revision 1.25  2002/02/20 16:11:15  jcs
// change loop over vertex table to test if first row is primary vertex
//
// Revision 1.24  2002/02/10 21:04:49  jcs
// Use primary vertex for tracking if it is available. Otherwise use preVertex.
//
// Revision 1.23  2002/02/05 13:53:09  jcs
// remove code for ZDC and Holm's primary vertex calculation methods
// if no preVertex available, stop FTPC tracking
//
// Revision 1.22  2001/09/19 20:58:40  jcs
// Use TPC preVertex if it exists, if not use ZDC vertex(slew correction hardwired)
//
// Revision 1.21  2001/07/26 13:42:27  oldi
// Two messages added for the case of missing data.
//
// Revision 1.20  2001/07/13 17:58:18  oldi
// Small change related to new StFtpcDisplay (commented).
//
// Revision 1.19  2001/07/12 13:08:49  oldi
// Remove display !@#$%^&*(.
//
// Revision 1.18  2001/07/12 13:05:00  oldi
// QA histogram of FTPC vertex estimation is generated.
// FTPC vertex estimation is stored as pre vertex (id = 301) in any case, now.
//
// Revision 1.17  2001/07/12 08:42:28  oldi
// Minor update.
//
// Revision 1.16  2001/04/27 10:20:24  oldi
// Moved function calls of StFtpcTrackEvalulator to constructor of StFtpcTrackEvalulator.
//
// Revision 1.15  2001/01/30 13:31:41  oldi
// New variable mTime introduced to count total time consumption.
//
// Revision 1.14  2001/01/25 15:22:25  oldi
// Review of the complete code.
// Fix of several bugs which caused memory leaks:
//  - Tracks were not allocated properly.
//  - Tracks (especially split tracks) were not deleted properly.
//  - TClonesArray seems to have a problem (it could be that I used it in a
//    wrong way). I changed all occurences to TObjArray which makes the
//    program slightly slower but much more save (in terms of memory usage).
// Speed up of HandleSplitTracks() which is now 12.5 times faster than before.
// Cleanup.
//
// Revision 1.13  2000/11/10 18:38:50  oldi
// Cleanup due to changes in other classes.
//
// Revision 1.12  2000/08/09 19:15:31  didenko
// remove unneeded include
//
// Revision 1.11  2000/08/07 00:20:03  jcs
// save prevertex information correctly
//
// Revision 1.10  2000/07/18 21:22:17  oldi
// Changes due to be able to find laser tracks.
// Cleanup: - new functions in StFtpcConfMapper, StFtpcTrack, and StFtpcPoint
//            to bundle often called functions
//          - short functions inlined
//          - formulas of StFormulary made static
//          - avoid streaming of objects of unknown size
//            (removes the bunch of CINT warnings during compile time)
//          - two or three minor bugs cured
//
// Revision 1.9  2000/07/03 12:45:23  jcs
// get (pre)Vertex coordinates directly from (pre)Vertex table instead of from
// fptpars
//
// Revision 1.8  2000/06/26 22:10:44  fisyak
// remove params
//
// Revision 1.7  2000/06/15 09:13:34  oldi
// No tracking is performed (return kStWarn instead) if the z-position of the
// main vertex is off by more than 100 cm from z = 0. Different error messages
// (depending on how far the vertex is off) are printed.
//
// Revision 1.6  2000/06/13 14:25:56  oldi
// Changed couts to gMessMgr->Message().
// Printed output changed (slightly).
//
// Revision 1.5  2000/06/07 11:16:29  oldi
// Changed 0 pointers to NULL pointers.
// Function HandleSplitTracks() called.
//
// Revision 1.4  2000/05/15 14:28:12  oldi
// problem of preVertex solved: if no main vertex is found (z = NaN) StFtpcTrackMaker stops with kStWarn,
// refitting procedure completed and included in StFtpcTrackMaker (commented),
// new constructor of StFtpcVertex due to refitting procedure,
// minor cosmetic changes
//
// Revision 1.3  2000/05/12 12:59:16  oldi
// removed delete operator for mSegment in StFtpcConfMapper (mSegment was deleted twice),
// add two new constructors for StFtpcTracker to be able to refit already existing tracks,
// minor cosmetics
//
// Revision 1.2  2000/05/11 15:14:52  oldi
// Changed class names *Hit.* due to already existing class StFtpcHit.cxx in StEvent
//
// Revision 1.1  2000/05/10 13:39:28  oldi
// Initial version of StFtpcTrackMaker
//

//----------Author:        Markus D. Oldenburg
//----------Last Modified: 10.11.2000
//----------Copyright:     &copy MDO Production 1999

#include "StFtpcTrackMaker.h"
#include "StFtpcVertex.hh"
#include "StFtpcConfMapper.hh"
#include "StFtpcDisplay.hh"
#include "StFtpcTrackEvaluator.hh"
#include "StFormulary.hh"
#include "StFtpcTrackingParams.hh"
#include "StFtpcTrackToStEvent.hh"
#include "StFtpcClusterMaker/StFtpcClusterDebug.hh"

#include "TObjArray.h"
#include "TObjectSet.h"
#include "StEvent.h"

#include <Stiostream.h>
#include <math.h>

#include "TDataSet.h"
#include "TDataSetIter.h"

#include "StVertexId.h"
#include "StFtpcHit.h"
#include "StPrimaryVertex.h"
#include "StCalibrationVertex.h"
#include "StMeasuredPoint.h"
#include "StMessMgr.h"

#include "tables/St_ffs_gepoint_Table.h"
#include "tables/St_g2t_track_Table.h"
#include "TH1.h"
#include "TH2.h"

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StFtpcTrkMaker class for Makers                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

ClassImp(StFtpcTrackMaker)

//_____________________________________________________________________________
StFtpcTrackMaker::StFtpcTrackMaker(const char *name) : StMaker(name)
{
  // Default constructor.
}

//_____________________________________________________________________________
StFtpcTrackMaker::~StFtpcTrackMaker()
{
  // Destructor.
}

//_____________________________________________________________________________
Int_t StFtpcTrackMaker::InitRun(Int_t run) {

  // get ftpc parameters
  TDataSet *ftpcParsDb = GetInputDB("ftpc");
  if (!ftpcParsDb) {
     LOG_ERROR << "StFtpcTrackMaker::Error Getting FTPC parameter database" << endm;
     return kStErr;
  }
  TDataSetIter ftpcPars(ftpcParsDb);

  // get ftpc geometry
  TDataSet *ftpcGeometryDb = GetDataBase("Geometry/ftpc");

  if (!ftpcGeometryDb){
    LOG_ERROR << "StFtpcTrackMaker::Error Getting FTPC database: Geometry" << endm;
    return kStErr;
  }

  TDataSetIter ftpcGeometry(ftpcGeometryDb);

  // get tracking parameters from database

    //  Explanation: 
    //  dbDate not yet set in Init but Geometry_ftpc/ftpcPadrowZ never changes
    //  and only Geometry_ftpc/ftpcDimensions.sizeOfTimebin changed but it was
    //  never used in StFtpcTrackMaker


  StFtpcTrackingParams::Instance(Debug(),
  				 (St_ftpcTrackingPars *)ftpcPars("ftpcTrackingPars"),
  				 (St_ftpcdEdxPars *)ftpcPars("ftpcdEdxPars"),
  				 (St_ftpcDimensions *)ftpcGeometry("ftpcDimensions"), 
  				 (St_ftpcPadrowZ *)ftpcGeometry("ftpcPadrowZ"));
cout<<"InitRun StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide() = "<<StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide()<<endl;

  if (StFtpcTrackingParams::Instance()->GetReturnCode() > 0) {
    LOG_ERROR << " FATAL error in StFtpcTrackingParams return code = "<< StFtpcTrackingParams::Instance()->GetReturnCode() <<endm;
    return kStErr;
  }

  // get ftpc calibration db
  TDataSet *ftpcCalibrationsDb = GetDataBase("Calibrations/ftpc");

  if (!ftpcCalibrationsDb){
    LOG_ERROR << "StFtpcTrackMaker::Error Getting FTPC database: Calibrations" << endm;
    return kStErr;
  }

  TDataSetIter ftpcCalibrations(ftpcCalibrationsDb);

  // get run dependent tracking parameters from database
  StFtpcTrackingParams::Instance(kTRUE, 
				 (St_ftpcCoordTrans *)ftpcCalibrations("ftpcCoordTrans"));
  
  if (StFtpcTrackingParams::Instance()->GetReturnCode() > 0) {
    LOG_ERROR << " FATAL error in StFtpcTrackingParams return code = "<< StFtpcTrackingParams::Instance()->GetReturnCode() <<endm;
    return kStErr;
  }

  return kStOK;
}

//_____________________________________________________________________________
Int_t StFtpcTrackMaker::Init()
{
  // Initialisation.

  // m_Mode is used to pass bfc option information to the Maker
  //        m_Mode            bfc option(s)      Comments
  //          0                                  use TwoCycle tracking
  //          1               flaser             use Laser Tracking - ALWAYS used in combination with the "fdbg" bfc option
  //          2               fdbg               add tracking information to the special Ftpc root file created in StFtpcClusterMaker
  //                                             for analysis in StFtpcCalibMaker
  //          3               fdbg flaser        both "fdbg" and "flaser" bfc options selected
  //          4               fgain              create and fill the special set of Ftpc track histograms used to evaluate
  //                                             the Ftpc gain scan runs

  LOG_INFO << "StFtpcTrackMaker entered with m_Mode = "<< m_Mode <<endm;

  // Create Histograms

if (m_Mode == 2 || m_Mode == 3) {
  LOG_INFO << "StFtpcTrackMaker running with fdbg option selected" << endm;
  if (m_Mode == 3) LOG_INFO << "StFtpcTrackMaker running with flaser option selected" << endm;
  m_vtx_pos      = new TH1F("fpt_vtx_pos", "FTPC estimated vertex position", 800, -400.0, 400.0);
}

//  m_vertex_east_xy = new TH2F("fpt_vertex_east_xy", 
//			      "FTPC east vertex xy estimation with resp. to TPC vertex", 
//			      80, -2., 2., 80, -2., 2.);
//  m_vertex_east_z  = new TH1F("fpt_vertex_east_z", 
//			      "FTPC east vertex z estimation with resp. to TPC vertex", 
//			      100, -10., 10.);
//  m_vertex_west_xy = new TH2F("fpt_vertex_west_xy", 
//			      "FTPC west vertex xy estimation with resp. to TPC vertex", 
//			      80, -2., 2., 80, -2., 2.);
//  m_vertex_west_z  = new TH1F("fpt_vertex_west_z", 
//			      "FTPC west vertex z estimation with resp. to TPC vertex",
//			      100, -10., 10.);

  if (IAttr(".histos")) {
     if (m_Mode == 4) {
        LOG_INFO << "StFtpcTrackMaker running with fgain option selected" <<endm;
        m_pointFW = new TH1F("NPntFW","N points on trk,ftpc west", 8, 4.,12.);
        m_pointFE = new TH1F("NPntFE","N points on trk,ftpc east", 8, 4.,12.);
        m_ratiomFW = new TH1F("RnmFW","ratio Nfit/max pnt, ftpc west", 55, 0., 1.1);
        m_ratiomFE = new TH1F("RnmFE","ratio Nfit/max pnt, ftpc east", 55, 0., 1.1);
        m_planefF = new TH1F("PlanefF","plane of first hit on trk, ftpc",20,0.5,20.5);
        m_psiFW   = new TH1F("PsiFW","psi, ftpc west", 90, 0.,360.);
        m_psiFE   = new TH1F("PsiFE","psi, ftpc east", 90, 0.,360.);
        m_xf_yfFW = new TH2F("XfYfFW","Y vs X of first hit on trk, ftpcW", 70,-35.,35.,70,-35.,35.);
        m_xf_yfFE = new TH2F("XfYfFE","Y vs X of first hit on trk, ftpcE", 70,-35.,35.,70,-35.,35.);
        m_good_trk = new TH2F("NtrkGoodF","total number tracks found ftpcE vs. ftpcW",150,0.,1500.,150,0.,1500.);
        m_good_trk->SetXTitle("FTPC East");
        m_good_trk->SetYTitle("FTPC West");
        m_pnt_padtimeFW    = new TH2F("PointPadTimeFtpcW","point: #pads vs #timebins of hits, ftpcW",12,0.5,12.5,10,0.5,10.5);
        m_pnt_padtimeFW->SetXTitle("#timebins");
        m_pnt_padtimeFW->SetYTitle("#pads");
        m_pnt_padtimeFE    = new TH2F("PointPadTimeFtpcE","point: #pads vs #timebins of hits, ftpcE",12,0.5,12.5,10,0.5,10.5);
        m_pnt_padtimeFE->SetXTitle("#timebins");
        m_pnt_padtimeFE->SetYTitle("#pads");
      }
//   m_nrec_track   = new TH2F("fpt_hits_mom", "FTPC: points found per track vs. momentum" , 10, 0.5, 10.5, 100, 0.0, 20.);

     m_maxadc_West = new TH1F("fpt_maxadcW", "FTPCW MaxAdc", 150, 0.5, 150.5);
     m_maxadc_East = new TH1F("fpt_maxadcE", "FTPCE MaxAdc", 150, 0.5, 150.5);

     m_charge_West = new TH1F("fpt_chargeW", "FTPCW charge", 80, 0.5, 800.5);
     m_charge_East = new TH1F("fpt_chargeE", "FTPCE charge", 80, 0.5, 800.5);
 
     m_xres   = new TH1F("fpt_x_res",   "FTPC x residuals",   100, -0.25, 0.25);
     m_yres   = new TH1F("fpt_y_res",   "FTPC y residuals",   100, -0.25, 0.25);
     m_rres   = new TH1F("fpt_r_res",   "FTPC r residuals",   100, -0.25, 0.25);
     m_phires = new TH1F("fpt_phi_res", "FTPC phi residuals", 100, -0.01, 0.01);

     m_rres_vs_r_east   = new TH2F("fpt_r_res_vs_r_east", "FTPC east r residuals vs. r", 
        			   100, -0.25, 0.25, 100, 6.5, 31.);
     m_phires_vs_r_east = new TH2F("fpt_phi_res_vs_r_east", "FTPC east phi residuals vs. r", 
				   100, -0.01, 0.01, 100, 6.5, 31.);
     m_rres_vs_r_west   = new TH2F("fpt_r_res_vs_r_west", "FTPC west r residuals vs. r", 
				   100, -0.25, 0.25, 100, 6.5, 31.);
     m_phires_vs_r_west = new TH2F("fpt_phi_res_vs_r_west", "FTPC west phi residuals vs. r", 
				   100, -0.01, 0.01, 100, 6.5, 31.);

     if (Debug()) {
        m_vertex_east_x_vs_sector = new TH2F("fpt_vertex_east_x_vs_sector", 
 	            		             "FTPC east vertex x estimation vs. sector with resp. to TPC vertex", 
				             6, 0.5, 6.5,  80,  -2.,  2.);
        m_vertex_east_y_vs_sector = new TH2F("fpt_vertex_east_y_vs_sector", 
				             "FTPC east vertex y estimation vs. sector with resp. to TPC vertex", 
				             6, 0.5, 6.5,  80,  -2.,  2.);
        m_vertex_east_z_vs_sector = new TH2F("fpt_vertex_east_z_vs_sector", 
				             "FTPC east vertex z estimation vs. sector with resp. to TPC vertex", 
				             6, 0.5, 6.5, 100, -10., 10.);
        m_vertex_west_x_vs_sector = new TH2F("fpt_vertex_west_x_vs_sector", 
				             "FTPC west vertex x estimation vs. sector with resp. to TPC vertex", 
				             6, 0.5, 6.5,  80,  -2.,  2.);
        m_vertex_west_y_vs_sector = new TH2F("fpt_vertex_west_y_vs_sector", 
				             "FTPC west vertex y estimation vs. sector with resp. to TPC vertex", 
				             6, 0.5, 6.5,  80,  -2.,  2.);
        m_vertex_west_z_vs_sector = new TH2F("fpt_vertex_west_z_vs_sector", 
				             "FTPC west vertex z estimation vs. sector with resp. to TPC vertex", 
				             6, 0.5, 6.5, 100, -10., 10.);
     }
  }  //end if IAttr(".histos")

  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StFtpcTrackMaker::Make()
{
  // Setup and tracking.
  
  LOG_INFO << "Tracking (FTPC) started..." << endm;

  TObjectSet* objSet = (TObjectSet*)GetDataSet("ftpcClusters");
  if (!objSet) {
    LOG_WARN << "StFtpcTrackMaker::Make(): TObjectSet of ftpc clusters is missing." << endm;
    return kStWarn;
  }
  TObjArray *ftpcHits = (TObjArray*)objSet->GetObject();
  if (!ftpcHits) {
    LOG_WARN << "No FTPC clusters available!" << endm;
    return kStWarn;
  }  
  Int_t rc = StFtpcTrackingParams::Instance()->GetReturnCode();
  if (rc > 0) {
    LOG_ERROR << "FTPC Database not available. Return Code = " << rc << endm;
    return kStErr;
  }

  StEvent *event = dynamic_cast<StEvent*>( GetInputDS("StEvent") );    

  StFtpcVertex vertex; // create vertex (all parameters set to 0)

  // Use the first primary vertex if any primary vertex exists
  // Multiple vertices have been ordered in StEvent::addPrimaryVertex

  if (event->numberOfPrimaryVertices() > 0) {
        vertex = StFtpcVertex(event->primaryVertex(0));
  }
  if (Int_t problem = vertex.CheckVertex()) {
    return problem;
  }

  StFtpcConfMapper tracker(ftpcHits, &vertex, kTRUE);
  
  // tracking 
  if (StFtpcTrackingParams::Instance()->MagFieldFactor() == 0.) {
    tracker.NoFieldTracking();
  }
  
  else if (m_Mode%2 == 0) {
    tracker.TwoCycleTracking();
    LOG_INFO << "StFtpcTrackMaker: Using TwoCycleTracking"<<endm;
  }
  else if (m_Mode%2 == 1) {
    tracker.LaserTracking();
    LOG_INFO << "StFtpcTrackMaker: Using LaserTracking"<<endm;
  }
  
  // for the line above you have these possibilities
  //tracker.MainVertexTracking();
  //tracker.FreeTracking();
  //tracker.TwoCycleTracking();
  //tracker.NoFieldTracking();
  //tracker.LaserTracking();
  
  // coordinate transformation due to rotation and shift of TPC with respect to the magnet 
  // (= global coordinate system). 
  // Since the simulator incorporates these transformations, the distinction between simulated
  // and real events isn't necessary any more. 
      
  TObjArray *clusters = tracker.GetClusters();
  StFtpcPoint *point;
    
  // loop over all clusters
  for (Int_t i = 0; i < clusters->GetEntriesFast(); i++) {
    point = (StFtpcPoint *)clusters->At(i);
    point->TransformFtpc2Global(); // errors are not transformed up to now (effect should be small) !!!
    if (event->ftpcHitCollection()) point->GetStFtpcHit()->update(*point);
  }

  // momentum fit, dE/dx calculation
  tracker.GlobalFitAnddEdx();

  // calculate FTPC calibration vertices
  if (tracker.GetNumberOfTracks() >= StFtpcTrackingParams::Instance()->MinNumTracks()) {
    tracker.EstimateVertex(tracker.GetVertex(), 1);
    
     // write FTPC calibration vertices (east and west) to StEvent
    if (tracker.GetVertexEast()->GetIFlag() == 0) {
       const StThreeVectorF east(tracker.GetVertexEast()->GetX(), tracker.GetVertexEast()->GetY(), tracker.GetVertexEast()->GetZ());
       StCalibrationVertex *ftpcEastVertex = new StCalibrationVertex();
       ftpcEastVertex->setPosition(east);
       ftpcEastVertex->setType(kFtpcEastCalVtxId);
       event->addCalibrationVertex(ftpcEastVertex);
       LOG_INFO << "Added FTPC East calibration vertex: x = "<<tracker.GetVertexEast()->GetX()<<" y = "<<tracker.GetVertexEast()->GetY()<<" z = "<<tracker.GetVertexEast()->GetZ() << endm;
    }

    if (tracker.GetVertexWest()->GetIFlag() == 0) {
       StThreeVectorF west(tracker.GetVertexWest()->GetX(), tracker.GetVertexWest()->GetY(), tracker.GetVertexWest()->GetZ());
       StCalibrationVertex *ftpcWestVertex = new StCalibrationVertex();
       ftpcWestVertex->setPosition(west);
       ftpcWestVertex->setType(kFtpcWestCalVtxId);
       event->addCalibrationVertex(ftpcWestVertex);
       LOG_INFO << "Added FTPC West calibration vertex: x = "<<tracker.GetVertexWest()->GetX()<<" y = "<<tracker.GetVertexWest()->GetY()<<" z = "<<tracker.GetVertexWest()->GetZ() << endm;
    }
  }

  // write global tracks, do primary fit, write primary tracks
  StFtpcTrackToStEvent trackToStEvent;
  trackToStEvent.FillEvent(event, tracker.GetTracks());  
  tracker.PrimaryFit();
  trackToStEvent.FillEventPrimaries(event, tracker.GetTracks());
  
  if (Debug()) {
    LOG_INFO << "Total time consumption         " << tracker.GetTime() << " s." << endm;
    StFtpcTrackingParams::Instance()->PrintParams();
    tracker.TrackingInfo();
  }
  
  else {
    //tracker.SettingInfo();
    // Cuts only set for laser tracking
    //tracker.CutInfo();
    tracker.TrackingInfo();
  }

if (m_Mode == 2 || m_Mode == 3) {
  Double_t vertexPos[3];
  if (m_Mode == 2) {
     vertexPos[0] = vertex.GetX();
     vertexPos[1] = vertex.GetY();
     vertexPos[2] = vertex.GetZ();
     LOG_INFO<<"TWOCYCLETRACKING: vertexPos[0] = "<<vertexPos[0]<<" vertexPos[1] = "<<vertexPos[1]<<" vertexPos[2] = "<<vertexPos[2]<<endm;
  }
  if (m_Mode == 3) {
     vertexPos[0] = 0.;
     vertexPos[1] = 0.;
     vertexPos[2] = 0.;
     LOG_INFO<<"LASER : No FTPC to global transformation !!!"<<endm;
  }
    StFtpcClusterDebug cldebug((int) GetRunNumber(),(int) GetEventNumber());
    //LOG_INFO<<"Debug fill tracktree"<<endm;
    cldebug.filltracktree(tracker.GetTracks(),vertexPos);
    //if (cldebug.drawvertexhisto!=0)
       //cldebug.drawvertex(m_vertex_east,m_vertex_west,m_vtx_pos);
}
  
  
/*
  // Track Display
  
  // Uncomment this block if you want to see (I mean see!) the found tracks.
  {
    StFtpcDisplay display(tracker.GetClusters(), tracker.GetTracks());
    //display.TrackInfo();
    //display.Info();
    //display.ShowClusters();
    //display.ShowTracks();
    display.WriteData("ftpc_display.root");
  }
  */
  
  /*
  // Track Evaluator
  
  // Uncomment this block to get information about the quality 
  // of the found tracks in comparison to the simulated input event.

  TDataSet *ftpc_data = GetDataSet("ftpc_hits");

  if (!ftpc_data) {
    LOG_WARN << "No FTPC data available!" << endm;
    return kStWarn;
  }

  {StFtpcTrackEvaluator eval                            (geant, 
							ftpc_data, 
							tracker.GetVertex(), 
							tracker.GetClusters(), 
							tracker.GetTracks(), 
							"ftpc_evaluator.root", 
							"RECREATE");
    
  // Uncomment the following line if you want to 'see' the information (split tracks, unclean tracks, ...) 
  // evaluated by the TrackEvaluator.  
  //eval.ShowTracks();
  }
  */
  
  if (tracker.GetNumberOfTracks() > 0) { // only done when some tracks found
    MakeHistograms(&tracker);
  }
  
  
  LOG_INFO << "Tracking (FTPC) completed." << endm;
  
  return kStOK;;
}



//_____________________________________________________________________________
void   StFtpcTrackMaker::MakeHistograms(StFtpcTracker *tracker)
{
  // Fill histograms.
  // This is done only if at least one track was found.
  // With that a crash in EstimateVertex is prohibited. 
  // (Problem to fit empty histograms.)

  if (tracker->GetVertex()->GetIFlag()) { // only do vertex estimation if some found vertex was used for tracking

    // vertex estimation for different sectors
    StFtpcVertex vertex;
    
    if (tracker->GetNumberOfTracks() >= StFtpcTrackingParams::Instance()->MinNumTracks()) {
      
// the following 4 histograms are now accumulated in StEventQAMaker.cxx
// J.Seyboth Nov 19,2004
      // vertex estimation for both FTPCs (using all tracks)
      //m_vertex_east_xy->Fill(tracker->GetVertexEast()->GetX()-tracker->GetVertex()->GetX(),
      //			     tracker->GetVertexEast()->GetY()-tracker->GetVertex()->GetY());
      //m_vertex_east_z->Fill(tracker->GetVertexEast()->GetZ()-tracker->GetVertex()->GetZ());
      //m_vertex_west_xy->Fill(tracker->GetVertexWest()->GetX()-tracker->GetVertex()->GetX(),
      //			     tracker->GetVertexWest()->GetY()-tracker->GetVertex()->GetY());
      //m_vertex_west_z->Fill(tracker->GetVertexWest()->GetZ()-tracker->GetVertex()->GetZ());    
      
      if (IAttr(".histos") && Debug()) {
         for (Int_t i = 1; i <= 6; i++) { // east
	   vertex = tracker->EstimateVertex(tracker->GetVertex(), -1, i, 1);
	   m_vertex_east_x_vs_sector->Fill((Float_t)i, vertex.GetX()-tracker->GetVertex()->GetX());
	   m_vertex_east_y_vs_sector->Fill((Float_t)i, vertex.GetY()-tracker->GetVertex()->GetY());
	   m_vertex_east_z_vs_sector->Fill((Float_t)i, vertex.GetZ()-tracker->GetVertex()->GetZ());
         }
      
         for (Int_t i = 1; i <= 6; i++) { // west
	   vertex = tracker->EstimateVertex(tracker->GetVertex(), +1, i, 1);
	   m_vertex_west_x_vs_sector->Fill((Float_t)i, vertex.GetX()-tracker->GetVertex()->GetX());
	   m_vertex_west_y_vs_sector->Fill((Float_t)i, vertex.GetY()-tracker->GetVertex()->GetY());
	   m_vertex_west_z_vs_sector->Fill((Float_t)i, vertex.GetZ()-tracker->GetVertex()->GetZ());
         }
      } 
    } 
  } 

  if (IAttr(".histos")) {
     Int_t nTrkW = 0;
     Int_t nTrkE = 0;
     for (Int_t t_counter = 0; t_counter < tracker->GetTracks()->GetEntriesFast(); t_counter++) {
    
       StFtpcTrack *track = (StFtpcTrack*) tracker->GetTracks()->At(t_counter);
       TObjArray   *fhits = (TObjArray*) track->GetHits();
    
       StFtpcPoint* firstPoint =  (StFtpcPoint*)track->GetHits()->First();
       StFtpcPoint* lastPoint =  (StFtpcPoint*)track->GetHits()->Last();
       Int_t iftpc = (firstPoint->GetDetectorId() == 5) ? 0 : 1; // 0 for detId == 5 and1 for detId == 4
       // StFtpcTrack lastPoint => StEvent firstPoint !!!
       if (m_Mode == 4) m_planefF->Fill(lastPoint->GetPadRow());

       //  momentum angle at start
       Float_t psi = TMath::ATan2(track->GetPy(), track->GetPx())/degree;
       if (psi<0) psi+=360;

       if ( m_Mode == 4 ) {
          if (iftpc==1) {
              nTrkW++;
              m_pointFW->Fill(track->GetNumberOfPoints());
              m_ratiomFW->Fill(Float_t(track->GetNumberOfPoints())/Float_t(track->GetNMax()));
              m_xf_yfFW->Fill(firstPoint->GetX(),firstPoint->GetY());
              m_psiFW->Fill(psi);
           }
           if (iftpc==0) {
               nTrkE++;
               m_pointFE->Fill(track->GetNumberOfPoints());
               m_ratiomFE->Fill(Float_t(track->GetNumberOfPoints())/Float_t(track->GetNMax()));
               m_xf_yfFE->Fill(firstPoint->GetX(),firstPoint->GetY());
               m_psiFE->Fill(psi);
           }
        }   

//      m_nrec_track->Fill(track->GetNumberOfPoints(),track->GetP());
    
       for (Int_t h_counter = 0; h_counter < fhits->GetEntriesFast(); h_counter++) {
      
         StFtpcPoint *mhit = (StFtpcPoint *) fhits->At(h_counter);
      
         // Residuals (fill globals only, during tracking primary isn't filled anyway)
         if (mhit->GetUsage()) {
	   m_xres->Fill(mhit->GetXGlobResidual());
	   m_yres->Fill(mhit->GetYGlobResidual());
	   m_rres->Fill(mhit->GetRGlobResidual());
	   m_phires->Fill(mhit->GetPhiGlobResidual());
         }
      
         if (mhit->GetPadRow() <= StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide()) {
           if ( m_Mode == 4) m_pnt_padtimeFW->Fill(mhit->GetNumberBins(),mhit->GetNumberPads());
	   m_maxadc_West->Fill(mhit->GetMaxADC());
	   m_charge_West->Fill(mhit->GetCharge());
	
	   if (mhit->GetUsage()) {
	     m_rres_vs_r_west->Fill(mhit->GetRGlobResidual(), mhit->GetRadius());
	     m_phires_vs_r_west->Fill(mhit->GetPhiGlobResidual(), mhit->GetRadius());
	   }
         }

         else if (mhit->GetPadRow() > StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide()) {
           if ( m_Mode == 4)  m_pnt_padtimeFE->Fill(mhit->GetNumberBins(),mhit->GetNumberPads());
	   m_maxadc_East->Fill(mhit->GetMaxADC());
	   m_charge_East->Fill(mhit->GetCharge());
	
	   if (mhit->GetUsage()) {
	     m_rres_vs_r_east->Fill(mhit->GetRGlobResidual(), mhit->GetRadius());
	     m_phires_vs_r_east->Fill(mhit->GetPhiGlobResidual(), mhit->GetRadius());
	   }
         }
       } //end for h_counter
     } //end for t_counter
     if ( m_Mode == 4 ) m_good_trk->Fill(nTrkE,nTrkW);
  } //end IAttr(".histos")
}


//_____________________________________________________________________________
Int_t StFtpcTrackMaker::Finish()
{
  // final cleanup
  // nothing to do right now
	
  return StMaker::Finish();
}


//_____________________________________________________________________________
Int_t StFtpcTrackMaker::FinishRun(Int_t run)
{
  // cleanup after every run

//  delete StFtpcTrackingParams::Instance();

  return StMaker::FinishRun(run);
}


//_____________________________________________________________________________
void StFtpcTrackMaker::PrintInfo()
{
  // Prints information.
  
  LOG_INFO << "******************************************************************" << endm;
  LOG_INFO << "* $Id: StFtpcTrackMaker.cxx,v 1.93 2012/11/07 23:30:18 fisyak Exp $ *" << endm;
  LOG_INFO << "******************************************************************" << endm;
  
  if (Debug()) {
    StMaker::PrintInfo();
  }
}

