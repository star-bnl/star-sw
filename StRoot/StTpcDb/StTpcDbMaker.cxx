/***************************************************************************
 *
 * $Id: StTpcDbMaker.cxx,v 1.50 2009/12/07 23:44:58 fisyak Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description:  
 *This make initializes and controls the TPC interface to the database
 *
 ***************************************************************************
 *
 * $Log: StTpcDbMaker.cxx,v $
 * Revision 1.50  2009/12/07 23:44:58  fisyak
 * Drop coordinate transformation for fortran, remove TpcHitErr
 *
 * Revision 1.49  2009/11/23 16:50:27  fisyak
 * St_tpcAnodeHVavgC => St_tpcAnodeHVC, comment out exported for fortran coordinate transformations
 *
 * Revision 1.48  2009/11/06 13:41:31  fisyak
 * Revert the change done 11/03/09
 *
 * Revision 1.47  2009/11/02 17:31:41  fisyak
 * use directly field from StarMagField, replace St_tpcGainC and St_tpcT0C by St_tpcPadGainT0C, add remove defaults in coordinate transformations
 *
 * Revision 1.46  2009/08/11 20:38:04  genevb
 * slightly more detailed message
 *
 * Revision 1.45  2009/05/01 19:09:23  fisyak
 * StTpcDbMaker::Make is aware about TPC trips and generagte EoF when this happenss
 *
 * Revision 1.44  2008/09/10 15:46:37  fisyak
 * Recalculate Tpc drift velocity once per event, avoid expensive conversion to unix time
 *
 * Revision 1.43  2008/08/01 14:28:34  fisyak
 * Add new getT0, clean up
 *
 * Revision 1.42  2008/07/10 20:25:31  fisyak
 * Warn of
 *
 * Revision 1.41  2007/12/28 00:30:06  fine
 * Add a function to calculate the tpc coord transfoirmation in one step
 *
 * Revision 1.40  2007/08/04 00:38:04  jeromel
 * SL4 issue: Removal of the inline func, moved to class implementation.
 *     Symbols may otherwise be hidden.
 *
 * Revision 1.39  2007/07/12 20:21:09  fisyak
 * Drift velocity depends on TPC half, use online RHIC clock
 *
 * Revision 1.38  2007/04/28 17:57:19  perev
 * Redundant StChain.h removed
 *
 * Revision 1.37  2007/03/21 17:27:02  fisyak
 * use TGeoHMatrix, change mode for switching drift velocities
 *
 * Revision 1.36  2006/02/27 19:20:53  fisyak
 * Set simu flag for tpcISTimeOffsets and tpcOSTimeOffsets tables
 *
 * Revision 1.35  2005/03/30 17:56:59  fisyak
 * Fix a bug with flavor handling, StTpcDb has to be instantiated after setting flavor
 *
 * Revision 1.34  2004/10/27 21:45:13  fisyak
 * Add debug print for tables Validities, add access to ExB correction
 *
 * Revision 1.33  2004/06/05 23:38:22  fisyak
 * Add more chairs for TPC Db parameters
 *
 * Revision 1.32  2004/05/03 23:29:28  perev
 * WarnOff
 *
 * Revision 1.31  2003/04/10 21:30:59  hardtke
 * Allow multiple InitRun calls
 *
 * Revision 1.30  2003/01/12 20:38:23  jeromel
 * fabs() not abs() for doube
 *
 * Revision 1.29  2002/04/02 00:16:31  hardtke
 * New class that gets hit errors from database
 *
 * Revision 1.28  2002/02/12 22:50:35  hardtke
 * separate geometrical tpc rotation from field twist
 *
 * Revision 1.27  2002/02/05 22:21:08  hardtke
 * Move Init code to InitRun
 *
 * Revision 1.26  2002/01/03 00:01:09  hardtke
 * Add switches for type of drift velocity data (i.e. laser vs. t0 analysis).  Default to use either.
 *
 * Revision 1.25  2001/10/25 22:59:36  hardtke
 * Add function tpc_localsector_to_local
 *
 * Revision 1.24  2001/10/24 21:36:20  hardtke
 * Add sim flavor option
 *
 * Revision 1.23  2001/07/27 23:52:33  hardtke
 * use Global (magnet) coordinates
 *
 * Revision 1.22  2001/06/21 16:27:52  perev
 * two error matrix transformation methods added
 *
 * Revision 1.21  2001/06/19 23:07:13  hardtke
 * Restore to old functionality using Tpc Local Coordinates
 *
 * Revision 1.20  2001/04/19 19:52:48  hardtke
 * add tpc_pad_time_offset function and add ifdef for static arrays
 *
 * Revision 1.19  2000/11/14 22:00:06  genevb
 * Switched several functions from float to double
 *
 * Revision 1.18  2000/08/09 14:54:54  hardtke
 * Add Clear option, set trigger table pointer to 0 after each event
 *
 * Revision 1.17  2000/08/08 19:15:23  hardtke
 * use correct trigger time offset in case of laser
 *
 * Revision 1.16  2000/07/06 21:37:34  hardtke
 * speed up tpc_pad_to_x function
 *
 * Revision 1.15  2000/05/31 19:50:16  hardtke
 * speed up tpc_time_to_z and tpc_z_to_time by factor of 5
 *
 * Revision 1.14  2000/04/11 16:06:26  hardtke
 * improve speed of tpc_row_par and tpc_global_to_sector
 *
 * Revision 1.13  2000/02/24 18:21:51  hardtke
 * re-define drift distance as central membrane to gating grid
 *
 * Revision 1.12  2000/02/23 22:21:09  hardtke
 * add tpc_global_to_local_p
 *
 * Revision 1.11  2000/02/23 21:03:17  hardtke
 * fix tpc_row_par -- causing tpt problems
 *
 * Revision 1.10  2000/02/23 15:09:57  hardtke
 * move tpg_detector and tpg_pad_plane from .const to .data
 *
 * Revision 1.9  2000/02/22 19:40:30  hardtke
 * fix tpc_row_par to give expected results
 *
 * Revision 1.8  2000/02/17 19:43:20  hardtke
 * fixes to tpc functions
 *
 * Revision 1.7  2000/02/10 00:29:09  hardtke
 * Add tpg functions to StTpcDbMaker, fix a few bugs
 *
 * Revision 1.6  2000/01/11 15:49:53  hardtke
 * get Electronics table from Calibrations database, Fix error messages
 *
 * Revision 1.5  1999/12/16 22:00:53  hardtke
 * add CVS tags
 *
 **************************************************************************/

#define StTpc_STATIC_ARRAYS
#include <assert.h>
#include "StTpcDbMaker.h"
#include "StTpcDb.h"
#include "StDbUtilities/StCoordinates.hh"
#include "StDbUtilities/StTpcPadCoordinate.hh"
#include "tables/St_tpg_pad_plane_Table.h"
#include "tables/St_tpg_detector_Table.h"
#include "StarMagField.h"
#include "math_constants.h"
#include "StDetectorDbMaker/StDetectorDbTpcRDOMasks.h"
#include "StDetectorDbMaker/StDetectorDbMagnet.h"
#include "StDetectorDbMaker/St_tpcAnodeHVC.h"
#include "StDetectorDbMaker/St_tpcPadGainT0C.h"
#if ROOT_VERSION_CODE < 331013
#include "TCL.h"
#else
#include "TCernLib.h"
#endif
ClassImp(StTpcDbMaker)
//_____________________________________________________________________________
StTpcDbMaker::StTpcDbMaker(const char *name): StMaker(name), m_TpcDb(0), m_tpg_pad_plane(0), m_tpg_detector(0) {}
//_____________________________________________________________________________
StTpcDbMaker::~StTpcDbMaker(){
  //delete m_TpcDb;
}
//_____________________________________________________________________________
Int_t StTpcDbMaker::Init(){


   return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StTpcDbMaker::InitRun(int runnumber){
    if (m_TpcDb) return 0;
    // Create Needed Tables:    
    Float_t gFactor = StarMagField::Instance()->GetFactor();
  // Set Table Flavors
   if (m_Mode%1000000==1){
     const Char_t *tabNames[5] = {"tpcGlobalPosition","tpcSectorPosition", "tpcISTimeOffsets", 
				  "tpcOSTimeOffsets","starClockOnl"};
     for (Int_t i = 0; i < 5; i++) {
       SetFlavor("sim",tabNames[i]); 
       gMessMgr->Info()  << "StTpcDbMaker::Setting Sim Flavor tag for table " << "\t" << tabNames[i] << endm;
     }
   }
   if ((m_Mode/1000000)%10==0) {
   SetFlavor("ofl+laserDV","tpcDriftVelocity");
   gMessMgr->Info() << "StTpcDbMaker::Using any drift velocity" << endm;
   }
   else if ((m_Mode/1000000)%10==1) {
   SetFlavor("ofl","tpcDriftVelocity");
   gMessMgr->Info() << "StTpcDbMaker::Using drift velocity from T0 analysis" << endm;
   }
   else if ((m_Mode/1000000)%10==2) {
   SetFlavor("laserDV","tpcDriftVelocity");
   gMessMgr->Info() << "StTpcDbMaker::Using drift velocity from laser analysis" << endm;
   }
   else if ((m_Mode/1000000)%10==3) {
   SetFlavor("NewlaserDV","tpcDriftVelocity");
   gMessMgr->Info() << "StTpcDbMaker::Using drift velocity from New laser analysis" << endm;
   }
   else {
     gMessMgr->Info() << "StTpcDbMaker::Undefined drift velocity flavor requested" << endm;
   }
   
 
//
  if (m_Mode%1000000 != 1){
   if (gFactor<-0.8) {
     gMessMgr->Info() << "StTpcDbMaker::Full Reverse Field Twist Parameters.  If this is an embedding run, you should not use it." << endm;
     SetFlavor("FullMagFNegative","tpcGlobalPosition");
   }
   else if (gFactor<-0.2) {
     gMessMgr->Info() << "StTpcDbMaker::Half Reverse Field Twist Parameters.  If this is an embedding run, you should not use it." << endm;
     SetFlavor("HalfMagFNegative","tpcGlobalPosition");
   }
   else if (gFactor<0.2) {
     gMessMgr->Info() << "StTpcDbMaker::Zero Field Twist Parameters.  If this is an embedding run, you should not use it." << endm;
     SetFlavor("ZeroMagF","tpcGlobalPosition");
   }
   else if (gFactor<0.8) {
     gMessMgr->Info() << "StTpcDbMaker::Half Forward Field Twist Parameters.  If this is an embedding run, you should not use it." << endm;
     SetFlavor("HalfMagFPositive","tpcGlobalPosition");
   }
   else if (gFactor<1.2) {
     gMessMgr->Info() << "StTpcDbMaker::Full Forward Field Twist Parameters.  If this is an embedding run, you should not use it." << endm;
     SetFlavor("FullMagFPositive","tpcGlobalPosition");
   }
  }

  m_TpcDb = new StTpcDb(this); if (Debug()) m_TpcDb->SetDebug(Debug());
  if (m_Mode%1000000 & 2) {
    Int_t option = (m_Mode & 0xfffc) >> 2;
    StMagUtilities *magU = new StMagUtilities(gStTpcDb, 0, option);
#if 0
    magU->SetMagFactor(gFactor);
#endif
    m_TpcDb->SetExB(magU);
    
  }
  m_TpcDb->SetDriftVelocity();
  m_tpg_pad_plane = new St_tpg_pad_plane("tpg_pad_plane",1);
  m_tpg_detector = new St_tpg_detector("tpg_detector",1);
  AddConst(m_tpg_pad_plane);
  AddConst(m_tpg_detector);
  if (tpcDbInterface()->PadPlaneGeometry()&&tpcDbInterface()->Dimensions())
   Update_tpg_pad_plane();
  if (tpcDbInterface()->Electronics()&&tpcDbInterface()->Dimensions()&&
      tpcDbInterface()->DriftVelocity()) 
   Update_tpg_detector();
  //Here I fill in the arrays for the row parameterization ax+by=1
  if (m_TpcDb->GlobalPosition()) {
    SetTpc2Global();
    for (int i=0;i<24;i++){
      for (int j=0;j<45;j++){
	int time[1] = {10}; 
	int ipad[2] = {20,40};
	StTpcPadCoordinate pad1(i+1, j+1, ipad[0], *time);
	StTpcPadCoordinate pad2(i+1, j+1, ipad[1], *time);
	StGlobalCoordinate gc1,gc2;
	StTpcCoordinateTransform transform(gStTpcDb);
	transform(pad1,gc1);
	transform(pad2,gc2);
	double x1,y1,x2,y2;
	double m,bb; // y = mx + bb
	x1 = gc1.position().x();
	y1 = gc1.position().y();
	x2 = gc2.position().x();
	y2 = gc2.position().y();
	if (fabs(x2-x1)<0.000001) {
	  aline[i][j] = 1/x1;
	  bline[i][j] = 0.;
	  continue;
	}
	m = (y2 - y1)/(x2 - x1);
	bb = y1 - m*x1;
	if (bb == 0) {
	  gMessMgr->Warning() << "StTpcDbMaker::Init() Row intersects 0,0" << endm;
	  continue;
	}
	aline[i][j] = (float) -m/bb;
	bline[i][j] = (float) 1.0/bb;
      }
    }
  }
  return 0;
}
//_____________________________________________________________________________
Int_t StTpcDbMaker::Make(){
  // check that TPC is tripped 
  if (St_tpcAnodeHVC::instance()->tripped()) {
    gMessMgr->Info() << "StTpcDbMaker::TPC has tripped - declaring EOF to avoid possibly bad data" << endm;
    return kStEOF;
  }
  if (!m_TpcDb) m_TpcDb = new StTpcDb(this);
  m_TpcDb->SetDriftVelocity();
  if (tpcDbInterface()->PadPlaneGeometry()&&tpcDbInterface()->Dimensions())
    Update_tpg_pad_plane();
  if (tpcDbInterface()->Electronics()&&tpcDbInterface()->Dimensions()&&
      tpcDbInterface()->DriftVelocity()) 
    Update_tpg_detector();
  SetTpc2Global();
  return kStOK;
}

//---------------------------------------------------------------------------
void StTpcDbMaker::Clear(const char *opt){
  if (m_TpcDb) m_TpcDb->Clear();
}

//_____________________________________________________________________________
void StTpcDbMaker::Update_tpg_pad_plane(){
  if (m_tpg_pad_plane) {
    tpg_pad_plane_st pp;
    memset(&pp, 0, sizeof(tpg_pad_plane_st));
    pp.nrow_in = tpcDbInterface()->PadPlaneGeometry()->numberOfInnerRows();
    pp.nrow_out = tpcDbInterface()->PadPlaneGeometry()->numberOfOuterRows();
    pp.pad_len_in = tpcDbInterface()->PadPlaneGeometry()->innerSectorPadLength();
    pp.pad_len_out = tpcDbInterface()->PadPlaneGeometry()->outerSectorPadLength();
    pp.pad_sep_in = tpcDbInterface()->PadPlaneGeometry()->innerSectorPadPitch();
    pp.pad_sep_out = tpcDbInterface()->PadPlaneGeometry()->outerSectorPadPitch();
    pp.pad_wid_in = tpcDbInterface()->PadPlaneGeometry()->innerSectorPadWidth();
    pp.pad_wid_out = tpcDbInterface()->PadPlaneGeometry()->outerSectorPadWidth();
    pp.nsect = tpcDbInterface()->Dimensions()->numberOfSectors();
    for (int i=1;i<=tpcDbInterface()->PadPlaneGeometry()->numberOfRows();i++){
      pp.npads[i-1] = tpcDbInterface()->PadPlaneGeometry()->numberOfPadsAtRow(i);
      pp.rad[i-1] = tpcDbInterface()->PadPlaneGeometry()->radialDistanceAtRow(i);
    }
    m_tpg_pad_plane->AddAt(&pp,0);
  }
}

//_____________________________________________________________________________
void StTpcDbMaker::Update_tpg_detector(){
 if (m_tpg_detector) {
   tpg_detector_st pp;
   memset(&pp, 0, sizeof(tpg_detector_st));
   pp.nsectors = 2*tpcDbInterface()->Dimensions()->numberOfSectors();
   // note tpg table define number of sectors as 48
   pp.drift_length = tpcDbInterface()->Dimensions()->outerEffectiveDriftDistance();
   pp.clock_frequency = 1e6*tpcDbInterface()->Electronics()->samplingFrequency();
   pp.z_inner_offset = 
     tpcDbInterface()->Dimensions()->innerEffectiveDriftDistance() - 
     tpcDbInterface()->Dimensions()->outerEffectiveDriftDistance();
   pp.vdrift = tpcDbInterface()->DriftVelocity();
   m_tpg_detector->AddAt(&pp,0);
 }
}
//_____________________________________________________________________________
void StTpcDbMaker::SetTpc2Global() {
  Double_t phi   = 0.0;  //large uncertainty, so set to 0
  Double_t theta = 0.0;
  Double_t psi   = 0.0;
  Double_t xyz[3] = {0,0,0};
  if (m_TpcDb->GlobalPosition()) {
    theta = m_TpcDb->GlobalPosition()->TpcRotationAroundGlobalAxisY()*180./TMath::Pi();
    psi   = m_TpcDb->GlobalPosition()->TpcRotationAroundGlobalAxisX()*180./TMath::Pi();
    xyz[0] =  m_TpcDb->GlobalPosition()->TpcCenterPositionX();
    xyz[1] =  m_TpcDb->GlobalPosition()->TpcCenterPositionY();
    xyz[2] =  m_TpcDb->GlobalPosition()->TpcCenterPositionZ();
  };
  TGeoHMatrix Tpc2Global("Tpc2Global");
  Tpc2Global.RotateX(-psi);
  Tpc2Global.RotateY(-theta);
  Tpc2Global.RotateZ(-phi);
  Tpc2Global.SetTranslation(xyz);
  m_TpcDb->SetTpc2GlobalMatrix(&Tpc2Global);
}


StTpcDb* StTpcDbMaker::tpcDbInterface() const {return m_TpcDb;}


