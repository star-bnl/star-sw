
/***************************************************************************
 *
 * $Id: StTpcDb.cxx,v 1.68 2018/09/06 14:21:13 genevb Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description: This is the interface between the database and the offline
 *              TPC software.  This classes takes care of the annoying
 *              calls to the root infrastucture, packages and manipulates
 *              the data, and returns the data to the user via simple
 *              interface classes.    
 *
 ***************************************************************************
 *
 * $Log: StTpcDb.cxx,v $
 * Revision 1.68  2018/09/06 14:21:13  genevb
 * SafeDelete requires class definition, not just declaration
 *
 * Revision 1.67  2018/07/06 22:13:16  smirnovd
 * [Cosmetic] Remove unused variables and commented code
 *
 * Revision 1.66  2018/06/29 21:46:21  smirnovd
 * Revert iTPC-related changes committed on 2018-06-20 through 2018-06-28
 *
 * Revert "NoDead option added"
 * Revert "Fill mag field more carefully"
 * Revert "Assert commented out"
 * Revert "Merging with TPC group code"
 * Revert "Remove too strong assert"
 * Revert "Restore removed by mistake line"
 * Revert "Remove not used anymore file"
 * Revert "iTPCheckIn"
 *
 * Revision 1.64  2018/04/11 02:43:22  smirnovd
 * Enable TPC/iTPC switch via St_tpcPadConfig
 *
 * This is accomplished by substituting St_tpcPadPlanes with St_tpcPadConfig.
 * A sector ID is passed to St_tpcPadConfig in order to extract parameters for
 * either TPC or iTPC
 *
 * Revision 1.63  2015/05/17 22:53:52  fisyak
 * Remove duplicted line
 *
 * Revision 1.62  2014/07/01 20:28:32  fisyak
 * Add alternative (B) table for new TPC alignment
 *
 * Revision 1.61  2014/06/27 14:04:25  fisyak
 * Add env. NewTpcAlignment to switch between new and old scheme
 *
 * Revision 1.60  2014/06/26 21:32:57  fisyak
 * New Tpc Alignment, v632
 *
 * Revision 1.59  2012/09/17 19:39:44  fisyak
 * Add rotation for Half Tpc's
 *
 * Revision 1.58  2012/05/03 23:56:48  fisyak
 * Set interpolation for one week only, fix sign of interpolation (thanks Gene), add TriggerId
 *
 * Revision 1.57  2011/07/21 16:48:53  fisyak
 * New schema for Sub Sector Alginement: SuperSectror position (defined by inner sub sector) and Outer sector position wrt SuperSectror position
 *
 * Revision 1.56  2011/01/18 14:39:43  fisyak
 * Clean up TpcDb interfaces and Tpc coordinate transformation
 *
 * Revision 1.55  2010/05/27 19:14:26  fisyak
 * Take out flavoring by 'sim' for tpcGlobalPosition,tpcSectorPosition and starClockOnl tables. remove usage tpcISTimeOffsets and tpcOSTimeOffsets tables
 *
 * Revision 1.54  2010/01/27 21:30:39  perev
 * GetValidity now is static
 *
 * Revision 1.53  2010/01/26 21:04:42  fisyak
 * Add new dE/dx calibration tables: TpcRowQ, tpcMethaneIn, tpcWaterOut, TpcZDC
 *
 * Revision 1.52  2009/12/07 23:44:58  fisyak
 * Drop coordinate transformation for fortran, remove TpcHitErr
 *
 * Revision 1.51  2009/11/02 17:31:41  fisyak
 * use directly field from StarMagField, replace St_tpcGainC and St_tpcT0C by St_tpcPadGainT0C, add remove defaults in coordinate transformations
 *
 * Revision 1.50  2009/03/16 14:13:30  fisyak
 * Use StDetectorDb chairs for TpcGlobalPosition and TpcSectorPosition
 *
 * Revision 1.49  2008/09/10 15:46:36  fisyak
 * Recalculate Tpc drift velocity once per event, avoid expensive conversion to unix time
 *
 * Revision 1.48  2008/08/01 14:28:22  fisyak
 * Add new getT0, clean up
 *
 * Revision 1.47  2007/10/29 21:37:27  fisyak
 * add protection from laserDriftVelocity and cathodeDriftVelocity mixing
 *
 * Revision 1.46  2007/08/12 15:06:30  fisyak
 * Use separated East/West drift velocities only >= 2007, for back compartibility
 *
 * Revision 1.45  2007/07/19 22:19:23  perev
 * Bug in drift velocity fixed
 *
 * Revision 1.44  2007/07/12 20:21:09  fisyak
 * Drift velocity depends on TPC half, use online RHIC clock
 *
 * Revision 1.43  2007/04/16 22:51:03  fisyak
 * Add protection from infinit endTime
 *
 * Revision 1.42  2007/04/15 20:57:01  fisyak
 * Add drift velocity interpolation between two measurement in time
 *
 * Revision 1.41  2007/03/21 17:27:01  fisyak
 * use TGeoHMatrix, change mode for switching drift velocities
 *
 * Revision 1.40  2005/07/06 22:26:53  fisyak
 * dEdx_t=>dEdxY2_t
 *
 * Revision 1.39  2005/03/30 17:56:59  fisyak
 * Fix a bug with flavor handling, StTpcDb has to be instantiated after setting flavor
 *
 * Revision 1.38  2004/11/19 10:21:54  jecc
 * Initialize pointers
 *
 * Revision 1.37  2004/10/27 21:44:28  fisyak
 * Add debug print for tables Validities, add access to ExB correction
 *
 * Revision 1.36  2004/03/16 22:17:46  jecc
 * Update triggerTimeOffset() due to a change in L0 TriggerActionWd
 *
 * Revision 1.35  2004/02/23 00:35:00  fisyak
 * Add access to tpcPadResponse
 *
 * Revision 1.34  2004/01/14 22:54:30  fisyak
 * Add hooks for Pedestal and tpcGain
 *
 * Revision 1.33  2002/04/02 00:16:30  hardtke
 * New class that gets hit errors from database
 *
 * Revision 1.32  2002/02/06 18:39:13  hardtke
 * Add tpc Field Cage structure
 *
 * Revision 1.31  2001/08/14 18:18:03  hardtke
 * Add sector position structures
 *
 * Revision 1.30  2001/06/20 22:25:26  hardtke
 * Get TRS gain parameters from tsspar table
 *
 * Revision 1.29  2001/05/21 23:25:34  hardtke
 * Add tpcGlobalPosition to StTpcDb.  This includes the global position offset and the rotation w.r.t. the magnet
 *
 * Revision 1.28  2000/08/18 17:19:21  hardtke
 * use laser velocity, if available
 *
 * Revision 1.27  2000/08/10 18:41:34  hardtke
 * only look for L0_trigger table once per event -- improves timing
 *
 * Revision 1.26  2000/08/09 14:54:54  hardtke
 * Add Clear option, set trigger table pointer to 0 after each event
 *
 * Revision 1.25  2000/08/09 13:00:03  hardtke
 * Add protections to make sure trigger table is filled before using
 *
 * Revision 1.24  2000/08/08 19:15:22  hardtke
 * use correct trigger time offset in case of laser
 *
 * Revision 1.23  2000/08/04 21:03:55  perev
 * Leaks + Clear() cleanup
 *
 * Revision 1.22  2000/05/12 20:31:38  fisyak
 * Add ClassImp for abstract classes, new rootcint requires them
 *
 * Revision 1.21  2000/05/11 17:17:27  hardtke
 * make trigger time offset available -- currently NOT different for beam and laser events
 *
 * Revision 1.20  2000/04/05 15:44:56  hardtke
 * fix solaris bug -- char* was too short for table name
 *
 * Revision 1.19  2000/03/30 17:02:36  hardtke
 * limit warning message in StRTpcPadPlane
 *
 * Revision 1.18  2000/02/23 21:03:17  hardtke
 * fix tpc_row_par -- causing tpt problems
 *
 * Revision 1.17  2000/02/15 22:21:47  hardtke
 * Add effective drift distances
 *
 * Revision 1.16  2000/02/10 00:29:08  hardtke
 * Add tpg functions to StTpcDbMaker, fix a few bugs
 *
 * Revision 1.15  2000/01/24 15:31:31  hardtke
 * change to use new gain and t0 tables
 *
 * Revision 1.14  2000/01/11 15:49:52  hardtke
 * get Electronics table from Calibrations database, Fix error messages
 *
 * Revision 1.13  1999/12/16 22:00:53  hardtke
 * add CVS tags
 *
 **************************************************************************/
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StTpcDb                                                              //
//                                                                      //
//                                                                      //
#include "StChain.h"
#include "StTpcDb.h"
#include "tables/St_trgTimeOffset_Table.h"
#include "tables/St_dst_L0_Trigger_Table.h"
#include "TUnixTime.h"
#include "StMessMgr.h"
#include "St_db_Maker/St_db_Maker.h"
#include "TVector3.h"
#include "TGeoManager.h"
#include "StDetectorDbMaker/StTpcSurveyC.h"
#include "StDetectorDbMaker/St_tpcDriftVelocityC.h"
#include "StarMagField.h"
#include "StDbUtilities/StMagUtilities.h"
#include "TEnv.h"
StTpcDb* gStTpcDb = 0;
Bool_t StTpcDb::mOldScheme = kTRUE;
// C++ routines:
//_____________________________________________________________________________
ClassImp(StTpcDb);
//_____________________________________________________________________________
StTpcDb::StTpcDb() {
  assert(gStTpcDb==0);
  memset(mBeg,0,mEnd-mBeg+1);
  mTpc2GlobMatrix = new TGeoHMatrix("Default Tpc2Glob"); 
  for (Int_t i = 1; i <= 24; i++) {
    for (Int_t k = 0; k < kTotalTpcSectorRotaions; k++) {
      mTpcSectorRotations[i-1][k] = new TGeoHMatrix(Form("Default %02i %i",i,k));
    }
  }
  mFlip = new TGeoHMatrix;
  mzGG = Dimensions()->gatingGridZ(); // zGG
  Double_t Rotation[9] = {0, 1, 0, 
			  1, 0, 0,
			  0, 0,-1};
  //  Double_t Translation[3] = {0, 0, mzGG};
  mFlip->SetName("Flip"); mFlip->SetRotation(Rotation);// mFlip->SetTranslation(Translation);
  mSwap[0] = new TGeoTranslation("Signed Drift distance to z for East", 0, 0, -mzGG);
  mSwap[1] = new TGeoTranslation("Signed Drift distance to z for West", 0, 0,  mzGG);
  mHalf[0] = new TGeoHMatrix("Default for east part of TPC");
  mHalf[1] = new TGeoHMatrix("Default for west part of TPC");
  gStTpcDb = this;
}
//_____________________________________________________________________________
//_____________________________________________________________________________
StTpcDb::~StTpcDb() {
  for (Int_t i = 0;i<24;i++) {
    for (Int_t k = 0; k < kTotalTpcSectorRotaions; k++) 
    SafeDelete(mTpcSectorRotations[i][k]);
  }
  SafeDelete(mHalf[0]);  
  SafeDelete(mHalf[1]);
  SafeDelete(mSwap[0]);  
  SafeDelete(mSwap[1]);
  SafeDelete(mExB);
  SafeDelete(mTpc2GlobMatrix);
  SafeDelete(mFlip);
  gStTpcDb = 0;
}
#if 0
//________________________________________________________________________________
Float_t StTpcDb::ScaleY() {return St_tpcDriftVelocityC::instance()->scaleY();}
//-----------------------------------------------------------------------------
float StTpcDb::DriftVelocity(Int_t sector, Double_t Y) {
  static UInt_t u2007 = TUnixTime(20070101,0,1).GetUTime(); // 
  assert(mUc > 0);
  if (mUc < u2007) sector = 24;
  UInt_t kase = 1;
  if (sector <= 12) kase = 0;
  return 1e6*mDriftVel[kase]*(1 + ScaleY()*Y);
}
#else
//-----------------------------------------------------------------------------
float StTpcDb::DriftVelocity(Int_t sector) {
  static UInt_t u2007 = TUnixTime(20070101,0,1).GetUTime(); // 
  assert(mUc > 0);
  if (mUc < u2007) sector = 24;
  UInt_t kase = 1;
  if (sector <= 12) kase = 0;
  return 1e6*mDriftVel[kase];
}
#endif
//-----------------------------------------------------------------------------
void StTpcDb::SetDriftVelocity() {
  static UInt_t u0 = 0; // beginTime of current Table
  static UInt_t u1 = 0; // beginTime for next Table
  static UInt_t umax = TUnixTime(20250101,0,1).GetUTime(); // maximum time allowed for next table
  // for back compartiblity switch to separated West and East drift velocities after 2007
  static St_tpcDriftVelocity *dvel0 = 0;
  static St_tpcDriftVelocity *dvel1 = 0;
  static TDatime t[2];
  UInt_t uc = TUnixTime(StMaker::GetChain()->GetDateTime(),1).GetUTime();
  if (uc != mUc) {
    if (! dvel0 || (uc < umax && ((uc < u0) || (uc > u1)))) {//First time only
      dvel0 = (St_tpcDriftVelocity *) St_tpcDriftVelocityC::instance()->Table();
      if (! dvel0) {
	gMessMgr->Message("StTpcDb::Error Finding Tpc DriftVelocity","E");
	mUc = 0;
 	return;
      }
      if (St_db_Maker::GetValidity(dvel0,t) < 0) {
	gMessMgr->Message("StTpcDb::Error Wrong Validity Tpc DriftVelocity","E");
	mUc = 0;
	return;
      }
      u0 = TUnixTime(t[0],1).GetUTime();
      u1 = TUnixTime(t[1],1).GetUTime();
      SafeDelete(dvel1);
      if (u1 < umax && u1 - u0 <  7*24*3600) // do not extrapolate for more than 1 week 
	dvel1 = (St_tpcDriftVelocity *) StMaker::GetChain()->GetDataBase("Calibrations/tpc/tpcDriftVelocity",&t[1]);
    }//End First time only
    
    if (!(u0<=uc && uc<u1)) {//current time out of validity
      
      SafeDelete(dvel1);
      if (u1 < umax && u1 - u0 < 7*24*3600 && uc - u0 < 7*24*3600) {// next drift velocity should within a week from current
	dvel1 = (St_tpcDriftVelocity *) StMaker::GetChain()->GetDataBase("Calibrations/tpc/tpcDriftVelocity",&t[1]);
	if (! dvel1) {
	  gMessMgr->Message("StTpcDb::Error Finding next Tpc DriftVelocity","W");
	}
      }
    }
    
    mDriftVel[0] = mDriftVel[1] = 0;
    tpcDriftVelocity_st *d0 = dvel0->GetTable();
    if (dvel1) {
      tpcDriftVelocity_st *d1 = dvel1->GetTable();
      if (d0->laserDriftVelocityWest > 0 && d1->laserDriftVelocityWest > 0)
	mDriftVel[0] = (d1->laserDriftVelocityWest  *(uc-u0) + d0->laserDriftVelocityWest  *(u1-uc))/(u1 - u0);
      if (d0->laserDriftVelocityEast > 0 && d1->laserDriftVelocityEast > 0) 
	mDriftVel[1] = (d1->laserDriftVelocityEast  *(uc-u0) + d0->laserDriftVelocityEast  *(u1-uc))/(u1 - u0);
      if (mDriftVel[0] <= 0.0 || mDriftVel[1] <= 0.0) {
	if (d0->cathodeDriftVelocityWest > 0 && d1->cathodeDriftVelocityWest > 0) 
	  mDriftVel[0] = (d1->cathodeDriftVelocityWest*(uc-u0) + d0->cathodeDriftVelocityWest*(u1-uc))/(u1 - u0);
	if (d0->cathodeDriftVelocityEast > 0 && d1->cathodeDriftVelocityEast > 0) 
	  mDriftVel[1] = (d1->cathodeDriftVelocityEast*(uc-u0) + d0->cathodeDriftVelocityEast*(u1-uc))/(u1 - u0);
      }
    }
    if (mDriftVel[0] <= 0.0 || mDriftVel[1] <= 0.0) {
      mDriftVel[0] = d0->laserDriftVelocityWest;
      mDriftVel[1] = d0->laserDriftVelocityEast;
      if (mDriftVel[0] <= 0.0) mDriftVel[0] = d0->cathodeDriftVelocityWest;
      if (mDriftVel[1] <= 0.0) mDriftVel[1] = d0->cathodeDriftVelocityEast;
    }
#if 0
    LOG_INFO << "Set Tpc Drift Velocity =" << mDriftVel[0]  << " (West) " << mDriftVel[0] << " (East) for "
	     << StMaker::GetChain()->GetDateTime().AsString() << endm;
#endif
    mUc = uc;
  }
}
//_____________________________________________________________________________
void StTpcDb::SetTpcRotations() {
  // Pad [== sector12 == localsector (SecL, ideal)] => subsector (SubS,local sector aligned) => flip => sector (SupS) => tpc => global
  //                                    ------        
  //old:  global = Tpc2GlobalMatrix() * SupS2Tpc(sector) *                                    Flip() * {SubSInner2SupS(sector) | SubSOuter2SupS(sector)}
  //new:  global = Tpc2GlobalMatrix() * SupS2Tpc(sector) * StTpcSuperSectorPosition(sector) * Flip() * {                     I | StTpcOuterSectorPosition(sector)}
  //      StTpcSuperSectorPosition(sector) * Flip() = Flip() * SubSInner2SupS(sector) 
  // =>  StTpcSuperSectorPosition(sector) = Flip() * SubSInner2SupS(sector) * Flip()^-1
  //      StTpcSuperSectorPosition(sector) * Flip() * StTpcOuterSectorPosition(sector) = Flip() *  SubSOuter2SupS(sector)
  // =>  StTpcOuterSectorPosition(sector) = Flip()^-1 * StTpcSuperSectorPosition(sector)^-1 *  Flip() *  SubSOuter2SupS(sector)
  /*
    .                                                                                             <-- the system of coordinate where Outer to Inner Alignment done -->
    global = Tpc2GlobalMatrix() * SupS2Tpc(sector) * StTpcSuperSectorPosition(sector) * Flip() * {                     I | StTpcOuterSectorPosition(sector)} * local
    .                                                result of super sector alignment                                      result of Outer to Inner sub sector alignment
  */
  /* 03/07/14
     StTpcPadCoordinate P(sector,row,pad,timebacket);
     x = xFromPad()
     z = zFromTB() - drift distance
     StTpcLocalSectorCoordinate LS(position(x,y,z),sector,row);
     
     StTpcLocalCoordinate  Tpc(position(x,y,z),sector,row);
     Pad2Tpc(sector,row).LocalToMaster(LS.position().xyz(), Tpc.postion().xyz())
     Flip transformation from Pad Coordinate system (xFromPad(pad), yFromRow(row), DriftDistance(timebacket)) => (y, x, -z): local sector CS => super sectoe CS 



             (0 1  0 0  ) ( x )    ( y )
     Flip ;  (1 0  0 0  ) ( y ) =  ( x )
             (0 0 -1 zGG) ( z )    ( zGG - z)
             (0 0  0 1  ) ( 1 )    ( 1 )
     Z_tpc is not changed during any sector transformation  !!!


   */
  //  TGeoTranslation T123(0,123,0); T123.SetName("T123"); if (Debug() > 1) T123.Print();
  assert(Dimensions()->numberOfSectors() == 24);
  Float_t gFactor = StarMagField::Instance()->GetFactor();
  Double_t phi, theta, psi;
  Int_t iphi;
  TGeoRotation *rotm = 0;
  TObjArray *listOfMatrices = 0;
  TString Rot;
  if (gEnv->GetValue("NewTpcAlignment",0) != 0) mOldScheme = kFALSE;
  if (! mOldScheme) {
    LOG_INFO << "StTpcDb::SetTpcRotations use new schema for Rotation matrices" << endm;
  } else {
    LOG_INFO << "StTpcDb::SetTpcRotations use old schema for Rotation matrices" << endm;
  }
  for (Int_t sector = 0; sector <= 24; sector++) {// loop over Tpc as whole, sectors, inner and outer subsectors
    Int_t k;
    Int_t k1 = kSupS2Tpc;
    Int_t k2 = kTotalTpcSectorRotaions;
    if (sector == 0) {k2 = k1; k1 = kUndefSector;}
    for (k = k1; k < k2; k++) {
      Int_t Id     = 0;
      TGeoHMatrix rotA; // After alignment
      if (!sector ) { // TPC Reference System
	if (mOldScheme) { // old scheme
	  St_tpcGlobalPositionC *tpcGlobalPosition = St_tpcGlobalPositionC::instance();
	  assert(tpcGlobalPosition);
	  Id = 1;
	  phi   = 0.0;                                               // -gamma large uncertainty, so set to 0
	  theta = tpcGlobalPosition->PhiXZ_geom()*TMath::RadToDeg(); // -beta
	  psi   = tpcGlobalPosition->PhiYZ_geom()*TMath::RadToDeg(); // -alpha
	  rotA.RotateX(-psi);
	  rotA.RotateY(-theta);
	  rotA.RotateZ(-phi);
	  Double_t transTpcRefSys[3] = {tpcGlobalPosition->LocalxShift(),
					tpcGlobalPosition->LocalyShift(),
					tpcGlobalPosition->LocalzShift()};
	  rotA.SetTranslation(transTpcRefSys);
	} else {
	  rotA = StTpcPosition::instance()->GetMatrix();
	  *mHalf[east] = StTpcHalfPosition::instance()->GetEastMatrix();
	  *mHalf[west] = StTpcHalfPosition::instance()->GetWestMatrix();
	}
      } else {
	Id = 10*sector + k;
	StBeamDirection part = east;
	if (sector <= 12) part = west;
	switch (k) {
	case kSupS2Tpc: // SupS => Tpc
	  if (sector <= 12) {iphi = (360 + 90 - 30* sector      )%360; Rot = Form("R%03i",iphi);}
	  else              {iphi = (      90 + 30*(sector - 12))%360; Rot = Form("Y%03i",iphi);}
	  rotm = 0;
	  if (gGeoManager) {
	    listOfMatrices =  gGeoManager->GetListOfMatrices();
	    rotm = (TGeoRotation *) listOfMatrices->FindObject(Rot);
	  }
	  if (! rotm) {
	    if (sector <= 12) rotm = new TGeoRotation(Rot);
	    else              rotm = new TGeoRotation(Rot,   90.0,    0.0,  90.0,  -90.0,  180.0,    0.00); // Flip (x,y,z) => ( x,-y,-z)
	    rotm->RotateZ(iphi);
	  }
	  rotA = (*mSwap[part]) * (*mHalf[part]) * (*rotm);
	  rotA *= StTpcSuperSectorPosition::instance()->GetMatrix(sector-1);
	  if (gGeoManager) rotm->RegisterYourself();
	  else             SafeDelete(rotm);
	  break;
	case kSupS2Glob:      // SupS => Tpc => Glob
	  rotA = Tpc2GlobalMatrix() * SupS2Tpc(sector); 
	  break; 
	case kSubSInner2SupS: 
	  if (mOldScheme) 	  rotA = Flip(); 
	  else                    rotA = Flip() * StTpcInnerSectorPosition::instance()->GetMatrix(sector-1); 
	  break;
	case kSubSOuter2SupS: 
	  if (mOldScheme) rotA = Flip() * StTpcOuterSectorPosition::instance()->GetMatrix(sector-1); 
	  else           {
	    rotA = Flip() * StTpcOuterSectorPosition::instance()->GetMatrix(sector-1); 
	    if (StTpcOuterSectorPosition::instance()->GetNRows() > 24) {
	      if (gFactor > 0.2) {
		rotA *= StTpcOuterSectorPosition::instance()->GetMatrix(sector-1+24);
	      } else if (gFactor < -0.2) {
		rotA *= StTpcOuterSectorPosition::instance()->GetMatrix(sector-1+24).Inverse();
	      }
	    }
	  }
	  break;
	case kSubSInner2Tpc:  rotA = SupS2Tpc(sector) * SubSInner2SupS(sector); break; // (Subs[io] => SupS) => Tpc
	case kSubSOuter2Tpc:  rotA = SupS2Tpc(sector) * SubSOuter2SupS(sector); break; // -"-

	case kSubSInner2Glob: rotA = Tpc2GlobalMatrix() * SubSInner2Tpc(sector);  break; // Subs[io] => SupS => Tpc) => Glob
	case kSubSOuter2Glob: rotA = Tpc2GlobalMatrix() * SubSOuter2Tpc(sector);  break; // -"-

	case kPadInner2SupS:  rotA = SubSInner2SupS(sector); break; // (Pad == SecL) => (SubS[io] => SupS)
	case kPadOuter2SupS:  rotA = SubSOuter2SupS(sector); break; // -"-
	case kPadInner2Tpc:   rotA = SupS2Tpc(sector) * PadInner2SupS(sector); break; // (Pad == SecL) => (SubS[io] => SupS => Tpc)
	case kPadOuter2Tpc:   rotA = SupS2Tpc(sector) * PadOuter2SupS(sector); break; // -"-

	case kPadInner2Glob:  rotA = Tpc2GlobalMatrix() * PadInner2Tpc(sector); break; // (Pad == SecL) => (SubS[io] => SupS => Tpc => Glob)
	case kPadOuter2Glob:  rotA = Tpc2GlobalMatrix() * PadOuter2Tpc(sector); break; // -"-
	default:
	  assert(0);
	}
      }
      // Normalize
      Double_t *r = rotA.GetRotationMatrix();
      Double_t norm;
      TVector3 d(r[0],r[3],r[6]); norm = 1/d.Mag(); d *= norm;
      TVector3 t(r[2],r[5],r[8]); norm = 1/t.Mag(); t *= norm;
      TVector3 n(r[1],r[4],r[7]);
      TVector3 c = d.Cross(t);
      if (c.Dot(n) < 0) c *= -1;
      Double_t rot[9] = {
	d[0], c[0], t[0],
	d[1], c[1], t[1],
	d[2], c[2], t[2]};
      rotA.SetRotation(rot);
      const Char_t *names[kTotalTpcSectorRotaions] = {
	"SupS_%02itoTpc",
	"SupS_%02itoGlob",
	"SubS_%02iInner2SupS",
	"SubS_%02iOuter2SupS",
	"SubS_%02iInner2Tpc",
	"SubS_%02iOuter2Tpc",
	"SubS_%02iInner2Glob",
	"SubS_%02iOuter2Glob",
	"PadInner2SupS_%02i",
	"PadOuter2SupS_%02i",
	"SupS_%02i12Inner2Tpc",
	"SupS_%02i12Outer2Tpc",
	"SupS_%02i12Inner2Glob",
	"SupS_%02i12Outer2Glob"
      };
      if (sector == 0) rotA.SetName("Tpc2Glob"); 
      else             rotA.SetName(Form(names[k],sector));
      if (Debug() > 1) {
	cout << "Id : " << Id << " "; rotA.Print();
      }
      SetTpcRotationMatrix(&rotA,sector,k);
    }
  }
}
