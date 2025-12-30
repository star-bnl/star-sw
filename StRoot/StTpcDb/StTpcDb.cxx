/***************************************************************************
 *
 * $Id: StTpcDb.cxx,v 1.69 2021/03/26 20:26:48 fisyak Exp $
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
 ***************************************************************************/
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
#include "StDetectorDbMaker/St_TpcDriftVelRowCorC.h"
#include "StarMagField.h"
#include "TEnv.h"
StTpcDb* gStTpcDb = 0;
Bool_t StTpcDb::mOldScheme = kTRUE;
Bool_t StTpcDb::mAlignment2024 = kFALSE;
Bool_t StTpcDb::mCosmics = kFALSE;
TGeoHMatrix   *StTpcDb::mTpc2GlobMatrix = 0;
// C++ routines:
//_____________________________________________________________________________
ClassImp(StTpcDb);
void StTpcDb::Clear() {
  for (Int_t i = 0;i<24;i++) {
    for (Int_t k = 0; k < kTotalTpcSectorRotaions; k++) 
      SafeDelete(mTpcSectorRotations[i][k]);
  }
  SafeDelete(mHalf[0]);  
  SafeDelete(mHalf[1]);
  SafeDelete(mShift[0]);  
  SafeDelete(mShift[1]);
  SafeDelete(mTpc2GlobMatrix);
  SafeDelete(mFlip);
}
//________________________________________________________________________________
void StTpcDb::SetAlignment2024(Bool_t k) {
  if (gStTpcDb && (! mTpc2GlobMatrix || mAlignment2024 != k)) {
    if (k) {LOG_INFO << "StTpcDb::SetAlignment2024 switch to new schema for Rotation matrices" << endm;}
    else   {LOG_INFO << "StTpcDb::SetAlignment2024 switch to old schema for Rotation matrices" << endm;}
    mAlignment2024 = k;
    gStTpcDb->Clear();
    gStTpcDb->SetTpcRotations();
  }
  mAlignment2024 = k;
}
//_____________________________________________________________________________
StTpcDb::StTpcDb() {
  assert(gStTpcDb==0);
  memset(mBeg,0,mEnd-mBeg+1);
  Clear();
  gStTpcDb = this;
  SetAlignment2024(mAlignment2024);
}
//_____________________________________________________________________________
StTpcDb::~StTpcDb() {
  Clear();
  gStTpcDb = 0;
}
//-----------------------------------------------------------------------------
float StTpcDb::DriftVelocity(Int_t sector, Int_t row) {
  static UInt_t u2007 = TUnixTime(20070101,0,1).GetUTime(); // 
  assert(mUc > 0);
  if (mUc < u2007) sector = 24;
  UInt_t kase = 1;
  if (sector <= 12) kase = 0;
  Float_t DV =1e6*mDriftVel[kase];
  if (row > 0) {
    // Extra row correction
    if (St_TpcDriftVelRowCorC::instance()->idx()) {
      DV *= (1. - St_TpcDriftVelRowCorC::instance()->CalcCorrection(0,row));
    }
  }
  return DV;
}
//-----------------------------------------------------------------------------
void StTpcDb::SetDriftVelocity() {
  static UInt_t u0 = 0; // beginTime of current Table
  static UInt_t u1 = 0; // beginTime for next Table
  static UInt_t umax = TUnixTime(20310101,0,1).GetUTime(); // maximum time allowed for next table
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
  //new:  global = Tpc2GlobalMatrix() * SupS2Tpc(sector) * StTpcSuperSectorPosition(sector) * Flip() * {StTpcInnerSectorPosition(sector)} | StTpcOuterSectorPosition(sector)}
  //      StTpcSuperSectorPosition(sector) * Flip() = Flip() * SubSInner2SupS(sector) 
  // =>  StTpcSuperSectorPosition(sector) = Flip() * SubSInner2SupS(sector) * Flip()^-1
  //      StTpcSuperSectorPosition(sector) * Flip() * StTpcOuterSectorPosition(sector) = Flip() *  SubSOuter2SupS(sector)
  // =>  StTpcOuterSectorPosition(sector) = Flip()^-1 * StTpcSuperSectorPosition(sector)^-1 *  Flip() *  SubSOuter2SupS(sector)
  /*
    .                                                                                             <-- the system of coordinate where Outer to Inner Alignment done -->
    global = Tpc2GlobalMatrix() * SupS2Tpc(sector) * StTpcSuperSectorPosition(sector) * Flip() * {    StTpcInnerSectorPosition(sector) | StTpcOuterSectorPosition(sector)} * local
    .                                                result of super sector alignment                                      result of Outxser to Inner sub sector alignment
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
     ================================================================================
     10/17/18 Just checking new schema
     02/02/2024 revist StTpcCoordinateTransorm 
     Global <=> TPCE =   StGlobalCoordinate <=> StTpcLocalCoordinate   
                         StTpcDb::Tpc2GlobalMatrix() = StTpcPosition::instance()->GetMatrix();

     TPCE <=> TPGV[2] => StTpcDb::TpcHalf(StBeamDirection part)
                                                    mHalf[east} = StTpcHalfPosition::instance()->GetEastMatrix()
                                                    mHalf[west] = StTpcHalfPosition::instance()->GetWestMatrix();   
     TPCE <=> TPSS =     StTpcLocalCoordinate <=> StTpcLocalSectorDirection

     StTpcDb::Pad2Tpc(sector,row) = StTpcDb::TpcRot(sector,(kPadInner2Tpc|kPadOuter2Tpc)) == 



          TPCE        (           TPGV                      ) * (              TPSS                                   )
     StTpcPosition * ((Shift(half) * StTpcHalfPosition(half)) * (rotmS(sector,iPhi) * StTpcSuperSectorPosition) * Flip) * (StTpcInnerSectorPosition || StTpcOuterSectorPosition)
        kTpcRefSys *          kTpcHalf                        *               kTpcPad       
        kTpcRefSys *          kSupS2Tpc                                                           

	kTpc2GlobalMatrix := StTpcPosition
	kSupS2Tpc         :=                 (Shift(half) * StTpcHalfPosition(half)) * (rotmS(sector,iPhi) * StTpcSuperSectorPosition * dR) 
	kSupS2Glob        := StTpcPosition * kSupS2Tpc										     
        kSubSInner2SupS   := Flip * StTpcInnerSectorPosition									     
        kSubSOuter2SupS   := Flip * StTpcOuterSectorPosition									     
        kSubSInner2Tpc    := kSupS2Tpc * dR^-1 *kSubSInner2SupS		dR => Outer wrt Inner								     
        kSubSOuter2Tpc    := kSupS2Tpc * dR    *kSubSOuter2SupS										     
	kPadInner2SupS    := kSubSInner2SupS											     
        kPadOuter2SupS    := kSubSOuter2SupS											     
        kPadInner2Tpc     := kSupS2Tpc * kPadInner2SupS                              == kSubSInner2Tpc				     
	kPadOuter2Tpc     := kSupS2Tpc * PadOuter2SupS  = kSupS2Tpc * kSubSOuter2SupS = kSubSOuter2Tpc				     
	kPadInner2Glob    := kTpc2GlobalMatrix * kPadInner2Tpc = kTpc2GlobalMatrix * kSubSInner2Tpc				     
	kPadOuter2Glob    := kTpc2GlobalMatrix * kPadOuter2Tpc = kTpc2GlobalMatrix * kSubSOuter2Tpc                                    
================================================================================
Revist 03/23/2024 move Flip
     kSubs2Tpc    = *mShift[part]) * (*mHalf[part]) * (*rotm) * Flip * TpcSuperSectorPosition * dR *  GG(z) * WHEEL * kSubS(Inner|Outer)2SupS ||  kPad(Inner|Outer)2SupS * dR * GG^-1(z)
                    <--                              Sup12S2Tpc                            ------>            <---                   Sub2SupS12                          --->
04/03/2014 
Old                                                                                                                      alpha = beta = 0
     kSubs2Tpc    = *mShift[part]) * (*mHalf[part]) * (*rotm) * Flip * TpcSuperSectorPosition * dR * dR' *  GG(z) * WHEEL * kSubS(Inner|Outer)2SupS ||  kPad(Inner|Outer)2SupS * dR * GG^-1(z)
                    <--                              Sup12S2Tpc                            ------>            <---                   Sub2SupS12                          --->
New 
     kSubs2Tpc    = *mShift[part]) * (*mHalf[part]) * (*rotm) * Flip * TpcSuperSectorPosition * dR * dR' *  GG(z) * WHEEL * kSubS(Inner|Outer)2SupS ||  kPad(Inner|Outer)2SupS * GG^-1(z)
                    <--                              Sup12S2Tpc                            ------>   <---                   Sub2SupS12                          --->
     kSubs2Tpc    = *mShift[part]) * (*mHalf[part]) * (*rotm) * Flip * TpcSuperSectorPosition * dR * dR' *  GG(z) * WHEEL * dR" * kSubS(Inner|Outer)2SupS ||  kPad(Inner|Outer)2SupS * GG^-1(z)
     kSubs2Tpc    = *mShift[part]) * (*mHalf[part]) * (*rotm) * Flip * TpcSuperSectorPosition * dR * dR' *  GG(z) * WHEEL       * kSubS(Inner|Outer)2SupS ||  kPad(Inner|Outer)2SupS * GG^-1(z)
     kSubs2Tpc    = *mShift[part]) * (*mHalf[part]) * (*rotm) * Flip * TpcSuperSectorPosition * dR *        GG(z) * WHEEL * dR" * kSubS(Inner|Outer)2SupS ||  kPad(Inner|Outer)2SupS * GG^-1(z)
                    <--                              Sup12S2Tpc                            ------>          <---                                       Sub2SupS12                          --->
                                                                                                     dR' *  GG(z) * WHEEL = GG(z) * WHEEL * dR"
                                                                                                     dR" =  WHEEL^1 * GG(z)^-1 * dR' * GG(z) * WHEEL ~ dR'

     kSubs2Tpc    = *mShift[part]) * (*mHalf[part]) * (*rotm) * Flip * TpcSuperSectorPosition * dR *        GG(z) * WHEEL * kSubS(Inner|Outer)2SupS ||  kPad(Inner|Outer)2SupS * dR" * GG^-1(z)
                    <--                                                                        -->          <--                                                                             -->
     xG           = <--                                                                        --> * x      <--                                                                             --> xPad
     kSubs2Tpc    = *mShift[part]) * (*mHalf[part]) * (*rotm) * Flip * TpcSuperSectorPosition * dR *        GG(z) * WHEEL * dR" * kSubS(Inner|Outer)2SupS ||  kPad(Inner|Outer)2SupS * GG^-1(z)
                                                                                                     x =    <--                                                                  --> * xPadGG
                                                                                                     x =    <--     dR' * WHEEL                                                             --> * xPad
                                                                                                                                                                                        xPadGG = GG^-1 * xPad
   dR' * WHEEL =  WHEEL * dR";  dR" = WHEEL^-1 * dR'* WHEEL  
--------------------------------------------------------------------------------
Revist 04/17/2024 modify Half (rotaion around Wheel position) and Flip (add shift before absorbed by mShift);
                                                                                    add to flip shift to by zGG
     kSubs2Tpc    = *mShift[part]) * (*mHalf[part]) * (*mShift[part])^^-1 * rotm) * Flip * TpcSuperSectorPosition * ddRO * dRO *  GG(z) * WHEEL * kSubS(Inner|Outer)2SupS ||  kPad(Inner|Outer)2SupS * GG^-1(z)
                    <--                              Sup12S2Tpc                                           ------>   <---                     Sub2SupS12                                                    --->

Mag.Flip:        HS == Half sum; HD == Half dif
FF: dRO = ROHS * dROHD
RF: dRO = ROHS * dROHD^^-1
dFF: dRO_new = ddROFF * ROHS * dROHD
dRF: dRO_new = ddRORF * ROHS * dROHD^^-1
      ddRO * dRO = FF ? ddROFF * dROHS * dROHD

dR( alpha,beta,gamma, x_0, y_0, z_0) := 
matrix([     1,-gamma,   beta, x_0],
       [ gamma,      1,-alpha, y_0],
       [ -beta, alpha,      1, z_0],
       [     0,     0,      0,   1]);
dRT( alpha,beta,gamma, x_0, y_0, z_0) :=  dR^-1
matrix([     1, gamma,  -beta,-x_0],
       [-gamma,      1, alpha,-y_0],
       [  beta,-alpha,      1,-z_0],
       [     0,     0,      0,   1]);
  */
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
  const Char_t *names[kTotalTpcSectorRotaions];
  names[kSupS2Tpc]          = "SupS_%02i2Tpc";        
  names[kSupS2Glob]         = "SupS_%02i2Glob";            
  names[kSubSInner2SupS]    = "SubS_%02iInner2SupS";  
  names[kSubSOuter2SupS]    = "SubS_%02iOuter2SupS";  
  names[kPadInner2SupS]     = "PadInner2SupS_%02i";   
  names[kPadOuter2SupS]     = "PadOuter2SupS_%02i";   
			                       
  names[kSup12S2Tpc]        = "Sup12S_%02i2Tpc";      
  names[kSup12S2Glob]       = "Sup12S_%02i2Glob";     
  names[kSubSInner2Sup12S]  = "SubS_%02iInner2Sup12S";
  names[kSubSOuter2Sup12S]  = "SubS_%02iOuter2Sup12S";
  names[kPadInner2Sup12S]   = "PadInner2Sup12S_%02i"; 
  names[kPadOuter2Sup12S]   = "PadOuter2Sup12S_%02i"; 
			                       
  names[kSubSInner2Tpc]     = "SubS_%02iInner2Tpc";   
  names[kSubSOuter2Tpc]     = "SubS_%02iOuter2Tpc";   
  names[kSubSInner2Glob]    = "SubS_%02iInner2Glob";  
  names[kSubSOuter2Glob]    = "SubS_%02iOuter2Glob";  
  names[kPadInner2Tpc]      = "PadInner2Tpc";    
  names[kPadOuter2Tpc]      = "PadOuter2Tpc";    
  names[kPadInner2Glob]     = "PadInner2Glob";   
  names[kPadOuter2Glob]     = "PadOuter2Glob";  
  names[kWheel]             = "Wheel";
  names[kRotM]              = "RotM";
  mTpc2GlobMatrix = new TGeoHMatrix("Default Tpc2Glob"); 
  mFlip = new TGeoHMatrix;
  mzGG = Dimensions()->gatingGridZ(); // zGG
  Double_t Rotation[9] = {0, 1, 0, 
			  1, 0, 0,
			  0, 0,-1};
  mFlip->SetName("Flip"); mFlip->SetTitle("flip to Sector 12 local coordiname"); mFlip->SetRotation(Rotation);
  if (mAlignment2024) {
    Double_t Translation[3] = {0, 0, mzGG};
    mFlip->SetTranslation(Translation);
  }
  if (! mAlignment2024) {
    mShift[0] = new TGeoTranslation("Signed Drift distance to z for East", 0, 0, -mzGG);
    mShift[1] = new TGeoTranslation("Signed Drift distance to z for West", 0, 0,  mzGG);
  } else {
    mzWheel = 229.71;
    mShift[0] = new TGeoTranslation("Signed Drift distance to z for East", 0, 0, -mzWheel);
    mShift[1] = new TGeoTranslation("Signed Drift distance to z for West", 0, 0,  mzWheel);
  }
  mHalf[0] = new TGeoHMatrix("Default for east part of TPC");
  mHalf[1] = new TGeoHMatrix("Default for west part of TPC");
  mWheel[0] = new TGeoHMatrix("Default for east wheel of TPC");
  mWheel[1] = new TGeoHMatrix("Default for west wheel of TPC");
  St_SurveyC *chair = 0;
  St_SurveyC *chairD = 0;
  Int_t Id = 0;
  TGeoHMatrix rotA;
  TGeoHMatrix dR;
  static Int_t newRotations[6] = {kSup12S2Tpc, kSup12S2Glob, kSubSInner2Sup12S, kSubSOuter2Sup12S, kPadInner2Sup12S, kPadOuter2Sup12S};
  static Int_t oldRotations[6] = {kSupS2Tpc  , kSupS2Glob  , kSubSInner2SupS  , kSubSOuter2SupS  , kPadInner2SupS  , kPadOuter2SupS  };
  for (Int_t sector = 0; sector <= 24; sector++) {// loop over Tpc as whole, sectors, inner and outer subsectors
    // Avoid mixure with Alignment2024
    Int_t k;
    Int_t k1 = kSupS2Tpc;
    Int_t k2 = kTotalTpcSectorRotaions - 2; // - kRotM && dRS12
    if (sector == 0) {k2 = k1; k1 = kUndefSector;}
    for (k = k1; k < k2; k++) {
      if (! mAlignment2024) {
	for (Int_t l = 0; l < 6; l++) if (k == newRotations[l]) goto ENDLOOP;
      } else {
	for (Int_t l = 0; l < 6; l++) if (k == oldRotations[l]) goto ENDLOOP;
      }
      Id     = 0;
      rotA = TGeoHMatrix();; // After alignment
      chair = 0;
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
	  *mWheel[east] = StTpcWheelPosition::instance()->GetEastMatrix();
	  *mWheel[west] = StTpcWheelPosition::instance()->GetWestMatrix();
	}
      } else {
	Id = 10*sector + k;
	StBeamDirection part = TpcPart(sector);
	switch (k) {
	case kSupS2Tpc: // SupS => Tpc
	case kSup12S2Tpc: // Sup12S => Tpc
	  chair = StTpcSuperSectorPosition::instance();
	  chairD = StTpcSuperSectorPositionD::instance();
	  iphi = SectorPhiDeg(sector);
	  if (sector <= 12) {Rot = Form("R%03i",iphi);}
	  else              {Rot = Form("Y%03i",iphi);}
	  rotm = 0;
	  if (gGeoManager) {
	    listOfMatrices =  gGeoManager->GetListOfMatrices();
	    rotm = (TGeoRotation *) listOfMatrices->FindObject(Rot);
	  }
	  if (! rotm) {
	    if (sector <= 12) rotm = new TGeoRotation(Rot);
	    else              rotm = new TGeoRotation(Rot,   90.0,    0.0,  90.0,  -90.0,  180.0,    0.00); // Flip (x,y,z) => ( x,-y,-z)
	    rotm->RotateZ(iphi);
	    if (gGeoManager) rotm->RegisterYourself();
	  }
	  if (Debug()) {
	    rotm->Print();
	  }
	  if (! mAlignment2024) { 
	    rotA = (*mShift[part]) * (*mHalf[part]) * (*rotm);
	  } else {
	    rotA = (*mShift[part]) * (*mHalf[part]) * (*mShift[part]).Inverse() * (*rotm);
	  }
	  if (k == kSup12S2Tpc) rotA *= Flip(); // new in 2024 schema
	  SetTpcRotationMatrix(&rotA,sector,kRotM);// Save ideal rotation
	  dR = chair->GetMatrix(sector-1);
	  if (chairD->getNumRows() == 24) {
	    if (gFactor > 0.2) {
	      dR *= chairD->GetMatrix(sector-1);
	    } else if (gFactor < -0.2) {
	      dR *= chairD->GetMatrix(sector-1).Inverse();
	    }
	  }
	  SetTpcRotationMatrix(&dR,sector,kdRS12);// Save correction
	  rotA = TpcRot(sector, kRotM) * dR;
	  //	  if (k == kSup12S2Tpc) rotA *= Flip();  // new in 2023 schema 
	  if (! gGeoManager) SafeDelete(rotm);
	  break;
	case kSupS2Glob:      // SupS => Tpc => Glob
	  rotA = Tpc2GlobalMatrix() * SupS2Tpc(sector); 
	  break; 
	case kSup12S2Glob:      // SupS => Tpc => Glob
	  rotA = Tpc2GlobalMatrix() * Sup12S2Tpc(sector); 
	  break; 
	case kSubSInner2SupS: 
	case kSubSInner2Sup12S: 
	  if (mOldScheme) 	  {rotA = Flip(); break;}
	  chair = StTpcInnerSectorPosition::instance();
	case kSubSOuter2SupS: 
	case kSubSOuter2Sup12S: 
	  if (mOldScheme) {rotA = Flip() * StTpcOuterSectorPosition::instance()->GetMatrix(sector-1); break;}
	  if (! chair) chair = StTpcOuterSectorPosition::instance();
	  //new 2024   Flip done in Sup12STpc
	  //new 2024	  if (k == kSubSInner2SupS || k == kSubSOuter2SupS) rotA = Flip() * chair->GetMatrix(sector-1);  // to Sector 12 
	  //new 2024	  else                                              rotA =          chair->GetMatrix(sector-1);  // to Sector 12 
	  if (k == kSubSOuter2SupS || 
	      k == kSubSInner2SupS) rotA = Flip() * chair->GetMatrix(sector-1);
	  else                      rotA =          chair->GetMatrix(sector-1);  
	  if (chair->GetNRows() == 48) {
	    if (gFactor > 0.2) {
	      rotA *= chair->GetMatrix(sector-1+24);
	    } else if (gFactor < -0.2) {
	      rotA *= chair->GetMatrix(sector-1+24).Inverse();
	    }
	  }
	  break;
	case kSubSInner2Tpc:  // (Subs[io] => SupS) => Tpc
	  if (! mAlignment2024) rotA = SupS2Tpc(sector)   * SubSInner2SupS(sector); 
	  else                  rotA = Sup12S2Tpc(sector) * SubSInner2Sup12S(sector); 
	  break; 
	case kSubSOuter2Tpc:  // -"-
	  if (! mAlignment2024) rotA = SupS2Tpc(sector)   * SubSOuter2SupS(sector);
	  else                  rotA = Sup12S2Tpc(sector) * SubSOuter2Sup12S(sector);
	  break; 
	case kSubSInner2Glob: rotA = Tpc2GlobalMatrix() * SubSInner2Tpc(sector);  break; // Subs[io] => SupS => Tpc) => Glob
	case kSubSOuter2Glob: rotA = Tpc2GlobalMatrix() * SubSOuter2Tpc(sector);  break; // -"-

	case kPadInner2SupS:  rotA = SubSInner2SupS(sector); break; // (Pad == SecL) => (SubS[io] => SupS)
	case kPadOuter2SupS:  rotA = SubSOuter2SupS(sector); break; // -"-
	case kPadInner2Sup12S:  rotA = SubSInner2Sup12S(sector); break; // (Pad == SecL) => (SubS[io] => Sup12S)
	case kPadOuter2Sup12S:  rotA = SubSOuter2Sup12S(sector); break; // -"-
	case kPadInner2Tpc:   // (Pad == SecL) => (SubS[io] => SupS => Tpc)
	  if (! mAlignment2024) rotA = SupS2Tpc(sector) * PadInner2SupS(sector); 
	  else                  rotA = Sup12S2Tpc(sector) * PadInner2Sup12S(sector); 
	  break; 
	case kPadOuter2Tpc:    // -"-
	  if (! mAlignment2024) rotA = SupS2Tpc(sector) * PadOuter2SupS(sector); 
	  else                  rotA = Sup12S2Tpc(sector) * PadOuter2Sup12S(sector); 
	  break;
	case kPadInner2Glob:  rotA = Tpc2GlobalMatrix() * PadInner2Tpc(sector); break; // (Pad == SecL) => (SubS[io] => SupS => Tpc => Glob)
	case kPadOuter2Glob:  rotA = Tpc2GlobalMatrix() * PadOuter2Tpc(sector); break; // -"-
	case kWheel:          
	  if (! mAlignment2024) {rotA = SupS2Tpc(sector).Inverse();   rotA *= *(mWheel[part]); rotA *= SupS2Tpc(sector);}
	  else                  {rotA = Sup12S2Tpc(sector).Inverse(); rotA *= *(mWheel[part]); rotA *= Sup12S2Tpc(sector);}
	  break; 
	default:
	  assert(0);
	}
      }
#if 0
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
#endif
      if (sector == 0) rotA.SetName("Tpc2Glob"); 
      else             rotA.SetName(Form(names[k],sector));
      if (Debug() > 1) {
	cout << "Id : " << Id << " "; rotA.Print();
      }
      SetTpcRotationMatrix(&rotA,sector,k);
    ENDLOOP:
      continue;
    }
  }
}
