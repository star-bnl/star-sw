/***************************************************************************
 *
 * $Id: StTpcDb.h,v 1.46 2021/03/26 20:26:48 fisyak Exp $
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
#ifndef ClassStTpcDb
#define ClassStTpcDb

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StTpcDb                                                              //
//                                                                      //
// This class implements to offline interface to the STAR database      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <assert.h>
#include "StMessMgr.h"
#include "StEnumerations.h"
#include "StDetectorDbMaker/St_tpcPadConfigC.h"
#include "StDetectorDbMaker/St_tpcWirePlanesC.h"
#include "StDetectorDbMaker/St_tpcDimensionsC.h"
#include "StDetectorDbMaker/St_tpcElectronicsC.h"
#include "StDetectorDbMaker/St_tpcSlowControlSimC.h"
#include "StDetectorDbMaker/St_tpcGlobalPositionC.h"
#include "StDetectorDbMaker/St_tpcSectorPositionC.h"
#include "StDetectorDbMaker/St_tpcFieldCageC.h"
#include "StDetectorDbMaker/St_tpcPedestalC.h"
#include "StDetectorDbMaker/St_tpcPadResponseC.h"
#include "StDetectorDbMaker/St_tpcPadGainT0BC.h"
#include "StDbUtilities/StMagUtilities.h"
#include "StDetectorDbMaker/St_trgTimeOffsetC.h"
#include "TGeoMatrix.h"
#include "TString.h"
class StTpcDb;
// Global pointers:
R__EXTERN StTpcDb* gStTpcDb;
class StTpcDb {
 public:    
  static StTpcDb* instance() {if (! gStTpcDb) new StTpcDb(); return gStTpcDb;}
  // Glob     = Global coordinate 
  // Tpc      = Tpc    -"-                survey
  // Half     = Tpc Half west / east -"-  survey
  // Wheel    = Wheel rotations from Survey, no drift 
  // SupS     = super sector misalignment(?)
  // SubS[io] = SubSector[io] misalignment
  // SecL     = sector -"- coordinate (y_p, x_p, DriftDistance - z_p);
  // Pad      = Pad -"- (x_p,y_p,z_p) (Sector12 coordinate system)
  // Tpc => Global is mTpc2GlobMatrix
  // Pad => SecL   is internal Flip matrix     Sup12 == Supersector 12 coordinate system x,y,z)_TPC => (-y,-x,drift = zGG - |z|), move Flip from Pad to Sup12 
  enum ETpcSectorRotationType {kUndefSector       = -2,							   
			       kFlip              = -1, // Flip * Subs[io] => SupS			   
						                                                            
			       kSupS2Tpc          =  0, // SupS(sector) => Tpc, SupS(0) == mTpc2GlobMatrix  
			       kSupS2Glob         =  1, // SupS => Tpc => Glob; 				   
			       kSubSInner2SupS    =  2, // Subs[io] => SupS				   
			       kSubSOuter2SupS    =  3, // -"-						   
			       kPadInner2SupS     =  4, // (Pad => SecL) => (SubS[io] => SupS)		   
			       kPadOuter2SupS     =  5, // -"-                                              

			       kSup12S2Tpc        =  6, // Sup12S => Tpc
			       kSup12S2Glob       =  7, // Sup12S => Tpc => Glob; 
			       kSubSInner2Sup12S  =  8, // Subs[io] => Sup12S
			       kSubSOuter2Sup12S  =  9, // -"-
			       kPadInner2Sup12S   = 10, // (Pad => SecL) => (SubS[io] => Sup12S)
			       kPadOuter2Sup12S   = 11, // -"- 

			       kSubSInner2Tpc     = 12, // (Subs[io] => SupS) => Tpc			     
			       kSubSOuter2Tpc     = 13, // -"-						     
			       kSubSInner2Glob    = 14, // (Subs[io] => SupS => Tpc) => Glob		     
			       kSubSOuter2Glob    = 15, // -"-						     
			       kPadInner2Tpc      = 16, // (Pad => SecL) => (SubS[io] => SupS => Tpc)	     
			       kPadOuter2Tpc      = 17, // -"- 						     
			       kPadInner2Glob     = 18, // (Pad => SecL) => (SubS[io] => SupS => Tpc => Glob) 
			       kPadOuter2Glob     = 19, // -"- 						    
			       			                                                              
                               kWheel             = 20, // Account of Wheel rotation around X and Y axes at GG
			       kRotM              = 21, // Ideal rotaion
			       kdRS12             = 22, // Super Sector correction
			       kTotalTpcSectorRotaions = 23};
 private:
  static TGeoHMatrix   *mTpc2GlobMatrix;//!
  Char_t                mBeg[1];        //!
  Int_t                 m_Debug;        //!
  TGeoTranslation      *mShift[2];      //! 
  TGeoHMatrix          *mFlip;          //!
  TGeoHMatrix          *mHalf[2];       //!
  TGeoHMatrix          *mWheel[2];      //!
  TGeoHMatrix          *mTpcSectorRotations[24][kTotalTpcSectorRotaions]; //!
  Float_t               mDriftVel[2];   //!
  UInt_t                mUc;            //! time for which above mDriftVel have been calculated
  Int_t                 mTriggerId;     //! to distinguish local clock and RHIC clock
  Double_t              mzGG;           //! Gating Grid z
  Double_t              mzWheel;        //! Wheel z
  Char_t                mEnd[1];        //!
  static Bool_t         mOldScheme;     //! switch between Old and New alignment scheme
  static Bool_t         mAlignment2024; //! switch between Old and 2024 alignment scheme
  static Bool_t         mCosmics;       //! if cosmics 
 private:
  StTpcDb();
 public:
  virtual ~StTpcDb();
  void                   Clear();
  St_tpcWirePlanesC     *WirePlaneGeometry() {return St_tpcWirePlanesC::instance();}
  St_tpcDimensionsC     *Dimensions() {return St_tpcDimensionsC::instance();}
  St_tpcSlowControlSimC *SlowControlSim() {return St_tpcSlowControlSimC::instance();}
  St_tpcElectronicsC    *Electronics() {return St_tpcElectronicsC::instance();}
  St_tpcGlobalPositionC *GlobalPosition() {return St_tpcGlobalPositionC::instance();}
  St_tpcFieldCageC      *FieldCage() {return St_tpcFieldCageC::instance();}
  St_tpcSectorPositionC *SectorPosition() {return St_tpcSectorPositionC::instance();}
  St_tpcPedestalC       *Pedestal() {return St_tpcPedestalC::instance();}
  St_tpcPadGainT0BC     *tpcGain() {return St_tpcPadGainT0BC::instance();}
  St_tpcPadGainT0BC     *tpcT0()   {return St_tpcPadGainT0BC::instance();}
  St_tpcPadResponseC    *PadResponse() {return St_tpcPadResponseC::instance();}
  Float_t                triggerTimeOffset()     {return 1e-6*(IsLaser() ? St_trgTimeOffsetC::instance()->laserOffset() : St_trgTimeOffsetC::instance()->offset());} // usec
  Float_t                triggerTimeOffsetWest() {return 1e-6*(IsLaser() ? St_trgTimeOffsetC::instance()->laserOffsetW():         0);} // usec
  Bool_t                 IsLaser()               {return mTriggerId != 0;}
  static Bool_t          IsOldScheme()           {return mOldScheme;}
  Double_t               zGG() {return mzGG;}
  static Int_t           SectorPhiDeg(Int_t sector) {// sector Phi degree
    Int_t iphi = 0;
    assert(sector >= 1 && sector <= 24);
    if (sector <= 12) {iphi = (360 + 90 - 30* sector      )%360;}
    else              {iphi = (      90 + 30*(sector - 12))%360;}
    return iphi;
  }
  static StBeamDirection TpcPart(Int_t sector) {StBeamDirection part = east; if (sector <= 12) part = west; return part;}
  //small pieces of data:
  void    SetDriftVelocity();
  Float_t DriftVelocity(Int_t sector=24) {return DriftVelocity(sector, 0);}
  Float_t DriftVelocity(Int_t sector, Int_t row);
  void SetTpcRotations();
  void SetTpc2GlobalMatrix(TGeoHMatrix *m) {SetTpcRotationMatrix(m);}
  void SetTpcRotationMatrix(TGeoHMatrix *m, Int_t sector = 0, Int_t k = kSupS2Tpc) {
    if (sector == 0)  {if (m) *mTpc2GlobMatrix = *m;}
    else              {if (m)  mTpcSectorRotations[sector-1][k] = new TGeoHMatrix(*m);}
  }
  void  SetDebug(Int_t m) {m_Debug = m;}
  Int_t Debug() {return m_Debug;}
  void  SetTriggerId(Int_t m) {mTriggerId = m;} // Laser Trigger
  Int_t TriggerId() {return mTriggerId;}
  const TGeoHMatrix &Flip()                           const {return *mFlip;}
  const TGeoHMatrix &TpcHalf(StBeamDirection part)    const {return *mHalf[part];}
  const TGeoHMatrix &TpcWheel(StBeamDirection part)   const {return *mWheel[part];}   // Wheel Rotation in TPC coordinate system
  const TGeoTranslation &Shift(StBeamDirection part)  const {return *mShift[part];}
  const TGeoHMatrix &Tpc2GlobalMatrix()               const {return *mTpc2GlobMatrix;}
  const TGeoHMatrix &TpcRot(Int_t sector, Int_t k)    const {assert(mTpcSectorRotations[sector-1][k]); return *mTpcSectorRotations[sector-1][k];}
  const TGeoHMatrix &SupS2Tpc(Int_t sector = 1)       const {return TpcRot(sector,kSupS2Tpc);}
  const TGeoHMatrix &SupS2Glob(Int_t sector = 1)      const {return TpcRot(sector,kSupS2Glob);}
  const TGeoHMatrix &SubSInner2SupS(Int_t sector = 1) const {return TpcRot(sector,kSubSInner2SupS);}
  const TGeoHMatrix &SubSOuter2SupS(Int_t sector = 1) const {return TpcRot(sector,kSubSOuter2SupS);}
  const TGeoHMatrix &SubSInner2Tpc(Int_t sector = 1)  const {return TpcRot(sector,kSubSInner2Tpc);}
  const TGeoHMatrix &SubSOuter2Tpc(Int_t sector = 1)  const {return TpcRot(sector,kSubSOuter2Tpc);}
  const TGeoHMatrix &SubSInner2Glob(Int_t sector = 1) const {return TpcRot(sector,kSubSInner2Glob);}
  const TGeoHMatrix &SubSOuter2Glob(Int_t sector = 1) const {return TpcRot(sector,kSubSOuter2Glob);}

  const TGeoHMatrix &PadInner2SupS(Int_t sector = 1)  const {return TpcRot(sector,kPadInner2SupS);}
  const TGeoHMatrix &PadOuter2SupS(Int_t sector = 1)  const {return TpcRot(sector,kPadOuter2SupS);}
  const TGeoHMatrix &PadInner2Tpc(Int_t sector = 1)   const {return TpcRot(sector,kPadInner2Tpc);}
  const TGeoHMatrix &PadOuter2Tpc(Int_t sector = 1)   const {return TpcRot(sector,kPadOuter2Tpc);}
  const TGeoHMatrix &PadInner2Glob(Int_t sector = 1)  const {return TpcRot(sector,kPadInner2Glob);}
  const TGeoHMatrix &PadOuter2Glob(Int_t sector = 1)  const {return TpcRot(sector,kPadOuter2Glob);}

  const TGeoHMatrix &SubS2SupS(Int_t sector = 1, Int_t row = 1) const {Int_t k = (row <= St_tpcPadConfigC::instance()->innerPadRows(sector)) ? kSubSInner2SupS : kSubSOuter2SupS; return TpcRot(sector,k);}
  const TGeoHMatrix &SubS2Tpc(Int_t sector = 1, Int_t row = 1)  const {Int_t k = (row <= St_tpcPadConfigC::instance()->innerPadRows(sector)) ? kSubSInner2Tpc : kSubSOuter2Tpc; return TpcRot(sector,k);}
  const TGeoHMatrix &SubS2Glob(Int_t sector = 1, Int_t row = 1) const {Int_t k = (row <= St_tpcPadConfigC::instance()->innerPadRows(sector)) ? kSubSInner2Glob: kSubSOuter2Glob; return TpcRot(sector,k);}

  const TGeoHMatrix &Pad2SupS(Int_t sector = 1, Int_t row = 1)  const {Int_t k = (row <= St_tpcPadConfigC::instance()->innerPadRows(sector)) ? kPadInner2SupS: kPadOuter2SupS; return TpcRot(sector,k);}
  const TGeoHMatrix &Pad2Tpc(Int_t sector = 1, Int_t row = 1)   const {Int_t k = (row <= St_tpcPadConfigC::instance()->innerPadRows(sector)) ? kPadInner2Tpc: kPadOuter2Tpc; return TpcRot(sector,k);}
  const TGeoHMatrix &Pad2Glob(Int_t sector = 1, Int_t row = 1)  const {Int_t k = (row <= St_tpcPadConfigC::instance()->innerPadRows(sector)) ? kPadInner2Glob: kPadOuter2Glob; return TpcRot(sector,k);}
  //--------------------------------------------------------------------------------
  const TGeoHMatrix &Sup12S2Tpc(Int_t sector = 1)       const {return TpcRot(sector,kSup12S2Tpc);}
  const TGeoHMatrix &Wheel(Int_t sector = 1)            const {return TpcRot(sector,kWheel);}       // TPC Wheel rotation in Super Sector coordinate system
  const TGeoHMatrix &Sup12S2Glob(Int_t sector = 1)      const {return TpcRot(sector,kSup12S2Glob);}
  const TGeoHMatrix &SubSInner2Sup12S(Int_t sector = 1) const {return TpcRot(sector,kSubSInner2Sup12S);}
  const TGeoHMatrix &SubSOuter2Sup12S(Int_t sector = 1) const {return TpcRot(sector,kSubSOuter2Sup12S);}

  const TGeoHMatrix &PadInner2Sup12S(Int_t sector = 1)  const {return TpcRot(sector,kPadInner2Sup12S);}
  const TGeoHMatrix &PadOuter2Sup12S(Int_t sector = 1)  const {return TpcRot(sector,kPadOuter2Sup12S);}

  const TGeoHMatrix &SubS2Sup12S(Int_t sector = 1, Int_t row = 1) const {Int_t k = (row <= St_tpcPadConfigC::instance()->innerPadRows(sector)) ? kSubSInner2Sup12S : kSubSOuter2Sup12S; return TpcRot(sector,k);}

  const TGeoHMatrix &Pad2Sup12S(Int_t sector = 1, Int_t row = 1)  const {Int_t k = (row <= St_tpcPadConfigC::instance()->innerPadRows(sector)) ? kPadInner2Sup12S: kPadOuter2Sup12S; return TpcRot(sector,k);}


  static void   SetAlignment2024(Bool_t k = kFALSE);
  static Bool_t Alignment2024() {return mAlignment2024;}
  static void   SetCosmics(Bool_t k = kFALSE) {mCosmics = k;}
  static Bool_t IsCosmics() {return mCosmics;}
  ClassDef(StTpcDb,0)
};
#endif
