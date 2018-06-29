/***************************************************************************
 *
 * $Id: StTpcDb.h,v 1.45 2018/06/29 21:46:22 smirnovd Exp $
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
 * $Log: StTpcDb.h,v $
 * Revision 1.45  2018/06/29 21:46:22  smirnovd
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
 * Revision 1.43  2018/06/07 04:30:35  genevb
 * Remove unnecessary dependence on StMagUtilities.h
 *
 * Revision 1.42  2018/04/11 02:43:22  smirnovd
 * Enable TPC/iTPC switch via St_tpcPadConfig
 *
 * This is accomplished by substituting St_tpcPadPlanes with St_tpcPadConfig.
 * A sector ID is passed to St_tpcPadConfig in order to extract parameters for
 * either TPC or iTPC
 *
 * Revision 1.41  2014/06/27 14:04:25  fisyak
 * Add env. NewTpcAlignment to switch between new and old scheme
 *
 * Revision 1.40  2014/06/26 21:32:57  fisyak
 * New Tpc Alignment, v632
 *
 * Revision 1.39  2012/09/17 19:39:44  fisyak
 * Add rotation for Half Tpc's
 *
 * Revision 1.38  2012/05/03 23:56:48  fisyak
 * Set interpolation for one week only, fix sign of interpolation (thanks Gene), add TriggerId
 *
 * Revision 1.37  2011/07/21 16:48:53  fisyak
 * New schema for Sub Sector Alginement: SuperSectror position (defined by inner sub sector) and Outer sector position wrt SuperSectror position
 *
 * Revision 1.36  2011/01/18 14:39:43  fisyak
 * Clean up TpcDb interfaces and Tpc coordinate transformation
 *
 * Revision 1.35  2010/05/27 19:14:26  fisyak
 * Take out flavoring by 'sim' for tpcGlobalPosition,tpcSectorPosition and starClockOnl tables. remove usage tpcISTimeOffsets and tpcOSTimeOffsets tables
 *
 * Revision 1.34  2009/12/07 23:44:58  fisyak
 * Drop coordinate transformation for fortran, remove TpcHitErr
 *
 * Revision 1.33  2009/11/02 17:31:41  fisyak
 * use directly field from StarMagField, replace St_tpcGainC and St_tpcT0C by St_tpcPadGainT0C, add remove defaults in coordinate transformations
 *
 * Revision 1.32  2009/03/16 14:13:31  fisyak
 * Use StDetectorDb chairs for TpcGlobalPosition and TpcSectorPosition
 *
 * Revision 1.31  2008/09/10 15:46:36  fisyak
 * Recalculate Tpc drift velocity once per event, avoid expensive conversion to unix time
 *
 * Revision 1.30  2008/08/01 14:28:25  fisyak
 * Add new getT0, clean up
 *
 * Revision 1.29  2007/08/12 15:06:30  fisyak
 * Use separated East/West drift velocities only >= 2007, for back compartibility
 *
 * Revision 1.28  2007/07/12 20:21:09  fisyak
 * Drift velocity depends on TPC half, use online RHIC clock
 *
 * Revision 1.27  2007/03/21 17:27:01  fisyak
 * use TGeoHMatrix, change mode for switching drift velocities
 *
 * Revision 1.26  2004/10/27 21:44:28  fisyak
 * Add debug print for tables Validities, add access to ExB correction
 *
 * Revision 1.25  2004/02/23 00:35:00  fisyak
 * Add access to tpcPadResponse
 *
 * Revision 1.24  2004/01/14 22:54:30  fisyak
 * Add hooks for Pedestal and tpcGain
 *
 * Revision 1.23  2002/04/02 00:16:31  hardtke
 * New class that gets hit errors from database
 *
 * Revision 1.22  2002/02/06 18:39:13  hardtke
 * Add tpc Field Cage structure
 *
 * Revision 1.21  2001/08/14 18:18:03  hardtke
 * Add sector position structures
 *
 * Revision 1.20  2001/05/21 23:25:34  hardtke
 * Add tpcGlobalPosition to StTpcDb.  This includes the global position offset and the rotation w.r.t. the magnet
 *
 * Revision 1.19  2000/08/10 18:41:34  hardtke
 * only look for L0_trigger table once per event -- improves timing
 *
 * Revision 1.18  2000/08/09 14:54:54  hardtke
 * Add Clear option, set trigger table pointer to 0 after each event
 *
 * Revision 1.17  2000/08/08 19:15:23  hardtke
 * use correct trigger time offset in case of laser
 *
 * Revision 1.16  2000/05/11 17:17:27  hardtke
 * make trigger time offset available -- currently NOT different for beam and laser events
 *
 * Revision 1.15  2000/03/27 21:21:22  fine
 * Adjusted to ROOT 2.24
 *
 * Revision 1.14  2000/02/10 00:29:09  hardtke
 * Add tpg functions to StTpcDbMaker, fix a few bugs
 *
 * Revision 1.13  2000/01/25 16:01:10  fisyak
 * Devorce with StAF
 *
 * Revision 1.12  2000/01/11 15:49:52  hardtke
 * get Electronics table from Calibrations database, Fix error messages
 *
 * Revision 1.11  1999/12/16 22:00:53  hardtke
 * add CVS tags
 *
 **************************************************************************/
#ifndef ClassStTpcDb
#define ClassStTpcDb

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StTpcDb                                                              //
//                                                                      //
// This class implements to offline interface to the STAR database      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

class StMagUtilities;
#include "StMessMgr.h"
#include "StEnumerations.h"
#include "StDetectorDbMaker/St_tpcPadPlanesC.h"
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
  // SupS     = super sector misalignment(?)
  // SubS[io] = SubSector[io] misalignment
  // SecL     = sector -"- coordinate (y_p, x_p, DriftDistance - z_p);
  // Pad      = Pad -"- (x_p,y_p,z_p) (Sector12 coordinate system)
  // Tpc => Global is mTpc2GlobMatrix
  // Pad => SecL   is internal Flip matrix
  enum ETpcSectorRotationType {kUndefSector     =-2,
			       kFlip            =-1, // Flip * Subs[io] => SupS
			       kSupS2Tpc        = 0, // SupS => Tpc
			       kSupS2Glob       = 1, // SupS => Tpc => Glob; 
			       kSubSInner2SupS  = 2, // Subs[io] => SupS
			       kSubSOuter2SupS  = 3, // -"-
			       kSubSInner2Tpc   = 4, // (Subs[io] => SupS) => Tpc
			       kSubSOuter2Tpc   = 5, // -"-
			       kSubSInner2Glob  = 6, // (Subs[io] => SupS => Tpc) => Glob
			       kSubSOuter2Glob  = 7, // -"-
			       kPadInner2SupS   = 8, // (Pad => SecL) => (SubS[io] => SupS)
			       kPadOuter2SupS   = 9, // -"- 
			       kPadInner2Tpc    =10, // (Pad => SecL) => (SubS[io] => SupS => Tpc)
			       kPadOuter2Tpc    =11, // -"- 
			       kPadInner2Glob   =12, // (Pad => SecL) => (SubS[io] => SupS => Tpc => Glob)
			       kPadOuter2Glob   =13, // -"- 
			       kTotalTpcSectorRotaions =14}; 
 private:
  Char_t                mBeg[1];        //!
  StMagUtilities*       mExB;           //!
  Int_t                 m_Debug;        //!
  TGeoTranslation      *mSwap[2];       //! 
  TGeoHMatrix          *mFlip;          //!
  TGeoHMatrix          *mTpc2GlobMatrix;//!
  TGeoHMatrix          *mHalf[2];       //!
  TGeoHMatrix          *mTpcSectorRotations[24][kTotalTpcSectorRotaions]; //!
  Float_t               mDriftVel[2];   //!
  UInt_t                mUc;            //! time for which above mDriftVel have been calculated
  Int_t                 mTriggerId;     //! to distinguish local clock and RHIC clock
  Double_t              mzGG;           //! Gating Grid z
  Char_t                mEnd[1];        //!
  static Bool_t         mOldScheme;     //! switch between Old and New alignment scheme
 private:
  StTpcDb();
 public:
  virtual ~StTpcDb();
  St_tpcPadPlanesC      *PadPlaneGeometry() {return St_tpcPadPlanesC::instance();}
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
  Float_t                triggerTimeOffset()     {return St_trgTimeOffsetC::instance()->triggerTimeOffset();}
  Float_t                triggerTimeOffsetWest() {return St_trgTimeOffsetC::instance()->triggerTimeOffsetWest();}
  static Bool_t          IsOldScheme()    {return mOldScheme;}
#if 0
  Float_t                ScaleY();
#endif
  Double_t               zGG() {return mzGG;}
  //small pieces of data:
  void    SetDriftVelocity();
#if 0
  Float_t DriftVelocity(Int_t sector=24, Double_t Y = 0);
#else
  Float_t DriftVelocity(Int_t sector=24);
#endif
  StMagUtilities* ExB() {return mExB;}
  void SetExB(StMagUtilities *m) {mExB = m;}
  void SetTpcRotations();
  void SetTpc2GlobalMatrix(TGeoHMatrix *m) {SetTpcRotationMatrix(m);}
  void SetTpcRotationMatrix(TGeoHMatrix *m, Int_t sector = 0, Int_t k = kSupS2Tpc) {
    if (sector == 0)  {if (m) *mTpc2GlobMatrix = *m;}
    else              {if (m) *mTpcSectorRotations[sector-1][k] = *m;}
  }
  void  SetDebug(Int_t m) {m_Debug = m;}
  Int_t Debug() {return m_Debug;}
  void  SetTriggerId(Int_t m) {mTriggerId = m;}
  Int_t TriggerId() {return mTriggerId;}
  const TGeoHMatrix &Flip()                           const {return *mFlip;}
  const TGeoHMatrix &TpcHalf(StBeamDirection part)    const {return *mHalf[part];}
  const TGeoTranslation &Swap(StBeamDirection part)   const {return *mSwap[part];}
  const TGeoHMatrix &Tpc2GlobalMatrix()               const {return *mTpc2GlobMatrix;}
  const TGeoHMatrix &TpcRot(Int_t sector, Int_t k)    const {return *mTpcSectorRotations[sector-1][k];}
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
  ClassDef(StTpcDb,0)
};
#endif
