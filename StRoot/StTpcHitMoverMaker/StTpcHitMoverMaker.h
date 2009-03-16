#ifndef StTpcHitMoverMaker
#define StTpcHitMoverMaker

#include "StMaker.h"
#include "StDbUtilities/StTpcLocalCoordinate.hh"
#include "StDbUtilities/StTpcLocalSectorCoordinate.hh"
#include "StDbUtilities/StTpcLocalSectorAlignedCoordinate.hh"
#include "StDbUtilities/StGlobalCoordinate.hh"

class StTpcCoordinateTransform;
class StMagUtilities;

//! StTpcHitMoverMaker - implements corrections on TPC hits
/*!
  A more detailled description...
*/

class StTpcHitMover : public StMaker {
 public:
  StTpcHitMover(const char *name="tpc_hit_mover");
  virtual ~StTpcHitMover();

  virtual Int_t Init();
  virtual Int_t InitRun(Int_t runnumber);
  virtual Int_t Make();
  virtual Int_t Finish();
  virtual void  AlignHits(Bool_t flag=kFALSE){mAlignSector=flag;}
  virtual void  FlushDB();


  void setInputDataSetName(const Char_t *inputDataSetName);
  inline TString getInputDataSetName() const {return mInputDataSetName;}

  void setInputHitName(const Char_t *inputHitName);
  inline TString getInputHitName() const {return mInputHitName;}

  void setOutputMode(Int_t mode);
  inline Int_t getOutputMode() const {return mOutputMode;}

  void setExB(StMagUtilities* ExB) {mExB = ExB;}
  inline StMagUtilities* getExB() const {return mExB;}

  virtual const Char_t *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StTpcHitMoverMaker.h,v 1.7 2009/03/16 14:16:44 fisyak Exp $ built "__DATE__" "__TIME__;
    return cvs;
  }
 protected:

 private:
  TString  mInputDataSetName;
  TString  mInputHitName;
  Bool_t   mAlignSector;
  Int_t    mOutputMode;
  StTpcCoordinateTransform *mTpcTransForm; 
  void moveTpcHit(StTpcLocalCoordinate &coorL, StGlobalCoordinate &coorG);

  StMagUtilities*   mExB; //!


  ClassDef(StTpcHitMover,0)
};

#endif
