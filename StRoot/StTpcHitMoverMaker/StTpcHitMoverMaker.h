#ifndef StTpcHitMoverMaker
#define StTpcHitMoverMaker

#include "StMaker.h"
#include "TString.h"

class StSectorAligner;
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
    static const char cvs[]="Tag $Name:  $ $Id: StTpcHitMoverMaker.h,v 1.6 2008/01/09 19:06:04 jeromel Exp $ built "__DATE__" "__TIME__;
    return cvs;
  }
 protected:

 private:
  TString  mInputDataSetName;
  TString  mInputHitName;
  Bool_t   mAlignSector;
  Int_t    mOutputMode;

  StSectorAligner*  mSectorAligner; //!
  StMagUtilities*   mExB; //!

  void moveTpcHit(Float_t pos[3], Float_t posMoved[3],
		  Short_t sector, Short_t row);


  ClassDef(StTpcHitMover,0)
};

#endif
