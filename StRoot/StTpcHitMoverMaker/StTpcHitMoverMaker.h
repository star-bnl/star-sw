#ifndef StTpcHitMoverMaker
#define StTpcHitMoverMaker

#include "StMaker.h"
#include "StDbUtilities/StTpcLocalCoordinate.hh"
#include "StDbUtilities/StTpcLocalSectorCoordinate.hh"
#include "StDbUtilities/StTpcLocalSectorAlignedCoordinate.hh"
#include "StDbUtilities/StGlobalCoordinate.hh"

class StTpcCoordinateTransform;

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
  virtual void  FlushDB();
  static  void moveTpcHit(StTpcLocalCoordinate &coorL, StGlobalCoordinate &coorG);
  static  void moveTpcHit(StTpcLocalCoordinate &coorL, StTpcLocalCoordinate &coorLD);
  virtual const Char_t *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StTpcHitMoverMaker.h,v 1.11 2014/07/27 13:23:09 fisyak Exp $ built " __DATE__ " " __TIME__;
    return cvs;
  }
 protected:

 private:
  static StTpcCoordinateTransform *mTpcTransForm; 

  static Int_t _debug;
  ClassDef(StTpcHitMover,0)
};

#endif
