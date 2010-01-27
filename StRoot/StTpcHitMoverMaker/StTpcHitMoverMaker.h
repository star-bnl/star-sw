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
  virtual void  FlushDB();
  virtual const Char_t *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StTpcHitMoverMaker.h,v 1.8 2010/01/27 21:40:04 fisyak Exp $ built "__DATE__" "__TIME__;
    return cvs;
  }
 protected:

 private:
  StTpcCoordinateTransform *mTpcTransForm; 
  void moveTpcHit(StTpcLocalCoordinate &coorL, StGlobalCoordinate &coorG);

  StMagUtilities*   mExB; //!

  ClassDef(StTpcHitMover,0)
};

#endif
