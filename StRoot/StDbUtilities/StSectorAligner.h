/*
  Little class to 'align' sector by moving hits.
  See StTpcDb/StTpcDb.h for the methods which returns the parameters
  from the db.
 */

#ifndef StSectorAligner_HH
#define StSectorAligner_HH

#include "TObject.h"
#include "StTpcDb/StTpcDb.h"

#include "StThreeVectorF.hh"
class StTpcDb;

class StSectorAligner {
 public: 
  StSectorAligner(StTpcDb* dbin);
  virtual ~StSectorAligner();

  //------ sets
  // we always translate sectors (if the value is not zero).
  // these just indicate if we should translate the outer or inner sector.
  // cant be both.  see the constructer to see which is the default.
  //
  void      setTranslateOuterSector(bool val) { mTranslateOuterSector=val; }
  void      setTranslateInnterSector(bool val) { mTranslateOuterSector=!val; }

  //------ gets
  // do we translate the inner or outer sector?
  //
  bool      translateOuterSector() const { return mTranslateOuterSector; }
  bool      translateInnerSector() const { return !mTranslateOuterSector; }
  
  //-------action
  
  void      moveHit(const float x[], float xprime[3],int sector, int row);
  
 private:
  StTpcDb* thedb;
  int   lastInnerSectorRow; //!
  float innerSectorRotatePoint; //! cm
  float outerSectorRotatePoint; //! cm

  void             rotateHit(float angle,int sector,float radius);
  void             rotateCW(const StThreeVectorF& point,StThreeVectorF& pointR,
			    float angle);
  void             rotateToSector(const StThreeVectorF& point, 
				  StThreeVectorF& pointRot,int sector);
  void             translateHit(float offset,int sector);

  //  StTpcDb*         gTpcDbPtr; //!
  bool             mTranslateOuterSector; //!
  StThreeVectorF*  mVecHit; //! holds the x,y,z position for the hit
  

  ClassDef(StSectorAligner,1)

};
#endif
