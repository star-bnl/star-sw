/**********************************************************
 * $Id: StRichRings.h,v 2.2 2000/11/01 17:42:08 lasiuk Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichRings.h,v $
 *  Revision 2.2  2000/11/01 17:42:08  lasiuk
 *  return containers and 3vectors by reference where applicable
 *
 *  Revision 2.1  2000/09/29 01:35:38  horsley
 *  Many changes, added StRichRingHits, StRichMcSwitch, TpcHitvecUtilities
 *  Modified the StRichCalculator, StRichTracks, StRichMCTrack, StRichRingPoint
 *
 *  Revision 1.2  2000/05/22 15:14:44  horsley
 *  modified StRichRings, StRichTDrawableRings to comply with sun compiler
 *
 *  Revision 1.1  2000/05/19 19:06:11  horsley
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
 *  Revision 1.1  2000/04/03 19:36:09  horsley
 *  initial revision 
 **********************************************************/

#ifndef StRichRings_H
#define StRichRings_H

#include "StParticleDefinition.hh"
#include <vector>
#include "StThreeVectorF.hh"
#include "StRrsMaker/StRichGeometryDb.h"

class StRichTrack;
class TPolyLine;

class StRichRings {

public:
  StRichRings(StRichTrack* , StParticleDefinition* );  
  ~StRichRings();
  
  vector<StThreeVectorF>& getInnerPoints(int );
  vector<StThreeVectorF>& getOuterPoints(int );

  StRichTrack* getTrack();
  StParticleDefinition* getParticle();
  
private:
  StRichTrack*          mTrack;
  StParticleDefinition* mParticle;
  StRichGeometryDb*     myGeometryDb;

  vector<StThreeVectorF > mInnerPoints;
  vector<StThreeVectorF > mOuterPoints;  

  bool inBounds(StThreeVectorF& );  
};

#endif











