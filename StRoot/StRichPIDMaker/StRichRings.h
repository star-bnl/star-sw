/**********************************************************
 * $Id: StRichRings.h,v 2.0 2000/08/09 16:26:20 gans Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichRings.h,v $
 *  Revision 2.0  2000/08/09 16:26:20  gans
 *  Naming Convention for TDrawable Ojects. All drawable objects now in StRichDisplayMaker
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
#include "StThreeVector.hh"

#include "StParticleDefinition.hh"
#include <vector>
#include "StThreeVectorF.hh"
#include "StRrsMaker/StRichGeometryDb.h"

class StRichTrack;
class TPolyLine;

class StRichRings {

  vector<StThreeVector<double> > getInnerPoints(int );
  vector<StThreeVector<double> > getOuterPoints(int );
  ~StRichRings();
  
  vector<StThreeVectorF > getInnerPoints(int );
  vector<StThreeVectorF > getOuterPoints(int );

  StRichTrack* getTrack();
  StParticleDefinition* getParticle();
  
private:
  vector<StThreeVector<double> > mInnerPoints;
  vector<StThreeVector<double> > mOuterPoints;  
  StRichGeometryDb*     myGeometryDb;
  bool inBounds(StThreeVector<double>& );  
  vector<StThreeVectorF > mInnerPoints;
  vector<StThreeVectorF > mOuterPoints;  

  bool inBounds(StThreeVectorF& );  
};

#endif











