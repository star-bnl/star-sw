/**********************************************************
 * $Id: StRichRings.h,v 1.2 2000/05/22 15:14:44 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichRings.h,v $
 *  Revision 1.2  2000/05/22 15:14:44  horsley
 *  modified StRichRings, StRichTDrawableRings to comply with sun compiler
 *
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











