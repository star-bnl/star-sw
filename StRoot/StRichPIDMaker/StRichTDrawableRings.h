/**********************************************************
 * $Id: StRichTDrawableRings.h,v 1.1 2000/05/19 19:06:11 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichTDrawableRings.h,v $
 *  Revision 1.1  2000/05/19 19:06:11  horsley
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
 *
 **********************************************************/

#ifndef StRichTDrawableRings_H
#define StRichTDrawableRings_H

#include "StParticleDefinition.hh"
#include "TObject.h"
#include "TPolyLine.h"

class StRichTrack;
class StRichRings;

class StRichTDrawableRings : public TObject {

public:
  StRichTDrawableRings();  
  StRichTDrawableRings(StRichRings& ring);  
  ~StRichTDrawableRings();
  TPolyLine*   getInnerRing();
  TPolyLine*   getOuterRing();
  StRichTrack* getTrack();
  StParticleDefinition* getParticle();

  
private:

  TPolyLine*   mInnerRing;
  TPolyLine*   mOuterRing;
  StRichTrack* mTrack;
  StParticleDefinition* mParticle;
  
};

#endif











