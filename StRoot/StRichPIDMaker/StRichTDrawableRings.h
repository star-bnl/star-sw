/**********************************************************
 * $Id: StRichTDrawableRings.h,v 1.2 2000/06/16 02:37:12 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichTDrawableRings.h,v $
 *  Revision 1.2  2000/06/16 02:37:12  horsley
 *  many additions, added features to pad plane display (MIPS, rings, etc)
 *  along with Geant info. Added StMcRichTrack. Modified access to hit collection.
 *
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
#include "StRchMaker/StRichDrawableTHit.h"
#include "TRandom.h"

#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif

class StRichTrack;
class StRichRings;

class StRichTDrawableRings : public TObject {

public:
    StRichTDrawableRings();  
    StRichTDrawableRings(StRichRings& ring);  
    ~StRichTDrawableRings();
    void draw();
    void clear(); // Clearss Ring?
    void addHit(double,double);
    StRichDrawableTHit* getHit(unsigned int);
    Int_t numberOfHits();
    
    StRichTrack* getTrack();
    StParticleDefinition* getParticle();

  
private:
    TRandom* rand;
    TPolyLine*   mInnerRing;
    TPolyLine*   mOuterRing;
    StRichTrack* mTrack;
    StParticleDefinition* mParticle;
    vector<StRichDrawableTHit*> mHits;  
  
};

#endif











