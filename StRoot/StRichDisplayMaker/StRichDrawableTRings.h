/**********************************************************
 * $Id: StRichDrawableTRings.h,v 2.0 2000/08/09 16:28:03 gans Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichDrawableTRings.h,v $
 *  Revision 2.0  2000/08/09 16:28:03  gans
 *  Created New Maker for all drawable objects.
 *
 *  Revision 2.0  2000/08/09 16:28:03  gans
 *  Created New Maker for all drawable objects.
 *
 *  Revision 1.2  2000/06/16 02:37:12  horsley
 *  many additions, added features to pad plane display (MIPS, rings, etc)
 *  along with Geant info. Added StMcRichTrack. Modified access to hit collection.
 *
 *  Revision 1.1  2000/05/19 19:06:11  horsley
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
 *
 **********************************************************/

#ifndef StRichDrawableTRings_H
#define StRichDrawableTRings_H

#include "StParticleDefinition.hh"
#include "TObject.h"
#include "TPolyLine.h"
#include "TRandom.h"

// DisplayMaker
#include "StRichDrawableTHit.h"

#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif
class StRichTrack;
class StRichRings;
class StRichHit;

class StRichDrawableTRings : public TObject {

public:
    StRichDrawableTRings();  
    StRichDrawableTRings(StRichRings& ring);  
    void addHit(double,double);
    void draw();
    void clear(); // Clearss Ring?
    void addHit(StRichHit*);
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











