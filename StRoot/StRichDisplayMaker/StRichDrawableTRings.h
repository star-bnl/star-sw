/**********************************************************
 * $Id: StRichDrawableTRings.h,v 2.2 2000/11/01 16:55:28 lasiuk Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichDrawableTRings.h,v $
 *  Revision 2.2  2000/11/01 16:55:28  lasiuk
 *  add hilite() members which utilize the flags defined in
 *  StEvent.  draw() member also added.  Hits are hilited only
 *  by demand.  Not by default
 *
 *  Revision 2.1  2000/09/29 17:36:58  gans
 *  Modified addHit(), StThreeVector<double> -> StThreeVectorF,other minor stuff
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

#include "StEnumerations.h"

class StRichTrack;
class StRichRings;
class StRichHit;

class StRichDrawableTRings : public TObject {

public:
    StRichDrawableTRings();  
    StRichDrawableTRings(StRichRings& ring);  
    ~StRichDrawableTRings();

    //
    // Draws the rings and loads the hits
    void draw();
    void clear();
    void addHit(StRichHit*);

    //
    // Draws (hilites) the hits
    //
    void hilite();
    void hilite(const StRichHitFlag&);
    
    StRichDrawableTHit* getHit(unsigned int);
    Int_t numberOfHits();
    
    StRichTrack* getTrack();
    StParticleDefinition* getParticle();

  
private:
    TRandom*       mRand;
    TPolyLine*     mInnerRing;
    TPolyLine*     mOuterRing;
    StRichTrack*   mTrack;

    StParticleDefinition* mParticle;
    vector<StRichDrawableTHit*> mHits; //! 
  
};

#endif











