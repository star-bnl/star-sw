/**********************************************************
 * $Id: StRichTDrawableTrack.h,v 1.1 2000/06/16 02:37:12 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichTDrawableTrack.h,v $
 *  Revision 1.1  2000/06/16 02:37:12  horsley
 *  many additions, added features to pad plane display (MIPS, rings, etc)
 *  along with Geant info. Added StMcRichTrack. Modified access to hit collection.
 *
 *  Revision 1.1  2000/05/19 19:06:11  horsley
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
 *
 **********************************************************/

#ifndef StRichTDrawableTrack_H
#define StRichTDrawableTrack_H

#include "StParticleDefinition.hh"
#include "TObject.h"
#include "TPolyLine.h"
#include "StRichTDrawableRings.h"
#include "StRichDrawableTMip.h"
#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif

class StRichTrack;
//class StRichRings;

class StRichTDrawableTrack : public TObject {

public:
    StRichTDrawableTrack();  
    StRichTDrawableTrack(StRichTrack* track);  
    ~StRichTDrawableTrack();
    StRichDrawableTMip * getProjectedMIP();
    
    StRichTrack* getTrack();
    Int_t numberOfRings();              // returns number of rings in track
    StRichTDrawableRings* getRing(unsigned int);
    StRichTDrawableRings* getRing(StParticleDefinition*);
    
  
private:

    StRichTrack* mTrack;
    vector<StRichTDrawableRings*> mVectorRings; // Vector of rings to be drawn
    StRichDrawableTMip * mProjectedMIP;

};

#endif











