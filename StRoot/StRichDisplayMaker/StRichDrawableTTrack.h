/**********************************************************
 * $Id: StRichDrawableTTrack.h,v 2.0 2000/08/09 16:28:04 gans Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichDrawableTTrack.h,v $
 *  Revision 2.0  2000/08/09 16:28:04  gans
 *  Created New Maker for all drawable objects.
 *
 *  Revision 2.0  2000/08/09 16:28:04  gans
 *  Created New Maker for all drawable objects.
 *
 *  Revision 1.1  2000/06/16 02:37:12  horsley
 *  many additions, added features to pad plane display (MIPS, rings, etc)
 *  along with Geant info. Added StMcRichTrack. Modified access to hit collection.
 *
 *  Revision 1.1  2000/05/19 19:06:11  horsley
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
 *
 **********************************************************/

#ifndef StRichDrawableTTrack_H
#define StRichDrawableTTrack_H

#include "StParticleDefinition.hh"
#include "TObject.h"
#include "TPolyLine.h"
#include "StRichDrawableTRings.h"
#include "StRichDrawableTMip.h"
#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif

class StRichTrack;
//class StRichRings;

class StRichDrawableTTrack : public TObject {

public:
    StRichDrawableTTrack();  
    StRichDrawableTTrack(StRichTrack* track);  
    ~StRichDrawableTTrack();
    StRichDrawableTMip * getProjectedMIP();
    
    StRichTrack* getTrack();
    Int_t numberOfRings();              // returns number of rings in track
    StRichDrawableTRings* getRing(unsigned int);
    StRichDrawableTRings* getRing(StParticleDefinition*);
    
  
private:

    StRichTrack* mTrack;
    vector<StRichDrawableTRings*> mVectorRings; // Vector of rings to be drawn
    StRichDrawableTMip * mProjectedMIP;

};

#endif







