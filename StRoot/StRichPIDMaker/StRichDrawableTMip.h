/****************************************************************
 * $Id: StRichDrawableTMip.h,v 1.1 2000/06/16 02:37:11 horsley Exp $
 *
 * Description:
 *   The MIP which is drawn in the pad monitor
 *
 ****************************************************************
 *
 * $Log: StRichDrawableTMip.h,v $
 * Revision 1.1  2000/06/16 02:37:11  horsley
 * many additions, added features to pad plane display (MIPS, rings, etc)
 * along with Geant info. Added StMcRichTrack. Modified access to hit collection.
 *
 * Revision 1.0  2000/05/25 21:35:32  gans
 * Intitial
 *
 *******************************************************************/
 #ifdef __ROOT__
#ifndef ST_RICH_DRAWABLE_TMIP_H
#define ST_RICH_DRAWABLE_TMIP_H


#include "TMarker.h"
#include "TLine.h"
class StRichTrack;
class StRichTDrawableTrack;
class StRichMCTrack;

class StRichDrawableTMip : public TMarker {
public:
    StRichDrawableTMip();
    StRichDrawableTMip(StRichTDrawableTrack*);

    void clearRings();     // *MENU*
    void drawPionRing();   // *MENU*
    void drawKaonRing();   // *MENU*
    void drawProtonRing(); // *MENU*
    void drawAllRings();   // *MENU*
    void findRealMip();    // *MENU*
    void clearRealMip();    // *MENU*
    
    virtual ~StRichDrawableTMip();

    virtual StRichTDrawableTrack * getTDrawableTrack();
protected:
    double mThreeMomMag;
    double mTheta,mPhi;
    double mXImpactRadiator,mYImpactRadiator;
    int mFastEnoughPion,mFastEnoughKaon,mFastEnoughProton;

    double mGeantThreeMomMag;
    double mGeantX;
    double mGeantY;
    double mGeantResidual;
    double mGeantTheta,mGeantPhi;
    double mGeantXImpactRadiator,mGeantYImpactRadiator;
    double mGeantXStopVertex,mGeantYStopVertex;
    long mGeantStopVertexProcess;
    unsigned int mGeantStopVertexNumDaught;
        
    int mGeantCommonTpcHits,mGeantNumberOfPartners,
	mGeantHitsInRadiator,mGeantHitsInGap;

    long mGeantTrackID;
    
    StRichTDrawableTrack * mTrackPointer; //!
    TLine * mTLineMIPPointer; //!
    TMarker * mTMarkerMIPPointer; //!
    StRichMCTrack * geantTrackP; //!
protected:

    ClassDef(StRichDrawableTMip,1)
};


#endif /* TMIP_H */
#endif /* ROOT */
