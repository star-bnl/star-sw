/****************************************************************
 * $Id: StRichDrawableTMip.h,v 2.1 2000/08/09 23:26:08 gans Exp $
 *
 * Description:
 *   The MIP which is drawn in the pad monitor
 *
 ****************************************************************
 *
 * $Log: StRichDrawableTMip.h,v $
 * Revision 2.1  2000/08/09 23:26:08  gans
 * Added Description comments for Inspect
 *
 * Revision 2.1  2000/08/09 23:26:08  gans
 * Added Description comments for Inspect
 *
 * Revision 2.0  2000/08/09 16:28:03  gans
 * Created New Maker for all drawable objects.
 *
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
#include "TText.h"
class StRichTrack;
class StRichDrawableTTrack;
class StRichMCTrack;

class StRichDrawableTMip : public TMarker {
public:
    StRichDrawableTMip();
    StRichDrawableTMip(StRichDrawableTTrack*);

    void clearRings();     // *MENU*
    void drawPionRing();   // *MENU*
    void drawKaonRing();   // *MENU*
    void drawProtonRing(); // *MENU*
    void drawAllRings();   // *MENU*
    void findRealMip();    // *MENU*
    void clearRealMip();   // *MENU*
    void clearMomText();   // *MENU*
    
    virtual ~StRichDrawableTMip();

    virtual StRichDrawableTTrack * getDrawableTTrack();
protected:
    double mThreeMomMag;//|p| of track
    double mTheta;//Theta From Rich Normal Degrees
    double mPhi;//Phi From Rich X Degrees
    double mEta;//pseudorapidity global
    double mXImpactRadiator;//Radiator Impact X Local
    double mYImpactRadiator;//Radiator Impact Y Local
    int mFastEnoughPion;//Pion Above Cherenkov Threshold
    int mFastEnoughKaon;//Kaon Above Cherenkov Threshold
    int mFastEnoughProton;//Proton Above Cherenkov Threshold

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
    
    StRichDrawableTTrack * mTrackPointer; //!
    TLine * mTLineMIPPointer; //!
    TMarker * mTMarkerMIPPointer; //!
    StRichMCTrack * geantTrackP; //!
    TText * mMomText;//!
protected:

    ClassDef(StRichDrawableTMip,1)
};


#endif /* TMIP_H */
#endif /* ROOT */
