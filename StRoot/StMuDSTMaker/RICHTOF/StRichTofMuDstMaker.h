/***************************************************************************
 *
 * $Id: StRichTofMuDstMaker.h,v 1.3 2002/03/18 22:06:34 dunlop Exp $
 *
 * Author: Thomas Ullrich, Oct 2000
 ***************************************************************************
 *
 * Description:  Example program to write miniDSTs based on StEvent.
 *               The miniDST will contain primary K+/K- tracks only.
 *
 ***************************************************************************
 *
 * $Log: StRichTofMuDstMaker.h,v $
 * Revision 1.3  2002/03/18 22:06:34  dunlop
 * Modified lambda cuts to match ones made by StRichPIDMaker and StRichSpectraMaker
 *
 * Revision 1.2  2002/03/10 17:59:33  dunlop
 * More clever with removing of RICH collection and pid traits when not wanted.
 *
 * Revision 1.1  2002/02/20 01:57:27  dunlop
 * New Rich and Tof combined maker
 *
 * Revision 1.2  2000/10/16 19:35:44  ullrich
 * Updated to run on Sun/CC5.
 *
 * Revision 1.1  2000/10/13 19:26:18  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StRichTofMuDstMaker_h
#define StRichTofMuDstMaker_h
#include "StMaker.h"
#include "StThreeVectorD.hh"
#include <vector>
class StEvent;
class StTrack;
class StSPtrVecTofData;
class StTofGeometry;
class StRichGeometryDb;
class StRichCoordinateTransform;
class StV0Vertex;

class StRichGeometryDb;
class StRichCoordinateTransform;
class StRichMomentumTransform;

class StRichTofMuDstMaker : public StMaker {
public:

    StRichTofMuDstMaker(const Char_t *name="StRichTofMuDstMaker");
    Int_t  Init();
    Int_t  InitRun(int runnumber);
    
    Int_t  Make();
    Int_t Finish();

    void rejectStrobeEvents(){mRejectStrobeEvents=true;};
    void acceptStrobeEvents(){mRejectStrobeEvents=false;};
    void SetOuterTrackGeometry(){mOuterTrackGeometry=true;};
    void SetStandardTrackGeometry(){mOuterTrackGeometry=false;};
    void SetTrackAcceptZ(Float_t a,Float_t b){ mTrackAcceptZ_min=a; mTrackAcceptZ_max=b;};
    void SetTrackAcceptPhi(Float_t a, Float_t b){mTrackAcceptPhi_min=a; mTrackAcceptPhi_max=b;};
	
protected:
    bool accept(StEvent*);
    bool acceptTof(StTrack*);
    bool acceptRich(StTrack*);

    unsigned int removeL3Tracks(StEvent*);
    void removeL3Hits(StEvent*);
    bool acceptL3TrackInRich(StTrack*);
    bool acceptLambdaDaughter(StTrack*);
    
    vector<StV0Vertex*> getLambdas(StEvent*);//!
    vector<unsigned int> removeV0sButLambdas(StEvent*,vector<StV0Vertex*>&);//!

    void removeRich(StEvent*);
    


private:
    bool mOuterTrackGeometry; //! use Outer (true, default) or Standard (false) track geometry
    StTofGeometry *mTofGeom; //!
    bool mRejectStrobeEvents; //! don't include TOF strobed events (true, default)
    bool strobeEvent(StSPtrVecTofData); //! determine if an event is strobed (true)
    Float_t mTrackAcceptZ_min;   //! Z and Phi ranges for
    Float_t mTrackAcceptZ_max;   //! global track selection
    Float_t mTrackAcceptPhi_min; //!
    Float_t mTrackAcceptPhi_max; //!
    
    bool mEventAcceptedTof;
    bool mEventAcceptedRich;
    
    StThreeVectorD mRichNormalVectorToPadPlane; //!
    StThreeVectorD mRichGlobalEdgeMin;//!
    StThreeVectorD mRichGlobalEdgeMax;//!
    StThreeVectorD mRichLocation;//!
    
    double mL3PCut;//!
    double mRichPathCut;//!
    
    StRichGeometryDb* mRichGeometryDb;//!
    StRichCoordinateTransform* mRichTrans;//!
    StThreeVectorD mRichPad;
    StThreeVectorD mRichNormal;
    StThreeVectorD mRichRadiator;

    double mPadPlaneCut;
    double mRadiatorCut;
    double mLambdaLastHitCut;
    double mLambdaPathCut;
    double mLambdaFitPointsCut;
    double mLambdaEtaCut;
    
    double mLambdaPCut;
    
    static const unsigned int mTofMaskInActionWord = 0x10;
    static const unsigned int mRichMaskInActionWord = 0x20;
    
    


    
    ClassDef(StRichTofMuDstMaker,1)
};

#endif
