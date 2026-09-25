#ifndef StRHICfSimDst_HH
#define StRHICfSimDst_HH

#include "TChain.h"
#include "TTree.h"
#include "TObject.h"
#include "TString.h"
#include "TClonesArray.h"

#include "StRHICfSimEvent.h"
#include "StRHICfSimTrack.h"
#include "StRHICfSimBTof.h"
#include "StRHICfSimBBC.h"
#include "StRHICfSimRHICfHit.h"
#include "StRHICfSimRHICfPoint.h"
#include "StRHICfSimZDC.h"

using namespace std;

class StRHICfSimDst : public TObject
{
    public:
        StRHICfSimDst();
        virtual ~StRHICfSimDst();

        void Clear(Option_t* option = "");

        Int_t Init();
        Int_t CreateDstArray(TTree* tree);
        Int_t ReadDstArray(TTree* tree);
        Int_t ReadDstArray(TChain* chain);

        StRHICfSimEvent* GetSimEvent();
        StRHICfSimTrack* GetSimTrack(int idx);
        StRHICfSimBTof* GetSimBTof(int idx);
        StRHICfSimBBC* GetSimBBC();
        StRHICfSimRHICfHit* GetSimRHICfHit();
        StRHICfSimRHICfPoint* GetSimRHICfPoint(int idx);
        StRHICfSimZDC* GetSimZDC();

        Int_t GetSimTrackNum();
        Int_t GetSimBTofNum();
        Int_t GetSimRHICfPointNum();
    
    private:
        StRHICfSimEvent* mSimEvent = 0;
        TClonesArray* mSimTrackArray = 0;
        TClonesArray* mSimBTofArray = 0;
        StRHICfSimBBC* mSimBBC = 0;
        StRHICfSimRHICfHit* mSimRHICfHit = 0;
        TClonesArray* mSimRHICfPointArray = 0;
        StRHICfSimZDC* mSimZDC = 0;
        
    ClassDef(StRHICfSimDst, 1)
};

#endif