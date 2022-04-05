////////////////////////////////////////////////////////////
//                                                        //
//    StGammaRawMaker                                     //
//                                                        //
//    Process and store raw detector information          //
//                                                        //
//    Original concept and implementation by Jason        //
//    Webb (Valpo) and Pibero Djawatho (IUCF)             //
//                                                        //
////////////////////////////////////////////////////////////

#ifndef STAR_StGammaRawMaker
#define STAR_StGammaRawMaker

#include "StMaker.h"

#include "StGammaTrack.h"
#include "StGammaTower.h"
#include "StGammaStrip.h"
#include "StGammaEvent.h"
#include "StGammaEventMaker.h"

#include "StEmcUtil/database/StBemcTables.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"

#include <vector>

class EEmcGeomSimple;
class StBemcTables;
class StMuTrack;

class StGammaRawMaker: public StMaker 
{

    public:

        StGammaRawMaker(const char *name = "gammaRawMaker");
        ~StGammaRawMaker();
        
        virtual const char* GetCVS() const
        {static const char cvs[] = "Tag $Name:  $ $Id: StGammaRawMaker.h,v 1.9 2014/08/06 11:43:18 jeromel Exp $ built " __DATE__ " " __TIME__; return cvs; }
        
        // Required Maker Methods
        Int_t Init();
        Int_t Make();
        Int_t Finish() { return kStOK; }
        void  Clear(Option_t *opts="");

        // Accessors
        const StGammaTrackVec_t &tracks(){ return mTracks; }
        const StGammaTowerVec_t &towers(){ return mTowers; }
        const StGammaTowerVec_t &preshower1(){ return mPreshower1; }
        const StGammaTowerVec_t &preshower2(){ return mPreshower2; }
        const StGammaTowerVec_t &postshower(){ return mPostshower; }
        const StGammaStripVec_t &strips(){ return mStrips; }

        StGammaTower *tower(Int_t id, Int_t layer);
        StGammaStrip *strip(Int_t sector, Int_t plane, Int_t index);

        // Mutators
        void SetTowerCutoff( Float_t t );        
        void SetTrackCutoff( Float_t t );
        
        void useBemc() { mUseBemc = true; }
        void useEemc() { mUseEemc = true; }

        void AddEtaStrip(StGammaStrip *strip);
        void AddPhiStrip(StGammaStrip *strip);

        void excludeBemcTower(int softId) { mExcludedBemcTowers.push_back(softId); }
        void shiftBemcGains(double shift) { mBemcGainShift = shift; }

    protected:

        Float_t mTowerCutoff;
        Float_t mTrackCutoff;
        
        StGammaTrackVec_t mTracks; // stores all tracks which pass QA
        StGammaTowerVec_t mTowers; // stores all towers which pass QA
        StGammaStripVec_t mStrips; // stores all strips which pass QA
        
        StGammaTowerVec_t mPreshower1;
        StGammaTowerVec_t mPreshower2;
        StGammaTowerVec_t mPostshower;
        
        void GetTracks();
        void GetBarrel();
        void GetEndcap();
        
        Bool_t Accept( StGammaTrack &track );
        Bool_t Accept( StGammaTower &tower );
        Bool_t Accept( StGammaStrip &strip );
        Bool_t Accept( StMuTrack *track );
        
        EEmcGeomSimple *mEEmcGeometry;
        
        StBemcTables *mTables;
        Bool_t mCorrupt;

        // store pointers to towers and strips for easier matching to clusters
        StGammaTower *mEEtowers[ kEEmcNumSectors * kEEmcNumSubSectors * kEEmcNumEtas ][ 4 ];
        StGammaStrip *mEEstrips[ kEEmcNumSectors ][ kEEmcNumSmdUVs     ][ kEEmcNumStrips ];
        
        StGammaTower* mBarrelEmcTower[4801];
        StGammaTower* mBarrelEmcPreshower[4801];
        
        map<int, StGammaStrip*> mBarrelSmdEtaStrip;
        map<int, StGammaStrip*> mBarrelSmdPhiStrip;
        
    private:
    
        bool mUseBemc;
        bool mUseEemc;

        double mBemcGainShift;

        vector<int> mExcludedBemcTowers;
    
        StMuDstMaker *mMuDstMaker;
        StGammaEventMaker *mGammaMaker;
        StGammaEvent *mGammaEvent;
  
    ClassDef(StGammaRawMaker, 3);

};

inline void StGammaRawMaker::SetTowerCutoff(Float_t t){ mTowerCutoff = t; }
inline void StGammaRawMaker::SetTrackCutoff(Float_t t){ mTrackCutoff = t; }

#endif
