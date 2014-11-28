////////////////////////////////////////////////////////////
//                                                        //
//    StGammaCandidateMaker                               //
//                                                        //
//    Run quality analysis on raw BEMC and EEMC clusters, //
//    associating various detector elements with those    //
//    clusters passing the desired criteria               //
//                                                        //
//    Original concept and implementation by Jason        //
//    Webb (Valpo) and Pibero Djawatho (IUCF)             //
//                                                        //
////////////////////////////////////////////////////////////

#ifndef STAR_StGammaCandidateMaker
#define STAR_StGammaCandidateMaker

class TClonesArray;
class TVector3;
class StGammaTrack;

#include "StMaker.h"
#include "StGammaCandidate.h"

class StGammaCandidateMaker: public StMaker
{

    public:
 
        StGammaCandidateMaker(const char *name = "mGammaCandidateMaker");
        ~StGammaCandidateMaker();
        
        virtual const char* GetCVS() const
        {static const char cvs[] = "Tag $Name:  $ $Id: StGammaCandidateMaker.h,v 1.11 2014/08/06 11:43:17 jeromel Exp $ built " __DATE__ " " __TIME__; return cvs; }
        
        // Required Maker Methods
        Int_t Init();
        Int_t Make();
        void  Clear(Option_t *opts="");
        Int_t Finish() { return kStOK; }
        
        // Mutators
        void SetMinimumEt(Float_t et) { mMinimumEt = et; }
        void SetRadius(Float_t r) { mRadius = r; }
        void SetBsmdRange(Float_t r){ mBsmdRange = r; }
        void SetEsmdRange(Float_t r){ mEsmdRange = r; }
        
        void useBemc() { mUseBemc = true; }
        void useEemc() { mUseEemc = true; }
        
        void useStrictBemcStatus() { mStrictBemcStatus = true; }
        
        enum { kNoCompress, kCompressSmd, kCompressAll  };
        // 0 = No compression
        // 1 = Compress SMD only
        // 2 = Compress All
        void  SetCompressLevel(Int_t level = kCompressSmd ) { mCompressLevel = level; }
        
        Int_t Compress();

    private:
    
    protected:

        bool mUseBemc;
        bool mUseEemc;
        
        bool mStrictBemcStatus;

        Float_t mMinimumEt; // GeV
        Float_t mRadius;    // sqrt( deta**2 + dphi**2 )
        Float_t mBsmdRange;  // eta-phi
        Float_t mEsmdRange;  // cm
        Int_t   mCompressLevel;
        
        Int_t mId;
        Int_t nextId(){ return mId++; }
        
        Int_t MakeBarrel();
        Int_t MakeEndcap();
        template<class T> void Compress(TClonesArray* clones);
        
        // Calculate position of EEMC cluster using intersection of max ESMD strips under
        // cluster seed tower. Returns (0,0,0) for failure.
        TVector3 getEEmcClusterPosition(const StEEmcCluster& cluster);


  ClassDef(StGammaCandidateMaker, 4);

};

#endif
