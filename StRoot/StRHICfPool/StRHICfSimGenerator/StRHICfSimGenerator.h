#ifndef __StRHICfSimGenerator_h__
#define __StRHICfSimGenerator_h__

#include "TFile.h"
#include "TTree.h"
#include "TString.h"
#include "TSystem.h"
#include "TParticle.h"

#include "StarGenerator/BASE/StarGenerator.h"

class StarGenEvent;
class StarGenPPEvent;

class StRHICfSimGenerator : public StarGenerator
{
    public:
        StRHICfSimGenerator( const char *name="StRHICfSimGenerator" );
        ~StRHICfSimGenerator(){};

        int Init();
        int Generate();

        void SetGenFile(TString file);

        int InitTree();

        void SetTotalEventNumber(int num);
        int GetTotalEventNumber();

        /// Return end-of-run statistics
        StarGenStats Stats();

    protected:
        void FillPP( StarGenEvent *event );

        TString mFileName;
        TFile* mGenFile;
        TTree* mGenRunTree;
        TTree* mGenTree;
        Int_t mEventIdx;
        Int_t mSetEventNumber;
        Int_t mTotalEventNumber;

        Int_t mProcessID;
        TClonesArray* mParticleArr;
        TParticle* mParticle;

        ClassDef(StRHICfSimGenerator,0);
};

#endif