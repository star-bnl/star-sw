////////////////////////////////////////////////////////////
//                                                        //
//    StGammaFilterMaker                                  //
//                                                        //
//    Michael Betancourt                                  //
//    Massachusetts Institute of Technology               //
//                                                        //
//    Maker designed for use with StEvent to abort        //
//    events without clusters that would eventually       //
//    produce StGammaCandidates.  The filter can be       //
//    run on either the BEMC or the EEMC.                 //
//                                                        //
//    IMPORTANT: This code will not run with the default  //
//    BFC as the BFC does not load the StSpinPool         //
//    libraries.  The BFC will fail if the "FiltGamma"    //
//    chain option is selected.  Instead use              //
//    gammaFilterBfc.C, which wraps the BFC in a macro    //
//    providing the necessary shared libraries in         //
//    addition to chain manipulation.                     //
//                                                        //
////////////////////////////////////////////////////////////

#ifndef STAR_StGammaFilterMaker
#define STAR_StGammaFilterMaker

#include "StMaker.h"

using namespace std;

// Forward class declarations
class StEmcCollection;
class StEmcGeom;
class StEmcPosition;
class StMcEmcHitCollection;
class StMcCalorimeterHit;
class TVector3;

class TFile;
class TTree;
class StPythiaEvent;

// Calorimeter geometry globals
const unsigned int nModules = 120;
const unsigned int nBemcTowers = 4800;

class StGammaFilterMaker: public StMaker
{

    public:
    
        StGammaFilterMaker(const char *name = "gammaFilterMaker");
        ~StGammaFilterMaker();
        
        virtual Int_t Init();
        virtual Int_t Make();
        virtual Int_t Finish();
        virtual void  Clear(const Option_t* = "");
        
        virtual const char *GetCVS() const
        {
            static const char cvs[]="$Id: StGammaFilterMaker.h, v1.0 2008/06/25 00:00:00 betan";
            return cvs;
        }

        void setThresholds(double seed, double cluster);
        double BEMCSamplingMultiplier(double eta);

        Int_t makeBEMC(StMcEmcHitCollection *mcEmcCollection, double zVertex);
        
    private:

        TVector3 *mMomentum;

        // Filter thresholds
        double mSeedEnergyThreshold;
        double mClusterEtThreshold;
    
        StMcCalorimeterHit *mBemcTowerHits[nBemcTowers + 1];
        
        StEmcGeom *mBemcGeom;
        StEmcPosition *mPosition;
        
        // Filter counters
        bool mFirst;

        unsigned int mTotal;
        unsigned int mAccepted;

        // Pythia output
        TString mPythiaName;
        TFile *mPythiaFile;
        TTree *mPythiaTree;
        StPythiaEvent *mPythiaEvent;

        void fStorePythia();

    ClassDef(StGammaFilterMaker, 0);
        
};

#endif
