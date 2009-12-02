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
//    To use with the BFC place the maker instantiation   //
//    before the line "#if 0" in bfc() of the bfc.C.      //
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
            static const char cvs[]=" StGammaFilterMaker.h, v1.0 2008/06/25 00:00:00 betan";
            return cvs;
        }

        void useBEMC();
        void useEEMC();
        void setThresholds(double seed, double cluster);
        void setMaxVertex(double maxVertex);
        void setEEMCSamplingFraction(double f);
        double BEMCSamplingMultiplier(double eta);
        
        Int_t makeBEMC(StMcEmcHitCollection *mcEmcCollection, double zVertex);
        
    private:
        // Flags
        bool mUseBemc;
        
        TVector3 *mMomentum;

        // Filter thresholds
        double mMaxVertex;
        double mSeedEnergyThreshold;
        double mClusterEtThreshold;
        double mEemcSamplingFraction;
    
        StMcCalorimeterHit *mBemcTowerHits[nBemcTowers + 1];
        
        StEmcGeom *mBemcGeom;
        StEmcPosition *mPosition;
        
        // Filter counters
        unsigned int mTotal;
        unsigned int mAccepted;
        unsigned int mVertexRejected;
        unsigned int mFilterRejected;

    ClassDef(StGammaFilterMaker, 0);
        
};

#endif
