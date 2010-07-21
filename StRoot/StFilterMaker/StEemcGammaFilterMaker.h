////////////////////////////////////////////////////////////
//                                                        //
//    StEemcGammaFilterMaker                                  //
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

#ifndef STAR_StEemcGammaFilterMaker
#define STAR_StEemcGammaFilterMaker

#include "StMaker.h"

using namespace std;

// Forward class declarations
class StEmcCollection;
class StEmcGeom;
class StEmcPosition;
class EEmcGeomSimple;
class StMcEmcHitCollection;
class StMcCalorimeterHit;
class TVector3;

// Calorimeter geometry globals

const unsigned int nEemcEtaBins = 12;
const unsigned int nEemcPhiBins = 60;
const unsigned int nEemcTowers = nEemcEtaBins * nEemcPhiBins;

class StEemcGammaFilterMaker: public StMaker
{

    public:
    
        StEemcGammaFilterMaker();
        virtual ~StEemcGammaFilterMaker();
        
        virtual Int_t Init();
        virtual Int_t Make();
        virtual Int_t Finish();
        virtual void  Clear(const Option_t* = "");
        
        virtual const char *GetCVS() const
        {
            static const char cvs[]=" StEemcGammaFilterMaker.h, v1.0 2008/06/25 00:00:00 betan";
            return cvs;
        }

      
        void setThresholds(double seed, double cluster);
        void setMaxVertex(double maxVertex);
        void setEEMCSamplingFraction(double f=0.05);//change to 5% hard coded
            
	Int_t makeEEMC(StEmcCollection *emcCollection, double zVertex);
        
	bool getUseDbParams() const {return mUseDbParams;}
	void setUseDbParams(bool use = true) {mUseDbParams = use;}

    private:
        // Flags
	TVector3 *mMomentum;

        // Filter thresholds
        double mMaxVertex;
        double mSeedEnergyThreshold;
        double mClusterEtThreshold;
        double mEemcSamplingFraction;
	double mEemcTowerHits[nEemcTowers];
        
        EEmcGeomSimple *mEemcGeom;
        
        // Filter counters
        unsigned int mTotal;
        unsigned int mAccepted;
        unsigned int mVertexRejected;
        unsigned int mFilterRejected;
	bool mFilterMode;
	bool mUseDbParams; // whether or not read parameters from the database
	
    ClassDef(StEemcGammaFilterMaker, 0);
        
};

#endif
