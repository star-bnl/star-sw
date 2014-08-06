////////////////////////////////////////////////////////////
//                                                        //
//    StBarrelEmcClusterMaker                             //
//                                                        //
//    Michael Betancourt                                  //
//    Massachusetts Institute of Technology               //
//                                                        //
//    Find 3x3 tower cluster in the BEMC                  //
//                                                        //
//    Original concept and implementation by              //
//    Pibero Djawatho (Indiana University)                //
//                                                        //
////////////////////////////////////////////////////////////

#ifndef STAR_StBarrelEmcClusterMaker
#define STAR_StBarrelEmcClusterMaker

class StBarrelEmcCluster;
class StGammaEventMaker;
class StGammaRawMaker;
class StGammaTower;

#include "StMaker.h"

class StBarrelEmcClusterMaker: public StMaker 
{

    public:
    
        StBarrelEmcClusterMaker(const char* name = "mBemcClusterMaker");
        ~StBarrelEmcClusterMaker() {};
        
        virtual const char* GetCVS() const
        {static const char cvs[] = "Tag $Name:  $ $Id: StBarrelEmcClusterMaker.h,v 1.6 2014/08/06 11:43:17 jeromel Exp $ built " __DATE__ " " __TIME__; return cvs; }
        
        // Required Maker Methods
        int  Init();
        void Clear(Option_t* option = "");
        int  Make();
        int  Finish() { return kStOK; }
                
        // Accessors
        vector<StBarrelEmcCluster*>& clusters();
        
        // Mutators
        void setSeedThreshold(double threshold);
        void setClusterThreshold(double threshold);

    private:
    
        float mHighTowerThreshold;
        float mClusterThreshold;
        
        StGammaEventMaker* mGammaEventMaker;
        StGammaRawMaker* mGammaRawMaker;
        TVector3 mVertex;
        vector<StBarrelEmcCluster*> mClusters;
        
        StBarrelEmcCluster* makeCluster(StGammaTower* tower) const;
        void getTowerPosition(int id, TVector3& position) const;

  ClassDef(StBarrelEmcClusterMaker, 2)
  
};

inline vector<StBarrelEmcCluster*>& StBarrelEmcClusterMaker::clusters()  { return mClusters; }
inline void StBarrelEmcClusterMaker::setSeedThreshold(double threshold) { mHighTowerThreshold = threshold; }
inline void StBarrelEmcClusterMaker::setClusterThreshold(double threshold) { mClusterThreshold = threshold; }

#endif
