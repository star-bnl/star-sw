// $Id: StEemcGammaFilterMaker.h,v 1.4 2014/08/06 11:43:14 jeromel Exp $

#ifndef STAR_StEemcGammaFilterMaker
#define STAR_StEemcGammaFilterMaker

/*!
*                                                                     
* \class  StEemcGammaFilterMaker
* \author Michael Betancourt, MIT 
* \author Alice Bridgeman, Argonne Lab
* \author Ilya Selyuzhenkov, Indiana U. CEEM
* \date   2010/07/21
* \brief  BFC level Endcap EMC gamma filter
*
*/                                                                      

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
      static const char cvs[]="Tag $Name:  $ $Id: StEemcGammaFilterMaker.h,v 1.4 2014/08/06 11:43:14 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
      return cvs;
    }
    
    void setThresholds(double seed, double cluster);
    void setMaxVertex(double maxVertex);
    void setEEMCSamplingFraction(double f=0.048);//change to 4.8% hard coded
    
    Int_t makeEEMC(StEmcCollection *emcCollection, double zVertex);
    
    bool getUseDbParams() const {return mUseDbParams;}
    void setUseDbParams(bool use = true) {mUseDbParams = use;}
    
  private:
    TVector3 *mMomentum;
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

// $Log: StEemcGammaFilterMaker.h,v $
// Revision 1.4  2014/08/06 11:43:14  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.3  2010/08/09 21:51:22  seluzhen
// updated comment field
//
// Revision 1.2  2010/08/09 21:38:21  seluzhen
// Gets sampling fraction from Fast simulator, db settings moved to Init
//
