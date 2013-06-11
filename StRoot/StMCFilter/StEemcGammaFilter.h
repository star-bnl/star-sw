// $Id: StEemcGammaFilter.h,v 1.3 2010/08/09 21:52:21 seluzhen Exp $

#ifndef STAR_StEemcGammaFilter
#define STAR_StEemcGammaFilter

/*!
*                                                                     
* \class  StEemcGammaFilter
* \author Michael Betancourt, MIT 
* \author Alice Bridgeman, Argonne Lab
* \author Ilya Selyuzhenkov, Indiana U. CEEM
* \date   2010/07/21
* \brief  Pythia level Endcap EMC gamma filter
*
*/                                                                      

#include "StMCFilter/StMCFilter.h"


// Forward declarations
class StGenParticleMaster;


class StEemcGammaFilter : public StMCFilter 
{
  
  public:
    
    StEemcGammaFilter();
    virtual ~StEemcGammaFilter() {};
    
    // Reject after vertex sampling
    int RejectGT(const StGenParticleMaster &ptl) const;
    
    virtual const char *GetCVS() const
    {
      static const char cvs[]="Tag $Name:  $ $Id: StEemcGammaFilter.h,v 1.3 2010/08/09 21:52:21 seluzhen Exp $ built "__DATE__" "__TIME__ ; 
      return cvs;
    }
    
  private:
    
    static const double mConeRadius;
    static const double mSeedThreshold;
    static const double mClusterThreshold;
    static const double mEtaLow;
    static const double mEtaHigh;
    static const double mCalDepth;
    static const double mHadronScale;
    static const double mMinPartEnergy;
    static const double mMaxVertex;
    
    int    mPrintLevel;
    int    mFilterMode;
    
};

#endif

// $Log: StEemcGammaFilter.h,v $
// Revision 1.3  2010/08/09 21:52:21  seluzhen
// updated comment field
//

