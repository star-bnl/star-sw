/*!
 * \class StPmdDetector
 * \author Subhasis Chattopadhyay
 */
/********************************************
 *
 * $Id: StPmdDetector.h,v 1.1 2002/08/27 12:22:01 subhasis Exp $
 *
 * Author:  Subhasis Chattopadhyay
 ********************************************
 *
 * Description: Base class for PMD detector
 *
 ********************************************
 * $Log: StPmdDetector.h,v $
 * Revision 1.1  2002/08/27 12:22:01  subhasis
 * First version
 *
 ********************************************/
#ifndef StPmdDetector_hh
#define StPmdDetector_hh

#include "StObject.h"
class StPmdHit;
class StPmdModule;
class StPmdClusterCollection;

class StPmdDetector : public StObject {
public:
    StPmdDetector();
    StPmdDetector(Int_t, unsigned int);
    ~StPmdDetector();
    
    unsigned int  numberOfModules() const;
    
    bool          addHit(StPmdHit*);
    unsigned int  numberOfHits() const;
    
    StPmdModule*       module(unsigned int);
    Int_t       module_hit(Int_t);

    void setModule(StPmdModule*,int);

    StPmdClusterCollection*         cluster();
    void setCluster(StPmdClusterCollection*);
        
private:
    Int_t            mDetectorId;
    UInt_t                  mNumberOfModules;
    
    Int_t             mModules_NHit[12];
    StPmdModule             *mModules[12];  //! pointer for hits
    StPmdClusterCollection  *mClusters;     //! pointer for clusters
    
    ClassDef(StPmdDetector,1)
};
#endif
