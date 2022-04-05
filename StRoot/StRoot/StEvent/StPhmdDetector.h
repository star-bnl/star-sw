/*!
 * \class StPhmdDetector
 * \author Subhasis Chattopadhyay
 */
/********************************************************************
 *
 * $Id: StPhmdDetector.h,v 2.2 2003/04/22 00:08:13 ullrich Exp $
 *
 * Author: Subhasis Chattopadhyay, Dec 2002
 ********************************************************************
 *
 * Description: Base class for PMD detector
 *
 ********************************************************************
 *
 * $Log: StPhmdDetector.h,v $
 * Revision 2.2  2003/04/22 00:08:13  ullrich
 * Removed //! comments
 *
 * Revision 2.1  2002/12/20 22:33:00  ullrich
 * Initial Revision.
 *
 ********************************************************************/
#ifndef StPhmdDetector_hh
#define StPhmdDetector_hh

#include "StObject.h"
#include "StEnumerations.h"

class StPhmdHit;
class StPhmdModule;
class StPhmdClusterCollection;

class StPhmdDetector : public StObject {
public:
    StPhmdDetector();
    StPhmdDetector(StDetectorId);
    ~StPhmdDetector();

    StDetectorId  id() const; 
    unsigned int  numberOfModules() const;
    
    bool          addHit(StPhmdHit*);
    unsigned int  numberOfHits() const;
    
    StPhmdModule*                   module(unsigned int);
    const StPhmdModule*             module(unsigned int) const;
    int                             moduleHits(unsigned int);

    StPhmdClusterCollection*        cluster();
    const StPhmdClusterCollection*  cluster() const;

    void setCluster(StPhmdClusterCollection*);
    void setModule(StPhmdModule*, unsigned int);

        
private:
    enum {mMaxModules = 12};
    StDetectorId             mDetectorId;
    
    Int_t                    mModulesNHit[mMaxModules];
    StPhmdModule*            mModules[mMaxModules];  // pointer for hits
    StPhmdClusterCollection* mClusters;              // pointer for clusters
    
    ClassDef(StPhmdDetector,1)
};

inline StDetectorId StPhmdDetector::id() const
{return mDetectorId;}

#endif
