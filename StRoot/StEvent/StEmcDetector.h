/*!
 * \class StEmcDetector 
 * \author Akio Ogawa, Jan 2000
 */
/***************************************************************************
 *
 * $Id: StEmcDetector.h,v 2.8 2004/10/15 18:17:58 ullrich Exp $
 *
 * Author: Akio Ogawa, Jan 2000`
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcDetector.h,v $
 * Revision 2.8  2004/10/15 18:17:58  ullrich
 * Increased mMaxNumberOfCrates to 36.
 *
 * Revision 2.7  2004/10/14 20:00:18  ullrich
 * Added member and methods to deal with crate status flags.
 *
 * Revision 2.6  2004/07/20 17:07:49  perev
 * Pavlinov corrs for TBrowser
 *
 * Revision 2.5  2003/09/12 21:54:53  jeromel
 * Zeroing
 *
 * Revision 2.4  2002/02/22 22:56:47  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.3  2001/04/05 04:00:35  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.2  2000/10/26 00:02:24  ullrich
 * Fixed various problems causing I/O failures.
 *
 * Revision 2.1  2000/02/23 17:33:59  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StEmcDetector_hh
#define StEmcDetector_hh

#include "StObject.h"
#include "StEnumerations.h"

class StEmcRawHit;
class StEmcModule;
class StEmcClusterCollection;

class StEmcDetector : public StObject {
public:
    StEmcDetector();
    StEmcDetector(StDetectorId, unsigned int);
    ~StEmcDetector();
    
    StDetectorId  detectorId() const;
    unsigned int  numberOfModules() const;
    
    bool          addHit(StEmcRawHit*);
    unsigned int  numberOfHits() const;
    void          printNumberOfHits() const; // *MENU*
    double        getEnergy(const int pri=0) const; // *MENU*
    
    StEmcModule*       module(unsigned int);
    const StEmcModule* module(unsigned int) const;
    
    StEmcClusterCollection*         cluster();
    const StEmcClusterCollection*   cluster() const;
    
    StEmcCrateStatus crateStatus(int) const;

    void setCluster(StEmcClusterCollection*);
    void setModule(StEmcModule*, int);
    void setCrateStatus(int, StEmcCrateStatus);

    virtual bool  IsFolder() const;    
    virtual void  Browse(TBrowser *b);

protected:
    void clear();
    
private:
    enum { mMaxNumberOfCrates = 36 };
    StDetectorId            mDetectorId;
    UInt_t                  mNumberOfModules;    
    StEmcModule             *mModules[120];
    StEmcClusterCollection  *mClusters;
    StEmcCrateStatus        mCrateStatusFlag[mMaxNumberOfCrates];
    
    ClassDef(StEmcDetector,2)
};
#endif
