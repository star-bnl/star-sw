/*!
 * \class StEmcCollection 
 * \author Akio Ogawa, Jan 2000
 */
/***************************************************************************
 *
 * $Id: StEmcCollection.h,v 2.5 2004/03/26 21:54:15 ullrich Exp $
 *
 * Author: Akio Ogawa, Jan 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcCollection.h,v $
 * Revision 2.5  2004/03/26 21:54:15  ullrich
 * Added StEmcRawData to collection.
 *
 * Revision 2.4  2002/02/22 22:56:47  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.3  2001/04/05 04:00:35  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.2  2000/03/23 22:24:06  akio
 * Initial version of Emc Point, and Inclusion of track pointers
 *
 * Revision 2.1  2000/02/23 17:34:07  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StEmcCollection_hh
#define StEmcCollection_hh

#include "StObject.h"
#include "StContainers.h"
#include "StEnumerations.h"
#include "StEmcPoint.h"
#include "StEmcRawData.h"

class StEmcDetector;

class StEmcCollection : public StObject {
public:
    StEmcCollection();
    ~StEmcCollection();
    
    StEmcDetector*            detector(StDetectorId);
    const StEmcDetector*      detector(StDetectorId) const;
    
    StSPtrVecEmcPoint&        barrelPoints();
    const StSPtrVecEmcPoint&  barrelPoints() const;
    StSPtrVecEmcPoint&        endcapPoints();
    const StSPtrVecEmcPoint&  endcapPoints() const;
    
    StEmcRawData*             bemcRawData();
    StEmcRawData*             eemcRawData();
    
    void addBarrelPoint(const StEmcPoint*);
    void addEndcapPoint(const StEmcPoint*);
  
    void setDetector(StEmcDetector*);
    
    void setBemcRawData(StEmcRawData*);
    void setEemcRawData(StEmcRawData*);
    

private:
    StEmcCollection(const StEmcCollection&);
    StEmcCollection& operator=(const StEmcCollection&);
    
private:
    StEmcDetector*            mDetector[8];
    StSPtrVecEmcPoint         mBarrel;
    StSPtrVecEmcPoint         mEndcap;
    StEmcRawData*             mBemcRawData;
    StEmcRawData*             mEemcRawData;

    ClassDef(StEmcCollection,2)
};
#endif








