/*!
 * \class StTofCollection 
 * \author Thomas Ullrich, Dec 2000
 */
/***************************************************************************
 *
 * $Id: StTofCollection.h,v 2.6 2003/05/23 16:10:59 ullrich Exp $
 *
 * Author: Thomas Ullrich, Dec 2000
 ***************************************************************************
 *
 * Description:
 *
 * Persistent data which is written into StEvent
 * directly from the reco chain. All ToF stuff goes here
 * except the StTofPidTraits and the StTofSoftwareMonitor.
 *
 ***************************************************************************
 *
 * $Log: StTofCollection.h,v $
 * Revision 2.6  2003/05/23 16:10:59  ullrich
 * Changed name of access functions.
 *
 * Revision 2.5  2003/05/21 18:22:46  ullrich
 * Major Revision of ToF classes (F. Geurts)
 *
 * Revision 2.4  2002/02/22 22:56:51  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.3  2001/10/01 19:40:58  ullrich
 * Added methods and members for StTofData.
 *
 * Revision 2.2  2001/04/24 18:20:13  ullrich
 * Added hits and slats to collection.
 *
 * Revision 2.1  2000/12/08 03:52:43  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StTofCollection_hh
#define StTofCollection_hh

#include "StObject.h"
#include "StContainers.h"
#include "StEnumerations.h"
#include "StTofHit.h"
#include "StTofCell.h"
#include "StTofSlat.h"
#include "StTofData.h"

class StTofCollection : public StObject {
public:
    StTofCollection();
    ~StTofCollection();

    //const StSPtrVecTofUnit&    tofUnits() const;
    //StSPtrVecTofUnit&          tofUnits();

    const StSPtrVecTofCell&    tofCells() const;
    StSPtrVecTofCell&          tofCells();

    const StSPtrVecTofSlat&    tofSlats() const;
    StSPtrVecTofSlat&          tofSlats();
    
    const StSPtrVecTofHit&     tofHits() const;
    StSPtrVecTofHit&           tofHits();

    const StSPtrVecTofData&    tofData() const;
    StSPtrVecTofData&          tofData();

    void addSlat(const StTofSlat*);
    void addCell(const StTofCell*);
    void addHit(const StTofHit*);
    void addData(const StTofData*); 

    bool cellsPresent()    const;
    bool slatsPresent()    const;
    bool hitsPresent()     const;
    bool dataPresent()     const;
    
private:
    StSPtrVecTofSlat           mTofSlat;
    StSPtrVecTofCell           mTofCell;
    StSPtrVecTofHit            mTofHits;
    StSPtrVecTofData           mTofData;
  
    ClassDef(StTofCollection, 3)
};
#endif
