/***************************************************************************
 *
 * $Id: StTofCollection.cxx,v 2.6 2003/05/23 20:06:12 ullrich Exp $
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
 * $Log: StTofCollection.cxx,v $
 * Revision 2.6  2003/05/23 20:06:12  ullrich
 * Restore plural for data members.
 *
 * Revision 2.5  2003/05/23 16:10:59  ullrich
 * Changed name of access functions.
 *
 * Revision 2.4  2003/05/21 18:22:46  ullrich
 * Major Revision of ToF classes (F. Geurts)
 *
 * Revision 2.3  2001/10/01 19:40:58  ullrich
 * Added methods and members for StTofData.
 *
 * Revision 2.2  2001/04/24 18:20:13  ullrich
 * Added hits and slats to collection.
 *
 * Revision 2.1  2000/12/08 03:52:42  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StTofCollection.h"

static const char rcsid[] = "$Id: StTofCollection.cxx,v 2.6 2003/05/23 20:06:12 ullrich Exp $";

ClassImp(StTofCollection)
    
StTofCollection::StTofCollection() { /* noop */ }

StTofCollection::~StTofCollection() { /* noop */ }

const StSPtrVecTofCell&
StTofCollection::tofCells() const
{
    return mTofCells;
}

StSPtrVecTofCell&
StTofCollection::tofCells()
{
    return mTofCells;
}

const StSPtrVecTofSlat&
StTofCollection::tofSlats() const
{
    return mTofSlats;
}

StSPtrVecTofSlat&
StTofCollection::tofSlats()
{
    return mTofSlats;
}

const StSPtrVecTofHit&
StTofCollection::tofHits() const
{
    return mTofHits;
}

StSPtrVecTofHit&
StTofCollection::tofHits()
{
    return mTofHits;
}

const StSPtrVecTofData&
StTofCollection::tofData() const
{
    return mTofData;
}

StSPtrVecTofData&
StTofCollection::tofData()
{
    return mTofData;
}


// void
// StTofCollection::addUnit(const StTofUnit* aUnit)  
// {
//     if (aUnit) mTofUnits.push_back(aUnit);
// }

void
StTofCollection::addCell(const StTofCell* aCell)  
{
    if (aCell) mTofCells.push_back(aCell);
}

void
StTofCollection::addSlat(const StTofSlat* aSlat)  
{
    if (aSlat) mTofSlats.push_back(aSlat);
}

void
StTofCollection::addHit(const StTofHit* aHit)
{
    if (aHit) mTofHits.push_back(aHit);
}

void
StTofCollection::addData(const StTofData* aData)
{
    if (aData) mTofData.push_back(aData);
}


bool
StTofCollection::cellsPresent() const
{
    return mTofCells.size();
}

bool
StTofCollection::slatsPresent() const
{
    return mTofSlats.size();
}

bool
StTofCollection::hitsPresent() const
{
    return mTofHits.size();
}

bool
StTofCollection::dataPresent() const
{
    return mTofData.size();
}

