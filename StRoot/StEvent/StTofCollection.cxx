/***************************************************************************
 *
 * $Id: StTofCollection.cxx,v 2.10 2008/06/03 17:41:28 ullrich Exp $
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
 * Revision 2.10  2008/06/03 17:41:28  ullrich
 * Assign defaults values in constructor.
 *
 * Revision 2.9  2007/08/27 22:57:49  ullrich
 * Fixed problem in numberOfVpdEast() and numberOfVpdWest() - X. Dong
 *
 * Revision 2.8  2007/04/03 18:16:48  ullrich
 * Add new data members and methods in preperation for new ToF.
 *
 * Revision 2.7  2005/04/11 22:35:25  calderon
 * Tof Classes for Run 5.  Modifications and additions from Xin to
 * take care of new TOF daq and electronics.  Added StTofRawData and
 * modified containers and includes.
 *
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

static const char rcsid[] = "$Id: StTofCollection.cxx,v 2.10 2008/06/03 17:41:28 ullrich Exp $";

ClassImp(StTofCollection)
    
StTofCollection::StTofCollection() {
    mVpdEast = 0;
    mVpdWest = 0;
    mTstart = -999.;
    mTdiff = -999.;
    mVzVpd = -999.;
}

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

const StSPtrVecTofRawData&
StTofCollection::tofRawData() const
{
  return mTofRawData;
}

StSPtrVecTofRawData&
StTofCollection::tofRawData()
{
  return mTofRawData;
}

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

void
StTofCollection::addRawData(const StTofRawData* aRawData)
{
    if (aRawData) mTofRawData.push_back(aRawData);
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

bool
StTofCollection::rawdataPresent() const
{
    return mTofRawData.size();
}

unsigned int
StTofCollection::numberOfVpdEast() const
{
    unsigned int num = 0;
    for(int i=0;i<32;i++) {
       num += mVpdEast>>i & 1;
    }
    return num;
}

unsigned int
StTofCollection::numberOfVpdWest() const
{
    unsigned int num = 0;
    for(int i=0;i<32;i++) {
       num += mVpdWest>>i & 1;
    }
    return num;
}
