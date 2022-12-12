/***************************************************************************
 * $Id: StFstEvtCollection.cxx$
 *
 * Author: Te-Chuan Huang, Aug. 2022
 ****************************************************************************
 * Description:
 * See header file.
 ***************************************************************************/

#include "StFstEvtCollection.h"
#include "StFstRawHit.h"

ClassImp(StFstEvtCollection)

StFstEvtCollection::StFstEvtCollection() {/* no operation*/}

void StFstEvtCollection::addRawHit(StFstRawHit *hit)
{
    if (!hit) return;

    int elecId = hit->getChannelId();
    if (elecId < 0 || elecId >= kFstNumElecIds) return;

    unsigned int w = (unsigned int)hit->getWedge() - 1;
    if (w >= kFstNumWedges) return;

    mRawHits.push_back(hit);
}

unsigned int StFstEvtCollection::numberOfRawHits() const { return mRawHits.size(); }

const StSPtrVecFstRawHit & StFstEvtCollection::rawHits() const { return mRawHits; }
StSPtrVecFstRawHit & StFstEvtCollection::rawHits() { return mRawHits; }

void StFstEvtCollection::print(int option) {
    cout << "  *** Print FST raw hit collection *** " << endl;
}

/***************************************************************************
 * StFstEvtCollection.cxx,v 1.0
 * Revision 1.0 2022/08/25 Te-Chuan Huang
 * Initial version
 ****************************************************************************/
