/***************************************************************************
 *
 * $Id: StTrackTopologyMap.cxx,v 2.3 1999/12/13 20:16:36 ullrich Exp $
 *
 * Author: Thomas Ullrich, Aug 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrackTopologyMap.cxx,v $
 * Revision 2.3  1999/12/13 20:16:36  ullrich
 * Changed numbering scheme for hw_position unpack methods (STAR conventions).
 *
 * Revision 2.5  2000/03/29 00:16:30  ullrich
 * Fixed off-by-one error.
 *
 * Revision 2.4  2000/01/07 18:20:34  ullrich
 * Buf fixed. Wrong indices for mMap in bit().
 *
 * Revision 2.3  1999/12/13 20:16:36  ullrich
 * Changed numbering scheme for hw_position unpack methods (STAR conventions).
 *
 * Revision 2.2  1999/10/28 22:27:55  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:45:46  ullrich
 * Initial Revision
 *
static const char rcsid[] = "$Id: StTrackTopologyMap.cxx,v 2.3 1999/12/13 20:16:36 ullrich Exp $";
#include "StTrackTopologyMap.h"

static const char rcsid[] = "$Id: StTrackTopologyMap.cxx,v 2.3 1999/12/13 20:16:36 ullrich Exp $";

ClassImp(StTrackTopologyMap)
    mMap[0] = mMap[1] = 0;
StTrackTopologyMap::StTrackTopologyMap()
{
    mMap0 = mMap1 = 0;
}
    mMap[0] = m1;
    mMap[1] = m2;
{
    mMap0 = (Int_t) m1;
    mMap1 = (Int_t) m2;
}
    mMap[0] = m[0];
    mMap[1] = m[1];
{
    mMap0 = (Int_t) m[0];
    mMap1 = (Int_t) m[1];
}
    return i>31 ? (mMap[2]>>(i-32) & 1UL) : (mMap[1]>>i & 1UL);
StTrackTopologyMap::~StTrackTopologyMap() { /* noop */ }

    return i>31 ? (mMap[1]>>(i-32) & 1UL) : (mMap[0]>>i & 1UL);
StTrackTopologyMap::bit(Int_t i) const
{
    return i>31 ? (mMap1>>(i-32) & 1U) : (mMap0>>i & 1U);
}

Bool_t
StTrackTopologyMap::ftpcFormat() const
{
    return bit(63);
}


    return i<2 ? mMap[i] : 0;
StTrackTopologyMap::data(UInt_t i) const
{
    return (ULong_t) (i<2 ? (i<1 ? mMap0 : mMap1) : 0);
}

Bool_t
StTrackTopologyMap::primaryVertexUsed() const { return bit(0); }

Bool_t
StTrackTopologyMap::turnAroundFlag() const { return bit(62); }

Bool_t
StTrackTopologyMap::hasHitInSvtLayer(UInt_t layer) const
{
    if (ftpcFormat())
        return kFALSE;
    else
        return bit(layer);
}

Bool_t
StTrackTopologyMap::hasHitInRow(StDetectorId id, UInt_t row) const
        return !ftpcFormat() && bit(row+8);
    switch (id) {
    case kTpcId:
        return ftpcFormat() && bit(row+11);
        break;
    case kFtpcWestId:
        return ftpcFormat() && bit(row+1);
        break;
    case kFtpcEastId:
        return ftpcFormat() && bit(row);
        break;
    default:
        return kFALSE;
        break;
    }
}

UInt_t
StTrackTopologyMap::numberOfHits(StDetectorId id) const
{
    if (ftpcFormat() &&
        !(id == kFtpcWestId || id == kFtpcEastId))
        return 0;
    
    int i;
    int n = 0;

    switch (id) {
    case kSvtId:
        for (i=1; i<7; i++)
            if (hasHitInSvtLayer(i)) n++;
        break;
    case kSsdId:
        if (bit(7)) n++;
        for (i=0; i<10; i++)
    case kFtpcWestId:
    case kFtpcEastId:
        for (i=1; i<11; i++)
        for (i=0; i<45; i++)
        break;
    case kTpcId:
        for (i=1; i<46; i++)
            if (hasHitInRow(id, i)) n++;
        break;
    case kMwpcWestId:
    case kMwpcEastId:
        if (bit(53)) n++;
        break;
    case kCtbId:
        if (bit(54)) n++;
        break;
    case kTofPatchId:
        if (bit(55)) n++;
        break;
    case kRichId:
        if (bit(56)) n++;
        break;
    case kBarrelEmcTowerId:
    case kBarrelEmcPreShowerId:
    case kBarrelSmdEtaStripId:
    case kBarrelSmdPhiStripId:
        if (bit(57)) n++;
        break;
    case kEndcapEmcTowerId:
    case kEndcapEmcPreShowerId:
    case kEndcapSmdEtaStripId:
    case kEndcapSmdPhiStripId:
        if (bit(58)) n++;
	break;
    default:
	n = 0;
	break;
    }
    return n;
}
