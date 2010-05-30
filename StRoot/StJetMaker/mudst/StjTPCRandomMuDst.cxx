// $Id: StjTPCRandomMuDst.cxx,v 1.2 2010/05/30 07:10:06 pibero Exp $
#include "StjTPCRandomMuDst.h"

#include <StMuDSTMaker/COMMON/StMuTrack.h>
#include <StMuDSTMaker/COMMON/StMuDst.h>
#include <StMuDSTMaker/COMMON/StMuDstMaker.h>
#include <StMuDSTMaker/COMMON/StMuEvent.h>

#include <mudst/StMuEmcPosition.h>
#include <StEmcUtil/geometry/StEmcGeom.h>

#include <TVector3.h>
#include <stdio.h>

StjTPCRandomMuDst::StjTPCRandomMuDst(StMuDstMaker* uDstMaker, Double_t randomSelectorProb, bool randomSelectorAt, UInt_t randomSelectorSeed)
  : StjTPCMuDst(),
    _randomSelector( randomSelectorProb, randomSelectorAt, randomSelectorSeed )
{
}

StjTrackList StjTPCRandomMuDst::getTrackList()
{
    StjTrackList ret;

    //StMuDst* uDst = _uDstMaker->muDst();

    //double magneticField = uDst->event()->magneticField()/10.0; // Tesla
    double magneticField = StMuDst::event()->magneticField()/10.0; // Tesla
    StMuTrack * mutrack = 0;
    TObjArray tracks;

    int nTracks = StMuDst::numberOfPrimaryTracks();

    //	I considered removing this and replacing it with simply a call to set
    //	the random selector container on the primary tracks of the StMuDst, but
    //	I don't know how that will bias the results when the cuts are made in
    //	the while loop below.  I think that it might be better to make the cuts
    //	building the data set that is then eventually used to actually grab the
    //	random tracks.
    for ( int ii = 0; ii < nTracks; ++ii )
    {
	mutrack = StMuDst::primaryTracks(ii);

	if ( mutrack->flag() < 0 )
	    continue;

	if (    mutrack->topologyMap().trackFtpcEast()
	     || mutrack->topologyMap().trackFtpcWest()
	)
	    continue;

	tracks.AddLast( mutrack );
    }

//	_randomSelector.SetContainer( uDstMaker->muDst()->primaryTracks() );
    _randomSelector.SetContainer( &tracks );

    while (
	(mutrack = dynamic_cast<StMuTrack *>(_randomSelector.GetNextRandom()))
    )
    {
	UInt_t trackIndex =
	      _randomSelector.GetNumberReturned()
	    + _randomSelector.GetNumberSkipped()
	    - 1;

	StjTrack track = createTrack(mutrack, trackIndex, magneticField);

	ret.push_back(track);
    }

    return ret;
}
