// $Id: StJetTPCTrackMaker.cxx,v 1.7 2008/07/24 20:57:03 tai Exp $
#include "StJetTPCTrackMaker.h"

#include "StJetTPCMuDst.h"

#include "StJetTrackListWriter.h"

#include <TDirectory.h>

#include <iostream>

using namespace std;
using namespace StSpinJet;

ClassImp(StJetTPCTrackMaker)

StJetTPCTrackMaker::StJetTPCTrackMaker(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker)
  : StMaker(name)
  , _file(file)
  , _uDstMaker(uDstMaker)
{ }

Int_t StJetTPCTrackMaker::Init()
{
  _tpc  = new StJetTPCMuDst(_uDstMaker);

  _writer = new StJetTrackListWriter("tpcTracks", _file);

  return kStOk;
}

Int_t StJetTPCTrackMaker::Make()
{
  TrackList trackList = _tpc->getTrackList();

  _writer->Fill(trackList);

  return kStOk;
}

Int_t StJetTPCTrackMaker::Finish()
{
  _writer->Finish();

  return kStOk;
}
