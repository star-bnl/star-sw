// $Id: StjTPCTrackMaker.cxx,v 1.4 2010/05/30 07:10:00 pibero Exp $
#include "StjTPCTrackMaker.h"

#include "StjTPCMuDst.h"

#include "StjTrackListWriter.h"

#include <TDirectory.h>

#include <iostream>

using namespace std;

ClassImp(StjTPCTrackMaker)

StjTPCTrackMaker::StjTPCTrackMaker(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker)
  : StMaker(name)
  , _file(file)
  , _uDstMaker(uDstMaker)
{ }

Int_t StjTPCTrackMaker::Init()
{
  _tpc  = new StjTPCMuDst;

  _writer = new StjTrackListWriter("tpcTracks", _file);

  return kStOk;
}

Int_t StjTPCTrackMaker::Make()
{
  StjTrackList trackList = _tpc->getTrackList();

  _writer->Fill(trackList);

  return kStOk;
}

Int_t StjTPCTrackMaker::Finish()
{
  _writer->Finish();

  return kStOk;
}
