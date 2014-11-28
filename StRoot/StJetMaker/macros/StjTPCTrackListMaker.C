#include "StMaker.h"

#include "StjTPCMuDst.h"

#include "StjTrackListWriter.h"

#include <TDirectory.h>

class StjTPCTrackListMaker : public StMaker {

public:

  StjTPCTrackListMaker(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker)
  : StMaker(name), _file(file), _uDstMaker(uDstMaker) { }
  virtual ~StjTPCTrackListMaker() { }

  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StjTPCTrackListMaker.C,v 1.3 2014/08/06 11:43:24 jeromel Exp $ built " __DATE__ " " __TIME__; return cvs;}

private:

  TDirectory* _file;

  StMuDstMaker* _uDstMaker;

  StjTPC*  _tpc;

  StjTrackListWriter* _writer;

public:

  Int_t Init()
  {
    _tpc  = new StjTPCMuDst(_uDstMaker);

    _writer = new StjTrackListWriter("tpcTracks", _file);

    return kStOk;
  }

  Int_t Make()
  {
    StjTrackList trackList = _tpc->getTrackList();

    _writer->Fill(trackList);

    return kStOk;
  }

  Int_t Finish()
  {
    _writer->Finish();

    return kStOk;
  }
    
  ClassDef(StjTPCTrackListMaker, 0)

};
