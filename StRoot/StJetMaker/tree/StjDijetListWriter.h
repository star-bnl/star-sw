// -*- mode: c++;-*-
// $Id: StjDijetListWriter.h,v 1.3 2008/09/19 23:19:22 tai Exp $
#ifndef STJDIJETLISTWRITER_H
#define STJDIJETLISTWRITER_H

#include <TObject.h>

#include "StjDijetList.h"

#include <Rtypes.h>

class TDirectory;
class TTree;

class StjDijetListWriter : public TObject {

public:

  StjDijetListWriter(const char* treeName, TDirectory* file);
  virtual ~StjDijetListWriter() { }

  void Fill(const StjDijetList& dijetList);
  void Finish();

private:

  TDirectory* _file;
  TTree* _tree;

  Int_t    _runNumber;
  Int_t    _eventId;
  Int_t    _dijetId;
  Double_t _m;
  Double_t _eta;
  Double_t _costh;
  Double_t _deta;
  Double_t _dphi;
  Double_t _vertexZ;
  Int_t    _jet3Id;
  Int_t    _jet4Id;
  Int_t    _jetSameSideId;
  Int_t    _jetAwaySideId;
  double   _neuRtSame;
  double   _neuRtAway;
  double   _neuRt3;
  double   _neuRt4;

  ClassDef(StjDijetListWriter, 1)

};

#endif // STJDIJETLISTWRITER_H
