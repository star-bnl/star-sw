// -*- mode: c++;-*-
// $Id: StjDijetListWriter.h,v 1.1 2008/08/22 16:13:59 tai Exp $
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
  Double_t _dphi;
  Double_t _vertexZ;
  Int_t    _jet3Id;
  Int_t    _jet4Id;

  ClassDef(StjDijetListWriter, 1)

};

#endif // STJDIJETLISTWRITER_H
