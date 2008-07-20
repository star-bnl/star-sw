// -*- mode: c++;-*-
// $Id: StJetTrgHTWriter.h,v 1.4 2008/07/20 06:17:09 tai Exp $
#ifndef STJETTRGHTWRITER_H
#define STJETTRGHTWRITER_H

#include "StJetTrgWriter.h"

#include <Rtypes.h>

class TDirectory;
class TTree;

class StMuDstMaker;
class StEmcTriggerMaker;

#include <string>

class StJetTrgHTWriter : public StJetTrgWriter {

public:

  StJetTrgHTWriter(const char *treeName, const char* treeTitle, int trgId, TDirectory* file, StMuDstMaker* uDstMaker, StEmcTriggerMaker* emcTrigMaker)
    : _treeName(treeName), _treeTitle(treeName)
    , _trgId(trgId)
    , _file(file)
    , _uDstMaker(uDstMaker)
    , _emcTrigMaker(emcTrigMaker) { }
  virtual ~StJetTrgHTWriter() { }

  void Init();
  void Make();
  void Finish();
    
private:

  std::string _treeName;
  std::string _treeTitle;
  int _trgId;

  TDirectory* _file;
  TTree*      _tree;

  Int_t    _runNumber;
  Int_t    _eventId;
  Double_t _vertexZ;
  Int_t    _trigID;
  Double_t _prescale;
  Int_t    _passed;
  Int_t    _hard;
  Int_t    _soft;
  Int_t    _nTowers;
  Int_t    _towerId[4800];

  StMuDstMaker* _uDstMaker;
  StEmcTriggerMaker* _emcTrigMaker;

};

#endif // STJETTRGHTWRITER_H
