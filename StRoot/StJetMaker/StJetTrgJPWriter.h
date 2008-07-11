// -*- mode: c++;-*-
// $Id: StJetTrgJPWriter.h,v 1.1 2008/07/11 23:32:19 tai Exp $
#ifndef STJETTRGJPWRITER_H
#define STJETTRGJPWRITER_H

#include "StJetTrgWriter.h"

#include <Rtypes.h>

class TDirectory;
class TTree;

class StMuDstMaker;
class StEmcTriggerMaker;

#include <string>

class StJetTrgJPWriter : public StJetTrgWriter {

public:

  StJetTrgJPWriter(const char *treeName, const char* treeTitle, int trgId, TDirectory* file, StMuDstMaker* uDstMaker, StEmcTriggerMaker* emcTrigMaker)
    : _treeName(treeName), _treeTitle(treeName)
    , _trgId(trgId)
    , _file(file)
    , _uDstMaker(uDstMaker)
    , _emcTrigMaker(emcTrigMaker) { }
  virtual ~StJetTrgJPWriter() { }

  void Init();
  void Make();
  void Finish();
    
private:

  std::string _treeName;
  std::string _treeTitle;
  int _trgId;

  TDirectory* _file;
  TTree* _tree;

  Int_t _runNumber;
  Int_t _eventId;
  Int_t _trigID;
  Double_t _prescale;
  Int_t _pass;
  Int_t _nJetPatches;
  Int_t _jetPatchId[12];

  StMuDstMaker* _uDstMaker;
  StEmcTriggerMaker* _emcTrigMaker;

};

#endif // STJETTRGJPWRITER_H
