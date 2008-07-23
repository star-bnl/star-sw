// -*- mode: c++;-*-
// $Id: StJetTrgJPWriter.h,v 1.5 2008/07/23 02:34:04 tai Exp $
#ifndef STJETTRGJPWRITER_H
#define STJETTRGJPWRITER_H

#include "StJetTrgWriter.h"

#include <Rtypes.h>

class TDirectory;
class TTree;

#include <string>

class StJetTrg;

class StJetTrgJPWriter : public StJetTrgWriter {

public:

  StJetTrgJPWriter(const char *treeName, const char* treeTitle, int trgId, TDirectory* file, StJetTrg* trg)
    : _treeName(treeName), _treeTitle(treeName)
    , _trgId(trgId)
    , _file(file)
    , _trg(trg)
  { }
  virtual ~StJetTrgJPWriter() { }

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
  Int_t    _nJetPatches;
  Int_t    _jetPatchId[12];

  StJetTrg* _trg;

};

#endif // STJETTRGJPWRITER_H
