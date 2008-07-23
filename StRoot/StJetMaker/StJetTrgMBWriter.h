// -*- mode: c++;-*-
// $Id: StJetTrgMBWriter.h,v 1.4 2008/07/23 02:34:05 tai Exp $
#ifndef STJETTRGMBWRITER_H
#define STJETTRGMBWRITER_H

#include "StJetTrgWriter.h"

#include <Rtypes.h>

class TDirectory;
class TTree;

#include <string>

class StJetTrg;

class StJetTrgMBWriter : public StJetTrgWriter {

public:

  StJetTrgMBWriter(const char *treeName, const char* treeTitle, int trgId, TDirectory* file, StJetTrg* trg)
    : _treeName(treeName), _treeTitle(treeName)
    , _trgId(trgId)
    , _file(file)
    , _trg(trg)
  { }
  virtual ~StJetTrgMBWriter() { }

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
  Double_t _vertexZ;
  Int_t _trigID;
  Double_t _prescale;
  Int_t _passed;

  StJetTrg* _trg;

};

#endif // STJETTRGMBWRITER_H
