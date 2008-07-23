// -*- mode: c++;-*-
// $Id: StJetTrgWriter.h,v 1.2 2008/07/23 23:21:05 tai Exp $
#ifndef STJETTRGWRITER_H
#define STJETTRGWRITER_H

#include "StJetTrgPassCondition.h"

#include <Rtypes.h>

class TDirectory;
class TTree;

#include <string>

class StJetTrg;

class StJetTrgWriter {

public:

  StJetTrgWriter(const char *treeName, const char* treeTitle, int trgId, TDirectory* file, StJetTrg* trg)
    : _treeName(treeName), _treeTitle(treeName)
    , _trgId(trgId)
    , _file(file)
    , _trg(trg)
    , _fillCondition(new StJetTrgPassConditionHardOrSoft(trgId, trg))
    , _passCondition(new StJetTrgPassConditionHardAndSoft(trgId, trg))
  { }
  StJetTrgWriter() { }
  virtual ~StJetTrgWriter() { }

  virtual void Init();
  virtual void Make();
  virtual void Finish();

private:

  virtual TTree* createTree();
  virtual void createBranch_general(TTree* tree);
  virtual void createBranch_trgSpecific(TTree* tree)  = 0;

  virtual void fillBranch_general();
  virtual void fillBranch_trgSpecific() = 0;

  std::string _treeName;
  std::string _treeTitle;
  int _trgId;

  TDirectory* _file;
  TTree*      _tree;

  StJetTrg* _trg;

  Int_t    _runNumber;
  Int_t    _eventId;
  Double_t _vertexZ;
  Int_t    _trigID;
  Double_t _prescale;
  Int_t    _passed;
  Int_t    _hard;
  Int_t    _soft;

  StJetTrgPassCondition* _fillCondition;
  StJetTrgPassCondition* _passCondition;

};

#endif // STJETTRGWRITER_H
