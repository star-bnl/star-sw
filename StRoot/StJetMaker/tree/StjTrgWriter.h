// -*- mode: c++;-*-
// $Id: StjTrgWriter.h,v 1.1 2008/08/11 04:48:32 tai Exp $
#ifndef STJTRGWRITER_H
#define STJTRGWRITER_H

#include "StjTrgPassCondition.h"

#include <Rtypes.h>

class TDirectory;
class TTree;

#include <string>

class StjTrg;

class StjTrgWriter {

public:

  StjTrgWriter(const char *treeName, const char* treeTitle,
	       TDirectory* file, StjTrg* trg,
	       StjTrgPassCondition* fillCondition)
    : _treeName(treeName), _treeTitle(treeName)
    , _file(file)
    , _trg(trg)
    , _fillCondition(fillCondition)
  { }
  virtual ~StjTrgWriter() { }

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

  TDirectory* _file;
  TTree*      _tree;

  StjTrg* _trg;

  Int_t    _runNumber;
  Int_t    _eventId;
  Double_t _vertexZ;
  Int_t    _trigID;
  Double_t _prescale;
  Int_t    _passed;
  Int_t    _hard;
  Int_t    _soft;

  StjTrgPassCondition* _fillCondition;

};

#endif // STJTRGWRITER_H
