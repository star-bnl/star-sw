// -*- mode: c++;-*-
// $Id: StjTrgMBWriter.h,v 1.1 2008/08/02 04:07:01 tai Exp $
#ifndef STJETTRGMBWRITER_H
#define STJETTRGMBWRITER_H

#include "StjTrgWriter.h"

#include <Rtypes.h>

class TDirectory;
class TTree;

#include <string>

class StJetTrg;

class StJetTrgMBWriter : public StJetTrgWriter {

public:

  StJetTrgMBWriter(const char *treeName, const char* treeTitle,
		   TDirectory* file, StJetTrg* trg,
		   StJetTrgPassCondition* fillCondition,
		   StJetTrgPassCondition* passCondition)
    : StJetTrgWriter(treeName, treeTitle, file, trg, fillCondition, passCondition)
  { }
  virtual ~StJetTrgMBWriter() { }

private:

  virtual void createBranch_trgSpecific(TTree* tree) { }
  virtual void fillBranch_trgSpecific() { }

};

#endif // STJETTRGMBWRITER_H
