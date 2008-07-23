// -*- mode: c++;-*-
// $Id: StJetTrgMBWriter.h,v 1.6 2008/07/23 23:21:04 tai Exp $
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
    : StJetTrgWriter(treeName, treeTitle, trgId, file, trg)
  { }
  virtual ~StJetTrgMBWriter() { }

private:

  virtual void createBranch_trgSpecific(TTree* tree) { }
  virtual void fillBranch_trgSpecific() { }

};

#endif // STJETTRGMBWRITER_H
