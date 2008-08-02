// -*- mode: c++;-*-
// $Id: StjTrgMBWriter.h,v 1.2 2008/08/02 22:43:43 tai Exp $
#ifndef STJTRGMBWRITER_H
#define STJTRGMBWRITER_H

#include "StjTrgWriter.h"

#include <Rtypes.h>

class TDirectory;
class TTree;

#include <string>

class StjTrg;

class StjTrgMBWriter : public StjTrgWriter {

public:

  StjTrgMBWriter(const char *treeName, const char* treeTitle,
		   TDirectory* file, StjTrg* trg,
		   StjTrgPassCondition* fillCondition,
		   StjTrgPassCondition* passCondition)
    : StjTrgWriter(treeName, treeTitle, file, trg, fillCondition, passCondition)
  { }
  virtual ~StjTrgMBWriter() { }

private:

  virtual void createBranch_trgSpecific(TTree* tree) { }
  virtual void fillBranch_trgSpecific() { }

};

#endif // STJTRGMBWRITER_H
