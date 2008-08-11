// -*- mode: c++;-*-
// $Id: StjTrgMBWriter.h,v 1.1 2008/08/11 04:48:31 tai Exp $
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
		 StjTrgPassCondition* fillCondition)
    : StjTrgWriter(treeName, treeTitle, file, trg, fillCondition)
  { }
  virtual ~StjTrgMBWriter() { }

private:

  virtual void createBranch_trgSpecific(TTree* tree) { }
  virtual void fillBranch_trgSpecific() { }

};

#endif // STJTRGMBWRITER_H
