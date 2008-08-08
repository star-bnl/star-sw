// -*- mode: c++;-*-
// $Id: StjTrgMBWriter.h,v 1.4 2008/08/08 22:53:18 tai Exp $
#ifndef STJTRGMBWRITER_H
#define STJTRGMBWRITER_H

#include "StjTrgWriter.h"

#include <Rtypes.h>

class TDirectory;
class TTree;

#include <string>

class StjTrgMuDst;

class StjTrgMBWriter : public StjTrgWriter {

public:

  StjTrgMBWriter(const char *treeName, const char* treeTitle,
		   TDirectory* file, StjTrgMuDst* trg,
		 StjTrgPassCondition* fillCondition)
    : StjTrgWriter(treeName, treeTitle, file, trg, fillCondition)
  { }
  virtual ~StjTrgMBWriter() { }

private:

  virtual void createBranch_trgSpecific(TTree* tree) { }
  virtual void fillBranch_trgSpecific() { }

};

#endif // STJTRGMBWRITER_H
