// -*- mode: c++;-*-
// $Id: StjTrgMuDstSoftware.h,v 1.1 2008/08/08 23:12:25 tai Exp $
#ifndef STJTRGMUDSTSOFTWARE_H
#define STJTRGMUDSTSOFTWARE_H

#include <vector>

class StjTrgMuDstSoftware {

public:
  StjTrgMuDstSoftware() { }
  virtual ~StjTrgMuDstSoftware() { }

  virtual bool soft(int trgId) = 0;

  virtual std::vector<int> towers(int trgId)  = 0;

  virtual std::vector<int> jetPatches(int trgId) = 0;

private:

};


#endif // STJTRGMUDSTSOFTWARE_H
