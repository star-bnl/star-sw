// -*- mode: c++;-*-
// $Id: StjTrgSoftware.h,v 1.2 2008/08/02 19:22:30 tai Exp $
#ifndef STJETTRGSOFTWARE_H
#define STJETTRGSOFTWARE_H

#include <vector>

class StjTrgSoftware {

public:
  StjTrgSoftware() { }
  virtual ~StjTrgSoftware() { }

  virtual bool soft(int trgId) = 0;

  virtual std::vector<int> towers(int trgId)  = 0;

  virtual std::vector<int> jetPatches(int trgId) = 0;

private:

};


#endif // STJETTRGSOFTWARE_H
