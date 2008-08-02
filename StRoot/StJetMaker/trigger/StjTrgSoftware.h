// -*- mode: c++;-*-
// $Id: StjTrgSoftware.h,v 1.2 2008/08/02 22:43:43 tai Exp $
#ifndef STJTRGSOFTWARE_H
#define STJTRGSOFTWARE_H

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


#endif // STJTRGSOFTWARE_H
