// -*- mode: c++;-*-
// $Id: StjTrgMuDstSoftware.h,v 1.2 2008/08/17 11:29:15 tai Exp $
#ifndef STJTRGMUDSTSOFTWARE_H
#define STJTRGMUDSTSOFTWARE_H

#include <TObject.h>

#include <vector>

class StjTrgMuDstSoftware : public TObject {

public:
  StjTrgMuDstSoftware() { }
  virtual ~StjTrgMuDstSoftware() { }

  virtual bool soft(int trgId) = 0;

  virtual std::vector<int> towers(int trgId)  = 0;
  virtual std::vector<int> towerDsmAdc(int trgId)  = 0;

  virtual std::vector<int> jetPatches(int trgId) = 0;
  virtual std::vector<int> jetPatchDsmAdc(int trgId)  = 0;

private:

  ClassDef(StjTrgMuDstSoftware, 1)

};


#endif // STJTRGMUDSTSOFTWARE_H
