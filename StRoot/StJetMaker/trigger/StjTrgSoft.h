// -*- mode: c++;-*-
// $Id: StjTrgSoft.h,v 1.1 2008/08/18 06:41:06 tai Exp $
#ifndef STJTRGMUDSTSOFTWARE_H
#define STJTRGMUDSTSOFTWARE_H

#include <TObject.h>

#include <vector>

class StjTrg;

class StjTrgSoft : public TObject {

public:
  StjTrgSoft() { }
  virtual ~StjTrgSoft() { }

  virtual bool soft() = 0;

  virtual std::vector<int> towers()  = 0;
  virtual std::vector<int> towerDsmAdc()  = 0;
  virtual std::vector<unsigned int> towerAdc()  = 0;
  virtual std::vector<double> towerEnergy()  = 0;
  virtual std::vector<double> towerEt()  = 0;

  virtual std::vector<int> jetPatches() = 0;
  virtual std::vector<int> jetPatchDsmAdc()  = 0;
  virtual std::vector<unsigned int> jetPatchAdc()  = 0;
  virtual std::vector<double> jetPatchEnergy()  = 0;
  virtual std::vector<double> jetPatchEt()  = 0;

  virtual void setTrg(StjTrg* trg) { _trg = trg; }

protected:

  StjTrg* _trg;

private:

  ClassDef(StjTrgSoft, 1)

};


#endif // STJTRGMUDSTSOFTWARE_H
