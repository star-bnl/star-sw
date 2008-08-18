// -*- mode: c++;-*-
// $Id: StjTrgSoft.h,v 1.2 2008/08/18 08:50:58 tai Exp $
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

  virtual std::vector<int> towers() { return std::vector<int>(); }
  virtual std::vector<int> towerDsmAdc() { return std::vector<int>(); }
  virtual std::vector<unsigned int> towerAdc() { return std::vector<unsigned int>(); }
  virtual std::vector<double> towerEnergy() { return std::vector<double>(); }
  virtual std::vector<double> towerEt() { return std::vector<double>(); }

  virtual std::vector<int> jetPatches() { return std::vector<int>(); }
  virtual std::vector<int> jetPatchDsmAdc() { return std::vector<int>(); }
  virtual std::vector<unsigned int> jetPatchAdc() { return std::vector<unsigned int>(); }
  virtual std::vector<double> jetPatchEnergy() { return std::vector<double>(); }
  virtual std::vector<double> jetPatchEt() { return std::vector<double>(); }

  virtual void setTrg(StjTrg* trg) { _trg = trg; }

protected:

  StjTrg* _trg;

private:

  ClassDef(StjTrgSoft, 1)

};


#endif // STJTRGMUDSTSOFTWARE_H
