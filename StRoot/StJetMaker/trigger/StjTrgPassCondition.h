// -*- mode: c++;-*-
// $Id: StjTrgPassCondition.h,v 1.2 2008/08/02 22:43:43 tai Exp $
#ifndef STJTRGPASSCONDITION_H
#define STJTRGPASSCONDITION_H

#include "StjTrg.h"

class StjTrgPassCondition {
public:
  StjTrgPassCondition(StjTrg* trg)
    : _trg(trg) { }
  virtual ~StjTrgPassCondition() { }
  virtual bool operator()() = 0;

protected:
  StjTrg* _trg;
};

class StjTrgPassConditionHardAndSoft : public StjTrgPassCondition {
public:
  StjTrgPassConditionHardAndSoft(StjTrg* trg)
    : StjTrgPassCondition(trg) { }
  virtual ~StjTrgPassConditionHardAndSoft() { }
  bool operator()() 
  {
    return (_trg->hard() && _trg->soft());
  }

};

class StjTrgPassConditionHardOrSoft : public StjTrgPassCondition {
public:
  StjTrgPassConditionHardOrSoft(StjTrg* trg)
    : StjTrgPassCondition(trg) { }
  virtual ~StjTrgPassConditionHardOrSoft() { }
  bool operator()() 
  {
    return (_trg->hard() || _trg->soft());
  }

};

class StjTrgPassConditionHardOnly : public StjTrgPassCondition {
public:
  StjTrgPassConditionHardOnly(StjTrg* trg)
    : StjTrgPassCondition(trg) { }
  virtual ~StjTrgPassConditionHardOnly() { }
  bool operator()() 
  {
    return (_trg->hard());
  }

};

class StjTrgPassConditionSoftOnly : public StjTrgPassCondition {
public:
  StjTrgPassConditionSoftOnly(StjTrg* trg)
    : StjTrgPassCondition(trg) { }
  virtual ~StjTrgPassConditionSoftOnly() { }
  bool operator()() 
  {
    return (_trg->soft());
  }

};

#endif // STJTRGPASSCONDITION_H
