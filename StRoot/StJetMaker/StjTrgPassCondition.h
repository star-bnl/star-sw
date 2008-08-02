// -*- mode: c++;-*-
// $Id: StjTrgPassCondition.h,v 1.1 2008/08/02 04:07:08 tai Exp $
#ifndef STJETTRGPASSCONDITION_H
#define STJETTRGPASSCONDITION_H

#include "StjTrg.h"

class StJetTrgPassCondition {
public:
  StJetTrgPassCondition(StJetTrg* trg)
    : _trg(trg) { }
  virtual ~StJetTrgPassCondition() { }
  virtual bool operator()() = 0;

protected:
  StJetTrg* _trg;
};

class StJetTrgPassConditionHardAndSoft : public StJetTrgPassCondition {
public:
  StJetTrgPassConditionHardAndSoft(StJetTrg* trg)
    : StJetTrgPassCondition(trg) { }
  virtual ~StJetTrgPassConditionHardAndSoft() { }
  bool operator()() 
  {
    return (_trg->hard() && _trg->soft());
  }

};

class StJetTrgPassConditionHardOrSoft : public StJetTrgPassCondition {
public:
  StJetTrgPassConditionHardOrSoft(StJetTrg* trg)
    : StJetTrgPassCondition(trg) { }
  virtual ~StJetTrgPassConditionHardOrSoft() { }
  bool operator()() 
  {
    return (_trg->hard() || _trg->soft());
  }

};

class StJetTrgPassConditionHardOnly : public StJetTrgPassCondition {
public:
  StJetTrgPassConditionHardOnly(StJetTrg* trg)
    : StJetTrgPassCondition(trg) { }
  virtual ~StJetTrgPassConditionHardOnly() { }
  bool operator()() 
  {
    return (_trg->hard());
  }

};

class StJetTrgPassConditionSoftOnly : public StJetTrgPassCondition {
public:
  StJetTrgPassConditionSoftOnly(StJetTrg* trg)
    : StJetTrgPassCondition(trg) { }
  virtual ~StJetTrgPassConditionSoftOnly() { }
  bool operator()() 
  {
    return (_trg->soft());
  }

};

#endif // STJETTRGPASSCONDITION_H
