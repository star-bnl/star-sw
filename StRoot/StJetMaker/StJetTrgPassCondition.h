// -*- mode: c++;-*-
// $Id: StJetTrgPassCondition.h,v 1.1 2008/07/23 23:21:04 tai Exp $
#ifndef STJETTRGPASSCONDITION_H
#define STJETTRGPASSCONDITION_H

#include "StJetTrg.h"

class StJetTrgPassCondition {
public:
  StJetTrgPassCondition(int trgId, StJetTrg* trg)
    : _trgId(trgId), _trg(trg) { }
  virtual ~StJetTrgPassCondition() { }
  virtual bool operator()() = 0;

protected:
  int _trgId;
  StJetTrg* _trg;
};

class StJetTrgPassConditionHardAndSoft : public StJetTrgPassCondition {
public:
  StJetTrgPassConditionHardAndSoft(int trgId, StJetTrg* trg)
    : StJetTrgPassCondition(trgId, trg) { }
  virtual ~StJetTrgPassConditionHardAndSoft() { }
  bool operator()() 
  {
    return (_trg->hard(_trgId) && _trg->soft(_trgId));
  }

};

class StJetTrgPassConditionHardOrSoft : public StJetTrgPassCondition {
public:
  StJetTrgPassConditionHardOrSoft(int trgId, StJetTrg* trg)
    : StJetTrgPassCondition(trgId, trg) { }
  virtual ~StJetTrgPassConditionHardOrSoft() { }
  bool operator()() 
  {
    return (_trg->hard(_trgId) || _trg->soft(_trgId));
  }

};

class StJetTrgPassConditionHardOnly : public StJetTrgPassCondition {
public:
  StJetTrgPassConditionHardOnly(int trgId, StJetTrg* trg)
    : StJetTrgPassCondition(trgId, trg) { }
  virtual ~StJetTrgPassConditionHardOnly() { }
  bool operator()() 
  {
    return (_trg->hard(_trgId));
  }

};

class StJetTrgPassConditionSoftOnly : public StJetTrgPassCondition {
public:
  StJetTrgPassConditionSoftOnly(int trgId, StJetTrg* trg)
    : StJetTrgPassCondition(trgId, trg) { }
  virtual ~StJetTrgPassConditionSoftOnly() { }
  bool operator()() 
  {
    return (_trg->soft(_trgId));
  }

};

#endif // STJETTRGPASSCONDITION_H
