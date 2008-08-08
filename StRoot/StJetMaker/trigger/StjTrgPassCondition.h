// -*- mode: c++;-*-
// $Id: StjTrgPassCondition.h,v 1.3 2008/08/08 21:16:44 tai Exp $
#ifndef STJTRGPASSCONDITION_H
#define STJTRGPASSCONDITION_H

#include "StjTrgMuDst.h"

class StjTrgPassCondition {
public:
  StjTrgPassCondition(StjTrgMuDst* trg)
    : _trg(trg) { }
  virtual ~StjTrgPassCondition() { }
  virtual bool operator()() = 0;

protected:
  StjTrgMuDst* _trg;
};

class StjTrgPassConditionHardAndSoft : public StjTrgPassCondition {
public:
  StjTrgPassConditionHardAndSoft(StjTrgMuDst* trg)
    : StjTrgPassCondition(trg) { }
  virtual ~StjTrgPassConditionHardAndSoft() { }
  bool operator()() 
  {
    return (_trg->hard() && _trg->soft());
  }

};

class StjTrgPassConditionHardOrSoft : public StjTrgPassCondition {
public:
  StjTrgPassConditionHardOrSoft(StjTrgMuDst* trg)
    : StjTrgPassCondition(trg) { }
  virtual ~StjTrgPassConditionHardOrSoft() { }
  bool operator()() 
  {
    return (_trg->hard() || _trg->soft());
  }

};

class StjTrgPassConditionHardOnly : public StjTrgPassCondition {
public:
  StjTrgPassConditionHardOnly(StjTrgMuDst* trg)
    : StjTrgPassCondition(trg) { }
  virtual ~StjTrgPassConditionHardOnly() { }
  bool operator()() 
  {
    return (_trg->hard());
  }

};

class StjTrgPassConditionSoftOnly : public StjTrgPassCondition {
public:
  StjTrgPassConditionSoftOnly(StjTrgMuDst* trg)
    : StjTrgPassCondition(trg) { }
  virtual ~StjTrgPassConditionSoftOnly() { }
  bool operator()() 
  {
    return (_trg->soft());
  }

};

#endif // STJTRGPASSCONDITION_H
