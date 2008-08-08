// -*- mode: c++;-*-
// $Id: StjTrgPassCondition.h,v 1.4 2008/08/08 22:53:19 tai Exp $
#ifndef STJTRGPASSCONDITION_H
#define STJTRGPASSCONDITION_H

#include "StjTrg.h"

class StjTrgPassCondition {
public:
  StjTrgPassCondition() { }
  virtual ~StjTrgPassCondition() { }
  virtual bool operator()(const StjTrg* trg) = 0;

};

class StjTrgPassConditionHardAndSoft : public StjTrgPassCondition {
public:
  StjTrgPassConditionHardAndSoft() { }
  virtual ~StjTrgPassConditionHardAndSoft() { }
  bool operator()(const StjTrg* trg) 
  {
    return (trg->hard() && trg->soft());
  }

};

class StjTrgPassConditionHardOrSoft : public StjTrgPassCondition {
public:
  StjTrgPassConditionHardOrSoft() { }
  virtual ~StjTrgPassConditionHardOrSoft() { }
  bool operator()(const StjTrg* trg) 
  {
    return (trg->hard() || trg->soft());
  }

};

class StjTrgPassConditionHardOnly : public StjTrgPassCondition {
public:
  StjTrgPassConditionHardOnly() { }
  virtual ~StjTrgPassConditionHardOnly() { }
  bool operator()(const StjTrg* trg) 
  {
    return (trg->hard());
  }

};

class StjTrgPassConditionSoftOnly : public StjTrgPassCondition {
public:
  StjTrgPassConditionSoftOnly() { }
  virtual ~StjTrgPassConditionSoftOnly() { }
  bool operator()(const StjTrg* trg) 
  {
    return (trg->soft());
  }

};

#endif // STJTRGPASSCONDITION_H
