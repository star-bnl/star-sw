// -*- mode: c++;-*-
// $Id: StjTrgPassCondition.h,v 1.6 2008/09/21 19:11:47 tai Exp $
#ifndef STJTRGPASSCONDITION_H
#define STJTRGPASSCONDITION_H

#include <TObject.h>

#include "StjTrg.h"

class StjTrgPassCondition : public TObject {
public:
  StjTrgPassCondition() { }
  virtual ~StjTrgPassCondition() { }
  virtual bool operator()(const StjTrg* trg) = 0;

  ClassDef(StjTrgPassCondition, 1)
};

class StjTrgPassConditionHardAndSoft : public StjTrgPassCondition {
public:
  StjTrgPassConditionHardAndSoft() { }
  virtual ~StjTrgPassConditionHardAndSoft() { }
  bool operator()(const StjTrg* trg) 
  {
    return (trg->hard() && trg->soft());
  }

  ClassDef(StjTrgPassConditionHardAndSoft, 1)
};

class StjTrgPassConditionHardOrSoft : public StjTrgPassCondition {
public:
  StjTrgPassConditionHardOrSoft() { }
  virtual ~StjTrgPassConditionHardOrSoft() { }
  bool operator()(const StjTrg* trg) 
  {
    return (trg->hard() || trg->soft());
  }

  ClassDef(StjTrgPassConditionHardOrSoft, 1)
};

class StjTrgPassConditionHardOnly : public StjTrgPassCondition {
public:
  StjTrgPassConditionHardOnly() { }
  virtual ~StjTrgPassConditionHardOnly() { }
  bool operator()(const StjTrg* trg) 
  {
    return (trg->hard());
  }

  ClassDef(StjTrgPassConditionHardOnly, 1)
};

class StjTrgPassConditionSoftOnly : public StjTrgPassCondition {
public:
  StjTrgPassConditionSoftOnly() { }
  virtual ~StjTrgPassConditionSoftOnly() { }
  bool operator()(const StjTrg* trg) 
  {
    return (trg->soft());
  }

  ClassDef(StjTrgPassConditionSoftOnly, 1)
};

class StjTrgPassConditionPass : public StjTrgPassCondition {
public:
  StjTrgPassConditionPass() { }
  virtual ~StjTrgPassConditionPass() { }
  bool operator()(const StjTrg* trg) 
  {
    return (trg->passed());
  }

  ClassDef(StjTrgPassConditionPass, 1)
};

#endif // STJTRGPASSCONDITION_H
