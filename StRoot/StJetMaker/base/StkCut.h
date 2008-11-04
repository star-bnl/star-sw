// -*- mode: c++;-*-
// $Id: StkCut.h,v 1.1 2008/11/04 05:54:40 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STKCUT_H
#define STKCUT_H

#include <TObject.h>

template <class T>
class StkCut : public TObject {

public:
  StkCut() { }
  virtual ~StkCut() { }

  virtual bool operator()(const T& t) = 0;

private:

  ClassDef(StkCut, 1)
};


#endif // STKCUT_H
