// -*- mode: c++;-*-
// $Id: StkListCut.h,v 1.1 2008/11/04 05:54:40 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STKLISTCUT_H
#define STKLISTCUT_H

#include <TObject.h>

#include "StkCut.h"

#include <vector>

template <class T>
class StkListCut : public TObject {

public:
  StkListCut() { }
  virtual ~StkListCut() { }
  
  std::vector<T> operator()(const std::vector<T>& aList)
  {
    std::vector<T> ret;
    for(typename std::vector<T>::const_iterator it = aList.begin(); it != aList.end(); ++it)
      {
	if(shouldNotKeep(*it)) continue;
	ret.push_back(*it);
      }
    return ret;
  }

  void addCut(StkCut<T>* cut) { _cutList.push_back(cut); }

  typedef std::vector<StkCut<T>*> CutList;
  CutList getCutList() { return _cutList; }

private:

  bool shouldNotKeep(const T& t)
  {
    for(typename CutList::iterator cut = _cutList.begin(); cut != _cutList.end(); ++cut)
      {
	if((**cut)(t)) return true;
      }
  return false;
  }

  CutList _cutList;

  ClassDef(StkListCut, 1)

};

#endif // STKLISTCUT_H
