// -*- mode: c++;-*-
// $Id: StjTrgSoftFactory.h,v 1.1 2008/08/18 06:41:06 tai Exp $
#ifndef STJTRGMUDSTSOFTWAREFACTORY_H
#define STJTRGMUDSTSOFTWAREFACTORY_H

#include <TObject.h>

class StjTrgSoft;

class StjTrgSoftFactory : public TObject {

public:
  StjTrgSoftFactory() { }
  virtual ~StjTrgSoftFactory() { }

  virtual StjTrgSoft* create() = 0;

  ClassDef(StjTrgSoftFactory, 1)

};


#endif // STJTRGMUDSTSOFTWAREFACTORY_H
