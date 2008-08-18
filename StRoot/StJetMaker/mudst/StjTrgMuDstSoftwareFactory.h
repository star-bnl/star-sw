// -*- mode: c++;-*-
// $Id: StjTrgMuDstSoftwareFactory.h,v 1.2 2008/08/18 06:20:46 tai Exp $
#ifndef STJTRGMUDSTSOFTWAREFACTORY_H
#define STJTRGMUDSTSOFTWAREFACTORY_H

#include <TObject.h>

class StjTrgMuDstSoftware;

class StjTrgMuDstSoftwareFactory : public TObject {

public:
  StjTrgMuDstSoftwareFactory() { }
  virtual ~StjTrgMuDstSoftwareFactory() { }

  virtual StjTrgMuDstSoftware* create() = 0;

  ClassDef(StjTrgMuDstSoftwareFactory, 1)

};


#endif // STJTRGMUDSTSOFTWAREFACTORY_H
