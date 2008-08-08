// -*- mode: c++;-*-
// $Id: StjTrgMuDstSoftwareFactory.h,v 1.1 2008/08/08 23:18:50 tai Exp $
#ifndef STJTRGMUDSTSOFTWAREFACTORY_H
#define STJTRGMUDSTSOFTWAREFACTORY_H

class StjTrgMuDstSoftware;

class StjTrgMuDstSoftwareFactory {

public:
  StjTrgMuDstSoftwareFactory() { }
  virtual ~StjTrgMuDstSoftwareFactory() { }

  virtual StjTrgMuDstSoftware* create() = 0;

};


#endif // STJTRGMUDSTSOFTWAREFACTORY_H
