// -*- mode: c++;-*-
// $Id: StjTrgSoftwareFactory.h,v 1.2 2008/08/02 22:43:44 tai Exp $
#ifndef STJTRGSOFTWAREFACTORY_H
#define STJTRGSOFTWAREFACTORY_H

class StjTrgSoftware;

class StjTrgSoftwareFactory {

public:
  StjTrgSoftwareFactory() { }
  virtual ~StjTrgSoftwareFactory() { }

  virtual StjTrgSoftware* create() = 0;

};


#endif // STJTRGSOFTWAREFACTORY_H
