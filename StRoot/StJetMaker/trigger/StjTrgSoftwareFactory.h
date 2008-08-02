// -*- mode: c++;-*-
// $Id: StjTrgSoftwareFactory.h,v 1.1 2008/08/02 22:21:33 tai Exp $
#ifndef STJETTRGSOFTWAREFACTORY_H
#define STJETTRGSOFTWAREFACTORY_H

class StjTrgSoftware;

class StjTrgSoftwareFactory {

public:
  StjTrgSoftwareFactory() { }
  virtual ~StjTrgSoftwareFactory() { }

  virtual StjTrgSoftware* create() = 0;

};


#endif // STJETTRGSOFTWAREFACTORY_H
