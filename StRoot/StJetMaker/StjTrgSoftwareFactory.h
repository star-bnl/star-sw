// -*- mode: c++;-*-
// $Id: StjTrgSoftwareFactory.h,v 1.1 2008/08/02 04:07:53 tai Exp $
#ifndef STJETTRGSOFTWAREFACTORY_H
#define STJETTRGSOFTWAREFACTORY_H

class StJetTrgSoftware;

class StJetTrgSoftwareFactory {

public:
  StJetTrgSoftwareFactory() { }
  virtual ~StJetTrgSoftwareFactory() { }

  virtual StJetTrgSoftware* create() = 0;

};


#endif // STJETTRGSOFTWAREFACTORY_H
