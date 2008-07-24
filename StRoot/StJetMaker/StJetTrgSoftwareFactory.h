// -*- mode: c++;-*-
// $Id: StJetTrgSoftwareFactory.h,v 1.1 2008/07/24 02:14:50 tai Exp $
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
