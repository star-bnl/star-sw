// -*- mode: c++;-*-
// $Id: StjMCParticleListToFourVecList.h,v 1.1 2008/08/22 18:36:20 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJMCPARTICLELISTTOFOURVECLIST_H
#define STJMCPARTICLELISTTOFOURVECLIST_H

#include <TObject.h>

#include "StjMCParticleList.h"
#include "StjFourVecList.h"

#include <utility>

class StjMCParticleListToFourVecList : public TObject {

public:

  StjFourVecList operator()(const StjMCParticleList& mcparticleList);

  ClassDef(StjMCParticleListToFourVecList, 1)

};

#endif // STJMCPARTICLELISTTOFOURVECLIST_H
