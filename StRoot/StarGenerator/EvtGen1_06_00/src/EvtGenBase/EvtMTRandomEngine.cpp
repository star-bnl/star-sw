#ifdef EVTGEN_CPP11
//--------------------------------------------------------------------------
//
// Environment:
//      This software is part of the EvtGen package. If you use all or part
//      of it, please give an appropriate acknowledgement.
//
// Copyright Information: See EvtGen/COPYRIGHT
//      Copyright (C) 2015      University of Warwick, UK
//
// Module: EvtMTRandomEngine
//
// Generate random numbers using the Mersenne-Twister MT19937. 
// This requires c++11 features, hence the EVTGEN_CPP11 preprocessor.
//
// Modification history:
//
//    John Back       Aug 2015            Module created
//
//------------------------------------------------------------------------
//
#include "EvtGenBase/EvtMTRandomEngine.hh"

#include "EvtGenBase/EvtPatches.hh"
#include "EvtGenBase/EvtReport.hh"

#include <iostream>

EvtMTRandomEngine::EvtMTRandomEngine(unsigned int seed) :
    engine_(seed),
    distribution_(URDist(0.0, 1.0))
{

    EvtGenReport(EVTGEN_INFO,"EvtMTRandomEngine")
	<<"Mersenne-Twister random number generator with seed = "<<seed<<std::endl;

}

double EvtMTRandomEngine::random() {
  
    return distribution_(engine_);

}

#endif
