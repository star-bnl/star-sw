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
// Module: EvtGenBase/EvtMTRandomEngine.hh
//
// Generate random numbers using the Mersenne-Twister MT19937.
// Member function random returns a random number in the range ]0..1[.
// This requires c++11 features, hence the EVTGEN_CPP11 preprocessor.
//
// Modification history:
//
//    John Back       Aug 2015            Module created
//
//------------------------------------------------------------------------

#ifndef EVTMTRANDOMENGINE_HH
#define EVTMTRANDOMENGINE_HH

#include "EvtGenBase/EvtRandomEngine.hh"

#include <random>

class EvtMTRandomEngine : public EvtRandomEngine {

public:

    EvtMTRandomEngine(unsigned int seed = 1430957218);

    virtual double random();

private:

    std::mt19937 engine_;

    typedef std::uniform_real_distribution<double> URDist;
    URDist distribution_;

};

#endif

#endif
