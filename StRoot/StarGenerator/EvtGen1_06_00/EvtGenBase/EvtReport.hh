//--------------------------------------------------------------------------
//
// Environment:
//      This software is part of the EvtGen package developed jointly
//      for the BaBar and CLEO collaborations.  If you use all or part
//      of it, please give an appropriate acknowledgement.
//
// Copyright Information: See EvtGen/COPYRIGHT
//      Copyright (C) 1998      Caltech, UCSB
//
// Module: EvtGen/EvtReport.hh
//
// Description:
//
// Modification history:
//
// Author:      Simon Patton
// Created:     Mon Jun  3 12:45:03 EDT 1996
//
//------------------------------------------------------------------------

#ifndef EVTREPORT_HH
#define EVTREPORT_HH

#include <iostream>

enum EvtGenSeverity {
  EVTGEN_EMERGENCY,           // fatal
  EVTGEN_ALERT,               // requires immediate action
  EVTGEN_CRITICAL,            // serious
  EVTGEN_ERROR,
  EVTGEN_WARNING,
  EVTGEN_NOTICE,              // "normal but significant"
  EVTGEN_INFO,                // informational
  EVTGEN_DEBUG                // debug
};

// function declaration
std::ostream& EvtGenReport(EvtGenSeverity severity,
			   const char* facility = 0);

#endif
