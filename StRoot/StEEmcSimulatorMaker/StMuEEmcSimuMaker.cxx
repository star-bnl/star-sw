/////////////////////////////////////////////////////////////////////////////
//
// StMuEEmcSimuMaker
//
// Author: Jason C. Webb <jwebb@iucf.indiana.edu>
//
// StMuEEmcSimuMaker takes the geant energy stored in the micro-dst and
// converts it into an ADC response based on eemc database information.
// 
// For now, the code simply inverts energy = (adc-ped)/gain.  Future 
// versions will improve on this... i.e. realistic pedestals, simulated
// photostatistics, simulated attenuation along fibers, etc...
//
/////////////////////////////////////////////////////////////////////////////

#include "StMuEEmcSimuMaker.h"

/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

// $Log: StMuEEmcSimuMaker.cxx,v $
// Revision 1.2  2004/05/26 21:28:37  jwebb
// o Changes to StEEmcFastMaker to provide methods to get sampling fraction,
//   gains, etc...
//
// o StMuEEmcSimuMaker is now just a shell of its former self
//
// o Added StMuEEmcSimuReMaker.  This maker takes a muDst as input, and uses
//   the database maker to "massage" the ADC response, to better simulate
//   the calorimeter as installed.  For now, it simply uses the geant
//   energy response, combined with a single sampling fraction and the
//   database gains and pedestals to come up with a new ADC response.
//
// Revision 1.1  2004/05/03 21:36:03  jwebb
// StMuEEmcSimuMaker -- maker to override the ADC values stored in Monte Carlo
// MuDsts with values calculated from gains in an instance of the StEEmcDbMaker.
//
