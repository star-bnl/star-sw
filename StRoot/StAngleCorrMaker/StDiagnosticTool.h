#ifndef StDiagnosticTool_HH
#define StDiagnosticTool_HH

///////////////////////////////////////////////////////////////////////////////
//
// StDiagnosticTool
//
// Description: 
//
// Environment:
//  Software developed for the STAR Detector at Brookhaven National Laboratory
//
// Author List: 
//  Craig Ogilvie, MIT
//  Matt Horsley,  YALE
// History:
//
///////////////////////////////////////////////////////////////////////////////

#include "StEvent.h"
#include <TNtuple.h>


class StDiagnosticTool {
public:
                  StDiagnosticTool();
                  ~StDiagnosticTool();
  void        Fill(StEvent& ev);
  TString GetName();

private:
  
};

#endif
