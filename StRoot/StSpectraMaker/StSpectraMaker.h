
// $Id: StSpectraMaker.h,v 1.3 1999/11/22 01:54:58 ogilvie Exp $
//
// $Log: StSpectraMaker.h,v $
// Revision 1.3  1999/11/22 01:54:58  ogilvie
// generalised analysis containers to beany object that inherits from StSpectraAnalysis
//
// Revision 1.2  1999/11/05 18:58:49  ogilvie
// general tidy up following Mike Lisa's review. List of analyses conntrolled via
// analysis.dat, rather than hardcoded into StSpectraMaker.cxx
//
// Revision 1.1  1999/11/03 21:22:42  ogilvie
// initial version
//

#ifndef StSpectraMaker_HH
#define StSpectraMaker_HH

///////////////////////////////////////////////////////////////////////////////
//
// StSpectraMaker
//
// Description: 
//  Sample maker to access and analyze StEvent
//
// Environment:
//  Software developed for the STAR Detector at Brookhaven National Laboratory
//
// Author List: 
//  Torre Wenaus, BNL
//
// History:
//
///////////////////////////////////////////////////////////////////////////////
#include "StMaker.h"
#include "StArray.h"
#include <TFile.h>
#include <vector>
#include "StSpectraAnalysis.h"
#include "StTpcDeviantSpectraAnalysis.h"
#include "StEfficiency.h"

class StEvent;
class StRun;

class StSpectraMaker : public StMaker {

private:

#ifdef ST_NO_TEMPLATE_DEF_ARGS
vector<StSpectraAnalysis*, allocator<StSpectraAnalysis*> > mSpectraAnalysisContainer;//!
#else
vector<StSpectraAnalysis*> mSpectraAnalysisContainer;//!
#endif
TFile* mOutput;

protected:

public:

  StSpectraMaker(const Char_t *name="spectra");

  virtual ~StSpectraMaker();
  virtual void Clear(Option_t *option="");
  virtual Int_t Init();
  virtual Int_t  Make();
  virtual Int_t  Finish();


  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StSpectraMaker.h,v 1.3 1999/11/22 01:54:58 ogilvie Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StSpectraMaker, 1)
};

#endif


