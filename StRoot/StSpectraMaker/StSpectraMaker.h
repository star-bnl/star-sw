
// $Id: StSpectraMaker.h,v 1.1 1999/11/03 21:22:42 ogilvie Exp $
//
// $Log: StSpectraMaker.h,v $
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

#ifndef ST_NO_TEMPLATE_DEF_ARGS
vector<StTpcDeviantSpectraAnalysis*> mSpectraAnalysisContainer;//!
#else
vector<StTpcDeviantSpectraAnalysis*, allocator<StTpcDeviantSpectraAnalysis*> > mSpectraAnalysisContainer;//!
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
  {static const char cvs[]="Tag $Name:  $ $Id: StSpectraMaker.h,v 1.1 1999/11/03 21:22:42 ogilvie Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StSpectraMaker, 1)
};

#endif


