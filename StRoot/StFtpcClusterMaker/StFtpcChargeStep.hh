// $Id: StFtpcChargeStep.hh,v 1.2 2001/03/06 23:33:34 jcs Exp $
//
// $Log: StFtpcChargeStep.hh,v $
// Revision 1.2  2001/03/06 23:33:34  jcs
// use database instead of params
//
// Revision 1.1  2000/11/14 13:07:56  hummler
// add charge step calculation, minor cleanup
//

#ifndef STAR_StFtpcChargeStep
#define STAR_StFtpcChargeStep
// #define DEBUG 1

#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDAQMaker/StDAQReader.h"
#include "StDAQMaker/StFTPCReader.h"
#include "StFtpcParamReader.hh"
#include "StFtpcDbReader.hh"
#include "TH2.h"

class StFtpcChargeStep
{

 private:
  StFTPCReader *mReader; 
  StFtpcParamReader *mParam;
  StFtpcDbReader *mDb;
  TH2F *mHisto;
  int mClear;
  double *pRadius;

 public:
  StFtpcChargeStep(TH2F *histo,
		   StFTPCReader *reader, 
		   StFtpcParamReader *paramReader,
                   StFtpcDbReader *dbReader);
  StFtpcChargeStep(StFTPCReader *reader, 
		   StFtpcParamReader *paramReader,
                   StFtpcDbReader *dbReader);
  ~StFtpcChargeStep();
  int histogram(int setPressure);
  int calcpadtrans(double *pradius);
};

#endif
