////////////////////////////////////////////////////////////////////////
//
//   $Id: StFtpcGasUtilities.hh,v 1.1 2003/11/13 14:12:17 jcs Exp $
//
//   StFtpcGasUtilities
//
//   Author:  Janet Seyboth     10/30/2003
//
////////////////////////////////////////////////////////////////////////
//
//   $Log: StFtpcGasUtilities.hh,v $
//   Revision 1.1  2003/11/13 14:12:17  jcs
//   move pressure and gas corrections from StFtpcClusterMaker.cxx to StFtpcGasUtilities
//
//
////////////////////////////////////////////////////////////////////////

#ifndef STAR_StFtpcGasUtilities
#define STAR_StFtpcGasUtilities

#include "StDetectorDbMaker/StDetectorDbFTPCGas.h"

class StFtpcGasUtilities
{
  private:
   StFtpcParamReader   *mParam;
   StFtpcDbReader      *mDb;
   StDetectorDbFTPCGas *mGas;
  
  public:
   StFtpcGasUtilities(StFtpcParamReader   *paramReader,
                      StFtpcDbReader      *dbReader,
	       	      StDetectorDbFTPCGas *mGas);

   ~StFtpcGasUtilities();

   Int_t barometricPressure();      //!
   Int_t averageTemperatureWest(Int_t dbDate);  //!
   Int_t averageTemperatureEast(Int_t dbDate);  //!
   Int_t defaultTemperatureWest(Int_t dbDate,Bool_t SVT_On); //!
   Int_t defaultTemperatureEast(Int_t dbDate,Bool_t SVT_On); //!

};

#endif
