////////////////////////////////////////////////////////////////////////
//
//   $Id: StFtpcGasUtilities.hh,v 1.4 2005/03/23 14:32:29 jcs Exp $
//
//   StFtpcGasUtilities
//
//   Author:  Janet Seyboth     10/30/2003
//
////////////////////////////////////////////////////////////////////////
//
//   $Log: StFtpcGasUtilities.hh,v $
//   Revision 1.4  2005/03/23 14:32:29  jcs
//   additional changes for using body + extra temperatures starting with y2005
//
//   Revision 1.3  2005/03/14 22:57:18  jcs
//   clean up code
//   use body + extra temperature readings starting with y2005
//
//   Revision 1.2  2004/07/18 14:12:45  jcs
//   use adjustAverageWest/East from database
//   always output temperature calculation information since this is a critical value for the FTPC
//
//   Revision 1.1  2003/11/13 14:12:17  jcs
//   move pressure and gas corrections from StFtpcClusterMaker.cxx to StFtpcGasUtilities
//
//
////////////////////////////////////////////////////////////////////////

#ifndef STAR_StFtpcGasUtilities
#define STAR_StFtpcGasUtilities

#include "StDetectorDbMaker/StDetectorDbFTPCGas.h"
#include "tables/St_ftpcTemps_Table.h"

class StFtpcGasUtilities
{
  private:
   StFtpcParamReader   *mParam;
   StFtpcDbReader      *mDb;
   StDetectorDbFTPCGas *mGas;
   St_ftpcTemps        *mTemps;
  
  public:
   StFtpcGasUtilities(StFtpcParamReader   *paramReader,
                      StFtpcDbReader      *dbReader,
	       	      StDetectorDbFTPCGas *mGas,
                      St_ftpcTemps        *mTemps);

   ~StFtpcGasUtilities();

   Int_t barometricPressure();      //!
   Int_t averageTemperatureWest(Int_t dbDate,Int_t runNumber);  //!
   Int_t averageTemperatureEast(Int_t dbDate,Int_t runNumber);  //!
   Int_t averageTemperatureWest(Int_t dbDate,Int_t runNumber,St_ftpcTemps *ftpcTemps);  //!
   Int_t averageTemperatureEast(Int_t dbDate,Int_t runNumber,St_ftpcTemps *ftpcTemps);  //!
   Int_t defaultTemperatureWest(Int_t dbDate,Bool_t SVT_On); //!
   Int_t defaultTemperatureEast(Int_t dbDate,Bool_t SVT_On); //!

};

#endif
