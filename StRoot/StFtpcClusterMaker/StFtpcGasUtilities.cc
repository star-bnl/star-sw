////////////////////////////////////////////////////////////////////////
//
//   $Id: StFtpcGasUtilities.cc,v 1.5 2004/03/11 22:26:26 jcs Exp $
//
//   StFtpcGasUtilities
//
//   Author:  Janet Seyboth     10/30/2003
//
//   Function: adjust pressure and ftpc gas temperature event by event
//
////////////////////////////////////////////////////////////////////////
//
//   $Log: StFtpcGasUtilities.cc,v $
//   Revision 1.5  2004/03/11 22:26:26  jcs
//   activate reading of additional body temperatures
//
//   Revision 1.4  2004/03/09 20:58:00  jcs
//   undo activation of additional body temperatures - they are not getting
//   thru from offline database to here
//
//   Revision 1.3  2004/03/09 20:22:27  jcs
//   activate use of additional body temperatures for y2004
//
//   Revision 1.2  2004/02/02 17:29:40  jcs
//   dbDate test works for y2004, now only print message for DEBUG
//
//   Revision 1.1  2003/11/13 14:12:17  jcs
//   move pressure and gas corrections from StFtpcClusterMaker.cxx to StFtpcGasUtilities
//
//
////////////////////////////////////////////////////////////////////////

#include "StFtpcParamReader.hh"
#include "StFtpcDbReader.hh"

#include "StFtpcGasUtilities.hh"
#include "StMessMgr.h"

#ifndef DEBUG
#define DEBUG 0
#endif


StFtpcGasUtilities::StFtpcGasUtilities(StFtpcParamReader   *paramReader,
				       StFtpcDbReader      *dbReader,
				       StDetectorDbFTPCGas *gas)
{				 
  mParam = paramReader;
  mDb    = dbReader;
  mGas   = gas;
}  


StFtpcGasUtilities::~StFtpcGasUtilities() {
}

Int_t StFtpcGasUtilities::barometricPressure() {
   if (mGas->getBarometricPressure() >= mDb->minPressure() && mGas->getBarometricPressure() <= mDb->maxPressure()) {
      gMessMgr->Info() <<"Change normalizedNowPressure from "<<mParam->normalizedNowPressure()<<" to "<<mGas->getBarometricPressure()<<endm;
      mParam->setNormalizedNowPressure(mGas->getBarometricPressure());
      return kStOK;
   }
   else {
      gMessMgr->Info() << "Invalid value ("<<mGas->getBarometricPressure()<<") from offline database for barometric pressure - using previous value ("<<mParam->normalizedNowPressure()<<")"<<endm;
      return kStWarn;
   }
}   

// Calculate FTPC gas temperature from body temperatures
 
Int_t StFtpcGasUtilities::averageTemperatureWest(Int_t dbDate) {
	
      Int_t numberBodyTemperaturesWest = 0;
      Float_t averageBodyTemperatureWest = 0.0;

      if (mGas->getBody1West() >= mDb->minGasTemperature() && mGas->getBody1West() <= mDb->maxGasTemperature()) {      
	 averageBodyTemperatureWest = averageBodyTemperatureWest + mGas->getBody1West();
	 numberBodyTemperaturesWest++;
	 if (DEBUG) cout<<"mGas->getBody1West() = "<<mGas->getBody1West()<<endl;
      }		 
      if (mGas->getBody2West() >= mDb->minGasTemperature() && mGas->getBody2West()<= mDb->maxGasTemperature() ) {
	 averageBodyTemperatureWest = averageBodyTemperatureWest + mGas->getBody2West();
	 numberBodyTemperaturesWest++;
	 if (DEBUG) cout<<"mGas->getBody2West() = "<<mGas->getBody2West()<<endl;
      }		 
      if (mGas->getBody3West() >= mDb->minGasTemperature() && mGas->getBody3West()<= mDb->maxGasTemperature() ) {
	 averageBodyTemperatureWest = averageBodyTemperatureWest + mGas->getBody3West();
	 numberBodyTemperaturesWest++;
	 if (DEBUG) cout<<"mGas->getBody3West() = "<<mGas->getBody3West()<<endl;
      }		 
      if (mGas->getBody4West() >= mDb->minGasTemperature() && mGas->getBody4West() <= mDb->maxGasTemperature() ) {
	 averageBodyTemperatureWest = averageBodyTemperatureWest + mGas->getBody4West();
	 numberBodyTemperaturesWest++;
	 if (DEBUG) cout<<"mGas->getBody4West() = "<<mGas->getBody4West()<<endl;
      }		 
      // as of 2003-10-31 there are 2 additional body temperature sensors
      if ( dbDate >= 20031031 ) {
         if (DEBUG) cout<<"dbDate = "<<dbDate<<" >= 20031031 activate additional body temperature sensors"<<endl;
         if (mGas->getBody5West() >= mDb->minGasTemperature() && mGas->getBody5West() <= mDb->maxGasTemperature() ) {
            averageBodyTemperatureWest = averageBodyTemperatureWest + mGas->getBody5West();	 
	    numberBodyTemperaturesWest++;
	    if (DEBUG) cout<<"mGas->getBody5West() = "<<mGas->getBody5West()<<endl;
	 }  
         if (mGas->getBody6West() >= mDb->minGasTemperature() && mGas->getBody6West() <= mDb->maxGasTemperature() ) {
	    averageBodyTemperatureWest = averageBodyTemperatureWest + mGas->getBody6West();
	    numberBodyTemperaturesWest++;
	    if (DEBUG) cout<<"mGas->getBody6West() = "<<mGas->getBody6West()<<endl;
	  }  
	 }  

   // calculate average body temperature west
  if (numberBodyTemperaturesWest > 0) {
     mParam->setGasTemperatureWest(averageBodyTemperatureWest/numberBodyTemperaturesWest); 
     return kStOK;
  }   
  //  if no body temperature readings return warning
  else {
      if (DEBUG) cout<<"No FTPC West body temperatures found for "<<dbDate<<endl;
      return kStWarn;
  }	  
}

 
Int_t StFtpcGasUtilities::averageTemperatureEast(Int_t dbDate) {
	
      Int_t numberBodyTemperaturesEast = 0;
      Float_t averageBodyTemperatureEast = 0.0;

      if (mGas->getBody1East() >= mDb->minGasTemperature() && mGas->getBody1East() <= mDb->maxGasTemperature()) {      
		 averageBodyTemperatureEast = averageBodyTemperatureEast + mGas->getBody1East();
		 numberBodyTemperaturesEast++;
		 if (DEBUG) cout<<"mGas->getBody1East() = "<<mGas->getBody1East()<<endl;
         }		 
	 if (mGas->getBody2East() >= mDb->minGasTemperature() && mGas->getBody2East()<= mDb->maxGasTemperature() ) {
		 averageBodyTemperatureEast = averageBodyTemperatureEast + mGas->getBody2East();
		 numberBodyTemperaturesEast++;
		 if (DEBUG) cout<<"mGas->getBody2East() = "<<mGas->getBody2East()<<endl;
         }		 
	 if (mGas->getBody3East() >= mDb->minGasTemperature() && mGas->getBody3East()<= mDb->maxGasTemperature() ) {
		 averageBodyTemperatureEast = averageBodyTemperatureEast + mGas->getBody3East();
		 numberBodyTemperaturesEast++;
		 if (DEBUG) cout<<"mGas->getBody3East() = "<<mGas->getBody3East()<<endl;
         }		 
	 if (mGas->getBody4East() >= mDb->minGasTemperature() && mGas->getBody4East() <= mDb->maxGasTemperature() ) {
		 averageBodyTemperatureEast = averageBodyTemperatureEast + mGas->getBody4East();
		 numberBodyTemperaturesEast++;
		 if (DEBUG) cout<<"mGas->getBody4East() = "<<mGas->getBody4East()<<endl;
         }		 
      // as of 2003-10-31 there are 2 additional body temperature sensors
      if ( dbDate >= 20031031 ) {
        if (DEBUG)  cout<<"dbDate = "<<dbDate<<" >= 20031031 activate additional body temperature sensors"<<endl;
         if (mGas->getBody5East() >= mDb->minGasTemperature() && mGas->getBody5East() <= mDb->maxGasTemperature() ) {
            averageBodyTemperatureEast = averageBodyTemperatureEast + mGas->getBody5East();	 
	    numberBodyTemperaturesEast++;
	    if (DEBUG) cout<<"mGas->getBody5East() = "<<mGas->getBody5East()<<endl;
	 }  
         if (mGas->getBody6East() >= mDb->minGasTemperature() && mGas->getBody6East() <= mDb->maxGasTemperature() ) {
	    averageBodyTemperatureEast = averageBodyTemperatureEast + mGas->getBody6East();
	    numberBodyTemperaturesEast++;
	    if (DEBUG) cout<<"mGas->getBody6East() = "<<mGas->getBody6East()<<endl;
	 } 
       }  

      // calculate average body temperature east
      if (numberBodyTemperaturesEast > 0) {
        mParam->setGasTemperatureEast(averageBodyTemperatureEast/numberBodyTemperaturesEast); 
        return kStOK;
     }   
     //  if no body temperature readings return warning
     else {
        if (DEBUG) cout<<"No FTPC East body temperatures found for "<<dbDate<<endl;
        return kStWarn;
     }	  
}

Int_t StFtpcGasUtilities::defaultTemperatureWest(Int_t dbDate,Bool_t SVT_On) {
    if ( !SVT_On) {
       mParam->setGasTemperatureWest(mDb->defaultTemperatureWest());
       if (DEBUG) cout<<"No valid body temperatures available for FTPC West; Initialize to defaultTemperatureWest (for dbDate = "<<dbDate<<" SVT off) = "<<mParam->gasTemperatureWest()<<endl;
    }
    if (SVT_On) {
	if (dbDate < 20021105) {
	   // for year 2001 data (AuAu,pp) FTPC west gas temperature is higher when SVT on
           mParam->setGasTemperatureWest(mDb->defaultTemperatureWest() + mDb->temperatureDifference());
           if (DEBUG) cout<<"No valid body temperatures available for FTPC West; Initialize to mDb->defaultTemperatureWest() + mDb->temperatureDifference() (for year2001 data, SVT on) = "<<mParam->gasTemperatureWest()<<endl;
	}	   
        if (dbDate >= 20021105) { 
           // for year 2003 data (dAu,pp) FTPC west gas temperature is not effected by SVT
           mParam->setGasTemperatureWest(mDb->defaultTemperatureWest());
           if (DEBUG) cout<<"No valid body temperatures available for FTPC West; Initialize to mDb->defaultTemperatureWest() (for year2003 data, SVT on) = "<<mParam->gasTemperatureWest()<<endl;
	}
  }	  
  return kStOK;
}    

Int_t StFtpcGasUtilities::defaultTemperatureEast(Int_t dbDate,Bool_t SVT_On) {
    if ( !SVT_On) {
       mParam->setGasTemperatureEast(mDb->defaultTemperatureEast());
       if (DEBUG) cout<<"No valid body temperatures available for FTPC East; Initialize to defaultTemperatureEast (for dbDate = "<<dbDate<<" SVT off) = "<<mParam->gasTemperatureEast()<<endl;
    }
    if (SVT_On) {
	if (dbDate < 20021105) {
           // for year 2001 data (dAu,pp) FTPC east gas temperature is not effected by SVT
           mParam->setGasTemperatureEast(mDb->defaultTemperatureEast());
           if (DEBUG) cout<<"No valid body temperatures available for FTPC East; Initialize to mDb->defaultTemperatureEast() (for year2001 data, SVT on) = "<<mParam->gasTemperatureEast()<<endl;
	}	   
        if (dbDate >= 20021105) { 
           // for year 2003 data (dAu,pp) FTPC east gas temperature is higher when SVT on
           mParam->setGasTemperatureEast(mDb->defaultTemperatureEast() + mDb->temperatureDifference());
           if (DEBUG) cout<<"No valid body temperatures available for FTPC East; Initialize to mDb->defaultTemperatureEast() + mDb->temperatureDifference() (for year2003 data, SVT on) = "<<mParam->gasTemperatureEast()<<endl;
	}
    }	
  return kStOK;
  }	  
