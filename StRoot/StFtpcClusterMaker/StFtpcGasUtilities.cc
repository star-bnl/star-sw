////////////////////////////////////////////////////////////////////////
//
//   $Id: StFtpcGasUtilities.cc,v 1.10 2004/07/18 14:12:45 jcs Exp $
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
//   Revision 1.10  2004/07/18 14:12:45  jcs
//   use adjustAverageWest/East from database
//   always output temperature calculation information since this is a critical value for the FTPC
//
//   Revision 1.9  2004/05/26 10:32:32  jcs
//   For all runs on or after 2004-01-25, we use only three temperature readings per FTPC
//   + and adjustment to compute the averageBodyTemperature
//   (this change is provisional - the adjustment must be added to the database)
//
//   Revision 1.8  2004/05/07 12:07:06  jcs
//   remove double code in averageTemperatureEast
//
//   Revision 1.7  2004/04/14 16:48:21  jcs
//   turn off DEBUG
//
//   Revision 1.6  2004/04/14 14:55:08  jcs
//   temporarily turn on debug
//   don't use flaky body temperature readings
//
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
 
Int_t StFtpcGasUtilities::averageTemperatureWest(Int_t dbDate, Int_t runNumber) {
	
   Int_t numberBodyTemperaturesWest = 0;
   Float_t averageBodyTemperatureWest = 0.0;
        
   // Year2004: from runs after run 5027147 on 2004-01-27 to the end of run on  2004-05-15, only Body1, Body3 and Body4 temperature readings are useable for FTPC West
   // The averageBodyTemperatureWest must be adjusted since only 3 instead of 6 temperature readings are used
   if ( runNumber > 5027147 && runNumber < 5136001 ) {
      cout<<"ruNumber = "<<runNumber<<" > 5027147 && <= 5136001: only Body1, Body3 and Body4 are useable for FTPC West. The averageBodyTemperatureWest is adjusted by  "<<mDb->adjustAverageWest()<<endl;
      if (mGas->getBody1West() >= mDb->minGasTemperature() && mGas->getBody1West() <= mDb->maxGasTemperature() ) {
         averageBodyTemperatureWest = averageBodyTemperatureWest + mGas->getBody1West();	 
	 numberBodyTemperaturesWest++;
	 cout<<mGas->getBody1West()<<" (body1West) + ";
      }  
      if (mGas->getBody3West() >= mDb->minGasTemperature() && mGas->getBody3West() <= mDb->maxGasTemperature() ) {
         averageBodyTemperatureWest = averageBodyTemperatureWest + mGas->getBody3West();	 
	 numberBodyTemperaturesWest++;
	 cout<<mGas->getBody3West()<<" (body3West) + ";
      }  
      if (mGas->getBody4West() >= mDb->minGasTemperature() && mGas->getBody4West() <= mDb->maxGasTemperature() ) {
         averageBodyTemperatureWest = averageBodyTemperatureWest + mGas->getBody4West();	 
	 numberBodyTemperaturesWest++;
	 cout<<mGas->getBody4West()<<" (body4West) ";
      }  
   }  

   else {
      if (mGas->getBody1West() >= mDb->minGasTemperature() && mGas->getBody1West() <= mDb->maxGasTemperature()) {      
	 averageBodyTemperatureWest = averageBodyTemperatureWest + mGas->getBody1West();
	 numberBodyTemperaturesWest++;
	 cout<<mGas->getBody1West()<<" (body1West) + ";
      }		 
      if (mGas->getBody2West() >= mDb->minGasTemperature() && mGas->getBody2West()<= mDb->maxGasTemperature() ) {
	 averageBodyTemperatureWest = averageBodyTemperatureWest + mGas->getBody2West();
	 numberBodyTemperaturesWest++;
	 cout<<mGas->getBody2West()<<" (body2West) + ";
      }		 
      if (mGas->getBody3West() >= mDb->minGasTemperature() && mGas->getBody3West()<= mDb->maxGasTemperature() ) {
	 averageBodyTemperatureWest = averageBodyTemperatureWest + mGas->getBody3West();
	 numberBodyTemperaturesWest++;
	 cout<<mGas->getBody3West()<<" (body3West) + ";
      }		 
      if (mGas->getBody4West() >= mDb->minGasTemperature() && mGas->getBody4West() <= mDb->maxGasTemperature() ) {
	 averageBodyTemperatureWest = averageBodyTemperatureWest + mGas->getBody4West();
	 numberBodyTemperaturesWest++;
	 cout<<mGas->getBody4West()<<" (body4West) ";
      }		 
      // from 2003-10-31 -> 2004-01-24 there are 2 additional body temperature sensors
      if ( dbDate >= 20031031 && dbDate <= 20040124) {
         cout<<"(dbDate = "<<dbDate<<" >= 20031031 && <= 20040124 activate additional body temperature sensors) ";
         if (mGas->getBody5West() >= mDb->minGasTemperature() && mGas->getBody5West() <= mDb->maxGasTemperature() ) {
            averageBodyTemperatureWest = averageBodyTemperatureWest + mGas->getBody5West();	 
	    numberBodyTemperaturesWest++;
	    cout<<" + "<<mGas->getBody5West()<<" (body5West) + ";
	 }  
         if (mGas->getBody6West() >= mDb->minGasTemperature() && mGas->getBody6West() <= mDb->maxGasTemperature() ) {
	    averageBodyTemperatureWest = averageBodyTemperatureWest + mGas->getBody6West();
	    numberBodyTemperaturesWest++;
	    cout<<mGas->getBody6West()<<" (body6West) ";
	  }  
      }  
   }      

   // calculate average body temperature west
  if (numberBodyTemperaturesWest > 0) {
     cout<<" = "<<averageBodyTemperatureWest<<endl;
     cout<<"averageBodyTemperatureWest = "<<averageBodyTemperatureWest<<"/"<<numberBodyTemperaturesWest<<" = ";
     averageBodyTemperatureWest = averageBodyTemperatureWest/numberBodyTemperaturesWest;
     cout<<averageBodyTemperatureWest<<endl;
     cout<<"setGasTemperatureWest = "<<averageBodyTemperatureWest<<" (averageBodyTemperatureWest) + "<<mDb->adjustAverageWest()<<" (adjustAverageWest) = "<<averageBodyTemperatureWest + mDb->adjustAverageWest()<<endl;	  
     mParam->setGasTemperatureWest(averageBodyTemperatureWest + mDb->adjustAverageWest()); 
     return kStOK;
  }   
  //  if no body temperature readings return warning
  else {
     cout<<"No FTPC West body temperatures found for "<<dbDate<<endl;
      return kStWarn;
  }	  
}

 
Int_t StFtpcGasUtilities::averageTemperatureEast(Int_t dbDate, Int_t runNumber) {
	
   Int_t numberBodyTemperaturesEast = 0;
   Float_t averageBodyTemperatureEast = 0.0;
     
   // Year2004: from runs after run 5027147 on 2004-01-27 to the end of run on  2004-05-15, only Body1, Body3 and Body4 temperature readings are useable for FTPC East
   // The averageBodyTemperatureEast must be adjusted since only 3 instead of 6 temperature readings are used
   if ( runNumber > 5027147 && runNumber < 5136001 ) {
      cout<<"ruNumber = "<<runNumber<<" > 5027147 && <= 5136001: only Body1, Body3 and Body4 are useable for FTPC East. The averageBodyTemperatureEast is adjusted by  "<<mDb->adjustAverageEast()<<endl;
      if (mGas->getBody3East() >= mDb->minGasTemperature() && mGas->getBody3East() <= mDb->maxGasTemperature() ) {
         averageBodyTemperatureEast = averageBodyTemperatureEast + mGas->getBody3East();	 
	 numberBodyTemperaturesEast++;
	 cout<<mGas->getBody3East()<<" (body3East) + ";
      }  
      if (mGas->getBody4East() >= mDb->minGasTemperature() && mGas->getBody4East() <= mDb->maxGasTemperature() ) {
         averageBodyTemperatureEast = averageBodyTemperatureEast + mGas->getBody4East();	 
	 numberBodyTemperaturesEast++;
	 cout<<mGas->getBody4East()<<" (body4East) + ";
      }  
      if (mGas->getBody5East() >= mDb->minGasTemperature() && mGas->getBody5East() <= mDb->maxGasTemperature() ) {
         averageBodyTemperatureEast = averageBodyTemperatureEast + mGas->getBody5East();	 
	 numberBodyTemperaturesEast++;
	 cout<<mGas->getBody5East()<<" (body5East) ";
      }  
   }  

   else {


      if (mGas->getBody1East() >= mDb->minGasTemperature() && mGas->getBody1East() <= mDb->maxGasTemperature()) {      
		 averageBodyTemperatureEast = averageBodyTemperatureEast + mGas->getBody1East();
		 numberBodyTemperaturesEast++;
	         cout<<mGas->getBody1East()<<" (body1East) + ";
      }		 
      if (mGas->getBody2East() >= mDb->minGasTemperature() && mGas->getBody2East()<= mDb->maxGasTemperature() ) {
		 averageBodyTemperatureEast = averageBodyTemperatureEast + mGas->getBody2East();
		 numberBodyTemperaturesEast++;
	         cout<<mGas->getBody2East()<<" (body2East) + ";
      }		 
      if (mGas->getBody3East() >= mDb->minGasTemperature() && mGas->getBody3East()<= mDb->maxGasTemperature() ) {
		 averageBodyTemperatureEast = averageBodyTemperatureEast + mGas->getBody3East();
		 numberBodyTemperaturesEast++;
	         cout<<mGas->getBody3East()<<" (body3East) + ";
      }		 
      if (mGas->getBody4East() >= mDb->minGasTemperature() && mGas->getBody4East() <= mDb->maxGasTemperature() ) {
		 averageBodyTemperatureEast = averageBodyTemperatureEast + mGas->getBody4East();
		 numberBodyTemperaturesEast++;
	         cout<<mGas->getBody4East()<<" (body4East) ";
      }		 
      // from 2003-10-31 -> 2004-01-24 there are 2 additional body temperature sensors
      if ( dbDate >= 20031031 && dbDate <= 20040124) {
         cout<<"(dbDate = "<<dbDate<<" >= 20031031 && <= 20040124 activate additional body temperature sensors) ";
         if (mGas->getBody5East() >= mDb->minGasTemperature() && mGas->getBody5East() <= mDb->maxGasTemperature() ) {
            averageBodyTemperatureEast = averageBodyTemperatureEast + mGas->getBody5East();	 
	    numberBodyTemperaturesEast++;
	    cout<<" + "<<mGas->getBody5East()<<" (body5East) ";
	 }  
         if (mGas->getBody6East() >= mDb->minGasTemperature() && mGas->getBody6East() <= mDb->maxGasTemperature() ) {
	    averageBodyTemperatureEast = averageBodyTemperatureEast + mGas->getBody6East();
	    numberBodyTemperaturesEast++;
	    cout<<mGas->getBody6East()<<" (body6East) ";
	 } 
       }  
   }      

      // calculate average body temperature east
      if (numberBodyTemperaturesEast > 0) {
        cout<<" = "<<averageBodyTemperatureEast<<endl;
	cout<<"averageBodyTemperatureEast = "<<averageBodyTemperatureEast<<"/"<<numberBodyTemperaturesEast<<" = ";
        averageBodyTemperatureEast = averageBodyTemperatureEast/numberBodyTemperaturesEast;
	cout<<averageBodyTemperatureEast<<endl;
        cout<<"setGasTemperatureEast = "<<averageBodyTemperatureEast<<" (averageBodyTemperatureEast) + "<<mDb->adjustAverageEast()<<" (adjustAverageEast) = "<<averageBodyTemperatureEast + mDb->adjustAverageEast()<<endl;	  
        mParam->setGasTemperatureEast(averageBodyTemperatureEast + mDb->adjustAverageEast()); 
        return kStOK;
     }   
     //  if no body temperature readings return warning
     else {
        cout<<"No FTPC East body temperatures found for "<<dbDate<<endl;
        return kStWarn;
     }	  
}

Int_t StFtpcGasUtilities::defaultTemperatureWest(Int_t dbDate,Bool_t SVT_On) {
    if ( !SVT_On) {
       mParam->setGasTemperatureWest(mDb->defaultTemperatureWest());
       cout<<"No valid body temperatures available for FTPC West; Initialize to defaultTemperatureWest (for dbDate = "<<dbDate<<" SVT off) = "<<mParam->gasTemperatureWest()<<endl;
    }
    if (SVT_On) {
	if (dbDate < 20021105) {
	   // for year 2001 data (AuAu,pp) FTPC west gas temperature is higher when SVT on
           mParam->setGasTemperatureWest(mDb->defaultTemperatureWest() + mDb->temperatureDifference());
           cout<<"No valid body temperatures available for FTPC West; Initialize to mDb->defaultTemperatureWest() + mDb->temperatureDifference() (for year2001 data, SVT on) = "<<mParam->gasTemperatureWest()<<endl;
	}	   
        if (dbDate >= 20021105) { 
           // for year 2003 data (dAu,pp) FTPC west gas temperature is not effected by SVT
           mParam->setGasTemperatureWest(mDb->defaultTemperatureWest());
           cout<<"No valid body temperatures available for FTPC West; Initialize to mDb->defaultTemperatureWest() (for year2003 data, SVT on) = "<<mParam->gasTemperatureWest()<<endl;
	}
  }	  
  return kStOK;
}    

Int_t StFtpcGasUtilities::defaultTemperatureEast(Int_t dbDate,Bool_t SVT_On) {
    if ( !SVT_On) {
       mParam->setGasTemperatureEast(mDb->defaultTemperatureEast());
       cout<<"No valid body temperatures available for FTPC East; Initialize to defaultTemperatureEast (for dbDate = "<<dbDate<<" SVT off) = "<<mParam->gasTemperatureEast()<<endl;
    }
    if (SVT_On) {
	if (dbDate < 20021105) {
           // for year 2001 data (dAu,pp) FTPC east gas temperature is not effected by SVT
           mParam->setGasTemperatureEast(mDb->defaultTemperatureEast());
           cout<<"No valid body temperatures available for FTPC East; Initialize to mDb->defaultTemperatureEast() (for year2001 data, SVT on) = "<<mParam->gasTemperatureEast()<<endl;
	}	   
        if (dbDate >= 20021105) { 
           // for year 2003 data (dAu,pp) FTPC east gas temperature is higher when SVT on
           mParam->setGasTemperatureEast(mDb->defaultTemperatureEast() + mDb->temperatureDifference());
           cout<<"No valid body temperatures available for FTPC East; Initialize to mDb->defaultTemperatureEast() + mDb->temperatureDifference() (for year2003 data, SVT on) = "<<mParam->gasTemperatureEast()<<endl;
	}
    }	
  return kStOK;
  }	  
