////////////////////////////////////////////////////////////////////////
//
//   $Id: StFtpcGasUtilities.cc,v 1.26 2010/04/27 13:50:25 jcs Exp $
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
//   Revision 1.26  2010/04/27 13:50:25  jcs
//   Use default temperatures for "2010-04-21 21:40:01" to "2010-04-26 17:19:00"
//   when extra temperature readings were stuck
//
//   Revision 1.25  2010/04/22 16:23:03  jcs
//   From "2010-04-13 00:20:01" to "2010-04-19 20:39:00" the extra temperatures for both FTPC West
//   and FTPC East were stuck. Use the default temperatures for all runs taken in this time period.
//
//   Revision 1.24  2009/11/14 12:51:08  jcs
//   added suggested parentheses to avoid warnings which appeared with system upgrade
//
//   Revision 1.23  2007/12/13 21:04:52  jcs
//   insert missing LOG_DEBUG statement
//
//   Revision 1.22  2007/02/01 11:57:04  jcs
//   move unessential output from INFO to DEBUG
//
//   Revision 1.21  2007/01/15 07:49:22  jcs
//   replace printf, cout and gMesMgr with Logger
//
//   Revision 1.20  2006/07/21 08:42:44  jcs
//   restore revision 1.18 code, this was applied to adjust radial step
//
//   Revision 1.19  2006/07/19 11:59:39  jcs
//   additional body temperatures now available starting with runNumber 5009073
//
//   Revision 1.18  2006/06/23 07:56:55  jcs
//   Starting in Y2006 use only the FTPC Extra Temperature readings plus an adjustment, if necessary, to determine the average gas temperature
//
//   Revision 1.17  2005/03/30 11:40:35  jcs
//   Calculate the average temperature correctly for year 2005+ runs when only one set of temperature readings (body or extra) is available
//
//   Revision 1.16  2005/03/23 14:32:28  jcs
//   additional changes for using body + extra temperatures starting with y2005
//
//   Revision 1.15  2005/03/14 22:57:18  jcs
//   clean up code
//   use body + extra temperature readings starting with y2005
//
//   Revision 1.14  2005/01/17 15:58:05  jcs
//   use all 6 ftpc west body temperature readings for 2005 run
//
//   Revision 1.13  2004/09/17 08:55:40  jcs
//   add corrections for processing 2004 pp data
//
//   Revision 1.12  2004/09/15 10:27:59  jcs
//   correct error in averageTemperatureEast comment and output statement (body3,4,5 are used NOT 1,3,4)
//
//   Revision 1.11  2004/07/20 18:30:17  jcs
//   change temperature output information
//
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


StFtpcGasUtilities::StFtpcGasUtilities(StFtpcParamReader   *paramReader,
				       StFtpcDbReader      *dbReader,
				       StDetectorDbFTPCGas *gas,
                                       St_ftpcTemps        *temps)
{				 
  mParam = paramReader;
  mDb    = dbReader;
  mGas   = gas;
  mTemps = temps;
}  


StFtpcGasUtilities::~StFtpcGasUtilities() {
}

Int_t StFtpcGasUtilities::barometricPressure() {
   if (mGas->getBarometricPressure() == mParam->normalizedNowPressure()) return kStOK;
   if (mGas->getBarometricPressure() >= mDb->minPressure() && mGas->getBarometricPressure() <= mDb->maxPressure())  {
      LOG_DEBUG <<"Change normalizedNowPressure from "<<mParam->normalizedNowPressure()<<" to "<<mGas->getBarometricPressure()<<endm;
      mParam->setNormalizedNowPressure(mGas->getBarometricPressure());
      return kStOK;
   }
   else {
      LOG_WARN << "Invalid value ("<<mGas->getBarometricPressure()<<") from offline database for barometric pressure - using previous value ("<<mParam->normalizedNowPressure()<<")"<<endm;
      return kStWarn;
   }
}   

 
Int_t StFtpcGasUtilities::averageTemperatureWest(Int_t dbDate, Int_t runNumber) {
	
   Int_t numberBodyTemperaturesWest = 0;
   Float_t averageBodyTemperatureWest = 0.0;
   Float_t averageGasTempWest = 0.0;

   // In 2010 from "2010-04-13 00:20:01" to "2010-04-19 20:39:00" and
   //         from "2010-04-21 21:40:01" to "2010-04-26 17:19:00" 
   // the extra temperatures for both FTPC West and FTPC East were stuck. 
   // Since one reading was propagated to the database for this whole time period, 
   // we decided to use the default temperatures for all runs
   // taken in this time period.

   if ( ((dbDate >= 20100413 && runNumber > 11102099) && (dbDate <= 20100419 && runNumber < 11109080)) ||
        ((dbDate >= 20100421 && runNumber > 1111068) && (dbDate <= 20100426 && runNumber < 11116056)) ) {
     mParam->setGasTemperatureWest(mDb->defaultTemperatureWest() + mDb->adjustAverageWest());
     LOG_WARN<<"FTPC West Extra temperatures bad for run number "<<runNumber<<" dbDate "<<dbDate<<" using defaultTemperatureWest = "<<mDb->defaultTemperatureWest()<<" + adjustAverageWest "<<mDb->adjustAverageWest()<<" = "<<mParam->gasTemperatureWest()<<endm;
     return kStWarn;
   } 


   // Starting Y2006, calculate FTPC West gas temperature from extra temperatures only 
   // and adjust with mDb->adjustAverageWest()
   if ( dbDate >= 20051030 ) {
     
     ftpcTemps_st* tempT = mTemps->GetTable();

     Int_t numberExtraTempsWest = 0;
     Float_t averageExtraTempsWest = 0.0;

     if (tempT->extra1West >= mDb->minGasTemperature() && tempT->extra1West <= mDb->maxGasTemperature() ) {
        averageExtraTempsWest += tempT->extra1West;
        numberExtraTempsWest++;
        LOG_DEBUG<<"   "<<tempT->extra1West<<"(extra1West)"<<endm;
     }
     //if (tempT->extra2West >= mDb->minGasTemperature() && tempT->extra2West <= mDb->maxGasTemperature() ) {
     //   averageExtraTempsWest += tempT->extra2West;
     //   numberExtraTempsWest++;
     // LOG_DEBUG<<" + "<<tempT->extra2West<<"(extra2West)"<<endm;
     //}
     if (tempT->extra3West >= mDb->minGasTemperature() && tempT->extra3West <= mDb->maxGasTemperature() ) {
        averageExtraTempsWest += tempT->extra3West;
        numberExtraTempsWest++;
        LOG_DEBUG<<" + "<<tempT->extra3West<<"(extra3West)"<<endm;
     }
     if (tempT->extra4West >= mDb->minGasTemperature() && tempT->extra4West <= mDb->maxGasTemperature() ) {
        averageExtraTempsWest += tempT->extra4West;
        numberExtraTempsWest++;
        LOG_DEBUG<<" + "<<tempT->extra4West<<"(extra4West)"<<endm;
     }
     if (tempT->extra5West >= mDb->minGasTemperature() && tempT->extra5West <= mDb->maxGasTemperature() ) {
        averageExtraTempsWest += tempT->extra5West;
        numberExtraTempsWest++;
        LOG_DEBUG<<" + "<<tempT->extra5West<<"(extra5West)"<<endm;
     }
     if (tempT->extra6West >= mDb->minGasTemperature() && tempT->extra6West <= mDb->maxGasTemperature() ) {
        averageExtraTempsWest += tempT->extra6West;
        numberExtraTempsWest++;
        LOG_DEBUG<<" + "<<tempT->extra6West<<"(extra6West)"<<endm;
     }
     LOG_DEBUG<<" = "<<averageExtraTempsWest<<endm;

     // calculate  average gas temperature west
     if (numberExtraTempsWest > 0) {
        LOG_DEBUG<<"setGasTemperatureWest = averageGasTempWest = "<<averageExtraTempsWest<<"/"<<numberExtraTempsWest<<" + "<<mDb->adjustAverageWest()<<"  = "<<averageExtraTempsWest/numberExtraTempsWest + mDb->adjustAverageWest()<<endm;
        averageGasTempWest =  averageExtraTempsWest/numberExtraTempsWest;
        mParam->setGasTemperatureWest(averageGasTempWest + mDb->adjustAverageWest());
        return kStOK;
     }
     else {
        LOG_WARN<<"No FTPC West Extra temperatures found for run number "<<runNumber<<"dbDate "<<dbDate<<" using defaultTemperatureWest = "<<mParam->gasTemperatureWest()<<endm; 
        return kStWarn;
     }	  
   }

   // For Y2005 calculate FTPC West gas temperature from body temperatures 
   // plus extra temperature readings
   // (the extra temperature readings are available as of 2005-01-01)

   if ( dbDate >= 20050101 && dbDate < 20051030 ) {

     LOG_DEBUG<<" dbDate = "<<dbDate<<"(>2005-01-01) Use FTPC West body temperatures + FTPC West extra temperature readings 1,3-6 (2 is unstable in the year 2005 run)"<<endm;

     if (mGas->getBody1West() >= mDb->minGasTemperature() && mGas->getBody1West() <= mDb->maxGasTemperature()) {      
         averageBodyTemperatureWest += mGas->getBody1West();
         numberBodyTemperaturesWest++;
         LOG_DEBUG<<"   "<<mGas->getBody1West()<<"(body1West)"<<endm;
     }		 
     if (mGas->getBody2West() >= mDb->minGasTemperature() && mGas->getBody2West()<= mDb->maxGasTemperature() ) {
          averageBodyTemperatureWest += mGas->getBody2West();
          numberBodyTemperaturesWest++;
          LOG_DEBUG<<" + "<<mGas->getBody2West()<<"(body2West)"<<endm;
     }		 
     if (mGas->getBody3West() >= mDb->minGasTemperature() && mGas->getBody3West()<= mDb->maxGasTemperature() ) {
          averageBodyTemperatureWest += mGas->getBody3West();
          numberBodyTemperaturesWest++;
          LOG_DEBUG<<" + "<<mGas->getBody3West()<<"(body3West)"<<endm;
     }		 
     if (mGas->getBody4West() >= mDb->minGasTemperature() && mGas->getBody4West() <= mDb->maxGasTemperature() ) {
          averageBodyTemperatureWest += mGas->getBody4West();
          numberBodyTemperaturesWest++;
          LOG_DEBUG<<" + "<<mGas->getBody4West()<<"(body4West)"<<endm;
     }		 
     if (mGas->getBody5West() >= mDb->minGasTemperature() && mGas->getBody5West() <= mDb->maxGasTemperature() ) {
          averageBodyTemperatureWest += mGas->getBody5West();	 
          numberBodyTemperaturesWest++;
          LOG_DEBUG<<" + "<<mGas->getBody5West()<<"(body5West)"<<endm;
     }  
     if (mGas->getBody6West() >= mDb->minGasTemperature() && mGas->getBody6West() <= mDb->maxGasTemperature() ) {
          averageBodyTemperatureWest += mGas->getBody6West();
          numberBodyTemperaturesWest++;
          LOG_DEBUG<<" + "<<mGas->getBody6West()<<"(body6West)"<<endm;
     }  
     LOG_DEBUG<<" = "<<averageBodyTemperatureWest<<endm;


     // calculate average body temperature west
     if (numberBodyTemperaturesWest > 0) {
        LOG_DEBUG<<"averageBodyTemperatureWest = "<<averageBodyTemperatureWest<<"/"<<numberBodyTemperaturesWest<<" = "<<averageBodyTemperatureWest/numberBodyTemperaturesWest<<endm;
     }
     
     ftpcTemps_st* tempT = mTemps->GetTable();

     Int_t numberExtraTempsWest = 0;
     Float_t averageExtraTempsWest = 0.0;

     if (tempT->extra1West >= mDb->minGasTemperature() && tempT->extra1West <= mDb->maxGasTemperature() ) {
        averageExtraTempsWest += tempT->extra1West;
        numberExtraTempsWest++;
        LOG_DEBUG<<"   "<<tempT->extra1West<<"(extra1West)"<<endm;
     }
     //if (tempT->extra2West >= mDb->minGasTemperature() && tempT->extra2West <= mDb->maxGasTemperature() ) {
     //   averageExtraTempsWest += tempT->extra2West;
     //   numberExtraTempsWest++;
     // LOG_DEBUG<<" + "<<tempT->extra2West<<"(extra2West)"<<endm;
     //}
     if (tempT->extra3West >= mDb->minGasTemperature() && tempT->extra3West <= mDb->maxGasTemperature() ) {
        averageExtraTempsWest += tempT->extra3West;
        numberExtraTempsWest++;
        LOG_DEBUG<<" + "<<tempT->extra3West<<"(extra3West)"<<endm;
     }
     if (tempT->extra4West >= mDb->minGasTemperature() && tempT->extra4West <= mDb->maxGasTemperature() ) {
        averageExtraTempsWest += tempT->extra4West;
        numberExtraTempsWest++;
        LOG_DEBUG<<" + "<<tempT->extra4West<<"(extra4West)"<<endm;
     }
     if (tempT->extra5West >= mDb->minGasTemperature() && tempT->extra5West <= mDb->maxGasTemperature() ) {
        averageExtraTempsWest += tempT->extra5West;
        numberExtraTempsWest++;
        LOG_DEBUG<<" + "<<tempT->extra5West<<"(extra5West)"<<endm;
     }
     if (tempT->extra6West >= mDb->minGasTemperature() && tempT->extra6West <= mDb->maxGasTemperature() ) {
        averageExtraTempsWest += tempT->extra6West;
        numberExtraTempsWest++;
        LOG_DEBUG<<" + "<<tempT->extra6West<<"(extra6West)"<<endm;
     }
     LOG_DEBUG<<" = "<<averageExtraTempsWest<<endm;

     // calculate average extra temperature west
     if (numberExtraTempsWest > 0) {
        LOG_DEBUG<<"averageExtraTempsWest = "<<averageExtraTempsWest<<"/"<<numberExtraTempsWest<<" = "<<averageExtraTempsWest/numberExtraTempsWest<<endm;
     }
    
     if ( (numberBodyTemperaturesWest + numberExtraTempsWest) > 0 ) {
        LOG_DEBUG<<"setGasTemperatureWest = averageGasTempWest = ("<<averageBodyTemperatureWest<<" + "<<averageExtraTempsWest<<")/("<<numberBodyTemperaturesWest<<" + "<<numberExtraTempsWest<<") + "<<mDb->adjustAverageWest()<<"  = "<<(averageBodyTemperatureWest+averageExtraTempsWest)/(numberBodyTemperaturesWest+numberExtraTempsWest) + mDb->adjustAverageWest()<<endm;
        averageGasTempWest = (averageBodyTemperatureWest + averageExtraTempsWest)/(numberBodyTemperaturesWest + numberExtraTempsWest);
        mParam->setGasTemperatureWest(averageGasTempWest + mDb->adjustAverageWest());
        return kStOK;
     }
     else {
        LOG_WARN<<"No FTPC West body temperatures or FTPC Extra temperatures found for run number "<<runNumber<<"dbDate "<<dbDate<<" using defaultTemperatureWest = "<<mParam->gasTemperatureWest()<<endm; 
        return kStWarn;
     }	  
   }
        
   // Year2004: from runs after run 5027147 on 2004-01-27 to the end of run on  2004-05-15, only Body1, Body3 and Body4 temperature readings are useable for FTPC West
   // The averageBodyTemperatureWest must be adjusted since only 3 instead of 6 temperature readings are used
   if ( runNumber > 5027147 && runNumber < 5136001 ) {
      LOG_DEBUG<<"runNumber = "<<runNumber<<" > 5027147 && <= 5136001: only Body1, Body3 and Body4 are useable for FTPC West. The averageBodyTemperatureWest is adjusted by  "<<mDb->adjustAverageWest()<<endm;
      if (mGas->getBody1West() >= mDb->minGasTemperature() && mGas->getBody1West() <= mDb->maxGasTemperature() ) {
         averageBodyTemperatureWest += mGas->getBody1West();	 
	 numberBodyTemperaturesWest++;
	 LOG_DEBUG<<"   "<<mGas->getBody1West()<<"(body1West)"<<endm;
      }  
      if (mGas->getBody3West() >= mDb->minGasTemperature() && mGas->getBody3West() <= mDb->maxGasTemperature() ) {
         averageBodyTemperatureWest += mGas->getBody3West();	 
	 numberBodyTemperaturesWest++;
	 LOG_DEBUG<<" + "<<mGas->getBody3West()<<"(body3West)"<<endm;
      }  
      if (mGas->getBody4West() >= mDb->minGasTemperature() && mGas->getBody4West() <= mDb->maxGasTemperature() ) {
         averageBodyTemperatureWest += mGas->getBody4West();	 
	 numberBodyTemperaturesWest++;
	 LOG_DEBUG<<" + "<<mGas->getBody4West()<<"(body4West)"<<endm;
      }  
   }  

   else {
      if (mGas->getBody1West() >= mDb->minGasTemperature() && mGas->getBody1West() <= mDb->maxGasTemperature()) {      
	 averageBodyTemperatureWest += mGas->getBody1West();
	 numberBodyTemperaturesWest++;
	 LOG_DEBUG<<mGas->getBody1West()<<"(body1West)"<<endm;
      }		 
      if (mGas->getBody2West() >= mDb->minGasTemperature() && mGas->getBody2West()<= mDb->maxGasTemperature() ) {
	 averageBodyTemperatureWest += mGas->getBody2West();
	 numberBodyTemperaturesWest++;
	 LOG_DEBUG<<" + "<<mGas->getBody2West()<<"(body2West)"<<endm;
      }		 
      if (mGas->getBody3West() >= mDb->minGasTemperature() && mGas->getBody3West()<= mDb->maxGasTemperature() ) {
	 averageBodyTemperatureWest += mGas->getBody3West();
	 numberBodyTemperaturesWest++;
	 LOG_DEBUG<<" + "<<mGas->getBody3West()<<"(body3West)"<<endm;
      }		 
      if (mGas->getBody4West() >= mDb->minGasTemperature() && mGas->getBody4West() <= mDb->maxGasTemperature() ) {
	 averageBodyTemperatureWest += mGas->getBody4West();
	 numberBodyTemperaturesWest++;
         LOG_DEBUG<<" + "<<mGas->getBody4West()<<"(body4West)"<<endm;
      }		 
      // from 2003-10-31 -> 2004-01-24 and as of 2004-04-04 there are 2 additional body temperature sensors
      if ( (dbDate >= 20031031 && dbDate <= 20040124) || dbDate >= 20040404) {
         LOG_DEBUG<<"(dbDate = "<<dbDate<<" >= 20031031 && <= 20040124 ||  >= 20040404 activate additional body temperature sensors) "<<endm;
         if (mGas->getBody5West() >= mDb->minGasTemperature() && mGas->getBody5West() <= mDb->maxGasTemperature() ) {
            averageBodyTemperatureWest += mGas->getBody5West();	 
	    numberBodyTemperaturesWest++;
	    LOG_DEBUG<<" + "<<mGas->getBody5West()<<"(body5West)"<<endm;
	 }  
         if (mGas->getBody6West() >= mDb->minGasTemperature() && mGas->getBody6West() <= mDb->maxGasTemperature() ) {
	    averageBodyTemperatureWest += mGas->getBody6West();
	    numberBodyTemperaturesWest++;
	    LOG_DEBUG<<" + "<<mGas->getBody6West()<<"(body6West)"<<endm;
	  }  
      }  
   }      
   LOG_DEBUG<<" = "<<averageBodyTemperatureWest<<endm;

   // calculate average body temperature west
  if (numberBodyTemperaturesWest > 0) {
     LOG_DEBUG<<"setGasTemperatureWest = "<<averageBodyTemperatureWest<<" (averageBodyTemperatureWest) + "<<mDb->adjustAverageWest()<<" (adjustAverageWest) = "<<averageBodyTemperatureWest + mDb->adjustAverageWest()<<endm;	  
     averageBodyTemperatureWest = averageBodyTemperatureWest/numberBodyTemperaturesWest;
     mParam->setGasTemperatureWest(averageBodyTemperatureWest + mDb->adjustAverageWest()); 
     return kStOK;
  }   
  //  if no body temperature readings return warning
  else {
     LOG_WARN<<"No FTPC West body temperatures found for "<<dbDate<<" using defaultTemperatureWest = "<<mParam->gasTemperatureWest()<<endm; 
     return kStWarn;
  }	  
}


Int_t StFtpcGasUtilities::averageTemperatureEast(Int_t dbDate, Int_t runNumber) {
	
   Int_t numberBodyTemperaturesEast = 0;
   Float_t averageBodyTemperatureEast = 0.0;
   Float_t averageGasTempEast = 0.0;

   // In 2010 from "2010-04-13 00:20:01" to "2010-04-19 20:39:00" and
   //         from "2010-04-21 21:40:01" to "2010-04-26 17:19:00" 
   // the extra temperatures for both FTPC West and FTPC East were stuck. 
   // Since one reading was propagated to the database for this whole time period, 
   // we decided to use the default temperatures for all runs
   // taken in this time period.

   if ( ((dbDate >= 20100413 && runNumber > 11102099) && (dbDate <= 20100419 && runNumber < 11109080)) ||
        ((dbDate >= 20100421 && runNumber > 1111068) && (dbDate <= 20100426 && runNumber < 11116056)) ) {
     mParam->setGasTemperatureEast(mDb->defaultTemperatureEast() + mDb->adjustAverageEast());
     LOG_WARN<<"FTPC East Extra temperatures bad for run number "<<runNumber<<" dbDate "<<dbDate<<" using defaultTemperatureEast = "<<mDb->defaultTemperatureEast()<<" + adjustAverageEast = "<<mDb->adjustAverageEast()<<" = "<<mParam->gasTemperatureEast()<<endm;
     return kStWarn;
   } 

   // Starting Y2006, calculate FTPC East gas temperature from extra temperatures only 
   // and adjust with mDb->adjustAverageEast()

   if ( dbDate >= 20051030 ) {
     
    ftpcTemps_st* tempT = mTemps->GetTable();

    Int_t numberExtraTempsEast = 0;
    Float_t averageExtraTempsEast = 0.0;

    if (tempT->extra1East >= mDb->minGasTemperature() && tempT->extra1East <= mDb->maxGasTemperature() ) {
        averageExtraTempsEast += tempT->extra1East;
        numberExtraTempsEast++;
        LOG_DEBUG<<"   "<<tempT->extra1East<<"(extra1East)"<<endm;
    }
    if (tempT->extra2East >= mDb->minGasTemperature() && tempT->extra2East <= mDb->maxGasTemperature() ) {
        averageExtraTempsEast += tempT->extra2East;
        numberExtraTempsEast++;
        LOG_DEBUG<<" + "<<tempT->extra2East<<"(extra2East)"<<endm;
    }
    if (tempT->extra3East >= mDb->minGasTemperature() && tempT->extra3East <= mDb->maxGasTemperature() ) {
        averageExtraTempsEast += tempT->extra3East;
        numberExtraTempsEast++;
        LOG_DEBUG<<" + "<<tempT->extra3East<<"(extra3East)"<<endm;
    }
    if (tempT->extra4East >= mDb->minGasTemperature() && tempT->extra4East <= mDb->maxGasTemperature() ) {
        averageExtraTempsEast += tempT->extra4East;
        numberExtraTempsEast++;
        LOG_DEBUG<<" + "<<tempT->extra4East<<"(extra4East)"<<endm;
    }
    if (tempT->extra5East >= mDb->minGasTemperature() && tempT->extra5East <= mDb->maxGasTemperature() ) {
        averageExtraTempsEast += tempT->extra5East;
        numberExtraTempsEast++;
        LOG_DEBUG<<" + "<<tempT->extra5East<<"(extra5East)"<<endm;
    }
    if (tempT->extra6East >= mDb->minGasTemperature() && tempT->extra6East <= mDb->maxGasTemperature() ) {
        averageExtraTempsEast += tempT->extra6East;
        numberExtraTempsEast++;
        LOG_DEBUG<<" + "<<tempT->extra6East<<"(extra6East)"<<endm;
    }
    if (tempT->extra7East >= mDb->minGasTemperature() && tempT->extra7East <= mDb->maxGasTemperature() ) {
        averageExtraTempsEast += tempT->extra7East;
        numberExtraTempsEast++;
        LOG_DEBUG<<" + "<<tempT->extra7East<<"(extra7East)"<<endm;
    }
    LOG_DEBUG<<" = "<<averageExtraTempsEast<<endm;

    // calculate average extra temperature east
    if (numberExtraTempsEast > 0) {
        LOG_DEBUG<<"setGasTemperatureEast = averageGasTempEast = "<<averageExtraTempsEast<<"/"<<numberExtraTempsEast<<" + "<<mDb->adjustAverageEast()<<"  = "<<averageExtraTempsEast/numberExtraTempsEast + mDb->adjustAverageEast()<<endm;
        averageGasTempEast =  averageExtraTempsEast/numberExtraTempsEast;
        mParam->setGasTemperatureEast(averageGasTempEast + mDb->adjustAverageEast());
        return kStOK;
     }
     else {
        LOG_WARN<<"No FTPC East Extra temperatures found for run number "<<runNumber<<"dbDate "<<dbDate<<" using defaultTemperatureEast = "<<mParam->gasTemperatureEast()<<endm; 
        return kStWarn;
     }	  
   }

   // For Y2005 calculate FTPC East gas temperature from body temperatures 
   // plus extra temperature readings
   // (the extra temperature readings are available as of 2005-01-01)

   if ( dbDate >= 20050101 && dbDate < 20051030 ) {

     LOG_DEBUG<<" dbDate = "<<dbDate<<"(>2005-01-01) Use FTPC East body temperatures + FTPC East extra temperature readings 1-7"<<endm;
        
     if (mGas->getBody1East() >= mDb->minGasTemperature() && mGas->getBody1East() <= mDb->maxGasTemperature()) {      
         averageBodyTemperatureEast += mGas->getBody1East();
         numberBodyTemperaturesEast++;
         LOG_DEBUG<<"   "<<mGas->getBody1East()<<"(body1East)"<<endm;
     }		 
     if (mGas->getBody2East() >= mDb->minGasTemperature() && mGas->getBody2East()<= mDb->maxGasTemperature() ) {
         averageBodyTemperatureEast += mGas->getBody2East();
         numberBodyTemperaturesEast++;
         LOG_DEBUG<<" + "<<mGas->getBody2East()<<"(body2East)"<<endm;
     }		 
     if (mGas->getBody3East() >= mDb->minGasTemperature() && mGas->getBody3East()<= mDb->maxGasTemperature() ) {
         averageBodyTemperatureEast += mGas->getBody3East();
         numberBodyTemperaturesEast++;
         LOG_DEBUG<<" + "<<mGas->getBody3East()<<"(body3East)"<<endm;
     }		 
     if (mGas->getBody4East() >= mDb->minGasTemperature() && mGas->getBody4East() <= mDb->maxGasTemperature() ) {
         averageBodyTemperatureEast += mGas->getBody4East();
         numberBodyTemperaturesEast++;
         LOG_DEBUG<<" + "<<mGas->getBody4East()<<"(body4East)"<<endm;
     }		 
     if (mGas->getBody5East() >= mDb->minGasTemperature() && mGas->getBody5East() <= mDb->maxGasTemperature() ) {
         averageBodyTemperatureEast += mGas->getBody5East();	 
         numberBodyTemperaturesEast++;
         LOG_DEBUG<<" + "<<mGas->getBody5East()<<"(body5East)"<<endm;
     }  
     if (mGas->getBody6East() >= mDb->minGasTemperature() && mGas->getBody6East() <= mDb->maxGasTemperature() ) {
         averageBodyTemperatureEast += mGas->getBody6East();
         numberBodyTemperaturesEast++;
         LOG_DEBUG<<" + "<<mGas->getBody6East()<<"(body6East)"<<endm;
     }  
     LOG_DEBUG<<" = "<<averageBodyTemperatureEast<<endm;

     // calculate average body temperature east
    if (numberBodyTemperaturesEast > 0) {
       LOG_DEBUG<<"averageBodyTemperatureEast = "<<averageBodyTemperatureEast<<"/"<<numberBodyTemperaturesEast<<" = "<<averageBodyTemperatureEast/numberBodyTemperaturesEast<<endm;
    }
     
    ftpcTemps_st* tempT = mTemps->GetTable();

    Int_t numberExtraTempsEast = 0;
    Float_t averageExtraTempsEast = 0.0;

    if (tempT->extra1East >= mDb->minGasTemperature() && tempT->extra1East <= mDb->maxGasTemperature() ) {
        averageExtraTempsEast += tempT->extra1East;
        numberExtraTempsEast++;
        LOG_DEBUG<<"   "<<tempT->extra1East<<"(extra1East)"<<endm;
    }
    if (tempT->extra2East >= mDb->minGasTemperature() && tempT->extra2East <= mDb->maxGasTemperature() ) {
        averageExtraTempsEast += tempT->extra2East;
        numberExtraTempsEast++;
        LOG_DEBUG<<" + "<<tempT->extra2East<<"(extra2East)"<<endm;
    }
    if (tempT->extra3East >= mDb->minGasTemperature() && tempT->extra3East <= mDb->maxGasTemperature() ) {
        averageExtraTempsEast += tempT->extra3East;
        numberExtraTempsEast++;
        LOG_DEBUG<<" + "<<tempT->extra3East<<"(extra3East)"<<endm;
    }
    if (tempT->extra4East >= mDb->minGasTemperature() && tempT->extra4East <= mDb->maxGasTemperature() ) {
        averageExtraTempsEast += tempT->extra4East;
        numberExtraTempsEast++;
        LOG_DEBUG<<" + "<<tempT->extra4East<<"(extra4East)"<<endm;
    }
    if (tempT->extra5East >= mDb->minGasTemperature() && tempT->extra5East <= mDb->maxGasTemperature() ) {
        averageExtraTempsEast += tempT->extra5East;
        numberExtraTempsEast++;
        LOG_DEBUG<<" + "<<tempT->extra5East<<"(extra5East)"<<endm;
    }
    if (tempT->extra6East >= mDb->minGasTemperature() && tempT->extra6East <= mDb->maxGasTemperature() ) {
        averageExtraTempsEast += tempT->extra6East;
        numberExtraTempsEast++;
        LOG_DEBUG<<" + "<<tempT->extra6East<<"(extra6East)"<<endm;
    }
    if (tempT->extra7East >= mDb->minGasTemperature() && tempT->extra7East <= mDb->maxGasTemperature() ) {
        averageExtraTempsEast += tempT->extra7East;
        numberExtraTempsEast++;
        LOG_DEBUG<<" + "<<tempT->extra7East<<"(extra7East)"<<endm;
    }
    LOG_DEBUG<<" = "<<averageExtraTempsEast<<endm;

     // calculate average extra temperature east
    if (numberExtraTempsEast > 0) {
       LOG_DEBUG<<"averageExtraTempsEast = "<<averageExtraTempsEast<<"/"<<numberExtraTempsEast<<" = "<<averageExtraTempsEast/numberExtraTempsEast<<endm;
    }

    // calculate average extra temperature east
    if (numberExtraTempsEast > 0) {
        LOG_DEBUG<<"setGasTemperatureEast = averageGasTempEast = ("<<averageBodyTemperatureEast<<" + "<<averageExtraTempsEast<<")/("<<numberBodyTemperaturesEast<<" + "<<numberExtraTempsEast<<") + "<<mDb->adjustAverageEast()<<"  = "<<(averageBodyTemperatureEast+averageExtraTempsEast)/(numberBodyTemperaturesEast+numberExtraTempsEast) + mDb->adjustAverageEast()<<endm;
        averageGasTempEast = (averageBodyTemperatureEast + averageExtraTempsEast)/(numberBodyTemperaturesEast + numberExtraTempsEast);
        mParam->setGasTemperatureEast(averageGasTempEast + mDb->adjustAverageEast());
        return kStOK;
     }
     else {
        LOG_WARN<<"No FTPC East body temperatures or FTPC Extra temperatures found for run number "<<runNumber<<"dbDate "<<dbDate<<" using defaultTemperatureEast = "<<mParam->gasTemperatureEast()<<endm; 
        return kStWarn;
     }	  
   }
     
   // Year2004: from runs after run 5027147 on 2004-01-27 to run 5093053, the last run of the AuAu 62GeV run on  2004-04-02, only Body3, Body4 and Body5 temperature readings are useable for FTPC East
   // The averageBodyTemperatureEast must be adjusted since only 3 instead of 6 temperature readings are used
   if ( runNumber > 5027147 && runNumber <= 5093053 ) {
      LOG_DEBUG<<"runNumber = "<<runNumber<<" > 5027147 && <= 5093053: only Body3, Body4 and Body5 are useable for FTPC East. The averageBodyTemperatureEast is adjusted by  "<<mDb->adjustAverageEast()<<endm;
      if (mGas->getBody3East() >= mDb->minGasTemperature() && mGas->getBody3East() <= mDb->maxGasTemperature() ) {
         averageBodyTemperatureEast += mGas->getBody3East();	 
	 numberBodyTemperaturesEast++;
	 LOG_DEBUG<<"   "<<mGas->getBody3East()<<"(body3East)"<<endm;
      }  
      if (mGas->getBody4East() >= mDb->minGasTemperature() && mGas->getBody4East() <= mDb->maxGasTemperature() ) {
         averageBodyTemperatureEast += mGas->getBody4East();	 
	 numberBodyTemperaturesEast++;
	 LOG_DEBUG<<" + "<<mGas->getBody4East()<<"(body4East)"<<endm;
      }  
      if (mGas->getBody5East() >= mDb->minGasTemperature() && mGas->getBody5East() <= mDb->maxGasTemperature() ) {
         averageBodyTemperatureEast += mGas->getBody5East();	 
	 numberBodyTemperaturesEast++;
	 LOG_DEBUG<<" + "<<mGas->getBody5East()<<"(body5East)"<<endm;
      }  
   }  

   else {

      if (mGas->getBody1East() >= mDb->minGasTemperature() && mGas->getBody1East() <= mDb->maxGasTemperature()) {      
		 averageBodyTemperatureEast += mGas->getBody1East();
		 numberBodyTemperaturesEast++;
	         LOG_DEBUG<<mGas->getBody1East()<<"(body1East)"<<endm;
      }		 
      if (mGas->getBody2East() >= mDb->minGasTemperature() && mGas->getBody2East()<= mDb->maxGasTemperature() ) {
		 averageBodyTemperatureEast += mGas->getBody2East();
		 numberBodyTemperaturesEast++;
	         LOG_DEBUG<<" + "<<mGas->getBody2East()<<"(body2East)"<<endm;
      }		 
      if (mGas->getBody3East() >= mDb->minGasTemperature() && mGas->getBody3East()<= mDb->maxGasTemperature() ) {
		 averageBodyTemperatureEast += mGas->getBody3East();
		 numberBodyTemperaturesEast++;
	         LOG_DEBUG<<" + "<<mGas->getBody3East()<<"(body3East)"<<endm;
      }		 
      if (mGas->getBody4East() >= mDb->minGasTemperature() && mGas->getBody4East() <= mDb->maxGasTemperature() ) {
		 averageBodyTemperatureEast += mGas->getBody4East();
		 numberBodyTemperaturesEast++;
	         LOG_DEBUG<<" + "<<mGas->getBody4East()<<"(body4East)"<<endm;
      }		 
      // from 2003-10-31 -> 2004-01-24 and as of 2004-04-04 there are 2 additional body temperature sensors
      if ( (dbDate >= 20031031 && dbDate <= 20040124) || dbDate >= 20040404 ) {
         LOG_DEBUG<<"(dbDate = "<<dbDate<<" >= 20031031 && <= 20040124 ||  >= 20040404 activate additional body temperature sensors) ";
         if (mGas->getBody5East() >= mDb->minGasTemperature() && mGas->getBody5East() <= mDb->maxGasTemperature() ) {
            averageBodyTemperatureEast += mGas->getBody5East();	 
	    numberBodyTemperaturesEast++;
	    LOG_DEBUG<<" + "<<mGas->getBody5East()<<"(body5East)"<<endm;
	 }  
         if (mGas->getBody6East() >= mDb->minGasTemperature() && mGas->getBody6East() <= mDb->maxGasTemperature() ) {
	    averageBodyTemperatureEast += mGas->getBody6East();
	    numberBodyTemperaturesEast++;
	    LOG_DEBUG<<mGas->getBody6East()<<"(body6East)"<<endm;
	 } 
       }  
   }      
   LOG_DEBUG<<" = "<<averageBodyTemperatureEast<<endm;

      // calculate average body temperature east
      if (numberBodyTemperaturesEast > 0) {
        LOG_DEBUG<<"setGasTemperatureEast = "<<averageBodyTemperatureEast<<" (averageBodyTemperatureEast) + "<<mDb->adjustAverageEast()<<" (adjustAverageEast) = "<<averageBodyTemperatureEast + mDb->adjustAverageEast()<<endm;	  
        averageBodyTemperatureEast = averageBodyTemperatureEast/numberBodyTemperaturesEast;
        mParam->setGasTemperatureEast(averageBodyTemperatureEast + mDb->adjustAverageEast()); 
        return kStOK;
     }   
     //  if no body temperature readings return warning
     else {
        LOG_WARN<<"No FTPC East body temperatures found for "<<dbDate<<" using defaultTemperatureEast = "<<mParam->gasTemperatureEast()<<endm; 
        return kStWarn;
     }	  
}

Int_t StFtpcGasUtilities::defaultTemperatureWest(Int_t dbDate,Bool_t SVT_On) {
    if ( !SVT_On) {
       mParam->setGasTemperatureWest(mDb->defaultTemperatureWest());
       LOG_WARN<<"No valid body temperatures available for FTPC West; Initialize to defaultTemperatureWest (for dbDate = "<<dbDate<<" SVT off) = "<<mParam->gasTemperatureWest()<<endm;
    }
    if (SVT_On) {
	if (dbDate < 20021105) {
	   // for year 2001 data (AuAu,pp) FTPC west gas temperature is higher when SVT on
           mParam->setGasTemperatureWest(mDb->defaultTemperatureWest() + mDb->temperatureDifference());
           LOG_WARN<<"No valid body temperatures available for FTPC West; Initialize to mDb->defaultTemperatureWest() + mDb->temperatureDifference() (for year2001 data,SVT on) = "<<mParam->gasTemperatureWest()<<endm;
	}	   
        if (dbDate >= 20021105) { 
           // starting in year 2003,the FTPC west gas temperature is not effected by SVT
           mParam->setGasTemperatureWest(mDb->defaultTemperatureWest());
           LOG_WARN<<"No valid body temperatures available for FTPC West; Initialize to mDb->defaultTemperatureWest() (as of year2003, SVT on) = "<<mParam->gasTemperatureWest()<<endm;
	}
  }	  
  return kStOK;
}    

Int_t StFtpcGasUtilities::defaultTemperatureEast(Int_t dbDate,Bool_t SVT_On) {
    if ( !SVT_On) {
       mParam->setGasTemperatureEast(mDb->defaultTemperatureEast());
       LOG_WARN<<"No valid body temperatures available for FTPC East; Initialize to defaultTemperatureEast (for dbDate = "<<dbDate<<" SVT off) = "<<mParam->gasTemperatureEast()<<endm;
    }
    if (SVT_On) {
	if (dbDate < 20021105) {
           // for year 2001 data (dAu,pp) FTPC east gas temperature is not effected by SVT
           mParam->setGasTemperatureEast(mDb->defaultTemperatureEast());
           LOG_WARN<<"No valid body temperatures available for FTPC East; Initialize to mDb->defaultTemperatureEast() (for year2001 data, SVT on) = "<<mParam->gasTemperatureEast()<<endm;
	}	   
        if (dbDate >= 20021105) { 
           // starting in year 2003 data the FTPC east gas temperature is higher when SVT on
           mParam->setGasTemperatureEast(mDb->defaultTemperatureEast() + mDb->temperatureDifference());
           LOG_WARN<<"No valid body temperatures available for FTPC East; Initialize to mDb->defaultTemperatureEast() + mDb->temperatureDifference() (as of year2003,SVT on) = "<<mParam->gasTemperatureEast()<<endm;
	}
    }	
  return kStOK;
  }	  
