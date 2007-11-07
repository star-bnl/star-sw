#include "StBemcTriggerDbThresholds.h"

//General
#include <StMessMgr.h>
#include "TDatime.h"
//Db
#include "St_db_Maker/St_db_Maker.h"
  

ClassImp(StBemcTriggerDbThresholds);

//==================================================

StBemcTriggerDbThresholds::StBemcTriggerDbThresholds() {
  
  LOG_INFO<<"StBemcTriggerDbThresholds::constructor"<<endm; 

 //settings for year 2006
  //#0   Runs#7097009 - 7101054   2006-04-07 04:43:31 to 2006-04-11 17:21:09
  start_2006[0].Set(2006,04,07,04,43,31);
  end_2006[0].Set(2006,04,11,17,21,9);
  //#1   Runs#7101075 - 7129065   2006-04-12 01:31:40 to 2006-05-09 21:16:07
  start_2006[1].Set(2006,4,12,1,31,40);
  end_2006[1].Set(2006,5,9,21,16,7);
  //#2   Runs#7131043 - 7132027   2006-05-12 03:59:16 to 2006-05-12 09:16:22
  start_2006[2].Set(2006,5,12,3,59,16);
  end_2006[2].Set(2006,5,12,9,16,22);
  //#3   Runs#7132062 - 7133051   2006-05-12 20:44:34 to 2006-05-13 21:50:03
  start_2006[3].Set(2006,5,12,20,44,34);
  end_2006[3].Set(2006,5,13,21,50,3);
  //#4   Runs#7133052 - 7135028   2006-05-13 21:52:49 to 2006-05-13 22:46:00
  start_2006[4].Set(2006,5,13,21,52,49);
  end_2006[4].Set(2006,5,13,22,46,0);		
  //#5   Runs#7135067 - 7156028   2006-05-16 02:38:55 to 2006-06-05 15:49:17
  start_2006[5].Set(2006,5,16,2,38,55);
  end_2006[5].Set(2006,6,5,15,49,17);
  
}

//==================================================
StBemcTriggerDbThresholds::~StBemcTriggerDbThresholds(){ 

  LOG_INFO<<"StBemcTriggerDbThresholds::deconstructor"<<endl;

}


//==================================================
//==================================================
Int_t  StBemcTriggerDbThresholds::GetHtFEEbitOffset(int year){

  LOG_INFO <<"StBemcTriggerDbThresholds::GetHtFEEbitOffset()"<<endm;
  
  int bitOffset=2;

  (year>2005) ? bitOffset=2 : bitOffset=3;
 
  return bitOffset;

}



//==================================================
//==================================================
Int_t  StBemcTriggerDbThresholds::GetHTTP_DSM0_threshold(Int_t DSMmodule, UInt_t timestamp, Int_t layer){

  LOG_INFO <<"StBemcTriggerDbThresholds::GetHTTP_DSM0_threshold()"<<endm;

  Int_t threshold=-1;

  const Int_t HTTP0_TH_2006[6]  = {  1,  1,  1,  1,  1,  1};
  const Int_t HTTP1_TH_2006[6]  = { 17, 17, 20, 20, 19, 19};
  const Int_t HTTP2_TH_2006[6]  = { 31, 31, 31, 31, 31, 31};
  for (int i=0; i<6;i++)
    {
      if (DSMmodule<15)
	{//WEST
	  if ((timestamp>=start_2006[i].Get())&&(timestamp<=end_2006[i].Get()))
	    {
	      if (layer==0) threshold=HTTP0_TH_2006[i];
	      if (layer==1) threshold=HTTP1_TH_2006[i];
	      if (layer==2) threshold=HTTP2_TH_2006[i];
	    } 
	}
      
      if (DSMmodule>=15)
	{//EAST 
	  if ((timestamp>=start_2006[i].Get())&&(timestamp<=end_2006[i].Get()))
	    {
	      
	      if (layer==0) threshold=HTTP0_TH_2006[i];
	      if (layer==1) threshold=HTTP1_TH_2006[i];
	      if (layer==2) threshold=HTTP2_TH_2006[i];
	    }
	}
    }
  
  return threshold;
}

//==================================================
//==================================================
Int_t  StBemcTriggerDbThresholds::GetJP_DSM0_threshold(Int_t DSMmodule,UInt_t timestamp, Int_t layer){

  LOG_INFO <<"StBemcTriggerDbThresholds::GetJP_DSM0_threshold()"<<endm;
  
  Int_t threshold=-1;

  const Int_t JP0_TH_2006[6]    = { 42, 42, 48, 49, 49, 49};
  const Int_t JP1_TH_2006[6]    = { 58, 58, 58, 60, 60, 60};
  const Int_t JP2_TH_2006[6]    = {110,110,110,110,110,110};

  for (int i=0;i<6;i++)
    {
      if (DSMmodule<15)
	{//WEST
	  if ((timestamp>=start_2006[i].Get())&&(timestamp<=end_2006[i].Get()))
	    {
	    if (layer==0) threshold=JP0_TH_2006[i];
	    if (layer==1) threshold=JP1_TH_2006[i];
	    if (layer==2) threshold=JP2_TH_2006[i];
	  }	  
	}
      
      
      if (DSMmodule>=15)
	{//EAST 
	  if ((timestamp>=start_2006[i].Get())&&(timestamp<=end_2006[i].Get()))
	    {      
	      if (layer==0) threshold=JP0_TH_2006[i];
	      if (layer==1) threshold=JP1_TH_2006[i];
	      if (layer==2) threshold=JP2_TH_2006[i];
	    }
	}
    }
  
  
  return threshold;
}

//==================================================
//==================================================
Int_t  StBemcTriggerDbThresholds::GetHT_DSM0_threshold(Int_t DSMmodule, UInt_t timestamp, Int_t layer){

  LOG_INFO <<"StBemcTriggerDbThresholds::GetHT_DSM0_threshold()"<<endm;

  int threshold=-1;

  //2006
  const Int_t HTW0_TH_2006[6]   = {  5,  5,  5,  5,  5,  5};
  const Int_t HTW1_TH_2006[6]   = { 12, 12, 16, 18, 16, 16};
  const Int_t HTW2_TH_2006[6]   = { 22, 24, 24, 24, 24, 24};
  const Int_t HTE0_TH_2006[6]   = { 11, 11,  5,  5,  5,  5};
  const Int_t HTE1_TH_2006[6]   = { 12, 12, 16, 18, 16, 16};
  const Int_t HTE2_TH_2006[6]   = { 24, 24, 24, 24, 24, 24};
  for (int i=0;i<6;i++){
    
    if (DSMmodule<15)
      {//WEST
	if ((timestamp>=start_2006[i].Get())&&(timestamp<=end_2006[i].Get()))
	  {
	    if (layer==0) threshold=HTW0_TH_2006[i];
	    if (layer==1) threshold=HTW1_TH_2006[i];
	    if (layer==2) threshold=HTW2_TH_2006[i];
	  }
	
      }
    
    
    if (DSMmodule>=15)
      {//EAST 
	if ((timestamp>=start_2006[i].Get())&&(timestamp<=end_2006[i].Get()))
	  {
	    
	    if (layer==0) threshold=HTE0_TH_2006[i];
	    if (layer==1) threshold=HTE1_TH_2006[i];
	    if (layer==2) threshold=HTE2_TH_2006[i];
	  }
      }
  }
  
  return threshold;
  
}





