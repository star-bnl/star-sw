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

}

//==================================================
StBemcTriggerDbThresholds::~StBemcTriggerDbThresholds(){ 

  LOG_INFO<<"StBemcTriggerDbThresholds::deconstructor"<<endl;

}

//==================================================
//==================================================
void StBemcTriggerDbThresholds::LoadTimeStamps(){

  //settings for year 2006
  // subtract 10 seconds from each beginTime and add 10 to each endTime
  // just in case DB record is off be a second (it happens) -- APK
 
  //#0   Runs#7097009 - 7101054   2006-04-07 04:43:31 to 2006-04-11 17:21:09
  start_2006[0].Set(2006,04,07,04,43,21);
  end_2006[0].Set(2006,04,11,17,21,19);
  //#1   Runs#7101075 - 7129065   2006-04-12 01:31:40 to 2006-05-09 21:16:07
  start_2006[1].Set(2006,4,12,1,31,30);
  end_2006[1].Set(2006,5,9,21,16,17);
  //#2   Runs#7131043 - 7132027   2006-05-12 03:59:16 to 2006-05-12 09:16:22
  start_2006[2].Set(2006,5,12,3,59,6);
  end_2006[2].Set(2006,5,12,9,16,32);
  //#3   Runs#7132062 - 7133051   2006-05-12 20:44:34 to 2006-05-13 21:50:03
  start_2006[3].Set(2006,5,12,20,44,24);
  end_2006[3].Set(2006,5,13,21,50,13);
  //#4   Runs#7133052 - 7135028   2006-05-13 21:52:49 to 2006-05-15 16:47:03
  start_2006[4].Set(2006,5,13,21,52,39);
  end_2006[4].Set(2006,5,15,16,47,13);		
  //#5   Runs#7135067 - 7156028   2006-05-16 02:38:55 to 2006-06-05 15:49:17
  start_2006[5].Set(2006,5,16,2,38,45);
  end_2006[5].Set(2006,6,5,15,49,27);
  

  //settings for year 2007

  //#0 Runs#8094008 - 8102029 2007-04-04 05:38:54 to 2007-04-12 09:13:17
  //200211 - bht2-mb (somewhere in here bit0 changed from 6 to 3!)
  start_2007[0].Set(2007,4,4,5,38,44);
  end_2007[0].Set(2007,4,12,9,13,27);

  //#1 Runs#8097121 - 8108014 2007-04-08 03:26:09 to 2007-04-18 08:23:00
  //200585 - bht2
  start_2007[1].Set(2007,4,8,3,25,59);
  end_2007[1].Set(2007,4,15,8,23,10);

  //#2 Runs#8103029 - 8113068 2007-04-13 20:09:39 to 2007-04-23 17:24:13 
  //200212 - bht2-mb 
  //200213 - btag, 200601 - L2-upsilon
  //200220 - bht2-mb
  //200601 - L2-upsilon bht1
  start_2007[2].Set(2007,4,13,20,9,29);
  end_2007[2].Set(2007,4,23,17,24,23);
  
  //#3 Runs#8109015 - 8177048 2007-04-19 05:01:58 to 2007-06-26 12:57:31 	
  //200221 - bht2-mb, 200620 - L2-gamma 
  //200586 - bht2, 200222 - bht2-mb, 200214 - btag, 
  //200602 - L2-upsilon, 200621 - L2-gamma (HTbits = 3,18,23)
  start_2007[3].Set(2007,4,19,5,1,48);
  end_2007[3].Set(2007,6,26,12,57,41);

  //settings for 2008  production_dAu2008

  //#1 Runs#8340015 - 8344057  2007-12-06 12:39:20 - 2007-12-10 14:42:45
  //210500 HT0, 210510 HT1, 210520 HT2
  start_2008[0].Set(2007,12,6,12,39,10);
  end_2008[0].Set(2007,12,10,2,42,55);

  //#2 Runs#8344088 - 9027087 2007-12-10 19:42:37 - 2008-01-28 00:55:57 
  //210501 HT0, 210511 HT1, 210521 HT2, 210541 HT4
  start_2008[1].Set(2007,12,10,19,42,27);
  end_2008[1].Set(2008,1,28,0,56,7);

  //settings for 2008 production_pp

  //#1 Runs#9045006-9070007 2008-02-14 07:03:52 - 2008-03-10 11:02:05 
  //220500 HT0, 220510 HT1, 220520 HT2
  start_2008[2].Set(2008,2,14,7,3,42);
  end_2008[2].Set(2008,3,10,11,1,55);

  //settings for 2009 production 500 GeV
  start_2009[0].Set(2009,2,17,0,0,0);
  end_2009[0].Set(2009,4,15,0,0,0);

  //settings for 2009 production 200 GeV
  start_2009[1].Set(2009,4,15,0,0,1);
  end_2009[1].Set(2009,5,15,0,0,0);


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
Int_t  StBemcTriggerDbThresholds::GetTP_DSM0_threshold(Int_t DSMmodule, UInt_t timestamp, Int_t layer){

  Int_t threshold=-1;

  //2006 TP thresholds
  const Int_t TP0_TH_2006[6]  = {  1,  1,  1,  1,  1,  1};
  const Int_t TP1_TH_2006[6]  = { 17, 17, 20, 20, 19, 19};
  const Int_t TP2_TH_2006[6]  = { 31, 31, 31, 31, 31, 31};
  for (int i=0; i<6;i++)
    {
      if (DSMmodule<15)
	{//WEST
	  if ((timestamp>=start_2006[i].Get())&&(timestamp<=end_2006[i].Get()))
	    {
	      if (layer==0) threshold=TP0_TH_2006[i];
	      if (layer==1) threshold=TP1_TH_2006[i];
	      if (layer==2) threshold=TP2_TH_2006[i];
	    } 
	}
      
      if (DSMmodule>=15)
	{//EAST 
	  if ((timestamp>=start_2006[i].Get())&&(timestamp<=end_2006[i].Get()))
	    {
	      
	      if (layer==0) threshold=TP0_TH_2006[i];
	      if (layer==1) threshold=TP1_TH_2006[i];
	      if (layer==2) threshold=TP2_TH_2006[i];
	    }
	}
    }


  //There were no TP thresholds in 2007 
  
  //2008 TP thresholds
  const Int_t TP0_TH_2008[6]  = {  24,  24,  24,  24,  24,  24};
  for (int i=0; i<6;i++)
    {
      if (DSMmodule<15)
	{//WEST
	  if ((timestamp>=start_2008[i].Get())&&(timestamp<=end_2008[i].Get()))
	    {
	      if (layer==0) threshold=TP0_TH_2008[i];
	    } 
	}
      
      if (DSMmodule>=15)
	{//EAST 
	  if ((timestamp>=start_2008[i].Get())&&(timestamp<=end_2008[i].Get()))
	    {	      
	      if (layer==0) threshold=TP0_TH_2008[i];
	    }
	}
    }

  return threshold;
}

//==================================================
//==================================================
Int_t  StBemcTriggerDbThresholds::GetJP_DSM1_threshold(Int_t DSMmodule,UInt_t timestamp, Int_t layer){

  Int_t threshold=-1;

  //2006 JP thresholds
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
  
  //There were no JP thresholds in 2007

  //2008 JP thresholds
  const Int_t JP0_TH_2008[6]    = { 50, 50, 50, 50, 50, 50};
  const Int_t JP1_TH_2008[6]    = { 87, 87, 87, 87, 87, 87};
  const Int_t JP2_TH_2008[6]    = {100,100,100,100,100,100};
  for (int i=0;i<6;i++)
    {
      if (DSMmodule<15)
	{//WEST
	  if ((timestamp>=start_2008[i].Get())&&(timestamp<=end_2008[i].Get()))
	    {
	      if (layer==0) threshold=JP0_TH_2008[i];
	      if (layer==1) threshold=JP1_TH_2008[i];
	      if (layer==2) threshold=JP2_TH_2008[i];
	    }	  
	}
      
      
      if (DSMmodule>=15)
	{//EAST 
	  if ((timestamp>=start_2008[i].Get())&&(timestamp<=end_2008[i].Get()))
	    {      
	      if (layer==0) threshold=JP0_TH_2008[i];
	      if (layer==1) threshold=JP1_TH_2008[i];
	      if (layer==2) threshold=JP2_TH_2008[i];
	    }
	}
    }
  
  
  


  //2009 JP thresholds
  const Int_t JP0_TH_2009[2]    = { 28, 20};
  const Int_t JP1_TH_2009[2]    = { 35, 28};
  const Int_t JP2_TH_2009[2]    = { 52, 35};
  for (int i=0;i<2;i++)
    {
      if (DSMmodule<15)
	{//WEST
	  if ((timestamp>=start_2009[i].Get())&&(timestamp<=end_2009[i].Get()))
	    {
	      if (layer==0) threshold=JP0_TH_2009[i];
	      if (layer==1) threshold=JP1_TH_2009[i];
	      if (layer==2) threshold=JP2_TH_2009[i];
	    }	  
	}
      
      
      if (DSMmodule>=15)
	{//EAST 
	  if ((timestamp>=start_2009[i].Get())&&(timestamp<=end_2009[i].Get()))
	    {      
	      if (layer==0) threshold=JP0_TH_2009[i];
	      if (layer==1) threshold=JP1_TH_2009[i];
	      if (layer==2) threshold=JP2_TH_2009[i];
	    }
	}
    }  
  

  return threshold;
}

//==================================================
//==================================================
Int_t  StBemcTriggerDbThresholds::GetHT_DSM0_threshold(Int_t DSMmodule, UInt_t timestamp, Int_t layer){

  int threshold=-1;

  //2006 HT East and West Thresholds
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


  //2007 HT East and West Thresholds (no separation of east or west in 2007)
  const Int_t HTW0_TH_2007[4]   = {  6,  4,  3,  3};
  const Int_t HTW1_TH_2007[4]   = { 18, 18, 18, 18};
  const Int_t HTW2_TH_2007[4]   = { 24, 24, 24, 23};
  const Int_t HTE0_TH_2007[4]   = {  6,  4,  3,  3};
  const Int_t HTE1_TH_2007[4]   = { 18, 18, 18, 18};
  const Int_t HTE2_TH_2007[4]   = { 24, 24, 24, 23};
  for (int i=0;i<4;i++){
    
    if (DSMmodule<15)
      {//WEST
	if ((timestamp>=start_2007[i].Get())&&(timestamp<=end_2007[i].Get()))
	  {
	    if (layer==0) threshold=HTW0_TH_2007[i];
	    if (layer==1) threshold=HTW1_TH_2007[i];
	    if (layer==2) threshold=HTW2_TH_2007[i];
	  }	
      }
    
    
    if (DSMmodule>=15)
      {//EAST 
	if ((timestamp>=start_2007[i].Get())&&(timestamp<=end_2007[i].Get()))
	  { 
	    if (layer==0) threshold=HTE0_TH_2007[i];
	    if (layer==1) threshold=HTE1_TH_2007[i];
	    if (layer==2) threshold=HTE2_TH_2007[i];
	  }
      }
  }


  //2008 HT East and West Thresholds
  const Int_t HTW0_TH_2008[6]   = { 11, 11, 11};
  const Int_t HTW1_TH_2008[6]   = { 15, 15, 15};
  const Int_t HTW2_TH_2008[6]   = { 18, 18, 18};
  const Int_t HTW3_TH_2008[6]   = { 35, 35, 16};
  const Int_t HTW4_TH_2008[6]   = { 35, 35, 63};

  const Int_t HTE0_TH_2008[6]   = { 11, 11, 11};
  const Int_t HTE1_TH_2008[6]   = { 15, 15, 15};
  const Int_t HTE2_TH_2008[6]   = { 18, 18, 18};
  const Int_t HTE3_TH_2008[6]   = { 35, 35, 16};
  const Int_t HTE4_TH_2008[6]   = { 35, 35, 63};

 for (int i=0;i<3;i++){
    
    if (DSMmodule<15)
      {//WEST
	if ((timestamp>=start_2008[i].Get())&&(timestamp<=end_2008[i].Get()))
	  {
	    if (layer==0) threshold=HTW0_TH_2008[i];
	    if (layer==1) threshold=HTW1_TH_2008[i];
	    if (layer==2) threshold=HTW2_TH_2008[i];
	    if (layer==3) threshold=HTW3_TH_2008[i];
	    if (layer==4) threshold=HTW4_TH_2008[i];
	  }
	
      }
    
    
    if (DSMmodule>=15)
      {//EAST 
	if ((timestamp>=start_2008[i].Get())&&(timestamp<=end_2008[i].Get()))
	  {
	    if (layer==0) threshold=HTE0_TH_2008[i];
	    if (layer==1) threshold=HTE1_TH_2008[i];
	    if (layer==2) threshold=HTE2_TH_2008[i];
	    if (layer==3) threshold=HTE3_TH_2008[i];
	    if (layer==4) threshold=HTE4_TH_2008[i]; 
	  }
      }
  }




  //2009 HT East and West Thresholds
  const Int_t HT0_TH_2009[2]   = { 11, 11};
  const Int_t HT1_TH_2009[2]   = { 15, 15};
  const Int_t HT2_TH_2009[2]   = { 19, 17};
  const Int_t HT3_TH_2009[2]   = { 25, 23};

 for (int i=0;i<2;i++){
    
    if (DSMmodule<15)
      {//WEST
	if ((timestamp>=start_2009[i].Get())&&(timestamp<=end_2009[i].Get()))
	  {
	    if (layer==0) threshold=HT0_TH_2009[i];
	    if (layer==1) threshold=HT1_TH_2009[i];
	    if (layer==2) threshold=HT2_TH_2009[i];
	    if (layer==3) threshold=HT3_TH_2009[i];
	  }
	
      }
    
    
    if (DSMmodule>=15)
      {//EAST 
	if ((timestamp>=start_2009[i].Get())&&(timestamp<=end_2009[i].Get()))
	  {
	    if (layer==0) threshold=HT0_TH_2009[i];
	    if (layer==1) threshold=HT1_TH_2009[i];
	    if (layer==2) threshold=HT2_TH_2009[i];
	    if (layer==3) threshold=HT3_TH_2009[i];
	  }
      }
  }

  return threshold;
  
}
