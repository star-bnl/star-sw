/***************************************************************************
 *
 * $Id: StSvtInverseProducts.cc,v 1.5 2003/09/02 17:59:08 perev Exp $
 *
 * Author: Selemon Bekele
 ***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************
 *
 * $Log: StSvtInverseProducts.cc,v $
 * Revision 1.5  2003/09/02 17:59:08  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.4  2000/11/30 20:45:56  caines
 * Dynamically calc prob values, use database
 *
 * Revision 1.2  2000/07/11 18:36:15  caines
 * Updates to save more of sequence for fitting
 *
 * Revision 1.1  2000/06/15 20:04:54  caines
 * Initial versions of sequence adjusting codes
 *
 **************************************************************************/
#include <Stiostream.h>
#include "Stiostream.h"
#include <stdlib.h>
#include <math.h>

#include "StSequence.hh"
#include "StSvtClassLibrary/StSvtHybridData.hh"
#include "StSvtInverseProducts.hh"


StSvtInverseProducts::StSvtInverseProducts()
{
  ResetBuffer();
}

StSvtInverseProducts::~StSvtInverseProducts()
{}

void StSvtInverseProducts::SetProbTable(StSvtProbValues* probValues)
{
  for( int j = 0; j < MAX_ADC_COUNTS; j++)
    mProbTable[j] = probValues->GetProbValue(j);
}


void StSvtInverseProducts::FindInvProducts(StSvtHybridData* hybridData, int anode, int pedOffSet)
{
 StSequence* mSequence;
 unsigned char*  adcValue;
 int mNumOfSequence,status;
 int startTBin, length;
 double mProdOfInvProb;

 status = hybridData->getListSequences(anode,mNumOfSequence,mSequence);
  
 for(int mSeq = 0; mSeq < mNumOfSequence; mSeq++)
   {
     startTBin = mSequence[mSeq].startTimeBin;
     length = mSequence[mSeq].length;
     adcValue = mSequence[mSeq].firstAdc;
     
     for(int i = 0; i < length ; i++)
       {	 
	 // Now actually calcs products not inverse products for speed  
         mProdOfInvProb = 1;
         for(int j = i - 1; j<= i+1; j++)
	   {int k;
	   if(j == -1 || j == length)
	     k = 0;
	   else{
	     k = (int)adcValue[j] - pedOffSet;
	   }
	   if( k >= 0 && k < (MAX_ADC_COUNTS-1)) mProdOfInvProb *= mProbTable[k]; 
	   else if(k >= -(MAX_ADC_COUNTS-1)  && k < 0) mProdOfInvProb *= 1/(1 - 1/mProbTable[abs(k)]);
	   else if(k > (MAX_ADC_COUNTS-1) ) mProdOfInvProb *= mProbTable[MAX_ADC_COUNTS - 1];
	   else  mProdOfInvProb *= 1/(1 - 1/mProbTable[MAX_ADC_COUNTS - 1]);
	   }

         mBuffer[i + startTBin] = log10(mProdOfInvProb);
	 
       }
   }
   
}

double  StSvtInverseProducts::GetBuffer(int timeBin)
{
  return mBuffer[timeBin];
}


void  StSvtInverseProducts::ResetBuffer()
{
    for(int mTBin = 0; mTBin <128; mTBin++)
      mBuffer[mTBin] = 0;
  
}

