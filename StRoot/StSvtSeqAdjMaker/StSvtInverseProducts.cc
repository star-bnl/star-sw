/***************************************************************************
 *
 * $Id: StSvtInverseProducts.cc,v 1.1 2000/06/15 20:04:54 caines Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************
 *
 * $Log: StSvtInverseProducts.cc,v $
 * Revision 1.1  2000/06/15 20:04:54  caines
 * Initial versions of sequence adjusting codes
 *
 **************************************************************************/
#include <iostream.h>
#include <fstream.h>
#include <stdlib.h>
#include <math.h>

#include "StSequence.hh"
#include "StSvtClassLibrary/StSvtHybridData.hh"
#include "StSvtInverseProducts.hh"


StSvtInverseProducts::StSvtInverseProducts()
{
 mHybridData = NULL;

}

StSvtInverseProducts::~StSvtInverseProducts()
{}

void StSvtInverseProducts::SetHybridPointer(StSvtHybridData* hybData)
{
 mHybridData = hybData;
  for(int j = 0; j < 128; j++)
   mBuffer[j] = 0;
}

void StSvtInverseProducts::FillProbTable(ifstream & inseqFile)
{
  for( int j=0; j<14; j++)
   {
     inseqFile >> mProbTable[j];
     //cout<<mProbTable[j]<<endl;
   }
}



void StSvtInverseProducts::FindInvProducts( int PedOffSet, int Anode)
{
 StSequence* mSequence;
 unsigned char*  adcValue;
 int mNumOfSequence,status;
 int startTBin, length;
 double mProdOfInvProb;

 
 status = mHybridData->getListSequences(Anode,mNumOfSequence,mSequence);
 
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
	     k = (int)adcValue[j] - PedOffSet;
	   }
	   if( k >= 0 && k < 13) mProdOfInvProb *= mProbTable[k]; 
	   else if(k >= -13 && k < 0) mProdOfInvProb *= (1 - mProbTable[abs(k)]);
	   else if(k > 13) mProdOfInvProb *= mProbTable[13];
	   else  mProdOfInvProb *= (1 - mProbTable[13]);
	   }
         //cout<<mProdOfInvProb<<endl;
	 // cout<< log(mProdOfInvProb)<<endl;
         mBuffer[i + startTBin] = log10(mProdOfInvProb);
	 
       }
   }
   
}

double  StSvtInverseProducts::GetBuffer(int timBin)
{
  return mBuffer[timBin];
}


void  StSvtInverseProducts::ResetBuffer()
{
    for(int mTBin = 0; mTBin <128; mTBin++)
      mBuffer[mTBin] = 0;
  
}

