/***************************************************************************
 *
 * $Id: StSvtProbValues.cc,v 1.1 2000/06/15 20:04:54 caines Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************
 *
 * $Log: StSvtProbValues.cc,v $
 * Revision 1.1  2000/06/15 20:04:54  caines
 * Initial versions of sequence adjusting codes
 *
 **************************************************************************/


#include <iostream.h>
#include <fstream.h>
#include <math.h>
#include <stdlib.h>
#include "StSvtProbValues.hh"

StSvtProbValues::StSvtProbValues()
{

  //Initialize the various variables
 mAdcCounts = 0; 
 mSigma = 2.1; //the standared deviation
 
 for(int i = 0; i <20; i++)
   {
    mProb[i] = 0;
    mSum[i] = 0;
    mErrf[i] = 0;
   }
}

StSvtProbValues::~StSvtProbValues()
{}



void StSvtProbValues::Prob()
{
 for(int i = 0; i < 15; i++)
  {
   mAdcCounts = i;

   double mFactorial = 0;
   double mPowerTerm = 0;
   double mPowerTermSquared = 0;
   double mCountTerm = 0;
  
   int N = 200; //number of terms to include( as many as needed)
 
  for(int j = 0; j<=N; j++)
    {
    
     if(j==0)
       {
        mFactorial = 1.0;
        mPowerTerm = (double)(mAdcCounts/(sqrt(2)*mSigma));  
        mPowerTermSquared = mPowerTerm*mPowerTerm; 
        mCountTerm = mPowerTerm; 
       }

     else 
      {
       mFactorial = (double)j*mFactorial;
       mPowerTerm = (mPowerTermSquared)*(double)fabs(mPowerTerm); // the numerator in each term of the error function sum without regard to sign since each numerator can be written as x**n = (x**2) * (x**(n-2)) where x**n is the current numerator and (x**(n-2)) the previous one


       if(j%2 == 1) //check for odd terms to get the right sign
        mPowerTerm = (-1)*mPowerTerm;
      
       mCountTerm =(mPowerTerm /( mFactorial*(double)(2*j + 1))); //the required term without the factor 2/sqrt(3.14)

      }

     mSum[i] += mCountTerm; //sum the terms
    
    }

  mErrf[i] = (double)(2.0/sqrt(acos(-1)))*mSum[i];
  mProb[i] = 0.5*(1.0 - mErrf[i]); //probability to get more than a given adc count
//prbability to get less than a given adc count but greater than 0 will be (1 - mProb[i])
 
  //Since a gaussian distribution is symmetric about the mean, for negative counts i the probability for the adc to be greater than i will be 1 - mProb[abs(i)] and mProb[abs(i)] for the adc to be less than i;
  }

 }

void StSvtProbValues::WriteTable()
{
  fstream probability;

  probability.open("prob.dat",ios::out);
 
  for(int i = 0; i < 14; i++)
    {
     double k = mProb[i];
     probability << 1/k <<"\t";
     cout<<"mProb "<<"\("<<i<<") = "<<k<<endl;
    }

 probability.close();
  
}

main()
{
  StSvtProbValues probValue;

  probValue.Prob();
  probValue.WriteTable();

}
