#include <Stiostream.h>
#include <stdlib.h>
#include <math.h>

#include "StSvtTable.hh"

//ClassImp(StSvtTable)

StSvtTable::StSvtTable()
{
  /*
  for(int j = 0; j < 1000; j++)
    for(int k = 0; k < 2000; k++)
      mFreq[j][k] = 0.0;
  */
}

StSvtTable::~StSvtTable()
{

}

void StSvtTable::setFreq(int i)
{
 double num = 0.0, width = 0.0;

    for(int j = 0; j < 1000; j++)
      {
       num = j*0.001 + (double)i;
       for(int k = 0; k < 2000; k++)
         {
          width = k*0.001;
          if(!width)
           mFreq[j][k] = 0.0;
          else 
           mFreq[j][k] = prob1(num,width);
           if(i == 1 && j == 33 && k == 132)
             cout<<"mFreq"<<" ["<<j<<"]"<<" ["<<k<<"] = "<<mFreq[j][k]<<"\t"; 
              
	 }
      }
}

double StSvtTable::getFreq(int j, int k)
{
 double freq =  mFreq[j][k];
 return freq;
}


double StSvtTable::prob1(double num , double  sigma)
{
   num = num/(::sqrt(2)*sigma);

   double fraction = 0.5*(1 + erf(num));

  return fraction; 

}


double StSvtTable::prob2(double num , double  sigma)
{
   double mSum = 0, mErrf = 0;
   double mFactorial = 0;
   double mPowerTerm = 0;
   double mPowerTermSquared = 0;
   double mCountTerm = 0;

  if(fabs(num) < 5*sigma)
    {
     for(int j = 0; j<=120; j++)
      {
    
      if(j==0)
       {
        mFactorial = 1.0;
        mPowerTerm = fabs(num)/(::sqrt(2)*sigma);
        mPowerTermSquared = mPowerTerm*mPowerTerm;
        mCountTerm = mPowerTerm;
       }

     else 
       {
        mFactorial = (double)j*mFactorial;
        mPowerTerm = (mPowerTermSquared)*(double)fabs(mPowerTerm);

        if(j%2 == 1)
         mPowerTerm = (-1)*mPowerTerm;
      
         mCountTerm =(mPowerTerm /( mFactorial*(double)(2*j + 1)));

        }

       mSum += mCountTerm;
    
     }

     mErrf = (2.0/::sqrt(acos(-1.)))*mSum;

     if(num < 0.0)
       mErrf = (-1.0)*mErrf;
   }

  else if(num > 5*sigma)
     mErrf = 1.0;
  else 
     mErrf = -1.0;

  return  0.5*(1.0 + mErrf);
}

