/***************************************************************************
 *
 * $Id: StSvtElectronCloud.cc,v 1.3 2003/09/02 17:59:09 perev Exp $
 *
 * Author: Selemon Bekele
 ***************************************************************************
 *
 * Description: Calculatos cloud expansion in principal axis frame
 *
 ***************************************************************************
 *
 * $Log: StSvtElectronCloud.cc,v $
 * Revision 1.3  2003/09/02 17:59:09  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.2  2003/07/31 19:18:09  caines
 * Petrs improved simulation code
 *
 * Revision 1.1  2000/11/30 20:47:48  caines
 * First version of Slow Simulator - S. Bekele
 *
 **************************************************************************/

#include <Stiostream.h>
#include <string.h>
#include "Stiostream.h"

#include "StSvtElectronCloud.hh"

//ClassImp(StSvtElectronCloud)

fstream out1N, out2N, out3N;

StSvtElectronCloud::StSvtElectronCloud(char* option1, int option2,int option3)
{
 mTotCharge = 0;
 mChargeNow = 0;
 mSigma10 = 0;
 mSigma1 = 0;
 mSigma20 = 0;
 mSigma2 = 0;

 mSigmaSq1Prev = 0;
 mSigmaSq2Prev = 0;
 mSigmaSq1Now = 0;
 mSigmaSq2Now = 0; 

 mOption = option1;
 mWrite = option2;
 mFineDiv = option3;

 //cout<<"mOption = "<<mOption<<endl;
 //cout<<"mWrite = "<<mWrite<<endl;
 //cout<<"mFineDiv ="<<mFineDiv<<endl;

 if(mWrite)
  openFiles();

 for(int i = 0; i < 4; i++)
   {
     dSigma1SqBydt[i] = 0;
     dSigma2SqBydt[i] = 0;
   }
}

StSvtElectronCloud::~StSvtElectronCloud()
{

}

void StSvtElectronCloud::openFiles()
{

 if(mWrite){

   out1N.open("coulOnlyNew.dat",ios::out);
   out2N.open("diffOnlyNew.dat",ios::out);
   out3N.open("coulAndDiffNew.dat",ios::out);
 }

 //out3N<<"Writing to file"<<endl;
 }

void StSvtElectronCloud::setSiliconProp()
{
 mSDD_thickness = 0.3;                                 //  [mm]
 mDiffConst = 0.0035;                                 //  [mm**2/micro seconds]
 mSi_DielConst = 12.0;               
 mSi_EnergyGap = 3.6;                                  // [eV]
 // mPermitivity = (8.854187817/1.60217733)*10000;       // [e/(mm-V)]
 mPermitivity = 55263.46959983512;
 mSi_Mobility = 0.135;                                 // [mm**2/(V-micro seconds)]
 mLifeTime = 1000000.0;                                // [micro seconds]
}
 

void StSvtElectronCloud::setPar(double energy,double theta, double phi, double timBinSize)
 {
  mTimBinSize = timBinSize;
  mEnergy = energy;
  mTheta = theta;
  mPhi = phi;
  //mTheta = 2*acos(-1.0)/360;
  
  //mTotCharge = mEnergy/mSi_EnergyGap;
  mTotCharge = mEnergy*0.27777777777777;    // in number of electrons
  //cout<<"mTotCharge = "<<mTotCharge<<endl;
 
 }


void StSvtElectronCloud::setInitWidths(double w1, double w2)
{
 double sigma1 = 0,sigma2 = 0;

if(mTheta == 0.)
   mSigma10 = w1;  //  [mm]
 else
   mSigma10 = 0.288675134*fabs(mSDD_thickness*tan(mTheta));  //  [mm]

  mSigma20 = w2;                                         //  [mm]

 mSigmaSq1Prev = mSigma10*mSigma10;
 mSigmaSq2Prev = mSigma20*mSigma20;
 sigma1 = ::sqrt(mSigmaSq1Prev);
 sigma2 = ::sqrt(mSigmaSq2Prev);

 dSigma1SqBydt[0] = func1(0.0,sigma1,sigma2);
 dSigma2SqBydt[0] = func2(0.0,sigma1,sigma2); 

 for(int i = 1; i < 4; i++)
   {
     dSigma1SqBydt[i] = 0;
     dSigma2SqBydt[i] = 0;
   }

}

void StSvtElectronCloud::calculateWidthAtAnode(double mTc)
 {
  int timeBin, binDiv, numSteps, status;
  double steplen;

  if(mWrite){

    char temp[3][250];
    strcpy(temp[0],"diffusion");
    strcpy(temp[1],"coulomb");
    strcpy(temp[2],"both");

    for(int i = 0; i < 3; i++){

      mOption = temp[i];

      binDiv = 2000;
 
      steplen  = (1.0/binDiv)*mTimBinSize;     //in micro seconds
      timeBin = (int) mTc;
      numSteps = (int) (mTc*mTimBinSize/steplen);

      //cout<<"numSteps = "<<numSteps<<endl;

      mSigmaSq1Prev = mSigma10*mSigma10;
      mSigmaSq2Prev = mSigma20*mSigma20;

      dSigma1SqBydt[0] = func1(0.0,sqrt(mSigmaSq1Prev),sqrt(mSigmaSq2Prev));
      dSigma2SqBydt[0] = func2(0.0,sqrt(mSigmaSq1Prev),sqrt(mSigmaSq2Prev)); 

      for(int i = 1; i < 4; i++)
	{
	  dSigma1SqBydt[i] = 0;
	  dSigma2SqBydt[i] = 0;
	}
     
      for(int n = 1; n <= timeBin + 1; n++)
	{
	  if(n > timeBin) binDiv = numSteps - binDiv*timeBin;
	  if( n < 4)
	    status = runge_kutta4(n - 1, binDiv, steplen);
	  else 
	    status = adamsBushFort(n, binDiv, steplen);
	}
    }

  } else {

    binDiv = 15;

    steplen  = (1.0/binDiv)*mTimBinSize;     //in micro seconds
    timeBin = (int) mTc;
    numSteps = (int) (mTc*mTimBinSize/steplen);

    for(int n = 1; n <= timeBin + 1; n++)
      {
	if(n > timeBin) binDiv = numSteps - binDiv*timeBin;
	if( n < 4)
	  status = runge_kutta4(n - 1, binDiv, steplen);
	else 
	  status = adamsBushFort(n, binDiv, steplen);
      }

  }
 
  mWrite = 0;

  //cout<<"status = passed"<<endl;
}


int StSvtElectronCloud::runge_kutta4(int stepBefore, int numBinDiv, double steplen)
{

 double  m1 = 0, m2 = 0, m3 = 0, m4 = 0,tim = 0;
 double  n1 = 0, n2 = 0, n3 = 0, n4 = 0,  a = 1000.0;  // a is conversion factor to micro meters

 int stepNow = stepBefore + 1;

 double sigma1 = mSigmaSq1Prev;
 double sigma2 = mSigmaSq2Prev;

 if(!stepBefore)
   {
     //cout<<"stepBefore ="<<stepBefore<<endl;
     //cout<<"sigma1Squared = "<<sigma1<<"    "<<"sigma2Squared = "<<sigma2<<endl;
   }

  if(sigma1 < sigma2)
    sigma1 = sigma2;

  //out3N<<"Writing to file"<<endl;

  if(!stepBefore && mWrite)
   {
     if(!strncmp(mOption , "coulomb", strlen("coulomb")))
       out1N<<tim<<setw(20)<<::sqrt(mSigmaSq1Prev)*a<<setw(20)<<::sqrt(mSigmaSq2Prev)*a<<"\n";
     else if(!strncmp(mOption,"diffusion",strlen("diffusion")))
       out2N<<tim<<setw(20)<<sqrt(mSigmaSq1Prev)*a<<setw(20)<<::sqrt(mSigmaSq2Prev)*a<<"\n";
     else if(!strncmp(mOption,"both",strlen("both")))
       out3N<<tim<<setw(20)<<sqrt(mSigmaSq1Prev)*a<<setw(20)<<::sqrt(mSigmaSq2Prev)*a<<"\n";

     //cout<<"I got here 1st"<<endl;  
     //cout<<tim<<setw(20)<<sqrt(mSigmaSq1Prev)*a<<setw(20)<<::sqrt(mSigmaSq2Prev)*a<<endl;
   }

 tim = stepBefore*mTimBinSize;
 //cout<<"tim = "<<tim<<endl;

 if(stepBefore == 0 && mWrite){
   //cout<<"I got here 2nd"<<endl;
   //cout<<"tim = "<<tim<<endl;
   //cout<<"mTimBinSize =  "<<mTimBinSize<<endl;
   //cout<<"stepBefore = "<<stepBefore<<endl;
 }

 for(int m = 1; m <= numBinDiv; m++)
  {
   tim = tim  + steplen;
  
   //if(numBinDiv == m)
   // {
   //   cout<<"tim = "<<tim<<endl;
   //  cout<<"steplen = "<<steplen<<endl;
      //  }
  

   m1 = func1(tim,sigma1,sigma2);
   //cout<<m1<<endl;
   n1 = func2(tim,sigma1,sigma2);
   //cout<<n1<<endl;
   m2 = func1(tim + 0.5*steplen, sigma1 + 0.5*steplen*m1, sigma2 + 0.5*steplen*m1);
   //cout<<m2<<endl;
   n2 = func2(tim + 0.5*steplen, sigma1 + 0.5*steplen*n1, sigma2 + 0.5*steplen*n1);
   //cout<<n2<<endl;
   m3 = func1(tim + 0.5*steplen, sigma1 + 0.5*steplen*m2, sigma2 + 0.5*steplen*m2);
   //cout<<m3<<endl;
   n3 = func2(tim + 0.5*steplen, sigma1 + 0.5*steplen*n2, sigma2 + 0.5*steplen*n2);
   //cout<<n3<<endl;
   m4 = func1(tim + steplen, sigma1 + 0.5*steplen*m3, sigma2 + 0.5*steplen*m3);
   //cout<<m4<<endl;
   n4 = func2(tim + steplen, sigma1 + 0.5*steplen*n3, sigma2 + 0.5*steplen*n3);
   //cout<<n4<<endl;

   sigma1 = sigma1 + ((steplen/6)*(m1 + 2*(m2 + m3) + m4));
   sigma2 = sigma2 + ((steplen/6)*(n1 + 2*(n2 + n3) + n4));
  
   if(mFineDiv && mWrite)
     {
       if(!strncmp(mOption , "coulomb", strlen("coulomb"))){
           out1N<<tim<<setw(20)<<::sqrt(sigma1)*a<<setw(20)<<::sqrt(sigma2)*a<<"\n";
	   //cout<<tim<<setw(20)<<::sqrt(sigma1)*a<<setw(20)<<::sqrt(sigma2)*a<<endl;
       }
       else if(!strncmp(mOption,"diffusion",strlen("diffusion"))){
           out2N<<tim<<setw(20)<<::sqrt(sigma1)*a<<setw(20)<<::sqrt(sigma2)*a<<"\n";
	   //cout<<tim<<setw(20)<<::sqrt(sigma1)*a<<setw(20)<<::sqrt(sigma2)*a<<endl;
       }
       else if(!strncmp(mOption,"both",strlen("both"))){
           out3N<<tim<<setw(20)<<::sqrt(sigma1)*a<<setw(20)<<::sqrt(sigma2)*a<<"\n";
	   //cout<<tim<<setw(20)<<::sqrt(sigma1)*a<<setw(20)<<::sqrt(sigma2)*a<<endl;
       }
     }
  
  }

 mSigmaSq1Now = sigma1;
 mSigmaSq2Now = sigma2;

 mSigmaSq1Prev = mSigmaSq1Now;
 mSigmaSq2Prev = mSigmaSq2Now;
 
 if(stepNow < 4)
 {
  dSigma1SqBydt[stepNow] = m1;
  dSigma2SqBydt[stepNow] = n1;
 }
 
 mChargeNow = mTotCharge*exp(-tim/mLifeTime);

 if(!mFineDiv && mWrite)
    {
     
       if(!strncmp(mOption , "coulomb", strlen("coulomb")))
           out1N<<tim<<setw(20)<<::sqrt(mSigmaSq1Prev)*a<<setw(20)<<::sqrt(mSigmaSq2Prev)*a<<"\n";
        else if(!strncmp(mOption,"diffusion",strlen("diffusion")))
           out2N<<tim<<setw(20)<<::sqrt(mSigmaSq1Prev)*a<<setw(20)<<::sqrt(mSigmaSq2Prev)*a<<"\n";
        else if(!strncmp(mOption,"both",strlen("both")))
           out3N<<tim<<setw(20)<<::sqrt(mSigmaSq1Prev)*a<<setw(20)<<::sqrt(mSigmaSq2Prev)*a<<"\n";
       //cout<<tim<<setw(20)<<sqrt(mSigmaSq1Now)*a<<setw(20)<<sqrt(mSigmaSq2Now)*a<<endl;
     }

return 0;

}

int StSvtElectronCloud::adamsBushFort(int n, int  numBinDiv, double steplen)
{
 int stepNow = 0, a = 1000;
 double tim = 0, sigma1 = 0, sigma2 = 0, dSigmaSqBydt1 = 0, dSigmaSqBydt2 = 0;
 double sigma1SqPre = 0,sigma1SqCor = 0,sigma2SqPre = 0,sigma2SqCor = 0; 

 stepNow = n;
 tim = (stepNow - 1)*mTimBinSize + numBinDiv*steplen;

 //cout<<"time = "<<tim<<endl;

 // tim = tim  + steplen;
 if(stepNow >= 4)
  {
   sigma1SqPre = mSigmaSq1Prev + (numBinDiv*steplen/24)*(-9.0*dSigma1SqBydt[0] + 37.0*dSigma1SqBydt[1] - 59.0*dSigma1SqBydt[2] + 55.0*dSigma1SqBydt[3]);
   sigma2SqPre = mSigmaSq2Prev + (numBinDiv*steplen/24)*(-9.0*dSigma2SqBydt[0] + 37.0*dSigma2SqBydt[1] - 59.0*dSigma2SqBydt[2] + 55.0*dSigma2SqBydt[3]);

    sigma1 = ::sqrt(sigma1SqPre);
    sigma2 = ::sqrt(sigma2SqPre);
    

    dSigmaSqBydt1 = func1(tim,sigma1,sigma2);
    dSigmaSqBydt2 = func2(tim,sigma1,sigma2);

    sigma1SqCor = mSigmaSq1Prev + (numBinDiv*steplen/24)*(dSigma1SqBydt[1]-5.0*dSigma1SqBydt[2] + 19.0*dSigma1SqBydt[3] + 9.0*dSigmaSqBydt1);
    sigma2SqCor = mSigmaSq2Prev + (numBinDiv*steplen/24)*(dSigma2SqBydt[1]-5.0*dSigma2SqBydt[2] + 19.0*dSigma2SqBydt[3] + 9.0*dSigmaSqBydt2);

    
    for(int i = 0; i < 4; i++)
     {
      if(i < 3)
       {
        dSigma1SqBydt[i] = dSigma1SqBydt[i+1];
        dSigma2SqBydt[i] = dSigma2SqBydt[i+1];
       }
      else
       {
        dSigma1SqBydt[i] = dSigmaSqBydt1;
        dSigma2SqBydt[i] = dSigmaSqBydt2;
       }
     }

     if(mFineDiv && mWrite)
     {
      
      if(!strncmp(mOption , "coulomb", strlen("coulomb"))){
	out1N<<tim<<setw(20)<<::sqrt(sigma1SqCor)*a<<setw(20)<<::sqrt(sigma2SqCor)*a<<"\n";
	//cout<<tim<<setw(20)<<::sqrt(sigma1SqCor)*a<<setw(20)<<::sqrt(sigma2SqCor)*a<<endl;
	}
      else if(!strncmp(mOption,"diffusion",strlen("diffusion"))){
	out2N<<tim<<setw(20)<<::sqrt(sigma1SqCor)*a<<setw(20)<<::sqrt(sigma2SqCor)*a<<"\n";
	//cout<<tim<<setw(20)<<::sqrt(sigma1SqCor)*a<<setw(20)<<::sqrt(sigma2SqCor)*a<<endl;
	}
      else if(!strncmp(mOption,"both",strlen("both"))){
	out3N<<tim<<setw(20)<<::sqrt(sigma1SqCor)*a<<setw(20)<<::sqrt(sigma2SqCor)*a<<"\n";
	//cout<<tim<<setw(20)<<::sqrt(sigma1SqCor)*a<<setw(20)<<::sqrt(sigma2SqCor)*a<<endl;
      }
     }

   }
  

  mSigmaSq1Now = sigma1SqCor;
  mSigmaSq2Now = sigma2SqCor;

  mSigmaSq1Prev = mSigmaSq1Now;
  mSigmaSq2Prev = mSigmaSq2Now;

  mChargeNow = mTotCharge*exp(-tim/mLifeTime);


  if(!mFineDiv && mWrite)
    {
      if(!strncmp(mOption , "coulomb", strlen("coulomb")))
           out1N<<tim<<setw(20)<<::sqrt(mSigmaSq1Prev)*a<<setw(20)<<::sqrt(mSigmaSq2Prev)*a<<"\n";
        else if(!strncmp(mOption,"diffusion",strlen("diffusion")))
           out2N<<tim<<setw(20)<<::sqrt(mSigmaSq1Prev)*a<<setw(20)<<::sqrt(mSigmaSq2Prev)*a<<"\n";
        else if(!strncmp(mOption,"both",strlen("both")))
           out3N<<tim<<setw(20)<<::sqrt(mSigmaSq1Prev)*a<<setw(20)<<::sqrt(mSigmaSq2Prev)*a<<"\n";
        
      
      //cout<<tim<<setw(20)<<::sqrt(mSigmaSq1Now)*a<<setw(20)<<::sqrt(mSigmaSq2Now)*a<<endl;
     }

return 0;

}

double StSvtElectronCloud::func1(double tim, double sigmaSq1, double sigmaSq2)
{

  // cout<<"I am Here"<<endl;
 double sigma1 ,sigma2 ;
 double sr, sr2, sr3,sr4, sd, sd2, cons,cons3; 
 double fun1 ,multFactor, denominator,numerator;
 // double const1 = 0.43, const2 = 0.25, const3 = 0.58;

 sigma1 = ::sqrt(sigmaSq1);
 sigma2 = ::sqrt(sigmaSq2);

 sr = sigma2/sigma1;
 sr2 = sr*sr; sr3 = sr*sr2; sr4 = sr*sr3;
 sd = mSDD_thickness/sigma2; sd2 = (1 + sd*sd)*(1 + sd*sd);
 
 //cons = const1*sigma2/(const2*mSDD_thickness);
 cons = 5.73333333333333*sigma2;
 cons3 = cons*cons*cons;
 cons3 = 1.0 + ::sqrt(cons3);

 mChargeNow = mTotCharge*exp(-tim/mLifeTime);

 if(!strncmp(mOption , "coulomb", strlen("coulomb"))){
     //multFactor = (1.0/(4*acos(-1)))*(mSi_Mobility/(mSi_DielConst*mPermitivity));
   
    multFactor = 0.00000001619961;
    //mChargeNow = mTotCharge*exp(-tim/mLifeTime);

    //numerator = const1*::pow(sr2, 1.0/3) - (const3/4)*::log(sr4 + 1.0/sd2);
    numerator = 0.43*::pow(sr2, 1.0/3) - 0.145*::log(sr4 + 1.0/sd2);

    denominator = sigma1*::pow(cons3, 1.0/3);
     
   fun1 = multFactor*mChargeNow*(numerator/denominator);

  }

 else if(!strncmp(mOption,"diffusion",strlen("diffusion"))){
   fun1 = 2*mDiffConst;
 }

 else if(!strncmp(mOption,"both",strlen("both"))){
     //multFactor = (1.0/(4*acos(-1)))*(mSi_Mobility/(mSi_DielConst*mPermitivity));
   
    multFactor = 0.00000001619961;
    //mChargeNow = mTotCharge*exp(-tim/mLifeTime);

    //numerator = const1*::pow(sr2, 1.0/3) - (const3/4)*::log(sr4 + 1.0/sd2);
    numerator = 0.43*::pow(sr2, 1.0/3) - 0.145*::log(sr4 + 1.0/sd2);

    denominator = sigma1*::pow(cons3, 1.0/3);
     
   fun1 = 2*mDiffConst + multFactor*mChargeNow*(numerator/denominator);

  }

 return fun1;
}

double StSvtElectronCloud::func2(double tim, double sigmaSq1, double sigmaSq2)
{
 double sigma1 ,sigma2 ;
 double cons, cons3;
 double fun2, multFactor, denominator,numerator;
 //double const1 = 0.43, const2 = 0.25, const3 = 0.58;

 sigma1 = ::sqrt(sigmaSq1);
 sigma2 = ::sqrt(sigmaSq2);

 //cons = const1*sigma2/(const2*mSDD_thickness);
 cons = 5.73333333333333*sigma2;
 cons3 = cons*cons*cons;
 cons3 = 1.0 + ::sqrt(cons3);

 mChargeNow = mTotCharge*exp(-tim/mLifeTime);
 
 if(!strncmp(mOption , "coulomb", strlen("coulomb"))){
    //multFactor = (1.0/(4*acos(-1)))*(mSi_Mobility/(mSi_DielConst*mPermitivity));
   multFactor = 0.00000001619961;

   mChargeNow  = mTotCharge*exp(-tim/mLifeTime);

   //numerator = const3 - (const3 - const1)*::sqrt(sigma2/sigma1);
    numerator = 0.58 - 0.15*::sqrt(sigma2/sigma1);

    denominator = sigma1*::pow(cons3, 1.0/3);
 
   fun2 = multFactor*mChargeNow*(numerator/denominator);

  }

else if(!strncmp(mOption,"diffusion",strlen("diffusion"))){
  fun2 = 2*mDiffConst;
  }
else if(!strncmp(mOption,"both",strlen("both"))){
    //multFactor = (1.0/(4*acos(-1)))*(mSi_Mobility/(mSi_DielConst*mPermitivity));
   multFactor = 0.00000001619961;

   mChargeNow  = mTotCharge*exp(-tim/mLifeTime);

   //numerator = const3 - (const3 - const1)*::sqrt(sigma2/sigma1);
    numerator = 0.58 - 0.15*::sqrt(sigma2/sigma1);

    denominator = sigma1*::pow(cons3, 1.0/3);
 
   fun2 = 2*mDiffConst + multFactor*mChargeNow*(numerator/denominator);

  }


return fun2;
}


double StSvtElectronCloud::getSigma1()
{
 double sigma1, sigma2,  sigmaSq1, sigmaSq2, sn, cs; 

 sigma1 = ::sqrt(mSigmaSq1Now);    // in mm
 sigma2 =  ::sqrt(mSigmaSq2Now);   // in mm
 sigmaSq1 = sigma1*sigma1;
 sigmaSq2 = sigma2*sigma2;

 sn = sin(mPhi); cs = cos(mPhi);

 return ::sqrt(sn*sn*sigmaSq1 + cs*cs*sigmaSq2);
}

double StSvtElectronCloud::getSigma2()
{
 double sigma1, sigma2,  sigmaSq1, sigmaSq2, sn, cs; 

 sigma1 = ::sqrt(mSigmaSq1Now);    // in mm
 sigma2 =  ::sqrt(mSigmaSq2Now);   // in mm
 sigmaSq1 = sigma1*sigma1;
 sigmaSq2 = sigma2*sigma2;

 sn = sin(mPhi); cs = cos(mPhi);

 return ::sqrt(cs*cs*sigmaSq1 + sn*sn*sigmaSq2);

}
double StSvtElectronCloud::getPhi()
{
 return mPhi;
}

double StSvtElectronCloud::getSigma1Sq()
{
  return mSigmaSq1Now;    //in mm squared
}


double StSvtElectronCloud::getSigma2Sq()
{
 return mSigmaSq2Now;     //in mm squared
}

double StSvtElectronCloud::getChargeAtAnode()
{
  //cout<<" mChargeNow = "<<mChargeNow<<endl;
return mChargeNow;
 
}


void StSvtElectronCloud::closeFiles()
{
if(mWrite){
 out1N.close();
 out2N.close();
 out3N.close();
 }
}
