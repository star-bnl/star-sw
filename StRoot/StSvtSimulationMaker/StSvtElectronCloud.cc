/***************************************************************************
 *
 * $Id: StSvtElectronCloud.cc,v 1.5 2003/11/15 20:24:29 caines Exp $
 *
 * Author: Selemon Bekele
 ***************************************************************************
 *
 * Description: Calculatos cloud expansion in principal axis frame
 *
 ***************************************************************************
 *
 * $Log: StSvtElectronCloud.cc,v $
 * Revision 1.5  2003/11/15 20:24:29  caines
 * fixes to remove warnings at compilation
 *
 * Revision 1.4  2003/11/13 16:24:59  caines
 * Further improvements to get simulator looking like reality
 *
 * Revision 1.1  2000/11/30 20:47:48  caines
 * First version of Slow Simulator - S. Bekele
 *
 **************************************************************************/
#include <string.h>
#include "StSvtElectronCloud.hh"

//ClassImp(StSvtElectronCloud)


fstream out1N, out2N, out3N;

StSvtElectronCloud::StSvtElectronCloud(char* option1, int option2,int option3)
{
  mLifeTime = 1000000.0;                                // [micro seconds]
  mTrapConst = 0;
  mDiffusionConst=0;

  mTotCharge = 0;
  mChargeNow = 0;
  mSigma10 = 0;
  mSigma1 = 0;
  mSigma20 = 0;
  mSigma2 = 0;
   
  mSigmaXSqPrev = 0;
  mSigmaYSqPrev = 0;
  mSigmaXSqNow = 0;
  mSigmaYSqNow = 0; 
  
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
      dSigmaXSqBydt[i] = 0;
      dSigmaYSqBydt[i] = 0;
      dSigmaXYSqBydt[i] = 0;
    }
}

StSvtElectronCloud::~StSvtElectronCloud()
{

}

void StSvtElectronCloud::openFiles()
{

 if(mWrite){

   out1N.open("coulOnly.dat",ios::out);
   out2N.open("diffOnly.dat",ios::out);
   out3N.open("coulAndDiff.dat",ios::out);
 }

 //out3N<<"Writing to file"<<endl;
 }


void StSvtElectronCloud::setSiliconProp()
{
 mSDD_thickness = 0.3;                                 //  [mm]
 mTrapConst = 0.0;                                     //  [micro seconds]
 mDiffusionConst=0.0035;                               //  [mm**2/micro seconds]
 CalculateDiffXY();

 mSi_DielConst = 12.0;               
 mSi_EnergyGap = 3.6;                                  // [eV]
 // mPermitivity = (8.854187817/1.60217733)*10000;       // [e/(mm-V)]
 mPermitivity = 55263.46959983512;
 mSi_Mobility = 0.135;                                 // [mm**2/(V-micro seconds)]
 
}


 
void StSvtElectronCloud::setElectronLifeTime(double tLife)
{
  mLifeTime = tLife;
}





void StSvtElectronCloud::CalculateDiffXY()   
{// recalculate "diffusion" in X and Y direction after the parameters has changed
  mDiffConstX = mDiffusionConst;
  mDiffConstY = mDiffusionConst;
  if(mTrapConst)
    mDiffConstX +=mTrapConst*pow(mDriftVel,2);     //  [mm**2/micro seconds]
}

void StSvtElectronCloud::setDiffusionConst(double diffConst)
{
  mDiffusionConst=diffConst;
  CalculateDiffXY();
}

void StSvtElectronCloud::setDriftVelocity(double driftVel)
{
  mDriftVel = driftVel; //  [mm/micro seconds]
  CalculateDiffXY();
}

void StSvtElectronCloud::setTrappingConst(double trapConst)
{

  mTrapConst = trapConst;   //  [micro seconds]
  CalculateDiffXY();
 
}

void StSvtElectronCloud::setPar(double energy,double theta, double phi, double timBinSize)
{
  setSiliconProp();
  
  mTimBinSize = timBinSize;
  mEnergy = energy;
  mTheta = theta;
  mInitPhi = phi;
  mPhi = mInitPhi;
  //mTheta = 2*acos(-1.0)/360;
  
  //mTotCharge = mEnergy/mSi_EnergyGap;
  mTotCharge = mEnergy*0.27777777777777;    // in number of electrons, ~25000 electrons for MIPs
  //cout<<"mTotCharge = "<<mTotCharge<<endl;
 
}

void StSvtElectronCloud::setInitWidths(double w1, double w2)
{
 double sigma1Sq = 0,sigma2Sq = 0;
 double sigmaXSq = 0,sigmaYSq = 0,sigmaXYSq = 0;

 mSigmaXSqPrev = 0;
 mSigmaYSqPrev = 0;
 mSigmaXSqNow = 0;
 mSigmaYSqNow = 0; 

  
if(mTheta == 0.){
   mSigma10 = w1;  //  [mm]
   mSigma20 = w2;
   mPhi = 0;
 
 }else{
   mSigma10 = 0.288675134*fabs(mSDD_thickness*tan(mTheta));  //  [mm] 
   mSigma20 = w2;                                         //  [mm]
  
 }


 if(mSigma10 > mSigma20){
   sigma1Sq = mSigma10*mSigma10;
   sigma2Sq = mSigma20*mSigma20;
 } else {
   sigma1Sq = mSigma20*mSigma20;
   sigma2Sq = mSigma10*mSigma10;
 }
  
 mPhi = mInitPhi;
 
 if(mTheta == 0){
   mPhi = 0;
   sigmaXSq = fabs(sigma1Sq)*pow(cos(mPhi),2) + fabs(sigma2Sq)*pow(sin(mPhi),2);
   sigmaYSq = fabs(sigma1Sq)*pow(sin(mPhi),2) + fabs(sigma2Sq)*pow(cos(mPhi),2);
   sigmaXYSq = 0.;
   /* 
   cout<<"initial mPhi = "<<mPhi<<endl;
   cout<<"initial width along drift= "<<sqrt(sigmaXSq)<<endl;
   cout<<"initial width along anode = "<<sqrt(sigmaYSq)<<endl;
   //cout<<"initial XY = "<<sqrt(sigmaXYSq)<<endl;
   */
 } else {
   if((1. - sqrt(fabs(sigma2Sq)/fabs(sigma1Sq))) > 0.001){
     
     sigmaXSq = fabs(sigma1Sq)*pow(cos(mPhi),2) + fabs(sigma2Sq)*pow(sin(mPhi),2);
     sigmaYSq = fabs(sigma1Sq)*pow(sin(mPhi),2) + fabs(sigma2Sq)*pow(cos(mPhi),2);
     sigmaXYSq = 0.5*(fabs(sigmaXSq) - fabs(sigmaYSq))*tan(2.*mPhi);
     
     if(mTheta != 0 && (fabs(sigmaXSq) == fabs(sigmaYSq))){
       mPhi = mInitPhi;
       sigmaXYSq = 0;
     } 
     /*
     cout<<"initial mPhi = "<<mPhi<<endl;
     cout<<"initial width along drift= "<<sqrt(fabs(sigmaXSq))<<endl;
     cout<<"initial width along anode = "<<sqrt(fabs(sigmaYSq))<<endl;
     cout<<"initial XY = "<<sqrt(fabs(sigmaXYSq))<<endl;
     */
   }
 }
 
 mSigmaXSqPrev = fabs(sigmaXSq);
 mSigmaYSqPrev = fabs(sigmaYSq);
 mSigmaXYSqPrev = fabs(sigmaXYSq);
 
 /*
 cout<<"mSigmaXSqPrev = "<<mSigmaXSqPrev<<endl;
 cout<<"mSigmaYSqPrev = "<<mSigmaYSqPrev<<endl;
 cout<<"mSigmaXYSqPrev = "<<mSigmaXYSqPrev<<endl;
 */
 dSigmaXSqBydt[0] = sigmaXSqFunc(0.0,fabs(sigma1Sq),fabs(sigma2Sq),mPhi);
 dSigmaYSqBydt[0] = sigmaYSqFunc(0.0,fabs(sigma1Sq),fabs(sigma2Sq),mPhi);
 dSigmaXYSqBydt[0] = sigmaXYSqFunc(0.0,fabs(sigma1Sq),fabs(sigma2Sq),mPhi);
 
 for(int i = 1; i < 4; i++)
   {
     dSigmaXSqBydt[i] = 0;
     dSigmaYSqBydt[i] = 0;
     dSigmaXYSqBydt[i] = 0;
   }
}

void StSvtElectronCloud::calculateWidthAtAnode(double mTc)
 {
  int timeBin, binDiv, numSteps, status;
  double steplen,sigmaXSq,sigmaYSq,sigmaXYSq,sigma1Sq,sigma2Sq;
  

  if(mWrite){

    //cout<<"Long initial width = "<<mSigma10<<endl;
    //cout<<"Short initial width = "<<mSigma20<<endl;

    char temp[3][250];
    strcpy(temp[0],"diffusion");
    strcpy(temp[1],"coulomb");
    strcpy(temp[2],"both");

    for(int i = 0; i < 3; i++){

      mOption = temp[i];

      cout<<"mOPtion = "<<mOption<<endl;

      binDiv = 2000;
 
      steplen  = (1.0/binDiv)*mTimBinSize;     //in micro seconds
      timeBin = (int) mTc;
      numSteps = (int) (mTc*mTimBinSize/steplen);

      //cout<<"numSteps = "<<numSteps<<endl;

      sigma1Sq = mSigma10*mSigma10;
      sigma2Sq = mSigma20*mSigma20;

      if(mSigma10 > mSigma20){
	sigma1Sq = mSigma10*mSigma10;
	sigma2Sq = mSigma20*mSigma20;
      } else if(mSigma10 < mSigma20){
	sigma1Sq = mSigma20*mSigma20;
	sigma2Sq = mSigma10*mSigma10;
      }

      mPhi = mInitPhi;
      sigmaXSq=0;
      sigmaYSq=0;
      sigmaXYSq=0;

      if(mTheta == 0){
	mPhi = 0;
	sigmaXSq = fabs(sigma1Sq)*pow(cos(mPhi),2) + fabs(sigma2Sq)*pow(sin(mPhi),2);
	sigmaYSq = fabs(sigma1Sq)*pow(sin(mPhi),2) + fabs(sigma2Sq)*pow(cos(mPhi),2);
	sigmaXYSq = 0.;
        /*
	cout<<"mPhi = "<<mPhi<<endl;
	cout<<" width along drift= "<<sqrt(sigmaXSq)<<endl;
	cout<<" width along anode = "<<sqrt(sigmaYSq)<<endl;
	cout<<"initial XY = "<<sqrt(sigmaXYSq)<<endl;
        */
      } else {
	if((1. - sqrt(fabs(sigma2Sq)/fabs(sigma1Sq))) > 0.001){
	
	  sigmaXSq = fabs(sigma1Sq)*pow(cos(mPhi),2) + fabs(sigma2Sq)*pow(sin(mPhi),2);
	  sigmaYSq = fabs(sigma1Sq)*pow(sin(mPhi),2) + fabs(sigma2Sq)*pow(cos(mPhi),2);
	  sigmaXYSq = 0.5*(fabs(sigmaXSq) - fabs(sigmaYSq))*tan(2.*mPhi);
	 	  
	  if(mTheta != 0 && (fabs(sigmaXSq) == fabs(sigmaYSq))){
	    mPhi = mInitPhi;
	    sigmaXYSq = 0;
	  } 
	  /*
	  cout<<"mPhi = "<<mPhi<<endl;
	  cout<<"initial width along drift= "<<sqrt(fabs(sigmaXSq))<<endl;
	  cout<<"initial width along anode = "<<sqrt(fabs(sigmaYSq))<<endl;
	  cout<<"initial XY = "<<sqrt(fabs(sigmaXYSq))<<endl;
	  */
	}
      }
      
      mSigmaXSqPrev = fabs(sigmaXSq);
      mSigmaYSqPrev = fabs(sigmaYSq);
      mSigmaXYSqPrev = fabs(sigmaXYSq);
      
      dSigmaXSqBydt[0] = sigmaXSqFunc(0.0,fabs(sigma1Sq),fabs(sigma2Sq),mPhi);
      dSigmaYSqBydt[0] = sigmaYSqFunc(0.0,fabs(sigma1Sq),fabs(sigma2Sq),mPhi);
      dSigmaXYSqBydt[0] = sigmaXYSqFunc(0.0,fabs(sigma1Sq),fabs(sigma2Sq),mPhi);
      
      for(int i = 1; i < 4; i++)
	{
	  dSigmaXSqBydt[i] = 0;
	  dSigmaYSqBydt[i] = 0;
	  dSigmaXYSqBydt[i] = 0;
	}

      for(int n = 1; n <= timeBin + 1; n++)
	{
	  //cout<<"timeBin = "<<timeBin<<endl;
	  if(n > timeBin) binDiv = numSteps - binDiv*timeBin;
	  if( n < 4){
	    status = runge_kutta4(n - 1, binDiv, steplen);
	  }else{ 
	    status = adamsBushFort(n, binDiv, steplen);
	  }
	}
    }

  } else {
    timeBin = (int)mTc;
    
    //cout<<"mTc = "<<(int)mTc<<endl;
    
   //  binDiv = 2000;
//     steplen  = (1.0/binDiv)*mTimBinSize;     //in micro seconds
//     numSteps = (int)(mTc*mTimBinSize/steplen);
    
    for(int n = 1; n <= timeBin + 1; n++)
      {
 	if(n < 4){
 	  binDiv = 2000;
	  steplen  = (1.0/binDiv)*mTimBinSize;     //in micro seconds
 	  numSteps = (int)(mTc*mTimBinSize/steplen);
	  if(n > timeBin) binDiv = numSteps - binDiv*timeBin;
 	}else{
 	  binDiv = 15;
 	  steplen  = (1.0/binDiv)*mTimBinSize;     //in micro seconds
 	  numSteps = (int)(mTc*mTimBinSize/steplen);
	  if(n > timeBin) binDiv = numSteps - binDiv*timeBin;
 	}
	
	
	if( n < 4)
	  status = runge_kutta4(n - 1, binDiv, steplen);
	else 
	  status = adamsBushFort(n, binDiv, steplen);
      }
    
  }
 
  mWrite = 0;         //write to files just once

 //cout<<"status = passed"<<endl;
}


int StSvtElectronCloud::runge_kutta4(int stepBefore, int numBinDiv, double steplen)
{

  double  m1 = 0.0, m2 = 0.0, m3 = 0.0, m4 = 0.0;
  double  n1 = 0.0, n2 = 0.0, n3 = 0.0, n4 = 0.0; 
  double  o1 = 0.0, o2 = 0.0, o3 = 0.0, o4 = 0.0; 
  double tim = 0.0, a = 1000.0;  // a is conversion factor to micro meters
  double phi = 0.0,sigma1Sq = 0.0,sigma2Sq = 0.0;

  int stepNow = stepBefore + 1;

  //sigmaXSq = mSigmaXSqPrev;
  //sigmaYSq = mSigmaYSqPrev;
  //sigmaXYSq = mSigmaXYSqPrev;
  
  sigma1Sq = 0.5*((mSigmaXSqPrev + mSigmaYSqPrev) + sqrt(pow((mSigmaXSqPrev - mSigmaYSqPrev),2) + 4.*pow(mSigmaXYSqPrev,2)));
  sigma2Sq = 0.5*((mSigmaXSqPrev + mSigmaYSqPrev) - sqrt(pow((mSigmaXSqPrev - mSigmaYSqPrev),2) + 4.*pow(mSigmaXYSqPrev,2)));
  phi = 0.5*atan(2.*fabs(mSigmaXYSqPrev)/(fabs(mSigmaXSqPrev) - fabs(mSigmaYSqPrev)));

  if(!stepBefore)
   {
     //cout<<"stepBefore ="<<stepBefore<<endl;
     //cout<<"sigmaXSquared = "<<mSigmaXSqPrev<<"    "<<"sigmaYSquared = "<<mSigmaYSqPrev<<endl;
   }

  //if(fabs(sigma1Sq) < fabs(sigma2Sq))
  //sigma1Sq = fabs(sigma2Sq);

  //out3N<<"Writing to file"<<endl;

  if(!stepBefore && mWrite)
   {
     if(!strncmp(mOption , "coulomb", strlen("coulomb")))
       out1N<<tim<<setw(20)<<sqrt(mSigmaXSqPrev)*a<<setw(20)<<sqrt(mSigmaYSqPrev)*a<<"\n";
     else if(!strncmp(mOption,"diffusion",strlen("diffusion")))
       out2N<<tim<<setw(20)<<sqrt(mSigmaXSqPrev)*a<<setw(20)<<sqrt(mSigmaYSqPrev)*a<<"\n";
     else if(!strncmp(mOption,"both",strlen("both")))
       out3N<<tim<<setw(20)<<sqrt(mSigmaXSqPrev)*a<<setw(20)<<sqrt(mSigmaYSqPrev)*a<<"\n";

     //cout<<"I got here 1st"<<endl;  
     //cout<<tim<<setw(20)<<sqrt(mSigmaXSqPrev)*a<<setw(20)<<sqrt(mSigmaYSqPrev)*a<<endl;
   }

 tim = stepBefore*mTimBinSize;
 /*
 cout<<"\n############## tim = "<<tim<<" #############"<<endl;
 cout<<"step length = "<<steplen<<endl;
 cout<<"theta = "<<mTheta<<"   phi = "<<phi<<endl;
 cout<<"sigma1Sq = "<<sigma1Sq<<"  sigma2Sq = "<<sigma2Sq<<endl;
 cout<<" width along drift= "<<sqrt(fabs(mSigmaXSqPrev))<<"  width along anode = "<<sqrt(fabs(mSigmaYSqPrev))<<endl;
 cout<<"initial XY = "<<sqrt(fabs(mSigmaXYSqPrev))<<endl;
 */

 
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
   //cout<<"tim = "<<tim<<endl;
   //  cout<<"steplen = "<<steplen<<endl;
      //  }
  
   sigma1Sq = 0.5*((mSigmaXSqPrev + mSigmaYSqPrev) + sqrt(pow((mSigmaXSqPrev - mSigmaYSqPrev),2) + 4.*pow(mSigmaXYSqPrev,2)));
   sigma2Sq = 0.5*((mSigmaXSqPrev + mSigmaYSqPrev) - sqrt(pow((mSigmaXSqPrev - mSigmaYSqPrev),2) + 4.*pow(mSigmaXYSqPrev,2)));

   /*
   if(m < 10){
     cout<<"tim = "<<tim<<"   sigma1Sq = "<<sigma1Sq<<"   sigma2Sq = "<<sigma2Sq<<endl;
   }
   */
   m1 = sigmaXSqFunc(tim,fabs(sigma1Sq),fabs(sigma2Sq),phi);
   n1 = sigmaYSqFunc(tim,fabs(sigma1Sq),fabs(sigma2Sq),phi);
   o1 = sigmaXYSqFunc(tim,fabs(sigma1Sq),fabs(sigma2Sq),phi);

   m2 = sigmaXSqFunc(tim + 0.5*steplen, fabs(sigma1Sq) + 0.5*steplen*m1, fabs(sigma2Sq) + 0.5*steplen*m1,phi);
   n2 = sigmaYSqFunc(tim + 0.5*steplen, fabs(sigma1Sq) + 0.5*steplen*n1, fabs(sigma2Sq) + 0.5*steplen*n1,phi);
   o2 = sigmaXYSqFunc(tim + 0.5*steplen, fabs(sigma1Sq) + 0.5*steplen*o1, fabs(sigma2Sq) + 0.5*steplen*o1,phi);

   m3 = sigmaXSqFunc(tim + 0.5*steplen, fabs(sigma1Sq) + 0.5*steplen*m2, fabs(sigma2Sq) + 0.5*steplen*m2,phi);
   n3 = sigmaYSqFunc(tim + 0.5*steplen, fabs(sigma1Sq) + 0.5*steplen*n2, fabs(sigma2Sq) + 0.5*steplen*n2,phi);
   o3 = sigmaXYSqFunc(tim + 0.5*steplen, fabs(sigma1Sq) + 0.5*steplen*o2, fabs(sigma2Sq) + 0.5*steplen*o2,phi);

   m4 = sigmaXSqFunc(tim + steplen, fabs(sigma1Sq) + 0.5*steplen*m3, fabs(sigma2Sq) + 0.5*steplen*m3,phi);
   n4 = sigmaYSqFunc(tim + steplen, fabs(sigma1Sq) + 0.5*steplen*n3, fabs(sigma2Sq) + 0.5*steplen*n3,phi);
   o4 = sigmaXYSqFunc(tim + steplen, fabs(sigma1Sq) + 0.5*steplen*o3, fabs(sigma2Sq) + 0.5*steplen*o3,phi);


   mSigmaXSqPrev = fabs(mSigmaXSqPrev + ((steplen/6)*(m1 + 2*(m2 + m3) + m4)));
   mSigmaYSqPrev = fabs(mSigmaYSqPrev + ((steplen/6)*(n1 + 2*(n2 + n3) + n4)));
   mSigmaXYSqPrev = fabs(mSigmaXYSqPrev + ((steplen/6)*(o1 + 2*(o2 + o3) + o4)));

   if(mTheta == 0){
     phi = 0;
     //mSigmaXSqPrev = fabs(sigma1Sq)*pow(cos(phi),2) + fabs(sigma2Sq)*pow(sin(phi),2);
     //mSigmaYSqPrev = fabs(sigma1Sq)*pow(sin(phi),2) + fabs(sigma2Sq)*pow(cos(phi),2);
     mSigmaXYSqPrev = 0.;

   } else {
     if((1. - sqrt(fabs(sigma2Sq)/fabs(sigma1Sq))) > 0.001){

       if((fabs(mSigmaXSqPrev) - fabs(mSigmaYSqPrev))){
	 phi = 0.5*atan(2.*fabs(mSigmaXYSqPrev)/(fabs(mSigmaXSqPrev) - fabs(mSigmaYSqPrev)));
       }else if(mTheta != 0 && (fabs(mSigmaXSqPrev) == fabs(mSigmaYSqPrev))){
	 phi = mInitPhi;
	 mSigmaXYSqPrev = 0;
       } 

     
     } else {
     
       phi = 0;
       mSigmaXSqPrev = fabs(sigma1Sq)*pow(cos(phi),2) + fabs(sigma2Sq)*pow(sin(phi),2);
       mSigmaYSqPrev = fabs(sigma1Sq)*pow(sin(phi),2) + fabs(sigma2Sq)*pow(cos(phi),2);
       mSigmaXYSqPrev = 0.;
     
     }

     // if(m < 2){
//        cout<<"\n******* Beginning integration ******* "<<endl;
//        cout<<"tim = "<<tim<<endl;
//        cout<<"m1 = "<<m1<<" n1 = "<<n1<<" o1 = "<<o1<<endl;
//        cout<<"m2 = "<<m2<<" n2 = "<<n2<<" o2 = "<<o2<<endl;
//        cout<<"m3 = "<<m3<<" n3 = "<<n3<<" o3 = "<<o3<<endl;
//        cout<<"m4 = "<<m4<<" n4 = "<<n4<<" o4 = "<<o4<<endl;
//        cout<<"mChargeNow = "<<mChargeNow<<endl;
//        cout<<"mLifeTime = "<<mLifeTime<<endl;
//        cout<<"\nnew phi = "<<phi<<" new sigma1Sq = "<<sigma1Sq<<"     new sigma2Sq = "<<sigma2Sq<<endl;
//        cout<<" new width along drift= "<<sqrt(fabs(mSigmaXSqPrev))<<" new width along anode = "<<sqrt(fabs(mSigmaYSqPrev))<<endl;
//        cout<<"new XY = "<<sqrt(fabs(mSigmaXYSqPrev))<<endl;
//      }
       //cout<<"phi = "<<phi<<endl;
       //cout<<" width along drift = "<<sqrt(fabs(mSigmaXSqPrev))<<endl;
       //cout<<" width along anode = "<<sqrt(fabs(mSigmaYSqPrev))<<endl;
     
   }
  
   if(mFineDiv && mWrite)
     {
       if(!strncmp(mOption , "coulomb", strlen("coulomb"))){
           out1N<<tim<<setw(20)<<sqrt(mSigmaXSqPrev)*a<<setw(20)<<sqrt(mSigmaYSqPrev)*a<<"\n";
	   //cout<<tim<<setw(20)<<sqrt(mSigmaXSqPrev)*a<<setw(20)<<sqrt(mSigmaYSqPrev)*a<<endl;
       }
       else if(!strncmp(mOption,"diffusion",strlen("diffusion"))){
           out2N<<tim<<setw(20)<<sqrt(mSigmaXSqPrev)*a<<setw(20)<<sqrt(mSigmaYSqPrev)*a<<"\n";
	   //cout<<tim<<setw(20)<<sqrt(mSigmaXSqPrev)*a<<setw(20)<<sqrt(mSigmaYSqPrev)*a<<endl;
       }
       else if(!strncmp(mOption,"both",strlen("both"))){
           out3N<<tim<<setw(20)<<sqrt(mSigmaXSqPrev)*a<<setw(20)<<sqrt(mSigmaYSqPrev)*a<<"\n";
	   //cout<<tim<<setw(20)<<sqrt(mSigmaXSqPrev)*a<<setw(20)<<sqrt(mSigmaYSqPrev)*a<<endl;
       }
     }
  }

 mPhi = phi;
      
 mSigmaXSqNow = fabs(mSigmaXSqPrev);
 mSigmaYSqNow = fabs(mSigmaYSqPrev);
 mSigmaXYSqNow = fabs(mSigmaXYSqPrev);

 /*
 cout<<"\nmSigmaXSqNow = "<<mSigmaXSqNow<<endl;
 cout<<"mSigmaYSqNow = "<<mSigmaYSqNow<<endl;
 cout<<"mSigmaXYSqNow = "<<mSigmaXYSqNow<<endl;
 cout<<"\n###########################"<<endl;
 */
 mSigmaXSqPrev = mSigmaXSqNow;
 mSigmaYSqPrev = mSigmaYSqNow;
 mSigmaXYSqPrev = mSigmaXYSqNow;
 
 if(stepNow < 4)
 {
  dSigmaXSqBydt[stepNow] = m1;
  dSigmaYSqBydt[stepNow] = n1;
  dSigmaXYSqBydt[stepNow] = o1;
 }
 
 //mChargeNow = mTotCharge*exp(-tim/mLifeTime);

 if(!mFineDiv && mWrite)
    {
     
       if(!strncmp(mOption , "coulomb", strlen("coulomb")))
           out1N<<tim<<setw(20)<<sqrt(mSigmaXSqPrev)*a<<setw(20)<<sqrt(mSigmaYSqPrev)*a<<"\n";
        else if(!strncmp(mOption,"diffusion",strlen("diffusion")))
           out2N<<tim<<setw(20)<<sqrt(mSigmaXSqPrev)*a<<setw(20)<<sqrt(mSigmaYSqPrev)*a<<"\n";
        else if(!strncmp(mOption,"both",strlen("both")))
           out3N<<tim<<setw(20)<<sqrt(mSigmaXSqPrev)*a<<setw(20)<<sqrt(mSigmaYSqPrev)*a<<"\n";
       //cout<<tim<<setw(20)<<sqrt(mSigmaXSqNow)*a<<setw(20)<<sqrt(mSigmaYSqNow)*a<<endl;
     }

return 0;

}

int StSvtElectronCloud::adamsBushFort(int n, int  numBinDiv, double steplen)
{
 int stepNow = 0;
 double  a = 1000, phi = 0.;
 double tim = 0, sigma1Sq = 0, sigma2Sq = 0;
 double dSigmaXSqBydt1 = 0, dSigmaYSqBydt1 = 0,dSigmaXYSqBydt1 = 0;
 double sigmaXSqPre = 0,sigmaXSqCor = 0,sigmaYSqPre = 0,sigmaYSqCor = 0, sigmaXYSqPre = 0,sigmaXYSqCor = 0; 

 stepNow = n;
 tim = (stepNow - 1)*mTimBinSize + numBinDiv*steplen;

 //cout<<"time = "<<tim<<endl;

 // tim = tim  + steplen;
 if(stepNow >= 4)
  {
   sigmaXSqPre = mSigmaXSqPrev + (numBinDiv*steplen/24)*(-9.0*dSigmaXSqBydt[0] + 37.0*dSigmaXSqBydt[1] - 59.0*dSigmaXSqBydt[2] + 55.0*dSigmaXSqBydt[3]);
   sigmaYSqPre = mSigmaYSqPrev + (numBinDiv*steplen/24)*(-9.0*dSigmaYSqBydt[0] + 37.0*dSigmaYSqBydt[1] - 59.0*dSigmaYSqBydt[2] + 55.0*dSigmaYSqBydt[3]);
   sigmaXYSqPre = mSigmaXYSqPrev + (numBinDiv*steplen/24)*(-9.0*dSigmaXYSqBydt[0] + 37.0*dSigmaXYSqBydt[1] - 59.0*dSigmaXYSqBydt[2] + 55.0*dSigmaXYSqBydt[3]);

   sigma1Sq = 0.5*((sigmaXSqPre + sigmaYSqPre) + sqrt(pow((sigmaXSqPre - sigmaYSqPre),2) + 4.*pow(sigmaXYSqPre,2)));
   sigma2Sq = 0.5*((sigmaXSqPre + sigmaYSqPre) - sqrt(pow((sigmaXSqPre - sigmaYSqPre),2) + 4.*pow(sigmaXYSqPre,2)));
   


   if(mTheta == 0){
     phi = 0;
     sigmaXSqPre = fabs(sigma1Sq)*pow(cos(phi),2) + fabs(sigma2Sq)*pow(sin(phi),2);
     sigmaYSqPre = fabs(sigma1Sq)*pow(sin(phi),2) + fabs(sigma2Sq)*pow(cos(phi),2);
     sigmaXYSqPre = 0.;

     /*
     cout<<"Tim = "<<tim<<"theta = "<<mTheta<<"  phi = "<<phi<<endl;
     cout<<" width along drift= "<<sqrt(sigmaXSqPre)<<endl;
     cout<<" width along anode = "<<sqrt(sigmaYSqPre)<<endl;
     // cout<<"initial XY = "<<sqrt(sigmaXYSqPre)<<endl;
     */
     
   } else {
     if((1. - sqrt(fabs(sigma2Sq)/fabs(sigma1Sq))) > 0.001){
       
       if((fabs(sigmaXSqPre) - fabs(sigmaYSqPre))){
	 phi = 0.5*atan(2.*fabs(sigmaXYSqPre)/(fabs(sigmaXSqPre) - fabs(sigmaYSqPre)));
       }else if(mTheta != 0 && (fabs(sigmaXSqPre) == fabs(sigmaYSqPre))){
	 phi = mInitPhi;
	 sigmaXYSqPre = 0;
       } 
       //cout<<"Tim = "<<tim<<"theta = "<<mTheta<<"   phi = "<<phi<<endl;
       //cout<<" width along drift= "<<sqrt(fabs(sigmaXSqPre))<<"  width along anode = "<<sqrt(fabs(sigmaYSqPre))<<endl;
       //cout<<"initial XY = "<<sqrt(fabs(sigmaXYSqPre))<<endl;

     
     } else {
     
       phi = 0;
       sigmaXSqPre = fabs(sigma1Sq)*pow(cos(phi),2) + fabs(sigma2Sq)*pow(sin(phi),2);
       sigmaYSqPre = fabs(sigma1Sq)*pow(sin(phi),2) + fabs(sigma2Sq)*pow(cos(phi),2);
       sigmaXYSqPre = 0.;
     
     } 
   
       //cout<<"phi = "<<phi<<endl;
       //cout<<" width along drift = "<<sqrt(fabs(sigmaXSqPre))<<endl;
       //cout<<" width along anode = "<<sqrt(fabs(sigmaYSqPre))<<endl;
       
   }
   
  
   mPhi = phi;

   dSigmaXSqBydt1 = sigmaXSqFunc(tim,fabs(sigma1Sq),fabs(sigma2Sq),phi);
   dSigmaYSqBydt1 = sigmaYSqFunc(tim,fabs(sigma1Sq),fabs(sigma2Sq),phi);
   dSigmaXYSqBydt1 = sigmaXYSqFunc(tim,fabs(sigma1Sq),fabs(sigma2Sq),phi);

    sigmaXSqCor = mSigmaXSqPrev + (numBinDiv*steplen/24)*(dSigmaXSqBydt[1]-5.0*dSigmaXSqBydt[2] + 19.0*dSigmaXSqBydt[3] + 9.0*dSigmaXSqBydt1);
    sigmaYSqCor = mSigmaYSqPrev + (numBinDiv*steplen/24)*(dSigmaYSqBydt[1]-5.0*dSigmaYSqBydt[2] + 19.0*dSigmaYSqBydt[3] + 9.0*dSigmaYSqBydt1);
    sigmaXYSqCor = mSigmaXYSqPrev + (numBinDiv*steplen/24)*(dSigmaXYSqBydt[1]-5.0*dSigmaXYSqBydt[2] + 19.0*dSigmaXYSqBydt[3] + 9.0*dSigmaXYSqBydt1);

    
    for(int i = 0; i < 4; i++)
     {
      if(i < 3)
       {
        dSigmaXSqBydt[i] = dSigmaXSqBydt[i+1];
        dSigmaYSqBydt[i] = dSigmaYSqBydt[i+1];
	dSigmaXYSqBydt[i] = dSigmaXYSqBydt[i+1];
       }
      else
       {
        dSigmaXSqBydt[i] = dSigmaXSqBydt1;
        dSigmaYSqBydt[i] = dSigmaYSqBydt1;
	dSigmaXYSqBydt[i] = dSigmaXYSqBydt1;
       }
     }

     if(mFineDiv && mWrite)
     {
      
      if(!strncmp(mOption , "coulomb", strlen("coulomb"))){
	out1N<<tim<<setw(20)<<sqrt(sigmaXSqCor)*a<<setw(20)<<sqrt(sigmaYSqCor)*a<<"\n";
	//cout<<tim<<setw(20)<<sqrt(sigmaXSqCor)*a<<setw(20)<<sqrt(sigmaYSqCor)*a<<endl;
	}
      else if(!strncmp(mOption,"diffusion",strlen("diffusion"))){
	out2N<<tim<<setw(20)<<sqrt(sigmaXSqCor)*a<<setw(20)<<sqrt(sigmaYSqCor)*a<<"\n";
	//cout<<tim<<setw(20)<<sqrt(sigmaXSqCor)*a<<setw(20)<<sqrt(sigmaYSqCor)*a<<endl;
	}
      else if(!strncmp(mOption,"both",strlen("both"))){
	out3N<<tim<<setw(20)<<sqrt(sigmaXSqCor)*a<<setw(20)<<sqrt(sigmaYSqCor)*a<<"\n";
	//cout<<tim<<setw(20)<<sqrt(sigmaXSqCor)*a<<setw(20)<<sqrt(sigmaYSqCor)*a<<endl;
      }
     }

   }
  

  mSigmaXSqNow = fabs(sigmaXSqCor);
  mSigmaYSqNow = fabs(sigmaYSqCor);
  mSigmaXYSqNow = fabs(sigmaXYSqCor);

  mSigmaXSqPrev = mSigmaXSqNow;
  mSigmaYSqPrev = mSigmaYSqNow;
  mSigmaXYSqPrev = mSigmaXYSqNow;

  //mChargeNow = mTotCharge*exp(-tim/mLifeTime);


  if(!mFineDiv && mWrite)
    {
      if(!strncmp(mOption , "coulomb", strlen("coulomb")))
           out1N<<tim<<setw(20)<<sqrt(mSigmaXSqPrev)*a<<setw(20)<<sqrt(mSigmaYSqPrev)*a<<"\n";
        else if(!strncmp(mOption,"diffusion",strlen("diffusion")))
           out2N<<tim<<setw(20)<<sqrt(mSigmaXSqPrev)*a<<setw(20)<<sqrt(mSigmaYSqPrev)*a<<"\n";
        else if(!strncmp(mOption,"both",strlen("both")))
           out3N<<tim<<setw(20)<<sqrt(mSigmaXSqPrev)*a<<setw(20)<<sqrt(mSigmaYSqPrev)*a<<"\n";
        
      
      //cout<<tim<<setw(20)<<sqrt(mSigmaXSqNow)*a<<setw(20)<<sqrt(mSigmaYSqNow)*a<<endl;
     }

return 0;

}

double StSvtElectronCloud::sigmaXSqFunc(double tim, double sigma1Sq, double sigma2Sq, double phi){

  double sumFunc = func1(tim,sigma1Sq,sigma2Sq) + func2(tim,sigma1Sq,sigma2Sq);
  double diffFunc = func1(tim,sigma1Sq,sigma2Sq) - func2(tim,sigma1Sq,sigma2Sq);
  double func = 2*mDiffConstX;

  if(!strncmp(mOption , "coulomb", strlen("coulomb"))){

    func = 0.5*(sumFunc + diffFunc*cos(2.*phi));
 
  } else if(!strncmp(mOption,"diffusion",strlen("diffusion"))){

    func = 2*mDiffConstX;
 
  } else if(!strncmp(mOption,"both",strlen("both"))){
    
    func = 2*mDiffConstX + 0.5*(sumFunc + diffFunc*cos(2.*phi));
  }

  return func;
}

double StSvtElectronCloud::sigmaYSqFunc(double tim, double sigma1Sq, double sigma2Sq, double phi){

  double sumFunc = func1(tim,sigma1Sq,sigma2Sq) + func2(tim,sigma1Sq,sigma2Sq);
  double diffFunc = func1(tim,sigma1Sq,sigma2Sq) - func2(tim,sigma1Sq,sigma2Sq);
  double func = 2.*mDiffConstY;

  if(!strncmp(mOption , "coulomb", strlen("coulomb"))){

    func = 0.5*(sumFunc - diffFunc*cos(2.*phi));
 
  } else if(!strncmp(mOption,"diffusion",strlen("diffusion"))){

    func = 2*mDiffConstY;
 
  } else if(!strncmp(mOption,"both",strlen("both"))){
    
    func = 2*mDiffConstY + 0.5*(sumFunc - diffFunc*cos(2.*phi));
  }
 
  return func;
}

double StSvtElectronCloud::sigmaXYSqFunc(double tim, double sigma1Sq, double sigma2Sq, double phi){


  double diffFunc = func1(tim,sigma1Sq,sigma2Sq) - func2(tim,sigma1Sq,sigma2Sq);
  double func = 0;

  if(!strncmp(mOption , "coulomb", strlen("coulomb"))){

    func =  0.5*diffFunc*sin(2.*phi); 

  } else if(!strncmp(mOption,"diffusion",strlen("diffusion"))){

    func = 2.*(mDiffConstX*pow(sin(phi),2) - mDiffConstY*pow(cos(phi),2));

  } else if(!strncmp(mOption,"both",strlen("both"))){

    func = 2.*(mDiffConstX*pow(sin(phi),2) - mDiffConstY*pow(cos(phi),2)) + 0.5*diffFunc*sin(2.*phi);
  }

  return func;
}

double StSvtElectronCloud::func1(double tim, double sigma1Sq, double sigma2Sq)
{

  // cout<<"I am Here"<<endl;
 double sigma1 ,sigma2 ;
 double sr, sr2, sr3,sr4, sd, sd2, cons,cons3; 
 double fun1 ,multFactor, denominator,numerator;
 // double const1 = 0.43, const2 = 0.25, const3 = 0.58;
 //if(tim < 8e-05){
 //  cout<<"**** In func1 ****"<<endl;
 //  cout<<"sigma1Sq = "<<sigma1Sq<<"  sigma2Sq = "<<sigma2Sq<<endl; 
 //}
 sigma1 = sqrt(sigma1Sq);
 sigma2 = sqrt(sigma2Sq);

 sr = sigma2/sigma1;
 sr2 = sr*sr; sr3 = sr*sr2; sr4 = sr*sr3;
 sd = mSDD_thickness/sigma2; sd2 = (1 + sd*sd)*(1 + sd*sd);
 
 //cons = const1*sigma2/(const2*mSDD_thickness);
 cons = 5.73333333333333*sigma2;
 cons3 = cons*cons*cons;
 cons3 = 1.0 + sqrt(cons3);

 mChargeNow = mTotCharge*exp(-tim/mLifeTime);

 //multFactor = (1.0/(4*acos(-1)))*(mSi_Mobility/(mSi_DielConst*mPermitivity));
   
 multFactor = 0.00000001619961;

 //numerator = const1*pow(sr2, 1.0/3) - (const3/4)*log(sr4 + 1.0/sd2);
 numerator = 0.43*pow(sr2, 1.0/3) - 0.145*log(sr4 + 1.0/sd2);

 denominator = sigma1*pow(cons3, 1.0/3);

 //cout<<"func1 denominator = "<<denominator<<endl;
     
 fun1 = multFactor*mChargeNow*(numerator/denominator);

 return fun1;

}

double StSvtElectronCloud::func2(double tim, double sigma1Sq, double sigma2Sq)
{
 double sigma1 ,sigma2 ;
 double cons, cons3;
 double fun2, multFactor, denominator,numerator;
 //double const1 = 0.43, const2 = 0.25, const3 = 0.58;

 //if(tim < 8e-05){
 //  cout<<"**** In func2 ****"<<endl;
 //  cout<<"sigma1Sq = "<<sigma1Sq<<"  sigma2Sq = "<<sigma2Sq<<endl; 
 //}

 sigma1 = sqrt(sigma1Sq);
 sigma2 = sqrt(sigma2Sq);

 //cons = const1*sigma2/(const2*mSDD_thickness);
 cons = 5.73333333333333*sigma2;
 cons3 = cons*cons*cons;
 cons3 = 1.0 + sqrt(cons3);

 mChargeNow = mTotCharge*exp(-tim/mLifeTime);
 
 
 //multFactor = (1.0/(4*acos(-1)))*(mSi_Mobility/(mSi_DielConst*mPermitivity));
 multFactor = 0.00000001619961;

 //numerator = const3 - (const3 - const1)*sqrt(sigma2/sigma1);
 numerator = 0.58 - 0.15*sqrt(sigma2/sigma1);
 
 denominator = sigma1*pow(cons3, 1.0/3);
 //cout<<"func2 denominator = "<<denominator<<endl;
 fun2 = multFactor*mChargeNow*(numerator/denominator);

 return fun2;

}


double StSvtElectronCloud::getSigma1()
{
 double sigma1Sq, sigma2Sq, sn, cs, phi;

 sigma1Sq = 0.5*((mSigmaXSqNow + mSigmaYSqNow) + sqrt(pow((mSigmaXSqNow - mSigmaYSqNow),2) + 4.*pow(mSigmaXYSqNow,2)));
 sigma2Sq = 0.5*((mSigmaXSqNow + mSigmaYSqNow) - sqrt(pow((mSigmaXSqNow - mSigmaYSqNow),2) + 4.*pow(mSigmaXYSqNow,2)));

 phi=0;
 if((1. - sqrt(fabs(sigma2Sq)/fabs(sigma1Sq))) > 0.001){

     if((mSigmaXSqNow - mSigmaYSqNow)){
       phi = 0.5*atan(2.*mSigmaXYSqNow/(mSigmaXSqNow - mSigmaYSqNow));
     }else if(mTheta != 0 && (fabs(mSigmaXSqNow) == fabs(mSigmaYSqNow))){
       phi = mInitPhi;
       mSigmaXYSqNow = 0;
     } 
     
     /*
     cout<<"phi = "<<phi<<endl;
     cout<<" width along drift= "<<sqrt(mSigmaXSqNow)<<endl;
     cout<<" width along anode = "<<sqrt(mSigmaYSqNow)<<endl;
     */
   } else {
     
     mSigmaXSqNow = fabs(sigma1Sq)*pow(cos(phi),2) + fabs(sigma2Sq)*pow(sin(phi),2);
     mSigmaYSqNow = fabs(sigma1Sq)*pow(sin(phi),2) + fabs(sigma2Sq)*pow(cos(phi),2);
     mSigmaXYSqNow = 0.5*(mSigmaXSqNow - mSigmaYSqNow)*tan(2.*phi);
     
     if(mTheta != 0 && (fabs(mSigmaXSqNow) == fabs(mSigmaYSqNow))){
       phi = mInitPhi;
       mSigmaXYSqNow = 0;
     } 
     /*
     cout<<"phi = "<<phi<<endl;
     cout<<" width along drift= "<<sqrt(mSigmaXSqNow)<<endl;
     cout<<" width along anode = "<<sqrt(mSigmaYSqNow)<<endl;
     */
   }
 
 sn = sin(phi); cs = cos(phi);

 return sqrt(sn*sn*sigma1Sq + cs*cs*fabs(sigma2Sq));
}

double StSvtElectronCloud::getSigma2()
{
  double sigma1Sq, sigma2Sq, sn, cs, phi;
  phi=0;
  sigma1Sq = 0.5*((mSigmaXSqNow + mSigmaYSqNow) + sqrt(pow((mSigmaXSqNow - mSigmaYSqNow),2) + 4.*pow(mSigmaXYSqNow,2)));
  sigma2Sq = 0.5*((mSigmaXSqNow + mSigmaYSqNow) - sqrt(pow((mSigmaXSqNow - mSigmaYSqNow),2) + 4.*pow(mSigmaXYSqNow,2)));

  if((1. - sqrt(fabs(sigma2Sq)/fabs(sigma1Sq))) > 0.001){

     if((mSigmaXSqNow - mSigmaYSqNow)){
       phi = 0.5*atan(2.*mSigmaXYSqNow/(mSigmaXSqNow - mSigmaYSqNow));
     }else if(mTheta != 0 && (fabs(mSigmaXSqNow) == fabs(mSigmaYSqNow))){
       phi = mInitPhi;
       mSigmaXYSqNow = 0;
     } 
     /*
     cout<<"phi = "<<phi<<endl;
     cout<<" width along drift= "<<sqrt(mSigmaXSqNow)<<endl;
     cout<<" width along anode = "<<sqrt(mSigmaYSqNow)<<endl;
     */
   } else {
     
     mSigmaXSqNow = fabs(sigma1Sq)*pow(cos(phi),2) + fabs(sigma2Sq)*pow(sin(phi),2);
     mSigmaYSqNow = fabs(sigma1Sq)*pow(sin(phi),2) + fabs(sigma2Sq)*pow(cos(phi),2);
     mSigmaXYSqNow = 0.5*(mSigmaXSqNow - mSigmaYSqNow)*tan(2.*phi);
     
     if(mTheta != 0 && (fabs(mSigmaXSqNow) == fabs(mSigmaYSqNow))){
       phi = mInitPhi;
       mSigmaXYSqNow = 0;
     } 
     /*
     cout<<"phi = "<<phi<<endl;
     cout<<" width along drift= "<<sqrt(mSigmaXSqNow)<<endl;
     cout<<" width along anode = "<<sqrt(mSigmaYSqNow)<<endl;
     */
   }
 
  sn = sin(phi); cs = cos(phi);

  return sqrt(cs*cs*sigma1Sq + sn*sn*fabs(sigma2Sq));

}

double StSvtElectronCloud::getPhi()
{
 return mPhi;
}

double StSvtElectronCloud::getSigma1Sq()
{
  
  double sigma1Sq = 0.5*((mSigmaXSqNow + mSigmaYSqNow)
			 + sqrt(pow((mSigmaXSqNow - mSigmaYSqNow),2)
				+ 4.*pow(mSigmaXYSqNow,2)));
  return sigma1Sq;    //in mm squared
}


double StSvtElectronCloud::getSigma2Sq()
{
  double sigma2Sq = 0.5*((mSigmaXSqNow + mSigmaYSqNow)
			 - sqrt(pow((mSigmaXSqNow - mSigmaYSqNow),2)
				+ 4.*pow(mSigmaXYSqNow,2)));
 return fabs(sigma2Sq);     //in mm squared
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
