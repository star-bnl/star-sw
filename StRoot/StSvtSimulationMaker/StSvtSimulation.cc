/***************************************************************************
 *
 * $Id: StSvtSimulation.cc,v 1.9 2003/11/15 20:24:29 caines Exp $
 *
 * Author: Selemon Bekele
 ***************************************************************************
 * 
 * Description: Fills hybrid pixel objects
 *
 ***************************************************************************
 *
 * $Log: StSvtSimulation.cc,v $
 * Revision 1.9  2003/11/15 20:24:29  caines
 * fixes to remove warnings at compilation
 *
 * Revision 1.8  2003/11/13 16:24:59  caines
 * Further improvements to get simulator looking like reality
 *
 * Revision 1.7  2003/09/02 17:59:09  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.6  2003/07/31 19:18:10  caines
 * Petrs improved simulation code
 *
 * Revision 1.5  2001/08/13 15:34:18  bekele
 * Debugging tools added
 *
 * Revision 1.4  2001/03/19 22:25:53  caines
 * Catch wrong wafer ids more elegantly
 *
 * Revision 1.3  2001/03/15 15:12:08  bekele
 * added a method to fill the whole SVT hybrid with background
 *
 * Revision 1.2  2001/02/07 19:13:51  caines
 * Small fixes to allow to run without setup from command line
 *
 * Revision 1.1  2000/11/30 20:47:49  caines
 * First version of Slow Simulator - S. Bekele
 *
 **************************************************************************/


#include "StSvtClassLibrary/StSvtHybridPixelsC.hh"
#include "StDbUtilities/StGlobalCoordinate.hh"
#include "StSvtClassLibrary/StSvtHybridPixelsD.hh"
//#include "StDbUtilities/StSvtCoordinateTransform.hh"
//#include "StDbUtilities/StSvtWaferCoordinate.hh"

#include "StSvtAngles.hh"
#include "StSvtSignal.hh"
#include "StSvtSimulation.hh"

//#include "tables/St_svg_geom_Table.h"

//ClassImp(StSvtSimulation)

// outputs for the anodes into which a signal is divided
// output.. for adc versus time, peak.. for peak versus width
fstream outputR1,outputR2,outputR3,outputR4,outputR5,outputR6,outputR7,outputR8,outputR9;
fstream outputS1,outputS2,outputS3,outputS4,outputS5,outputS6,outputS7,outputS8,outputS9;
fstream outputSR1,outputSR2,outputSR3,outputSR4,outputSR5,outputSR6,outputSR7,outputSR8,outputSR9;
fstream peakR1,peakR2,peakR3,peakR4,peakR5,peakR6,peakR7,peakR8,peakR9;
fstream peakS1,peakS2,peakS3,peakS4,peakS5,peakS6,peakS7,peakS8,peakS9;
fstream peakSR1,peakSR2,peakSR3,peakSR4,peakSR5,peakSR6,peakSR7,peakSR8,peakSR9;

StSvtSimulation::StSvtSimulation()
{

 mElectronCloud = NULL;
 mSvtSignal = NULL;
 mSvtAngles = NULL;

 mSvtSignal = new StSvtSignal();

 mLowBin = 1;
 mHiBin = 128;
 mNumOfAnodesPerHit = 9;

 setPasaSigAttributes(kFALSE,8);
}

StSvtSimulation::~StSvtSimulation()
{
  delete mSvtSignal;
}

void StSvtSimulation::setOptions(Bool_t backgr, int option)
{
  mBackGrOption = backgr;
  mSignalOption = option;

  mSvtSignal->setOption(mSignalOption);
  
}

void StSvtSimulation::setPointers(StSvtElectronCloud* elCloud,StSvtAngles* svtAngles)
{

 mElectronCloud = elCloud;
 mSvtAngles = svtAngles;

}

void StSvtSimulation::setAnodeTimeBinSizes(double timBinSize, double anodeSize)
 {
  mTimeBinSize = timBinSize ;
  mAnodeSize = anodeSize;
  mSvtSignal->setAnodeTimeBinSizes(mTimeBinSize,mAnodeSize);
 }
  
  
void StSvtSimulation::setDriftVelocity(double driftVelocity)
{

  mDriftVelocity = driftVelocity;
  mSvtSignal->setDriftVelocity(mDriftVelocity);
  mSvtSignal->pasaRelatedStuff();
}

void StSvtSimulation::setTrappingConst(double trapConst)
{

  mTrapConst = trapConst;                   // [micro seconds]

}

void StSvtSimulation::setPasaSigAttributes(int pasaSigAttributes, int numOfAnodesPerHit)
 {
   mNumOfAnodesPerHit = numOfAnodesPerHit;
   mPasaSigAttributes = pasaSigAttributes;
     
   if(mNumOfAnodesPerHit == 0){

     mUpperAn = 3;
     mLowerAn = -3;

   } else {

     mUpperAn = (int)(mNumOfAnodesPerHit/2);

     if((mNumOfAnodesPerHit%2) == 0){

       mUpperAn = mUpperAn + 1;
     }

     mLowerAn = -mUpperAn;
   }
  cout<<"upperAn = "<<mUpperAn<<endl;
  cout<<"lowerAn = "<<mLowerAn<<endl;

 }

//____________________________________________________________________________
void StSvtSimulation::doCloud_FixHitPos(double anode,double time, double Energy)
{

  // double wStep = 0.001;    // mm

  //mTimeBinSize = 0.04;  //micro sec ---> 0.27 mm at 6.75 mm/us
  //2.7mm -----> a width of 10 time bins

  mSvtSignal->doPasaOnly(0);     //unnormalized response
  mSvtSignal->doPasaOnly(1);    //normalized response

  mElectronCloud->setPar(Energy,0,0,mTimeBinSize);  //this has to be done for each hit

  for(int j = 0; j < 3;j++){
    //do this for jth signal option

    for(int n = 1; n <= 2700; n++){

      mElectronCloud->setInitWidths(n*0.001,0.001);   //this has to be done for each hit
      mElectronCloud->setDriftVelocity(mDriftVelocity);
      mElectronCloud->setTrappingConst(mTrapConst);   
      mElectronCloud->calculateWidthAtAnode(time);
      mSvtSignal->getCloud(mElectronCloud);
      //calcPeakAndWidth(1,anode,time,mSignalOption);
      calcPeakAndWidth(0,anode,time,j);
      calcPeakAndWidth(1,anode,time,j);
    }
  }

}


//____________________________________________________________________________
void StSvtSimulation::doCloud_VaryHitPos(double anode,double Energy)
{
  double t;
   double wStep = 0.001;    // mm

  //mTimeBinSize = 0.04;  //micro sec ---> 0.27 mm at 6.75 mm/us
  //2.7mm -----> a width of 10 time bins

  mElectronCloud->setPar(Energy,0,0,mTimeBinSize);  //this has to be done for each hit
  
  for(int j = 0; j < 3;j++){
    //do this for jth signal option

    for(int n = 1; n <= 128; n++){

      t = (double)n - 0.5;
      mElectronCloud->setInitWidths(wStep,wStep);   //this has to be done for each hit
      mElectronCloud->setDriftVelocity(mDriftVelocity);
      mElectronCloud->setTrappingConst(mTrapConst);   
      mElectronCloud->calculateWidthAtAnode(t);
      mSvtSignal->getCloud(mElectronCloud);
      //calcPeakAndWidth(1,anode,t,mSignalOption);
      calcPeakAndWidth(0,anode,t,j);
      calcPeakAndWidth(1,anode,t,j);
    }
  }
   

}


//____________________________________________________________________________
void StSvtSimulation::doCloud(double time, double Energy,double mTheta,double mPhi)
{
  mElectronCloud->setPar(Energy,mTheta,mPhi,mTimeBinSize);  //this has to be done for each hit
  mElectronCloud->setInitWidths(0.001,0.001);   //this has to be done for each hit 
  mElectronCloud->setDriftVelocity(mDriftVelocity);
  mElectronCloud->setTrappingConst(mTrapConst);  
  mElectronCloud->calculateWidthAtAnode(time);
  mSvtSignal->getCloud(mElectronCloud);

}

//____________________________________________________________________________

void StSvtSimulation::calcPeakAndWidth(int k,double mAnHit, double mTimeHit, int option){

  float chargeOnAnode = 0.0, adc;// back;
  int counter = 0, anode,status ,tLowBin,tHiBin;
  //static int hit = 0;
  
   float t = 0.0;

   anode = (int)(mAnHit) + 1;    //mAnHit in fraction of anode numbers
 
   for(int an = mLowerAn; an <= mUpperAn ; an++)
     {
       ++counter;
      if(anode + an > 0 && anode + an <= 240)
       {
         //cout<<"****** an = "<<anode + an<<" *******"<<endl;

         chargeOnAnode = mSvtSignal->chargeFraction(anode + an,mAnHit); //mAnHit in fraction of anodes

         //cout<<"\n";
         //cout<<"chargeOnAnode = "<<chargeOnAnode<<endl;
       
         if(chargeOnAnode == 0.0)
           continue;

	 status = mSvtSignal->timeCenterAndWidth(mAnHit,mTimeHit);  //mTimeHit in fraction of time buckets

         mSvtSignal->calcConvSignal(chargeOnAnode);

	 tLowBin = mSvtSignal->getLowTBin();
	 tHiBin = mSvtSignal->getHiTBin();

	 if(k == 0){

	   for(int n = tLowBin; n <= tHiBin; n++)
	     {
	       t = n*mTimeBinSize;
           
	       adc = mSvtSignal->getSignal(n-1)/4;   // in counts with 1 count <-> 4mV

	       writeFiles1(counter, option, n, t,adc);
	    
	     }
	 }
     
	 if(k == 1){

	   writeFiles2(counter,option,mTimeHit,mSvtSignal->getTimeCenter(),mSvtSignal->getTimeWidth(),mSvtSignal->getPeak());
	 }
       }

     }
}
 

//____________________________________________________________________________
void StSvtSimulation::fillBuffer(double mAnHit, double mTimeHit, StSvtHybridPixelsD *svtSimDataPixels)
 {
   double chargeOnAnode, adc, peak;
   int counter = 0, anode,status;

   float t = 0.0;

   //cout<<"StSvtSimulation::fillBuffer"<<endl;
   
   anode = (int)(mAnHit) + 1;    //mAnHit in fraction of anode numbers
 
   mPeakSignal = 0.0;

   if(mPasaSigAttributes)
     resetAnodeAttributes(mNumOfAnodesPerHit + 1);
  
   for(int an = mLowerAn; an <= mUpperAn ; an++)
     {
      if(anode + an > 0 && anode + an <= 240)
       {
         //cout<<"****** an = "<<anode + an<<" *******"<<endl;
	
         chargeOnAnode = mSvtSignal->chargeFraction(anode + an,mAnHit); //mAnHit in fraction of anodes
         //cout<<"chargeOnAnode  "<<anode + an<<" = "<<chargeOnAnode<<endl;
	 //cout<<"\n";

         if(mPasaSigAttributes){
	 
	   resetSignal(counter,mLowBin,mHiBin);

	   mPasaSignals.mCharge[counter] = chargeOnAnode;
	 }

         if(chargeOnAnode == 0.0)
           continue;
	 
	 status = mSvtSignal->timeCenterAndWidth(mAnHit,mTimeHit);  //mTimeHit in fraction of time buckets
      
         mSvtSignal->calcConvSignal(chargeOnAnode);

	 mLowBin = mSvtSignal->getLowTBin();
	 mHiBin = mSvtSignal->getHiTBin();

	 peak = -1.0e-20;

         for(int n = mLowBin; n <= mHiBin; n++)
          {
           t = n*mTimeBinSize;
	    adc = mSvtSignal->getSignal(n-1);
	   if(adc==0.0) continue;
           
            adc = adc/3.90625;   // in counts with 1 count <-> 4mV*1000/1024

	    if(adc > peak) peak = adc;

             if(mPasaSigAttributes){
	       
              mPasaSignals.mTempBuffer[counter][n-1] = adc;
              //cout<<"adc = "<<adc<<"\t";
	     }

	     //if(anode + an == 15)
	     // cout<<"time bin = "<<n-1<<"\tadc = "<<adc<<endl;

             int pixelIndex = svtSimDataPixels->getPixelIndex(anode + an, n - 1);	     

	     /*Petr -background is done from outside
              int offset = svtSimDataPixels->getPedOffset();
	     //cout<<"offset = "<<offset <<endl;
	     
	     double adc1 = (double)svtSimDataPixels->getPixelContent(anode + an,n - 1);
	     //if(adc1 < offset) 
	     adc1 = adc1 - (double)offset;
	     

             if(mBackGrOption){
	       if(adc1 == 0.0){               
                   back = makeGausDev(backgrsigma);
                   adc = back + adc + offset;   // (int)adc will be in counts with 1 count <-> 4 mV
                   svtSimDataPixels->addToPixel(pixelIndex,adc);

	          }
               else{
                    adc = adc1 + adc + offset;   // (int)adc will be in counts with 1 count <-> 4 mV
                    svtSimDataPixels->addToPixel(pixelIndex,adc);
	         }
	        }
	     else {

	       adc = adc1 + adc;
	       //adc = adc + (double)offset;
	       //cout<<"adc = "<<adc<<endl;
		svtSimDataPixels->addToPixel(pixelIndex,adc);
		//svtSimDataPixels->AddAt(adc,pixelIndex);
		}*/
	    
	     double adc1 = svtSimDataPixels->getPixelContent(anode + an,n - 1);
	     adc+=adc1;
	     svtSimDataPixels->AddAt(adc,pixelIndex);
	     //adc can be negative - it should be taken care of during rounding to 8 bits
	  }

	 if(peak > mPeakSignal)
	   mPeakSignal = peak;

         if(mPasaSigAttributes){
        
         mPasaSignals.anode[counter] = anode + an;                   //actual anode
         mPasaSignals.mPeak[counter] = mSvtSignal->getPeak()/4;             //in counts (not exactly an integer)
         mPasaSignals.mTimeCenter[counter] = mSvtSignal->getTimeCenter();
         mPasaSignals.mTimeWidth[counter] = mSvtSignal->getTimeWidth();
         mPasaSignals.mUnderShoot[counter] = mSvtSignal->getMinUnderShoot();

         ++counter;
	 }
    
       }
     }

 }


double StSvtSimulation::makeGausDev(double sigma)
{

 static int iset = 0;
 static double gset;
 double fac,rsq,v1,v2;

 //if(*idum < 0) iset = 0;
  if(iset == 0)
   {
       
    do {
        v1 = 2.0*((float)rand()/(float)RAND_MAX) - 1.0;
        v2 = 2.0*((float)rand()/(float)RAND_MAX) - 1.0;
        rsq = v1*v1 + v2*v2;
        
    } while(rsq >= 1.0 || rsq == 0.0);

     fac = sigma*::sqrt(-2.0*::log(rsq)/rsq);

     gset = v1*fac;  // gset = 3.0*::sqrt(-2.0*::log(rsq))*(v1/::sqrt(rsq))
     iset = 1;
     return v2*fac;
   }
  else
    {
     iset = 0;
     return gset;
    }
}

void StSvtSimulation::resetAnodeAttributes(int numOfAnodes)
{

      for(int j = 0; j <numOfAnodes; j++){

      mPasaSignals.anode[j] = 0;                   //actual anode
      mPasaSignals.mPeak[j] = 0;
      mPasaSignals.mTimeCenter[j] = 0;
      mPasaSignals.mTimeWidth[j] = 0;
      mPasaSignals.mUnderShoot[j] = 0;
      }
}

void StSvtSimulation::resetSignal(int an, int lTBin, int hTBin)
{

  for(int n = lTBin; n <= hTBin; n++)
    {
      mPasaSignals.mTempBuffer[an][n-1] = 0;    //adc counts, an is anode counter
    }
}
 

int StSvtSimulation::writeFiles1(int c, int i, int n, double t, double adc)
{
  double mTimBin = (double)n - 0.5;

  if(c == 0)
   {
    if(i ==1)
     outputR1<<mTimBin<<setw(20)<<t<<setw(20)<<adc<<endl;
    else if(i == 2)
     outputS1<<mTimBin<<setw(20)<<t<<setw(20)<<adc<<endl;
    else
     outputSR1<<mTimBin<<setw(20)<<t<<setw(20)<<adc<<endl; 
   }

  else if(c == 1)
   {
    if(i== 1)
     outputR2<<mTimBin<<setw(20)<<t<<setw(20)<<adc<<endl;
    else if(i == 2)
     outputS2<<mTimBin<<setw(20)<<t<<setw(20)<<adc<<endl;
    else
     outputSR2<<mTimBin<<setw(20)<<t<<setw(20)<<adc<<endl; 
   }

  else if(c == 2)
   {
    if(i==1)
     outputR3<<mTimBin<<setw(20)<<t<<setw(20)<<adc<<endl; 
    else if(i == 2)
     outputS3<<mTimBin<<setw(20)<<t<<setw(20)<<adc<<endl;
    else
     outputSR3<<mTimBin<<setw(20)<<t<<setw(20)<<adc<<endl;  
   }

  else if(c == 3)
    {
     if(i== 1)
      outputR4<<mTimBin<<setw(20)<<t<<setw(20)<<adc<<endl;
     else if(i == 2)
      outputS4<<mTimBin<<setw(20)<<t<<setw(20)<<adc<<endl;
     else
      outputSR4<<mTimBin<<setw(20)<<t<<setw(20)<<adc<<endl; 
     }

  else if(c == 4)
   {
    if(i== 1)
      outputR5<<mTimBin<<setw(20)<<t<<setw(20)<<adc<<endl;
     else if(i == 2)
      outputS5<<mTimBin<<setw(20)<<t<<setw(20)<<adc<<endl;
     else
      outputSR5<<mTimBin<<setw(20)<<t<<setw(20)<<adc<<endl; 
   }

  else if(c == 5)
   {
     if(i== 1)
      outputR6<<mTimBin<<setw(20)<<t<<setw(20)<<adc<<endl;
     else if(i == 2)
      outputS6<<mTimBin<<setw(20)<<t<<setw(20)<<adc<<endl;
     else
      outputSR6<<mTimBin<<setw(20)<<t<<setw(20)<<adc<<endl; 
   }

 else if(c == 6)
   {
     if(i== 1)
      outputR7<<mTimBin<<setw(20)<<t<<setw(20)<<adc<<endl;
     else if(i == 2)
      outputS7<<mTimBin<<setw(20)<<t<<setw(20)<<adc<<endl;
     else
      outputSR7<<mTimBin<<setw(20)<<t<<setw(20)<<adc<<endl; 
   }

 else if(c == 7)
   {
    if(i==1)
      outputR8<<mTimBin<<setw(20)<<t<<setw(20)<<adc<<endl;
     else if(i ==2)
      outputS8<<mTimBin<<setw(20)<<t<<setw(20)<<adc<<endl;
     else
      outputSR8<<mTimBin<<setw(20)<<t<<setw(20)<<adc<<endl;
   }

 else if(c == 8)
   {
    if(i==1)
      outputR9<<mTimBin<<setw(20)<<t<<setw(20)<<adc<<endl;
     else if(i ==2)
      outputS9<<mTimBin<<setw(20)<<setw(20)<<t<<setw(20)<<adc<<endl;
     else
      outputSR9<<mTimBin<<setw(20)<<t<<setw(20)<<adc<<endl;
   }
  return 0;
}


int StSvtSimulation::writeFiles2(int c, int i, double timeBin,double timeCenter,double width,double peak)
{
 if(c == 1)
   {
    if(i ==1)
     peakR1<<timeBin<<setw(20)<<timeCenter<<setw(20)<<width<<setw(20)<<peak<<endl;
    else if(i ==2)
     peakS1<<timeBin<<setw(20)<<timeCenter<<setw(20)<<width<<setw(20)<<peak<<endl;
   else 
     peakSR1<<timeCenter<<setw(20)<<width<<setw(20)<<peak<<endl;
   }

if(c == 2)
   {
    if(i ==1)
     peakR2<<timeBin<<setw(20)<<timeCenter<<setw(20)<<width<<setw(20)<<peak<<endl;
    else if(i ==2)
     peakS2<<timeBin<<setw(20)<<timeCenter<<setw(20)<<width<<setw(20)<<peak<<endl;
    else 
     peakSR2<<timeCenter<<setw(20)<<width<<setw(20)<<peak<<endl;
 
   }

if(c == 3)
   {
    if(i ==1)
     peakR3<<timeBin<<setw(20)<<timeCenter<<setw(20)<<width<<setw(20)<<peak<<endl;
    else if(i ==2)
     peakS3<<timeBin<<setw(20)<<timeCenter<<setw(20)<<width<<setw(20)<<peak<<endl;
   else 
     peakSR3<<timeCenter<<setw(20)<<width<<setw(20)<<peak<<endl;
   }

if(c == 4)
   {
    if(i ==1)
     peakR4<<timeBin<<setw(20)<<timeCenter<<setw(20)<<width<<setw(20)<<peak<<endl;
    else if(i ==2)
     peakS4<<timeBin<<setw(20)<<timeCenter<<setw(20)<<width<<setw(20)<<peak<<endl;
    else 
     peakSR4<<timeCenter<<setw(20)<<width<<setw(20)<<peak<<endl;
}

if(c == 5)
   {
    if(i ==1)
     peakR5<<timeBin<<setw(20)<<timeCenter<<setw(20)<<width<<setw(20)<<peak<<endl;
    else if(i ==2)
     peakS5<<timeBin<<setw(20)<<timeCenter<<setw(20)<<width<<setw(20)<<peak<<endl;
   else 
     peakSR5<<timeCenter<<setw(20)<<width<<setw(20)<<peak<<endl;
}

if(c == 6)
   {
    if(i ==1)
     peakR6<<timeBin<<setw(20)<<timeCenter<<setw(20)<<width<<setw(20)<<peak<<endl;
    else if(i ==2)
     peakS6<<timeBin<<setw(20)<<timeCenter<<setw(20)<<width<<setw(20)<<peak<<endl;
    else 
     peakSR6<<timeCenter<<setw(20)<<width<<setw(20)<<peak<<endl;
   }

if(c == 7)
   {
    if(i ==1)
     peakR7<<timeBin<<setw(20)<<timeCenter<<setw(20)<<width<<setw(20)<<peak<<endl;
    else if(i == 2)
     peakS7<<timeBin<<setw(20)<<timeCenter<<setw(20)<<width<<setw(20)<<peak<<endl;
    else 
     peakSR7<<timeCenter<<setw(20)<<width<<setw(20)<<peak<<endl;
   }

if(c == 8)
   {
    if(i ==1)
     peakR8<<timeBin<<setw(20)<<timeCenter<<setw(20)<<width<<setw(20)<<peak<<endl;
    else if(i ==2)
     peakS8<<timeBin<<setw(20)<<timeCenter<<setw(20)<<width<<setw(20)<<peak<<endl;
    else 
     peakSR8<<timeCenter<<setw(20)<<width<<setw(20)<<peak<<endl;
   }

if(c == 9)
   {
    if(i ==1)
     peakR9<<timeBin<<setw(20)<<timeCenter<<setw(20)<<width<<setw(20)<<peak<<endl;
    else if(i ==2)
      peakS9<<timeBin<<setw(20)<<timeCenter<<setw(20)<<width<<setw(20)<<peak<<endl;
    else 
     peakSR9<<timeCenter<<setw(20)<<width<<setw(20)<<peak<<endl;
   }

 return 0;
}


void StSvtSimulation::openFiles(int k, int option)
{
 
/* peaks.open("peakPos.dat",ios::out);*/
  if(k==0){
    if(option == 1){

      outputR1.open("simR1.dat",ios::out);  outputR2.open("simR2.dat",ios::out);  outputR3.open("simR3.dat",ios::out);
      outputR4.open("simR4.dat",ios::out);  outputR5.open("simR5.dat",ios::out);  outputR6.open("simR6.dat",ios::out);
      outputR7.open("simR7.dat",ios::out);  outputR8.open("simR8.dat",ios::out);  outputR9.open("simR9.dat",ios::out);

    } else if(option == 2){

      outputS1.open("simS1.dat",ios::out);  outputS2.open("simS2.dat",ios::out);  outputS3.open("simS3.dat",ios::out);
      outputS4.open("simS4.dat",ios::out);  outputS5.open("simS5.dat",ios::out);  outputS6.open("simS6.dat",ios::out);
      outputS7.open("simS7.dat",ios::out);  outputS8.open("simS8.dat",ios::out);  outputS9.open("simS9.dat",ios::out);

    } else {

      outputSR1.open("simSR1.dat",ios::out);  outputSR2.open("simSR2.dat",ios::out);  outputSR3.open("simSR3.dat",ios::out);
      outputSR4.open("simSR4.dat",ios::out);  outputSR5.open("simSR5.dat",ios::out);  outputSR6.open("simSR6.dat",ios::out);
      outputSR7.open("simSR7.dat",ios::out);  outputSR8.open("simSR8.dat",ios::out);  outputSR9.open("simSR9.dat",ios::out);
    }
  } else {

    if(option == 1){

      peakR1.open("wpeakR1.dat",ios::out);  peakR2.open("wpeakR2.dat",ios::out);  peakR3.open("wpeakR3.dat",ios::out);
      peakR4.open("wpeakR4.dat",ios::out);  peakR5.open("wpeakR5.dat",ios::out);  peakR6.open("wpeakR6.dat",ios::out);
      peakR7.open("wpeakR7.dat",ios::out);  peakR8.open("wpeakR8.dat",ios::out);  peakR9.open("wpeakR9.dat",ios::out);

    } else if(option == 2){

      peakS1.open("wpeakS1.dat",ios::out);  peakS2.open("wpeakS2.dat",ios::out);  peakS3.open("wpeakS3.dat",ios::out);
      peakS4.open("wpeakS4.dat",ios::out);  peakS5.open("wpeakS5.dat",ios::out);  peakS6.open("wpeakS6.dat",ios::out);
      peakS7.open("wpeakS7.dat",ios::out);  peakS8.open("wpeakS8.dat",ios::out);  peakS9.open("wpeakS9.dat",ios::out);

    } else {

      peakSR1.open("wpeakSR1.dat",ios::out);  peakSR2.open("wpeakSR2.dat",ios::out);  peakSR3.open("wpeakSR3.dat",ios::out);
      peakSR4.open("wpeakSR4.dat",ios::out);  peakSR5.open("wpeakSR5.dat",ios::out);  peakSR6.open("wpeakSR6.dat",ios::out);
      peakSR7.open("wpeakSR7.dat",ios::out);  peakSR8.open("wpeakSR8.dat",ios::out);  peakSR9.open("wpeakSR9.dat",ios::out);
    }
  }
}

void StSvtSimulation::closeFiles(int k, int option)
{

/*peaks.close();*/
 if(k==0){
    if(option == 1){

      outputR1.close(); outputR2.close(); outputR3.close(); outputR4.close(); outputR5.close();
      outputR6.close(); outputR7.close(); outputR8.close(); outputR9.close();

    } else if(option == 2){

      outputS1.close(); outputS2.close(); outputS3.close(); outputS4.close(); outputS5.close();
      outputS6.close(); outputS7.close(); outputS8.close(); outputS9.close();

    } else {

      outputSR1.close(); outputSR2.close(); outputSR3.close(); outputSR4.close(); outputSR5.close();
      outputSR6.close(); outputSR7.close(); outputSR8.close(); outputSR9.close();
    }
 } else {
   if(option == 1){

     peakR1.close(); peakR2.close(); peakR3.close(); peakR4.close(); peakR5.close();
     peakR6.close(); peakR7.close(); peakR8.close(); peakR9.close();

    } else if(option == 2){

     peakS1.close(); peakS2.close(); peakS3.close(); peakS4.close(); peakS5.close();
     peakS6.close(); peakS7.close(); peakS8.close(); peakS9.close();

    } else {

     peakSR1.close(); peakSR2.close(); peakSR3.close(); peakSR4.close(); peakSR5.close();
     peakSR6.close(); peakSR7.close(); peakSR8.close(); peakSR9.close();
    }
 }

}
   

