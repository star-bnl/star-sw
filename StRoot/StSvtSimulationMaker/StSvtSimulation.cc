/***************************************************************************
 *
 * $Id: StSvtSimulation.cc,v 1.3 2001/03/15 15:12:08 bekele Exp $
 *
 * Author: Selemon Bekele
 ***************************************************************************
 *
 * Description: Fills hybrid pixel objects
 *
 ***************************************************************************
 *
 * $Log: StSvtSimulation.cc,v $
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

#include <fstream.h>

#include "StSvtClassLibrary/StSvtHybridPixels.hh"
#include "StDbUtilities/StGlobalCoordinate.hh"
#include "StDbUtilities/StSvtCoordinateTransform.hh"
#include "StDbUtilities/StSvtWaferCoordinate.hh"
#include "StThreeVector.hh"

#include "StSvtAngles.hh"
#include "StSvtSignal.hh"
#include "StSvtSimulation.hh"

#include "tables/St_svg_geom_Table.h"

//ClassImp(StSvtSimulation)

fstream outputR1,outputR2,outputR3,outputR4,outputR5,outputR6,outputR7,outputR8,outputR9;
fstream outputS1,outputS2,outputS3,outputS4,outputS5,outputS6,outputS7,outputS8,outputS9;
fstream outputSR1,outputSR2,outputSR3,outputSR4,outputSR5,outputSR6,outputSR7,outputSR8,outputSR9;
fstream peakR1,peakR2,peakR3,peakR4,peakR5,peakR6,peakR7,peakR8,peakR9;
fstream peakS1,peakS2,peakS3,peakS4,peakS5,peakS6,peakS7,peakS8,peakS9;

StSvtSimulation::StSvtSimulation()
{

 mElectronCloud = NULL;
 mSvtSignal = NULL;
 mSvtAngles = NULL;
}

StSvtSimulation::~StSvtSimulation()
{
 
}

void StSvtSimulation::setOptions(char* backgr)
{
 
  mBackGrOption = backgr;
}

void StSvtSimulation::setPointers(StSvtElectronCloud* elCloud, StSvtSignal* svtSignal ,StSvtAngles* svtAngles)
{
 mSvtSignal = svtSignal;
 mElectronCloud = elCloud;
 mSvtAngles = svtAngles;

}

void StSvtSimulation::setParam(double timBinSize, double anodeSize)
 {
  mTimeBinSize = timBinSize ;
  mAnodeSize = anodeSize;
 
 }


StSvtWaferCoordinate StSvtSimulation::toLocalCoord(double x, double y, double z,StSvtCoordinateTransform  *coTransform)
{
   StThreeVector<double> pos(0,0,0);
   StSvtWaferCoordinate waferCoord(0,0,0,0,0,0);
   StGlobalCoordinate globalCor;

   pos.setX(x);  pos.setY(y);  pos.setZ(z);
     
   globalCor.setPosition(pos);

   coTransform->operator()(globalCor,waferCoord);

   return  waferCoord;

 }


void StSvtSimulation::calcAngles(svg_geom_st *geom_st, double x, double y, double z, int mLayer, int mLadder, int mWafer )
{
  int hardWarePosition ,index = 0;
  StThreeVector<double> mom(0,0,0);
  StThreeVector<double> uVecN(0,0,0);
  StThreeVector<double> uVecD(0,0,0);
  StThreeVector<double> uVecT(0,0,0);

  //hardWarePosition = getLayerID()*1000 + 100*wafer + ladder;

   hardWarePosition = mLayer*1000 + 100*mWafer + mLadder;

   for( index=0; index < 216; index++){
    if( geom_st[index].id == hardWarePosition) 
       break;
     }
    
    mom.setX(x);
    mom.setY(y);
    mom.setZ(z);

    uVecN.setX(geom_st[index].n[0]);
    uVecN.setY(geom_st[index].n[1]);
    uVecN.setZ(geom_st[index].n[2]);

    uVecD.setX(geom_st[index].d[0]);
    uVecD.setY(geom_st[index].d[1]);
    uVecD.setZ(geom_st[index].d[2]);

    uVecT.setX(geom_st[index].t[0]);
    uVecT.setY(geom_st[index].t[1]);
    uVecT.setZ(geom_st[index].t[2]);

    mSvtAngles->svtTheta(mom,uVecN);
    mSvtAngles->svtPhi(mom,uVecD,uVecT);

}

//____________________________________________________________________________
void StSvtSimulation::doCloud(double time, double Energy,double mTheta,double mPhi)
{

      mElectronCloud->setPar(Energy,mTheta,mPhi,mTimeBinSize);  //this has to be done for each hit
      mElectronCloud->setInitWidths();   //this has to be done for each hit 
      mElectronCloud->calculateWidthAtAnode(time);
 
}
//____________________________________________________________________________
void StSvtSimulation::fillBuffer(double mAnHit, double mTimeHit, double backgrsigma, StSvtHybridPixels *svtSimDataPixels)
 {
   float chargeOnAnode = 0.0, adc, back;
   int counter = 0, anode,status ;
 
   float t = 0.0;

   anode = (int)(mAnHit) + 1;    //mAnHit in fraction of anode numbers
 
   for(int an = -3; an <= 3; an++)
     {
      if(anode + an > 0 && anode + an <= 240)
       {
         //cout<<"****** an = "<<anode + an<<" *******"<<endl;

         chargeOnAnode = mSvtSignal->chargeFraction(anode + an,mAnHit); //mAnHit in fraction of anodes

         //cout<<"\n";
         //cout<<"chargeOnAnode = "<<chargeOnAnode<<endl;
       
	 mCharge[counter] = chargeOnAnode;

         if(chargeOnAnode == 0.0)
           continue;

	 status = mSvtSignal->timeCenterAndWidth(mAnHit,mTimeHit);  //mTimeHit in fraction of time buckets
      
         mSvtSignal->calcConvSignal(chargeOnAnode);

         for(int n = 1; n <= 128; n++)
          {
           t = n*mTimeBinSize;
           
            adc = mSvtSignal->getSignal(n-1)/4;   // in counts with 1 count <-> 4mV

            if(adc == 0.0) continue;

             mTempBuffer[counter][n-1] = adc;
             // cout<<"adc = "<<adc;

             int pixelIndex = svtSimDataPixels->getPixelIndex(anode + an, n - 1);
	     double adc1 = svtSimDataPixels->getPixelContent(anode + an,n - 1);
             
             if(!strncmp(mBackGrOption,"doHitBackGr",strlen("doHitBackGr"))){
	       if(adc1 == 0.0){               
                   back = makeGausDev(backgrsigma);
                   adc = back + adc;   // (int)adc will be in counts with 1 count <-> 4 mV
                   svtSimDataPixels->addToPixel(pixelIndex,adc);

	          }
               else{
                    adc = adc1 + adc;   // (int)adc will be in counts with 1 count <-> 4 mV
                    svtSimDataPixels->addToPixel(pixelIndex,adc);
	         }
	        }
	     else {	      
                   adc = adc1 + adc;
                   svtSimDataPixels->addToPixel(pixelIndex,adc);
	        }
	      } 
        
         mPeakArray[counter] = mSvtSignal->getPeak();

         ++counter;
 
         mSvtSignal->setSignal();
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

     fac = sigma*sqrt(-2.0*log(rsq)/rsq);

     gset = v1*fac;  // gset = 3.0*sqrt(-2.0*log(rsq))*(v1/sqrt(rsq))
     iset = 1;
     return v2*fac;
   }
  else
    {
     iset = 0;
     return gset;
    }
}

void StSvtSimulation::reset()
{
  for(int i = 0; i < 7; i++){
    mCharge[i] = 0;
    for(int n = 0; n <128; n++){
     mTempBuffer[i][n] = 0;
    }
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
      outputS9<<mTimBin<<setw(20)<<t<<setw(20)<<adc<<endl;
     else
      outputSR9<<mTimBin<<setw(20)<<t<<setw(20)<<adc<<endl;
   }
  return 0;
}


int StSvtSimulation::writeFiles2(int c, int i, double width,double peak)
{
 if(c == 0)
   {
    if(i ==1)
     peakR1<<width<<setw(20)<<peak<<endl;
    else if(i ==2)
     peakS1<<width<<setw(20)<<peak<<endl;
   }

if(c == 1)
   {
    if(i ==1)
     peakR2<<width<<setw(20)<<peak<<endl;
    else if(i ==2)
     peakS2<<width<<setw(20)<<peak<<endl;
   }

if(c == 2)
   {
    if(i ==1)
     peakR3<<width<<setw(20)<<peak<<endl;
    else if(i ==2)
     peakS3<<width<<setw(20)<<peak<<endl;
   }

if(c == 3)
   {
    if(i ==1)
     peakR4<<width<<setw(20)<<peak<<endl;
    else if(i ==2)
     peakS4<<width<<setw(20)<<peak<<endl;
   }

if(c == 4)
   {
    if(i ==1)
     peakR5<<width<<setw(20)<<peak<<endl;
    else if(i ==2)
     peakS5<<width<<setw(20)<<peak<<endl;
   }

if(c == 5)
   {
    if(i ==1)
     peakR6<<width<<setw(20)<<peak<<endl;
    else if(i ==2)
     peakS6<<width<<setw(20)<<peak<<endl;
   }

if(c == 6)
   {
    if(i ==1)
     peakR7<<width<<setw(20)<<peak<<endl;
    else if(i == 2)
     peakS7<<width<<setw(20)<<peak<<endl;
   }

if(c == 7)
   {
    if(i ==1)
     peakR8<<width<<setw(20)<<peak<<endl;
    else if(i ==2)
     peakS8<<width<<setw(20)<<peak<<endl;
   }

if(c == 8)
   {
    if(i ==1)
     peakR9<<width<<setw(20)<<peak<<endl;
    else if(i ==2)
     peakS9<<width<<setw(20)<<peak<<endl;
   }

 return 0;
}


void StSvtSimulation::openFiles()
{
 
/* peaks.open("peakPos.dat",ios::out);*/
 

  outputR1.open("simR1.dat",ios::out);  outputR2.open("simR2.dat",ios::out);  outputR3.open("simR3.dat",ios::out);
  outputR4.open("simR4.dat",ios::out);  outputR5.open("simR5.dat",ios::out);  outputR6.open("simR6.dat",ios::out);
  outputR7.open("simR7.dat",ios::out);  outputR8.open("simR8.dat",ios::out);  outputR9.open("simR9.dat",ios::out);

  outputS1.open("simS1.dat",ios::out);  outputS2.open("simS2.dat",ios::out);  outputS3.open("simS3.dat",ios::out);
  outputS4.open("simS4.dat",ios::out);  outputS5.open("simS5.dat",ios::out);  outputS6.open("simS6.dat",ios::out);
  outputS7.open("simS7.dat",ios::out);  outputS8.open("simS8.dat",ios::out);  outputS9.open("simS9.dat",ios::out);

  outputSR1.open("simSR1.dat",ios::out);  outputSR2.open("simSR2.dat",ios::out);  outputSR3.open("simSR3.dat",ios::out);
  outputSR4.open("simSR4.dat",ios::out);  outputSR5.open("simSR5.dat",ios::out);  outputSR6.open("simSR6.dat",ios::out);
  outputSR7.open("simSR7.dat",ios::out);  outputSR8.open("simSR8.dat",ios::out);  outputSR9.open("simSR9.dat",ios::out);

  peakR1.open("wpeakR1.dat",ios::out);  peakR2.open("wpeakR2.dat",ios::out);  peakR3.open("wpeakR3.dat",ios::out);
  peakR4.open("wpeakR4.dat",ios::out);  peakR5.open("wpeakR5.dat",ios::out);  peakR6.open("wpeakR6.dat",ios::out);
  peakR7.open("wpeakR7.dat",ios::out);  peakR8.open("wpeakR8.dat",ios::out);  peakR9.open("wpeakR9.dat",ios::out);

  peakS1.open("wpeakS1.dat",ios::out);  peakS2.open("wpeakS2.dat",ios::out);  peakS3.open("wpeakS3.dat",ios::out);
  peakS4.open("wpeakS4.dat",ios::out);  peakS5.open("wpeakS5.dat",ios::out);  peakS6.open("wpeakS6.dat",ios::out);
  peakS7.open("wpeakS7.dat",ios::out);  peakS8.open("wpeakS8.dat",ios::out);  peakS9.open("wpeakS9.dat",ios::out);

 
}

void StSvtSimulation::closeFiles()
{

/*peaks.close();*/

 outputR1.close(); outputR2.close(); outputR3.close(); outputR4.close(); outputR5.close();
 outputR6.close(); outputR7.close(); outputR8.close(); outputR9.close();

 outputS1.close(); outputS2.close(); outputS3.close(); outputS4.close(); outputS5.close();
 outputS6.close(); outputS7.close(); outputS8.close(); outputS9.close();

 outputSR1.close(); outputSR2.close(); outputSR3.close(); outputSR4.close(); outputSR5.close();
 outputSR6.close(); outputSR7.close(); outputSR8.close(); outputSR9.close();

 peakR1.close(); peakR2.close(); peakR3.close(); peakR4.close(); peakR5.close();
 peakR6.close(); peakR7.close(); peakR8.close(); peakR9.close();

 peakS1.close(); peakS2.close(); peakS3.close(); peakS4.close(); peakS5.close();
 peakS6.close(); peakS7.close(); peakS8.close(); peakS9.close();

}
   

