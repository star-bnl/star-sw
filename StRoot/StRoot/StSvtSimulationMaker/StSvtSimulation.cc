/***************************************************************************
 *
 * $Id: StSvtSimulation.cc,v 1.13 2009/02/21 14:22:03 caines Exp $
 *
 * Author: Selemon Bekele
 ***************************************************************************
 * 
 * Description: Fills hybrid pixel objects
 *
 ***************************************************************************
 *
 * $Log: StSvtSimulation.cc,v $
 * Revision 1.13  2009/02/21 14:22:03  caines
 * Comment out debugging statements
 *
 * Revision 1.12  2005/07/23 03:37:34  perev
 * IdTruth + Cleanup
 *
 * Revision 1.11  2005/02/09 14:33:35  caines
 * New electron expansion routine
 *
 * Revision 1.10  2004/01/22 16:30:47  caines
 * Getting closer to a final simulation
 *
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


#include "StSvtClassLibrary/StSvtHybridPixelsD.hh"

#include "StSvtSignal.hh"
#include "StSvtSimulation.hh"
#include "StMCTruth.h"

//#include "tables/St_svg_geom_Table.h"

ClassImp(StSvtSimulation)


StSvtSimulation::StSvtSimulation()
{

 mElectronCloud = NULL;
 mSvtSignal = new StSvtSignal();
 mSvtSignal->setOption(0); //as default do use mixed method for PASA

 setPasaSigAttributes(kFALSE,8);
}

StSvtSimulation::~StSvtSimulation()
{
  delete mSvtSignal;
}

void StSvtSimulation::setOptions(int option)
{
  mSignalOption = option;
  mSvtSignal->setOption(mSignalOption);  
}

void StSvtSimulation::setElCloud(StSvtElectronCloud* elCloud)
{
 mElectronCloud = elCloud;
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



void StSvtSimulation::setPasaSigAttributes(int pasaSigAttributes, int numOfAnodesPerHit)
 {
   mNumOfAnodesPerHit = numOfAnodesPerHit;
   mPasaDebug = pasaSigAttributes;
     
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
   //cout<<"upperAn = "<<mUpperAn<<endl;
   //cout<<"lowerAn = "<<mLowerAn<<endl;

 }



//____________________________________________________________________________
void StSvtSimulation::doCloud(double time, double Energy,double mTheta,double mPhi,int trackId)
{
  mElectronCloud->setPar(Energy,mTheta,mPhi,mTimeBinSize,trackId);  //this has to be done for each hit
  mElectronCloud->setDriftVelocity(mDriftVelocity);
  mElectronCloud->CalcExpansion(time);
  //cout<<"Time " << time<<" sigAn= "<<mElectronCloud->getSigmaAnode()<<" sigT= "<<mElectronCloud->getSigmaDrift()<<endl;
  mSvtSignal->setCloud(mElectronCloud);
}


//____________________________________________________________________________
void StSvtSimulation::fillBuffer(double mAnHit, double mTimeHit, StSvtHybridPixelsD *svtSimDataPixels)
{
  //cout<<"StSvtSimulation::fillBuffer"<<endl;
 
  if(mPasaDebug){
    resetAnodeAttributes();
    resetBuffer(); 
  }

  mPeakSignal = 0.0;
  int centAn = (int)(mAnHit) + 1;    //mAnHit in fraction of anode numbers
  
  for(int an = mLowerAn; an <= mUpperAn ; an++)
    {
      int anode = centAn+an;
      if( (anode<=0)||(anode>240) ) continue;
      int counter = an-mLowerAn;

      //cout<<"****** an = "<<anode<<" *******"<<endl;
      double chargeOnAnode = mSvtSignal->chargeFraction(anode,mAnHit); //mAnHit in fraction of anodes
      //cout<<"\n";
      
      if(mPasaDebug)mPasaSignals.mCharge[counter] = chargeOnAnode;
            
      if(chargeOnAnode == 0.0) continue;
      
      mSvtSignal->timeCenterAndWidth(mAnHit,mTimeHit);  //mTimeHit in fraction of time buckets
      mSvtSignal->calcConvSignal(chargeOnAnode);
      
      int lowBin = mSvtSignal->getLowTBin();
      int hiBin = mSvtSignal->getHiTBin();
      // cout<<"   chargeOnAnode  "<<anode<<" = "<<chargeOnAnode<<  " for timebin " <<lowBin << " to " << hiBin << endl;
      mPeakSignal = -1.0e-20;
      for(int n = lowBin; n <= hiBin; n++)
	{
	  //double t = n*mTimeBinSize;
	  double adc = mSvtSignal->getSignal(n-1); //this is in mV
	  if(adc==0.0) continue;
	  adc = adc/3.90625;   // in counts with 1 count <-> 4mV*1000/1024
	  //cout<< "n = " << n << " Setting adc to " << adc << endl;
	  if(adc > mPeakSignal) mPeakSignal = adc;
          int trackId = mSvtSignal->getTrackId();

	  	  
	  if(mPasaDebug) mPasaSignals.mTempBuffer[counter][n-1] = adc;
	  	  
	  int pixelIndex = svtSimDataPixels->getPixelIndex(anode, n - 1);	     
	  svtSimDataPixels->addToPixel(pixelIndex, adc,trackId);
	  //adc can be negative - it should be taken care of during rounding to 8 bits
	}
      
         
      if(mPasaDebug){
	mPasaSignals.anode[counter] = anode;                   //actual anode
	mPasaSignals.mPeak[counter] = mSvtSignal->getPeak()/4;             //in counts (not exactly an integer)
	mPasaSignals.mTimeCenter[counter] = mSvtSignal->getTimeCenter();
	mPasaSignals.mTimeWidth[counter] = mSvtSignal->getTimeWidth();
	mPasaSignals.mUnderShoot[counter] = mSvtSignal->getMinUnderShoot();
      }

    }
}

void StSvtSimulation::resetAnodeAttributes()
{
      for(int j = 0; j <SvtSim_MaxBufferSize; j++)
	{
	  mPasaSignals.anode[j] = 0;                   //actual anode
	  mPasaSignals.mPeak[j] = 0;
	  mPasaSignals.mTimeCenter[j] = 0;
	  mPasaSignals.mTimeWidth[j] = 0;
	  mPasaSignals.mUnderShoot[j] = 0;
	}
}

void StSvtSimulation::resetBuffer()
{
  for (int an= 0; an<SvtSim_MaxBufferSize;an++) for(int n = 0; n < 128; n++)
    mPasaSignals.mTempBuffer[an][n] = 0;    //adc counts, an is anode counter
}
  
