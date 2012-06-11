/***************************************************************************
 *
 * $Id: StTrsWireHistogram.cc,v 1.33 2012/06/11 15:04:56 fisyak Exp $
 *
 * Author: brian, May 1998 
 ***************************************************************************
 *
 * Description: Collection of all StTrsMiniChargeSegment transported to
 *              the pad-plane
 *
 ***************************************************************************
 *
 * $Log: StTrsWireHistogram.cc,v $
 * Revision 1.33  2012/06/11 15:04:56  fisyak
 * std namespace
 *
 * Revision 1.32  2009/11/03 14:34:19  fisyak
 * Remove default in zFromTB
 *
 * Revision 1.31  2005/12/12 21:00:12  perev
 * 3 random generators ==> 1
 *
 * Revision 1.30  2004/04/07 18:57:25  perev
 * Improve memory usage
 *
 * Revision 1.29  2003/12/24 13:44:54  fisyak
 * Add (GEANT) track Id information in Trs; propagate it via St_tpcdaq_Maker; account interface change in StTrsZeroSuppressedReaded in StMixerMaker
 *
 * Revision 1.28  2003/12/17 18:22:01  fisyak
 * mIoSectorSpacing has never been defined and this causes that Inner Sector has randomly been ejected
 *
 * Revision 1.27  2003/09/02 17:59:19  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.26  2003/04/30 20:39:09  perev
 * Warnings cleanup. Modified lines marked VP
 *
 * Revision 1.25  2001/02/15 21:34:54  perev
 * clear improved
 *
 * Revision 1.24  2000/08/07 22:44:39  perev
 * srand48 added
 *
 * Revision 1.23  2000/08/04 03:32:18  long
 * nQOnWire<12--->nQOnWire<6.5
 * add "bin.sigmaT()+d[0]/12"
 *
 * Revision 1.22  2000/07/30 22:26:14  long
 * nQOnWire<12-->nQOnWire<6.5
 *
 * Revision 1.21  2000/07/30 02:45:58  long
 * 1)add random class
 * 2)add erf look up table and table builder,EXB pull dx[],time delay dz[]
 *   OmegaTau
 * 3)do Charge to wire assignment!!!
 * 4)add polya distribution for single electron avalanch.
 *
 * Revision 1.20  2000/02/24 16:42:22  long
 *  StTrsWireBinEntry
 * 		theNewSegment(position,0);--->
 * StTrsWireBinEntry
 * 		theNewSegment(position,0,bin.sigmaL(),bin.sigmaT());
 *
 * Revision 1.19  2000/01/10 23:14:31  lasiuk
 * Include MACROS for compatiblity with SUN CC5
 *
 * Revision 1.18  1999/12/08 02:10:43  calderon
 * Modified to eliminate warnings on Linux.
 *
 * Revision 1.17  1999/10/22 15:51:48  calderon
 * Remove ifdefs for erf.  Problem is solved by loading libm at the
 * macro level.
 *
 * Revision 1.16  1999/10/22 00:00:15  calderon
 * -added macro to use Erf instead of erf if we have HP and Root together.
 * -constructor with char* for StTrsDedx so solaris doesn't complain
 * -remove mZeros from StTrsDigitalSector.  This causes several files to
 *  be modified to conform to the new data format, so only mData remains,
 *  access functions change and digitization procedure is different.
 *
 * Revision 1.15  1999/10/04 16:17:33  long
 * wireCoordinate(kk)--> bin.position().y()
 *
 * Revision 1.14  1999/07/19 21:39:19  lasiuk
 * - addEntry() distributes charge on a (user) settable range of wires
 * - setRangeOfWiresForChargeDistribution(int) added (default is 0)
 *
 * Revision 1.13  1999/07/09 03:47:04  lasiuk
 * add switch for singleElectron multiplication, gaussian random
 * number generator
 *
 * Revision 1.12  1999/04/20 20:06:04  ward
 * Protection against pointer error in StTrsWireHistogram::clear
 *
 * Revision 1.11  1999/04/07 00:49:05  lasiuk
 * use the z offset for driftLength
 *
 * Revision 1.10  1999/02/16 23:40:32  lasiuk
 * check clear/add entry
 *
 * Revision 1.9  1999/02/14 20:44:32  lasiuk
 * gas gain settable via member function
 * escape if min()<0
 *
 * Revision 1.8  1999/02/12 01:26:38  lasiuk
 * Limit debug output
 *
 * Revision 1.7  1999/02/10 18:03:42  lasiuk
 * gas gain manual setting
 * debug output
 *
 * Revision 1.6  1999/02/10 04:28:29  lasiuk
 * comment debug
 *
 * Revision 1.5  1999/01/18 20:59:39  lasiuk
 * change gas gain 10^4
 *
 * Revision 1.4  1999/01/18 10:17:07  lasiuk
 * distanceToWire
 *
 * Revision 1.3  1998/11/16 14:47:25  lasiuk
 * use wireIndex to clarify name (not wireNumber)
 * remove mLastWire, mLastEntry
 *
 * Revision 1.2  1998/11/13 21:32:38  lasiuk
 * gains
 *
 * Revision 1.1  1998/11/10 17:12:28  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.5  1998/11/08 17:08:02  lasiuk
 * change from boolean macros
 * use resize() for LINUX/ allocators for SUN
 * add typedefs for vector<> types
 *
 * Revision 1.4  1998/11/03 17:31:57  lasiuk
 * incorporate gas gain/fluctuations
 * add time delay of collection depending on charge position wrt wire position
 * rename wire()
 *
 * Revision 1.3  1998/10/22 00:24:28  lasiuk
 * Oct 22
 *
 * Revision 1.2  1998/06/30 22:48:58  lasiuk
 * use db numbers; typecast yposition to wire position
 *
 * Revision 1.1  1998/06/04 23:31:58  lasiuk
 * Initial Revision
 *
 **************************************************************************/
#include <algorithm>
#include <unistd.h> // sleep

#ifndef ST_NO_EXCEPTIONS
#   include <stdexcept>
#   if !defined(ST_NO_NAMESPACES)
        using std::range_error;
#   endif
#endif
using std::min;
using std::max;
#include "SystemOfUnits.h"
#ifndef ST_NO_NAMESPACES
using namespace units;
#endif
#include "StTrsWireHistogram.hh"
#include "StTrsRandom.hh"
#include "TMath.h"

StTrsWireHistogram* StTrsWireHistogram::mInstance = 0; // static data member
HepJamesRandom  StTrsWireHistogram::mEngine;
RandGauss       StTrsWireHistogram::mGaussianDistribution(mEngine);
RandExponential StTrsWireHistogram::mExponentialDistribution(mEngine);

StTrsWireHistogram::StTrsWireHistogram(StTpcGeometry* geoDb, StTpcSlowControl* scDb, StTrsDeDx* gasDb,StMagneticField* mag)
    : mMin(-1),
      mMax(-1),
      mGeomDb(geoDb),
      mSCDb(scDb),
      mGasDb(gasDb),
      mDoGasGain(true),
      mDoGasGainFluctuations(true),
      mGasGainCalculationDone(false),
      mDoSingleElectronMultiplication(false),
      mDoTimeDelay(false),
      mSectorWires(0)    
      
{
    mSectorWires.reserve(10000);    
//VP    srand48(19460510);
    random=&(StTrsRandom::inst()); 
    mNumberOfEntriesInTable=4000;
    mRangeOfTable=4.0;
    mNumberOfInnerSectorAnodeWires =
	mGeomDb->numberOfInnerSectorAnodeWires();
    mNumberOfOuterSectorAnodeWires =
	mGeomDb->numberOfOuterSectorAnodeWires();

    mTotalNumberOfAnodeWires =
	mNumberOfInnerSectorAnodeWires + mNumberOfOuterSectorAnodeWires;
    mSectorWires.resize(mTotalNumberOfAnodeWires);
//     PR(mTotalNumberOfAnodeWires);

    gasGainCalculation();
    mGasGainCalculationDone = true;
    FreqFunctionTableBuilder();
    mOmegaTau=2.34*mag->at(StThreeVector<double>(0,0,0)).z()/kilogauss/5.0;
    dx[0]=mOmegaTau*0.14/::sqrt((1+mOmegaTau*mOmegaTau)*(1+mOmegaTau*mOmegaTau)*0.75+0.25);
    dx[1]=mOmegaTau*0.04/::sqrt((1+mOmegaTau*mOmegaTau)*(1+mOmegaTau*mOmegaTau)*0.925+0.075);
    dx[2]=-dx[1];
    dx[3]=-dx[0];
    dz[0]=0.08; //cm old 0.068cm
    dz[1]=-0.08; //cm 
    dz[2]=-0.08; //cm 
    dz[3]=0.08; //cm
    
   
}

StTrsWireHistogram::~StTrsWireHistogram() {/* nopt */}

void StTrsWireHistogram::FreqFunctionTableBuilder()
{//  erf(x)
     int  cntr=0;
    
do { 
  Double_t x = 1.0*cntr*mRangeOfTable/mNumberOfEntriesInTable;
  Double_t y = TMath::Erf(x);
  mFreqFunctionTable.push_back(y);
  cntr++; 
 }while(cntr < mNumberOfEntriesInTable);
 
}  
double  StTrsWireHistogram::table_fast( double argument) const
{
  

 
  
 
  float a =argument/mRangeOfTable*mNumberOfEntriesInTable;
  int index;
  if(a>=0){
   index = (int)(a+0.5);
   if(index>=mNumberOfEntriesInTable)return 1.0;
   return mFreqFunctionTable[index];}
  else
   {index = (int)(-a+0.5);
   if(index>=mNumberOfEntriesInTable)return -1.0;
   return -mFreqFunctionTable[index];}
}

  
   
StTrsWireHistogram* StTrsWireHistogram::instance(StTpcGeometry* geoDb, StTpcSlowControl* scDb, StTrsDeDx* gasDb,StMagneticField *mag)
{
    if(!mInstance) {
	mInstance = new StTrsWireHistogram(geoDb, scDb, gasDb,mag);
    }
    else { // do nothing
	std::cerr << "Cannot make a second instance of StTrsWireHistogram() " << endl;
	std::cerr << "Continuing..." << endl;
    }
    return mInstance;
}

void StTrsWireHistogram::addEntry(StTrsWireBinEntry& bin,int sector)
{
   // also needs db information to create coordinate->wire map
    // Find closes wire to: bin.position().y()
  double Q=bin.numberOfElectrons();
   double *d=bin.d();
   double innerSectorBoundary = mGeomDb->firstOuterSectorAnodeWire() - 0.5*mGeomDb->anodeWirePitch();
   //      mGeomDb->outerSectorEdge()  - mGeomDb->ioSectorSpacing(); mIoSectorSpacing has never been defined
    double yCoordinateOfHit = random->Gaus(bin.position().y(),(bin.sigmaT()+d[1]/12)/::sqrt(Q)); 
    double sigma=bin.sigmaT();
    double D=mGeomDb->anodeWirePitch()/2.;
  
    double dy=d[1];
    
    //  if(dy<=0.){cout<<"  wrong in dySubsegment calculation..."<<dy<<" "<<bin.position().y()<<" "<<bin.position().x()<<" "<<bin.position().z()<<endl;
    //    return ;}
    //     dy=0.;
       double yMax=yCoordinateOfHit+1.5*sigma+dy/2.0;
       double yMin=yCoordinateOfHit-1.5*sigma-dy/2.0;
	  //  double yMax=yCoordinateOfHit+3.0*sigma+dy/2.0;
	  //  double yMin=yCoordinateOfHit-3.0*sigma-dy/2.0;
   
    int WireHigh,WireLow;
    if(yCoordinateOfHit < innerSectorBoundary) { // in inner part of sector
	WireHigh =
          (int)((yMax - mGeomDb->firstInnerSectorAnodeWire())/mGeomDb->anodeWirePitch()+0.5);      
        WireLow =
	    (int)((yMin - mGeomDb->firstInnerSectorAnodeWire())/mGeomDb->anodeWirePitch() + .5);     
        WireHigh=min(mGeomDb->numberOfInnerSectorAnodeWires() - 1,WireHigh);
        WireLow=max(1,WireLow); 
     
	
    }
    else { // in outer part of sectortmpWireHigh =
            WireHigh=(int)((yMax - mGeomDb->firstOuterSectorAnodeWire())/mGeomDb->anodeWirePitch() + .5);	        
       	
             WireLow =(int)((yMin - mGeomDb->firstOuterSectorAnodeWire())/mGeomDb->anodeWirePitch() + .5);     
            WireHigh=min(mGeomDb->numberOfOuterSectorAnodeWires() - 1,WireHigh)+mGeomDb->numberOfInnerSectorAnodeWires();
            WireLow=max(1,WireLow)+mGeomDb->numberOfInnerSectorAnodeWires(); 
	
    } 
  
    int WireIndex,gWire;
    double Prob;
    double  nQOnWire;
    double y1,y2,y3,y4;
//VPunused    double ax=0.;
//VPunused    double  az=0.;
    double nQ=0; 
    int Qtotal=0;
    D=D/4;
    float w=0.1   ;  // pitch of gating grid ;
    for(WireIndex=WireLow;WireIndex<=WireHigh;WireIndex++)
      {
	for(gWire=-2;gWire<2;gWire++){
        
	 y2=(wireCoordinate(WireIndex)+w*gWire+w/2+D+dy-yCoordinateOfHit)/sigma/M_SQRT2;
         y1=(wireCoordinate(WireIndex)+w*gWire+w/2-D+dy-yCoordinateOfHit)/sigma/M_SQRT2;
         y4=(wireCoordinate(WireIndex)+w*gWire+w/2+D-dy-yCoordinateOfHit)/sigma/M_SQRT2;
         y3=(wireCoordinate(WireIndex)+w*gWire+w/2-D-dy-yCoordinateOfHit)/sigma/M_SQRT2;
	  

	  //    y2=(wireCoordinate(WireIndex)+w*gWire+D-yCoordinateOfHit)/sigma/M_SQRT2;
	  //  y1=(wireCoordinate(WireIndex)+w*gWire-D-yCoordinateOfHit)/sigma/M_SQRT2;
	 Prob=y2*table_fast(y2)-y1*table_fast(y1)
             -y4*table_fast(y4)+y3*table_fast(y3)
             +1./::sqrt(M_PI)*(exp(-y2*y2)- exp(-y1*y1)
	     +exp(-y3*y3)- exp(-y4*y4));
	    Prob=Prob*sigma/dy/4.0*M_SQRT2;
	    // Prob=0.5*(table_fast(y2)-table_fast(y1));
        nQOnWire=	(int)(Prob* Q+0.5);  
        Qtotal+=(int)nQOnWire;
	  if( Qtotal> Q)nQOnWire--;  
            
               
        double newx,newz;  
        
        if(nQOnWire>0){
          
          

          nQ+=nQOnWire;
	  //  float unknown=0.02;
	  //	  newx=random->Gaus(bin.position().x(),bin.sigmaT()/::sqrt(nQOnWire)+unknown); 
	  //	  cout<<bin.position().x()<<" "<<bin.sigmaT()<<" "<<nQOnWire<<" "<<bin.position().z()<<" "<<bin.sigmaL()<<endl;
	  // cin>>newx;
          newx=random->Gaus(bin.position().x(),(bin.sigmaT()+d[0]/12)/::sqrt(nQOnWire)); 
          
	  newz=random->Gaus(bin.position().z(),(bin.sigmaL()+d[2]/12)/::sqrt(nQOnWire)); 
     
           if(newz<0.)continue; 
	    if(sector>12.5)newx+=dx[gWire+2];
	   else
	    newx-=dx[gWire+2];  
           

	      newz+=dz[gWire+2]; 
          
	    //   ax=ax+newx*nQOnWire;
	    //   az=az+newz*nQOnWire;
	   //  newx=0;
	   //  newz=100;
	       StThreeVector<double>
	          position(newx,
			   wireCoordinate(WireIndex),
			  
			   //	    bin.position().y(),
	   		    newz); //HL 04/25/00
    
             
	
	   

	    //
	    // Gas Gain
	    double avalancheFactor = nQOnWire;
	    if(mDoGasGain) {
	       if(nQOnWire<6.5)mDoSingleElectronMultiplication=1;
	       //    mDoSingleElectronMultiplication=0;
		if(mDoSingleElectronMultiplication) {
		    avalancheFactor =
		      //	exponentialAvalanche(WireIndex,avalancheFactor );	
                                 polyaAvalanche(WireIndex,avalancheFactor );
		}
		else {  // Do Gaussian scaling
		    avalancheFactor =
			gaussianAvalanche(WireIndex, avalancheFactor);
		}
	    }  // end of gas gain
           
	    if(avalancheFactor<0.5)continue;
            StTrsWireBinEntry
		theNewSegment(position,0,bin.sigmaL(),sigma,bin.d(),bin.id()); 
	    theNewSegment.setNumberOfElectrons((int)(avalancheFactor+0.5)); 
	  
	    // Time Delay
	    // Increase DriftLength Proportional to the Distance from the Wire
	    // Keeping in mind a 2mm shift is approximately .1 timebins
	    // This means 500 um is 2 mm!
	    // Currently a linear function is used, but a better profile
	    // can Probably be found with some study.

	    if(mDoTimeDelay) {
		double distanceToWire =
		    fabs(yCoordinateOfHit - wireCoordinate(WireIndex));
		{
#ifndef ST_NO_NAMESPACES
		    using namespace units;
#endif
		    double increase = 250*micrometer/millimeter*distanceToWire;

		    double oldDriftLength = bin.position().z();
		    theNewSegment.position().setZ((oldDriftLength+increase));
		}
	    }

	    //
	    // You had better convince yourself
	    // that this is really right!
	    //
	    // Change coordinate of the wire
	    // due to diffusion.  Remember that this
	    // is not needed in the single electron
	    // limit!  That is the reason for the
	    // strange scale factor!  Must be done
	    // before the gas gain is calculated or
	    // the factor is ~1
	    //
// 	    double ne = bin.numberOfElectrons()*chargeFraction;
// 	    double scaledSigma = ::sqrt(fabs((ne-1)/ne)*sigma);
// 	    double xPosition =
// 		mGaussianDistribution.shoot(theNewSegment.position().x(),
// 					    (scaledSigma));
// 	    theNewSegment.position().setX(xPosition);

	    mSectorWires[WireIndex].push_back(theNewSegment);

	    //
	    // Set the limits for the wire Histogram
	    if (mMin<0) mMin = WireIndex;
	    if (mMax<0) mMax = WireIndex;
	    if (mMin >WireIndex ) mMin = WireIndex;
	    if (mMax <WireIndex) mMax = WireIndex;
	  
	} //endif nQ>0
	}  //loop over gWire
      }//  for loop over  WireIndex 
   
   
   
 
 
}

void StTrsWireHistogram::clear()
{
//VP    for(int ii=mMin; ii<= mMax; ii++) {
//VP        if(ii<0) continue; // Iwona and Herb, April 20 1999.
//VP	mSectorWires[ii].clear();
//VP    }
    
    int sz = mSectorWires.size();
    mSectorWires.clear();
    mSectorWires.resize(sz);
    mMin = -1;
    mMax = -1;

}

double StTrsWireHistogram::wireCoordinate(int index) const
{
    double wireY = (index < mNumberOfInnerSectorAnodeWires) ?
	mGeomDb->firstInnerSectorAnodeWire() + (index)*mGeomDb->anodeWirePitch() :
	mGeomDb->firstOuterSectorAnodeWire() + (index-mGeomDb->numberOfInnerSectorAnodeWires())*mGeomDb->anodeWirePitch();
    return wireY;
}

//vector<StTrsWireBinEntry>&
aTpcWire& StTrsWireHistogram::getWire(int num)
{
    if(num>=0 && num<mTotalNumberOfAnodeWires) {
	return mSectorWires[num];
    }
    else {
#ifndef ST_NO_EXCEPTIONS
	throw range_error("Bounds error in StTrsWireHistogram::wire()");
#else
	std::cerr << "Bounds error...only " << mTotalNumberOfAnodeWires << " wires." << endl;
	std::cerr << "Exitting..." << endl;
	return mSectorWires[0];  // This is a sad/sad workaround for no exceptions!
#endif
    }
}

//vector<vector<StTrsWireBinEntry> >&
aTpcWirePlane& StTrsWireHistogram::getWireHistogram()
{
    return mSectorWires;
}

// void StTrsWireHistogram::putWire(int wNumber, vector<StTrsWireBinEntry>& wire)
// {
//     if(wNumber>=0 && wNumber<mTotalNumberOfAnodeWires)
// 	mSectorWires[wNumber] = wire;
//     else {
// #ifndef ST_NO_EXCEPTIONS
// 	throw range_error("StTrsWireHistogram::putWire() index error!");
// #else
// 	std::cerr << "StTrsWireHistogram::putWire() index error!" << endl;
// #endif
//     }
// }

void StTrsWireHistogram::setDoGasGainFluctuations(bool doIt)
{
    // Make sure if you turn "on" fluctuations, that gas gain is on!
    if(doIt && !mDoGasGain)
	mDoGasGain = true;

    mDoGasGainFluctuations = doIt;
}

// These two could be inline, but are only called once
// so Probably doesn't matter performance wise
void StTrsWireHistogram::setGasGainInnerSector(double v)
{
    mInnerSectorGasGain = v;
    cout << "Gas gain IS: " << mInnerSectorGasGain << endl;
}


void StTrsWireHistogram::setGasGainOuterSector(double v)
{
    mOuterSectorGasGain = v;
    cout << "Gas gain OS: " << mOuterSectorGasGain << endl;
}

double StTrsWireHistogram::polya(){
  double x,y;
  do{
    x=StTrsRandom::inst().Rndm()*8.0;
    y=StTrsRandom::inst().Rndm();
  }while(y>2.4395225*x*::sqrt(x)/exp(x));
  return x;
}
          
double StTrsWireHistogram::polyaAvalanche(int iWire, double numElec)
{
    double gasGainFactor;
    double electrons = 0;
    double  b=0.4;
         mInnerSectorGasGain=3558;
    //   mOuterSectorGasGain=1315;
         mOuterSectorGasGain=1310;
    if(mDoGasGainFluctuations) {
	int ctr = 0;
	do {
	    gasGainFactor = polya();
	    electrons += gasGainFactor;
	    ctr++;
	} while(ctr<numElec);
         
  electrons =(iWire < mNumberOfInnerSectorAnodeWires) ?
	    electrons*b*mInnerSectorGasGain: 
	    electrons*b*mOuterSectorGasGain; 
    }
    else {
	electrons = (iWire < mNumberOfInnerSectorAnodeWires) ?
	    numElec*mInnerSectorGasGain :
	    numElec*mOuterSectorGasGain;
    }
    return electrons;
}
double StTrsWireHistogram::exponentialAvalanche(int iWire, double numElec)
{
    double gasGainFactor;
    double electrons = 0;
         mInnerSectorGasGain=3558;
  
         mOuterSectorGasGain=1310;
    if(mDoGasGainFluctuations) {
	int ctr = 0;
	do {
	    gasGainFactor = (iWire < mNumberOfInnerSectorAnodeWires) ?
		mExponentialDistribution.shoot(mInnerSectorGasGain) :
		mExponentialDistribution.shoot(mOuterSectorGasGain);
	    electrons += gasGainFactor;
	    ctr++;
	} while(ctr<numElec);
    }
    else {
	electrons = (iWire < mNumberOfInnerSectorAnodeWires) ?
	    numElec*mInnerSectorGasGain :
	    numElec*mOuterSectorGasGain;
    }
    return electrons;
}

double StTrsWireHistogram::gaussianAvalanche(int iWire, double numElec)
{
    //
    // The fluctuation values (13%) should be set via the
    // db or some macro call.  For now we will keep them
    // here
//VPunused    double innerFluc;
//VPunused    double outerFluc;
    double b=0.4;
    //   mInnerSectorGasGain=2503;
         mInnerSectorGasGain=3558;
    //   mOuterSectorGasGain=1315;
         mOuterSectorGasGain=1310;
    //  mDoGasGainFluctuations=0;
    if(mDoGasGainFluctuations) 
        return (iWire < mNumberOfInnerSectorAnodeWires) ?
	mGaussianDistribution.shoot(mInnerSectorGasGain*numElec,mInnerSectorGasGain*::sqrt(numElec*b)):
	mGaussianDistribution.shoot(mOuterSectorGasGain*numElec,mOuterSectorGasGain*::sqrt(numElec*b));
      
    
    else
         return (iWire < mNumberOfInnerSectorAnodeWires) ?
	 mInnerSectorGasGain*numElec:
	 mOuterSectorGasGain*numElec; 

   
}


double StTrsWireHistogram::noFluctuations(int wireIndex) const
{
    return (wireIndex<mNumberOfInnerSectorAnodeWires) ?
         mInnerSectorGasGain : mOuterSectorGasGain;
}

void StTrsWireHistogram::gasGainCalculation()
{
    // do calculation here and use values from db
    // For now use values from SN263
    //     mInnerSectorGasGain =
    // 	exp(scDb->innerSectorGasGainb*(innerSectorAnodeVoltage-innerSectorGasGainVzero));
    //     mOuterSectorGasGain =
    // 	exp(scDb->outerSectorGasGainb*(outerSectorAnodeVoltage-outerSectorGasGainVzero));
}
