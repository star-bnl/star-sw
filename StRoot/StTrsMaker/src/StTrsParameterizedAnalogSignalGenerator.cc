/***************************************************************************
 *
 * $Id: StTrsParameterizedAnalogSignalGenerator.cc,v 1.44 2019/02/08 21:33:54 iraklic Exp $
 *
 * Author: Hui Long
 ***************************************************************************
 *
 * Description:tss alogrithm for signal generator in the trsRevision 1.11  2000/02/10 01:21:50  calderon
 * normalization factors
 ***************************************************************************
 *
 *
 * $Log: StTrsParameterizedAnalogSignalGenerator.cc,v $
 * Revision 1.44  2019/02/08 21:33:54  iraklic
 * adding sector to the function call [iTPC related changes] : this also partial respons to ticket #3376
 *
 * Revision 1.43  2018/06/21 22:23:08  perev
 * TpcGroup fixes
 *
 * Revision 1.42  2018/02/20 22:45:53  smirnovd
 * Revert "Changes from Irakli's directory to make the code compile"
 *
 * Revision 1.40  2009/12/11 21:55:15  fisyak
 * Account that the space between anode wires and pad plane is sensitive, bug #1715
 *
 * Revision 1.39  2009/11/03 14:34:19  fisyak
 * Remove default in zFromTB
 *
 * Revision 1.38  2008/10/13 19:56:12  fisyak
 * Account that Z-offset is sector dependent
 *
 * Revision 1.37  2008/06/20 15:01:18  fisyak
 * move from StTrsData to StTpcRawData
 *
 * Revision 1.36  2007/07/12 20:25:05  fisyak
 * Use StarLogger, use time of flight, fix cluster shape
 *
 * Revision 1.35  2005/12/12 21:00:12  perev
 * 3 random generators ==> 1
 *
 * Revision 1.34  2005/09/09 22:12:49  perev
 * Bug fix + IdTruth added
 *
 * Revision 1.32  2005/07/19 22:23:05  perev
 * Bug fix
 *
 * Revision 1.31  2004/05/03 23:31:12  perev
 * Possible non init WarnOff
 *
 * Revision 1.30  2004/03/26 04:18:35  fisyak
 * Assign IdTruth even two or more tracks hit the same pixel
 *
 * Revision 1.29  2003/12/24 13:44:53  fisyak
 * Add (GEANT) track Id information in Trs; propagate it via St_tpcdaq_Maker; account interface change in StTrsZeroSuppressedReaded in StMixerMaker
 *
 * Revision 1.28  2003/09/02 17:59:19  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.27  2003/05/02 23:54:22  hardtke
 * Allow user to adjust normalFactor (i.e. Fudge Factor)
 *
 * Revision 1.26  2003/04/30 20:39:08  perev
 * Warnings cleanup. Modified lines marked VP
 *
 * Revision 1.25  2002/04/29 21:42:15  hardtke
 * change normalFactor back to 1.0
 *
 * Revision 1.24  2001/11/21 01:57:29  long
 * adding log message for 3/2001 long;
 * delete empty command line
 * double InOuterFactor=1.0075,normalFactor=1.4 ;;--->
 * double InOuterFactor=1.0075,normalFactor=1.4 ;
 *
 * Revision 1.23  2001/03/13 22:08:56  long
 * *** empty log message ***
 *
 * Revision 1.22  2000/08/04 03:30:18  long
 * 1)parameterize reponse function in Z
 * 2) add normalization factor 1.4 to match real data.It should
 *    eventually go to database somewhere.
 *
 * Revision 1.21  2000/07/30 02:50:05  long
 * 1) restructure code
 * 2)add new pad response function to take into account of the charge spread
 *   in x direction additional to transverse gaussian diffusion.
 *
 * Revision 1.20  2000/07/17 19:19:10  long
 * get zoffset by calling zFromTB
 *
 * Revision 1.19  2000/06/23 17:54:44  long
 * offset-->offset*mTimeBinWidth
 *
 * Revision 1.18  2000/06/23 00:12:40  snelling
 * Removed dependence on local files now pointed to StDbUtilities
 *
 * Revision 1.17  2000/06/22 17:52:44  long
 * get zoffset by calling tBFromZ
 *
 * Revision 1.16  2000/06/07 02:03:11  lasiuk
 * exit/abort ultimatum
 *
 * Revision 1.15  2000/05/19 17:22:29  long
 * fix bug in central pad calculation (mCentralPad) for non_central rows.
 *
 * Revision 1.14  2000/04/20 21:25:20  long
 * timeBinLowerLimit---><-----timeBinUpperLimit in
 * "if( signalTime-timeBinT> timeBinLowerLimit*mTimeBinWidth) break;
 *  if( timeBinT-signalTime> timeBinUpperLimit*mTimeBinWidth)"
 *
 * Revision 1.13  2000/03/15 02:13:20  calderon
 * Fixed bug from pad response function sigma:
 * The data member mPadResponseFunctionSigma was assigned the right values
 * but never used, whereas the temporary mPadRespondFunctionSigma was
 * not initialized and then used.  Removed the temporary one altogether.
 * Also removed declaration of two_pi, use twopi from PhysicalConstants.h
 *
 * Revision 1.12  2000/02/24 16:35:03  long
 * modification for step functions, normalization factors of the padresponse functions of inner ,outer sector
 *
 *Revision 1.12  2000/02/23 01:21:50  long
 * modification for step functions, normalization factors of the padresponse functions of inner ,outer sector
 * Revision 1.11  2000/02/10 01:21:50  calderon
 * Switch to use StTpcDb.
 * Coordinates checked for consistency.
 * Fixed problems with StTrsIstream & StTrsOstream.
 *
 * Revision 1.10  2000/01/10 23:14:31  lasiuk
 * Include MACROS for compatiblity with SUN CC5
 *
 * Revision 1.9  1999/11/11 19:45:11  calderon
 * Made variables-> data members in analog signal generator to avoid
 * initialization time when member functions are called.
 * Inlined:
 *  StTrsParameterizedAnalogSignalGenerator::signalSampler()
 *  StTrsSector::addEntry()
 *  StTrsSector::assignTimeBins()
 *
 * Revision 1.8  1999/11/10 15:46:25  calderon
 * Made changes to reduce timing, including:
 * Made coordinate transfrom a data member of StTrsAnalogSignalGenerator
 * Added upper-lower bound instead of symmetric cut.
 * Revived checking if signal is above threshold.
 *
 * Revision 1.7  1999/11/09 19:31:33  calderon
 * Modified loop over ContinuosAnalogTimeSequence to make it
 * more efficient.
 *
 * Revision 1.6  1999/11/05 22:18:17  calderon
 *
 * Made private copy constructor and operator= in StTrsDigitalSector.
 * Renamed DigitalSignalGenerators: Fast -> Old, Parameterized -> Fast
 * and use new "Fast" as default.
 * Added StTrsZeroSuppressedReader and StTrsZeroSuppressedReader for DAQ type
 * data access.
 * Removed vestigial for loop in sampleAnalogSignal() method.
 * Write version of data format in .trs data file.
 *
 * Revision 1.5  1999/10/25 18:38:49  calderon
 * changed mPos and pos() to mPosition and position() to
 * be compatible with StEvent/StMcEvent.
 *
 * Revision 1.4  1999/10/22 15:51:47  calderon
 * Remove ifdefs for erf.  Problem is solved by loading libm at the
 * macro level.
 *
 * Revision 1.3  1999/10/22 00:00:14  calderon
 * -added macro to use Erf instead of erf if we have HP and Root together.
 * -constructor with char* for StTrsDedx so solaris doesn't complain
 * -remove mZeros from StTrsDigitalSector.  This causes several files to
 *  be modified to conform to the new data format, so only mData remains,
 *  access functions change and digitization procedure is different.
 *
 * Revision 1.2  1999/10/06 16:50:44  long
 *  in the calculation of sigma_x,iter->position().z()----->mGeomDb->frischGrid()-iter->position().z()
 *
 * Revision 1.1  1999/10/04 15:43:00  long
 * TrsParameterizedAnalogSignalGenerator using tss algorithm
 *
 *
 **************************************************************************/
 
#include <unistd.h>
#include <math.h>
#include <algorithm>
#include <ctime>
#include "SystemOfUnits.h"
#include "PhysicalConstants.h"

#include "StTrsParameterizedAnalogSignalGenerator.hh"
#ifndef TPC_DATABASE_PARAMETERS
#include "StTpcLocalSectorCoordinate.hh"
#else
#include "StDbUtilities/StTpcLocalSectorCoordinate.hh"
#endif
#if defined (__SUNPRO_CC) && __SUNPRO_CC >= 0x500
using std::sort;
#endif
#include "StTrsRandom.hh"
#include "TMath.h"
static const double sigmaL = .037*centimeter/::sqrt(centimeter);
//static const double sigmaT = .0633*centimeter/::sqrt(centimeter);
static const double sqrtTwoPi = ::sqrt(twopi);
StTrsAnalogSignalGenerator* StTrsParameterizedAnalogSignalGenerator::mInstance = 0; 
// static data member
static const double FractionCut = 0.7;// minimum Fracktion of charge to be identified as generated by the track
//________________________________________________________________________________
struct SignalSum_t {
  double Sum;
  int    TrackId;
};

//StTrsParameterizedAnalogSignalGenerator::StTrsParameterizedAnalogSignalGenerator() {/* nopt */}

StTrsParameterizedAnalogSignalGenerator::StTrsParameterizedAnalogSignalGenerator(StTpcGeometry* geo, StTpcSlowControl* sc, StTpcElectronics* el, StTrsSector* sec)
    : StTrsAnalogSignalGenerator(geo, sc, el, sec),
      mPadResponseFunctionSigmaOuter(0.3913),//old 0.395
      mPadResponseFunctionSigmaInner(0.1964)//old 0.198
{

  //
  // Define here instead of calculating...
  //
  
  mDriftVelocity             = mSCDb->driftVelocity(13);
 
  mSamplingFrequency         = mElectronicsDb->samplingFrequency();//hz
  mTimeBinWidth              = 1./mSamplingFrequency;//s
  //mTau                     = mSigma1;
  mTau                       =mElectronicsDb->tau();// s   HL, 8/31/99   
  mPadResponseFunctionSigma   =0;                  //HL,defined in member function
  //                                              for inner and outer setor.
  //
  mFractionSampled=1.0;//HL,8/31/99
  normalFactor= 1.25; // YF from comparision AdcSum wrt data 04/04/08 | 1.0;//Default DH  5/2/03
  
//   PR(mDriftVelocity/(centimeter/(1.e-6*second)));
//   PR(mSamplingFrequency/MHz);
//   PR(mTimeBinWidth/nanosecond);
//   PR(mTau/nanosecond);

  // 
  // Set TSS parameters for signal generation
  //
   mChargeFractionOuter.push_back(0.33);
   mChargeFractionOuter.push_back(0.32); 
   mChargeFractionOuter.push_back(0.2515);
   mChargeFractionOuter.push_back(0.0856); 
   mChargeFractionOuter.push_back(0.01607);
   mChargeFractionOuter.push_back(0.0036);
   mChargeFractionInner.push_back(0.333); 
   mChargeFractionInner.push_back(0.298);
   mChargeFractionInner.push_back(0.038); 
   mChargeFractionInner.push_back(0.00181);
   mChargeFractionInner.push_back(0.0000);
   mChargeFractionInner.push_back(0.0000);
  
   mYb.push_back(0.2);
   mYb.push_back(0.6); 
   mYb.push_back(1.0);
   mYb.push_back(1.4); 
   mYb.push_back(1.8); 
   mNumberOfEntriesInTable=4000;
   mRangeOfTable=4.0;
   errorFunctionTableBuilder();
   
   mCentralPad = mCentralRow = 0;
   mNumberOfRows = mGeomDb->numberOfRows();
   mNumberOfInnerRows = mGeomDb->numberOfInnerRows();
   mFrischGrid    = mGeomDb->frischGrid();
   rowNormalization = padWidth = padLength = 0;
   zoffset = wire_to_plane_coupling = 0;
   delx = gridMinusZ = sigma_x = 0;
   dely = constant = localYDirectionCoupling = 0;
   timeOfSignal = chargeOfSignal = 0;
   t = tzero = K = 0;
   sigmaLoverTau = sigmaL/mTau;
   lambda = lambdasqr=0;
   //    srand(0);
   localArrayBuilder();

    mAddNoise = true;
   mNoiseRMS=1.2;
   mAdcConversion=mElectronicsDb->adcConversion();
   landauConstant=0.2703;
   landauMean=2.256; 
   landauSigma=1.197;
   expConstant=1.56538/10.;
   expSlope=-0.589033;
   landauCut=3.6;

   GausConstant[0]=0.2482;
   GausMean[0]=2.317; 
   GausSigma2[0]=1.326*1.326;
   
   ExpConstant[0]=1.46831;
   ExpSlope[0]=-0.569438;
   cutT[0]=3.6;
   
   GausConstant[1]=0.315;
   GausMean[1]=2.385; 
   GausSigma2[1]=1.464*1.464;
   
   ExpConstant[1]=1.44213;
   ExpSlope[1]=-0.55952;
   cutT[1]=3.7;
  
   GausConstant[2]=0.2153;
   GausMean[2]=2.473; 
   GausSigma2[2]=1.636*1.636;
   
   ExpConstant[2]=1.4916;
   ExpSlope[2]=-0.55763;
   cutT[2]=4;
  
   GausConstant[3]=0.2;
   GausMean[3]=2.566; 
   GausSigma2[3]=1.83*1.83;
   
   ExpConstant[3]=1.73945;
   ExpSlope[3]=-0.57317;
   cutT[3]=4.5;
 
   GausConstant[4]=0.1861;
   GausMean[4]=2.599; 
   GausSigma2[4]=1.983*1.983;
   
   ExpConstant[4]=1.54719;
   ExpSlope[4]=-0.544514;
   cutT[4]=4.5; 
   GausConstant[5]=0.1734;
   GausMean[5]=2.658; 
   GausSigma2[5]=2.17*2.17;
   
   ExpConstant[5]=1.70843;
   ExpSlope[5]=-0.54852;
   cutT[5]=5;
   
   
}
StTrsParameterizedAnalogSignalGenerator::~StTrsParameterizedAnalogSignalGenerator() {/* missing */}

StTrsAnalogSignalGenerator*
StTrsParameterizedAnalogSignalGenerator::instance()
{
    if (!mInstance) {
#ifndef ST_NO_EXCEPTIONS
	throw domain_error("StTrsParameterizedAnalogSignalGenerator::instance() Invalid Arguments");
#else
	cerr << "StTrsParameterizedAnalogSignalGenerator::instance() Invalid Arguments" << endl;
	cerr << "Cannot create Instance" << endl;
	cerr << "Exitting..." << endl;
	exit(1);
#endif
    }
    return mInstance;
}

StTrsAnalogSignalGenerator*
StTrsParameterizedAnalogSignalGenerator::instance(StTpcGeometry* geo, StTpcSlowControl* sc, StTpcElectronics* el, StTrsSector* sec)
{
    if (!mInstance) {
	mInstance = new StTrsParameterizedAnalogSignalGenerator(geo, sc, el, sec);
    }
    // else do nothing
    return mInstance;
}

void StTrsParameterizedAnalogSignalGenerator::errorFunctionTableBuilder()
{
  
  int  cntr=0;
 
  do { 
    Double_t x = 1.0*cntr*mRangeOfTable/mNumberOfEntriesInTable;
    Double_t y = TMath::Erf(x);
    mErrorFunctionTable.push_back(y);
    cntr++;
    
  }while(cntr < mNumberOfEntriesInTable);
 
}
void StTrsParameterizedAnalogSignalGenerator::localArrayBuilder()
{
  int rows=mNumberOfRows;
  
  int pad,row;
  for(row=0;row<rows;row++)
     {int max_pads=mGeomDb->numberOfPadsAtRow(row+1);
      mPadsAtRow[row]=max_pads;
      yCentroid[row]=transformer.yFromRow(20,row+1);
      for(pad=0;pad<max_pads;pad++)
	{ xCentroid[row][pad]=transformer.xFromPad(20,row+1,pad+1);     
	 // gain[row][pad]=1.0+(StTrsRandom::inst().Rndm()-0.5)*0.20;
              }
        
      
     }
  
 
}

double  StTrsParameterizedAnalogSignalGenerator::erf_fast(double argument) const
{
  

 float a =argument/mRangeOfTable*mNumberOfEntriesInTable;
  int index;
  if(a>=0){
   index = (int)(a+0.5);
   if(index>=mNumberOfEntriesInTable)return 1.0;
   return mErrorFunctionTable[index];}
  else
   {index = (int)(-a+0.5);
   if(index>=mNumberOfEntriesInTable)return -1.0;
   return -mErrorFunctionTable[index];}
}
  
 


void StTrsParameterizedAnalogSignalGenerator::inducedChargeOnPad(StTrsWireHistogram* wireHistogram, Int_t sector)
{ double offset=transformer.zFromTB(0,sector,45);

    int PadsAtRow;
    double sigma_xpad2;
    double InOuterFactor=1.0075;
    double charge_fraction[7]; 
    int wire_index;
    SignalSum_t *SignalSum = 0;//yf    double *SignalSum;
    double pitch=mGeomDb->anodeWirePitch();
//VPunused    int PadAtRow;
    
    //double mPadResponseFunctionSigma;
    //
    // This should probably be made a data member at some point!
//     StTpcCoordinateTransform transformer(mGeomDb, mSCDb, mElectronicsDb);
#if 0
    PR(wireHistogram->minWire());
    PR(wireHistogram->maxWire());
#endif
    if(wireHistogram->minWire()<0) {
	cerr << "Wire Plane is empty" << endl;
	return;
    }
        double x,y,z,xCentroidOfPad,yCentroidOfPad;
        double t; //VP xp,yp unused
//VPunused        double electrons;
        double tZero;
        tZero=mElectronicsDb->tZero();
	//reset all the datatbase value to keep them updated
       mDriftVelocity             = mSCDb->driftVelocity(sector);
 
       mSamplingFrequency         = mElectronicsDb->samplingFrequency();
       mTimeBinWidth              = 1./mSamplingFrequency;
       mTau                       =mElectronicsDb->tau();  
    int bin_start,bin_end;
    int pad_start,pad_end=max(mPadsAtRow[mNumberOfInnerRows-1],mPadsAtRow[mNumberOfRows-1])+1;
    int row_start,row_end;
    int pad_shift,pad_shift0=-999999;
    bin_start=0;
    bin_end=mGeomDb->numberOfTimeBuckets();
    row_start=0;
    row_end=  mNumberOfRows;
    
    int Nrbp = row_end*bin_end*pad_end;
    SignalSum              = (SignalSum_t  *) calloc(Nrbp, sizeof(SignalSum_t)); assert(SignalSum);
    //yf      SignalSum=(double  *)calloc(row_end*bin_end*pad_end,sizeof(double));
    int index;
    
       for(int jj=wireHistogram->minWire(); jj<=wireHistogram->maxWire(); jj++) {

	// StTrsWireHistogram defines typedefs:
	// ABOVE: typedef vector<StTrsWireBinEntry> aTpcWire
	aTpcWire currentWire = wireHistogram->getWire(jj);
	aTpcWire::iterator iter;
        int Repeat;
	//    int timeBinUpperLimit = 6;
	//    int timeBinLowerLimit = 2; 
             int timeBinUpperLimit = 6;
	     int timeBinLowerLimit = 2;
	     double dAvalanch=0.015;//cm
        int bin_low,bin_high; 
        double signalTime;	
        double pulseHeight ;
        int max_bin=bin_end;
        double  timeBinT;
        double *D,Dx,sigmaLz,Dt,sigmaLt;
        double A;
	for(iter  = currentWire.begin();
	    iter != currentWire.end();
	    iter++) {
	  const int id = iter->id(); // track Id
	  z=iter->position().z();
	  D=iter->d();
	  Dx=D[0]+dAvalanch;// approximation
	  Dt=D[2]/mDriftVelocity;   //mDriftVelocity :  cm/s
	  //   cout<<Dx<<" "<<D[2]<<" "<<mDriftVelocity<<endl;
          
	  if( z < 0.0) { // account anode wire pad plane space
	    mCentralRow = transformer.rowFromLocal(iter->position(),sector);
	    if ((mCentralRow <= 13 && z > -0.2) || 
		(mCentralRow >  13  && z > -0.4)) {z = - z;}
	    else {
	      cout<<"wrong z in function StTrsPara..e..."<<z<<endl;
	      continue;
	    } 
 	  }  
	   sigmaLz=sigmaL* ::sqrt(z);
	   sigmaLt=sigmaLz/mDriftVelocity;
	   signalTime =
	     (z-offset)/mDriftVelocity; //inner outer offset is already in z
            
	     //     cout<<signalTime<<" "<<z<<endl;
	     //    cin>>z;
             if(signalTime<0.)continue;
            
             
    
            pulseHeight= iter->numberOfElectrons();
           
            bin_low=max(0,(int)(signalTime/mTimeBinWidth)-timeBinLowerLimit+1);            
            bin_high=min(max_bin-1,(int)(signalTime/mTimeBinWidth)+timeBinUpperLimit);     
	  
	    //  K = sigmaLoverTau*::sqrt(z)/mDriftVelocity; 
	   

           
                 A=sigmaLt/mTau;
		 if(A<0.6){
		  
                      
                      for(int itbin=bin_low;itbin<=bin_high;itbin++){
	                SignalInTimeBin[itbin]=0.;
                        timeBinT = itbin*mTimeBinWidth; 
                      
                        t=(timeBinT-signalTime)/mTau;
			if(t<=landauCut)
			SignalInTimeBin[itbin]=landauConstant*exp(-(t-landauMean)*(t-landauMean))/(2.*landauSigma*landauSigma)*mTimeBinWidth/mTau;
                        else
                        SignalInTimeBin[itbin]=expConstant*exp(t*expSlope)*mTimeBinWidth/mTau;
			  
		                                                     }     
                                           
                           }
                 else
		    {
		      int index=(int)((A-0.6)/0.2);
                      if(index>=5.5)index=5;
                      for(int itbin=bin_low;itbin<=bin_high;itbin++){
	                SignalInTimeBin[itbin]=0.;
                        timeBinT = itbin*mTimeBinWidth; 
                      
                        t=(timeBinT-signalTime)/mTau;
			if(t<=cutT[index])
			SignalInTimeBin[itbin]=GausConstant[index]*exp(-(t-GausMean[index])*(t-GausMean[index])/(2.*GausSigma2[index]))*mTimeBinWidth/mTau;
                        else
			  SignalInTimeBin[itbin]=ExpConstant[index]*exp(t*ExpSlope[index])*mTimeBinWidth/mTau;}
		
		        //for 
		      /*     for(itbin=bin_low;itbin<=bin_high;itbin++){
	                SignalInTimeBin[itbin]=0.;
                        timeBinT = itbin*mTimeBinWidth; 
		
                        lambda =  (signalTime-timeBinT)/(K*mTau) + K;
                        lambdasqr = lambda*lambda;
			SignalInTimeBin[itbin]=0.5/mTau*(sqr(K)*exp(K*(lambda-.5*K))*
	( .5*(1+lambdasqr)*(1-erf_fast(lambda/M_SQRT2)) -
	  
	  lambda/sqrtTwoPi*exp(-.5*lambdasqr)))
                                      *mTimeBinWidth;
		       
			if(SignalInTimeBin[itbin]<0.)SignalInTimeBin[itbin]=0.;
		
			}// for itbin */
		       }//if endif
 	              
	                                                                  
	       


	    Repeat=0;
	    x=iter->position().x();
            y=iter->position().y(); 
              
	    
	    //    electrons=iter->numberOfElectrons();
            gridMinusZ     = z; // for new coordinates
	    sigma_x        = iter->sigmaT();
	    
		     
         
	 
	 
	    // StTpcLocalSectorCoordinate  xyCoord(iter->position(),12);
	    //  transformer(xyCoord,mTpcRaw);
	    //
	    mCentralRow = transformer.rowFromLocal(iter->position(), sector)-1;
	   
	  
	    // Calculate the row/pad limits
	    //   mRowLimits.first  = (mCentralRow > mDeltaRow) ?
	    //

         
	    if(y < mGeomDb->outerSectorEdge()) {
	      //	mRowLimits.second = min(mRowLimits.second, mNumberOfInnerRows); 
                     mRowLimits.second= mCentralRow;	
                     mRowLimits.first=  mCentralRow; //HL,2/20/00
                     
                     mPadResponseFunctionSigma= mPadResponseFunctionSigmaInner;  
			 //	 rowNormalization =0.285 ; //old 1.24
			 //   zoffset=mGeomDb->innerSectorzOffSet();
                        wire_to_plane_coupling=0.533*InOuterFactor;//HL,02/20/00
                        charge_fraction[0]=mChargeFractionInner[0];
                        charge_fraction[1]=mChargeFractionInner[1]; 
                        charge_fraction[2]=mChargeFractionInner[2];
                        charge_fraction[3]=mChargeFractionInner[3];
                        charge_fraction[4]=mChargeFractionInner[4];
                        charge_fraction[5]=mChargeFractionInner[5];
	    }
	    else {
                
		mRowLimits.first  = max(mCentralRow - mDeltaRow, mNumberOfInnerRows);
		mRowLimits.second = min(mCentralRow + mDeltaRow,mNumberOfRows-1);
                

                mPadResponseFunctionSigma= mPadResponseFunctionSigmaOuter;
			//   zoffset=mGeomDb->outerSectorzOffSet();
			//	rowNormalization = 0.62;//old 2.04
                        wire_to_plane_coupling=0.512;
			charge_fraction[0]=mChargeFractionOuter[0];
                        charge_fraction[1]=mChargeFractionOuter[1]; 
                        charge_fraction[2]=mChargeFractionOuter[2];
                        charge_fraction[3]=mChargeFractionOuter[3];
                        charge_fraction[4]=mChargeFractionOuter[4];
                        charge_fraction[5]=mChargeFractionOuter[5];
	    }
// 	    

	    //   

	   sigma_xpad2=sigma_x *sigma_x+ mPadResponseFunctionSigma*mPadResponseFunctionSigma; 
	    for(int irow2=mRowLimits.second; irow2>=mRowLimits.first; irow2--) {              
	     
	        PadsAtRow=mPadsAtRow[irow2]-1;
                yCentroidOfPad = yCentroid[irow2];
		
	     
                dely           = fabs(yCentroidOfPad-y);            
                wire_index=(int)(fabs(dely)/pitch+0.5);
		if(wire_index<6)
		  localYDirectionCoupling =charge_fraction[wire_index];
		else 
		  localYDirectionCoupling =0.;
                if(localYDirectionCoupling<1.e-5)continue;
                 
	       
                if(Repeat<0.5){ 
                  
		  mCentralPad = (Int_t) transformer.padFromLocal(iter->position(),sector,irow2+1)-1;
		   if(mCentralPad>  PadsAtRow)mCentralPad= PadsAtRow;//upper limit boundary check.

                 
		
		    mPadLimits.first  = max(mCentralPad - mDeltaPad ,0); 
		    mPadLimits.second =min(mCentralPad +mDeltaPad ,PadsAtRow);
                    pad_shift0=PadsAtRow;
		    //       cout<< mPadLimits.first<< " "<<mPadLimits.second<<" "<<PadsAtRow<<" "<<irow2<<endl;
//  		  
		     for(int ipad2=mPadLimits.first; ipad2<=mPadLimits.second; ipad2++) {
// 		    cout << " row: " << irow << " pad: " << ipad << endl;

		      
		      
		   
	            
                  
		    // Integral limits for nearest pad
		    xCentroidOfPad = xCentroid[irow2][ipad2];
		    //  cout<<  xCentroidOfPad << " xce  "<<ipad2<<endl;
		    //	    cin>>hhh;
		    delx           = xCentroidOfPad-x;
		    
		     

                    
		    //	    localXDirectionCoupling[ipad2]  =

		    //     mPadResponseFunctionSigma/::sqrt(sigma_xpad2)*exp(-0.5*delx*delx/sigma_xpad2);   //::sqrt(2pi) is absorbed in the local Y coupling
                     localXDirectionCoupling[ipad2]= mPadResponseFunctionSigma/Dx*(erf_fast((Dx/2-delx)/::sqrt(2*sigma_xpad2))-erf_fast((-Dx/2-delx)/::sqrt(2*sigma_xpad2)))*0.5;   //::sqrt(2pi) is absorbed in the local Y coupling
                                
                      
		    //	    cout<<ipad2<<" "<<localXDirectionCoupling[ipad2]<<" "<<endl;
                    
		      
		     
		       
		    //  chargeOfSignal=localYDirectionCoupling*localXDirectionCoupling[ipad2]*wire_to_plane_coupling*pulseHeight*mGain*gain[irow2][ipad2]; 
                   
		    chargeOfSignal=localYDirectionCoupling*localXDirectionCoupling[ipad2]*pulseHeight*mGain;
                    if(chargeOfSignal<0.)  continue;// ASIC threshold 
		    
		   

                    for(int itbin2=bin_low;itbin2<=bin_high;itbin2++){
                   
//VP                  index=irow2*bin_end*pad_end+ipad2*bin_end+itbin2;
                      index=(irow2*pad_end+ipad2)*bin_end+itbin2;
		      //yf		      *(SignalSum+index)+=chargeOfSignal*SignalInTimeBin[itbin2]; 
		      addSignal(id, chargeOfSignal*SignalInTimeBin[itbin2], *(SignalSum+index));
                   
		    
		    }
		} // pad limits
		Repeat=1;
		} // if repeat<0.5
	     else{
                     pad_shift=(PadsAtRow-pad_shift0)/2; 
		     //      cout<<mPadLimits.first<<" "<<mPadLimits.second<<" "<<irow2<<" "<<pad_shift<<" "<<PadsAtRow<<" repeat"<<endl;
                     for(int ipad22=mPadLimits.first; ipad22<=mPadLimits.second; ipad22++) {
// 		    cout << " row: " << irow << " pad: " << ipad << endl;

		

		       if((ipad22+pad_shift)<0||(ipad22+pad_shift)> PadsAtRow)continue;    
		    
		   
		   

                    
		  
		  
		   

                   
		       	       
		       //     cout<<ipad22+pad_shift<<" repeat "<<localXDirectionCoupling[ipad22]<<" "<<endl;
		   
		          chargeOfSignal=localYDirectionCoupling*localXDirectionCoupling[ipad22]*pulseHeight*mGain ;
		       //     chargeOfSignal=localYDirectionCoupling*localXDirectionCoupling[ipad22]*pulseHeight*mGain*gain[irow2][ipad22]; 
                   
                    if(chargeOfSignal<0.)  continue;// ASIC threshold 
		    
//    		   
		    
		    //    chargeOfSignal *= electrons*mTimeBinWidth*mGain*0.5/mTau;;
		    
 		    
		  
		    
		 
		    for(int itbin22=bin_low;itbin22<=bin_high;itbin22++){
                      index=irow2*bin_end*pad_end+(ipad22+pad_shift)*bin_end+itbin22;
                       
		      //yf		       *(SignalSum+index)+=chargeOfSignal*SignalInTimeBin[itbin22];
		       addSignal(id, chargeOfSignal*SignalInTimeBin[itbin22], *(SignalSum+index));
		      
		       }
		
		    
		} // pad limits
      
	     }  //if Repeat>0.5
		//         cin>>pad_start;
	    } // row limits
	  
	  
	 	    		   
	} // (iterator) Loop over all wires

    } // (jj)Loop over the wires of the Histogram
    
    
       //   cout<<" kkkkkkkkkkkk"<<endl;
    	
     pad_start=0;

    for(int irow3=row_start;irow3<row_end;irow3++)
      {
      
        int pad_end2= mPadsAtRow[irow3];
        for(int ipad3=pad_start;ipad3<pad_end2;ipad3++)
	  { // mSector->addEntry(irow,ipad,dummy);
	    mDiscreteAnalogTimeSequence.clear();
          for(int itbin3=bin_start;itbin3<bin_end;itbin3++)
	    {
              index=irow3*pad_end*bin_end+ipad3*bin_end+itbin3;
	      //yf              double a=*(SignalSum+index);
	      double a=(*(SignalSum+index)).Sum;
             
	      if(a<=0)continue;
            
	        a+=(addNoise(mNoiseRMS)*mAdcConversion);
          
	      if(a<=mSignalThreshold)continue;
	  int id = SignalId(*(SignalSum+index));
	  mElectronicSignal.setId(id);
               mElectronicSignal.setTime(itbin3);
	      mElectronicSignal.setAmplitude(a*normalFactor);
              mDiscreteAnalogTimeSequence.push_back(mElectronicSignal);
	      
            }  
           
	   if(mDiscreteAnalogTimeSequence.size()>0)mSector->assignTimeBins(irow3+1,ipad3+1,mDiscreteAnalogTimeSequence);
	  }
      }
    free(SignalSum);
    
}

double StTrsParameterizedAnalogSignalGenerator::realShaperResponse(double tbin, StTrsAnalogSignal& sig)
{
    
    double value=0.0;
    
    
    //double sigmaL = .05*centimeter/::sqrt(centimeter);
    // double t = mTimeBinWidth*(tbin+.5);//started from the center of time bin
    //double t = mTimeBinWidth*(tbin);//  started from the edge of time bin ,HL
    t = tbin; //passed it directly.
    
    tzero = sig.time() ;// Hui Long,8/26/99
    // tzero =0;
    //double K = sigmaL*::sqrt(sig.time())/(tau*::sqrt(driftVelocity));
    K = sigmaLoverTau*::sqrt((tzero- mTimeShiftOfSignalCentroid)/mDriftVelocity);//retrieve the real drift length,HL,8/31/99
    //UNITS:   sigmaL:  cm/::sqrt(cm)
    //         mTau:    second
    //         tzero:  second
    //          mTimeBinwidth : second
    //         <mDriftVelocity  :cm/second

    lambda =  (tzero - t)/(K*mTau) + K;
    lambdasqr = lambda*lambda;
    // Corrected From SN197
    value = .5/(mTau)*sqr(K)*exp(K*(lambda-.5*K))*
	( .5*(1+lambdasqr)*(1-erf_fast(lambda)) -
	  
	  lambda/sqrtTwoPi*exp(-.5*lambdasqr));
    
    //  value*=mTimeBinWidth;
    
    
    //  value *= mFractionSampled*mGain*sig.amplitude();
        value *= sig.amplitude();
   
    return value;

}
//________________________________________________________________________________
void  StTrsParameterizedAnalogSignalGenerator::addSignal(const int id, const double signal, SignalSum_t &S) {
  S.Sum += signal;
  if ( id ) {
    if (! S.TrackId ) S.TrackId = id;
    else  // switch Id, works only for 2 tracks, more tracks ?
      if ( S.TrackId != id && S.Sum < 2*signal) S.TrackId = id;
  }
  return;
}
//________________________________________________________________________________
int  StTrsParameterizedAnalogSignalGenerator::SignalId(SignalSum_t &S) {
  return S.TrackId > 0 ? S.TrackId : 0;
}
//________________________________________________________________________________

double StTrsParameterizedAnalogSignalGenerator::addNoise(double sigma)
 {
   float  x, y, z;

   y = StTrsRandom::inst().Rndm();
   if (!y) y = StTrsRandom::inst().Rndm();
   z = StTrsRandom::inst().Rndm();
   x = z * 6.283185;
   
   return sigma*sin(x)*::sqrt(-2*::log(y));//Gaussian with mean=0
    
 }


void StTrsParameterizedAnalogSignalGenerator::sampleAnalogSignal()
{
  cout << "StTrsParameterizedAnalogSignalGenerator::sampleAnalogSignal()" << endl;
    
    // operates on mSector (an StTrsSector)

    // I have the centroid IN TIME (make sure!!!!) of each hit!
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    vector<StTrsAnalogSignal> continuousAnalogTimeSequence;
//VPunused    vector<StTrsAnalogSignal>::iterator lowerBound;
#else
    vector<StTrsAnalogSignal, allocator<StTrsAnalogSignal> > continuousAnalogTimeSequence;
//VPunused    vector<StTrsAnalogSignal, allocator<StTrsAnalogSignal> >::iterator lowerBound;
#endif
    //double freq = mElectronicsDb->samplingFrequency();
    //PR(freq);
    int timeBinUpperLimit = 7;
    int timeBinLowerLimit = 3;
   
    int max_pad,max_bin;  
   double timeBinT = 0;
   int bin_low,bin_high;
   double signalTime;	  
   max_bin=mGeomDb->numberOfTimeBuckets();    
    double pulseHeight ;
    for(int irow=1; irow<=mSector->numberOfRows(); irow++) {
      max_pad=mSector->numberOfPadsInRow(irow);
	for(int ipad=1; ipad<=max_pad; ipad++) {
       
	    continuousAnalogTimeSequence = mSector->timeBinsOfRowAndPad(irow,ipad);

	    mDiscreteAnalogTimeSequence.clear();

	    // Make sure it is not empty
           
	    if(!continuousAnalogTimeSequence.size()) continue; 
              
	    

	   
		
                   
		    for(mTimeSequenceIterator = continuousAnalogTimeSequence.begin();
  			mTimeSequenceIterator!=continuousAnalogTimeSequence.end();
 			mTimeSequenceIterator++) {
		     
 	 		signalTime = mTimeSequenceIterator->time();    
                        pulseHeight=mTimeSequenceIterator->amplitude();
      		 	bin_low=max(0,(int)(signalTime/mTimeBinWidth)-timeBinLowerLimit+1);
                         bin_high=min(max_bin,(int)(signalTime/mTimeBinWidth)+timeBinUpperLimit);
                        


                        for(int itbin=bin_low;itbin<=bin_high;itbin++){
                        timeBinT = itbin*mTimeBinWidth; 
                        K = sigmaLoverTau*::sqrt((signalTime- mTimeShiftOfSignalCentroid)/mDriftVelocity);
                        lambda =  (-timeBinT +signalTime)/(K*mTau) + K;
                        lambdasqr = lambda*lambda;
			SignalInTimeBin[itbin]+=(sqr(K)*exp(K*(lambda-.5*K))*
	( .5*(1+lambdasqr)*(1-erf_fast(lambda)) -
	  
	  lambda/sqrtTwoPi*exp(-.5*lambdasqr)))
                                      *pulseHeight;
 		        }// for itbin 
		    }   // for mtimeSequences
       
         
// 		   
		
		    
	   
              

		for(int itbin=0;itbin<max_bin;itbin++){
//
		  //	if(mAddNoise) SignalInTimeBin[itbin] += generateNoise(); 
		        
		if(	SignalInTimeBin[itbin]<mSignalThreshold) continue;
	       

		//Do not store analog Signal if it is not above a
		// minimal threshold (should read value from database) rowN
  		

		mElectronicSignal.setTime(itbin);
		mElectronicSignal.setAmplitude(SignalInTimeBin[itbin]);
		mDiscreteAnalogTimeSequence.push_back(mElectronicSignal);
	    
             
	        } // loop over time bins
// 	   
	       
	    mSector->assignTimeBins(irow,ipad,mDiscreteAnalogTimeSequence);
	    //  cout<<ipad<<" "<<irow<<endl;
	} // loop over pads
	
    } // loop over rows
    

}



