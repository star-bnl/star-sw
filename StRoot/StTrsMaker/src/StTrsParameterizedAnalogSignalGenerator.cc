/***************************************************************************
 *
 * $Id: StTrsParameterizedAnalogSignalGenerator.cc,v 1.19 2000/06/23 17:54:44 long Exp $
 *
 * Author: Hui Long
 ***************************************************************************
 *
 * Description:tss alogrithm for signal generator in the trsRevision 1.11  2000/02/10 01:21:50  calderon
 * normalization factors
 ***************************************************************************
 *
 * $Log: StTrsParameterizedAnalogSignalGenerator.cc,v $
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
 * Added StTrsDetectorReader and StTrsZeroSuppressedReader for DAQ type
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
#include "StDbUtilities/StTpcLocalSectorCoordinate.hh"
#if defined (__SUNPRO_CC) && __SUNPRO_CC >= 0x500
using std::sort;
#endif



static const double sigmaL = .037*centimeter/sqrt(centimeter);
//static const double sigmaT = .0633*centimeter/sqrt(centimeter);
static const double sqrtTwoPi = sqrt(twopi);
StTrsAnalogSignalGenerator* StTrsParameterizedAnalogSignalGenerator::mInstance = 0; 
// static data member

//StTrsParameterizedAnalogSignalGenerator::StTrsParameterizedAnalogSignalGenerator() {/* nopt */}

StTrsParameterizedAnalogSignalGenerator::StTrsParameterizedAnalogSignalGenerator(StTpcGeometry* geo, StTpcSlowControl* sc, StTpcElectronics* el, StTrsSector* sec)
    : StTrsAnalogSignalGenerator(geo, sc, el, sec),
      mPadResponseFunctionSigmaOuter(0.37),//old 0.395
      mPadResponseFunctionSigmaInner(0.17)//old 0.198
{

  //
  // Define here instead of calculating...
  //
  
  mDriftVelocity             = mSCDb->driftVelocity();
  mSamplingFrequency         = mElectronicsDb->samplingFrequency();//hz
  mTimeBinWidth              = 1./mSamplingFrequency;//s
  //mTau                     = mSigma1;
  mTau                       =mElectronicsDb->tau();// s   HL, 8/31/99   
  mPadResponseFunctionSigma   =0;                  //HL,defined in member function
  //                                              for inner and outer setor.
  //
  mFractionSampled=1.0;//HL,8/31/99
  
//   PR(mDriftVelocity/(centimeter/(1.e-6*second)));
//   PR(mSamplingFrequency/MHz);
//   PR(mTimeBinWidth/nanosecond);
//   PR(mTau/nanosecond);

  // 
  // Set TSS parameters for signal generation
  //
   mChargeFractionOuter.push_back(1.95456);
   mChargeFractionOuter.push_back(1.88591); 
   mChargeFractionOuter.push_back(1.43655);
   mChargeFractionOuter.push_back(0.45415); 
   mChargeFractionOuter.push_back(0.0813517);
   mChargeFractionInner.push_back(1.86332); 
   mChargeFractionInner.push_back(1.62671);
   mChargeFractionInner.push_back(0.163125); 
   mChargeFractionInner.push_back(0.00515731);
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
   zoffset = wire_to_plane_coupling = xCentroidOfPad = yCentroidOfPad = 0;
   delx = gridMinusZ = sigma_x = localXDirectionCoupling = 0;
   dely = constant = localYDirectionCoupling = 0;
   timeOfSignal = chargeOfSignal = 0;
   t = tzero = K = 0;
   sigmaLoverTau = sigmaL/mTau;
   lambda = 0;
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
	cerr << "Aborting..." << endl;
	abort();
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
    mErrorFunctionTable.push_back(erf(1.0*cntr*mRangeOfTable/mNumberOfEntriesInTable));
    cntr++;
    
  }while(cntr < mNumberOfEntriesInTable);
 
}

double  StTrsParameterizedAnalogSignalGenerator::erf_fast(double argument) const
{
  

 
  
  int index = static_cast<int>(argument/mRangeOfTable*mNumberOfEntriesInTable);
 

  if(index>=mNumberOfEntriesInTable)return 1.0;
  if(index<=-mNumberOfEntriesInTable)return  -1.0;
 
  
  return   index>0? mErrorFunctionTable[index] : -mErrorFunctionTable[abs(index)];
  
}


void StTrsParameterizedAnalogSignalGenerator::inducedChargeOnPad(StTrsWireHistogram* wireHistogram)
{
    double offset=transformer.tBFromZ(0.)*mTimeBinWidth;
    
    double sigma_xpad2;
    double InOuterFactor=1.0075;
    double charge_fraction[5]; 
    //double mPadResponseFunctionSigma;
    //
    // This should probably be made a data member at some point!
//     StTpcCoordinateTransform transformer(mGeomDb, mSCDb, mElectronicsDb);
    PR(wireHistogram->minWire());
    PR(wireHistogram->maxWire());
    if(wireHistogram->minWire()<0) {
	cerr << "Wire Plane is empty" << endl;
	return;
    }
   
    for(int jj=wireHistogram->minWire(); jj<=wireHistogram->maxWire(); jj++) {

	// StTrsWireHistogram defines typedefs:
	// ABOVE: typedef vector<StTrsWireBinEntry> aTpcWire
	aTpcWire currentWire = wireHistogram->getWire(jj);
	aTpcWire::iterator iter;

	for(iter  = currentWire.begin();
	    iter != currentWire.end();
	    iter++) {
	    
	    // What is the location of the avalanche
	    // center of Pad that is being processed?
	    // the y coordinate is the position of the wire

//   	    PR(*iter);
	    
	  //  Info for segment:
	  // x =>  iter->position().x()
	  // y =>  iter->position().y()    // right now, wire position
	                        // I will modify this for the y position of the hit
	  // z =>  iter->position().z()
	  // dE => iter->numberOfElectrons()


	    // StTpcLocalCoordinate  xyCoord(iter->position());
	    StTpcLocalSectorCoordinate  xyCoord(iter->position(),12);//HL,9/10/99 setor12coordinate,mVolumeId is invalid,just set to 12 to indicate sector 12.
	    //
	    // THIS IS IMPORTANT TO REALIZE!!!
	    //
	    // xyCoord is now:
	    //   x position of hit
	    //   y position of wire  <----CAREFUL
	    //   z drift length
// 	    PR(iter->position());
//  	    PR(xyCoord);


// 	    cout << "**Transform2Raw" << endl;
	    transformer(xyCoord,mTpcRaw);
//  	    PR(mTpcRaw);
	    //  mCentralPad = mTpcRaw.pad();
	    mCentralRow = mTpcRaw.row();
           
	    //	    PR(mCentralRow);
//   	    PR(mDeltaRow);
	    //	    PR(mCentralPad);
 //  	    PR(mDeltaPad);
	  
	    // Calculate the row/pad limits
	    mRowLimits.first  = (mCentralRow > mDeltaRow) ?
		mCentralRow - mDeltaRow : mCentralRow;
	    mRowLimits.second = (mCentralRow < (mNumberOfRows-mDeltaRow)) ?
		mCentralRow + mDeltaRow : mCentralRow;

	    // Careful No inner/outer sector coupling!!
	    if(xyCoord.position().y() < mGeomDb->outerSectorEdge()) {
	      //	mRowLimits.second = min(mRowLimits.second, mNumberOfInnerRows); 
                     mRowLimits.second= mCentralRow;	
                     mRowLimits.first=  mCentralRow; //HL,2/20/00
	    }
	    else {
		mRowLimits.first  = max(mRowLimits.first, (mNumberOfInnerRows+1));
		mRowLimits.second = min(mRowLimits.second,(mNumberOfRows));
	    }
// 	    PR(mRowLimits.first);
// 	    PR(mRowLimits.second);

	    //	    mPadLimits.first  = (mCentralPad > mDeltaPad) ?
	    //	mCentralPad - mDeltaPad : mCentralPad; 

	    //
	    // STEP SIZEs OF THE FACTORIZARION FUNCTION OF PAD RESPONSE FUNCTION 
	    //IN LOCAL Y DIRECTION see tss note
            // coupling normailization from tss
	    // pad Dimenstions
	    //
	    // Loop over the cross coupled rows(irow)/pads(ipad)
           
	    //
	    for(int irow=mRowLimits.first; irow<=mRowLimits.second; irow++) {
		
              mTpcRaw.setRow(irow);
              transformer(mTpcRaw,xyCoord);
              xyCoord.position().setX(iter->position().x());  
              transformer(xyCoord,mTpcRaw);
              mCentralPad = mTpcRaw.pad();	

                mPadLimits.first  = (mCentralPad > mDeltaPad) ?
	    		mCentralPad - mDeltaPad : mCentralPad; 


                mPadLimits.second =
		    (mCentralPad < (mGeomDb->numberOfPadsAtRow(irow) - mDeltaPad)) ?
		    mCentralPad + mDeltaPad : mGeomDb->numberOfPadsAtRow(irow);
//  		PR(mPadLimits.first);
//  		PR(mPadLimits.second);
            
		


		for(int ipad=mPadLimits.first; ipad<=mPadLimits.second; ipad++) {
// 		    cout << " row: " << irow << " pad: " << ipad << endl;
#ifdef ST_SECTOR_BOUNDS_CHECK
		    if( (ipad<0 || ipad>mGeomDb->numberOfPadsAtRow(irow)) )
			continue;
#endif
                    
		    if(irow > mNumberOfInnerRows) {  // pad in Outer Sector
		      //  padWidth  = mGeomDb->outerSectorPadWidth();//cm, HL,8/31/99
		      //	padLength = mGeomDb->outerSectorPadLength();//cm
			mPadResponseFunctionSigma= mPadResponseFunctionSigmaOuter;
			//   zoffset=mGeomDb->outerSectorzOffSet();
			//	rowNormalization = 0.62;//old 2.04
                        wire_to_plane_coupling=0.512;
			charge_fraction[0]=mChargeFractionOuter[0];
                        charge_fraction[1]=mChargeFractionOuter[1]; 
                        charge_fraction[2]=mChargeFractionOuter[2];
                        charge_fraction[3]=mChargeFractionOuter[3];
                        charge_fraction[4]=mChargeFractionOuter[4];
		    }
		    else {
		      // padWidth  = mGeomDb->innerSectorPadWidth();//cm
		      //	padLength = mGeomDb->innerSectorPadLength();// cm
		         mPadResponseFunctionSigma= mPadResponseFunctionSigmaInner;  
			 //	 rowNormalization =0.285 ; //old 1.24
			 //   zoffset=mGeomDb->innerSectorzOffSet();
                        wire_to_plane_coupling=0.533*InOuterFactor;//HL,02/20/00
                        charge_fraction[0]=mChargeFractionInner[0];
                        charge_fraction[1]=mChargeFractionInner[1]; 
                        charge_fraction[2]=mChargeFractionInner[2];
                        charge_fraction[3]=mChargeFractionInner[3];
                        charge_fraction[4]=mChargeFractionInner[4];
                       
		    }
		    mTpcRaw.setPad(ipad);
		    mTpcRaw.setRow(irow);
                   
		    transformer(mTpcRaw,xyCoord);
// 		    PR(mTpcRaw);
// 		    PR(xyCoord);
		    // Integral limits for nearest pad
		    xCentroidOfPad = xyCoord.position().x();
		    yCentroidOfPad = xyCoord.position().y();
		    delx           = xCentroidOfPad-iter->position().x();
		    gridMinusZ     = iter->position().z(); // for new coordinates
		    sigma_x        = iter->sigmaT();
		    sigma_xpad2=sigma_x *sigma_x+ mPadResponseFunctionSigma*mPadResponseFunctionSigma; 
		     

                    
		    localXDirectionCoupling  =

                       mPadResponseFunctionSigma/sqrt(sigma_xpad2)*exp(-0.5*delx*delx/sigma_xpad2);   //sqrt(2pi) is absorbed in the local Y coupling
                                
                      
		  
		   

                        //  dely           = fabs(yCentroidOfPad-iter->position().y()); 
                    dely           = fabs(yCentroidOfPad-iter->position().y());            //        cout<<delx<<""<<dely<<" "<<yCentroidOfPad<<" "<<iter->position().y()<<endl;
                    
		    constant       =sigma_x*M_SQRT2 ;
		     		     

		    localYDirectionCoupling =

		      0.5/twopi*((charge_fraction[0]-charge_fraction[1])*(erf_fast((dely+mYb[0])/constant)-
						     erf_fast((dely-mYb[0])/constant))+
					    (charge_fraction[1]-charge_fraction[2])*(erf_fast((dely+mYb[1])/constant)-
						     erf_fast((dely-mYb[1])/constant))+
					    (charge_fraction[2]-charge_fraction[3])*(erf_fast((dely+mYb[2])/constant)-
						     erf_fast((dely-mYb[2])/constant))+
                                            (charge_fraction[3]-charge_fraction[4])*(erf_fast((dely+mYb[3])/constant)-
						     erf_fast((dely-mYb[3])/constant))+
					    (charge_fraction[4])                   *(erf_fast((dely+mYb[4])/constant)-
					             erf_fast((dely-mYb[4])/constant))
                                            );
		       

		   
		    chargeOfSignal=localYDirectionCoupling*localXDirectionCoupling*wire_to_plane_coupling; 
                   
                    if(chargeOfSignal<0.0) { chargeOfSignal=0.0; continue; }
		    
//    		    PR(chargeOfSignal);
//    		    PR(iter->numberOfElectrons());
		    
		    chargeOfSignal *= iter->numberOfElectrons();
		    
 		    
		    //
		    // This should really be from the Coordinate transform!
		    // otherwise code has to be changed twice!
		    // time = (localz + zoffset)/v_drift + tZero
		    // The z position already has a z offset, it was put in
		    // the fast charge transporter.
		    timeOfSignal =
			(iter->position().z())/mDriftVelocity+offset;
		    	
		  
		    // OH-OH OFFSET (replaced!...)
		     //	     timeOfSignal =
		     //  	iter->position().z()/mSCDb->driftVelocity();

	     
		     
		    // Check the threshold before you
		    // make and store an analog signal
		    if(!chargeOfSignal) continue;
		   
		    StTrsAnalogSignal padSignal(timeOfSignal, chargeOfSignal);

		    //
		    // DIAGNOSTIC: Print out all the signals on the pad
// 		    cout << "padSignal "
// 			 << padSignal.time()/nanosecond << " ns\t"
// 			 << padSignal.amplitude() << '\t'
// 			 << irow << "," << ipad <<endl;
		    mSector->addEntry(irow,ipad,padSignal);
		    
		} // pad limits
	       	       
         
	    } // row limits
	   
	 	    		   
	} // (iterator) Loop over all wires

    } // (jj)Loop over the wires of the Histogram
    

}

double StTrsParameterizedAnalogSignalGenerator::realShaperResponse(double tbin, StTrsAnalogSignal& sig)
{
    //
    // Take the value of the function at the mid-point of the
    // time bin, and multiply it by the width of the bin!
    // charge = F(t) dt
    double value=0.0;
    
    // Use mDriftVelocity...
    //double driftVelocity = mSCDb->driftVelocity();
    
    // use mTau
    //double tau = mSigma1;
    
    // Oh darn! Why do we need gas parmeters here...it is because
    // we convolute the response of the electronics with the diffusion
    // DON'T DO THAT!!!!
    //double sigmaL = .05*centimeter/sqrt(centimeter);
    // double t = mTimeBinWidth*(tbin+.5);//started from the center of time bin
    //double t = mTimeBinWidth*(tbin);//  started from the edge of time bin ,HL
    t = tbin; //passed it directly.
    // Remember centroid is at 2tau+10ns
    // should be calculated via the extremem...but this
    // can only be found numerically...There is a routine
    // that will do this, but I won't incorporate it yet.
    // For now use the approximation:
    //  double tzero = sig.time() - 2.*mTau+10.*nanosecond;
    tzero = sig.time() ;// Hui Long,8/26/99
    
    //double K = sigmaL*sqrt(sig.time())/(tau*sqrt(driftVelocity));
    K = sigmaLoverTau*sqrt((tzero- mTimeShiftOfSignalCentroid)/mDriftVelocity);//retrieve the real drift length,HL,8/31/99
    //UNITS:   sigmaL:  cm/sqrt(cm)
    //         mTau:    second
    //         tzero:  second
    //          mTimeBinwidth : second
    //         <mDriftVelocity  :cm/second

    lambda =  (tzero - t)/(K*mTau) + K;
    double lambdasqr = lambda*lambda;
    // Corrected From SN197
    value = .5/(mTau)*sqr(K)*exp(K*(lambda-.5*K))*
	( .5*(1+lambdasqr)*(1-erf_fast(lambda/M_SQRT2)) -
	  
	  lambda/sqrtTwoPi*exp(-.5*lambdasqr));
    
    value=value*mTimeBinWidth;
    
    
    value *= mFractionSampled*mGain*sig.amplitude();
    

    // Amount of Charge in the bin
//     PR(sig.amplitude());
//     PR(t/nanosecond);
//     PR(tzero/nanosecond);
//     PR((tzero-t)/nanosecond);
//     PR(value);
    //    cout << "gain/volt " << (s.amplitude()*mGain/(.001*volt)) << " mV" << endl;
    return value;

}

// double StTrsParameterizedAnalogSignalGenerator::signalSampler(double t, StTrsAnalogSignal& sig)
// {
//     //
//     // This is where the function for the Signal Sampling is selected
//     // Add a function that returns the amplitude of a signal at
//     // a time 't' given the position in time and amplitude of all
//     // the other signals (contained in the StTrsAnalogSignal 'sig'
  

//   return realShaperResponse(t,sig);
  
// }


void StTrsParameterizedAnalogSignalGenerator::sampleAnalogSignal()
{
  cout << "StTrsParameterizedAnalogSignalGenerator::sampleAnalogSignal()" << endl;
    
    // operates on mSector (an StTrsSector)

    // I have the centroid IN TIME (make sure!!!!) of each hit!
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    vector<StTrsAnalogSignal> continuousAnalogTimeSequence;
    vector<StTrsAnalogSignal>::iterator lowerBound;
#else
    vector<StTrsAnalogSignal, allocator<StTrsAnalogSignal> > continuousAnalogTimeSequence;
    vector<StTrsAnalogSignal, allocator<StTrsAnalogSignal> >::iterator lowerBound;
#endif
    //double freq = mElectronicsDb->samplingFrequency();
    //PR(freq);
    double timeBinUpperLimit = 6.;
    double timeBinLowerLimit = 3.;

//     double sortTime = 0;
//     time_t sortBegin;
//     time_t sortEnd;
//     double timeBinTime = 0;
//     time_t timeBinBegin;
//     time_t timeBinEnd;
//     double seqIterTime = 0;
//     time_t seqIterBegin;
//     time_t seqIterEnd;
//     double samplerTime = 0;
//     time_t samplerBegin;
//     time_t samplerEnd;
    
    for(int irow=1; irow<=mSector->numberOfRows(); irow++) {
	for(int ipad=1; ipad<=mSector->numberOfPadsInRow(irow); ipad++) {
	    continuousAnalogTimeSequence = mSector->timeBinsOfRowAndPad(irow,ipad);

	    mDiscreteAnalogTimeSequence.clear();

	    // Make sure it is not empty
           
	    if(!continuousAnalogTimeSequence.size()) continue; 
// 	    sortBegin = time(0);
	    sort(continuousAnalogTimeSequence.begin(),continuousAnalogTimeSequence.end(), StTrsAnalogSignalComparator());
// 	    sortEnd = time(0);
// 	    sortTime += difftime(sortEnd, sortBegin);
	    double maxTime = continuousAnalogTimeSequence.back().time();

// 	    PR(maxTime/nanosecond);

// 	    for(mTimeSequenceIterator  = continuousAnalogTimeSequence.begin();
// 		mTimeSequenceIterator != continuousAnalogTimeSequence.end();
// 		mTimeSequenceIterator ++) {
                    
// //   		    PR(mTimeSequenceIterator->time());
// //   		    PR(mTimeShiftOfSignalCentroid);
// 	      //		    double tmpTime =
// 	      //	      mTimeSequenceIterator->time() +mTimeShiftOfSignalCentroid;//shift was already done before the input
// 		  	    double tmpTime =
// 			      mTimeSequenceIterator->time();//HL,9/8/99
// 			    //	    cout<<tmpTime<<" time after shifting offset"<<endl;
// 		    mTimeSequenceIterator->setTime(tmpTime);
// //   		PR(mTimeSequenceIterator->time());
// //   		PR(mTimeSequenceIterator->time()/nanosecond);
// 	    }
//  	    cout << "row/pad " << irow << '/' << ipad << ' ' << continuousAnalogTimeSequence.size() << endl;
	    
	    // Calculate the analog signal at the centroid of the time bin
	    // Loop over all the time bins:

	    double timeBinT = 0;
// 	    timeBinBegin = time(0);
	    lowerBound = continuousAnalogTimeSequence.begin();
	    for(int itbin=0; itbin<mGeomDb->numberOfTimeBuckets(); itbin++) {
		timeBinT = itbin*mTimeBinWidth;

// 		int d;
// 		cin >> d;
// 		cout << itbin << ", ";
//  		PR(timeBinT/nanosecond);
// 		PR(maxTime/nanosecond);
		double pulseHeight = 0.0;
		
		if (timeBinT-maxTime < timeBinUpperLimit*mTimeBinWidth &&
		    lowerBound->time()-timeBinT < timeBinLowerLimit*mTimeBinWidth) {
		    //Not gone beyond max time & 
		    //reached at least the lowerBound time.
// 		    seqIterBegin = time(0);
		    double signalTime = 0;
		    for(mTimeSequenceIterator =lowerBound;
			mTimeSequenceIterator!=continuousAnalogTimeSequence.end();
			mTimeSequenceIterator++) {
			signalTime = mTimeSequenceIterator->time();

//  			PR(signalTime/nanosecond);
			
			//
			// The current time bin will be filled with
			// charge from any signal that is within
			// timeBinCut time bins.  This should be a settable
			// parameter.
			//
			
			if( signalTime-timeBinT> timeBinLowerLimit*mTimeBinWidth) break;

			if( timeBinT-signalTime> timeBinUpperLimit*mTimeBinWidth) {
			    lowerBound++;
			    continue;
			}
// 			samplerBegin = time(0);
			pulseHeight +=
			    signalSampler(timeBinT, *mTimeSequenceIterator);
// 			samplerEnd = time(0);
// 			samplerTime += difftime(samplerEnd,samplerBegin);

// 			cout << " tb " << itbin << " "
// 			     << signalTime/nanosecond << " "
// 			     << (*mTimeSequenceIterator) << " "
// 			     << "pulseHeight "<< pulseHeight << endl;
		    }
// 		    seqIterEnd = time(0);
// 		    seqIterTime += difftime(seqIterEnd, seqIterBegin);
		//
		// DIAGNOSTIC
		// Print out the pulse height in each time bin
		    
		    //cout << itbin << " pulse Height: " << pulseHeight << '\t' << (pulseHeight/(.001*volt)) << " mV" << endl;

		} // if itbin is beyond maxTime

		if (!mAddNoise && timeBinT-maxTime > timeBinUpperLimit*mTimeBinWidth) break;
		//
		// Add noise here 
		//
		if(mAddNoise) {
		    if (!mAddNoiseUnderSignalOnly)
			pulseHeight += generateNoise(); // noise everywhere;
		    if(mAddNoiseUnderSignalOnly && pulseHeight){
			pulseHeight += generateNoise(); //noise only under signal;
		    }
			
		}

		//Do not store analog Signal if it is not above a
		// minimal threshold (should read value from database) rowN
  		if(pulseHeight < mSignalThreshold) continue;

		mElectronicSignal.setTime(itbin);
		mElectronicSignal.setAmplitude(pulseHeight);
		mDiscreteAnalogTimeSequence.push_back(mElectronicSignal);
		

	    } // loop over time bins
// 	    timeBinEnd = time(0);
// 	    timeBinTime += difftime(timeBinEnd, timeBinBegin);
	    
	    mSector->assignTimeBins(irow,ipad,mDiscreteAnalogTimeSequence);
	    
	} // loop over pads
	
    } // loop over rows
    
//     cout << "Time to sort Analog Sequence: " << sortTime << " sec\n\n";
//     cout << "Time to loop over Time bins : " << timeBinTime << " sec\n";
//     cout << "\tSeq Iter T : " << seqIterTime << " sec\n";
//     cout << "\t Sampler T : " << samplerTime << " sec\n";
}

