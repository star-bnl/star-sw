/***************************************************************************
 *
 * $Id: StTrsParameterizedAnalogSignalGenerator.cc,v 1.6 1999/11/05 22:18:17 calderon Exp $
 *
 * Author: Hui Long
 ***************************************************************************
 *
 * Description:tss alogrithm for signal generator in the trs
 *
 ***************************************************************************
 *
 * $Log: StTrsParameterizedAnalogSignalGenerator.cc,v $
 * Revision 1.6  1999/11/05 22:18:17  calderon
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

#include "SystemOfUnits.h"
#include "PhysicalConstants.h"
#include "StCoordinates.hh"

#include "StTrsParameterizedAnalogSignalGenerator.hh"


static const double sigmaL = .037*centimeter/sqrt(centimeter);
static const double sigmaT = .0633*centimeter/sqrt(centimeter);

StTrsAnalogSignalGenerator* StTrsParameterizedAnalogSignalGenerator::mInstance = 0; 
// static data member

//StTrsParameterizedAnalogSignalGenerator::StTrsParameterizedAnalogSignalGenerator() {/* nopt */}

StTrsParameterizedAnalogSignalGenerator::StTrsParameterizedAnalogSignalGenerator(StTpcGeometry* geo, StTpcSlowControl* sc, StTpcElectronics* el, StTrsSector* sec)
    : StTrsAnalogSignalGenerator(geo, sc, el, sec)
{

  //
  // Define here instead of calculating...
  //
  
  mDriftVelocity             = mSCDb->driftVelocity();
  mSamplingFrequency         = mElectronicsDb->samplingFrequency();//hz
  mTimeBinWidth              = 1./mSamplingFrequency;//s
  //mTau                     = mSigma1;
  mTau                       =mElectronicsDb->tau();// s   HL, 8/31/99   
  mPadRespondFunctionSigma   =0;                  //HL,defined in member function
  //                                              for inner and outer setor.
  //
 
//   PR(mDriftVelocity/(centimeter/(1.e-6*second)));
//   PR(mSamplingFrequency/MHz);
//   PR(mTimeBinWidth/nanosecond);
//   PR(mTau/nanosecond);

  // 
  // Set TSS parameters for signal generation
  //
   mChargeFraction.push_back(1.0);
   mChargeFraction.push_back(0.7); 
   mChargeFraction.push_back(0.3);
   mChargeFraction.push_back(0.05); 
   mNumberOfEntriesInTable=4000;
   mRangeOfTable=4.0;
   errorFunctionTableBuilder();
 
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
    mErrorFuctionTable.push_back(erf(1.0*cntr*mRangeOfTable/mNumberOfEntriesInTable));
    cntr++;
    
  }while(cntr < mNumberOfEntriesInTable);
 
}

double  StTrsParameterizedAnalogSignalGenerator::erf_fast(double argument) const
{
  

 
  
  int index         =argument/mRangeOfTable*mNumberOfEntriesInTable;
 

  if(index>=mNumberOfEntriesInTable)return 1.0;
  if(index<=-mNumberOfEntriesInTable)return  -1.0;
 
  
  return   index>0? mErrorFuctionTable[index] : -mErrorFuctionTable[abs(index)];
  
}


void StTrsParameterizedAnalogSignalGenerator::inducedChargeOnPad(StTrsWireHistogram* wireHistogram)
{
    //
    // This should probably be made a data member at some point!
    StTpcCoordinateTransform transformer(mGeomDb, mSCDb, mElectronicsDb);
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

  	    //PR(*iter);
	    
	  //  Info for segment:
	  // x =>  iter->position().x()
	  // y =>  iter->position().y()    // right now, wire position
	                        // I will modify this for the y position of the hit
	  // z =>  iter->position().z()
	  // dE => iter->numberOfElectrons()


	    StTpcPadCoordinate    tpcRaw;
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
	    transformer(xyCoord,tpcRaw);
//  	    PR(tpcRaw);
	    int centralPad = tpcRaw.pad();
	    int centralRow = tpcRaw.row();
           
	    //	    PR(centralRow);
//   	    PR(mDeltaRow);
	    //	    PR(centralPad);
 //  	    PR(mDeltaPad);
	  
	    // Calculate the row/pad limits
	    mRowLimits.first  = (centralRow > mDeltaRow) ?
		centralRow - mDeltaRow : centralRow;
	    mRowLimits.second = (centralRow < (mGeomDb->numberOfRows()-mDeltaRow)) ?
		centralRow + mDeltaRow : centralRow;

	    // Careful No inner/outer sector coupling!!
	    if(xyCoord.position().y() < mGeomDb->outerSectorEdge()) {
		mRowLimits.second = min(mRowLimits.second, mGeomDb->numberOfInnerRows());
	    }
	    else {
		mRowLimits.first  = max(mRowLimits.first, (mGeomDb->numberOfInnerRows()+1));
		mRowLimits.second = min(mRowLimits.second,(mGeomDb->numberOfRows()));
	    }
// 	    PR(mRowLimits.first);
// 	    PR(mRowLimits.second);

	    mPadLimits.first  = (centralPad > mDeltaPad) ?
		centralPad - mDeltaPad : centralPad; 

	    //
	    // STEP SIZEs OF THE FACTORIZARION FUNCTION OF PAD RESPONSE FUNCTION 
	    //IN LOCAL Y DIRECTION see tss note
	    double yb1, yb2, yb3, yb4;

            // coupling normailization from tss
	    double rowNormalization;   // 
	    
	    // pad Dimenstions
            double padWidth, padLength;	    
	    //
	    // Loop over the cross coupled rows(irow)/pads(ipad)
           
	    //
	    for(int irow=mRowLimits.first; irow<=mRowLimits.second; irow++) {
		mPadLimits.second =
		    (centralPad < (mGeomDb->numberOfPadsAtRow(irow) - mDeltaPad)) ?
		    centralPad + mDeltaPad : mGeomDb->numberOfPadsAtRow(irow);
//  		PR(mPadLimits.first);
//  		PR(mPadLimits.second);
            
		


		for(int ipad=mPadLimits.first; ipad<=mPadLimits.second; ipad++) {
// 		    cout << " row: " << irow << " pad: " << ipad << endl;
#ifdef ST_SECTOR_BOUNDS_CHECK
		    if( !(ipad>0 && ipad<mGeomDb->numberOfPadsAtRow(irow)) )
			continue;
#endif
		    double padWidth, padLength;
		    double rowNormalization;   // coupling normailization from tss                
                    double zoffset;
                    double wire_to_plane_coupling;
		    if(irow > mGeomDb->numberOfInnerRows()) {  // pad in Outer Sector
		      padWidth  = mGeomDb->outerSectorPadWidth();//cm, HL,8/31/99
			padLength = mGeomDb->outerSectorPadLength();//cm
			mPadRespondFunctionSigma= 0.395       ;             //cm
                        zoffset=mGeomDb->outerSectorzOffSet();
			rowNormalization = 2.04;
                        wire_to_plane_coupling=0.512;
			yb1        =0.6;
			yb2        =1.0;
			yb3        =1.4;
			yb4        =1.8;
		    }
		    else {
		      padWidth  = mGeomDb->innerSectorPadWidth();//cm
			padLength = mGeomDb->innerSectorPadLength();// cm
		         mPadRespondFunctionSigma= 0.25   ;   //cm
			rowNormalization = 1.24; 
                        zoffset=mGeomDb->innerSectorzOffSet();
                        wire_to_plane_coupling=0.533;
                        yb1       =0.2;
                        yb2       =0.6;
			yb3       =1.0;
                        yb4       =1.4;
		    }
		    tpcRaw.setPad(ipad);
		    tpcRaw.setRow(irow);
                   
		    transformer(tpcRaw,xyCoord);
// 		    PR(tpcRaw);
// 		    PR(xyCoord);
		    // Integral limits for nearest pad
		    double xCentroidOfPad = xyCoord.position().x();
		    double yCentroidOfPad = xyCoord.position().y();
                    double delx           = xCentroidOfPad-iter->position().x();
		    double sigma_x        = sigmaT*sqrt(mGeomDb->frischGrid()-iter->position().z());
                     sigma_x=sqrt(sqr(sigma_x )+sqr( mPadRespondFunctionSigma)); 
		     

                     

		    double localXDirectionCoupling  =
		      (padWidth/sigma_x)*exp(-0.5*delx*delx/sigma_x/sigma_x)/M_SQRT2/2.0*M_2_PI;
                      
		  
		   
                    double dely           = fabs(yCentroidOfPad-iter->position().y()); 
		    double constant       =sigma_x*M_SQRT2 ;
		     		     

		    double localYDirectionCoupling =
		      0.5/rowNormalization*((mChargeFraction[0]-mChargeFraction[1])*(erf_fast((dely+yb1)/constant)-
						     erf_fast((dely-yb1)/constant))+
					    (mChargeFraction[1]-mChargeFraction[2])*(erf_fast((dely+yb2)/constant)-
						     erf_fast((dely-yb2)/constant))+
					    (mChargeFraction[2]-mChargeFraction[3])*(erf_fast((dely+yb3)/constant)-
						     erf_fast((dely-yb3)/constant))+
					    (mChargeFraction[3])                   *(erf_fast((dely+yb4)/constant)-
					             erf_fast((dely-yb4)/constant))
                                           );
		    //		    cout<<ipad <<" pad   "<<irow<<" row  "<<"  x="<<iter->position().x()<<"  y="<<iter->position().y()<<"   dely=   "<<dely<<"  "<<"y couple=  " <<localYDirectionCoupling<<"  constant   "<<constant<<" sigmax= "<<sigma_x<<endl;
		      
		    double chargeOfSignal=localYDirectionCoupling*localXDirectionCoupling*wire_to_plane_coupling;
                    if(chargeOfSignal<0.0)chargeOfSignal=0.0;
                   

                  
//    		    PR(chargeOfSignal);
//    		    PR(iter->numberOfElectrons());
		    
		    chargeOfSignal *= iter->numberOfElectrons();
		   
//    		    PR(chargeOfSignal);
		    //
		    // This should really be from the Coordinate transform!
		    // otherwise code has to be changed twice!
		    //
		   double timeOfSignal =
		      			(mGeomDb->frischGrid()-iter->position().z()-zoffset +mElectronicsDb->tZero()*mSCDb->driftVelocity())
		    	/mSCDb->driftVelocity();
		 
		    // OH-OH OFFSET (replaced!...)
		     //	     timeOfSignal =
		     //  	iter->position().z()/mSCDb->driftVelocity();

	     
		     
		    // Check the threshold before you
		    // make and store an analog signal
		    //if() continue;
		   
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
    double t = mTimeBinWidth*(tbin);//  started from the edge of time bin ,HL
    // Remember centroid is at 2tau+10ns
    // should be calculated via the extremem...but this
    // can only be found numerically...There is a routine
    // that will do this, but I won't incorporate it yet.
    // For now use the approximation:
    //  double tzero = sig.time() - 2.*mTau+10.*nanosecond;
     double tzero = sig.time() ;// Hui Long,8/26/99
    
    //double K = sigmaL*sqrt(sig.time())/(tau*sqrt(driftVelocity));
     double K = sigmaL/mTau*sqrt((tzero- mTimeShiftOfSignalCentroid)/mDriftVelocity);//retrieve the real drift length,HL,8/31/99
    //UNITS:   sigmaL:  cm/sqrt(cm)
    //         mTau:    second
    //         tzero:  second
    //          mTimeBinwidth : second
    //         <mDriftVelocity  :cm/second
    //double lambda =  (sig.time() - t)/(K*tau) + K;
    double lambda =  (tzero - t)/(K*mTau) + K;
   
    // Corrected From SN197
    value = 1./(2.*mTau)*sqr(K)*exp(K*(lambda-.5*K))*
	( .5*(1+sqr(lambda))*(1-erf_fast(lambda/M_SQRT2)) -

	  lambda/(sqrt(2*M_PI))*exp(-sqr(lambda)/2));
   
    value=value* mTimeBinWidth;
   
    mFractionSampled=1.0;//HL,8/31/99
 
    value *= mFractionSampled*mGain*sig.amplitude();
  
   
    // Amount of Charge in the bin
  
   
    //  cout<<value<<endl;
 
    //    cout << "gain/volt " << (s.amplitude()*mGain/(.001*volt)) << " mV" << endl;
    return value;

}

double StTrsParameterizedAnalogSignalGenerator::signalSampler(double t, StTrsAnalogSignal& sig)
{
    //
    // This is where the function for the Signal Sampling is selected
    // Add a function that returns the amplitude of a signal at
    // a time 't' given the position in time and amplitude of all
    // the other signals (contained in the StTrsAnalogSignal 'sig'
  

  return realShaperResponse(t,sig);
  
}


void StTrsParameterizedAnalogSignalGenerator::sampleAnalogSignal()
{
  cout << "StTrsParameterizedAnalogSignalGenerator::sampleAnalogSignal()" << endl;
    
    // operates on mSector (an StTrsSector)

    // I have the centroid IN TIME (make sure!!!!) of each hit!
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    vector<StTrsAnalogSignal> continuousAnalogTimeSequence;
#else
    vector<StTrsAnalogSignal, allocator<StTrsAnalogSignal> > continuousAnalogTimeSequence;
#endif
    //double freq = mElectronicsDb->samplingFrequency();
    //PR(freq);
    for(int irow=1; irow<=mSector->numberOfRows(); irow++) {
	for(int ipad=1; ipad<=mSector->numberOfPadsInRow(irow); ipad++) {
	 
	    continuousAnalogTimeSequence = mSector->timeBinsOfRowAndPad(irow,ipad);

	    mDiscreteAnalogTimeSequence.clear();

	    // Make sure it is not empty
           
	    if(!continuousAnalogTimeSequence.size()) continue; 
           
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
//
//   	    cout << "How many signals? " << endl;
//   	    PR(continuousAnalogTimeSequence.size());
//   	    for(int bbb=0; bbb<continuousAnalogTimeSequence.size(); bbb++)
//   		cout << " " << bbb << " " << continuousAnalogTimeSequence[bbb] << endl;
//   	    cout << "row: " << irow << " pad: " << ipad << " timeBin: " << endl;

	    double timeBinT;
	    for(int itbin=0; itbin<mGeomDb->numberOfTimeBuckets(); itbin++) {   	//	cout << itbin << ", ";
		timeBinT = itbin*mTimeBinWidth;
//  		PR(timeBinT);
// 		PR(timeBinT/nanosecond);
		
		double pulseHeight = 0.0;
		for(mTimeSequenceIterator =continuousAnalogTimeSequence.begin();
		    mTimeSequenceIterator!=continuousAnalogTimeSequence.end();
		    mTimeSequenceIterator++) {

		    //
		    // The current time bin will be filled with
		    // charge from any signal that is within
		    // 10 time bins.  This should be a settable
		    // parameter.
		    //
		  if( timeBinT-mTimeSequenceIterator->time() < -3.*mTimeBinWidth ||
		      timeBinT-mTimeSequenceIterator->time() >  6.*mTimeBinWidth)
		      continue; 
                        
//    		    cout << " tb " << itbin << " "
// 			 << mTimeSequenceIterator->time()/nanosecond << " " << (*mTimeSequenceIterator) << endl;
		  pulseHeight +=
		    signalSampler(itbin, *mTimeSequenceIterator);
		  //	  cout<<itbin<<"=bin "<< pulseHeight<<"==signal"<<endl;
		         	
		}
		// int iii;
               
		//	 cout<< pulseHeight<<"==signal  final"<<endl;
		 //  cin>>iii;
		//
		// DIAGNOSTIC
		// Print out the pulse height in each time bin
//   		cout << itbin << " pulse Height: " << pulseHeight << '\t' << (pulseHeight/(.001*volt)) << " mV" << endl;

		//
		// Add noise here 
		//
		// : Everywhere
		if(!mAddNoiseUnderSignalOnly && mAddNoise) {
		  pulseHeight += generateNoise(); // noise;
		}

		//Do not store analog Signal if it is not above a
		// minimal threshold (should read value from database) rowN
// 	        *********************************************************
  		//if(pulseHeight < mSignalThreshold) continue;
// 	        *********************************************************
		//
		// : Only Under Signal
		if(mAddNoiseUnderSignalOnly && mAddNoise) {
		    //double noise = generateNoise();
		    pulseHeight += generateNoise(); //noise;
		}
                
	      
	    	

		mElectronicSignal.setTime(itbin);


	      
	      
		mElectronicSignal.setAmplitude(pulseHeight);
               
		 	
                
//   		if(mElectronicSignal.amplitude() !=0 ) {
// 		if(irow == 14 && (ipad == 14 || ipad == 52)) {
//  		    cout << "mElectronicSignal " << mElectronicSignal
//  			 << '\t' << (mElectronicSignal.amplitude()/(.001*volt)) << endl;
//  		}
		mDiscreteAnalogTimeSequence.push_back(mElectronicSignal);

	    } // loop over time bins


           
           
	    mSector->assignTimeBins(irow,ipad,mDiscreteAnalogTimeSequence);
	    
	} // loop over pads

    } // loop over rows

}

