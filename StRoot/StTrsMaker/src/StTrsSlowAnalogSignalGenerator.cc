/***************************************************************************
 *
 * $Id: StTrsSlowAnalogSignalGenerator.cc,v 1.2 1998/11/13 21:32:16 lasiuk Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************
 *
 * $Log: StTrsSlowAnalogSignalGenerator.cc,v $
 * Revision 1.2  1998/11/13 21:32:16  lasiuk
 * adjust charge generated
 *
 * merge operations for speed up (after profiler0
 *
 * Revision 1.10  1999/02/15 03:38:16  lasiuk
 * protection if min()<0
 *
 * Revision 1.9  1999/02/14 20:46:09  lasiuk
 * debug info
 *
 * Revision 1.8  1999/02/12 01:27:18  lasiuk
 * Limit Debug output
 *
 * Revision 1.7  1999/02/10 20:55:16  lasiuk
 * Feb 10,1999
 *
 * Revision 1.6  1999/01/22 23:38:28  lasiuk
 * set defaults for signal sampling and induced charge
 *
 * Revision 1.5  1999/01/18 21:01:30  lasiuk
 * use fractionSampled(); enumerated types for function selection
 *
 * Revision 1.4  1999/01/18 10:21:10  lasiuk
 * use integral to deposit total charge in time bin
 *
 * Revision 1.3  1998/11/16 14:48:19  lasiuk
 * use wire index instead of wireNumber
 * comment signal threshold
 * add deltaResponse()
 * Revision 1.2  1998/11/13 21:32:16  lasiuk
 * adjust charge generated
 * Put Brian trs versin into StRoot
 *
 * Revision 1.5  1998/11/08 17:30:26  lasiuk
 * allocators for SUN
 *

 * Revision 1.4  1998/11/04 18:47:12  lasiuk
 * signal sampler machinery
 * Revision 1.3  1998/10/22 14:58:27  lasiuk
 * image charge returns double and uses PRF integral
 *
 * Revision 1.2  1998/10/22 00:24:27  lasiuk
 * Oct 22
 *
{/* nopt */}
    : StTrsAnalogSignalGenerator(geo, sc, el, sec)
{
//static const double tau1              = mSigma1;
//static const double tau2              = 2.*mSigma1;
//static const double asymGausAppFactor = (M_2_SQRTPI/M_SQRT2)/(tau1+tau2);
  mAsymGausApproxFactor = (M_2_SQRTPI/M_SQRT2)/(mTau1+mTau2);
  mAsymGausUnRestFactor = (M_2_SQRTPI/M_SQRT2)/(mSigma1+mSigma2);

//   PR(mDriftVelocity/(centimeter/(1.e-6*second)));
//   PR(mSamplingFrequency/MHz);
//   PR(mTimeBinWidth/nanosecond);
//   PR(mTau/nanosecond);
//   PR(mTau1/nanosecond);
//   PR(mTau2/nanosecond);
//   PR(mSigma1/nanosecond);
//   PR(mSigma2/nanosecond);
//   PR(mSymGausApproxFactor);
//   PR(mAsymGausApproxFactor);
//   PR(mAsymGausUnRestFactor);
}

StTrsSlowAnalogSignalGenerator::~StTrsSlowAnalogSignalGenerator() {/* missing */}

StTrsAnalogSignalGenerator*
StTrsSlowAnalogSignalGenerator::instance()
{
    if (!mInstance) {
    double L = 4*millimeter;
    //double L = (yo < mGeomDb->firstOuterSectorPadRow()) ?
    //   mGeomDb->innerSectorAnodeWirePadPlaneSeparation() :
    //   mGeomDb->outerSectorAnodeWirePadPlaneSeparation();
    cout << "endo charge " << endl;
	cerr << "Cannot create Instance" << endl;
	cerr << "Exitting..." << endl;
	exit(1);
#endif
    }
    return mInstance;
    PR(L);

StTrsAnalogSignalGenerator*
StTrsSlowAnalogSignalGenerator::instance(StTpcGeometry* geo, StTpcSlowControl* sc, StTpcElectronics* el, StTrsSector* sec)
{
    if (!mInstance) {
	mInstance = new StTrsSlowAnalogSignalGenerator(geo, sc, el, sec);
    }
    // else do nothing
    return mInstance;
}

double StTrsSlowAnalogSignalGenerator::endoChargeIntegral(double xo, double yo, double xl, double xu, double yl, double yu)
{
#ifndef ST_NO_NAMESPACES
    using namespace units;
#endif
    double L = (yo < mGeomDb->lastInnerSectorAnodeWire()) ?
    cout << "got to image charge " << endl;
	mGeomDb->outerSectorAnodeWirePadPlaneSeparation();
    //PR(L);
    double t3 = 1/L;
    double t17 = -atan(exp(pi*(xu-xo)*t3/2))+atan(exp(pi*(xl-xo)*t3/2));
    double t19 = pi*pi;
    double t20 = 1/t19;
    double t29 = 
	-4.0*atan(exp(-pi*(-yu+yo)*t3/2))*t17*t20
	+4.0*atan(exp(-pi*(-yl+yo)*t3/2))*t17*t20;

    return t29;
}

//double StTrsSlowAnalogSignalGenerator::gattiChargeIntegral(double xo, double yo, double xl, double xu, double yl, double yu)
double StTrsSlowAnalogSignalGenerator::imageChargeIntegral(double xo, double yo, double xl, double xu, double yl, double yu)
{
#ifndef ST_NO_NAMESPACES
    using namespace units;
#endif
//     cout << "StTrsSlowAnalogSignalGenerator::imageChargeIntegral" << endl;
    // Arguments
    //  double xo, double yo,  //charge location 
    //	double xp, double yp,  //pad centroid
    //	double xl, double xu,  //integration limits
    //	double yl, double yu   // "
    
	//
	// Integrate a charge distribution sigma:
	//
	//                                          q d
	//            sigma := 1/2 ----------------------------------
	//                                     2           2    2 3/2
	//                         Pi ((x - xo)  + (y - yo)  + d )
	//
	// position of charge        (xo,yo)
	// wire/pad-plane separation  d
	//
	//              yu   xu
	//             /    /
	//            |    |                       q d
	//      G :=  |    |   1/2 ---------------------------------- dx dy
	//            |    |                   2           2    2 3/2
	//           /    /        Pi ((x - xo)  + (y - yo)  + d )
	//           yl   xl
	//

    
	double q = 1;
	double d = 4*millimeter;

	//double d = (yo < mGeomDb->firstOuterSectorPadRow()) ?
	//   mGeomDb->innerSectorAnodeWirePadPlaneSeparation() :
	//   mGeomDb->outerSectorAnodeWirePadPlaneSeparation();
	double t1 = xu*xu;
	double t2 = xo*xo;
	double t3 = xo*xu;
	double t4 = t1+t2-2.0*t3;
	double t5 = yo-yu;
	double t8 = pow(-xu+xo,2.0);
	double t9 = sqrt(t8);
	double t10 = 1/t9;
	double t11 = yu*yu;
	double t12 = yo*yu;
	double t13 = yo*yo;
	double t15 = 5*sqrt(t1+t2+t11+(d*d)/100.-2.*t12-2.*t3+t13);
	double t19 = atan((50./d)*t4*t5*t10/t15);
	double t22 = pow(-xl+xo,2.0);
	double t23 = sqrt(t22);
	double t55 = atan((50./d)*t4*t46*t10/t51);
	double t62 = 5*sqrt(t27+t2+t48+(d*d)/100-2.*t49-2.*t28+t13);
	double t66 = atan((50./d)*t29*t46*t31/t62);
    return endoChargeIntegral(xo,yo,xl,xu,yl,yu);
//     return gattiChargeIntegral(xo,yo,xl,xu,yl,yu);
//     return imageChargeIntegral(xo,yo,xl,xu,yl,yu);
 	cout << "Wire: " << jj << endl;
	    cerr << "Default Function Selected. ERROR!" << endl;
	    exit(0);

    for(int jj=wireHistogram->min(); jj<wireHistogram->max(); jj++) {
 	cout << "Wire Index: " << jj << endl;
// 	    cout << "Gatti Distribution Not Implemented Yet!" << endl;
// 	    exit(0);
 	cout << "Wire Index: " << jj << '\n' << endl;
// 	case dipole:
    StTpcCoordinateTransform transformer(mGeomDb, mSCDb);
 	//cout << "Wire Index: " << jj << endl;
// }

	// typedef vector<StTrsWireBinEntry> aTpcWire
{
//     cout << "StTrsSlowAnalogSignalGenerator::inducedChargeOnPad()" << endl;
	
	    PR(currentWire.size());
    StTpcCoordinateTransform transformer(mGeomDb, mSCDb, mElectronicsDb);
// 	PR(currentWire.size());
// 	    PR(currentWire.size());
// 	    ivb PR(ycoord);  // ycoord of Wire
// 	    ivb PR(currentWire[jj].position());
    if(wireHistogram->min()<0) {
	cerr << "Wire Plane is empty" << endl;
 	    PR(ycoord);  // ycoord of Wire
	aTpcWire::iterator iter;
	    // 
	    // CAREFUL, coordinate is (x,y,driftLength)
	    //
	    PR(iter->position());
	    PR(xyCoord);
	    // center of Pad that is being processed?
	    PR(iter->position().z()/mSCDb->driftVelocity());
// 	    PR(xyCoord);
	    //xyCoord.pos().setY(ycoord);
	    float ycoord = wireHistogram->wireCoordinate(jj);
	    PR(tpcRaw);

	    //PR(iter->position().z()/mSCDb->driftVelocity());
// 	    PR(tpcRaw);
	    PR(centralRow);
	    PR(centralPad);
	    PR(mDeltaRow);
	    PR(mDeltaPad);
	    // THIS IS IMPORTANT TO REALIZE!!!
	    //
	    // xyCoord is now:
	    //   x position of hit
	    //   y position of wire
	    //   z drift length
// 	    PR(iter->position());
//  	    PR(xyCoord);


	    //PR(centralPad);
	    //PR(mDeltaRow);
	    //PR(mDeltaPad);
	    //PR(centralRow);
//  	    cout << "AnsigGen r/p " << centralRow << '/' << centralPad << endl;
	    PR(mRowLimits.first);
	    PR(mRowLimits.second);
//  	    PR(mDeltaRow);
// 	    PR(centralPad);
//   	    PR(mDeltaPad);

	    // Calculate the row/pad limits
	    mRowLimits.first  = (centralRow > mDeltaRow) ?
	    
		centralRow + mDeltaRow : centralRow;

		PR(mPadLimits.first);
		PR(mPadLimits.second);
		mRowLimits.second = min(mRowLimits.second, mGeomDb->numberOfInnerRows());
	    }
		    cout << " row: " << irow << " pad: " << ipad << endl;
		mRowLimits.first  = max(mRowLimits.first, (mGeomDb->numberOfInnerRows()+1));
		//PR(mPadLimits.first);
		//PR(mPadLimits.second);
	    //PR(mRowLimits.first);
	    //PR(mRowLimits.second);
		    //cout << " row: " << irow << " pad: " << ipad << endl;
// 	    PR(centralPad);
	    mPadLimits.first  = (centralPad > mDeltaPad) ?
		centralPad - mDeltaPad : centralPad;

	    // Loop over the cross coupled rows(irow)/pads(ipad)
	    for(int irow=mRowLimits.first; irow<=mRowLimits.second; irow++) {
// 		PR(mPadLimits.first);
// 		PR(mPadLimits.second);
		    centralPad + mDeltaPad : mGeomDb->numberOfPadsAtRow(irow);
//  		PR(mPadLimits.first);
//  		PR(mPadLimits.second);
		    PR(tpcRaw);
		    PR(xyCoord);

		    if( !(ipad>0 && ipad<mGeomDb->numberOfPadsAtRow(irow)) )
			continue;
#endif
		    double padWidth, padLength;
		        iter->numberOfElectrons()*
		    cout << "  xl " << xl << " xu " << xu << endl;
		    cout << "  yl " << yl << " yu " << yu << endl;

		    //cout << "  xl " << xl << " xu " << xu << endl;
		    //cout << "  yl " << yl << " yu " << yu << endl;
// 		    PR(padLength);
		    PR(chargeOfSignal);
		    
		    transformer(tpcRaw,xyCoord);
		    PR(chargeOfSignal);
		    // Integral limits for nearest pad
// 		    PR(chargeOfSignal);
		    double xu = xyCoord.pos().x() + padWidth/2;
		    PR(chargeOfSignal);
		    PR(timeOfSignal);
		    
		    //PR(chargeOfSignal);
		    mSector->addEntry(irow,ipad,padSignal);		    
		    PR(ipad);
		    PR(irow);
		    PR(mSector->timeBinsOfRowAndPad(irow,ipad).size());
//   		    PR(padSignal);
		
		    // Make an analog signal
	

	} // (iterator) Loop over all wires
        //                                                2
    } // (jj)Loop over the wires of the Histogram

double StTrsSlowAnalogSignalGenerator::symmetricGaussianResponse(double tbin, StTrsAnalogSignal& s)
// Signal Sampling
// For all functions below: 
// tbin = 0,1,2,3...
// t    = 20,150,250,350...

    // These are taken from DB at Constructor
    //mSigma1 = sqrt(8.)/mSamplingFrequency;

double StTrsSlowAnalogSignalGenerator::deltaResponse(double tbin, StTrsAnalogSignal& s)
{

    // Calculate pulse Height at bin Centroid
    double to = tbin*(1./mSamplingFrequency);
    double factor = 1/(sqrt(2*pi)*mSigma1);
    //    cout << s.amplitude() << '\t' << factor << '\t' << t << '\t' << s.time() << '\t' << mSigma1 << endl;
    value =  s.amplitude()*factor*exp(-sqr(t-s.time())/(2*sqr(mSigma1)));
    //    cout << "v " << value << " " << endl;
    return mGain*value;
    //cout << "v " << (value/(.001*volt)) << " mV" << endl;
    value =  mGain*s.amplitude()*factor*exp(-sqr(t-s.time())/(2*sqr(mSigma1)));
    // Clculate at bin Centroid
    double t = 1./mSamplingFrequency*(tbin+.5);
    //    PR(t/nanosecond);
    //    cout << "gain/volt " << (s.amplitude()*mGain/(.001*volt)) << " mV" << endl;
double StTrsSlowAnalogSignalGenerator::symmetricGaussianExactResponse(double tbin, StTrsAnalogSignal& s)
double StTrsSlowAnalogSignalGenerator::asymmetricGaussianResponse(double t, StTrsAnalogSignal& sig)
    //    F :=   ------------------   exp(-  ---------- )

    double value=0;
    // These are taken from DB at Constructor
    //mSigma1 = 1;
    //mSigma2 = sqrt(8.);
//     PR(lowerBound);
    double factor = sqrt(2/pi)/(mSigma1+mSigma2);
    value =  (mGain*s.amplitude()/2.)*
    if(t<sig.time())
	value =  sig.amplitude()*factor*exp(-sqr(t-sig.time())/(2*sqr(mSigma1)));
    double tau2 = 2.*mSigma1;
	value =  sig.amplitude()*factor*exp(-sqr(t-sig.time())/(2*sqr(mSigma2)));
	
    return mGain*value;
    // that will do this, but I won't incorporate it yet.
    // For now use the approximation:
    
    value *= 1./mSamplingFrequency;

    double K = sigmaL/mTau*sqrt(tzero/mDriftVelocity);

    // Corrected From SN197
    value = 1./(2.*mTau)*sqr(K)*exp(K*(lambda-.5*K))*
	( .5*(1+sqr(lambda))*(1-erf(lambda/M_SQRT2)) -
	  lambda/(sqrt(2*M_PI))*exp(-sqr(lambda)/2));

    value *= mFractionSampled*mGain*sig.amplitude();

    // Amount of Charge in the bin
    value *= mTimeBinWidth;
double StTrsSlowAnalogSignalGenerator::asymmetricGaussianResponseWithUnrestoredBaseline(double t, StTrsAnalogSignal& sig)
{
    double value=0;
{
    // These are taken from DB at Constructor
//     double mSigma1 = 1;
//     double mSigma2 = sqrt(8.);

    double factor = sqrt(2/pi)/(mSigma1+mSigma2);
    // undershoot or unrestored baseline to the signal profile.
    if(t<sig.time())
	value =  sig.amplitude()*factor*exp(-sqr(t-sig.time())/(2*sqr(mSigma1)));
    else {
	if ((t-sig.time())<mSigma2) {  // if inside 1*sigma
	    value =  sig.amplitude()*factor*exp(-sqr(t-sig.time())/(2*sqr(mSigma2)));
	}
	else {  // generate a fraction of undershoot/unrestored
	    value =
		sig.amplitude()*factor*exp(-sqr(t-sig.time())/(2*sqr(mSigma2))) + oneOverT(t,sig.time());
	}	    
    }
}
    return mGain*value;
}


// 		sig.amplitude()*factor*exp(-sqr(t-sig.time())/(2*sqr(mSigma2))) + oneOverT(t,sig.time());
// //     double mSigma2 = sqrt(8.);
//     }
//
//     return (mGain*value);
// }
void StTrsSlowAnalogSignalGenerator::setElectronicSampler(StSignal t)
double StTrsSlowAnalogSignalGenerator::signalSampler(double t, StTrsAnalogSignal& sig)
{
    return symmetricGaussianResponse(t, sig);
    //
    //return asymmetricGaussianResponse(t, sig);
    //return asymmetricGaussianApproximateResponse(t, sig);
    //return realShaperResponse(t,sig);
    //return asymmetricGaussianResponseWithUnRestoredBaseline(t, sig);
	    //break;
	default:
    mTau    = mElectronicsDb->tau();
    mSamplingFrequency = mElectronicsDb->samplingFrequency();
    cout << "got to SlowAnalogsampler " << endl;
    mGain              = mElectronicsDb->nominalGain();

    PR(mSigma1);
    PR(mSigma2);
    PR(mSamplingFrequency);
    PR(mGain);
    // ??sort??
//     PR(mSamplingFrequency);
//     PR(mGain);
    // ??sort?? ---> not necessary
// 	    break;
// 	case realShaper:
// 	default:
// 	    cerr << "Default Function Selected. ERROR!" << endl;
	    cout << "row: " << irow << " pad: " << ipad << " timeBin: " << endl;
	    mDiscreteAnalogTimeSequence.clear();
{
//     cout << "StTrsSlowAnalogSignalGenerator::sampleAnalogSignal()" << endl;
	    cout << "row/pad " << irow << '/' << ipad << ' ' << continuousAnalogTimeSequence.size() << endl;
    // operates on mSector 

	    //cout << "row/pad " << irow << '/' << ipad << ' ' << continuousAnalogTimeSequence.size() << endl;
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    vector<StTrsAnalogSignal> continuousAnalogTimeSequence;
#else
	    //cout << "row: " << irow << " pad: " << ipad << " timeBin: " << endl;
// 		cout << " " << bbb << " " << continuousAnalogTimeSequence[bbb] << endl;
		    cout << "pulse Height: " << pulseHeight << endl;
		//cout << itbin << ", ";

		
		    //cout << "pulse Height: " << pulseHeight << endl;

//
		//cout << " pulse Height: " << pulseHeight << endl;
//  	    PR(continuousAnalogTimeSequence.size());
//  	    for(int bbb=0; bbb<continuousAnalogTimeSequence.size(); bbb++)
		//cout << itbin << " pulse Height: " << pulseHeight << endl;
 		if(pulseHeight < mSignalThreshold)
 		    continue;
// 		cout << itbin << " pulse Height: " << pulseHeight << " " << (*mTimeSequenceIterator) << endl;
		    // charge from any signal that is within
//  		    PR(mTimeSequenceIterator->time()/nanosecond);
//  		    PR(timeBinT-mTimeSequenceIterator->time()/nanosecond);
		    // 10 time bins.  This should be a settable
		    // parameter.
//  		    cout << " tb " << itbin << " " << (*mTimeSequenceIterator) << endl;
		    //if(itbin/freq
		    //discreteAnalogTimeSequence[itbin].second +=
		PR(mElectronicSignal);
		    PR(mElectronicSignal);
		    PR(mElectronicSignal.amplitude()/(.001*volt));
		}

		//pulseHeight += abs(gauss.shoot(0.,.1));

	    cout << " Finished Time Bins: irow: " << irow << " ipad " << ipad << endl;
		}

// 	    cout << " Finished Time Bins: irow: " << irow << " ipad " << ipad << endl;
	    
		// minimal threshold (should read value from database)
		    pulseHeight += generateNoise(); //noise;
		}
// 		cout << itbin << " pulse Height: " << pulseHeight << '\t' << (pulseHeight/(.001*volt)) << endl;

		mElectronicSignal.setTime(itbin);
		mElectronicSignal.setAmplitude(pulseHeight);
//  		if(mElectronicSignal.amplitude() !=0 ) {
// 		    cout << "mElectronicSignal"
// 			 << (mElectronicSignal.amplitude()) << '\t'
// 			 << (mElectronicSignal.amplitude()/(.001*volt)) << endl;
// 		}
//  	    cout << " Finished Time Bins: irow: " << irow << " ipad " << ipad << endl;
// 	    cout << "--> mSector->timeBinsOfRowAndPad(irow,ipad).size() " <<  mSector->timeBinsOfRowAndPad(irow,ipad).size() << endl;

	    } // loop over time bins

	    mSector->assignTimeBins(irow,ipad,mDiscreteAnalogTimeSequence);
	    
	} // loop over pads

    } // loop over rows

}
