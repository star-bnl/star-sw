/***************************************************************************
 *
 * $Id: StRichClusterAndHitFinder.cxx,v 1.4 2000/05/23 16:55:40 lasiuk Exp $
 *
 * Author: bl
 ***************************************************************************
 *
 * Description: Definition of the Cluster Finder
 *  The work is done in:
 *
 ***************************************************************************
 *
 * $Log: StRichClusterAndHitFinder.cxx,v $
 * Revision 1.4  2000/05/23 16:55:40  lasiuk
 * Incorporate new MC info
 * add clone() where necessary
 * accomodate name changes
 *
 * MC info restored in classifyHit() member
 * cut parameters (for decon) added in initializeCutParameters()
 * startAmplitude set to 0.  This keeps track of the local
 * max of the hit now.
 *
 * Revision 2.1  2000/09/13 21:00:42  lasiuk
 * Begin modification for cluster/hit deconvolution
 * - remove matrix interface
 * - add necessary members
 * - unify cog calculation
 * - mapping code is not in place
 *
 * Revision 2.0  2000/08/09 16:22:11  gans
 * Cosmetic Changes. Naming convention for TDrawable objects
 *

 * Filled the id associated to hit
 *
 * add clone() where necessary
 * Revision 1.3  2000/05/18 11:42:25  lasiuk
 * mods for pre StEvent writing
 *
 * Revision 1.2  2000/04/07 12:50:21  dunlop
 * Fixed clearAndDestroy of StRichSinglePixelCollection mThePixels
 * Revision 1.1  2000/04/05 16:39:27  lasiuk
 * Initial Revision
 **************************************************************************/
#define RICH_CF_DEBUG 0
#define ivb if(RICH_CF_DEBUG)cout
#include <iostream.h>
#include <vector>
#include <algorithm>
#include <map>
#ifndef ST_NO_NAMESPACES
using std::vector;
using std::map;
using std::multimap;
#else
typedef multimap < chargeToHitIDMapKey, chargeToHitIDMapValue,  greater<chargeToHitIDMapKey>,
    allocator< OS_PAIR(chargeToHitIDMapKey, chargeToHitIDMapValue) > > chargeToHitIDMapType;
#endif
typedef chargeToHitIDMapType::iterator   chargeToHitIDMapIter;
typedef chargeToHitIDMapType::value_type chargeToHitIDMapValType;		
using std::back_inserter;
#endif

#include "StThreeVector.hh"

#include "StRichClusterAndHitFinder.h"

#include "StRrsMaker/StRichGeometryDb.h"
#include "StRrsMaker/StRichCoordinates.h"
    mGeometryDb = StRichGeometryDb::getDb(); // Singleton
StRichClusterAndHitFinder::StRichClusterAndHitFinder()
{
    mX = mGeometryDb->numberOfPadsInARow();
    mY = mGeometryDb->numberOfRowsInAColumn();

    // --> and should probably go back there
    // if we get rid of the pixels.
    //
    mThePixels.resize(mX,mY);

    this->initializeCutParameters();
    mGeometryDb = StRichGeometryDb::getDb(); // Singleton

StRichClusterAndHitFinder::StRichClusterAndHitFinder(unsigned int x, unsigned int y)
    cout << "\tERROR! SHOULD NEVER BE CALLED" << endl;
    
    cout << "StRichClusterAndHitFinder::StRichClusterAndHitFinder(int,int)";
    cout << "\tSHOULD NEVER BE CALLED" << endl;
    cout << "\tARE YOU SURE YOU MEAN TO DO THIS?" << endl;
    mX = x;
    mY = y;

    this->printQuadrantThreshold();
}

void StRichClusterAndHitFinder::clearAndDestroyThePixels()
{

  // This is not a simple vector.
  mThePixels.clearAndDestroy();

}
void StRichClusterAndHitFinder::clearAndDestroyTheClusters()
{
    for(size_t i=0; i<mTheClusters.size(); i++) {
	delete mTheClusters[i];
    }
    mTheClusters.clear();


void StRichClusterAndHitFinder::clearAndDestroyTheHits()
{
    for(size_t i=0; i<mTheHits.size(); i++) {
	delete mTheHits[i];
    }
//     PR(mTheClusters.size());
//     PR(mTheHits.size());
}

StRichClusterAndHitFinder::~StRichClusterAndHitFinder()

{
    //this->clearAndDestroyAll();
}

{
    mMaxSaturatedPads  = 5;
    mMaxClusterLength  = 12;
    mMaxClusterWidth   = 10;
    mMaxAspectRatio    = 5./2.;
    mMinChargeFraction = 0.20;
}

#ifdef NEVER
void StRichClusterAndHitFinder::loadPixels(StRichReaderInterface* interface)
{
    this->clearAndDestroyAll();
    
    unsigned short theADC;
    //mThePixels.resize(mGeometryDb->numberOfColumns(),mGeometryDb->numberOfRows());
    for(int iRow=0; iRow<=mGeometryDb->numberOfRowsInAColumn(); iRow++) {
 	for(int iPad=0; iPad<=mGeometryDb->numberOfPadsInARow(); iPad++) {
 	    theADC = interface->GetADCFromCoord(iPad,iRow);//GetADCFromCoord
 	    if(theADC) {
		cout << "p/r/adc " << iPad << '/' << iRow << '/' << theADC << endl;
 		mThePixels.push_back(new StRichSinglePixel(iPad,iRow,theADC));
	    }
 	}
    } // loop over rows
}
#endif

void StRichClusterAndHitFinder::loadPixels(vector<StRichSinglePixel>& pixs)
{
    this->clearAndDestroyAll();
    ivb << "StRichClusterAndHitFinder::loadPixels() allocate for " << pixs.size() << endl;
    //mThePixels.resize(mX,mY);
    for(size_t ii=0; ii<pixs.size(); ii++) {
	mThePixels.push_back(new StRichSinglePixel(pixs[ii]));
    }
    ivb << "StRichClusterAndHitFinder::loadPixels() OKAY" << endl;
}

void StRichClusterAndHitFinder::loadPixels(vector<StRichSinglePixel*>& pixs)
{
    this->clearAndDestroyAll();

    ivb << "StRichClusterAndHitFinder::loadPixels(vector<StRichSinglePixel*>&)\n";
    ivb << "\tallocate for " << pixs.size() << endl;

    for(size_t ii=0; ii<pixs.size(); ii++) {
	    mThePixels.push_back(pixs[ii]);
    }

    ivb << "StRichClusterAndHitFinder::loadPixels() OKAY" << endl;   
}

void StRichClusterAndHitFinder::addSinglePixel(StRichSinglePixel* pix)
{
    // use interface to make a pixel container
    mThePixels.push_back(pix);
}

void StRichClusterAndHitFinder::setBorderFlags()
{
    for(size_t ii=0; ii<mThePixels.size(); ii++) {
	int tmpPad = mThePixels[ii]->pad();
	int tmpRow = mThePixels[ii]->row();
	if(tmpPad == 0   ||
	   tmpPad == 79  ||
    cout << "StRichClusterAndHitFinder::makeClusters()" << endl;
	   tmpPad == 159 ||
	   tmpRow == 0   ||
	   tmpRow == 47  ||
	   tmpRow == 48  ||
	   tmpRow == 95    ) {
	    //cout << "setPixels()=>Border " << *mThePixels[ii] << endl;
    os << " Settings for " << mGainVoltage << " V" << endl;
    os << "---------------------------------------------------" << endl;
}

bool StRichClusterAndHitFinder::makeTheClustersAndFilter() 
{
    if(!makeClusters(2))
	return false;
//    if (!makeClusters(5)) return false;
    const double startAmplitude = 10;
//    if (!makeClusters(1)) return false;
//    removeNoiseClusters();
//    if (!splitClusters()) return false;
//    if (!removeSmallClusters()) return false;
    return true;
} 

bool StRichClusterAndHitFinder::makeClusters(double minimumAmplitude)
{
    //cout << "StRichClusterAndHitFinder::makeClusters()" << endl;

    //these are typedef'd:
    //vector<StRichSinglePixel*> PixelVector;
    //stack<StRichSinglePixel*> PixelStack;
    PixelVector newPads;  //single pixels which can be id'd with a single cluster
    PixelStack  theCandidatePads;
    PixelStack  lowAmplitudePixels;
    
    const double startAmplitude = 0;
    //
    // first pass cluster finder
    // Loop over single list;
    //
//     PR(mThePixels.size());

    // Is it necessary to mark all UNUSED? unSetBit(eUsed)
    // IF there is more than 1 pass...YES
		ivb << "x/y-amplitude " << ix << '/' << iy << '-' << amplitude << endl;
    size_t ii;
    for(ii=0; ii<mThePixels.size(); ii++) {
	// if adc is finite and NOT marked used:
	if( mThePixels[ii]->charge() > minimumAmplitude  &&
	    !(mThePixels[ii]->isSet(eUsed)) ) {//!(mThePixels[ii]->isSet(eBorder)
	    
	    mTheClusters.push_back(new StRichSimpleCluster);
	    mTheClusters.back()->setFirstPad(newPads.size());
	    mTheClusters.back()->setMinimumAmplitudeOfLocalMax(startAmplitude);

	    theCandidatePads.push(mThePixels[ii]);
	    mThePixels[ii]->setBit(eUsed);
	    while (!theCandidatePads.empty()) {

		// Move it to the new List and remove!
		newPads.push_back(theCandidatePads.top());
		theCandidatePads.pop();		

		newPads.back()->setBit(eLocalMaximum);
		newPads.back()->setClusterNumber(mTheClusters.size()-1);
	    
		int ix = newPads.back()->pad();
		int iy = newPads.back()->row();
		double amplitude = newPads.back()->charge();
		ivb << "x/y-amplitude ";
		ivb << ix << '/' << iy << '-' << amplitude << endl;

		// No pixel on the border is used,
		// so no n eed to check for boundaries here 
		int iPad;
		int iRow = iy;
		for(iPad = ix-1; iPad<ix+2; iPad+=2) {
		    if(mThePixels(iPad,iRow) == 0) continue;

		    //
		    // Nearest neighbor search
		    //
		    if( mThePixels(iPad,iRow)->charge() > amplitude     ||
			(mThePixels(iPad,iRow+1) != 0 &&
			 mThePixels(iPad,iRow+1)->charge() > amplitude) ||
			(mThePixels(iPad,iRow-1) != 0 &&
			 mThePixels(iPad,iRow-1)->charge() > amplitude) ) {
			newPads.back()->unSetBit(eLocalMaximum);
		    }
		    
		    if( mThePixels(iPad,iRow)->charge() > minimumAmplitude &&
			(!mThePixels(iPad,iRow)->isSet(eUsed)) ) {
			theCandidatePads.push(mThePixels(iPad,iRow));
			mThePixels(iPad,iRow)->setBit(eUsed);
		    }
		} // loop over neighbors in x

		iPad = ix;
		for(iRow = iy-1; iRow<iy+2; iRow+=2) {
		    if(mThePixels(iPad,iRow) == 0) continue;

		    if( mThePixels(iPad,iRow)->charge() > amplitude     ||
			(mThePixels(iPad+1,iRow) != 0 &&
			 mThePixels(iPad+1,iRow)->charge() > amplitude) ||
			(mThePixels(iPad-1,iRow) != 0 &&
			 mThePixels(iPad-1,iRow)->charge() > amplitude) ) {
			newPads.back()->unSetBit(eLocalMaximum);
		    }
		    
		    if( mThePixels(iPad,iRow)->charge() > minimumAmplitude &&
			(!mThePixels(iPad,iRow)->isSet(eUsed)) ) {
			theCandidatePads.push(mThePixels(iPad,iRow));
			mThePixels(iPad,iRow)->setBit(eUsed);
		    }
		} // loop over neighbors in y

		// Update cluster Properties based on the above:
		mTheClusters.back()->increaseNumberOfPads();
		mTheClusters.back()->updateAmplitude(newPads.back()->charge());

		if( newPads.back()->isSet(eLocalMaximum) ) {
		    mTheClusters.back()->increaseNumberOfLocalMax();
		}

		if( newPads.back()->isSet(eLocalMaximum) &&
    PR(newPads.size());
    PR(lowAmplitudePixels.size());
	    } // while(theCandidatePads.size())

	    mTheClusters.back()->rms2Calc();
	    
	} // check if pixel is used
	else if( mThePixels[ii]->charge() <= minimumAmplitude) {
	    ivb << "minimum amp: " << *mThePixels[ii] << endl;
	    lowAmplitudePixels.push(mThePixels[ii]);
	}

    } // loop over all  pixels :=> mThePixels.size()

//     PR(mThePixels.size());
//     PR(newPads.size());
    //
    // Take into account the border pixels that have not been
    // assigned to a cluster
    //
    for(size_t m=0; m<mThePixels.size(); m++) {
	if(mThePixels[m]->charge()>minimumAmplitude &&
	   !mThePixels[m]->isSet(eUsed))
	    cout << "==> ?m " << m << " " << *mThePixels[m] << endl;
    }

    // Diagnostic for total number of pads
    //
    //PR(newPads.size());
    //PR(lowAmplitudePixels.size());
    while(!lowAmplitudePixels.empty()) {
	newPads.push_back(lowAmplitudePixels.top());
	lowAmplitudePixels.pop();
	newPads.back()->setClusterNumber(-1);
    }

    //
    // Update the pixel container
    //

    //if(RICH_CF_DEBUG) PR(mThePixels.size());
    //if(RICH_CF_DEBUG) PR(newPads.size());

    for(unsigned int im=0; im<mThePixels.size(); im++) {
  	if(mThePixels[im]->charge()>minimumAmplitude &&
  	   !mThePixels[im]->isSet(eUsed))
  	    cout << "?im " << im << " " << *mThePixels[im] << endl;
    }

    if(mThePixels.size() != newPads.size()) {
	cout << "StRichClusterAndHitFinder::makeClusters()\n";
	cout << "\tERROR\n";
	cout << "\tPointer lengths mixed up\n";
// 	ofstream highpix("highpix.txt");
// 	ofstream lowpix("lowpix.txt");
// 	int ctr=0;
// 	int high=0;    
// 	for(ii=0; ii<mThePixels.size();ii++) {
// 	    if(mThePixels[ii]->charge()<=2) {
// 		lowpix << *mThePixels[ii] << endl;
// 		ctr++;
// 	    }
// 	    else {
// 		high++;
 	cout << "\taborting\n" << endl;
// 	    }
// 	}
	
// 	PR(ctr);
// 	PR(high);
// 	ofstream highnew("highnew.txt");
// 	ofstream lownew("lownew.txt");

// 	PixelVector::iterator iter;
// 	ctr=0;
// 	high=0;
// 	for(iter=newPads.begin();
// 	    iter!=newPads.end();
// 	    iter++) {
// 	    if((*iter)->charge() <=2) {
// 		ctr++;
// 		lownew << **iter << endl;
// 	    }
// 	    else {
// 		high++;
// 		highnew << **iter << endl;
// 	    }
// 	}
// 	PR(ctr);
// 	PR(high);
 	cout << "\taborting list problems...\n" << endl;
 	abort();
    }
    else {
	for(ii=0; ii<newPads.size();ii++) {
	    mThePixels(ii) = newPads[ii];
	}
    }

    if(RICH_CF_DEBUG) printList();
    //printPadPlane();

    return true;
}

void StRichClusterAndHitFinder::dumpClusterInformation(ostream& os) const
{
    os << '\n' << endl;
    if(!mTheClusters.size()) {
	os << "StRichClusterAndHitFinder::dumpClusterInformation()";
	os << "\tWARNING:";
	os << "\tCluster vector is empty!" << endl;
    }
    else {
	os << mTheClusters.size() << " Clusters found." << endl;
	os << "cluster #pads firstPad #LocMax AmpLocMax charge rms" << endl;
	for(size_t ii=0; ii<mTheClusters.size(); ii++) {
	    os << ii << '\t'
	       << mTheClusters[ii]->numberOfPads()     << '\t'
	       << mTheClusters[ii]->firstPad()         << '\t'
	       << mTheClusters[ii]->numberOfLocalMax() << '\t'
	       << mTheClusters[ii]->minimumAmplitudeOfLocalMax() << '\t'
	       << mTheClusters[ii]->amplitudeSum()     << '\t'
	       << mTheClusters[ii]->rms2() << endl;
	}
    }
}
//     cout << "\n***********************************************************" << endl;
void StRichClusterAndHitFinder::dumpHitInformation(ostream& os) const
{
    os << "\nStRichClusterAndHitFinder::dumpHitInformation\n" << endl;
	os << "hit cluster# charge AmpLocMax rms internal position" << endl;
	os << "StRichClusterAndHitFinder::dumpHitInformation()";
    
	os << "\tHit vector is empty!" << endl;
    // This is what we want for input to the PID algorithm!

    }
//     PR(mTheHits.size());
	       << mTheHits[ii]->internal()     << '\t'
//     PR(mTheHits.size());
    bool makeHitFromCluster = false;
//     PR(mTheClusters.size());
    for(size_t ii=0; ii<mTheClusters.size(); ii++) {
	       << mTheHits[ii]->numberOfPads()      << endl;
	makeHitFromCluster = false;
       
	}
    }
    int firstPadOfCluster = 0;
	ivb << "\t#localMax " << mTheClusters[ii]->numberOfLocalMax() << endl;
    int lastPadOfCluster  = 0;
	// What if cluster has a single local max
    StRichHitInformation hitInfo;
	if(mTheClusters[ii]->numberOfLocalMax() == 1) {
	    makeHitFromCluster = true;
	}
	int numberOfLocalMax = mTheClusters[ii]->numberOfLocalMax();
	// or it has 2 local max
// 	PR(theLocalMaxima.size());
	else if(mTheClusters[ii]->numberOfLocalMax() == 2) {
	// No local max
	    // must be at least 3 pads
	if(!numberOfLocalMax) continue;
	    if(mTheClusters[ii]->numberOfPads() > 3)
		makeHitFromCluster = true;
	vector<StRichSinglePixel*> theLocalMaxima;
	size_t numberOfIsolatedLocalMax =
	// more than 2 local max!
	else {
	    //?
	//
	// A single local max
	double sum = 0;
	double x   = 0;
	double y   = 0;
	double amp = 0;
	double maxAmp = 0;
	int jj;
	if(makeHitFromCluster) {
	    mAnMCHit = 0;
	    for(jj=firstPadOfCluster; jj<lastPadOfCluster; jj++) {
		if(dynamic_cast<StRichSingleMCPixel*>(mThePixels[jj])) {
		    mAnMCHit = 1;
		    //
		    // Here's where it gets interesting
		    // We can store ALL the mcInfo's from the
		    // pixels and evaluate them to assign the
		    // MC information to the hit, or wait until
		    // later and loop over all the pixels in
		    // the clusters...This will have to be studied.
		    //
		}
	    //
		ivb << '\t' << *mThePixels[jj] << endl;
		amp = mThePixels[jj]->charge();
		maxAmp = max(maxAmp,amp);
		sum += amp;
		x   += mThePixels[jj]->pad()*amp;
		y   += mThePixels[jj]->row()*amp;
	    } // loop over all pads in cluster
	    // Loop over the isolated max
	    if(RICH_CF_DEBUG) {
		PR(x);
		PR(y);
		PR(sum);
	    }
		else {
		    cout << "Matrix failed " << kk << '/' << theLocalMaxima.size() << endl;
	//	if(makeHitFromCluster) {
	if(true) {
	    if(mAnMCHit) {
		mTheHits.push_back(new StRichSimpleMCHit);
		//
		// Here is where we could do the second loop
		// for evaluation and assignment of the
		// MC information to the hit?
		// Find the biggest contribution to the pixel and add it
		//mMCInfo.push_back(dynamic_cast<StRichSingleMCPixel*>(mThePixels[jj].);
		//
		// Just temporary
		//
		dynamic_cast<StRichSimpleMCHit*>(mTheHits.back())->
		    setMCInfo(StRichID(-1,-1,-1,-1,eUnknown));
	    }
	    else {
		mTheHits.push_back(new StRichSimpleHit);
	    }
	    mTheHits.back()->internal() = StThreeVector<double>(x/sum,y/sum,0);
	    mTheHits.back()->setCharge(sum);
	    mTheHits.back()->setClusterNumber(mThePixels[jj-1]->clusterNumber());
	    mTheHits.back()->setMaxAmplitude(maxAmp);
	    //mTheHits.back()->setRms()
	    if(RICH_CF_DEBUG)
		cout << *mTheHits.back() << endl;

// 		// unroll the map.  ordered by GREATEST charge.

    cout << "StRichClusterAndHitFinder::simpleHitsFromCluster() # =" << mTheHits.size() << endl;
// 	    else {
// 	    }
	    //
{
bool StRichClusterAndHitFinder::constructTheMatrix(int ipad, int irow,
						   double& fx, double& fy,
						   double& fx2, double& fy2,
						   double& ampsum, StRichSinglePixel*& maxadc)
{
    double amp = 0;
    ampsum = 0;
    maxadc = 0;
    
    for(int iPad=ipad-1; iPad<=ipad+1; iPad++) {
	for(int iRow=irow-1; iRow<=irow+1; iRow++) {
	    if(mThePixels(iPad,iRow) == 0 ||
	       mThePixels(iPad,iRow)->charge()<=0) continue;
	    else {
		amp = mThePixels(iPad,iRow)->charge();
		ampsum += amp;
		fx     += amp*iPad;
		fy     += amp*iRow;
		fx2    += amp*iPad*iPad;
		fy2    += amp*iRow*iRow;
		
		if(amp>maxadc->charge())
		    maxadc = mThePixels(iPad,iRow);
	    } // else
	}
    }
    //
    //
    if(ampsum>0) {
	//
	// do the calculations:
	fx /= ampsum;
	fy /= ampsum;
	fx2 /= ampsum;
	fy2 /= ampsum;
    }
    else {
	fx = fy = fx2 = fy2 = 0;
	    padShift = (neighborVector[ir]-1)/2;
	    int ipad = centralPad-padShift+ip;
	    if(mThePixels(iPad,iRow) == 0) continue;
	    aVectorOfPixels->push_back(mThePixels(ipad,irow));
bool StRichClusterAndHitFinder::useTheMovingMatrix(StRichSinglePixel* pix)
}

    double fractionalPad  = 0;
    double fractionalPad2 = 0;
    double fractionalRow  = 0;
    double fractionalRow2 = 0;
{
    StRichSinglePixel* maxAdc = 0;
    
    bool   anotherIteration = false;
//     cout << "construct square matrix" << endl;
bool StRichClusterAndHitFinder::useTheMovingMatrix(StRichSinglePixel* pix, StRichHitInformation* info)

    //double amp  = pix->charge();
    const int maximumNumberOfIterations = 7;
    for(int ii=0;
	for(int iRow=row-1; iRow<row+2; iRow++) {
    double totalCharge = 0;
	    if(!mThePixels(iPad,iRow)) continue;

	constructTheMatrix(iPad,iRow,
			   fractionalPad,fractionalRow,
			   fractionalPad2,fractionalRow2,totalCharge,maxAdc);
// 	PR(iPad);
// 	PR(iRow);
// 	PR(fractionalPad);
// 	PR(fractionalRow);
// 	PR(totalCharge);
// 	PR(maxAdc);

	if(nearestInteger(fractionalPad) != iPad) {
	    iPad = nearestInteger(fractionalPad);
    return true;
	vector<StRichSinglePixel*> aVectorOfPixels;
	if(nearestInteger(fractionalRow) != iRow) {
	    iRow = nearestInteger(fractionalRow);
	    anotherIteration = false;
						   StRichHitInformation* info)
    const unsigned short maximumNumberOfIterations = 7;
    bool   anotherIteration = true;

    //double totalCharge = 0;
    
    double iPad = pix->pad();
	//cout << "Iteration " << ii << " x= " << info->position() << endl;
    for(size_t ii=0;
    //
    // then make the hit
    //
    mTheHits.push_back(new StRichSimpleHit);
    mTheHits.back()->internal() = StThreeVector<double>(fractionalPad,fractionalRow,0);
    mTheHits.back()->setCharge(totalCharge);
    mTheHits.back()->setClusterNumber(mThePixels(iPad,iRow)->clusterNumber());
    mTheHits.back()->setMaxAmplitude(maxAdc->charge());
    //mTheHits.back()->setRms()
    //cout << *mTheHits.back() << endl;

    return true;
}

bool StRichClusterAndHitFinder::makeHitsFromPixelMatrix()
{
//     cout << "\n***************************************************************" << endl;
//     cout << "StRichClusterAndHitFinder::makeHitsFromPixelMatrix()" << endl;

    const int minimumNumberOfPixelsForMovingMatrix = 10;
    double x,x2;
    double y,y2;
    double sum;
    //    StRichSimpleHit* theCurrentHit;
    
    //
    // This is what we want for input to the PID algorithm!
//     PR(mTheHits.size());
    clearAndDestroyTheHits();
//     PR(mTheHits.size());

    // Loop over the clusters!
//     PR(mTheClusters.size());
    int firstPadOfCluster = 0;
    int lastPadOfCluster  = 0;
    for(size_t ii=0; ii<mTheClusters.size(); ii++) {
	cout << "*** Cluster " << ii << " #pixels: " << mTheClusters[ii]->numberOfPads() << endl;

	firstPadOfCluster = mTheClusters[ii]->firstPad();
	lastPadOfCluster  = firstPadOfCluster+mTheClusters[ii]->numberOfPads();

	cout << "\t#localMax " << mTheClusters[ii]->numberOfLocalMax() << endl;
	//
	// What if cluster has a single local max
	//
	if(mTheClusters[ii]->numberOfLocalMax() == 1 &&
	   mTheClusters[ii]->numberOfPads() < minimumNumberOfPixelsForMovingMatrix) {

	    //
	    // If there are less than 10 pads,
	    // and only 1 local maximum
	    // do it "quick"
	    //
	    x = x2 = y = y2 = sum = 0;
	    double amp = 0;
	    int jj;
	    for(jj=firstPadOfCluster; jj<lastPadOfCluster; jj++) {
		amp  = mThePixels[jj]->charge();
		sum += amp;
		x   += mThePixels[jj]->pad()*amp;
		x2  += mThePixels[jj]->pad()*mThePixels[jj]->pad()*amp;
		y   += mThePixels[jj]->row()*amp;
		y2  += mThePixels[jj]->row()*mThePixels[jj]->row()*amp;
	    }

	    PR(x);
 	    PR(y);
 	    PR(sum);
	    
	    mTheHits.push_back(new StRichSimpleHit);
	    mTheHits.back()->internal() = StThreeVector<double>(x/sum,y/sum,0);
	    mTheHits.back()->setCharge(sum);
	    mTheHits.back()->setClusterNumber(mThePixels[jj-1]->clusterNumber());
	    //mTheHits.back()->setRms()
	    cout << *mTheHits.back() << endl;
	}

	// 
	// Use the walking matrix
	//
	else {
	    
	    if(!useTheMovingMatrix(mThePixels[firstPadOfCluster])) {
		cout << "StRichCF::makeHitsFromPixelMatrix()\n";
		cout << "\tWARNING\n";
		cout << "\tuseTheMovingMatrix() failed!" << endl;
		return false;
	    }
	}
	
    } // loop over all clusters
//     mTheHits.push_back(new StRichSimpleHit);
//     mTheHits.back()->internal() = StThreeVector<double>(fractionalPad,fractionalRow,0);
//     mTheHits.back()->setCharge(totalCharge);
//     mTheHits.back()->setClusterNumber(mThePixels(iPad,iRow)->clusterNumber());
//     mTheHits.back()->setMaxAmplitude(maxAdc->charge());
//     //mTheHits.back()->setRms()
//     //cout << *mTheHits.back() << endl;

    return true;
    // -- too close to existing hits
    //
	if( abs(info->position() - mTheHits[ii]->internal()) < 1 ) {
	    status = false;
// 	    double distance = abs(info->position() - mTheHits[ii]->internal());
// 	    PR(distance);
	    break;
	}
    
//     cout << "useTheMovingMatrix::call classify HitType" << endl;
//     PR(status);
    if(status) {
	mTheHits.back()->setBit(eDeconvoluted);
{
    
	StRichRawCoordinate raw(mTheHits[ii]->internal().x(),
				mTheHits[ii]->internal().y());
	(*mTransform)(raw,local);
	mTheHits[ii]->local() = local.position();
	
	if(RICH_CF_DEBUG)
	    cout << "\t" << ii << " " << *mTheHits[ii] << " " << local << endl;
	
    }
}

    StRichCoordinateTransform* myTransform =
	StRichCoordinateTransform::getTransform(mGeometryDb);
void StRichClusterAndHitFinder::calculateHitsInGlobalCoordinates()
{
//     cout << "StRichClusterAndHitFinder::calculateHitsInLocalCoordinates()" << endl;
    // Loop over hits:
    StGlobalCoordinate global;
    
	(*myTransform)(raw,global);
    for(size_t ii =0; ii<mTheHits.size(); ii++) {
	StRichRawCoordinate raw(mTheHits[ii]->internal().x(),
				mTheHits[ii]->internal().y());
	(*mTransform)(raw,global);
	mTheHits[ii]->global() = global.position();
    }
}

void StRichClusterAndHitFinder::printList(ostream& os)
{
//     cout << "Print Vector" << endl;
//     PR(mThePixels.size());
    for(size_t ii=0; ii<mThePixels.size(); ii++) {
	os.width(3);
		}
		else {
		  mTheHits.push_back(new StRichSimpleHit);
		}
	}
		chargeToHitIDMapIter iter = chargeToHitID.begin();
		float topCharge=iter->first;
		int topHitID=iter->second;
		id_type theID = hitIDToRichID[topHitID];

		// Overwrite its charge with the total charge
		theID.mCharge = topCharge;
		
		(dynamic_cast<StRichSimpleMCHit*>(mTheHits.back()))->setMCInfo(theID);
		
	    }
	    else {
		mTheHits.push_back(new StRichSimpleHit);
	    }



	    //########################################################


#endif
