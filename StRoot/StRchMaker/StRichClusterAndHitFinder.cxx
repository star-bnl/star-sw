/***************************************************************************
 *
 * $Id: StRichClusterAndHitFinder.cxx,v 2.6 2000/11/21 19:45:04 lasiuk Exp $
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
 * Revision 2.6  2000/11/21 19:45:04  lasiuk
 * Include threshold for different sectors
 *
 * Revision 2.5  2000/11/07 14:12:44  lasiuk
 * init() information and
 * quadrant threshold cuts default is 200 ADC counts
 *
 * Revision 2.4  2000/11/01 16:51:10  lasiuk
 * print the number of pads in dumpHitInfo()
 *
 * Revision 2.2  2000/09/29 19:04:40  lasiuk
 * hit calculation factorized to allow
 * deconvolution.  A flag was added to denote
 * this process (eDeconvolution).
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
 * Revision 1.6  2000/06/01 21:11:16  dunlop
 * Filled the id associated to hit
 *
 * Revision 1.5  2000/05/31 19:26:15  dunlop
 * Filling non-ctor entries in persistent hits + support for this
 *
 * Revision 1.4  2000/05/23 16:55:40  lasiuk
 * Incorporate new MC info
 * add clone() where necessary
 * accomodate name changes
 *
 * Revision 1.3  2000/05/18 11:42:25  lasiuk
 * mods for pre StEvent writing
 *
 * Revision 1.2  2000/04/07 12:50:21  dunlop
 * Fixed clearAndDestroy of StRichSinglePixelCollection mThePixels
 *
 * Revision 1.1  2000/04/05 16:39:27  lasiuk
 * Initial Revision
 *
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
using std::pair;
using std::make_pair;
using std::greater;
using std::max;
using std::max_element;
using std::sort;
using std::unique;
using std::back_inserter;
#endif

#include "StThreeVector.hh"

#include "StRichClusterAndHitFinder.h"

#include "StRrsMaker/StRichGeometryDb.h"
#include "StRrsMaker/StRichCoordinates.h"
#include "StRrsMaker/StRichCoordinateTransform.h"
#include "StRrsMaker/StRichPadPlane.h"
#include "StRrsMaker/StRichEnumeratedTypes.h"
#include "StRrsMaker/StRichOtherAlgorithms.h"

#include "StRrsMaker/StRrsReader.h"

//#include "StDaqLib/GENERIC/EventReader.hh"
//#include "StDaqLib/RICH/RICH_Reader.hh"


StRichClusterAndHitFinder::StRichClusterAndHitFinder()
{
    this->init();
    
    mX = mGeometryDb->numberOfPadsInARow();
    mY = mGeometryDb->numberOfRowsInAColumn();

    //
    // this came from the load() functions
    // --> and should probably go back there
    // if we get rid of the pixels.
    //
    mThePixels.resize(mX,mY);

    this->initializeCutParameters();
}

StRichClusterAndHitFinder::StRichClusterAndHitFinder(unsigned int x, unsigned int y)
{
    this->init();
    
    cout << "StRichClusterAndHitFinder::StRichClusterAndHitFinder(int,int)";
    cout << "\tSHOULD NEVER BE CALLED" << endl;
    cout << "\tARE YOU SURE YOU MEAN TO DO THIS?" << endl;
    mX = x;
    mY = y;
    mThePixels.resize(mX,mY);
    this->initializeCutParameters();

}

void StRichClusterAndHitFinder::init()
{
    //
    // Must be called from any c'tor to initialize
    // the Geometry and Transformation Classes
    // which are both Singletons
    //
    
    mGeometryDb = StRichGeometryDb::getDb();

    mTransform  = StRichCoordinateTransform::getTransform(mGeometryDb);

    //
    // Set the array which determines the Threshold for
    // calling a hit either a "photon" or "Mip"
    // Default setting is 2100 V
    //
    // do not use the index 0 for quadrant
    // index definitions:  [quad][decon]
    //

    //
    // numbers taken from Jamie-arich
    // e-mail Nov7,2000 (Gains)
    // reference /plots/Data/Gain/vsrun
    //
    mQuadrantThresholdCharge[0][0] = 0;
    mQuadrantThresholdCharge[0][1] = 0;
    mQuadrantThresholdCharge[1][0] = 251;
    mQuadrantThresholdCharge[1][1] = 251;
    mQuadrantThresholdCharge[2][0] = 227;
    mQuadrantThresholdCharge[2][1] = 227;
    mQuadrantThresholdCharge[3][0] = 193;
    mQuadrantThresholdCharge[3][1] = 193;
    mQuadrantThresholdCharge[4][0] = 177;
    mQuadrantThresholdCharge[4][1] = 177;
    mGainVoltage = 2100;

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
}

void StRichClusterAndHitFinder::clearAndDestroyTheHits()
{
    for(size_t i=0; i<mTheHits.size(); i++) {
	delete mTheHits[i];
    }
    mTheHits.clear();
}

void StRichClusterAndHitFinder::clearAndDestroyAll()
{
//     cout << "StRichClusterAndHitFinder::clearAndDestroyAll()" << endl;
    this->clearAndDestroyThePixels();
    this->clearAndDestroyTheClusters();
    this->clearAndDestroyTheHits();
//     PR(mThePixels.size());
//     PR(mTheClusters.size());
//     PR(mTheHits.size());
}

StRichClusterAndHitFinder::~StRichClusterAndHitFinder()
{
    //this->clearAndDestroyAll();
}

void StRichClusterAndHitFinder::initializeCutParameters()
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
	   tmpPad == 80  ||
	   tmpPad == 159 ||
	   tmpRow == 0   ||
	   tmpRow == 47  ||
	   tmpRow == 48  ||
	   tmpRow == 95    ) {
	    //cout << "setPixels()=>Border " << *mThePixels[ii] << endl;
	    mThePixels[ii]->setBit(eBorder);
	}
    }
}

void StRichClusterAndHitFinder::setQuadrantThreshold(int quad,
						     int decon,
						     int value,
						     int voltage)
{
    if(quad==0 || quad>4) {
	cout << "StRichClusterAndHitFinder::setQuadrantThreshold()" << endl;
	cout << "\tERROR: You are trying to set parameters for a non-existent sector" << endl;
	cout << "\tAbort()" << endl;
	abort();
    }
    
    if(voltage>2200) {
	cout << "StRichClusterAndHitFinder::setQuadrantThreshold()" << endl;
	cout << "\tERROR: You are trying to set a bad voltage" << endl;
	cout << "\tAbort()" << endl;
	abort();
    }
    
    if(decon<0 || decon>1) {
	cout << "StRichClusterAndHitFinder::setQuadrantThreshold()" << endl;
	cout << "\tERROR: You are trying to an invalid deconvolution value" << endl;
	cout << "\tAbort()" << endl;
	abort();
    }

    if(value<0 || value>1023) {
	cout << "StRichClusterAndHitFinder::setQuadrantThreshold()" << endl;
	cout << "\tERROR: You are trying to an invalid ADC Threshold" << endl;
	cout << "\tAbort()" << endl;
	abort();
    }
    
    mQuadrantThresholdCharge[quad][decon] = value;
    mGainVoltage = voltage;
}

void StRichClusterAndHitFinder::printQuadrantThreshold(ostream& os) const {

    os << "===================================================" << endl;
    os << "StRichClusterAndHitFinder::printQuadrantThreshold()" << endl;
    os << " Isolated     Hit Threshold" << endl;
    os << " Deconvoluted Hit Threshold" << endl;
    os << "---------------------------------------------------" << endl;
    os << " Quadrant 2:             Quadrant 1:             " << endl;
    os << "\t"     << mQuadrantThresholdCharge[2][0]
       << "\t\t\t" << mQuadrantThresholdCharge[1][0] << endl;
    os << "\t"     << mQuadrantThresholdCharge[2][1]
       << "\t\t\t" << mQuadrantThresholdCharge[1][1] << endl;
    os << endl;
    os << " Quadrant 3:             Quadrant 4:             " << endl;
    os << "\t"     << mQuadrantThresholdCharge[3][0]
       << "\t\t\t" << mQuadrantThresholdCharge[4][0] << endl;
    os << "\t"     << mQuadrantThresholdCharge[3][1]
       << "\t\t\t" << mQuadrantThresholdCharge[4][1] << endl;
    os << "---------------------------------------------------" << endl;
    os << " Settings for " << mGainVoltage << " V" << endl;
    os << "---------------------------------------------------" << endl;
}

bool StRichClusterAndHitFinder::makeTheClustersAndFilter() 
{
    if(!makeClusters(2))
	return false;
//    if (!makeClusters(5)) return false;
//    if (!removeBigClusters()) return false;
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

    // Check if there are neighbours
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
		    newPads.back()->charge() > mTheClusters.back()->minimumAmplitudeOfLocalMax() ) {
		    mTheClusters.back()->setMinimumAmplitudeOfLocalMax(newPads.back()->charge());
		}
				
	    } // while(theCandidatePads.size())

	    mTheClusters.back()->rms2Calc();
	    
	} // check if pixel is used
	else if( mThePixels[ii]->charge() <= minimumAmplitude) {
	    ivb << "minimum amp: " << *mThePixels[ii] << endl;
	    lowAmplitudePixels.push(mThePixels[ii]);
	}

    } // loop over all  pixels :=> mThePixels.size()

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
// 		highpix << *mThePixels[ii] << endl;
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

void StRichClusterAndHitFinder::dumpHitInformation(ostream& os) const
{
    os << "\nStRichClusterAndHitFinder::dumpHitInformation\n" << endl;
    if(!mTheHits.size()) {
	os << "StRichClusterAndHitFinder::dumpHitInformation()";
	os << "\tWARNING:";
	os << "\tHit vector is empty!" << endl;
    }
    else {
	os << mTheHits.size() << " Hits identified." << endl;
	os << "hit cluster# charge AmpLocMax rms internal position pads" << endl;
	for(size_t ii=0; ii<mTheHits.size(); ii++) {
	    os << ii << '\t'
	       << mTheHits[ii]->clusterNumber()     << '\t'
	       << mTheHits[ii]->charge()            << '\t'
	       << mTheHits[ii]->maxAmplitude()      << '\t'
	       << "rms"                             << '\t'
	       << mTheHits[ii]->internal()          << '\t'
	       << mTheHits[ii]->local()             << '\t'
	       << mTheHits[ii]->numberOfPads()      << endl;
	}
    }
}

bool StRichClusterAndHitFinder::simpleHitsFromClusters()
{
//     cout << "************";
//     cout << "StRichClusterAndHitFinder::simpleHitsFromClusters()" << endl;

//      PR(mTheHits.size());
    clearAndDestroyTheHits();
//      PR(mTheHits.size());

    //
    // Loop over the clusters!
    //
//      PR(mTheClusters.size());
    int firstPadOfCluster = 0;
    int lastPadOfCluster  = 0;

    size_t ii, kk;
    StRichHitInformation hitInfo;
    vector<StRichSinglePixel*> aVectorOfPixels;
    
    for(ii=0; ii<mTheClusters.size(); ii++) {

	firstPadOfCluster = mTheClusters[ii]->firstPad();
	lastPadOfCluster  = firstPadOfCluster+mTheClusters[ii]->numberOfPads();
	
	int numberOfLocalMax = mTheClusters[ii]->numberOfLocalMax();
//   	cout << " Cluster #" << ii << "\t";
//  	PR(numberOfLocalMax);

	//
	// No local max
	//
	if(!numberOfLocalMax) continue;

	//
	// IF more than 1 max:
	//   find the local max and store them
	//
	vector<StRichSinglePixel*> theLocalMaxima;

	//
	// These are the isolated local maxima in a row
	//  --> not in a column
	//
	size_t numberOfIsolatedLocalMax =
	    this->findTheLocalMaximaInCluster(mTheClusters[ii], theLocalMaxima);
//   	PR(theLocalMaxima.size());
//   	PR(numberOfIsolatedLocalMax);

	aVectorOfPixels.clear();
	
	//
	// A single local max
	//
	if(numberOfLocalMax == 1) {

	    //
	    // Make one hit
	    //
	    this->constructSquareMatrix(theLocalMaxima.front(), ii, &aVectorOfPixels);
	    this->classifyHitType(aVectorOfPixels);
	    this->centerOfGravity(aVectorOfPixels, &hitInfo);
	    this->fillHitInformation(hitInfo);
	    continue;
	}
    
	//
	// Topology check for the cluster
	//
	// Calculate the number of rows and columns the cluster spans
	//
	vector<int> clusterRows;
	vector<int> clusterPads;

	mNumberOfSaturatedPads = 0;

	float maxCharge = 1023;
	for(int uu=firstPadOfCluster; uu<lastPadOfCluster; uu++) {
	    clusterPads.push_back(mThePixels[uu]->pad());
	    clusterRows.push_back(mThePixels[uu]->row());
	    if(mThePixels[uu]->charge() >= maxCharge)
		mNumberOfSaturatedPads++;
	}

	//    ^  <----- x -----> length (# of pads) 
	//    |
	//    
	//    y  width (# of rows)
	//    
	//    |
	//    v
	
	if(mNumberOfSaturatedPads)PR(mNumberOfSaturatedPads);

	sort(clusterPads.begin(),clusterPads.end());
	vector<int> uniqueClusterPads(clusterPads.begin(),
				      unique(clusterPads.begin(), clusterPads.end()));
	mClusterLength = uniqueClusterPads.size();
//  	ostream_iterator<double>  printer(cout,",");
//  	copy(uniqueClusterPads.begin(), uniqueClusterPads.end(),printer);
//  	cout << " << pads in cluster " << endl;

	sort(clusterRows.begin(),clusterRows.end());
	vector<int> uniqueClusterRows(clusterRows.begin(),
				      unique(clusterRows.begin(), clusterRows.end()));
	mClusterWidth = uniqueClusterRows.size();
//  	copy(uniqueClusterRows.begin(), uniqueClusterRows.end(), printer);
//  	cout << " << rows in cluster " << endl;

	//
	// check the cluster characteristics
	//
	int badCluster = false;
	if( mClusterLength > mMaxClusterLength ) {
	    PR(mClusterLength);
	    badCluster = true;
	}
	if( mClusterWidth  > mMaxClusterWidth  ) {
	    PR(mClusterWidth);
	    badCluster = true;
	}
	if( mNumberOfSaturatedPads > mMaxSaturatedPads) {
	    PR(mNumberOfSaturatedPads);
	    badCluster = true;
	}

	// Dervived Quantities
	// Aspect Ratio
	// total numberofpads etc...
	if(badCluster) continue;
	
	//
	// 2 local max
	//

	if(numberOfLocalMax == 2) {
	    //
	    // special case for 2 adjacent local max pads (equal amplitude)
	    // --> make only a single hit
	    if( (numberOfIsolatedLocalMax == 1) &&
		(mClusterWidth == 1) ||                         // 1 row only
		(mClusterWidth == 2 && mClusterLength == 2) ) { // 2x2
		if((theLocalMaxima.front()->isSet(eMaxHasAHorizontalNeighbor) &&
		    theLocalMaxima.back()->isSet(eMaxHasAHorizontalNeighbor)  )) {
// 		    cout << "make a single hit with adjacent neighbours" << endl;
		    this->constructSquareMatrix(theLocalMaxima.front(), ii, &aVectorOfPixels);
		    this->classifyHitType(aVectorOfPixels);
		    this->centerOfGravity(aVectorOfPixels, &hitInfo);
		    this->fillHitInformation(hitInfo);
		    continue;
		}
	    }

	
	    //
	    // else we are making two hits!
	    //
// 	    cout << "StRichClusterAndHitFinder::simpleHitsFromClusters()";
// 	    cout << "\tMake two hits" << endl;

	    //
	    // are they vertical neighbors?
	    //
	    if(numberOfIsolatedLocalMax == 1) {
		for(kk=0; kk<theLocalMaxima.size(); kk++) {
		    if(!theLocalMaxima[kk]->isSet(eMaxHasAVerticalNeighbor)){
// 			cout << "Not Vertical Neighbors! But isolated" << endl;
// 			PR(*theLocalMaxima[0]);
// 			PR(*theLocalMaxima[1]);
		    }
		    this->constructTheAdjacentNeighbors(theLocalMaxima[kk], ii,
							&aVectorOfPixels);
		    this->classifyHitType(aVectorOfPixels);
		    mTheHits.back()->setBit(eDeconvoluted);
		    this->centerOfGravity(aVectorOfPixels, &hitInfo);
		    this->fillHitInformation(hitInfo);
		    aVectorOfPixels.clear();
		}

		continue;
	    }

	    // Are they in the same row? (with small amplitudes?)
	    // use only that row

//  	    cout << "Loop over them all..." << endl;
	    for(kk=0; kk<theLocalMaxima.size(); kk++) {
		if(uniqueClusterPads.size() > 1)
		    this->constructTheAdjacentNeighbors(theLocalMaxima[kk], ii,
							&aVectorOfPixels);
		else
		    this->constructSquareMatrix(theLocalMaxima[kk], ii,
						&aVectorOfPixels);

		this->classifyHitType(aVectorOfPixels);
		mTheHits.back()->setBit(eDeconvoluted);
		this->centerOfGravity(aVectorOfPixels, &hitInfo);
		this->fillHitInformation(hitInfo);
		
		aVectorOfPixels.clear();
	    }
	    continue;

	    //
	    // otherwise there are no MAX that are NEAREST NEIGHBORS so
	    // use ONLY nearest neigbours to calculate position!
	    for(kk=0; kk<theLocalMaxima.size(); kk++) {
		//PR(*theLocalMaxima[kk]);
// 		cout << "2 isolated max use all nearest neighbors" << endl;
		
		this->constructTheNearestNeighbors(theLocalMaxima[kk], ii,
						   &aVectorOfPixels);
		this->classifyHitType(aVectorOfPixels);
		mTheHits.back()->setBit(eDeconvoluted);
		this->centerOfGravity(aVectorOfPixels, &hitInfo);
		this->fillHitInformation(hitInfo);
		aVectorOfPixels.clear();
	    }
	    continue;
	} // if #local max == 2

	//
	// if there are between 3-5 local max
	else if(numberOfLocalMax<5) {
//  	    cout << "=>numberOfLocalMax < 5: " << numberOfLocalMax << endl;
	    //
	    // Loop over the isolated max
	    // use the walking matrix on all
	    //?
	    for(kk=0; kk<theLocalMaxima.size(); kk++) {
//   		cout << kk << "(*theLocalMaxima[kk]) " << (*theLocalMaxima[kk]) << endl;

		if(this->useTheMovingMatrix(theLocalMaxima[kk], ii, &hitInfo)) {
//   		    PR(*theLocalMaxima[kk]);
//   		    PR(hitInfo);
		    this->fillHitInformation(hitInfo);
		}
		else {
		    cout << "Matrix failed " << kk << '/' << theLocalMaxima.size() << endl;
		}
		
		aVectorOfPixels.clear();
	    } // loop over the local maxima

	}
	else {
	    //
	    // If the number of local maxima are bigger than 4
	    // neglect it...this a very rare occurence 
	    // ...This is a project for someone...someday...
	    
	    cout << "StRichClusterAndHitFinder::simpleHitsFromCluster()\n";
	    cout << "\tBad Cluster\n";
	    cout << "\tNumber of Local Max: " << numberOfLocalMax << "/" << mTheClusters[ii]->numberOfPads(); 
	    cout << "\tSkipping...." << endl;
	}


    
    } // loop over all clusters!

//     cout << "StRichClusterAndHitFinder::simpleHitsFromCluster() # =" << mTheHits.size() << endl;
    
    return true;
}

void StRichClusterAndHitFinder::calculateNeighborVector(int n, vector<int>& vec, StRichBoxType btyp)
{
    // make the sequence: (default is eOdd)
    // n : eOdd          eEven
    // 1 : 1              2
    // 2 : 1 3 1          2 4 2
    // 3 : 1 3 5 3 1      2 4 6 4 2
    // 4 : 1 3 5 7 5 3 1  2 4 6 8 6  4 2
    int size = 2*n-1;
    vec.resize(size);
    for(int ii=0; ii<n; ii++) {
	int value = (btyp == eOdd) ? (2*ii+1) : (2*ii+2);
	vec[ii] = value;
	vec[size-1-ii] = value;
    }
    vec[n-1] = (btyp == eOdd) ? size : (size+1);  
}

bool StRichClusterAndHitFinder::constructNeighborMatrixFromVector(StRichSinglePixel* pix,
								  vector<int>& neighborVector,
								  vector<StRichSinglePixel*>* aVectorOfPixels)
{
    int centralRow = pix->row();
    int centralPad = pix->pad();
    
    int numberOfRows = neighborVector.size();
    int numberOfPads = (*max_element(neighborVector.begin(),neighborVector.end()));
    
    int deltaRow = (numberOfRows-1)/2;
    int padShift;
    for(int ir=0; ir<numberOfRows; ir++) { // number of rows to loop
	int irow = centralRow-deltaRow+ir;
	if(!(numberOfPads%2))
	    padShift = neighborVector[ir]/2;
	else
	    padShift = (neighborVector[ir]-1)/2;
	for(int ip=0; ip<neighborVector[ir]; ip++) { // number of pads to loop
	    int ipad = centralPad-padShift+ip;
	    if(!mThePixels(ipad,irow)) continue;
	    aVectorOfPixels->push_back(mThePixels(ipad,irow));
	}
    }
    
    return true;
}

bool StRichClusterAndHitFinder::constructSquareMatrix(StRichSinglePixel* pix, int clusNum,
						      vector<StRichSinglePixel*>* aVectorOfPixels)
{
//     cout << "construct square matrix" << endl;
    int pad = pix->pad();
    int row = pix->row();

//     PR(pad); PR(row); PR(aVectorOfPixels->size());
    for(int iPad=pad-1; iPad<pad+2; iPad++) {
	for(int iRow=row-1; iRow<row+2; iRow++) {

	    if(!mThePixels(iPad,iRow)) continue;

// 	    PR(*mThePixels(iPad,iRow));
// 	    PR(aVectorOfPixels->size());
	    if(clusNum == mThePixels(iPad,iRow)->clusterNumber())
		aVectorOfPixels->push_back(mThePixels(iPad,iRow));
	} 
    }
    
    return true;
}

bool StRichClusterAndHitFinder::useTheMovingMatrix(StRichSinglePixel* pix, int clusNum,
						   StRichHitInformation* info)
{
    const unsigned short maximumNumberOfIterations = 7;
    bool   anotherIteration = true;

    //double totalCharge = 0;
    
    double iPad = pix->pad();
    double iRow = pix->row();

    vector<StRichSinglePixel*> aVectorOfPixels;
    for(size_t ii=0;
	(anotherIteration && ii<maximumNumberOfIterations);
	ii++) {

	anotherIteration = false;
	aVectorOfPixels.clear();
	this->constructSquareMatrix(pix, clusNum, &aVectorOfPixels);
	this->centerOfGravity(aVectorOfPixels, info);
	if(nearestInteger(info->position().x()) != iPad) {
	    iPad = nearestInteger(info->position().x());
	    anotherIteration = true;
	}
	if(nearestInteger(info->position().y()) != iRow) {
	    iRow = nearestInteger(info->position().y());
	    anotherIteration = true;
	}
//   	cout << "Iteration " << ii << " x= " << info->position() << endl;
	if(mThePixels(iPad,iRow))
	    pix = mThePixels(iPad,iRow);
    }

    //
    // do the diagnostics:
    // Do I have a hit that makes sense?
    bool status = true;
    //
    // check the properties of the hit
    // -- central pix has at least 20% of charge
//     cout << "Check (movingMatrix)" << endl;
    if(!mThePixels(iPad,iRow)) {
	//cout << "Central Pixel does not exist" << endl;
	//status =  false;
	return false;
    }
    else if(!mThePixels(iPad,iRow)->isSet(eLocalMaximum)) {
	if( (mMinChargeFraction*info->charge()) > mThePixels(iPad,iRow)->charge() ) {
	    PR(mMinChargeFraction*info->charge());PR( mThePixels(iPad,iRow)->charge());
	    //status = false;
	    return false;
	}
	else {
  	    //cout << "ok" << endl;
	}
    }
    //
    // -- too close to existing hits
    //
    for(size_t ii=0; ii<mTheHits.size(); ii++) {
	if( abs(info->position() - mTheHits[ii]->internal()) < 1 ) {
	    status = false;
// 	    double distance = abs(info->position() - mTheHits[ii]->internal());
// 	    PR(distance);
	    break;
	}
    }
    
//     cout << "useTheMovingMatrix::call classify HitType" << endl;
//     PR(status);
    if(status) {
	this->classifyHitType(aVectorOfPixels);
	mTheHits.back()->setBit(eDeconvoluted);
    }
    
    return status;
}

void StRichClusterAndHitFinder::calculateHitsInLocalCoordinates()
{
//     cout << "StRichClusterAndHitFinder::calculateHitsInLocalCoordinates()" << endl;
    // Loop over hits:

    StRichLocalCoordinate local;
    
//     PR(mTheHits.size());
    for(size_t ii =0; ii<mTheHits.size(); ii++) {
	StRichRawCoordinate raw(mTheHits[ii]->internal().x(),
				mTheHits[ii]->internal().y());
	(*mTransform)(raw,local);
	mTheHits[ii]->local() = local.position();
	
	if(RICH_CF_DEBUG)
	    cout << "\t" << ii << " " << *mTheHits[ii] << " " << local << endl;
	
    }
}

void StRichClusterAndHitFinder::calculateHitsInGlobalCoordinates()
{
//     cout << "StRichClusterAndHitFinder::calculateHitsInLocalCoordinates()" << endl;
    // Loop over hits:
    StGlobalCoordinate global;
    
//     PR(mTheHits.size());
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
	os << ii;
	os << "-";
	os.width(3);
	//os << mThePixels(ii)->charge() << '\t';
	os <<  "p/r/adc "
	   << mThePixels(ii)->pad() << " " 
	   <<  mThePixels(ii)->row() << " " 
	   << mThePixels(ii)->charge() << endl;
// 	if(ii!=0 && ii%5==0)
// 	    os << endl;
    }
}

void StRichClusterAndHitFinder::printPadPlane(ostream& os)
{
    os << "\nprintPadPlane" << endl;
    for(int iRow=0; iRow<mY; iRow++) {
	for(int iPad=0; iPad<mX; iPad++) {
	    os.width(3);
	    os << mThePixels(iPad,iRow)->charge() << " ";
	}
	os << endl;
    }
    os << endl;
}

bool StRichClusterAndHitFinder::centerOfGravity(vector<StRichSinglePixel*>& aVectorOfPixels,
						StRichHitInformation* hitInfo)
{

    double amp = 0;      double maxAdc = 0;
    double tmpX = 0;     double tmpX2 = 0;
    double tmpY = 0;     double tmpY2 = 0;
    double pixelCharge = 0;

//     PR(aVectorOfPixels.size());
    for(size_t jj=0; jj<aVectorOfPixels.size(); jj++) {
	float pad = aVectorOfPixels[jj]->pad();
	float row = aVectorOfPixels[jj]->row();
	
	pixelCharge = aVectorOfPixels[jj]->charge();
	maxAdc = max(maxAdc,pixelCharge);
	amp  += pixelCharge;
	tmpX += pad * pixelCharge;
	tmpY += row * pixelCharge;

	tmpX2 += (pad*pad)*pixelCharge;
	tmpY2 += (row*row)*pixelCharge;
    } // loop over all pads in cluster

    hitInfo->setPosition(tmpX/amp,tmpY/amp, 0);
    hitInfo->setPosition2(tmpX2/amp,tmpY2/amp, 0);

    //
    // Calculate errors here
    //  this is temp:
    //                2       2        2
    //              dx  =  < x  > - < x >
    hitInfo->setPositionError(sqrt(tmpX2/amp-sqr(tmpX/amp)),
			      sqrt(tmpY2/amp-sqr(tmpY/amp)),
			      0.);

    hitInfo->setCharge(amp);
    hitInfo->setMaxAdc(maxAdc);

    // pre cautionary -- hits that do not make the threshold
    // cut have a cluster number assigned of -1
    // Cluster Number
    int cn;
    for(size_t ii=0; ii<aVectorOfPixels.size(); ii++) {
	if( (aVectorOfPixels[ii]->clusterNumber() >= 0) ) {
	    cn = aVectorOfPixels[ii]->clusterNumber();
	    break;
	}
// 	cout << " cn=" << cn << endl;
    }
    hitInfo->setClusterNumber(cn);
    hitInfo->setNumberOfPads(aVectorOfPixels.size());
    
    return 0;
}

bool StRichClusterAndHitFinder::constructTheAdjacentNeighbors(StRichSinglePixel* maxPixel, int clusNum,
							      vector<StRichSinglePixel*>* aVec)
{
    short row = maxPixel->row();
    short pad = maxPixel->pad();

    for(short jj=pad-1; jj<pad+2; jj++) {

      if(!mThePixels(jj,row)) {
	continue;
      }
      //PR(*mThePixels(jj,row));

      if(clusNum == mThePixels(jj,row)->clusterNumber())
	  aVec->push_back(mThePixels(jj,row));
      
    } // loop over all pads in cluster

    return true;
}

bool StRichClusterAndHitFinder::constructTheNearestNeighbors(StRichSinglePixel* maxPixel, int clusNum,
							    vector<StRichSinglePixel*>* aVectorOfPixels)
{
    short row = maxPixel->row();
    short pad = maxPixel->pad();

    //cout << "Nearest" << endl;
    for(short irow=row-1; irow<row+2; irow++) {  // ii = irow
	for(short ipad=pad-1; ipad<pad+2; ipad++) { // jj = ipad
	    // if it doesn't exist
	    if(!mThePixels(ipad,irow)) continue;
	    short difference =
		abs(static_cast<int>(mThePixels(ipad,irow)->pad() - pad)) +
		abs(static_cast<int>(mThePixels(ipad,irow)->row() - row));
	    if(difference > 1) continue;

// 	    PR(*mThePixels(ipad,irow));

	    if(clusNum == mThePixels(ipad,irow)->clusterNumber())
		aVectorOfPixels->push_back(mThePixels(ipad,irow));
	    
	} // loop over all pads in cluster
    }

    return true;
}


size_t StRichClusterAndHitFinder::findTheLocalMaximaInCluster(StRichSimpleCluster* clus,
							      vector<StRichSinglePixel*>& maxPixels)
{
    // Loop over all pixels in the cluster picking out the local max
    size_t ii,jj;
    size_t firstPad = clus->firstPad();
    size_t lastPad  = firstPad + clus->numberOfPads();

    for(ii=firstPad; ii<lastPad; ii++) {
	if (mThePixels[ii]->isSet(eLocalMaximum)) {
	    maxPixels.push_back(mThePixels[ii]);
	}
    }

    //
    // Characterize the types of local maxima
    // -- Is it a central maximum?
    //
    size_t numberOfCentralMax = maxPixels.size();
    for(ii=0; ii<(maxPixels.size()-1); ii++) {
	int irow = maxPixels[ii]->row();
	int ipad = maxPixels[ii]->pad();
	
	for(jj=1;jj<maxPixels.size();jj++) {
	    int jrow = maxPixels[jj]->row();
	    int jpad = maxPixels[jj]->pad();

	    int diff = abs(ipad-jpad) + abs(irow-jrow);
	    if( (diff == 1) ) {
		// they are nearest neighbors
		// set a flag for the pixel
		maxPixels[jj]->setBit(eNotACentralMaximum);
		numberOfCentralMax--;
	    }
	}
    }

    for(ii=0; ii<(maxPixels.size()-1); ii++) {
	int irow = maxPixels[ii]->row();
	int ipad = maxPixels[ii]->pad();

	for(jj=1; jj<maxPixels.size(); jj++) {
	    int jrow = maxPixels[jj]->row();
	    int jpad = maxPixels[jj]->pad();
	    
	    if( abs(irow-jrow) == 1 && (ipad==jpad) ) {
		maxPixels[ii]->setBit(eMaxHasAVerticalNeighbor);
		maxPixels[jj]->setBit(eMaxHasAVerticalNeighbor);
	    }
	    if( abs(ipad-jpad) == 1 && (irow==jrow) ) {
		maxPixels[ii]->setBit(eMaxHasAHorizontalNeighbor);
		maxPixels[jj]->setBit(eMaxHasAHorizontalNeighbor);
	    }
	}
    }

    
    return numberOfCentralMax;
}

bool StRichClusterAndHitFinder::fillHitInformation(StRichHitInformation& info)
{
//      cout << "StRichClusterAndHitFinder::fillHitStructure()" << endl;
    //
    // Remember the internal coordinate is in pad/row fractions
    //
    mTheHits.back()->internal() = info.position();
    mTheHits.back()->sigma() = info.positionError();
    
    mTheHits.back()->setCharge(info.charge());
    mTheHits.back()->setClusterNumber(info.clusterNumber());
    mTheHits.back()->setMaxAmplitude(info.maxAdc());
    mTheHits.back()->setNumberOfPads(info.numberOfPads());
//     mTheHits.back()->setRms();

    //
    // classification of the hit based on amplitude
    // -- in sector #
    // -- deconvoluted

    int quadrantNumber =
	(*mTransform).whichQuadrant(StRichRawCoordinate(info.position().x(),
							info.position().y()));

    if(!mTheHits.back()->isSet(eDeconvoluted)) { // Isolated clusters
	if(info.charge() < mQuadrantThresholdCharge[quadrantNumber][0]) {
	    mTheHits.back()->setBit(ePhotoElectron);
	}
	else {
	    mTheHits.back()->setBit(eMip);
	}
    }
    else {// Deconvoluted Cluster
	if(info.charge() < mQuadrantThresholdCharge[quadrantNumber][1]) {
	    mTheHits.back()->setBit(ePhotoElectron);
	}
	else {
	    mTheHits.back()->setBit(eMip);
	}
    }
    
    //      cout << *mTheHits.back() << endl;

    return 0;
}


bool StRichClusterAndHitFinder::classifyHitType(vector<StRichSinglePixel*>& aPixelVector)
{
  //
  // This function determines the hit type (MC or not) and
  // adds it to the HitVector.  In addition in modifies the
  // MCInfo at the pixel level in the necessary manner 

  //
  // typedefs for mappings:
  //
    typedef int hitIDToChargeMapKey;
    typedef float hitIDToChargeMapValue;
    typedef int hitIDToRichIDMapKey;
    typedef const id_type hitIDToRichIDMapValue;

#ifndef ST_NO_TEMPLATE_DEF_ARGS
    typedef map<hitIDToChargeMapKey,hitIDToChargeMapValue> hitIDToChargeMapType;
    typedef map<hitIDToRichIDMapKey,hitIDToRichIDMapValue> hitIDToRichIDMapType;
#else
    typedef map<hitIDToChargeMapKey,hitIDToChargeMapValue, less<HitIDToChargeMapKey>, 
	allocator< OS_PAIR(hitIDToChargeMapKey,hitIDToChargeMapValue) > > hitIDToChargeMapType;
    typedef map<hitIDToRichIDMapKey,hitIDToRichIDMapValue, less<HitIDToRichIDMapKey>, 
	allocator< OS_PAIR(hitIDToRichIDMapKey,hitIDToRichIDMapValue) > > hitIDToRichIDMapType;
#endif
    typedef hitIDToChargeMapType::iterator hitIDToChargeMapIter;
    typedef hitIDToChargeMapType::const_iterator hitIDToChargeMapConstIter;
    typedef hitIDToChargeMapType::value_type hitIDToChargeMapValType;
    typedef hitIDToRichIDMapType::iterator hitIDToRichIDMapIter;
    typedef hitIDToRichIDMapType::iterator hitIDToRichIDMapConstIter;
    
    typedef hitIDToRichIDMapType::value_type hitIDToRichIDMapValType;
    typedef pair<hitIDToChargeMapIter,bool> hitIDToChargeMapRetType;
    typedef pair<hitIDToRichIDMapIter,bool> hitIDToRichIDMapRetType;
    
    //
    // For unrolling the map
    //
    typedef const float chargeToHitIDMapKey;
    typedef const int chargeToHitIDMapValue;
    
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    typedef multimap < chargeToHitIDMapKey, chargeToHitIDMapValue,
	greater<chargeToHitIDMapKey> > chargeToHitIDMapType;
#else
    typedef multimap < chargeToHitIDMapKey, chargeToHitIDMapValue,
	greater<chargeToHitIDMapKey>,
	allocator< OS_PAIR(chargeToHitIDMapKey, chargeToHitIDMapValue) > > chargeToHitIDMapType;
#endif
    typedef chargeToHitIDMapType::iterator   chargeToHitIDMapIter;
    typedef chargeToHitIDMapType::value_type chargeToHitIDMapValType;		
    
    
    //
    // Define and fill the maps.
    // Need to SUM the charge for a given hitid
    //
    
    hitIDToChargeMapType hitIDToCharge;
    hitIDToRichIDMapType hitIDToRichID;
    
    mAnMCHit = false;
    for(size_t jj=0; jj<aPixelVector.size(); jj++) {
	
	StRichSingleMCPixel* pix =
	    dynamic_cast<StRichSingleMCPixel*>(aPixelVector[jj]);
	
	if (pix) {
	    //
	    // The group of pixels has at least ONE MC pixel associated with it
	    //
	    mAnMCHit = 1;
	    { // scope for sun
		for (const_id_iter iter=pix->MCInfo().begin(); 
		     iter!=pix->MCInfo().end(); ++iter) {
		    int myHitID;
		    if (iter->mSignalType == eFeedback) {
			myHitID = -(iter->mHitID);
		    }
		    else {
			myHitID = iter->mHitID;
		    }
		    hitIDToCharge[myHitID] += iter->mCharge;
		    
		    hitIDToRichIDMapRetType ret = 
			hitIDToRichID.insert(hitIDToRichIDMapValType(myHitID,*iter));
		    if ( (!ret.second) && (!(*iter == hitIDToRichID[myHitID]))) { 
			cout << "StRichClusterAndHitFinder::makeSimpleHitsFromClusters(): ";
			cout << "Warning:  HitID " << myHitID << " not unique: " << endl;
			cout << "\t\t\t G_ID: " << iter->mG_ID 
			     << " blocked by " << hitIDToRichID[myHitID].mG_ID << endl;
			cout << "\t\t\t HitID: " << iter->mHitID 
			     << " blocked by " << hitIDToRichID[myHitID].mHitID << endl;
			cout << "\t\t\t SignalType: " << static_cast<int>(iter->mSignalType) 
			     << " blocked by " 
			     << static_cast<int>(hitIDToRichID[myHitID].mSignalType) << endl;
		    } // if
		} // iter over IDList
	    } // scope for sun
	} // if (dynamic_cast<>) is it an mc pixel?
    
    
    } // loop over all pads in cluster
  

    //
    // if the cluster possess an MCPixel, add
    // an MCHit to the HitVector
    //
    if(mAnMCHit) {
	mTheHits.push_back(new StRichSimpleMCHit);
	//
	// Here is where we could do the second loop
	// for evaluation and assignment of the
	// MC information to the hit?
	// Find the biggest contribution to the pixel and add it
    
	// unroll the map.  ordered by GREATEST charge.
    
	chargeToHitIDMapType chargeToHitID;
	
	{ // Sun scope
	    for (hitIDToChargeMapConstIter iter = hitIDToCharge.begin(); 
		 iter!=hitIDToCharge.end(); ++iter) {
		chargeToHitID.insert(chargeToHitIDMapValType(iter->second,iter->first));
	    }
	}
	
	// grab the first of the greatest.  
	// Don't know what else to do (equal contributions)
    
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

    return true;
}

#ifdef NEVER
	if(true) {
	    mAnMCHit = 0;
	    //
	    // Fill the maps.  Need to SUM the charge for a given hitid
	    //
	    hitIDToChargeMapType hitIDToCharge;
	    hitIDToRichIDMapType hitIDToRichID;

	    for(jj=firstPadOfCluster; jj<lastPadOfCluster; jj++) {
		StRichSingleMCPixel* p = dynamic_cast<StRichSingleMCPixel*>(mThePixels[jj]);
		if (p) {
		    mAnMCHit = 1;
		    { // scope for sun
			for (const_id_iter iter=p->MCInfo().begin(); iter!=p->MCInfo().end(); ++iter) {
			    int myHitID;
			    if (iter->mSignalType == eFeedback) {
				myHitID = -(iter->mHitID);
			    }
			    else {
				myHitID = iter->mHitID;
			    }
			    hitIDToCharge[myHitID] += iter->mCharge;
			    
			    hitIDToRichIDMapRetType ret = 
				hitIDToRichID.insert(hitIDToRichIDMapValType(myHitID,*iter));
			    if ( (!ret.second) && (!(*iter == hitIDToRichID[myHitID]))) { 
				cout << "StRichClusterAndHitFinder::makeSimpleHitsFromClusters(): ";
				cout << "Warning:  HitID " << myHitID << " not unique: " << endl;
				cout << "\t\t\t G_ID: " << iter->mG_ID 
				     << " blocked by " << hitIDToRichID[myHitID].mG_ID << endl;
				cout << "\t\t\t HitID: " << iter->mHitID 
				     << " blocked by " << hitIDToRichID[myHitID].mHitID << endl;
				cout << "\t\t\t SignalType: " << static_cast<int>(iter->mSignalType) 
				     << " blocked by " << static_cast<int>(hitIDToRichID[myHitID].mSignalType) << endl;
			    } // if
			} // iter over IDList
		    } // scope for sun
		} // if (p) is it an mc pixel?
		

		//ivb  << '\t' << *mThePixels[jj] << endl;
		amp = mThePixels[jj]->charge();
		maxAmp = max(maxAmp,amp);
		sum += amp;
		x   += mThePixels[jj]->pad()*amp;
		y   += mThePixels[jj]->row()*amp;
	    } // loop over all pads in cluster

	    if(RICH_CF_DEBUG) {
		PR(x);
		PR(y);
		PR(sum);
	    }

	    //
	    // Determine whether it is an MC Hit or not?
	    //
	    if(mAnMCHit) {
		mTheHits.push_back(new StRichSimpleMCHit);
		//
		// Here is where we could do the second loop
		// for evaluation and assignment of the
		// MC information to the hit?
		// Find the biggest contribution to the pixel and add it

		// unroll the map.  ordered by GREATEST charge.

		chargeToHitIDMapType chargeToHitID;

		{ // Sun scope
		    for (hitIDToChargeMapConstIter iter = hitIDToCharge.begin(); 
			 iter!=hitIDToCharge.end();
			 ++iter) {
			chargeToHitID.insert(chargeToHitIDMapValType(iter->second,iter->first));
		    }
		}
		
		// grab the first of the greatest.  Don't know what else to do (equal contributions)
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
