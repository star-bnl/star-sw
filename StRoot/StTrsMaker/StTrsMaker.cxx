// $Id: StTrsMaker.cxx,v 1.37 1999/07/09 03:45:50 lasiuk Exp $
//
// $Log: StTrsMaker.cxx,v $
// Revision 1.37  1999/07/09 03:45:50  lasiuk
// set switch for the wireHistogram to determine the use
// of a Gaussian or exponential gas gain based on a collection
// or single electrons
//
// Revision 1.36  1999/06/17 19:04:40  lasiuk
// Rotate the momentum the same way that the position is rotated
//
// Revision 1.35  1999/06/16 14:26:49  fisyak
// Add flags for egcs on Solaris
//
// Revision 1.34  1999/05/28 02:55:44  lasiuk
// change default settings for testing.  Only in the Maker!
// Remove histograms
//
// Revision 1.33  1999/04/29 00:15:10  lasiuk
// make sure to clean up pointers that are stored
// in the StTrsRawDataEvent() because they are allocated
// for each event.  This is done with StTrsRawDataEvent::clear()
// which has been added.
//
// Revision 1.32  1999/04/27 19:38:37  lasiuk
// Bfield units in kG from STAR database
//
// Revision 1.31  1999/04/23 19:18:08  lasiuk
// change magnetic field initialization to use gufld from GEANT
//
// Revision 1.30  1999/04/07 01:02:41  lasiuk
// use gas gain from db
//
// Revision 1.29  1999/03/28 02:59:11  perev
// Put interfaces to .const area
//
// Revision 1.28  1999/03/24 22:16:24  lasiuk
// ROOT deletes the dataSet so you have to make
// a new one each time thru Make().  Don't check the
// pointers!
//
// Revision 1.27  1999/03/23 03:37:26  lasiuk
// incorporate ROOT dataSets for DB initialization
// move construction and destruction of "mAllthedata"
//
// Revision 1.26  1999/03/20 20:07:56  fisyak
// Add access to DataSet with parameters
//
// Revision 1.25  1999/03/20 03:23:57  lasiuk
// setMiniSegmentLength()
// setFirstSectorToProcess()
// setLastSectorToProcess()
//
// Revision 1.24  1999/03/19 13:27:11  lasiuk
// change sectors to process 1 ONLY!
//
// Revision 1.23  1999/03/17 17:11:12  lasiuk
// comment out data set deletion for SL99d
//
// Revision 1.22  1999/03/16 02:03:46  lasiuk
// Add Finish(); correct breakNumber calculation; Use C++ casts;
// Change defaults (again) including P10;
// New mechanism for selecting which sectors to process;
// Add flag for processing pseudo-pad rows
//
// Revision 1.21  1999/03/15 02:52:26  perev
// new Maker schema
//
// Revision 1.20  1999/03/02 17:50:39  lasiuk
// for testing/defaults/geantPID
//
// Revision 1.19  1999/02/24 16:59:24  lasiuk
// turn off histos
//
// Revision 1.18  1999/02/24 16:57:02  lasiuk
// x --> -x for sectors>12
//
// Revision 1.17  1999/02/23 19:15:05  lasiuk
// Change in the rotation angle for GEANT global coordinates
//
// Revision 1.16  1999/02/23 01:10:42  lasiuk
// 1st production version
//
// Revision 1.15  1999/02/17 17:02:16  lasiuk
// streamline for production
// remove debug.
// switch for #sectors to be processed
//
// Revision 1.14  1999/02/16 18:15:40  fisyak
// Check in the latest updates to fix them
//
// Revision 1.13  1999/02/15 03:32:09  lasiuk
// coordinate system for input data is global
// deltapad(1)
//
// Revision 1.12  1999/02/12 01:26:51  lasiuk
// Limit debug output
//
// Revision 1.11  1999/02/10 18:01:31  lasiuk
// remove debug/sleep
// ROOT passing
// set defaults
//
// Revision 1.10  1999/02/10 04:30:02  lasiuk
// add unpacker and rawevent as data members/ passed by dataset
//
// Revision 1.9  1999/02/05 23:08:34  fisyak
// Add Valery's update of DataSet
//
// Revision 1.7  1999/01/28 02:46:09  lasiuk
// SUN compile with new GEANT interface
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StTrsMaker class for Makers                                          //
//                                                                      //
#define hISTOGRAM  1
#define uNPACK_ALL 1
#define vERBOSITY  0
//
// You must select a data base initializer method
#define ROOT_DATABASE_PARAMETERS
#define aSCII_DATABASE_PARAMETERS
//////////////////////////////////////////////////////////////////////////

#include "StTrsMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "St_ObjectSet.h"

#include <iostream.h>
#include <unistd.h>    // needed for access()/sleep()
#include <fstream.h>

#include <string>
#include <vector>
#include <list>
#include <utility>    // pair
#include <algorithm>  // min() max()

// SCL
#include "StGlobals.hh"
#include "Randomize.h"
#ifdef HISTOGRAM
#include "StHbook.hh"
#endif

// General TRS
#include "StCoordinates.hh"
#include "StTpcCoordinateTransform.hh"

// TRS
// DataBase Initialization
#ifdef ROOT_DATABASE_PARAMETERS
#include "StTpcROOTGeometry.hh"
#include "StTpcROOTSlowControl.hh"
#include "StTpcROOTElectronics.hh"
#include "StROOTMagneticField.hh"
#endif

#include "StSimpleMagneticField.hh"
#ifdef ASCII_DATABASE_PARAMETERS
#include "StTpcSimpleGeometry.hh"
#include "StTpcSimpleSlowControl.hh"
#include "StTpcSimpleElectronics.hh"
#endif
#ifdef __ROOT__
#define gufld   gufld_
//#define gufld   GUFLD
extern "C" {void gufld(Float_t *, Float_t *);}
#endif
#include "StTrsDeDx.hh"

#include "electronicsDataSet.h"
#include "geometryDataSet.h"
#include "slowcontrolDataSet.h"

// processes
#include "StTrsFastChargeTransporter.hh"
#include "StTrsSlowAnalogSignalGenerator.hh"
#include "StTrsFastDigitalSignalGenerator.hh"

// containers
#include "StTrsChargeSegment.hh"
#include "StTrsMiniChargeSegment.hh"
#include "StTrsAnalogSignal.hh"
#include "StTrsWireBinEntry.hh"
#include "StTrsWireHistogram.hh"

#include "StTrsSector.hh"
#include "StTrsDigitalSector.hh"

// outPut Data--decoder
#include "StTrsRawDataEvent.hh"
#include "StTrsUnpacker.hh"
#include "StSequence.hh"

// g2t tables
#include "St_g2t_tpc_hit_Table.h"
#include "St_g2t_track_Table.h"

//#define VERBOSE 1
//#define ivb if(VERBOSE)

static const char rcsid[] = "$Id: StTrsMaker.cxx,v 1.37 1999/07/09 03:45:50 lasiuk Exp $";

ClassImp(electronicsDataSet)
ClassImp(geometryDataSet)
ClassImp(slowcontrolDataSet)
ClassImp(StTrsMaker)

StTrsMaker::StTrsMaker(const char *name):
mMiniSegmentLength(400.*millimeter),  // used to be 4mm
mFirstSectorToProcess(1),
mLastSectorToProcess(24),
StMaker(name)
{/* nopt */ }

StTrsMaker::~StTrsMaker() { /* nopt */ }

Int_t StTrsMaker::Init()
{
//     // Create tables
//     St_DataSetIter       local(GetDataBase("params"));
  
    //
    // Set up the DataBase access
  St_DataSet *TrsPars = GetDataBase("params/tpc/trspars");
  assert(TrsPars);
  // should use dynamic_cast when available
  geometryDataSet *Geometry    =
      static_cast<geometryDataSet*>(TrsPars->Find("Trs/Geometry"));
  electronicsDataSet *Electronics =
      static_cast<electronicsDataSet*>(TrsPars->Find("Trs/Electronics"));
  slowcontrolDataSet *SlowControl =
      static_cast<slowcontrolDataSet*>(TrsPars->Find("Trs/SlowControl"));

#ifdef ROOT_DATABASE_PARAMETERS
  mGeometryDb =
     StTpcROOTGeometry::instance(Geometry);
  //mGeometryDb->print();

  mSlowControlDb =
       StTpcROOTSlowControl::instance(SlowControl);
  //mSlowControlDb->print();

//   mMagneticFieldDb =
//       StROOTMagneticField::instance();  // default is .5T field in z direction
  
  mElectronicsDb =
      StTpcROOTElectronics::instance(Electronics);
   //mElectronicsDb->print();
#endif
#ifdef ASCII_DATABASE_PARAMETERS
    //
    // Check File access
    //
    cout << "StTrsMaker::Init()" << endl;
    string geoFile("../run/TPCgeo.conf");
    if (access(geoFile.c_str(),R_OK)) {
	cerr << "ERROR:\n" << geoFile.c_str() << " cannot be opened" << endl;
	cerr << "Exitting..." << endl;
	exit(1);
    }

    string scFile = "../run/sc.conf";
    if (access(scFile.c_str(),R_OK)) {
     cerr << "ERROR:\n" << scFile.c_str() << " cannot be opened" << endl;
     cerr << "Exitting..." << endl;
     exit(1);
    }

    string electronicsFile = "../run/electronics.conf";
    if (access(electronicsFile.c_str(),R_OK)) {
	cerr << "ERROR:\n" << electronicsFile.c_str() << " cannot be opened" << endl;
	cerr << "Exitting..." << endl;
	exit(1);
    }

    string magFile = "../run/example.conf";         // contains B field
    if (access(magFile.c_str(),R_OK)) {
	cerr << "ERROR:\n" << magFile.c_str() << " cannot be opened" << endl;
	cerr << "Exitting..." << endl;
	exit(1);
    }

   //
   // The DataBases
   //
   mGeometryDb =
     StTpcSimpleGeometry::instance(geoFile.c_str());
   //mGeometryDb->print();

   mSlowControlDb =
       StTpcSimpleSlowControl::instance(scFile.c_str());
   //mSlowControlDb->print();

//    mMagneticFieldDb =
//        StSimpleMagneticField::instance(magFile.c_str());

   mElectronicsDb =
       StTpcSimpleElectronics::instance(electronicsFile.c_str());
   //mElectronicsDb->print();
#endif

   //
   // Select the gas: Ar, NeCO2, P10 available
   string gas =("P10");
   mGasDb = new StTrsDeDx(gas);
   //mGasDb->print();


   /////////// Magnetic Field
   float x[3] = {0,0,0};
   float B[3];
   gufld(x,B);
   StThreeVector<double> Bfield(B[0],B[1],B[2]);
   Bfield*=kilogauss;
   PR(Bfield/tesla);
   mMagneticFieldDb =
      StSimpleMagneticField::instance(Bfield);  // default is .5T field in z direction
   
   //
   // Containers
   //

   // A Wire Plane
   mWireHistogram =
       StTrsWireHistogram::instance(mGeometryDb, mSlowControlDb);
   mWireHistogram->setDoGasGain(true);
   mWireHistogram->setDoGasGainFluctuations(false); // used to be true
   mWireHistogram->setDoSingleElectronMultiplication(false);
   mWireHistogram->setGasGainInnerSector(mSlowControlDb->innerSectorGasGain());
   mWireHistogram->setGasGainOuterSector(mSlowControlDb->outerSectorGasGain());
   mWireHistogram->setDoTimeDelay(false);
   
   //cout << ">innerSectorGasGain= " << mSlowControlDb->innerSectorGasGain() << endl;
   //cout << ">outerSectorGasGain= " << mSlowControlDb->outerSectorGasGain() << endl;

   //
   // An Analog (for calculation)
   mSector = 
       new StTrsSector(mGeometryDb);

   //
   // Processes
   //
   mChargeTransporter =
       StTrsFastChargeTransporter::instance(mGeometryDb, mSlowControlDb, mGasDb, mMagneticFieldDb);
   // set status:
   mChargeTransporter->setChargeAttachment(false); // used to be true
   mChargeTransporter->setGatingGridTransparency(false);
   mChargeTransporter->setTransverseDiffusion(false);  // used to be true
   mChargeTransporter->setLongitudinalDiffusion(false); // used to be true
   mChargeTransporter->setExB(false);


   mAnalogSignalGenerator =
       StTrsSlowAnalogSignalGenerator::instance(mGeometryDb, mSlowControlDb, mElectronicsDb, mSector);
   //
   // Set the function for the induced charge on Pad
   // -->StTrsSlowAnalogSignalGenerator::endo
   //-->StTrsSlowAnalogSignalGenerator::gatti
   //-->StTrsSlowAnalogSignalGenerator::dipole
   static_cast<StTrsSlowAnalogSignalGenerator*>(mAnalogSignalGenerator)->
       setChargeDistribution(StTrsSlowAnalogSignalGenerator::endo);
   //
   // Set the function for the Analog Electronics signal shape
   //-->StTrsSlowAnalogSignalGenerator::delta
   //-->StTrsSlowAnalogSignalGenerator::symmetricGaussianApproximation
   //-->StTrsSlowAnalogSignalGenerator::symmetricGaussianExact
   //-->asymmetricGaussianApproximation
   //-->StTrsSlowAnalogSignalGenerator::realShaper
   static_cast<StTrsSlowAnalogSignalGenerator*>(mAnalogSignalGenerator)->
       setElectronicSampler(StTrsSlowAnalogSignalGenerator::symmetricGaussianApproximation);
   mAnalogSignalGenerator->setDeltaRow(0);
   mAnalogSignalGenerator->setDeltaPad(2);
   mAnalogSignalGenerator->setSignalThreshold(.1*millivolt);
   mAnalogSignalGenerator->setSuppressEmptyTimeBins(true);
   mAnalogSignalGenerator->addNoise(false);
   mAnalogSignalGenerator->generateNoiseUnderSignalOnly(false);
   mAnalogSignalGenerator->setNoiseRMS(900);  // set in  #e
	

   mDigitalSignalGenerator =
       StTrsFastDigitalSignalGenerator::instance(mElectronicsDb, mSector);

   //
   // Output is into an StTpcRawDataEvent* vector<StTrsDigitalSector*>
   // which is accessible via the StTrsUnpacker.  Initialize the pointers!
   mUnPacker=0;
   mAllTheData=0;

   //
   // Maker Initialization ---now given by default arguments in the constructor
   //    mFirstSectorToProcess = 1;
   //    mLastSectorToProcess  = 1;


   // This should really be a boolean...when ROOT can handle it, change it!
   mProcessPseudoPadRows = 0;  // 0 is no, 1 is yes!

   //
   // Construct constant data sets.  This is what is passed downstream
   mUnPacker = new StTrsUnpacker();
   mAllTheData = new StTrsRawDataEvent();
   AddConst(new St_ObjectSet("Event"  , mAllTheData));
   AddConst(new St_ObjectSet("Decoder", mUnPacker));

// #ifdef HISTOGRAM
//     //
//     //  Open histogram file and book tuple
//     //
//     string fname = "hbook";
//     mHbookFile = new StHbookFile(fname.c_str());

//     mTupleSize = 2;
//     float tuple[mTupleSize];
//     StHbookTuple *theTuple  =
// 	new StHbookTuple("segment", tupleSize1);
//     *theTuple << "seg"  << "n" << book;
        
// #endif

   
   return StMaker::Init();
}

//
// My Member Functions
//
void StTrsMaker::whichSector(int volId, int* isDet, int* sector, int* padrow){

    //cout << "StTrsMaker::whichSector()" << endl;
    *isDet  = (volId/100000);

    volId  -= (*isDet)*100000;
    *sector = volId/100;

    volId  -= (*sector)*100;
    *padrow = volId;
	
}
Int_t StTrsMaker::Make(){
    //  PrintInfo();

    
    //Do not use this unless you really know what you are
    // doing...This is a histogram diagnostic to compare
    // the GEANT hits to those produced by TRS!

    int currentSectorProcessed = mFirstSectorToProcess;

    cout << "Processing sectors "
	 << mFirstSectorToProcess
	 << "--"
	 << mLastSectorToProcess << endl;

    //
    cout << "make sure pointer are clean" << endl;
    mAllTheData->clear();
    //
    //
    
    //cout << "Make ofstream" << endl;
    //ofstream ofs("/star/u2b/lasiuk/geantdebug.txt", ios::out);
    //ofstream raw("/star/u2b/lasiuk/event.txt",ios::out);
    if (!m_DataSet->GetList())  {//if DataSet is empty fill it
    //
    // Read the GEANT info
    // these structures/classes are defined in:
    // $STAR/pams/sim/idl/g2t_tpc_hit.idl 
    // $STAR/StRoot/base/St_DataSet.h & St_Table.h 
    //
    St_DataSetIter geant(GetDataSet("geant"));
    
    //St_g2t_tpc_hit *g2t_tpc_hit = (St_g2t_tpc_hit *) geant("g2t_tpc_hit");
    St_g2t_tpc_hit *g2t_tpc_hit =
	static_cast<St_g2t_tpc_hit *>(geant("g2t_tpc_hit"));
    int no_tpc_hits         =  g2t_tpc_hit->GetNRows();
    g2t_tpc_hit_st *tpc_hit =  g2t_tpc_hit->GetTable();

    //St_g2t_track *g2t_track = (St_g2t_track *) geant("g2t_track");
    St_g2t_track *g2t_track =
	static_cast<St_g2t_track *>(geant("g2t_track"));
    g2t_track_st *tpc_track = g2t_track->GetTable();

    //int geantPID = tpc_track->ge_pid;
    //PR(geantPID);

    // inRange should be a boolean
    bool inRange;
    int  bisdet, bsectorOfHit, bpadrow;
    int  numberOfProcessedPointsInCurrentSector = 0;
    // Limit the  processing to a fixed number of segments
    //no_tpc_hits = 20;
    for (int i=1; i<=no_tpc_hits; i++){
// 	cout << "--> tpc_hit:  " << i << endl;
//  	raw << tpc_hit->volume_id   << ' '
//  	    << tpc_hit->de          << ' '
//  	    << tpc_hit->ds          << ' '
//  	    << tpc_hit->x[0]        << ' '
//  	    << tpc_hit->x[1]        << ' '
//  	    << tpc_hit->x[2]        << ' '
//  	    << tpc_hit->p[0]        << ' '
//  	    << tpc_hit->p[1]        << ' '
//  	    << tpc_hit->p[2]        << ' '  << endl;

	whichSector(tpc_hit->volume_id, &bisdet, &bsectorOfHit, &bpadrow);
// 	PR(bsectorOfHit);
	if(bsectorOfHit >= mFirstSectorToProcess &&
	   bsectorOfHit <= mLastSectorToProcess)
	    inRange = true;
	else
	    inRange = false;

	// Save time initially  - by not processing pseudo padrows
	if(bisdet && !mProcessPseudoPadRows) {
 	    //cout << "Segment in a pseudo-padRow. Skipping..." << endl;
	    tpc_hit++;
	    if(i != no_tpc_hits) continue;
	}
	//sleep(2);

	//
	// If not in range AND there are no points processed, skip to next point
	if(!inRange && !numberOfProcessedPointsInCurrentSector) {
	    cout << "out of range and no points" << endl;
	    tpc_hit++;
	    continue;
	}
	
	if((inRange)  &&
	   (bsectorOfHit == currentSectorProcessed) &&
	   (i            != no_tpc_hits           )) {

	    // CAREFUL:
	    // GEANT uses: (which is not correct!)
	    //double GEANTDriftLength = 208.55119*centimeter;
	    //double GEANTOffSet      = mGeometryDb->frischGrid() - GEANTDriftLength;
	    
	    // Now relative to this, we get the zPosition in coordinates where :
	    // 0 is the membrane, 208+/-dz is the wire grid
	    //double zPosition =
	    //GEANTDriftLength/2. + tpc_hit->x[2]*centimeter + GEANTOffSet;
	    //--->double zPosition = tpc_hit->x[2]*centimeter;
	    //PR(tpc_hit->x[2]*centimeter);
	    //PR(zPosition);
	    
	    StThreeVector<double> hitPosition(tpc_hit->x[0]*centimeter,
	    				      tpc_hit->x[1]*centimeter,
	    				      tpc_hit->x[2]*centimeter); 
	    //PR(hitPosition);

// 	    // Drift Length is calculated with respect to the FG!
// 	    double fgOffSet = (bpadrow <= mGeometryDb->numberOfInnerRows()) ?
// 		mGeometryDb->innerSectorFrischGridPadPlaneSeparation() :
// 		mGeometryDb->innerSectorFrischGridPadPlaneSeparation();

	    // In GEANT Global Coordinates we have to rotate
	    // to the sector 12 position
	    // Should use StMatrix??? or StTpcCoordinateTransform
	    // but it is slower
	    // It is also in StTrsChargeSegment::rotate()
	    // should change to this SOON, but there is a time penalty because
	    // a 2x2 matrix must be constructed
	    double beta = (bsectorOfHit>12) ?
		-bsectorOfHit*M_PI/6 :
		bsectorOfHit*M_PI/6 ;   //(30 degrees)
	    double cb   = cos(beta);
	    double sb   = sin(beta);
	    double xp = hitPosition.x()*cb - hitPosition.y()*sb;
	    double yp = hitPosition.x()*sb + hitPosition.y()*cb;

	    StThreeVector<double>
		sector12Coordinate(xp,yp,tpc_hit->x[2]);

	    if(bsectorOfHit>12) {
		sector12Coordinate.setX(-xp);
	    }

	    // Must rotate the momentum as well,  BUT you should
	    // only incur this calculational penalty if you split
	    // the segment into more than 1 mini segement
	    double pxPrime = tpc_hit->p[0]*cb - tpc_hit->p[1]*sb;
	    double pyPrime = tpc_hit->p[0]*sb + tpc_hit->p[1]*cb;
	    
	    StThreeVector<double> hitMomentum(pxPrime*GeV,
					      pyPrime*GeV,
					      tpc_hit->p[2]*GeV);


//	    PR(tpc_hit->p[0]*GeV);
//	    PR(pxPrime*GeV);
//	    PR(hitMomentum);

	    
	    // I need PID info here, for the ionization splitting (beta gamma)!
	    int geantPID = tpc_track[tpc_hit->track_p].ge_pid;
	    //cout << "gentPID " << gentPID << " " << tpc_hit->de << endl;
	    // WARNING:  cannot use "abs" (not overloaded (double) for LINUX!
	    StTrsChargeSegment aSegment(sector12Coordinate,
					hitMomentum,
					(fabs(tpc_hit->de*GeV)),
					tpc_hit->ds*centimeter,
					geantPID);

// 	    PR(hitPosition);
// 	    PR(sector12Coordinate);
// 	    PR(hitMomentum.mag());

// 	    ofs << " " << aSegment << endl;
//   	    PR(aSegment);
	    
#ifndef ST_NO_TEMPLATE_DEF_ARGS
	    vector<int> all[3];
#else
	    vector<int,allocator<int> > all[3];
#endif
	    
#ifndef ST_NO_TEMPLATE_DEF_ARGS
	    list<StTrsMiniChargeSegment> comp;
	    list<StTrsMiniChargeSegment>::iterator iter;
#else
	    list<StTrsMiniChargeSegment,allocator<StTrsMiniChargeSegment> > comp;
	    list<StTrsMiniChargeSegment,allocator<StTrsMiniChargeSegment> >::iterator iter;
#endif
	    //
	    // Break the segment for diffusion reproduction.
	    // Fast Simulation can use breakNumber = 1
	    //
	    int breakNumber = max(aSegment.ds()/mMiniSegmentLength,1.);
// 	    PR(aSegment.ds()/millimeter);
// 	    PR(breakNumber);
#ifdef HISTOGRAM
	    tuple[0] = static_cast<float>(aSegment.ds()/millimeter);
	    tuple[1] = static_cast<float>(breakNumber);
	    theTuple->fill(tuple);
#endif

	    aSegment.split(mGasDb, mMagneticFieldDb, breakNumber, &comp);
	    
#ifndef ST_NO_TEMPLATE_DEF_ARGS
	    //copy(comp.begin(), comp.end(), ostream_iterator<StTrsMiniChargeSegment>(cout,"\n"));
#endif
	    
	    // Loop over the miniSegments
	    for(iter = comp.begin();
		iter != comp.end();
		iter++) {
		
		//
	        // TRANSPORT HERE
	        //
		mChargeTransporter->transportToWire(*iter);
  		//PR(*iter);
		
		//
		// CHARGE COLLECTION AND AMPLIFICATION
	        //
		
		//#if  defined(__sun) && !defined(__GNUG__)   
// Bug in the sun iterators.  Must Explicitly dereference!
		//		StTrsWireBinEntry anEntry(iter->position(), iter->charge());
// 		PR(anEntry);
		//#else
		StTrsWireBinEntry anEntry((*iter).position(), (*iter).charge());
		//#endif
		mWireHistogram->addEntry(anEntry);
		
	    } // Loop over the list of iterators

	    tpc_hit++;  // increase the pointer to the next hit
	    numberOfProcessedPointsInCurrentSector++;
	    continue;   // don't digitize, you still have data in the same sector to process
	} // if (currentSector == bsectorOfHit)
	// Otherwise, do the digitization...
	
	cout << "Current Sector: " << currentSectorProcessed << endl;

	//
	// Generate the ANALOG Signals on pads
	//
	cout << "--->inducedChargeOnPad()..." << endl;
	mAnalogSignalGenerator->inducedChargeOnPad(mWireHistogram);

	cout << "--->sampleAnalogSignal()..." << endl;
	mAnalogSignalGenerator->sampleAnalogSignal();

	//
	// Digitize the Signals
	//
	// First make a sector where the data can go...
	StTrsDigitalSector* aDigitalSector =
	    new StTrsDigitalSector(mGeometryDb);
	//
	// Point to the object you want to fill
	//
	mDigitalSignalGenerator->fillSector(aDigitalSector);

	//
	// ...and digitize it
	cout << "--->digitizeSignal()..." << endl;
	mDigitalSignalGenerator->digitizeSignal();

	//
	// Fill it into the event structure...
	// and you better check the sector number!
	
	mAllTheData->mSectors[(currentSectorProcessed-1)] = aDigitalSector;
	// Clear and reset for next sector:
	mWireHistogram->clear();
	mSector->clear();
	
	//
	// Go to the next sector --> should be identical to a simple increment
	currentSectorProcessed = bsectorOfHit;
        numberOfProcessedPointsInCurrentSector = 0;
// you can skip out here if you only want to process a single sector...
// 	if(currentSectorProcessed>3)
// 	    break;  // Finish here
	//
    } // loop over all segments: for(int i...
  } // mDataSet
  
  // The access stuff:
#ifdef UNPACK_ALL
  //
  // Access the data with
  //   *mUnPacker  

    //
    // Loop around the sectors: (should be from db, or size of the structure!)
    //
    for(int isector=1; isector<=24; isector++) {
	int getSectorStatus =
	    mUnPacker->getSector(isector,
				 static_cast<StTpcRawDataEvent*>(mAllTheData));
	//PR(getSectorStatus);
	
	// if getSectorStatus is bad move on to the next sector
	if(getSectorStatus) continue;

	// otherwise, let's decode it
	unsigned char* padList;
	for(int irow=1; irow<=45; irow++) {
//  	  PR(irow);
	    int numberOfPads = mUnPacker->getPadList(irow, &padList);
// 	  PR(numberOfPads);

	    // If there are no pads, go to the next row...
	    if(!numberOfPads) continue;
	      
	    for(int ipad = 0; ipad<numberOfPads; ipad++) {
		//PR(static_cast<int>(padList[ipad]));
		int nseq;
		  
		StSequence* listOfSequences;
		int getSequencesStatus =
		    mUnPacker->getSequences(irow,
					    padList[ipad],
					    &nseq,
					    &listOfSequences);
	      //PR(getSequencesStatus);
		      
		for(int kk=0; kk<nseq; kk++) {
		    //PR(listOfSequences[kk].length);
		    for(int zz=0; zz<listOfSequences[kk].length; zz++) {
#ifdef VERBOSITY
			cout << " " << kk
			     << " " << zz << '\t'
			     << static_cast<int>(*(listOfSequences[kk].firstAdc)) << endl;
#endif
			listOfSequences[kk].firstAdc++;
		    } // zz

		} // Loop kk

	    } // loop over pads
	    //
	    // One would do the data manipulation here!
	    // Then deallocate the memory
	    mUnPacker->clear();
	} // Loop over rows!
    } // Loop over sectors
#endif
  
    //cout << "Got to the end of the maker" << endl;
#ifdef HISTOGRAM
    cout << "Save and close " << endl;
    hbookFile->saveAndClose();
#endif    
    // CAUTION: ROOT is resposible for the memory at this point
    // ROOT deletes m_DataSet in the chain after every event.

    return kStOK;
}

// *****************************************************************
// Make sure the memory is deallocated!
//
Int_t StTrsMaker::Finish()
{
    //Clean up all the pointers that were initialized in StTrsMaker::Init()
    delete mGeometryDb;
    delete mSlowControlDb;
    delete mMagneticFieldDb;
    delete mElectronicsDb;
    delete mGasDb;
    
    delete mWireHistogram;
    delete mSector;
    delete mUnPacker;
    delete mAllTheData;
    
    delete mChargeTransporter;
    delete mAnalogSignalGenerator;
    delete mDigitalSignalGenerator;

    return kStOK;
}

void StTrsMaker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: StTrsMaker.cxx,v 1.37 1999/07/09 03:45:50 lasiuk Exp $\n");
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
}
