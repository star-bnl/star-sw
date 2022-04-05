////////////////////////////////////
//
//
#define HISTOGRAM 1
#define UNPACK_ALL
#define ALL_OF_IT
//
//
////////////////////////////////////
#include <Stiostream.h>
#include <unistd.h>

#include <string>
#include <vector>
#include <list>
#include <utility>    // pair
#include <algorithm>  // min() max()

// SCL
#include "StGlobals.hh"
#include "StMatrix.hh"
#include "Randomize.h"
#ifdef HISTOGRAM
#include "StHbook.hh"
#endif
// General TRS
#include "StCoordinates.hh"
#include "StTpcCoordinateTransform.hh"

// TRS
// db
#include "StTpcSimpleGeometry.hh"
#include "StTpcSimpleSlowControl.hh"
#include "StTpcSimpleElectronics.hh"
#include "StSimpleMagneticField.hh"
#include "StTrsDeDx.hh"

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
#include "g2t_tpc_hit.hh"
//#include "St_g2t_tpc_hit_Table.h"
//#include "St_g2t_track_Table.h"

void whichSector(int volId, int* isDet, int* sector, int* padrow){

    //cout << "StTrsMaker::whichSector()" << endl;
    //cout << volId << endl;
    *isDet  = (volId/100000);

    volId  -= (*isDet)*100000;
    *sector = volId/100;

    volId  -= (*sector)*100;
    *padrow = volId;
	
}

int main()
{
#ifdef HISTOGRAM
    cout << "Histogram" << endl;
    
    //
    //  Open histogram file and book tuple
    //
    string fname = "hbook";
    StHbookFile *hbookFile =
	new StHbookFile(fname.c_str());

    //
    // Unpacker Data
    const int tupleSize1 = 5;
    float tuple[tupleSize1];
    StHbookTuple *theTuple  =
	new StHbookTuple("data", tupleSize1);
    *theTuple << "sec" << "row" << "pad" << "amp" << "t" << book;

    //
    const int tupleSize2 = 5;
    float tuple2[tupleSize2];
    StHbookTuple *secTuple  =
	new StHbookTuple("dedx", tupleSize2);

    *secTuple << "sec" << "row" << "x" << "y" << "z" << book;
#endif
    //
    // Make the DataBase
    //
    // Check File access
    //
    cout << "*********************************************" << endl;
    cout << "Try Initialize" << endl;
    string geoFile("../run/TPCgeo.conf");
    if (access(geoFile.c_str(),R_OK)) {
	cerr << "ERROR:\n" << geoFile.c_str() << " cannot be opened" << endl;
	//shell(pwd);
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
   StTpcGeometry *mGeometryDb =
     StTpcSimpleGeometry::instance(geoFile.c_str());
   //mGeometryDb->print();

   StTpcSlowControl *mSlowControlDb =
       StTpcSimpleSlowControl::instance(scFile.c_str());

   StMagneticField *mMagneticFieldDb =
       StSimpleMagneticField::instance(magFile.c_str());

   StTpcElectronics *mElectronicsDb =
       StTpcSimpleElectronics::instance(electronicsFile.c_str());
   //mElectronicsDb->print();
   
   string gas =("Ar");
   StTrsDeDx *mGasDb = new StTrsDeDx(gas);
   //mGasDb->print();

   //
   // Containers
   //

     // A Wire Plane
   StTrsWireHistogram *mWireHistogram =
       StTrsWireHistogram::instance(mGeometryDb, mSlowControlDb);
//    mWireHistogram->setDoGasGain(true);  // True by default
//    mWireHistogram->setDoGasGainFluctuations(true);
   mWireHistogram->setGasGainInnerSector(2000);  // True by default
   mWireHistogram->setGasGainOuterSector(1000);
   mWireHistogram->setDoTimeDelay(false);
   
    // create a Sector:
    // Analog (for calculation)
   StTrsSector *mSector = 
       new StTrsSector(mGeometryDb);

   // Output is into an StTpcRawDataEvent* vector<StTrsDigitalSector*>
   // which is accessible via the StTrsUnpacker
   StTrsUnpacker *mUnPacker = new StTrsUnpacker;

   StTrsRawDataEvent *mAllTheData =
       new StTrsRawDataEvent();
    //
    // Processes
    //
    StTrsChargeTransporter *mChargeTransporter =
      StTrsFastChargeTransporter::instance(mGeometryDb, mSlowControlDb, mGasDb, mMagneticFieldDb);
    // set status:
    mChargeTransporter->setChargeAttachment(true);
    mChargeTransporter->setGatingGridTransparency(false);
//     mChargeTransporter->setTransverseDiffusion(true);
//     mChargeTransporter->setLongitudinalDiffusion(true);
    mChargeTransporter->setExB(false);


    StTrsAnalogSignalGenerator *mAnalogSignalGenerator =
	StTrsSlowAnalogSignalGenerator::instance(mGeometryDb, mSlowControlDb, mElectronicsDb, mSector);
    //
    // Set the function for the induced charge on Pad
    //
    static_cast<StTrsSlowAnalogSignalGenerator*>(mAnalogSignalGenerator)->
        setChargeDistribution(StTrsSlowAnalogSignalGenerator::endo);
	//setChargeDistribution(StTrsSlowAnalogSignalGenerator::gatti);
	//setChargeDistribution(StTrsSlowAnalogSignalGenerator::dipole);
    //
    // Set the function for the Analog Electronics signal shape
    //
    static_cast<StTrsSlowAnalogSignalGenerator*>(mAnalogSignalGenerator)->
	setElectronicSampler(StTrsSlowAnalogSignalGenerator::symmetricGaussianApproximation);
	//setElectronicSampler(StTrsSlowAnalogSignalGenerator::delta);
	//setElectronicSampler(StTrsSlowAnalogSignalGenerator::symmetricGaussianExact);
	//setElectronicSampler(StTrsSlowAnalogSignalGenerator::asymmetricGaussianApproximation);
	//setElectronicSampler(StTrsSlowAnalogSignalGenerator::realShaper); 
    mAnalogSignalGenerator->setDeltaRow(0);
    mAnalogSignalGenerator->setDeltaPad(2);
    mAnalogSignalGenerator->setSignalThreshold(1.*(.001*volt));
    mAnalogSignalGenerator->setSuppressEmptyTimeBins(true);
    mAnalogSignalGenerator->addNoise(false);
    mAnalogSignalGenerator->generateNoiseUnderSignalOnly(false);
    mAnalogSignalGenerator->setNoiseRMS(900.);  // set in #e

    StTrsDigitalSignalGenerator *mDigitalSignalGenerator =
	StTrsFastDigitalSignalGenerator::instance(mElectronicsDb, mSector);

    ///////////////////////  Initialization complete ////////////////////
    
//     // Read the Ionization
//     //

    cout << "Processing" << endl;

    int bisdet, bsectorOfHit, bpadrow;

    // Where is the first hit in the TPC
    //whichSector(tpc_hit->volume_id, &bisdet, &bsectorOfHit, &bpadrow);
//     PR(bisdet);
//     PR(bsectorOfHit);
//     PR(bpadrow);
//     PR(currentSectorProcessed);

    //ifstream ifs("/data/stix/event_global.txt",ios::in);
    ifstream ifs("/data/stix/event_14.txt",ios::in);
    //ifstream ifs("/data/stix/trial.txt",ios::in);  // 2000 hits in 13 sectors
    g2t_tpc_hit tpc_hit;

    //
    // the Limits
    int firstSectorToProcess = 1;
    int lastSectorToProcess  = 1;
    PR(firstSectorToProcess);
    PR(lastSectorToProcess);
    bool low = false;
    bool high = false;

    //
    // Bookeeping
    int currentSectorProcessed = firstSectorToProcess;
    int lastSectorProcessed    = currentSectorProcessed;

    //
    //
    int numberOfPointsInWirePlane = 0;
    
    int no_tpc_hits = 400000;
    //int no_tpc_hits = 15000;
    //int no_tpc_hits = 18412;   // hits in sector 1
    //int no_tpc_hits = 331;
    int i=0;
    do {
	i++;
  	cout << "--> tpc_hit:  " << i << endl;
 	ifs >> tpc_hit.volume_id
	    >> tpc_hit.de
	    >> tpc_hit.ds
	    >> tpc_hit.x
	    >> tpc_hit.p;
	if(ifs.eof() || ifs.bad()) break;
// 	cout << tpc_hit << endl;
	whichSector(tpc_hit.volume_id, &bisdet, &bsectorOfHit, &bpadrow);
//  	PR(bisdet);
//   	PR(bsectorOfHit);
//  	PR(bpadrow);
	
	if(bsectorOfHit >= firstSectorToProcess)
	    low = true;
	else
	    low = false;

	if(bsectorOfHit <= lastSectorToProcess)
	    high = true;
	else
	    high = false;
	
	// Save time initially  - by not processing pseudo padrows
	if(bisdet) {
// 	    cout << "Segment in a pseudo-padRow. Skipping..." << endl;
	    if(i != no_tpc_hits) continue;
	}

	
	if( low                &&
	    high               &&
	    i != no_tpc_hits   &&
	    (bsectorOfHit == currentSectorProcessed)) {
	    
	    int pID = 1;  // I need to know the PID for ionization calculations

	    // I can't believe this.  After such careful design of the coordinate
	    // transformation, I am reduced to this because I am fixed to GEANT
	    // coordinates.  This is the problem you get if you
	    // don't use a common data base
	    // GEANT uses: (which is not correct!)	    
	    
	    double GEANTDriftLength = 208.55119*centimeter;
	    double GEANTOffSet      = mGeometryDb->frischGrid() - GEANTDriftLength;
// 	    PR(GEANTOffSet);
// 	    PR(GEANTDriftLength);
	    
	    // Now relative to this, we get the zPosition in coordinates where :
	    // 0 is the membrane, 208+/-dz is the wire grid
	    //double zPosition = tpc_hit.x[2]*centimeter + GEANTOffSet; // ?
	    double zPosition = tpc_hit.x[2]*centimeter;
// 	    PR(tpc_hit->x[2]*centimeter);
// 	    PR(zPosition);
	    
	    StThreeVector<double> hitPosition(tpc_hit.x[0]*centimeter,
					      tpc_hit.x[1]*centimeter,
					      tpc_hit.x[2]*centimeter); 
//  	    PR(hitPosition);

// 	    // Drift Length is calculated with respect to the FG!
// 	    double fgOffSet = (bpadrow <= mGeometryDb->numberOfInnerRows()) ?
// 		mGeometryDb->innerSectorFrischGridPadPlaneSeparation() :
// 		mGeometryDb->innerSectorFrischGridPadPlaneSeparation();

// 	    PR(mGeometryDb->radialDistanceAtRow(bpadrow));
// 	    PR(mGeometryDb->radialDistanceAtRow(bpadrow)/centimeter);
// 	    PR(mGeometryDb->frischGrid());

	    // And we have our choice of coordinate system.
	    // Let us use globals for now:
	    // could use Matrix, but let's not for now.
	    // This is code copy from coordinate transform...
// 	    PR(bsectorOfHit);
	    double beta = (bsectorOfHit>12) ?
		-bsectorOfHit*M_PI/6 :
		bsectorOfHit*M_PI/6 ;   //(30 degrees)
// 	    PR(beta*30/(M_PI/6));
	    double xp = hitPosition.x()*cos(beta) - hitPosition.y()*sin(beta);
	    double yp = hitPosition.x()*sin(beta) + hitPosition.y()*cos(beta);

	    StThreeVector<double>
		sector12Coordinate(xp*centimeter,
				   yp*centimeter,
				   zPosition);
//  	    PR(sector12Coordinate);
	    //tpc_hit->x[2]*centimeter + mGeometryDb->frischGrid() + fgOffSet);
	    StThreeVector<double> hitMomentum(tpc_hit.p[0]*GeV,
					      tpc_hit.p[1]*GeV,
					      tpc_hit.p[2]*GeV);

	    // I need PID info here, otherwise it is a pion.  This is needed for the ionization splitting!
	    StTrsChargeSegment aSegment(sector12Coordinate,
					hitMomentum,
					(fabs(tpc_hit.de*GeV)),  // cannot use abs (not overloaded for LINUX!
					tpc_hit.ds*centimeter,
					pID);

#ifdef HISTOGRAM
	    tuple2[0] = static_cast<float>(bsectorOfHit);
	    tuple2[1] = static_cast<float>(bpadrow);
	    tuple2[2] = static_cast<float>(sector12Coordinate.x());
	    tuple2[3] = static_cast<float>(sector12Coordinate.y());
	    tuple2[4] = static_cast<float>(sector12Coordinate.z());
	    secTuple->fill(tuple2);
#endif

// 	    PR(hitMomentum.mag());	    
//  	    PR(aSegment);
	    
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
	    int breakNumber = 1;
// 	    PR(aSegment.ds()/millimeter);
	    //breakNumber = aSegment.ds()/(4.*millimeter);
// 	    PR(breakNumber);
	    aSegment.split(mGasDb, mMagneticFieldDb, breakNumber, &comp);


#ifndef ST_NO_TEMPLATE_DEF_ARGS
//   	    copy(comp.begin(), comp.end(), ostream_iterator<StTrsMiniChargeSegment>(cout,"\n"));
//  	    cout << endl;
#endif
	    
//  	    cout << "-->Number of \"miniSegments\": " << (comp.size()) << endl;
	    
	    // Loop over the miniSegments
	    for(iter = comp.begin();
		iter != comp.end();
		iter++) {
		
		//
	        // TRANSPORT HERE
	        //
		mChargeTransporter->transportToWire(*iter);
//  		PR(*iter);
		
		//
		// CHARGE COLLECTION AND AMPLIFICATION
	        //
		
#ifndef __sun   // Bug in the sun iterators.  Must Explicitly dereference!
		StTrsWireBinEntry anEntry(iter->position(), iter->charge());
// 		PR(anEntry);
#else
		StTrsWireBinEntry anEntry((*iter).position(), (*iter).charge());
#endif
		mWireHistogram->addEntry(anEntry);
		
	    } // Loop over the list of iterators
	    
	    //
	    // Evaluate the situation:
	    // are you at the end of a data file?
	    // should you continue processing this sector?
	    // if eof, break;
	    // 	    PR(i);
	    // 	    PR(no_tpc_hits);
	    
	    //tpc_hit++;  // increase the pointer to the next hit
	    numberOfPointsInWirePlane++;
	    continue;   // don't digitize, you still have data in the same sector to process
	} // if (currentSector == bsectorOfHit)
	
	// Do the digitization, if there are points in the wire plane...
	if(numberOfPointsInWirePlane) {    
	    cout << "Processing Sector: " << currentSectorProcessed << endl;
	    //sleep(3);
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
	    // Point to the object you wnat to fill
	    //
	    mDigitalSignalGenerator->fillSector(aDigitalSector);

	    //
	    // ...and digitize it
	    cout << "--->digitizeSignal()..." << endl;
	    mDigitalSignalGenerator->digitizeSignal();

	    //
	    // Fill it into the event structure...
	    // and you better check the sector number!
	    // 	PR(currentSectorProcessed);
	    // 	PR(mAllTheData->mSectors.size());
	
	    //  	cout << "Try add it" << endl;
	    mAllTheData->mSectors[(currentSectorProcessed-1)] = aDigitalSector;
// 	    PR(mAllTheData->mSectors[(currentSectorProcessed-1)]->numberOfRows());
// 	    cout << "okay" << endl;
	
	    // Clear and reset for next sector:
	    mWireHistogram->clear();
	    mSector->clear();
	    numberOfPointsInWirePlane = 0;
	    //
	    // Go to the next sector
	    PR(currentSectorProcessed);
	    currentSectorProcessed = bsectorOfHit;
// 	    if(currentSectorProcessed>13) break;
	    PR(currentSectorProcessed);
	    PR(bsectorOfHit);

	    // This is the place if you want to process one sector only.
	    //
	    //break;  // Finish here
	    //
	} // digitization
	
	// tpc_hit++;
	
    } while(true);// loop over all segments: for(int i...
    //} // mDataSet
  
  // The access stuff:
#ifdef UNPACK_ALL
  //
  // Access it!
  // with:
  //   *mUnPacker  
    cout << "Try Access the data!" << endl;
  //
  // Loop around the sectors: (should be from db, or size of the structure!)
  PR(mAllTheData->mSectors.size());
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
#ifdef ALL_OF_IT
 	  PR(irow);
#endif
	  int numberOfPads = mUnPacker->getPadList(irow, &padList);
	  PR(numberOfPads);
	      
	  if(!numberOfPads)
	      continue;  // That is, go to the next row...

 	  for(int ipad = 0; ipad<numberOfPads; ipad++) {
#ifdef ALL_OF_IT
//  	      PR(ipad);
 	      PR(static_cast<int>(padList[ipad]));
#endif
	      int nseq;

	      //mUnPacker->clearSequences();
	      StSequence* listOfSequences;
	      int getSequencesStatus =
		  mUnPacker->getSequences(irow, padList[ipad], &nseq, &listOfSequences);
		      
 	      PR(getSequencesStatus);
#ifdef ALL_OF_IT
 	      PR(nseq);
#endif
	      for(int kk=0; kk<nseq; kk++) {
#ifdef ALL_OF_IT
 		  PR(listOfSequences[kk].length);
 		  PR(listOfSequences[kk].startTimeBin);
		  
		  for(int zz=0; zz<listOfSequences[kk].length; zz++) {
 		      cout << " " << kk  << " " << zz << '\t'
 			   << '\t' << static_cast<int>(*(listOfSequences[kk].firstAdc)) << endl;
 		      tuple[0] = static_cast<float>(isector);
 		      tuple[1] = static_cast<float>(irow);
 		      tuple[2] = static_cast<float>(padList[ipad]);
 		      tuple[3] = static_cast<float>(static_cast<int>(*(listOfSequences[kk].firstAdc)));
 		      tuple[4] = static_cast<float>(listOfSequences[kk].startTimeBin+zz);
 		      theTuple->fill(tuple);

		      listOfSequences[kk].firstAdc++;
		  } // zz
#endif
// 		  cout << endl;
	      }   // Loop kk
	      //mUnPacker->clearSequences();
	  } // loop over pads
	  
	    // Do the data manipulation here!
	  mUnPacker->clear();
	  
      } // Loop over rows!
      
  } // Loop over sectors
#endif
  
  cout << "Got to the end of the maker" << endl;

#ifdef HISTOGRAM
    cout << "Save and close " << endl;
    hbookFile->saveAndClose();
#endif  
  return 0;
}

// *****************************************************************
// Make sure the memory is deallocated!
// Either Finish() or clean()
// Make sure the return type is proper!
//
// void StTrsMaker::Finish()
//{
// Clean up all the pointers that were initialized in StTrsMaker::Init()
//     delete mGeometryDb;
//     delete mSlowControlDb;
//     delete mMagneticFieldDb;
//     delete mElectronicsDb;
//     delete mGasDb;

//     delete mWireHistogram;
//     delete mSector;
//     delete mUnPacker;
//     delete mAllTheData;
    
//     delete mChargeTransporter;
//     delete mAnalogSignalGenerator;
//     delete mDigitalSignalGenerator;
//}
