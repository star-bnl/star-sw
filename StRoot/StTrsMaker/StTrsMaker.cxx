// $Id: StTrsMaker.cxx,v 1.15 1999/02/17 17:02:16 lasiuk Exp $
//
// $Log: StTrsMaker.cxx,v $
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
// StTrsMaker class for Makers                                        //
//                                                                      //
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
#include "St_g2t_tpc_hit_Table.h"
#include "St_g2t_track_Table.h"

//#define VERBOSE 1
//#define ivb if(VERBOSE)

static const char rcsid[] = "$Id: StTrsMaker.cxx,v 1.15 1999/02/17 17:02:16 lasiuk Exp $";

ClassImp(StTrsMaker)

StTrsMaker::StTrsMaker(const char *name, const char *title):StMaker(name,title)
{
   drawinit=kFALSE;
}

StTrsMaker::~StTrsMaker() { /* nopt */ }

Int_t StTrsMaker::Init()
{
//     // Create tables
//     St_DataSetIter       local(gStChain->DataSet("params"));

    //
    // Set up the DataBase access
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

   mMagneticFieldDb =
       StSimpleMagneticField::instance(magFile.c_str());

   mElectronicsDb =
       StTpcSimpleElectronics::instance(electronicsFile.c_str());

   string gas =("Ar");
   mGasDb = new StTrsDeDx(gas);
   //mGasDb->print();

   //
   // Containers
   //

   // A Wire Plane
   mWireHistogram =
       StTrsWireHistogram::instance(mGeometryDb, mSlowControlDb);
   mWireHistogram->setDoGasGain(true);  // True by default
   mWireHistogram->setDoGasGainFluctuations(false);
   mWireHistogram->setGasGainInnerSector(4000);  // True by default
   mWireHistogram->setGasGainOuterSector(1000);
   mWireHistogram->setDoTimeDelay(false);

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
   mChargeTransporter->setChargeAttachment(true);
   mChargeTransporter->setGatingGridTransparency(false);
   mChargeTransporter->setTransverseDiffusion(true);
   mChargeTransporter->setLongitudinalDiffusion(true);
   mChargeTransporter->setExB(false);


   mAnalogSignalGenerator =
       StTrsSlowAnalogSignalGenerator::instance(mGeometryDb, mSlowControlDb, mElectronicsDb, mSector);
   //
   // Set the function for the induced charge on Pad
   // -->StTrsSlowAnalogSignalGenerator::endo
   //-->StTrsSlowAnalogSignalGenerator::gatti
   //-->StTrsSlowAnalogSignalGenerator::dipole
    //dynamic_cast<StTrsSlowAnalogSignalGenerator*>(mAnalogSignalGenerator)->
    //setChargeDistribution(StTrsSlowAnalogSignalGenerator::endo);
   //
   // Set the function for the Analog Electronics signal shape
   //-->StTrsSlowAnalogSignalGenerator::delta
   //-->StTrsSlowAnalogSignalGenerator::symmetricGaussianApproximation
   //-->StTrsSlowAnalogSignalGenerator::symmetricGaussianExact
   //-->asymmetricGaussianApproximation
   //-->StTrsSlowAnalogSignalGenerator::realShaper
     //dynamic_cast<StTrsSlowAnalogSignalGenerator*>(mAnalogSignalGenerator)->
     //setElectronicSampler(StTrsSlowAnalogSignalGenerator::symmetricGaussianApproximation);
   mAnalogSignalGenerator->setDeltaRow(0);
   mAnalogSignalGenerator->setDeltaPad(1);
   mAnalogSignalGenerator->setSignalThreshold(.0001*(.001*volt));
   mAnalogSignalGenerator->setSuppressEmptyTimeBins(true);
	

   mDigitalSignalGenerator =
       StTrsFastDigitalSignalGenerator::instance(mElectronicsDb, mSector);

   //
   // Output is into an StTpcRawDataEvent* vector<StTrsDigitalSector*>
   // which is accessible via the StTrsUnpacker
   mUnPacker = new StTrsUnpacker;

   mAllTheData =
       new StTrsRawDataEvent();

   
    return StMaker::Init();
}


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

    //cout << "Make ofstream" << endl;
    //ofstream ofs("/star/u2b/lasiuk/geantdebug.txt", ios::out);
    //ofstream raw("/star/u2b/lasiuk/event.txt",ios::out);
    if (!m_DataSet->GetList())  {//if DataSet is empty fill it
    //
    // Read the Ionization
    //
    St_DataSetIter geant(gStChain->DataSet("geant"));
    // $STAR/pams/sim/idl/g2t_tpc_hit.idl 
    
    St_g2t_tpc_hit *g2t_tpc_hit = (St_g2t_tpc_hit *) geant("g2t_tpc_hit");
    // $STAR/StRoot/base/St_DataSet.h & St_Table.h 
    int no_tpc_hits         =  g2t_tpc_hit->GetNRows();
    g2t_tpc_hit_st *tpc_hit =  g2t_tpc_hit->GetTable();

    //StTpcCoordinateTransform transformer(mGeometryDb, mSlowControlDb);


    int bisdet, bsectorOfHit, bpadrow;

    // Where is the first hit in the TPC
    whichSector(tpc_hit->volume_id, &bisdet, &bsectorOfHit, &bpadrow);
    int currentSectorProcessed = bsectorOfHit;

    // Limit the  processing to a fixed number of segments
    //no_tpc_hits = 20;
    for (int i=1; i<=no_tpc_hits; i++){
	cout << "--> tpc_hit:  " << i << endl;
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
	
	// Process until the next sector is reached.

	// Save time initially  - by not processing pseudo padrows
	if(bisdet) {
	    cout << "Segment in a pseudo-padRow. Skipping..." << endl;
	    tpc_hit++;
	    if(i != no_tpc_hits) continue;
	}
	//sleep(2);
	
	if( (currentSectorProcessed == bsectorOfHit) &&
	    (i                      != no_tpc_hits )) {
	    
	    int pID = 1;  // I need to know the PID for ionization calculations

	    // I can't believe this.  After such careful design of the coordinate
	    // transformation, I am reduced to this because I am fixed to GEANT
	    // coordinates.  This is the problem you get if you
	    // don't use a common data base
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
	    double beta = bsectorOfHit*M_PI/6.;  // (30 degrees)
	    double cb   = cos(beta);
	    double sb   = sin(beta);
	    double xp = hitPosition.x()*cb - hitPosition.y()*sb;
	    double yp = hitPosition.x()*sb + hitPosition.y()*cb;
	    
	    StThreeVector<double>
		sector12Coordinate(xp,yp,tpc_hit->x[2]);
	    
	    StThreeVector<double> hitMomentum(tpc_hit->p[0]*GeV,
					      tpc_hit->p[1]*GeV,
					      tpc_hit->p[2]*GeV);

	    // I need PID info here, otherwise it is a pion.
	    // This is needed for the ionization splitting!
	    // WARNING:  cannot use "abs" (not overloaded (double) for LINUX!
	    StTrsChargeSegment aSegment(sector12Coordinate,
					hitMomentum,
					(fabs(tpc_hit->de*GeV)),
					tpc_hit->ds*centimeter,
					pID);
	    
// 	    PR(hitMomentum.mag());

// 	    ofs << " " << aSegment << endl;
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
	    //
	    // Better be smarter and  split the segment differnetly
	    // on the inner and outer part of the sectors...
	    //
	    int breakNumber = 1;
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
// 		PR(*iter);
		
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
	    
	    tpc_hit++;  // increase the pointer to the next hit
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
// 	PR(currentSectorProcessed);
// 	PR(mAllTheData->mSectors.size());
	
	mAllTheData->mSectors[(currentSectorProcessed-1)] = aDigitalSector;
	
	// Clear and reset for next sector:
	mWireHistogram->clear();
	mSector->clear();
	
	//
	// Go to the next sector
	currentSectorProcessed = bsectorOfHit;

	//
	// you can skip out here if you only want to process a single sector...
	if(currentSectorProcessed>3)
	    break;  // Finish here
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
			cout << " " << kk
			     << " " << zz << '\t'
			     << '\t' << static_cast<int>(*(listOfSequences[kk].firstAdc)) << endl;
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
  
    cout << "Got to the end of the maker" << endl;
  
    // Pass the decoder and data
    if (m_DataSet) delete m_DataSet;
    m_DataSet =  new St_DataSet(GetName());
    m_DataSet->Add(new St_ObjectSet("Event", mAllTheData));
    m_DataSet->Add(new St_ObjectSet("Decoder", mUnPacker));
  
    return kStOK;
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

void StTrsMaker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: StTrsMaker.cxx,v 1.15 1999/02/17 17:02:16 lasiuk Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}
