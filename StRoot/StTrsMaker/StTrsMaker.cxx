// $Id: StTrsMaker.cxx,v 1.11 1999/02/10 18:01:31 lasiuk Exp $
//
// $Log: StTrsMaker.cxx,v $
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
// Revision 1.6  1999/01/23 18:47:21  fisyak
// Cleanup for SL98l
//
// Revision 1.5  1999/01/23 05:03:18  lasiuk
// ready for test
//
// Revision 1.4  1999/01/23 02:33:03  lasiuk
// sun compatible
//
// Revision 1.3  1999/01/22 23:37:50  lasiuk
// root does not eat dynamic_cast<>
//
// Revision 1.2  1999/01/22 22:05:08  fisyak
// Rename
//
// Revision 1.1  1999/01/22 21:31:57  lasiuk
// name change
//
// Revision 1.3  1999/01/22 08:45:14  lasiuk
// example
//
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
#include <unistd.h>    // needed for access()

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

static const char rcsid[] = "$Id: StTrsMaker.cxx,v 1.11 1999/02/10 18:01:31 lasiuk Exp $";

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
   mWireHistogram->setGasGainInnerSector(8000);  // True by default
   mWireHistogram->setGasGainOuterSector(2000);
   mWireHistogram->setDoTimeDelay(false);
   
    // create a Sector:
    // Analog (for calculation)
   mSector = 
       new StTrsSector(mGeometryDb);

   // Output is into an StTpcRawDataEvent* vector<StTrsDigitalSector*>
   // which is accessible via the StTrsUnpacker
   mUnPacker = new StTrsUnpacker;

   mAllTheData =
       new StTrsRawDataEvent();
    //
    // Processes
    //
    mChargeTransporter =
      StTrsFastChargeTransporter::instance(mGeometryDb, mSlowControlDb, mGasDb, mMagneticFieldDb);
    // set status:
    //mChargeTransporter->setChargeAttachment(true);
    //mChargeTransporter->setGatingGridTransparency(true);
    //mChargeTransporter->setTransverseDiffusion(true);
    //mChargeTransporter->setLongitudinalDiffusion(true);
    //mChargeTransporter->setExB(true);


    mAnalogSignalGenerator =
	StTrsSlowAnalogSignalGenerator::instance(mGeometryDb, mSlowControlDb, mElectronicsDb, mSector);
    //
    // Set the function for the induced charge on Pad
    //
    //dynamic_cast<StTrsSlowAnalogSignalGenerator*>(mAnalogSignalGenerator)->
        //setChargeDistribution(StTrsSlowAnalogSignalGenerator::endo);
	//setChargeDistribution(StTrsSlowAnalogSignalGenerator::gatti);
	//setChargeDistribution(StTrsSlowAnalogSignalGenerator::dipole);
    //
    // Set the function for the Analog Electronics signal shape
    //
    //dynamic_cast<StTrsSlowAnalogSignalGenerator*>(mAnalogSignalGenerator)->
	//setElectronicSampler(StTrsSlowAnalogSignalGenerator::delta);
        //setElectronicSampler(StTrsSlowAnalogSignalGenerator::symmetricGaussianApproximation);
	//setElectronicSampler(StTrsSlowAnalogSignalGenerator::symmetricGaussianExact);
	//setElectronicSampler(StTrsSlowAnalogSignalGenerator::asymmetricGaussianApproximation);
	//setElectronicSampler(StTrsSlowAnalogSignalGenerator::realShaper); 
    mAnalogSignalGenerator->setDeltaRow(0);
    mAnalogSignalGenerator->setDeltaPad(0);
    mAnalogSignalGenerator->setSignalThreshold(.0001*(.001*volt));
    mAnalogSignalGenerator->setSuppressEmptyTimeBins(true);
	

    mDigitalSignalGenerator =
	StTrsFastDigitalSignalGenerator::instance(mElectronicsDb, mSector);

    return StMaker::Init();
}


void StTrsMaker::whichSector(int volId, int* isDet, int* sector, int* padrow){

    //cout << "StTrsMaker::whichSector()" << endl;
    //cout << volId << endl;
    *isDet  = (volId/100000);

    volId  -= (*isDet)*100000;
    *sector = volId/100;

    volId  -= (*sector)*100;
    *padrow = volId;
	
}
    
Int_t StTrsMaker::Make(){
//  PrintInfo();
  if (!m_DataSet->GetList())  {//if DataSet is empty fill it
    //
    // Read the Ionization
    //
    St_DataSetIter geant(gStChain->DataSet("geant"));
    // $STAR/pams/sim/idl/g2t_tpc_hit.idl 
    
    St_g2t_tpc_hit *g2t_tpc_hit = (St_g2t_tpc_hit *) geant("g2t_tpc_hit");
    // $STAR/StRoot/base/St_DataSet.h & St_Table.h 
    Int_t no_tpc_hits =  g2t_tpc_hit->GetNRows();
    g2t_tpc_hit_st *tpc_hit =  g2t_tpc_hit->GetTable();

    //StTpcCoordinateTransform transformer(mGeometryDb, mSlowControlDb);


    cout << "Loop over: " << no_tpc_hits << endl;
    cout << "First sector" << endl;
    int currentSectorProcessed;
    //int nextSectorProcessed;
      
    int bisdet, bsectorOfHit, bpadrow;

    // Where is the first hit in the TPC
    PR(tpc_hit->id);
    whichSector(tpc_hit->volume_id, &bisdet, &bsectorOfHit, &bpadrow);
    PR(bisdet);
    PR(bsectorOfHit);
    PR(bpadrow);
    currentSectorProcessed = bsectorOfHit;
    PR(currentSectorProcessed);

    // Limit the  processing to a fixed number of segments
    //no_tpc_hits = 20;
    for (int i=1; i<=no_tpc_hits; i++){
	cout << "********************tpc_hit number:  " << i << "  ************" << endl;
// 	cout << tpc_hit->id                << ' '
// 	     << (tpc_hit->de/eV)           << ' '
// 	     << (tpc_hit->ds/centimeter)   << ' '
// 	     << tpc_hit->volume_id         << endl;


	whichSector(tpc_hit->volume_id, &bisdet, &bsectorOfHit, &bpadrow);
// 	PR(bisdet);
// 	PR(bsectorOfHit);
// 	PR(bpadrow);
	
	// Process until the next sector is reached.
// 	PR(currentSectorProcessed);
// 	PR(bsectorOfHit);

	// Save time initially  - by not processing pseudo padrows
	if(bisdet) {
	    cout << "Segment in a pseudo-padRow. Skipping..." << endl;
	    tpc_hit++;
	    if(i != no_tpc_hits) continue;
	}
	
	if( (currentSectorProcessed == bsectorOfHit) &&
	    (i                      != no_tpc_hits )) {
	    
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
	    double zPosition = GEANTDriftLength/2. + tpc_hit->x[2]*centimeter + GEANTOffSet;
// 	    PR(tpc_hit->x[2]*centimeter);
// 	    PR(zPosition);
	    
	    StThreeVector<double> hitPosition(tpc_hit->x[0]*centimeter,
					      tpc_hit->x[1]*centimeter,
					      zPosition); 
// 	    PR(hitPosition);

// 	    // Drift Length is calculated with respect to the FG!
// 	    double fgOffSet = (bpadrow <= mGeometryDb->numberOfInnerRows()) ?
// 		mGeometryDb->innerSectorFrischGridPadPlaneSeparation() :
// 		mGeometryDb->innerSectorFrischGridPadPlaneSeparation();

// 	    PR(mGeometryDb->radialDistanceAtRow(bpadrow));
// 	    PR(mGeometryDb->radialDistanceAtRow(bpadrow)/centimeter);
// 	    PR(mGeometryDb->frischGrid());
	    
	    StThreeVector<double>
		sector12Coordinate(tpc_hit->x[0]*centimeter,
				   tpc_hit->x[1]*centimeter + mGeometryDb->radialDistanceAtRow(bpadrow),
				   zPosition);
	    //tpc_hit->x[2]*centimeter + mGeometryDb->frischGrid() + fgOffSet);
	    
	    StThreeVector<double> hitMomentum(tpc_hit->p[0]*GeV,
					      tpc_hit->p[1]*GeV,
					      tpc_hit->p[2]*GeV);

	    // I need PID info here, otherwise it is a pion.  This is needed for the ionization splitting!
	    StTrsChargeSegment aSegment(sector12Coordinate,
					hitMomentum,
					(fabs(tpc_hit->de*GeV)),  // cannot use abs (not overloaded for LINUX!
					tpc_hit->ds*centimeter,
					pID);
	    
// 	    PR(hitMomentum.mag());
	    
	    PR(aSegment);
	    //sleep(2);
// 	    cout << "^^^^Segment Definition^^^^" << '\n' << endl;
	    
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
	    cout << "call StTrsSegment::split() into " << breakNumber << " segments." << endl;
	    aSegment.split(mGasDb, mMagneticFieldDb, breakNumber, &comp);
	    
#ifndef ST_NO_TEMPLATE_DEF_ARGS
	    copy(comp.begin(), comp.end(), ostream_iterator<StTrsMiniChargeSegment>(cout,"\n"));
#endif
	    cout << endl;
	    
	    cout << "Number of \"miniSegments\": " << (comp.size()) << endl;
	    
	    // Loop over the miniSegments
	    for(iter = comp.begin();
		iter != comp.end();
		iter++) {
		
// 		cout << endl;
// 		cout << " *iter " << (*iter) << endl;
 		PR(*iter);
		//
	        // TRANSPORT HERE
	        //
		mChargeTransporter->transportToWire(*iter);
		PR(*iter);
		
		//
		// CHARGE COLLECTION AND AMPLIFICATION
	        //
		
#ifndef __sun   // Bug in the sun iterators.  Must Explicitly dereference!
		StTrsWireBinEntry anEntry(iter->position(), iter->charge());
		PR(anEntry);
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
	
	cout << "\a**********End Of Current Sector***************\a\n" << endl;
	sleep(3);
	//
	// Generate the ANALOG Signals on pads
	//
	mAnalogSignalGenerator->inducedChargeOnPad(mWireHistogram);
	
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
	mDigitalSignalGenerator->digitizeSignal();

	//
	// Fill it into the event structure...
	// and you better check the sector number!
	PR(currentSectorProcessed);
	PR(mAllTheData->mSectors.size());
	
// 	cout << "Try add it" << endl;
	mAllTheData->mSectors[(currentSectorProcessed-1)] = aDigitalSector;
	//PR(mAllTheData->mSectors[(currentSectorProcessed-1)].size())
// 	cout << "okay" << endl;
	
	// Clear and reset for next sector:
	mWireHistogram->clear();
	mSector->clear();
	
	// Just temporary for debugging to make sure filled properly
// 	  pair<digitalTimeBins*, digitalTimeBins*>
// 	      tmpPad = mAllTheData->mSectors[(currentSectorProcessed-1)]->timeBinsOfRowAndPad(theRow,thePad);
// 	  PR(tmpPad.first->size());
// 	  PR(tmpPad.second->size());
// 	  break;

	//
	// Go to the nexe sector
	currentSectorProcessed = bsectorOfHit;
	
    } // loop over all segments: for(int i...
  } // mDataSet
  
  // The access stuff:

  //
  // Access it!
  // with:
  //   *mUnPacker  

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
	  PR(irow);
	  int numberOfPads = mUnPacker->getPadList(irow, &padList);
	  //PR(numberOfPads);
	      
	  if(!numberOfPads)
	      continue;  // That is, go to the next row...
	      
	  for(int ipad = 0; ipad<numberOfPads; ipad++) {
	      //PR(static_cast<int>(padList[ipad]));
	      int nseq;
		  
	      StSequence* listOfSequences;
	      int getSequencesStatus =
		  mUnPacker->getSequences(irow, padList[ipad], &nseq, &listOfSequences);
		      
	      //PR(getSequencesStatus);
		      
	      for(int kk=0; kk<nseq; kk++) {
		  //PR(listOfSequences[kk].length);
		  for(int zz=0; zz<listOfSequences[kk].length; zz++) {
		      cout << " " << kk
			   << " " << zz << '\t'
			   << '\t' << static_cast<int>(*(listOfSequences[kk].firstAdc)) << endl;
		      listOfSequences[kk].firstAdc++;
		  } // zz
		  cout << endl;
	      } // Loop kk
	  } // loop over pads
	  // Do the data manipulation here!
	  mUnPacker->clear();
      } // Loop over rows!
  } // Loop over sectors

  
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
  printf("* $Id: StTrsMaker.cxx,v 1.11 1999/02/10 18:01:31 lasiuk Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}
