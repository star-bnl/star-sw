// $Id: St_trs_Maker.cxx,v 1.2 1998/11/25 21:58:30 fisyak Exp $
// $Log: St_trs_Maker.cxx,v $
// Revision 1.2  1998/11/25 21:58:30  fisyak
// Cleanup
//
// Revision 1.2  1998/11/25 21:58:30  fisyak
// Cleanup
//
// Revision 1.1  1998/11/10 17:11:57  fisyak
// Put Brian trs versin into StRoot
//
// Revision 1.7  1998/10/31 00:25:45  fisyak
// Makers take care about branches
//
// Revision 1.6  1998/10/06 18:00:29  perev
// cleanup
//
// Revision 1.5  1998/10/02 13:46:08  fine
// DataSet->DataSetIter
//
// Revision 1.4  1998/08/14 15:25:58  fisyak
// add options
//
// Revision 1.3  1998/08/10 02:32:07  fisyak
// Clean up
//
// Revision 1.2  1998/07/20 15:08:15  fisyak
// Add tcl and tpt
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_trs_Maker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "St_trs_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"

#include <iostream.h>
#include <unistd.h>    // needed for access()

#include <string>
#include <vector>
#include <utility>    // pair
#include <algorithm>  // min() max()

// SCL
#include "Randomize.h"
#include "StHbook.hh"

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
#include "StTrsAnalogSignal.hh"
#include "StTrsWireBinEntry.hh"
#include "StTrsWireHistogram.hh"

#include "StTrsSector.hh"
// g2t tables
#include "St_g2t_tpc_hit_Table.h"
#include "St_g2t_track_Table.h"

#define VERBOSE 1
#define ivb if(VERBOSE)

// #define sINGLE
// #ifdef SINGLE
// const int ROWS = 1;
// const int PADS[ROWS] = {1};
// const int SIGNALS    = 1;
// const int TBINS      = 10;
// #endif


ClassImp(St_trs_Maker)

//_____________________________________________________________________________
St_trs_Maker::St_trs_Maker(const char *name, const char *title):StMaker(name,title){
   drawinit=kFALSE;
}
//_____________________________________________________________________________
St_trs_Maker::~St_trs_Maker(){
}
//_____________________________________________________________________________
Int_t St_trs_Maker::Init(){
// Create tables
   St_DataSetIter       local(gStChain->DataSet("params"));

   const int tupleSize = 4;
#if 0
   StHbookFile hbookFile("hbook");
   StHbookTuple theTuple("signal", tupleSize);
   float tuple[tupleSize];
   theTuple << "row" << "pad" << "time" << "sig" << book;
#endif

   int irow, ipad, itbin;   // ctrs


   //
   // Make the DataBase
   //
   // Check File access
   //
   string geoFile("../run/TPCgeo.conf");
   if (access(geoFile.c_str(),R_OK)) {
     cerr << "ERROR:\n" << geoFile << " cannot be opened" << endl;
     //shell(pwd);
     cerr << "Exitting..." << endl;
     exit(1);
   }

   string scFile("../run/example.conf");         // contains B field
   if (access(scFile.c_str(),R_OK)) {
     cerr << "ERROR:\n" << scFile << " cannot be opened" << endl;
     cerr << "Exitting..." << endl;
     exit(1);
   }

   string electronicsFile("../run/electronics.conf");
   if (access(electronicsFile.c_str(),R_OK)) {
     cerr << "ERROR:\n" << electronicsFile << " cannot be opened" << endl;
     cerr << "Exitting..." << endl;
     exit(1);
   }

   //
   // The DataBases
   //
   mGeometryDb =
     StTpcSimpleGeometry::instance(geoFile.c_str());

    mSlowControlDb =
	StTpcSimpleSlowControl::instance(scFile.c_str());

    mMagneticFieldDb =
	StSimpleMagneticField::instance(scFile.c_str());

    mElectronicsDb =
	StTpcSimpleElectronics::instance(electronicsFile.c_str());

    
    string gas("Ar");
    mGasDb = new StTrsDeDx(gas);

    //
    // Containers
    //

    // create a Sector:
    mSector = 
      new StTrsSector(mGeometryDb);

    mWireHistogram =
      StTrsWireHistogram::instance(mGeometryDb, mSlowControlDb);
//     mWireHistogram->setDoGasGain(true);  // True by default
//     mWireHistogram->setDoGasGainFluctuations(false);
//     mWireHistogram->setDoTimeDelay(false);

    
    //
    // Processes
    //
    mChargeTransporter =
      StTrsFastChargeTransporter::instance(mGeometryDb, mSlowControlDb, mGasDb, mMagneticFieldDb);
    // set status:
//     mChargeTransporter->setChargeAttachment(true);
//     mChargeTransporter->setGatingGridTransparency(true);
//     mChargeTransporter->setTransverseDiffusion(true);
//     mChargeTransporter->setLongitudinalDiffusion(true);
//     mChargeTransporter->setExB(true);


    mAnalogSignalGenerator =
	StTrsSlowAnalogSignalGenerator::instance(mGeometryDb, mSlowControlDb, mElectronicsDb, mSector);
//     mAnalogSignalGenerator->setDeltaRow(0);
//     mAnalogSignalGenerator->setDeltaPad(0);
//     mAnalogSignalGenerator->setSignalThreshold(.0001);
//     mAnalogSignalGenerator->setSuppressEmptyTimeBins(true);
    //     ??select the type of function??
	
    // CAREFUL pass mSector!!!
    mDigitalSignalGenerator =
	StTrsFastDigitalSignalGenerator::instance(mElectronicsDb, mSector);

// Create Histograms    

    return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_trs_Maker::Make(){
//  PrintInfo();
  if (!m_DataSet->GetList())  {//if DataSet is empty fill it
    //
    // Generate the Ionization
    //
    St_DataSetIter geant(gStChain->DataSet("geant"));
    // $STAR/pams/sim/idl/g2t_tpc_hit.idl 
    St_g2t_tpc_hit *g2t_tpc_hit = (St_g2t_tpc_hit *) geant("g2t_tpc_hit");
    Int_t no_tpc_hits =  g2t_tpc_hit->GetNRows(); // $STAR/StRoot/base/St_DataSet.h & St_Table.h 
    g2t_tpc_hit_st *tpc_hit =  g2t_tpc_hit->GetTable();
    for (Int_t i=0; i< no_tpc_hits; i++){
      printf("id =%i  de%f  tof=%f \n",tpc_hit->id,tpc_hit->de,tpc_hit->tof);
      tpc_hit++;
    }
    St_g2t_track   *g2t_track   = (St_g2t_track *)   geant("g2t_track");
    
    float maxDistance = mGeometryDb->lastOuterSectorAnodeWire();
    PR(maxDistance);
    float zPosition = 1.*meter;
    float position = 52.*centimeter;
    float dS;
    do {
	dS = 500.*mGasDb->nextInteraction();
// 	PR(dS);
	position += dS;

	if(position>maxDistance) break;

	double primaryEnergyDistribution;
	int totalElectrons = mGasDb->secondary(&primaryEnergyDistribution) + 1;
 	PR(totalElectrons);

	// Make a StTrsMiniChargeSegment (the thing that must be transported)
	StTrsMiniChargeSegment
	    aMiniSegment(StThreeVector<double>(0, position, zPosition),
			 totalElectrons,  // q
			 0);              // dl
	PR(aMiniSegment);

	//
	// TRANSPORT HERE
	//
	mChargeTransporter->transportToWire(aMiniSegment);
	PR(aMiniSegment);

	//
	// CHARGE COLLECTION AND AMPLIFICATION
	//
	StTrsWireBinEntry anEntry(aMiniSegment.position(), aMiniSegment.charge());
 	PR(anEntry);
	mWireHistogram->addEntry(anEntry);
	
    }while(TRUE);

    cout << "\a***************************\a\n" << endl;

    //
    // Generate the ANALOG Signals on pads
    //
    mAnalogSignalGenerator->inducedChargeOnPad(mWireHistogram);

    mAnalogSignalGenerator->sampleAnalogSignal();


    //
    // Digitize the Signals
    //
    mDigitalSignalGenerator->digitizeSignal();


    //
    // Write it out!
    //
    
}
 return kStOK;
}
//_____________________________________________________________________________
void St_trs_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_trs_Maker.cxx,v 1.2 1998/11/25 21:58:30 fisyak Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}

