//*******************************************************/
// Program: prf.cc
//
// Purpose: Prototypes the operation of StTrsSector
//          in filling and data manipulation
//
// Modifications
// 10-24-98
// :: Handle signals instead of floats
// :: Signal generating mechanism
/********************************************************/
#include <iostream.h>
#include <unistd.h>    // needed for access()
//#include <fstream.h> // streams
//#include <stdlib.h>
// #include <math.h>
// #include <stdio.h>

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

// containers
#include "StTrsAnalogSignal.hh"
#include "StTrsWireBinEntry.hh"
#include "StTrsWireHistogram.hh"

#include "StTrsSector.hh"


#define VERBOSE 1
#define ivb if(VERBOSE)

// #define sINGLE
// #ifdef SINGLE
// const int ROWS = 1;
// const int PADS[ROWS] = {1};
// const int SIGNALS    = 1;
// const int TBINS      = 10;
// #endif


void printPad(tpcSector& a)
{
    ostream_iterator<StTrsAnalogSignal> out(cout,",");
    
    for(int irow=0; irow<a.size(); irow++)
	cout << irow << " " <<  "<" << a[irow].size() << "> ";

// 	for(int ipad=0; ipad<a[irow].size(); ipad++) {
// 	    cout << irow << " " << ipad << "<" << a[irow][ipad].size() << "> ";
// 	    //copy(a[irow][ipad].begin(),a[irow][ipad].end(),out);
// 	    cout << endl;
// 	}
}

// Sort the "analogSignal"s on a pad according to the time
// void sortTime(sector& a)
// {
//     for(int irow=0; irow<a.size(); irow++)
// 	for(int ipad=0; ipad<PADS[irow]; ipad++) {
// 	    sort(a[irow][ipad].begin(),a[irow][ipad].end(),comp_last());
// 	}
// }


/* -------------------------------------------------------------------- */
/*                         Main Program                                 */
/* -------------------------------------------------------------------- */
int main (int argc,char* argv[])
{
    const int tupleSize = 4;
    StHbookFile hbookFile("hbook");
    StHbookTuple theTuple("signal", tupleSize);
    float tuple[tupleSize];
    theTuple << "row" << "pad" << "time" << "sig" << book;

    vector<StTrsAnalogSignal>          timeBins;
    vector<vector<StTrsAnalogSignal> > pad;
    tpcSector                          sector;  // 3 dimensional vector<>

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
    StTpcGeometry *geomDb =
	StTpcSimpleGeometry::instance(geoFile.c_str());

    StTpcSlowControl *scDb =
	StTpcSimpleSlowControl::instance(scFile.c_str());

    StMagneticField *magDb =
	StSimpleMagneticField::instance(scFile.c_str());

    StTpcElectronics *electronicsDb =
	StTpcSimpleElectronics::instance(electronicsFile.c_str());

    
    string gas("Ar");
    StTrsDeDx myEloss(gas);


    //
    // create a Sector:
    //
    tpcSector sector;
    
    //
    // Processes
    //
    StTrsChargeTransporter *trsTransporter =
	StTrsFastChargeTransporter::instance(geomDb, scDb, &myEloss, magDb);
    // set status:
//     trsTransporter->setChargeAttachment(TRUE);
//     trsTransporter->setGatingGridTransparency(TRUE);
//     trsTransporter->setTransverseDiffusion(TRUE);
//     trsTransporter->setLongitudinalDiffusion(TRUE);
//     trsTransporter->setExB(TRUE);

    StTrsWireHistogram *myWireHistogram =
	StTrsWireHistogram::instance(geomDb, scDb);
//     myWireHistogram->setDoGasGain(TRUE);  // True by default
//     myWireHistogram->setDoGasGainFluctuations(FALSE);
//     myWireHistogram->setDoTimeDelay(FALSE);

    StTrsAnalogSignalGenerator *trsSignalGenerator =
	StTrsSlowAnalogSignalGenerator::instance(geomDb, scDb, electronicsDb, sector);
//     trsSignalGenerator->setDeltaRow(0);
//     trsSignalGenerator->setDeltaPad(0);
//     trsSignalGenerator->setSignalThreshold(.0001);
//     trsSignalGenerator->setSuppressEmptyTimeBins(TRUE);
    //     ??select the type of function?
	
    StTrsDigitalSignalGenerator *trsDigitalGenerator =
	StTrsFastSignalGenerator::instance(electronicsDb, sector);

    //
    // Generate the Ionization
    //
    
    float maxDistance = geomDb->lastOuterSectorAnodeWire();
    PR(maxDistance);
    float zPosition = 1.*meter;
    float position = 52.*centimeter;
    float dS;
    do {
	dS = 500.*myEloss.nextInteraction();
// 	PR(dS);
	position += dS;

	if(position>maxDistance) break;

	double primaryEnergyDistribution;
	int totalElectrons = myEloss.secondary(&primaryEnergyDistribution) + 1;
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
	trsTransporter->transportToWire(aMiniSegment);
	PR(aMiniSegment);

	//
	// CHARGE COLLECTION AND AMPLIFICATION
	//
	StTrsWireBinEntry anEntry(aMiniSegment.position(), aMiniSegment.charge());
 	PR(anEntry);
	myWireHistogram->addEntry(anEntry);
	
    }while(TRUE);

    cout << "\a***************************\a\n" << endl;

    //
    // Generate the ANALOG Signals on pads
    //
    trsSignalGenerator->inducedChargeOnPads(myWireHistogram);

    trsSignalGenerator->sampleAnalogSignal();


    //
    // Digitize the Signals
    //
    trsDigitizer->digitize(sector);


    //
    // Write it out!
    //
    
    return 0;
}
