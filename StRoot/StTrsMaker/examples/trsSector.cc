//*******************************************************/
// $Id: trsSector.cc,v 1.3 2003/09/02 17:59:15 perev Exp $
//
// Author: brian, October, 1998
//
// Purpose: Prototypes the operation of TRS and provides diagnostic
//          at several levels
//
// $Log: trsSector.cc,v $
// Revision 1.3  2003/09/02 17:59:15  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.2  1998/11/13 21:33:06  lasiuk
// update
//
/********************************************************/
#include <Stiostream.h>
#include <unistd.h>    // needed for access()
// #include <math.h>

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


#define VERBOSE 1
#define ivb if(VERBOSE)

// #define sINGLE
// #ifdef SINGLE
// const int ROWS = 1;
// const int PADS[ROWS] = {1};
// const int SIGNALS    = 1;
// const int TBINS      = 10;
// #endif


void printPad(StTrsSector *a)
{
    ostream_iterator<StTrsAnalogSignal> out(cout,",");
    
    for(int irow=0; irow<a->size(); irow++)
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

    string scFile("../run/sc.conf");         // contains B field
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
    scDb->print();

    StMagneticField *magDb =
	StSimpleMagneticField::instance(scFile.c_str());

    StTpcElectronics *electronicsDb =
	StTpcSimpleElectronics::instance(electronicsFile.c_str());

    
    string gas("Ar");
    StTrsDeDx myEloss(gas);


    //
    // create a Sector:
    //
    StTrsSector *sector = new StTrsSector(geomDb);
    
    //
    // Processes
    //
    StTrsChargeTransporter *trsTransporter =
	StTrsFastChargeTransporter::instance(geomDb, scDb, &myEloss, magDb);
    // set status:
//     trsTransporter->setChargeAttachment(true);
//     trsTransporter->setGatingGridTransparency(true);
//     trsTransporter->setTransverseDiffusion(true);
//     trsTransporter->setLongitudinalDiffusion(true);
//     trsTransporter->setExB(true);

    StTrsWireHistogram *myWireHistogram =
	StTrsWireHistogram::instance(geomDb, scDb);
//     myWireHistogram->setDoGasGain(true);  // True by default
//     myWireHistogram->setDoGasGainFluctuations(false);
//     myWireHistogram->setDoTimeDelay(false);

    StTrsAnalogSignalGenerator *trsAnalogSignalGenerator =
	StTrsSlowAnalogSignalGenerator::instance(geomDb, scDb, electronicsDb, sector);
//     trsAnalogSignalGenerator->setDeltaRow(0);
//     trsAnalogSignalGenerator->setDeltaPad(0);
//     trsAnalogSignalGenerator->setSignalThreshold(.0001);
//     trsAnalogSignalGenerator->setSuppressEmptyTimeBins(true);
    //     ??should the type of function be an option ???
	
    StTrsDigitalSignalGenerator *trsDigitalSignalGenerator =
	StTrsFastDigitalSignalGenerator::instance(electronicsDb, sector);

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
	
    }while(true);

    cout << "\a***************************\a\n" << endl;

    //
    // Generate the ANALOG Signals on pads
    //
    trsAnalogSignalGenerator->inducedChargeOnPad(myWireHistogram);

    trsAnalogSignalGenerator->sampleAnalogSignal();


    //
    // Digitize the Signals
    //
    trsDigitalSignalGenerator->digitizeSignal();

    //
    // Write it out!
    //

    cout << "Write out the Sector " << endl;
    for(irow=0; irow<sector->numberOfRows(); irow++) {
      PR(irow);
      for(ipad=0; ipad<sector->numberOfPadsInRow(irow); ipad++) {
	tpcTimeBins currentTimeBins = sector->timeBinsOfRowAndPad(irow, ipad);
	for(itbin=0; itbin<currentTimeBins.size(); itbin++) {
	  if(currentTimeBins[itbin].amplitude() > 1.)
	    PR(currentTimeBins[itbin]);
	}
      }
    }
    
    return 0;
}
