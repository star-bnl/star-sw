/******************************************************
 * $Id: maker.cxx,v 1.2 2000/02/08 16:35:29 lasiuk Exp $
 * Description:
 *  Stand-alone test module
 *
 * $Log: maker.cxx,v $
 * Revision 1.2  2000/02/08 16:35:29  lasiuk
 * Hp compatible
 *
 * Revision 1.1  2000/01/27 17:10:59  lasiuk
 * stand-alone maker
 *
 ******************************************************/
#define RICH_DIAGNOSTIC 1
#define RICH_DECODE_DATA 1
#define RICH_HISTOGRAM 1

#ifdef RICH_HISTOGRAM
#include "StHbook.hh"
#endif
#include <iostream.h>
#include <string>

#ifndef ST_NO_NAMESPACES
using std::string;
#endif

// dbs
#include "StRichPhysicsDb.h"
#include "StRichGeometryDb.h"

#include "StRichGHit.h"

//#include "StRichFilter.h"
#include "StRichIonization.h"
#include "StRichInduceSignal.h"
#include "StRichPadPlane.h"
#include "StRichNoiseSimulator.h"
#include "StRichAnalogToDigitalConverter.h"

// i/o
#include "StRichASCIIReader.h"
#include "StRichWriter.h"

#ifdef RICH_DECODE_DATA
#include "StRrsReader.h"
#endif
// SCL
#include "StGlobals.hh"
#include "StThreeVector.hh"

int main()
{
#ifdef RICH_HISTOGRAM
    cout << "Histogram" << endl;
    
    //
    //  Open histogram file and book tuple
    //
    string fname = "hbook";
    StHbookFile *hbookFile =
	new StHbookFile(fname.c_str());

    //
    // Unpacker Data
    const int tupleSize1 = 8;
    float tuple[tupleSize1];
    StHbookTuple *theTuple  =
	new StHbookTuple("data", tupleSize1);
    *theTuple << "x" << "y" << "z" << "xx" << "yy" << "zz" << "q" << "vid" << book;

    //
//     const int tupleSize2 = 3;
//     float tuple2[tupleSize2];
//     StHbookTuple *secTuple  =
// 	new StHbookTuple("dedx", tupleSize2);

//     *secTuple << "sec" << "row" << "x" << book;
#endif
    // DBs are here    
    StRichPhysicsDb* myPhysicsDb   = StRichPhysicsDb::getDb();
    StRichGeometryDb* myGeometryDb = StRichGeometryDb::getDb();

    if ( !myGeometryDb ) {
      cerr << "Geometry database could not be initialized. Aborting!!!\n";
      return 1;
    }

    if ( !myPhysicsDb ) {
      cerr << "Physics database could not be initialized. Aborting!!!\n";
      return 1;
    }

    myGeometryDb->print();
    myPhysicsDb->print();
    //exit(0);

    //Process Selection
    //StRichFilter myFilter;
    StRichIonization   myIonize;
    StRichInduceSignal myInduceSignal;
    
    // ADC
    StRichAnalogToDigitalConverter myADC;
    myADC.setAddPedestal(0);  // adds a DC level to each pad!
    
    // PadPlane
    StRichPadPlane* myPadPlane =
	new StRichPadPlane(2*myGeometryDb->n_pad_x, 2*myGeometryDb->n_pad_z);

    // Data Writer is here
    StRichWriter* myWriter = StRichWriter::getInstance(myPadPlane);
    
    if ( !myWriter ) {
	cerr << "Output module could not be initialized. Aborting!!!\n";
	return 1;
    }
    myWriter->clear();


    //*************************************************************//
    cout << " -- Begin Processing --" << endl;
#ifdef RICH_DIAGNOSTIC
    ofstream raw("data.txt");
#endif
    StRichGHit hit;
    StRichGHit theRealHit;

    string theFile("/home/star/lasiuk/richsoftware/data/geantdata.txt");
    StRichASCIIReader input(theFile);

    string volumeName;
    int    quadrant;

    int numberOfHits = 10;
    int vid;
    //while( input(hit) == 0 )  {   // if OK
    for(int ii=0; ii<numberOfHits; ii++) {
	cout << "************ " << ii << endl;
	int status = input(hit);
	cout << hit << endl;
	hit.full(cout);
	cout << endl;
	
	theRealHit.fill(hit.position().x(),
			hit.position().y(),
			hit.position().z(),
			hit.quadrant(),
			hit.cosX(),
			hit.cosY(),
			hit.cosZ(),
			hit.ds(),
			hit.dE(),
			hit.id(),
			hit.volumeID());
	theRealHit.full(cout);;
	cout << endl;
	cout << "vvvvvvvvvvvvvvvvvvvv" << endl;

#ifdef RICH_HISTOGRAM
// 	tuple[0] = static_cast<float>(hit.x().x());
// 	tuple[1] = static_cast<float>(hit.x()y);
// 	tuple[2] = static_cast<float>(hit.z);
// 	tuple[3] = static_cast<float>(hit.xx);
// 	tuple[4] = static_cast<float>(hit.yy);
// 	tuple[5] = static_cast<float>(hit.zz);
// 	tuple[6] =
// 	    static_cast<float>(hit.quad);
// 	if(hit.mVolumeID == "RCSI")
// 	    vid = 2;
// 	else
// 	    vid = 1;
// 	tuple[7] = static_cast<float>(vid);
// 	theTuple->fill(tuple);
#endif

	//myFilter(theRealHit);
	PR(hit.volumeID());
	if ( hit.volumeID() != "RCSI" ) {
	    cout << "call myIonize()" << endl;
	    myIonize( hit );		 
	}
	else {
	    //
	    // check if it is photon
	    PR(hit.dE());
	    if ( hit.dE() > 0 )
		cout << "call myInduceSignal()" << endl;
		myInduceSignal ( hit );
	}
    
#ifdef RICH_WITH_VIEWER
    if (StRichViewer::histograms ) 
	StRichViewer::getView()->mPadPlane->Fill( hit.position().z(), hit.position().x(), 1);
#endif
	
    }  // loop over hits


    cout << "Try Write...Finish Looping over all hits." << endl;
    for ( int irow = 0; irow < myWriter->rows(); irow++ ) {
	for ( int jpad = 0; jpad < myWriter->cols(); jpad++ ) {
	    //mWriter->getSignal(i,j).signal +=  mNoiseSimulator();


		myWriter->getSignal(irow,jpad).signal =
		    myADC( myWriter->getSignal(irow,jpad).signal );

		if(myWriter->getSignal(irow,jpad).signal >0) {
		    cout << "maker::getSignal (r,p): (" << irow << ", " << jpad << ")=> "
			 << myWriter->getSignal(irow,jpad).signal << endl;

	    }
	}
    }
#ifdef RICH_DECODE_DATA
    int version = 1;
    unsigned int theADCValue = 0;
    
    StRrsReader theReader(myPadPlane,version);
    cout << "DECODER " << endl;
    for(int iRow=0; iRow<(myGeometryDb->numberOfRowsInAColumn()); iRow++) {  // 96
	for(int iCol=0; iCol<(myGeometryDb->numberOfPadsInARow()) ; iCol++) {
	    
	    theADCValue = theReader.GetADCFromCoord(iRow,iCol);
	    if(theADCValue) {
		//cout << "r/c/adc: " << iRow << ' ' << iCol << ' ' << theADCValue << endl;
#ifdef RICH_DIAGNOSTIC
		raw << "r/c/adc: "
		    << iRow << ' '
		    << iCol << ' '
		    << theADCValue << endl;
		anIDList MCInfo = theReader.GetMCDetectorInfo(iRow, iCol);
		anIDList::iterator iter;
		for(iter = MCInfo.begin();
		    iter!= MCInfo.end();
		    iter++) {
		    raw << ">>* MCinfo.G_ID= " << iter->G_ID
			<< "MCinfo.amount= "   << iter->amount << endl;
		}
#endif
		
	    }
#endif


	}
    } // 96


    // clean up
    delete myGeometryDb;
    delete myPhysicsDb;
    delete myWriter;
    delete myPadPlane;

#ifdef RICH_HISTOGRAM
    cout << "Save and close " << endl;
    hbookFile->saveAndClose();
#endif
    
    return 0;
} 

