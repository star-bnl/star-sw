/******************************************************
 * $Id: maker.cxx,v 1.1 2000/01/27 17:10:59 lasiuk Exp $
 * Description:
 *  Stand-alone test module
 *
 * $Log: maker.cxx,v $
 * Revision 1.1  2000/01/27 17:10:59  lasiuk
 * stand-alone maker
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

#if defined (__SUNPRO_CC) && __SUNPRO_CC >= 0x500
using std::string;
#endif

// dbs
#include "StRichPhysicsDb.h"
#include "StRichGeometryDb.h"

#include "StRichGHit.h"

#include "StRichFilter.h"
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
    StRichFilter myFilter;
    
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

    string theFile("/home/star/lasiuk/richsoftware/data/geantdata.txt");
    StRichASCIIReader input(theFile);

    string volumeName;
    int    quadrant;

    int numberOfHits = 1000;
    int vid;
    //while( input(hit) == 0 )  {   // if OK
    for(int ii=0; ii<numberOfHits; ii++) {
	//PR(hit);
	int status = input(hit);
	cout << hit.mVolumeID << endl;
#ifdef RICH_HISTOGRAM
	tuple[0] = static_cast<float>(hit.x);
	tuple[1] = static_cast<float>(hit.y);
	tuple[2] = static_cast<float>(hit.z);
	tuple[3] = static_cast<float>(hit.xx);
	tuple[4] = static_cast<float>(hit.yy);
	tuple[5] = static_cast<float>(hit.zz);
	tuple[6] =
	    static_cast<float>(hit.quad);
	if(hit.mVolumeID == "RCSI")
	    vid = 2;
	else
	    vid = 1;
	tuple[7] = static_cast<float>(vid);
	theTuple->fill(tuple);
#endif

	myFilter(hit);
	    
    }  // loop over hits


    cout << "Try Write" << endl;
    for ( int i = 0; i < myWriter->rows(); i++ ) {
	for ( int j = 0; j < myWriter->cols(); j++ ) {
	    
	    //mWriter->getSignal(i,j).signal +=  mNoiseSimulator();
	    
	    myWriter->getSignal(i,j).signal =
		myADC( myWriter->getSignal(i,j).signal );
	}
    }
#ifdef RICH_DECODE_DATA
    int version = 1;
    unsigned int theADCValue = 0;
    
    StRrsReader theReader(myPadPlane,version);
    cout << "DECODER " << endl;
    for(int iRow=0; iRow<(2*myGeometryDb->n_pad_x); iRow++) {  // 96
	for(int iCol=0; iCol<(2*myGeometryDb->n_pad_z) ; iCol++) {
	    
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

