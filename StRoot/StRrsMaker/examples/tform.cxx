/******************************************************
 * $Id: tform.cxx,v 1.1 2000/02/08 16:35:27 lasiuk Exp $
 * Description:
 *  Stand-alone test module
 *
 * $Log: tform.cxx,v $
 * Revision 1.1  2000/02/08 16:35:27  lasiuk
 * Hp compatible
 *
 ******************************************************/
#define RICH_DIAGNOSTIC 1
#define RICH_DECODE_DATA 1
#define RICH_HISTOGRAM 1

#define DEBUG 0
#define idb if(DEBUG)cout

#include "StGlobals.hh"
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
#include "StRichCoordinateTransform.h"
int main()
{
#ifdef RICH_HISTOGRAM
    cout << "Histogram" << endl;
    
    //
    //  Open histogram file and book tuple
    //
    string fname = "cood";
    StHbookFile *hbookFile =
	new StHbookFile(fname.c_str());

    //
    // Unpacker Data
    const int tupleSize1 = 21;
    float tuple[tupleSize1];
    StHbookTuple *theTuple  =
	new StHbookTuple("data", tupleSize1);
    *theTuple << "pad" << "row" << "quad"
	      << "xq" << "yq" << "zq"
	      << "xl" << "yl" << "zl"
	      << "xg" << "yg" << "zg"
	      << "bxl" << "byl" << "bzl"
	      << "bxq" << "byq" << "bzq" << "bquad"
	      << "bpad" << "brow"<< book;
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
    //myPhysicsDb->print();

    

    StRichCoordinateTransform transform(myGeometryDb);
    StRichRawCoordinate      raw;
    StRichRawCoordinate      rawb;
    StRichQuadrantCoordinate quad;
    StRichLocalCoordinate    local;
    StGlobalCoordinate       global;

    PR(myGeometryDb->numberOfPadsInARow());
    PR(myGeometryDb->numberOfRowsInAColumn());

    int startRow = 0;
    int endRow   = myGeometryDb->numberOfRowsInAColumn();
    int startPad = 0;
    int endPad = myGeometryDb->numberOfPadsInARow();
    for(int iRow=startRow; iRow<endRow; iRow++) {
	raw.setRow(iRow);
	for(int iPad=startPad; iPad<endPad; iPad++) {
	    raw.setPad(iPad);

#ifdef RICH_HISTOGRAM
	    tuple[0] = static_cast<float>(iPad);
	    tuple[1] = static_cast<float>(iRow);
	    idb << "R p/r " << iPad << '\t' << iRow << endl;
	    
	    transform(raw,quad);
	    tuple[2] = static_cast<float>(quad.quadrant());
	    tuple[3] = static_cast<float>(quad.position().x());
	    tuple[4] = static_cast<float>(quad.position().y());
	    tuple[5] = static_cast<float>(quad.position().z());
	    idb << "Q: x/y/z " << quad.position() << endl;
	    
	    transform(quad,local);
	    tuple[6] = static_cast<float>(local.position().x());
	    tuple[7] = static_cast<float>(local.position().y());
	    tuple[8] = static_cast<float>(local.position().z());
	    idb << "L: x/y/z " << local.position() << endl;
	    
	    transform(local,global);
	    tuple[9]  = static_cast<float>(global.position().x());
	    tuple[10] = static_cast<float>(global.position().y());
	    tuple[11] = static_cast<float>(global.position().z());
	    idb << "G: x/y/z " << global.position() << endl;
	    
	    transform(global,local);
	    tuple[12] = static_cast<float>(local.position().x());
	    tuple[13] = static_cast<float>(local.position().y());
	    tuple[14] = static_cast<float>(local.position().z());
	    idb << "Lb: x/y/z " << local.position() << endl;
	    
	    transform(local,quad);
	    tuple[15] = static_cast<float>(quad.position().x());
	    tuple[16] = static_cast<float>(quad.position().y());
	    tuple[17] = static_cast<float>(quad.position().z());
	    tuple[18] = static_cast<float>(quad.quadrant());
	    idb << "Qb: x/y/z " << quad.position() << endl;
	    
	    transform(quad,rawb);
	    tuple[19] = static_cast<float>(rawb.pad());
	    tuple[20] = static_cast<float>(rawb.row());
	    idb << "Rb p/r " << rawb.pad() << '\t' << rawb.row() << '\n' << endl;
	    
	    theTuple->fill(tuple);
#endif
	} // row
    } // pad

    delete myGeometryDb;
    delete myPhysicsDb;

#ifdef RICH_HISTOGRAM
    cout << "Save and close " << endl;
    hbookFile->saveAndClose();
#endif  
    return 0;
} 

