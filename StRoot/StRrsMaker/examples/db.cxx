/******************************************************
 * $Id: db.cxx,v 1.2 2000/03/12 23:58:08 lasiuk Exp $
 * Description:
 *  Stand-alone test module
 *
 * $Log: db.cxx,v $
 * Revision 1.2  2000/03/12 23:58:08  lasiuk
 * db update
 *
 * Revision 1.1  2000/02/08 16:35:28  lasiuk
 * Hp compatible
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
#include <vector>
#if defined (__SUNPRO_CC) && __SUNPRO_CC >= 0x500
using std::string;
#endif

// dbs
#include "StRichPhysicsDb.h"
#include "StRichGeometryDb.h"
#include "StRichCoordinateTransform.h"
#include "StRichOtherAlgorithms.h"

int main()
{

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


    StRichCoordinateTransform* transform =
	StRichCoordinateTransform::getTransform(myGeometryDb);
    StRichRawCoordinate      rawA;
    StRichQuadrantCoordinate quad;
    StRichLocalCoordinate    localA;
    StGlobalCoordinate       global;
    
    cout << "Raw --> Quadrant" << endl;
    vector<StRichRawCoordinate> raw;
    raw.push_back(StRichRawCoordinate(0,0));
    raw.push_back(StRichRawCoordinate(79,0));
    raw.push_back(StRichRawCoordinate(159,0));
    raw.push_back(StRichRawCoordinate(0,47));
    raw.push_back(StRichRawCoordinate(79,47));

    raw.push_back(StRichRawCoordinate(80,0));
    raw.push_back(StRichRawCoordinate(80,47));

    vector<StRichLocalCoordinate> local;
    local.push_back(StRichLocalCoordinate(0,0,0));
    local.push_back(StRichLocalCoordinate(65,0,0));
    local.push_back(StRichLocalCoordinate(65.5,0.2,0));

    int ii;
    for(ii=0; ii<raw.size(); ii++) {
	cout << "ii= " << ii << endl;
	(*transform)(raw[ii],quad);
	cout << "raw --> quad:  " << raw[ii]   << " " << quad << endl;

	(*transform)(raw[ii],localA);
	cout << "raw --> local: " << raw[ii]   << " " << localA << endl;

	(*transform)(raw[ii], global);
	cout << "raw-->global:  " << raw[ii]   << " " << global << endl;

	(*transform)(global,rawA);
	cout << "global->rawA:  " << global  << " " << rawA
	     << "(" << nearestInteger(rawA.pad()) << ", " << nearestInteger(rawA.row()) << ")" << endl;
    }

    cout << "*****************************************\n" << endl;
    for(ii=0; ii<local.size(); ii++) {
	cout << "ii= " << ii << endl;
	cout << local[ii] << endl;
	(*transform)(local[ii],rawA);
	cout << "local --> local: " << local[ii]   << " " << rawA << endl;

	(*transform)(local[ii],quad);
	cout << "local --> quad:  " << local[ii]   << " " << quad << endl;

	(*transform)(local[ii], global);
	cout << "local-->global:  " << local[ii]   << " " << global << endl;

	(*transform)(global,localA);
	cout << "global->localA:  " << global  << " " << localA << endl;
    }


    delete myGeometryDb;
    delete myPhysicsDb;
    
    return 0;
} 

