/******************************************************
 * $Id: db.cxx,v 1.1 2000/02/08 16:35:28 lasiuk Exp $
 * Description:
 *  Stand-alone test module
 *
 * $Log: db.cxx,v $
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

#if defined (__SUNPRO_CC) && __SUNPRO_CC >= 0x500
using std::string;
#endif

// dbs
#include "StRichPhysicsDb.h"
#include "StRichGeometryDb.h"
#include "StRichCoordinateTransform.h"
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


    StRichCoordinateTransform transform(myGeometryDb);
    StRichRawCoordinate      raw;
    StRichQuadrantCoordinate quad;
    StRichLocalCoordinate    local;
    StGlobalCoordinate       global;
    
    cout << "Raw --> Quadrant" << endl;
    StRichRawCoordinate rawa(95,0);
    StRichRawCoordinate rawb(0,0);
    StRichRawCoordinate rawc(0,48);
    StRichRawCoordinate raw1(159,95);
    StRichRawCoordinate rawd(95,159);
    
    transform(raw1,quad);
    cout << "raw --> quad:  " << raw1   << " " << quad << endl;

    transform(raw1,local);
    cout << "raw --> local: " << raw1   << " " << local << endl;

    transform(raw1, global);
    cout << "raw-->global:  " << raw1   << " " << global << endl;

    transform(quad,local);
    cout << "quad-->local:  " << quad   << " " << local << endl;
    
    transform(quad,global);
    cout << "quad-->global: " << quad   << " " << global << endl;

    transform(local,global);
    cout << "local-->global:  " << local  << " " << global << endl;

    //
    // And backwards
    //
    
    transform(global,local);
    cout << "global-->local:  " << global   << " " << local << endl;

    transform(global,quad);
    cout << "global-->quad:  " << global  << " " << quad << endl;

    transform(global,raw);
    cout << "global-->raw:  " << global  << " " << raw << endl;
    
    transform(local,quad);
    cout << "local-->quad:  " << local  << " " << quad << endl;

    transform(local,raw);
    cout << "local-->raw:  " << local  << " " << raw << endl;

    transform(quad,raw);
    cout << "quad-->raw:  " << quad  << " " << raw << endl;

    
    delete myGeometryDb;
    delete myPhysicsDb;
    
    return 0;
} 

