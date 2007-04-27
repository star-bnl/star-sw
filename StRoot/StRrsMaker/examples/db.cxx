/******************************************************
 * $Id: db.cxx,v 2.2 2007/04/27 13:53:30 hippolyt Exp $
 * Description:
 *  Stand-alone test module
 *
 * $Log: db.cxx,v $
 * Revision 2.2  2007/04/27 13:53:30  hippolyt
 * Star logger recommendations
 *
 * Revision 2.1  2003/09/02 17:58:58  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 2.0  2000/08/09 16:10:28  gans
 * Cosmetic Changes. Naming convention of StRichDrawableT(foo)
 *
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
#include "StMessMgr.h"
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
      { LOG_ERROR << "Geometry database could not be initialized. Aborting!!!" << endm; }
      return 1;
    }

    if ( !myPhysicsDb ) {
      { LOG_ERROR << "Physics database could not be initialized. Aborting!!!" << endm; }
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
    
    { LOG_INFO << "Raw --> Quadrant" << endm; }
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
      { LOG_INFO << "ii= " << ii << endm; }
      (*transform)(raw[ii],quad);
      { LOG_INFO << "raw --> quad:  " << raw[ii]   << " " << quad << endm; }
	(*transform)(raw[ii],localA);
	{ LOG_INFO << "raw --> local: " << raw[ii]   << " " << localA << endm; }
	(*transform)(raw[ii], global);
	{ LOG_INFO << "raw-->global:  " << raw[ii]   << " " << global << endm; }
	(*transform)(global,rawA);
	{ LOG_INFO << "global->rawA:  " << global  << " " << rawA
	     << "(" << nearestInteger(rawA.pad()) << ", " << nearestInteger(rawA.row()) << ")" << endm; }
    }

    { LOG_INFO << "*****************************************\n" << endm; }
    for(ii=0; ii<local.size(); ii++) {
	{ LOG_INFO << "ii= " << ii << endm; }
	{ LOG_INFO << local[ii] << endm; }
	(*transform)(local[ii],rawA);
	{ LOG_INFO << "local --> local: " << local[ii]   << " " << rawA << endm; }

	(*transform)(local[ii],quad);
	{ LOG_INFO << "local --> quad:  " << local[ii]   << " " << quad << endm; }

	(*transform)(local[ii], global);
	{ LOG_INFO << "local-->global:  " << local[ii]   << " " << global << endm; }

	(*transform)(global,localA);
	{ LOG_INFO << "global->localA:  " << global  << " " << localA << endm; }
    }


    delete myGeometryDb;
    delete myPhysicsDb;
    
    return 0;
} 

