//
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StTpcDb                                                            //
//                                                                      //
// This class implements to offline interface to the STAR database      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef ClassStTpcDb
#define ClassStTpcDb

#ifndef __CINT__
#include "fortranc.h"
#define numberOfPadsAtRow_ F77_NAME(numberofpadsatrow,NUMBEROFPADSATROW)
extern "C" {
R__EXTERN  int type_of_call numberOfPadsAtRow_(int *row);
}
#endif
#include "StMessMgr.h"
#include "StMaker.h"
#include "StDbLib/StDbTableI.h"
#include "StDbLib/StDbDataSet.h"
//#include "Geometry/StDbTpcGeomTables.hh"
#include "StRTpcPadPlane.h"
class StMaker;


class StTpcDb {
 private:
 StMaker* mk;
 StTpcPadPlaneI* PadPlane;
 St_DataSet* tpc_geometry;
 St_DataSet* tpc_calibrations;
 St_DataSet* tpc_conditions;

 protected:
 
 public:
   StTpcDb(StMaker* mk);
   ~StTpcDb();
   StTpcPadPlaneI* PadPlaneGeometry();

#ifdef __ROOT__
   ClassDef(StTpcDb,0)
#endif
};

// Global pointers:
R__EXTERN StTpcDb* gStTpcDb;

#endif

