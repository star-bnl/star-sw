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
#include "StRTpcPadPlane.h"
#include "StRTpcWirePlane.h"
#include "StRTpcDimensions.h"
#include "StRTpcGain.h"
#include "StRTpcT0.h"
#include "StRTpcSlowControlSim.h"
class StMaker;


class StTpcDb {
 private:
 StMaker* mk;
 StTpcPadPlaneI* PadPlane;
 StTpcWirePlaneI* WirePlane;
 StTpcDimensionsI* dimensions;
 StTpcSlowControlSimI* slowControlSim;
 StTpcGainI* gain[24];
 StTpcT0I* t0[24];
 St_DataSet* tpc_geometry;
 St_DataSet* tpc_calibrations;
 St_DataSet* tpc_conditions;

 protected:
 
 public:
   StTpcDb(StMaker* mk);
   ~StTpcDb();
   StTpcPadPlaneI* PadPlaneGeometry();
   StTpcWirePlaneI* WirePlaneGeometry();
   StTpcDimensionsI* Dimensions();
   StTpcSlowControlSimI* SlowControlSim();
   StTpcGainI* Gain(int sector);
   StTpcT0I* T0(int sector);

#ifdef __ROOT__
   ClassDef(StTpcDb,0)
#endif
};

// Global pointers:
R__EXTERN StTpcDb* gStTpcDb;

#endif









