//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StTpcDb                                                     //
//                                                                      //
//                                                                      //
 
#ifdef __ROOT__
#include "TROOT.h"
#endif

#include "StTpcDb.h"

StTpcDb* gStTpcDb = 0;

//
// C and Fortran routines:
//________________________________________
int type_of_call numberOfPadsAtRow_(int *row) {
    return gStTpcDb->PadPlaneGeometry()->numberOfPadsAtRow(*row);
}
//
// C++ routines:
//_____________________________________________________________________________


#ifdef __ROOT__
ClassImp(StTpcDb)
#endif
//_____________________________________________________________________________
StTpcDb::StTpcDb(StMaker* input) {
St_DataSet* temp=0;
St_DataSet* temp1=0;
mk = input;
temp = (StDbDataSet*)mk->GetData("StarDb");
 if (temp){
   temp1 = temp->Find("Calibrations");
   if(temp1){tpc_calibrations = temp1->Find("tpc");}
   else{gMessMgr->Message("StTpcDb::Error Getting TPC Calibrations database","E");}
   temp1 = temp->Find("Geometry");
   if(temp1)tpc_geometry = temp1->Find("tpc");
   else{gMessMgr->Message("StTpcDb::Error Getting TPC Geometry database","E");}
   temp1 = temp->Find("Conditions");
   if(temp1)tpc_conditions = temp1->Find("tpc");
   else{gMessMgr->Message("StTpcDb::Error Getting TPC Conditions database","E");}
   
 }
 else{
   gMessMgr->Message("StTpcDb::Error Getting TPC database","E");
 }

PadPlane=0;
WirePlane=0;
dimensions=0;
gStTpcDb = this;
}
//_____________________________________________________________________________
StTpcDb::~StTpcDb() {
delete PadPlane;
delete WirePlane;
delete dimensions;
gStTpcDb = 0;
}
//_____________________________________________________________________________

StTpcPadPlaneI* StTpcDb::PadPlaneGeometry(){
  if (PadPlane==0){            // get pad plane from data base
   StDbDataSet* pp = (StDbDataSet*)tpc_geometry->Find("tpcPadPlanes");
   StDbTableI* table=pp->GetDbObject();
   tpcPadPlanes* tpd;
   tpd=(tpcPadPlanes*)table->GetTable(); 
   StRTpcPadPlane* pptemp = new StRTpcPadPlane();
   pptemp->AddData(tpd);
   PadPlane = (StTpcPadPlaneI*)pptemp; 
  }
 return PadPlane;
}

StTpcWirePlaneI* StTpcDb::WirePlaneGeometry(){
  if (WirePlane==0){            // get wire plane from data base
   StDbDataSet* wp = (StDbDataSet*)tpc_geometry->Find("tpcWirePlanes");
   StDbTableI* table=wp->GetDbObject();
   tpcWirePlanes* tpd;
   tpd=(tpcWirePlanes*)table->GetTable(); 
   StRTpcWirePlane* wptemp = new StRTpcWirePlane();
   wptemp->AddData(tpd);
   WirePlane = (StTpcWirePlaneI*)wptemp; 
  }
 return WirePlane;
}

StTpcDimensionsI* StTpcDb::Dimensions(){
  if (dimensions==0){            // get wire plane from data base
   StDbDataSet* wp = (StDbDataSet*)tpc_geometry->Find("tpcDimensions");
   StDbTableI* table=wp->GetDbObject();
   tpcDimensions* tpd;
   tpd=(tpcDimensions*)table->GetTable(); 
   StRTpcDimensions* wptemp = new StRTpcDimensions();
   wptemp->AddData(tpd);
   dimensions = (StTpcDimensionsI*)wptemp; 
  }
 return dimensions;
}


