//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StTpcDb                                                     //
//                                                                      //
//                                                                      //
 
#ifdef __ROOT__
#include "TROOT.h"
#endif

#include "StTpcDb.h"

static StTpcDb* gStTpcDb = 0;

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
   else{gMessMgr->Message("StTpcDb::Error 
         Getting TPC Calibrations database","F");}
   temp1 = temp->Find("Geometry");
   if(temp1)tpc_geometry = temp1->Find("tpc");
   else{gMessMgr->Message("StTpcDb::Error 
         Getting TPC Geometry database","F");}
   temp1 = temp->Find("Conditions");
   if(temp1)tpc_conditions = temp1->Find("tpc");
   else{gMessMgr->Message("StTpcDb::Error 
         Getting TPC Conditions database","F");}
   
 }
 else{
   gMessMgr->Message("StTpcDb::Error Getting TPC database","F");
 }

PadPlane=0;
gStTpcDb = this;
}
//_____________________________________________________________________________
StTpcDb::~StTpcDb() {
delete PadPlane;
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


