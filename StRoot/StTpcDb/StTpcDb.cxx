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
slowControlSim=0;
electronics=0;
dimensions=0;
 for (int i=0;i<24;i++){
   gain[i]=0;
   t0[i]=0;
 }
gStTpcDb = this;
}

//_____________________________________________________________________________
StTpcDb::~StTpcDb() {
delete PadPlane;
delete WirePlane;
delete dimensions;
delete slowControlSim;
delete electronics;
delete gain;
delete t0;
gStTpcDb = 0;
}
//_____________________________________________________________________________

StTpcPadPlaneI* StTpcDb::PadPlaneGeometry(){
  if (PadPlane==0){            // get pad plane from data base
   StDbDataSet* pp = (StDbDataSet*)tpc_geometry->Find("tpcPadPlanes");
   if (pp==0){
    gMessMgr->Message("StTpcDb::Error Finding Tpc Pad Planes","E");
    return 0;
   }
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
   if (wp==0){
    gMessMgr->Message("StTpcDb::Error Finding Tpc Wire Planes","E");
    return 0;
   }
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
   if (wp==0){
    gMessMgr->Message("StTpcDb::Error Finding Tpc Dimensions","E");
    return 0;
   }
   StDbTableI* table=wp->GetDbObject();
   tpcDimensions* tpd;
   tpd=(tpcDimensions*)table->GetTable(); 
   StRTpcDimensions* wptemp = new StRTpcDimensions();
   wptemp->AddData(tpd);
   dimensions = (StTpcDimensionsI*)wptemp; 
  }
 return dimensions;
}

StTpcSlowControlSimI* StTpcDb::SlowControlSim(){
  if (slowControlSim==0){            // get wire plane from data base
   StDbDataSet* wp = (StDbDataSet*)tpc_calibrations->Find("tpcSlowControlSim");
   if (wp==0){
    gMessMgr->Message("StTpcDb::Error Finding Slow Control Simulations Parameters","E");
    return 0;
   }
   StDbTableI* table=wp->GetDbObject();
   tpcSlowControlSim* tpd;
   tpd=(tpcSlowControlSim*)table->GetTable(); 
   StRTpcSlowControlSim* wptemp = new StRTpcSlowControlSim();
   wptemp->AddData(tpd);
   slowControlSim = (StTpcSlowControlSimI*)wptemp; 
  }
 return slowControlSim;
}

StTpcElectronicsI* StTpcDb::Electronics(){
  if (electronics==0){            // get electronics from data base
   StDbDataSet* wp = (StDbDataSet*)tpc_geometry->Find("tpcElectronics");
   if (wp==0){
    gMessMgr->Message("StTpcDb::Error Finding Tpc Electronics","E");
    return 0;
   }
   StDbTableI* table=wp->GetDbObject();
   tpcElectronics* tpd;
   tpd=(tpcElectronics*)table->GetTable(); 
   StRTpcElectronics* wptemp = new StRTpcElectronics();
   wptemp->AddData(tpd);
   electronics = (StTpcElectronicsI*)wptemp; 
  }
 return electronics;
}

StTpcGainI* StTpcDb::Gain(int sector){
  if(sector<1||sector>24){
    gMessMgr->Message("StTpcDb::Gains request for invalid sector","E");
    return 0;
  }
  if(gain[sector-1]==0){
   char dbname[25];
   sprintf(dbname,"Sector_%.2d/tpcGainFactors",sector);
   printf("Getting %s \n",dbname);
   StDbDataSet* wp = (StDbDataSet*)tpc_calibrations->Find(dbname);
   if (wp==0){
    gMessMgr->Message("StTpcDb::Error Finding Tpc Gain Factors","E");
    return 0;
   }
   StDbTableI* table=wp->GetDbObject();
   tpcGainFactors* tpd;
   tpd=(tpcGainFactors*)table->GetTable(); 
   StRTpcGain* wptemp = new StRTpcGain();
   wptemp->AddData(tpd);
   wptemp->SetPadPlanePointer(PadPlaneGeometry());
   gain[sector-1] = (StTpcGainI*)wptemp;
  }
 return gain[sector-1];
}

StTpcT0I* StTpcDb::T0(int sector){
  if(sector<1||sector>24){
    gMessMgr->Message("StTpcDb::T0s request for invalid sector","E");
    return 0;
  }
  if(t0[sector-1]==0){
   char dbname[25];
   sprintf(dbname,"Sector_%.2d/tpcTimeOffsets",sector);
   printf("Getting %s \n",dbname);
   StDbDataSet* wp = (StDbDataSet*)tpc_calibrations->Find(dbname);
   if (wp==0){
    gMessMgr->Message("StTpcDb::Error Finding Tpc Time Offsets","E");
    return 0;
   }
   StDbTableI* table=wp->GetDbObject();
   tpcTimeOffsets* tpd;
   tpd=(tpcTimeOffsets*)table->GetTable(); 
   StRTpcT0* wptemp = new StRTpcT0();
   wptemp->AddData(tpd);
   wptemp->SetPadPlanePointer(PadPlaneGeometry());
   t0[sector-1] = (StTpcT0I*)wptemp;
  }
 return t0[sector-1];
}





