//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StDAQfilterMaker class for Makers                                        //
//                                                                      //

#include "StDAQfilterMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"

#include "tables/St_tcl_tphit_Table.h"
//tmp
#include "tables/St_tpt_track_Table.h"

ClassImp(StDAQfilterMaker)

//_____________________________________________________________________________
StDAQfilterMaker::StDAQfilterMaker(const char *name):StMaker(name){
}
//_____________________________________________________________________________
StDAQfilterMaker::~StDAQfilterMaker(){
}
//_____________________________________________________________________________
Int_t StDAQfilterMaker::Init(){
  return kStOK;
}


//_____________________________________________________________________________
Int_t StDAQfilterMaker::Make(){
 printf("------------------- %s-maker event start-------------\n",GetName());

 //   G E T   D A T A
 St_DataSet *ds=GetDataSet("tpc_hits"); assert(ds);
 St_tcl_tphit  *tpcl=(St_tcl_tphit  *) ds->Find( "tphit");
 if(tpcl==0) printf("NULL pointer to St_tcl_tphit table\n");
 if( tpcl) Printf(" BB Ntpcl=%d\n", tpcl->GetNRows());

#if 0
 St_DataSet    *ds1=GetDataSet("tpc_tracks"); assert(ds1);
 St_tpt_track  *tptr=(St_tpt_track   *) ds1->Find("tptrack");
 if(tptr==0) printf("NULL pointer to St_tpt_track table\n");
 
#endif

 if(  tpcl->GetNRows()>4000) return kStErr;
 //if(  tptr->GetNRows()>50) return kStErr;
 return kStOK;
}





