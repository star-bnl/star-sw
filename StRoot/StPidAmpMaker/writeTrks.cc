/***************************************************************************
 *
 * $Id: writeTrks.cc,v 1.2 2000/04/09 18:51:20 aihong Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             write StPidAmpTrks to the disk 
 *             for quick tune purpose
 ***************************************************************************
 *
 * $Log: writeTrks.cc,v $
 * Revision 1.2  2000/04/09 18:51:20  aihong
 * change Make() to read directly from dst tables instead of StEvent
 *
 * Revision 1.1.1.1  2000/03/09 17:48:34  aihong
 * Installation of package
 *
 **************************************************************************/


#include <fstream.h>
#include "St_Table.h"
#include "StHelixModel.h"
#include "StPhysicalHelixD.hh" 
#include "StPrimaryVertex.h"
#include "StEnumerations.h"


#include "tables/St_dst_track_Table.h"
#include "tables/St_dst_dedx_Table.h"
#include "tables/St_dst_vertex_Table.h"



void writeTrks(St_Table* theTrackTable, St_Table* theDedxTable, St_Table* theVertexTable){

  ofstream foutD;

  foutD.open("80evtsTrks.dat", ios::app);

  if (!foutD) {cout<<"Cannot open file.\n"; }//handle error

   double dedx, rig, pt, dca;
   int    nhits,charge;
   int    i=0;

 

 St_dst_track*  globtrk_table=(St_dst_track *)  theTrackTable;
 St_dst_dedx*     dedx_table =(St_dst_dedx *)   theDedxTable;
 St_dst_vertex* vertex_table =(St_dst_vertex *) theVertexTable;


 table_head_st* globtrk_h = globtrk_table->GetHeader();
 table_head_st* dedx_h    =    dedx_table->GetHeader();
 table_head_st* vertex_h  =  vertex_table->GetHeader();

 dst_track_st*  globtrk_v = globtrk_table->GetTable();
 dst_dedx_st*   dedx_v    =    dedx_table->GetTable();
 dst_vertex_st* vertex_v  =  vertex_table->GetTable();
 

 //now get primary vetex
  StPrimaryVertex *pvtx=0;
 for (i=0; i<vertex_h->nok;i++)
 if (vertex_v[i].iflag < 100 && vertex_v[i].iflag%10 == 1 &&
            vertex_v[i].vtx_id == kEventVtxId) 
 pvtx = new StPrimaryVertex(vertex_v[i]);




 for (i=0; i<dedx_h->nok; i++){ //dedx loop

   if (dedx_v[i].det_id !=kTpcId ) continue;//not from tpc dector.
   if (dedx_v[i].method !=kTruncatedMeanId ) continue; //not from truncated Mn.
   

   dedx=dedx_v[i].dedx[0];
   if (!dedx) continue;  //bad assinment.  

   nhits=dedx_v[i].ndedx;


   if (dedx_v[i].id_track < globtrk_h->nok){//global track bound check

 
  
 StHelixModel* theHelixModel= new StHelixModel(globtrk_v[dedx_v[i].id_track-1]);

 charge=theHelixModel->charge();

   const  StPhysicalHelixD& thePhysicalHelix=theHelixModel->helix();

   const  StThreeVectorF& p=theHelixModel->momentum();
   pt=double(p.perp()); 





   rig=double(p.mag()/double(charge));

   if (!rig) continue;
  
   dca=thePhysicalHelix.distance(pvtx->position());
   

   
 foutD<<rig<<" "<<dedx<<" "<<charge<<" "<<pt<<" "<<nhits<<" "<<dca<<endl;
   

   if (theHelixModel) delete theHelixModel;
   }



 }

   if (pvtx) delete pvtx;

   foutD.close();

}
