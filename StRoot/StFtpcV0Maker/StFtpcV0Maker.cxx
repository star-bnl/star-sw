// $Id: StFtpcV0Maker.cxx,v 1.14 2003/09/02 17:58:19 perev Exp $
//
// $Log: StFtpcV0Maker.cxx,v $
// Revision 1.14  2003/09/02 17:58:19  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.13  2000/11/29 08:32:48  jcs
// remove obsolete include
//
// Revision 1.12  2000/11/16 12:48:33  jcs
// Save FTPC vzero inforamtion in correct banks
// Use correct FTPC track class
//
// Revision 1.11  2000/08/09 22:55:28  fisyak
// correction for deleted module
//
// Revision 1.10  2000/08/09 19:16:45  didenko
// remove unneeded include
//
// Revision 1.9  2000/07/04 02:36:50  perev
// formal corrections, gStChain removed
//
// Revision 1.8  2000/06/26 22:10:55  fisyak
// remove params
//
// Revision 1.7  2000/01/03 13:16:15  jcs
// Add CVS Id strings
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StFtpcV0Maker                                                         //
//                                                                      //
//  This Module does the inital search for V0's in the FTPC.            //
//  Written by Mike Heffner 8 Apr 99                                    //
//////////////////////////////////////////////////////////////////////////

// Regular C
#include <stdlib.h>

// Regular C++ 
#include <Stiostream.h>

#include "TStyle.h"

// StarClassLibrary
#include "StThreeVector.hh"

// V0 Specific
#include "StFtpcV0Track.hh"
#include "StFtpcV0.hh"

// Star Module and Table stuff
#include "tables/St_dst_vertex_Table.h"
#include "tables/St_dst_v0_vertex_Table.h"
#include "tables/St_fpt_fptrack_Table.h"

#include "StFtpcV0Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"

#include "StMessMgr.h"

#include "StDetectorId.h"
#include "StVertexId.h"

ClassImp(StFtpcV0Maker)

//_____________________________________________________________________________
StFtpcV0Maker::StFtpcV0Maker(const char *name, const char *title):StMaker(name,title){
   drawinit=kFALSE;
}
//_____________________________________________________________________________
StFtpcV0Maker::~StFtpcV0Maker(){
}
//_____________________________________________________________________________
Int_t StFtpcV0Maker::Init(){
// Create tables
	
gStyle->SetOptStat(111111);

// Create Histograms   
   m_b = new TH1F("fv0_impactParam","FTPC V0 Impact Parameter to Main vertex",100,0,10);
   m_dca = new TH1F("fv0_dca","FTPC V0 dist. of closest appro. of the daughters",100,0,10);
   m_kaonMass = new TH1F("fv0_kaon","FTPC V0 kaon mass",100,250,1000);
   m_lambdaMass = new TH1F("fv0_lambda","FTPC V0 lambda mass",100,1000,1500);
   m_antiLambdaMass = new TH1F("fv0_antiLambda","FTPC V0 Anti-Lambda Mass",100,1000,1500);

   m_z  = new TH1F("ftpc_vzero_z","ftpc vzeros:z",50,-250.,250.);

   return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StFtpcV0Maker::Make(){
  //  PrintInfo();

  // MOVE parameters TO DATA BASE !!!!!!!!!!!!!!!

  if (!m_DataSet->GetList())  {//if DataSet is empty fill it
    double mag = 5*kilogauss; // magnetic filed used for helix
    // Track momentum cuts
    double cutPzMin         = 0.6*GeV; // Initial momentum cut on all tracks
    double cutPzKaonDecay   = 1.0*GeV; // Momentum cut for kaon decay
    double cutPzLambdaDecay = 2.0*GeV; // Momentum cut for lambda,antilambda decay 
    // Track dca cuts
    double cutDcaMin          = 1.2*centimeter;  // Minimum dca cut
    double cutDcaKaonDecay    = 1.2*centimeter;  // Minimum distance between daughter 
                                                 // tracks and event vertex for kaon 
                                                 //  decays
    double cutDcaLambdaDecay1 = 1.3*centimeter;  // Minimum distance between one of the
                                                 // daughter tracks and event vertex for
                                                 // lambda or antilambda decay
    double cutDcaLambdaDecay2 = 5.0*centimeter;  // Minimum distance between one of the
                                                 // daughter tracks and event vertex for
                                                 // lambda or antilambda decay
    // V0 cuts
    double cutV0Dca           = 1.0*centimeter;
//  double cutV0Dca           = 0.5*centimeter;  // Minimum distance between daughters
                                                 // at v0 vertex
      double cutV0ImpactParam   = 1.4*centimeter;
//    double cutV0ImpactParam   = 0.7*centimeter;  // Minimum distance between V0 and
                                                 // event vertex
 
cout<<"Cuts used: "<<endl;
cout<<" Track momentum cuts - "<<endl;
cout<<"   cutPzMin           = "<<cutPzMin<<endl;
cout<<"   cutPzKaonDecay     = "<<cutPzKaonDecay<<endl;
cout<<"   cutPzLambdaDecay   = "<<cutPzLambdaDecay<<endl;
cout<<" Track dca cuts - "<<endl;
cout<<"   cutDcaMin          =  "<<cutDcaMin<<endl;
cout<<"   cutDcaKaonDecay    =  "<<cutDcaKaonDecay<<endl;
cout<<"   cutDcaLambdaDecay1 = "<<cutDcaLambdaDecay1<<endl;
cout<<"   cutDcaLambdaDecay2 = "<<cutDcaLambdaDecay2<<endl;
cout<<" V0 vertex cuts - "<<endl;
cout<<"   cutV0Dca           = "<<cutV0Dca<<endl;
cout<<"cutV0ImpactParam = "<<cutV0ImpactParam<<endl;

// Locate dst_vertex table

  St_DataSet     *primary = GetDataSet("primary");

  if (!primary) {
    gMessMgr->Warning() << "StFtpcV0Maker::Make(): primary is missing" << endm;
    return kStWarn;
  }

  St_DataSetIter primaryI(primary);

  St_dst_vertex *vertex   = (St_dst_vertex *) primary->Find("vertex");
  if (!vertex) {
     gMessMgr->Warning() << "StFtpcV0Maker::Make(): vertex is missing" << endm;
     return kStWarn;
  }

  dst_vertex_st *vtx = vertex->GetTable();
  Int_t No_of_vertices = vertex->GetNRows();

// Locate primary vertex


 if( vtx->vtx_id != kEventVtxId || vtx->iflag != 1){
    for( Int_t no_rows=0; no_rows<No_of_vertices; no_rows++,vtx++){
      if( vtx->vtx_id == kEventVtxId && vtx->iflag == 1 ) break;
    }
  }
 if( vtx->vtx_id != kEventVtxId || vtx->iflag != 1){
     gMessMgr->Warning() << "StFtpcV0Maker::Make(): primary vertex is missing"<< endm;
     return kStWarn;
   }


  StThreeVector<double> origin(vtx->x,vtx->y,vtx->z); // beam interaction pt.

    cout<<"**************************************"<<endl;
    cout<<"**************************************"<<endl;
    cout<<"FTPC V0 finder"<<endl;
    cout<<" origin "<<vtx->x<<" "<<vtx->y<<" "<<vtx->z<<" "<<endl;
    cout<<"**************************************"<<endl;
    cout<<"**************************************"<<endl;

    
// Reallocate dst_vertex table with enough rows to contain FTPC V0 information

    Int_t fv0_limit = 20000;
    vertex->ReAllocate(No_of_vertices + fv0_limit);
    dst_vertex_st *fvtx = vertex->GetTable();
    fvtx = fvtx + No_of_vertices;
  
//  Locate dst_v0_vertex table

    St_DataSet     *dst_v0 = GetDataSet("v0");
    St_dst_v0_vertex *fv0_vertex = 0;
    Int_t V0Count = 0;
    Int_t dst_v0_vertex_max = 0;
    if (dst_v0) {
       fv0_vertex = (St_dst_v0_vertex *) dst_v0->Find("dst_v0_vertex");
       if (fv0_vertex) {
          V0Count = fv0_vertex->GetNRows();

          fv0_vertex->ReAllocate(V0Count + fv0_limit);
          dst_v0_vertex_max = V0Count + fv0_limit;
       }
     }
    if (!fv0_vertex) {
       fv0_vertex = new St_dst_v0_vertex("dst_v0_vertex",fv0_limit);
       AddData(fv0_vertex);
       dst_v0_vertex_max = fv0_limit;
    }

    dst_v0_vertex_st *fv0_table = fv0_vertex->GetTable();
    fv0_table = fv0_table + V0Count;

    //Get the ftpc tracks table    
    St_DataSet *ftpc_data = GetDataSet("ftpc_tracks");
    if (ftpc_data) { //if good data
      // data exist 
      St_DataSetIter next(ftpc_data);
      St_fpt_fptrack *fpt_fptrack = (St_fpt_fptrack *) next("fpt_fptrack");
      
      fpt_fptrack_st *trk  = fpt_fptrack->GetTable();
      
      //Loop over tracks to sort into positive and negative
      // accepting only tracks with momentum > cutPzMin
      Int_t NumberPos=0;
      Int_t NumberNeg=0;
      Int_t *pos = new Int_t[fpt_fptrack->GetNRows()];
      Int_t *neg = new Int_t[fpt_fptrack->GetNRows()];
      for (Int_t counter_i=0;counter_i<fpt_fptrack->GetNRows();counter_i++){ 
	if(trk[counter_i].q == 1 && fabs(trk[counter_i].p[2]*GeV)>cutPzMin){
	  pos[NumberPos]=counter_i;
	  NumberPos++;
	}
        else if(trk[counter_i].q==-1 && fabs(trk[counter_i].p[2]*GeV)>cutPzMin){
	  neg[NumberNeg]=counter_i;
	  NumberNeg++;
	}
      }	
//    cout<<"Number of Positive Tracks = "<<NumberPos<<endl;
//    cout<<"Number of Negative Tracks = "<<NumberNeg<<endl;


      StThreeVector<double> P;
      StThreeVector<double> X;

      //Start of double loop over pairs

      for(Int_t p=0;p<NumberPos;p++){//loop over positive

//	  cout<<"---------Positive Track Number"<<p<<" of "<<NumberPos<<endl;
        Int_t ip = pos[p];
	P.setX(trk[ip].p[0]*GeV); 
	P.setY(trk[ip].p[1]*GeV); 
	P.setZ(trk[ip].p[2]*GeV); 
   	X.setX(trk[ip].v[0]*centimeter); 
   	X.setY(trk[ip].v[1]*centimeter); 
   	X.setZ(trk[ip].v[2]*centimeter);
	
	StFtpcV0Track PosTrack(P,X,mag,1);
	double dcaTp=PosTrack.distance(origin);

//  cuts on positive tracks
 
    if (dcaTp>cutDcaMin) { 

      for(Int_t n=0;n<NumberNeg;n++){//loop over negative
//      cout<<"---------Negative Track Number"<<n<<" of "<<NumberNeg<<endl;
        Int_t in = neg[n];
        // Momentum cuts for kaon.lambda and antilambda decays
        if( (fabs(trk[ip].p[2]*GeV)>cutPzKaonDecay && fabs(trk[in].p[2]*GeV)>cutPzKaonDecay) || fabs(trk[ip].p[2]*GeV)>cutPzLambdaDecay || fabs(trk[in].p[2]*GeV)>cutPzLambdaDecay ) {
	  P.setX(trk[in].p[0]*GeV); 
	  P.setY(trk[in].p[1]*GeV); 
	  P.setZ(trk[in].p[2]*GeV); 
   	  X.setX(trk[in].v[0]*centimeter); 
   	  X.setY(trk[in].v[1]*centimeter); 
   	  X.setZ(trk[in].v[2]*centimeter);
	  
	  StFtpcV0Track NegTrack(P,X,mag,-1);
	  double dcaTn=NegTrack.distance(origin);

	  // Make the V0 calculations

  	  if( (dcaTp>cutDcaKaonDecay&&dcaTn>cutDcaKaonDecay) || (dcaTp>cutDcaLambdaDecay1 && dcaTn>cutDcaLambdaDecay2) || (dcaTp>cutDcaLambdaDecay2 && dcaTn>cutDcaLambdaDecay1)){ //pre-cuts 
	    //figure out what FTPC the track is in
	    double whichFTPC=0;
            // FTPC West (rows  1-10, z>0) det_id = 4
	    if(trk[in].p[2]>0 && trk[ip].p[2]>0) whichFTPC=1;
            // FTPC East (rows 11-20, z<0) det_id = 5
	    if(trk[in].p[2]<0 && trk[ip].p[2]<0) whichFTPC=-1;

	    // make a V0
	    StFtpcV0 v0(PosTrack,NegTrack);
	    
	    if(whichFTPC!=0){
	      if( v0.helixIntersect(-1*whichFTPC*centimeter,250*whichFTPC*centimeter)){
		for(Int_t r=0;r<5;r++){
		  if(v0.Refit()<.01) break;
		}
		v0.ComputeMassAndMomentum();
	      }
	    }
	    
	    
	    // make final cuts and fill output table and histograms
	    if(v0.GetDca()>0&&v0.GetDca()<cutV0Dca&&v0.GetImpactParameter(origin)<cutV0ImpactParam){

              //fill dst_vertex Table

              No_of_vertices++;
              V0Count++;
              if (V0Count > dst_v0_vertex_max ){
    gMessMgr->Warning() << "StFtpcV0Maker::Make(): more than "<<dst_v0_vertex_max<<" vzeros" << endm;
                fv0_vertex->SetNRows(dst_v0_vertex_max);
                vertex->SetNRows(No_of_vertices);
                return kStWarn;
              }
              fvtx->x = v0.GetDecayVertex().x();
              fvtx->y = v0.GetDecayVertex().y();
              fvtx->z = v0.GetDecayVertex().z();
              m_z->Fill((float)fvtx->z);
              fvtx->covar[0] = 0.0;
              fvtx->covar[1] = 0.0;
              fvtx->covar[2] = 0.0;
              fvtx->covar[3] = 0.0;
              fvtx->covar[4] = 0.0;
              fvtx->covar[5] = 0.0;
              fvtx->chisq[0] = 0.0;
              fvtx->chisq[1] = 0.0;
              fvtx->id = No_of_vertices;
              fvtx->iflag = 0;
              fvtx->det_id = 0;
              if (whichFTPC == 1)  fvtx->det_id = kFtpcWestId*100 + kFtpcWestId;
              if (whichFTPC == -1) fvtx->det_id = kFtpcEastId*100 + kFtpcEastId;
              fvtx->id_aux_ent = V0Count;
              fvtx->vtx_id = kV0VtxId;
              fvtx->n_daughters = 2;
              fvtx++; 

	      //fill dst_v0_vertex table


	      fv0_table->id = V0Count;     //The V0's id
              fv0_table->id_vertex = No_of_vertices;  //vertex_id
	      fv0_table->idneg = trk[neg[n]].id_globtrk;   //Negative daughter's id
	      fv0_table->idpos = trk[pos[p]].id_globtrk;   //Positive daughter's id
	      fv0_table->dcan = dcaTn;     //DCA of neg daughter to origin
	      fv0_table->dcap = dcaTp;     //DCA of pos daughter to origin
	      fv0_table->dcapn = v0.GetDca(); //DCA of daughters at decay pt
	      fv0_table->dcav0 = v0.GetImpactParameter(origin); //DCA of V0 to origin
	      fv0_table->neg_px = v0.GetTrack1Momentum().x(); //Neg Track P
	      fv0_table->neg_py = v0.GetTrack1Momentum().y(); 
	      fv0_table->neg_pz = v0.GetTrack1Momentum().z(); 
	      fv0_table->pos_px = v0.GetTrack2Momentum().x(); //Pos Track P
	      fv0_table->pos_py = v0.GetTrack2Momentum().y(); 
	      fv0_table->pos_pz = v0.GetTrack2Momentum().z(); 
	      fv0_table++;
	      // fill histos
  	      m_b->Fill((float)v0.GetImpactParameter(origin));
  	      m_dca->Fill((float)v0.GetDca());
              m_kaonMass->Fill((float)v0.GetKaonMass()/MeV);
  	      m_lambdaMass->Fill((float)v0.GetLambdaMass()/MeV);
  	      m_antiLambdaMass->Fill((float)v0.GetAntiLambdaMass()/MeV);
	    }
	  } //pre cuts 
         } // kaon,lambda,antilambda momentum cuts
	} // loop over negatives
       }    //  cuts on positive tracks
      } // loop over positives
       fv0_vertex->SetNRows(V0Count);
       vertex->SetNRows(No_of_vertices);
    } //if good data
   
  }
  return kStOK;
}
//_____________________________________________________________________________

