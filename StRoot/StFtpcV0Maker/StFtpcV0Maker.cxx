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
#include <iostream.h>

// StarClassLibrary
#include "StThreeVector.hh"

// V0 Specific
#include "StFtpcTrack.hh"
#include "StFtpcV0.hh"

// Star Module and Table stuff
#include "ftpc/St_fpt_Module.h"
#include "ftpc/St_fte_Module.h"
#include "tables/St_fv0_vertex_Table.h"

#include "StFtpcV0Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
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
   St_DataSetIter       local(gStChain->DataSet("params"));
	

// Create Histograms   
   m_b = new TH1F("fv0_impactParam","FTPC V0 Impact Parameter to Main vertex",100,0,10);
   m_dca = new TH1F("fv0_dca","FTPC V0 dist. of closest appro. of the daughters",100,0,10);
   m_kaonMass = new TH1F("fv0_kaon","FTPC V0 kaon mass",100,250,1000);
   m_lambdaMass = new TH1F("fv0_lambda","FTPC V0 lambda mass",100,1000,1500);
   m_antiLambdaMass = new TH1F("fv0_antiLambda","FTPC V0 Anti-Lambda Mass",100,1000,1500);

   return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StFtpcV0Maker::Make(){
  //  PrintInfo();
  if (!m_DataSet->GetList())  {//if DataSet is empty fill it

   // parameters that should go somewhere else
  double mag = 5*kilogauss; // magnetic filed used for helix
  double cutDcaPosTrk = 200*centimeter;
  double cutDcaNegTrk =200*centimeter;
  double cutPzNegTrk = 100*MeV;
  double cutPzPosTrk = 100*MeV;     
  double cutDca = 25*centimeter;
  double cutV0ImpactParam = 50*centimeter;
  StThreeVector<double> origin(0,0,0); // beam interaction pt.

    cout<<"**************************************"<<endl;
    cout<<"**************************************"<<endl;
    cout<<"FTPC V0 finder"<<endl;
    cout<<"fV0 "<<endl;
    cout<<"**************************************"<<endl;
    cout<<"**************************************"<<endl;

    
    //make output table
    St_fv0_vertex *fv0_vertex = new St_fv0_vertex("fv0_vertex",20000);
    m_DataSet->Add(fv0_vertex);
    fv0_vertex_st *fv0_table = fv0_vertex->GetTable();

    //Get the ftpc tracks table    
    St_DataSet *ftpc_data = gStChain->DataSet("ftpc_tracks");
    if (ftpc_data) { //if good data
      // data exist 
      St_DataSetIter next(ftpc_data);
      St_fpt_fptrack *fpt_fptrack = (St_fpt_fptrack *) next("fpt_fptrack");
      
      fpt_fptrack_st *trk  = fpt_fptrack->GetTable();
      
      //Loop over tracks to sort into positive and negative
      int NumberPos=0;
      int NumberNeg=0;
      int *pos = new int[fpt_fptrack->GetNRows()];
      int *neg = new int[fpt_fptrack->GetNRows()];
      for (Int_t counter_i=0;counter_i<fpt_fptrack->GetNRows();counter_i++){ 
	if(trk[counter_i].q == 1){
	  pos[NumberPos]=counter_i;
	  NumberPos++;
	}else if(trk[counter_i].q == -1){
	  neg[NumberNeg]=counter_i;
	  NumberNeg++;
	}
      }	
      cout<<"Number of Positive Tracks = "<<NumberPos<<endl;
      cout<<"Number of Negative Tracks = "<<NumberNeg<<endl;


      StThreeVector<double> P;
      StThreeVector<double> X;
      int V0Count =0;
      //Start of double loop over pairs

      for(int p=0;p<NumberPos;p++){//loop over positive

	//cout<<"---------Positive Track Number"<<p<<" of "<<NumberPos<<endl;
	P.setX(trk[p].p[1]*GeV); 
	P.setY(trk[p].p[2]*GeV); 
	P.setZ(trk[p].p[3]*GeV); 
	X.setX(trk[p].v[1]*centimeter); 
	X.setY(trk[p].v[2]*centimeter); 
	X.setZ(trk[p].v[3]*centimeter);
	
	StFtpcTrack PosTrack(P,X,mag,1);
	double dcaTp=PosTrack.distance(origin);

	for(int n=0;n<NumberNeg&&dcaTp<cutDcaPosTrk;n++){//loop over negative
	  //cout<<"---------Negative Track Number"<<n<<" of "<<NumberNeg<<endl;
	  
	  P.setX(trk[n].p[1]*GeV); 
	  P.setY(trk[n].p[2]*GeV); 
	  P.setZ(trk[n].p[3]*GeV); 
	  X.setX(trk[n].v[1]*centimeter); 
	  X.setY(trk[n].v[2]*centimeter); 
	  X.setZ(trk[n].v[3]*centimeter);
	  
	  StFtpcTrack NegTrack(P,X,mag,-1);
	  double dcaTn=NegTrack.distance(origin);
	  
	  // Make the V0 claculations
	  if(fabs(trk[n].p[3]*GeV)>cutPzNegTrk && fabs(trk[p].p[3]*GeV)>cutPzPosTrk && dcaTn<cutDcaNegTrk){ //pre-cuts
	    //figure out what FTPC the track is in
	    double whichTPC=0;
	    if(trk[n].p[3]>0 && trk[p].p[3]>0) whichTPC=1;
	    if(trk[n].p[3]<0 && trk[p].p[3]<0) whichTPC=-1;
	    // make a V0
	    StFtpcV0 v0(PosTrack,NegTrack);
	    
	    if(whichTPC!=0){
	      if( v0.helixIntersect(-1*whichTPC*centimeter,250*whichTPC*centimeter)){
		for(int r=0;r<5;r++){
		  if(v0.Refit()<.01) break;
		}
		v0.ComputeMassAndMomentum();
	      }
	    }
	    
	    
	    // make final cuts and fill output table and histograms
	    if(v0.GetDca()>0&&v0.GetDca()<cutDca&&v0.GetImpactParameter(origin)<cutV0ImpactParam){
	      //fill V0 Table
	      fv0_table->id = V0Count;     //The V0's id
	      fv0_table->idneg = neg[n];   //Negative daughter's id
	      fv0_table->idpos = pos[p];   //Positive daughter's id
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
       	      V0Count++;
	      fv0_table++;
	      // fill histos
	      m_b->Fill((float)v0.GetImpactParameter(origin));
	      m_dca->Fill((float)v0.GetDca());
	      m_kaonMass->Fill((float)v0.GetKaonMass()/MeV);
	      m_lambdaMass->Fill((float)v0.GetLambdaMass()/MeV);
	      m_antiLambdaMass->Fill((float)v0.GetAntiLambdaMass()/MeV);
	    }
	  } //pre cuts 
	} // loop over negatives
	
      } // loop over positives
       fv0_vertex->SetNRows(V0Count);
    } //if good data
   
  }
  return kStOK;
}
//_____________________________________________________________________________

