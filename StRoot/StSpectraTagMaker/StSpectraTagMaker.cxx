// StSpectraTagMaker class for Spectra Reconstruction Tags                  //

#include "StSpectraTagMaker.h"
#include "StChain.h"
#include "tables/St_SpectraTag_Table.h"
#include "StEventTypes.h"
#include "Stypes.h"
#include "StMessMgr.h"
#include "StEventUtilities/StuRefMult.hh"
double dEdx_formula(double momentum, double mass);
ClassImp(StSpectraTagMaker)

StSpectraTagMaker::StSpectraTagMaker(const char *name):StMaker(name){
 //  StSpectraTagMaker constructor
}

StSpectraTagMaker::~StSpectraTagMaker(){
  // StSpectraTagMaker destructor
}

Int_t StSpectraTagMaker::Init(){
 //  Init - is a first method the top level StChain calls to initialize all its makers
   return StMaker::Init();
}

Int_t StSpectraTagMaker::Make(){


 //  Make - this method is called in loop for each event


  // Create a data set and add the table to it.
  St_SpectraTag *tagtab= new St_SpectraTag("SpectraTag",1); m_DataSet->Add(tagtab);
  SpectraTag_st *tagtab_st = (SpectraTag_st *) tagtab->GetTable();  
 
  SpectraTag_st row;
  //fill default values for the Spectra Tags
  row.standardNegativeChargedMultiplicity = 0;
  row.numberOfAntiNucleusCandidates = 0;
  int ANucs = 0;

  tagtab->AddAt(&row,0);


  StEvent* event = (StEvent *)GetInputDS("StEvent");
  if (!event) {
    tagtab->AddAt(&row,0);
    return kStErr;
  }

  StEvent& ev = *event;
  row.standardNegativeChargedMultiplicity = uncorrectedNumberOfNegativePrimaries(ev);


  // now count up the number of interesting tracks

   StPrimaryTrackIterator itr;
   StPrimaryTrack *track;
   float px;
   float py;
   float pz;
   float momentum;
   int chargesign;
   int npntpossible;
   int npntfit;
   int ndedx;
   float dedx;
   float dedx_deuteron;
   float z_deuteron;
   if (ev.primaryVertex()){
      const StSPtrVecPrimaryTrack& tracks = ev.primaryVertex()->daughters();
    for (itr=tracks.begin();itr != tracks.end(); itr++){
     track = *itr;    
     npntpossible = track->numberOfPossiblePoints();
     npntfit = track->fitTraits().numberOfFitPoints();
     px = track->geometry()->momentum().x();
     py = track->geometry()->momentum().y();
     pz = track->geometry()->momentum().z();
     momentum = sqrt(px*px+py*py+pz*pz);
     dedx_deuteron = dEdx_formula(momentum, 1.88);
     chargesign = track->geometry()->charge();
     dedx = 0.0;
     ndedx = 0;
     StSPtrVecTrackPidTraits& traits=track->pidTraits();
     StDedxPidTraits* dedxPidTr;
      for (int itrait = 0; itrait < traits.size(); itrait++){
       dedxPidTr = 0;
        if (traits[itrait]->detector() == kTpcId) {
             StTrackPidTraits* thisTrait = traits[itrait];
  	     dedxPidTr = dynamic_cast<StDedxPidTraits*>(thisTrait);
	     if (dedxPidTr && dedxPidTr->method() == kTruncatedMeanId) {
              dedx = dedxPidTr->mean();
              ndedx = dedxPidTr->numberOfPoints();
             }
        }
      }
      //look for Abar candidate:  require 20 hits, 10 dE/dx samples, negative charge, Z_deuteron>0.5, apparent momentum < 5, apparent momentum <0.2
     z_deuteron = log(dedx/dedx_deuteron);
     if (z_deuteron<0.5) continue;
     if (npntfit<20) continue;
     if (ndedx<10) continue;
     if (chargesign>0) continue;
     if (momentum>5||momentum<0.2) continue;
     ANucs++;
    }  // loop over tracks
   } // check for primary vertex
   row.numberOfAntiNucleusCandidates = ANucs;

  tagtab->AddAt(&row,0);

 return kStOK;

}










