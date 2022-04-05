////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowDirectCumulantMaker.cxx,v 1.2 2010/06/10 16:33:53 posk Exp $
//
// Authors: Dhevan Gangadharan, UCLA, Dec 2009
//
///////////////////////////////////////////////////////////////////////////////
//
// Description:  Maker to analyze Flow using direct cumulants.
//  It needs only one pass through the data.
//  The outputs are 2D histograms for the needed terms, of events vs centrality and pt.
//  The output file is flow.dirCumulant.root.
//  The macro directCumulants_v2.C then converts these terms to cumulants
//  and then v2(pt) for each centrality and min bias.
//  cent=1 is the most central, cent=0 is the yield weighted average of all the others.
//  The integrated v2 is also calculated by integrating over pt.
//  Some documentation is at http://www.star.bnl.gov/protected/bulkcorr/posk/direct4part/index.html
//
////////////////////////////////////////////////////////////////////////////

#include <Stiostream.h>
#include <stdlib.h>
#include <math.h>
#include "StMaker.h"
#include "StFlowDirectCumulantMaker.h"
#include "StFlowMaker/StFlowMaker.h"
#include "StFlowMaker/StFlowEvent.h"
#include "StFlowMaker/StFlowConstants.h"
#include "StFlowMaker/StFlowSelection.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "TFile.h"
#include "TString.h"
#include "TH2.h"
#include "StMessMgr.h"
#define PR(x) cout << "##### FlowAnalysis: " << (#x) << " = " << (x) << endl;

ClassImp(StFlowDirectCumulantMaker)

//-----------------------------------------------------------------------
StFlowDirectCumulantMaker::StFlowDirectCumulantMaker(const Char_t* name): StMaker(name),
  MakerName(name) {
  pFlowSelect = new StFlowSelection();
}

StFlowDirectCumulantMaker::StFlowDirectCumulantMaker(const Char_t* name,
					 const StFlowSelection& flowSelect) :
  StMaker(name), MakerName(name) {
  pFlowSelect = new StFlowSelection(flowSelect); // copy constructor
}

StFlowDirectCumulantMaker::~StFlowDirectCumulantMaker() {
}

//-----------------------------------------------------------------------
Int_t StFlowDirectCumulantMaker::Make() {
  // do events

  // Get a pointer to StFlowEvent
  StFlowMaker* pFlowMaker = NULL;
  pFlowMaker = (StFlowMaker*)GetMaker("Flow");
  if (pFlowMaker) pFlowEvent = pFlowMaker->FlowEventPointer();
  if (pFlowEvent) {
    int runID = pFlowEvent->RunID();
    //cout << "runID= " << runID << endl;

    if(runID < 8000000) { // not year 7, Mweights = 1
      if (Mweights[0][0] != 1.) { // first event
	gMessMgr->Info("##### FlowDirCumulant: Setting Multiplicity Weights to 1");
	//cout << "Multiplicity Weights:" << endl;
	for(int i=0; i<30; i++){
	  //cout << endl;
	  for(int j=0; j<1000; j++){
	    //cout << setprecision(3) << Mweights[i][j] << ", ";	
	    Mweights[i][j]=1;
	  }
	  //cout << endl;
	}
      }
    }

    FillParticleArrays(); // This fills the particle arrays from pFlowTrack
    CalculateTerms();
    Fill_Histograms(); // Fill Histograms and let things accumulate
  } else {
    gMessMgr->Info("##### FlowDirCumulant: FlowEvent pointer null");
    return kStOK;
  }
    
  if (Debug()) StMaker::PrintInfo();
  
  return kStOK;
}

//-----------------------------------------------------------------------
Int_t StFlowDirectCumulantMaker::Init() {
  // Book histograms
  cout << endl <<"StFlowDirectCumulantMaker::Init()" << endl;

  GetMweights(); // Y7 mutiplicity weights; due to Y7 centrality bias

  TERM1=0;
  TERM2=1;
  TERM3=2;
  TERM4=3;
  TERM5=4;
  TERM6=5;
  TERM7=6;
  TERM8=7;
  TERM9=8;
  TERM10=9;

  DIF=0;
  INT=1; 
  COS=0;
  SIN=1;
  
  TString *name;
  const int ptbins = Flow::PTBINS - 1;
  
  for(int term=0; term<Flow::TERMS; term++){
    for(int type=0; type<Flow::TYPES; type++){
      for(int phase=0; phase<Flow::PHASES; phase++){
	for(int species=0; species<Flow::SPECIES; species++){
	  	  
	  name = new TString("Term_");
	  *name += term+1;

	  if(type==0) name->Append("_D");
	  else name->Append("_I");
	  
	  if(phase==0) name->Append("_cos_");
          else name->Append("_sin_");

	  *name += species;

	  Term[term].Type[type].Phase[phase].Species[species].Sum_Singles = new TH2D(name->Data(),"Correlation Sum",Flow::nCents,.5,Flow::nCents+0.5,ptbins,0.05,ptbins/10.+0.05);
	  Term[term].Type[type].Phase[phase].Species[species].Sum_Singles->GetXaxis()->SetTitle("Centrality bin");
          Term[term].Type[type].Phase[phase].Species[species].Sum_Singles->GetYaxis()->SetTitle("p_{t} bin/10");

	  name->Append("_Sq");
	  Term[term].Type[type].Phase[phase].Species[species].Sum_Squares = new TH2D(name->Data(),"Squares Sum",Flow::nCents,.5,Flow::nCents+0.5,ptbins,0.05,ptbins/10.+0.05);
	  Term[term].Type[type].Phase[phase].Species[species].Sum_Squares->GetXaxis()->SetTitle("Centrality bin");
	  Term[term].Type[type].Phase[phase].Species[species].Sum_Squares->GetYaxis()->SetTitle("p_{t} bin/10");

	}
      }
    }
  }

  for(int species=0; species<Flow::SPECIES; species++){
    name = new TString("Cent_Pt_yields_");
    *name += species;
    Entries[species].cent_yields = new TH2D(name->Data(),"Pt_spectrum",Flow::nCents,.5,Flow::nCents+0.5,ptbins,0.05,ptbins/10.+0.05);
    Entries[species].cent_yields->GetXaxis()->SetTitle("Centrality bin");
    Entries[species].cent_yields->GetYaxis()->SetTitle("p_{t} bin");
  }
  
  Event_counter = new TH1D("Event_counter","Event_counter",Flow::nCents,0.5,Flow::nCents+0.5);
  Event_counterWeighted = new TH1D("Event_counterWeighted","Event_counterWeighted",Flow::nCents,0.5,Flow::nCents+0.5);
  Event_counterWeighted->Sumw2();

  gMessMgr->SetLimit("##### FlowDirCumu", 2);
  gMessMgr->Info("##### FlowDirCumu: $Id: StFlowDirectCumulantMaker.cxx,v 1.2 2010/06/10 16:33:53 posk Exp $");

  return StMaker::Init();
}

//-----------------------------------------------------------------------
void StFlowDirectCumulantMaker::GetMweights() {
  // Y7 mutiplicity weights; due to Y7 centrality bias

  TFile* WeightFile = new TFile("M_WeightsY7.root","READ");

  if(WeightFile->IsOpen()) {    
    for(int i=0; i<30; i++){ // vertex z
      for(int j=0; j<1000; j++){ // ref mult
	
	Mweights[i][j] = ((TH2D*)WeightFile->Get("Weights"))->GetBinContent(i+1,j+1);
	
      }
    }
    WeightFile->Close();
  } else {
    gMessMgr->Info("##### FlowDirCumulant: No year 7 Multiplicity Weights");
    for(int i=0; i<30; i++){
      for(int j=0; j<1000; j++){
	Mweights[i][j]=1;
      }
    }
  }
}

//----------------------------------------------------------------------
void StFlowDirectCumulantMaker::FillParticleArrays() {
  // Fill _p primary quantities

  grefmult = pFlowEvent->MultEta();

  StThreeVectorF vertex = pFlowEvent->VertexPos();
  zbin=0; // z_vertex bin used for multiplicity weights for the Y7 AuAu Centrality bias
  for(int i=0; i<30; i++){
    if( (vertex[2] > (2*i-30)) && (vertex[2] < (2*(i+1)-30)) ){
      zbin=i+1;
      break;
    }
  }

  // Clear previous event quantities
  p_count=0;

  for(int i=0; i<Flow::PTBINS; i++) {
    pt_bin_counter_p[i]=0;
  } 

  // Primary tracks
  StFlowTrackCollection* pFlowTracks_primary = pFlowEvent->TrackCollection();
  StFlowTrackIterator itr;

  ///////////////////////////////////////////////////////////////////////
  // Primary track arrays
  for (itr = pFlowTracks_primary->begin(); itr != pFlowTracks_primary->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;
    // cuts were made as StFlowTrack was filled

    phi_p[p_count] = pFlowTrack->Phi();
    if(phi_p[p_count] < 0.) phi_p[p_count] += twopi;
    id_p[p_count] = pFlowTrack->id();
    pt_p[p_count] = pFlowTrack->Pt();

    weights_p[p_count] = 1.0;
    if (pFlowEvent->PtWgt()) { // pt wgt
      weights_p[p_count] *= (pt_p[p_count] < pFlowEvent->PtWgtSaturation()) ? pt_p[p_count] : 
	pFlowEvent->PtWgtSaturation();  // pt weighting going constant
    }

    if(pt_p[p_count]      <= .2 ) { ptbin_p[p_count] = 1; goto ptbin; }
    else if(pt_p[p_count] <= .4 ) { ptbin_p[p_count] = 3; goto ptbin; } 
    else if(pt_p[p_count] <= .6 ) { ptbin_p[p_count] = 5; goto ptbin; } 
    else if(pt_p[p_count] <= .8 ) { ptbin_p[p_count] = 7; goto ptbin; }
    else if(pt_p[p_count] <= 1.0) { ptbin_p[p_count] = 9; goto ptbin; }
    else if(pt_p[p_count] <= 1.2) { ptbin_p[p_count] =11; goto ptbin; }
    else if(pt_p[p_count] <= 1.4) { ptbin_p[p_count] =13; goto ptbin; }
    else if(pt_p[p_count] <= 1.6) { ptbin_p[p_count] =15; goto ptbin; }
    else if(pt_p[p_count] <= 1.8) { ptbin_p[p_count] =17; goto ptbin; }
    else if(pt_p[p_count] <= 2.0) { ptbin_p[p_count] =19; goto ptbin; }
    else if(pt_p[p_count] <= 2.2) { ptbin_p[p_count] =21; goto ptbin; }
    else if(pt_p[p_count] <= 2.4) { ptbin_p[p_count] =23; goto ptbin; }
    else if(pt_p[p_count] <= 2.6) { ptbin_p[p_count] =25; goto ptbin; }
    else if(pt_p[p_count] <= 2.8) { ptbin_p[p_count] =27; goto ptbin; }
    else if(pt_p[p_count] <= 3.2) { ptbin_p[p_count] =30; goto ptbin; }
    else if(pt_p[p_count] <= 3.6) { ptbin_p[p_count] =34; goto ptbin; }
    else if(pt_p[p_count] <= 4.0) { ptbin_p[p_count] =38; goto ptbin; }
    else if(pt_p[p_count] <= 4.4) { ptbin_p[p_count] =42; goto ptbin; }
    else if(pt_p[p_count] <= 4.8) { ptbin_p[p_count] =46; goto ptbin; }
    else if(pt_p[p_count] <= 5.2) { ptbin_p[p_count] =50; goto ptbin; }
    else if(pt_p[p_count] <= 5.6) { ptbin_p[p_count] =54; goto ptbin; }
    else if(pt_p[p_count] <= 6.0) { ptbin_p[p_count] =58; goto ptbin; }
    else                          { ptbin_p[p_count] =60; }

  ptbin:
    pt_bin_counter_p[ptbin_p[p_count]]++;
    p_count++;
    if(p_count == Flow::MAXMULT) {
      gMessMgr->Info("##### FlowDirCumulant: MAXMULT not big enough");
      break;
    }
  }
 
}

//-----------------------------------------------------------------------
void StFlowDirectCumulantMaker::CalculateTerms() {
  // Calculate terms

  if(p_count < 4) return; // Not a good event

  ClearTerms(); // Clears all the sum terms
  CalculateSumTerms(kTRUE); // Fills the sum terms for the case of integrated flow
  CalculateCorrelationsIntegrated(); // Calculates all the integrated correlations: 4-particle, 3-particle, 2-particle, and 1-particle of all needed types

  ClearTerms(); // Clears all the sum terms
  CalculateSumTerms(kFALSE);// Fills the sum terms for the case of differential flow
  CalculateCorrelationsDifferential(); // Calculates all the differential correlations: 4-particle, 3-particle, 2-particle,and 1-particle of all needed types

}

//----------------------------------------------------------------------
void StFlowDirectCumulantMaker::CalculateSumTerms(bool integrated) {
  // Calculate component sum terms

  double phi_one   = -1;
  int    ptbin_one = -1;
  int    id_one    = -1;
  double W_one     =  1;
  double W_two     =  1;
 
  // species=0 == charged_hadron,   
  // All of the sum terms used below are components of the 4,3,2 particle correlations done with 2 loops. 
  // 1-particle correlations are of course still done with 1 loop.

  for(int species=0; species<Flow::SPECIES; species++){
    //////////////////////////////////////////////////////////////////////////////
    
    int max = p_count;
    for(int one=0; one<max; one++){
      /////////////////////////////////////////////////////////////////////////////////////////
      
      phi_one   = phi_p[one];
      ptbin_one = ptbin_p[one];
      id_one    = id_p[one];
      if(integrated == kTRUE) W_one = weights_p[one];
      else W_one = 1.;
       
      sum_1_cos_diff[species][ptbin_one] += cos(2*phi_one)*W_one;
      sum_1_sin_diff[species][ptbin_one] += sin(2*phi_one)*W_one;
      sum_2_cos_diff[species][ptbin_one] += cos(-2*phi_one)*W_one;
      sum_2_sin_diff[species][ptbin_one] += sin(-2*phi_one)*W_one;
      
      sum_1_cos_full[species] += cos(2*phi_one)*W_one;
      sum_1_sin_full[species] += sin(2*phi_one)*W_one;
      sum_2_cos_full[species] += cos(-2*phi_one)*W_one;
      sum_2_sin_full[species] += sin(-2*phi_one)*W_one;      

      for(int two=0; two<p_count; two++){
	//////////////////////////////////////////////////////////////////////////////////
	
	if(integrated == kTRUE || species==0) {if(id_p[two] == id_one) continue;}	
	W_two = weights_p[two];

	sum_3_cos_diff[species][ptbin_one][ptbin_p[two]] += cos(2*(phi_one-phi_p[two]))*W_one*W_two;
        sum_3_sin_diff[species][ptbin_one][ptbin_p[two]] += sin(2*(phi_one-phi_p[two]))*W_one*W_two;

        sum_4_cos_diff[species][ptbin_one][ptbin_p[two]] += cos(2*(phi_one-phi_p[two]))*cos(2*(phi_one-phi_p[two]))*W_one*W_two*W_one*W_two;
        sum_4_sin_diff[species][ptbin_one][ptbin_p[two]] += sin(2*(phi_one-phi_p[two]))*sin(2*(phi_one-phi_p[two]))*W_one*W_two*W_one*W_two;

        sum_5_cos_diff[species][ptbin_one][ptbin_p[two]] += cos(2*(phi_one-phi_p[two]))*cos(2*phi_p[two])*W_one*W_two*W_two;
	sum_5_sin_diff[species][ptbin_one][ptbin_p[two]] += sin(2*(phi_one-phi_p[two]))*sin(2*phi_p[two])*W_one*W_two*W_two;

        sum_6_cos_sin_diff[species][ptbin_one][ptbin_p[two]] += cos(2*(phi_one-phi_p[two]))*sin(2*phi_p[two])*W_one*W_two*W_two;
	sum_6_sin_cos_diff[species][ptbin_one][ptbin_p[two]] += sin(2*(phi_one-phi_p[two]))*cos(2*phi_p[two])*W_one*W_two*W_two;

        sum_7_diff[species][ptbin_one][ptbin_p[two]] += cos(2*(phi_one-phi_p[two]))*cos(2*phi_p[two])*cos(2*phi_one)*W_one*W_two*W_one*W_two;

        sum_8_diff[species][ptbin_one][ptbin_p[two]] += cos(2*(phi_one-phi_p[two]))*cos(2*phi_p[two])*cos(2*phi_p[two])*W_one*W_two*W_two*W_two;

        sum_9_diff[species][ptbin_one][ptbin_p[two]] += cos(2*(phi_one-phi_p[two]))*sin(2*phi_p[two])*sin(-2*phi_one)*W_one*W_two*W_one*W_two;

        sum_10_diff[species][ptbin_one][ptbin_p[two]] += cos(2*(phi_one-phi_p[two]))*sin(2*phi_p[two])*sin(-2*phi_p[two])*W_one*W_two*W_two*W_two;

        sum_11_cos_diff[species][ptbin_one][ptbin_p[two]] += cos(2*(phi_one+phi_p[two]))*W_one*W_two;
        sum_11_sin_diff[species][ptbin_one][ptbin_p[two]] += sin(2*(phi_one+phi_p[two]))*W_one*W_two;

      }
    }
  
    int min_bin=0, max_bin=0;
    for(int ptbin1=0; ptbin1<Flow::PTBINS; ptbin1++){
      /////////////////////////////////////////////////////
      if(integrated == kTRUE) {min_bin=0; max_bin=Flow::PTBINS;} 
      else {min_bin = ptbin1; max_bin = ptbin1+1;}
      
      for(int ptbin2=min_bin; ptbin2<max_bin; ptbin2++){
	//////////////////////////////////////////////////
	if(integrated == kTRUE) {if(species==0) if(ptbin1==ptbin2) continue;}
	
	for(int ptbin3=0; ptbin3<Flow::PTBINS; ptbin3++){
	  ///////////////////////////////////////////
	  if(species==0) if(ptbin1==ptbin3) continue;
	  
	  sum_3_cos[species][ptbin1] += sum_3_cos_diff[species][ptbin2][ptbin3]; 
	  sum_3_sin[species][ptbin1] += sum_3_sin_diff[species][ptbin2][ptbin3];
	  	  
	  sum_4_cos[species][ptbin1] += sum_4_cos_diff[species][ptbin2][ptbin3];
	  sum_4_sin[species][ptbin1] += sum_4_sin_diff[species][ptbin2][ptbin3];
	  
	  sum_5_cos[species][ptbin1] += sum_5_cos_diff[species][ptbin2][ptbin3];
	  sum_5_sin[species][ptbin1] += sum_5_sin_diff[species][ptbin2][ptbin3];
	  
	  sum_6_cos_sin[species][ptbin1] += sum_6_cos_sin_diff[species][ptbin2][ptbin3];
	  sum_6_sin_cos[species][ptbin1] += sum_6_sin_cos_diff[species][ptbin2][ptbin3];
	  
	  sum_7[species][ptbin1] += sum_7_diff[species][ptbin2][ptbin3];

	  sum_8[species][ptbin1] += sum_8_diff[species][ptbin2][ptbin3];

	  sum_9[species][ptbin1] += sum_9_diff[species][ptbin2][ptbin3];

	  sum_10[species][ptbin1] += sum_10_diff[species][ptbin2][ptbin3];

	  sum_11_cos[species][ptbin1] += sum_11_cos_diff[species][ptbin2][ptbin3];
	  sum_11_sin[species][ptbin1] += sum_11_sin_diff[species][ptbin2][ptbin3];

	}
      }
   
      if(integrated == kTRUE){
	sum_3_cos_reference[species][ptbin1] = sum_3_cos[species][ptbin1];
	sum_3_sin_reference[species][ptbin1] = sum_3_sin[species][ptbin1];

	sum_11_cos_reference[species][ptbin1] = sum_11_cos[species][ptbin1];
	sum_11_sin_reference[species][ptbin1] = sum_11_sin[species][ptbin1];
      
	if(species==0){
	  sum_1_cos[species][ptbin1] = sum_1_cos_full[species] - sum_1_cos_diff[species][ptbin1];
	  sum_1_sin[species][ptbin1] = sum_1_sin_full[species] - sum_1_sin_diff[species][ptbin1];
	  sum_2_cos[species][ptbin1] = sum_2_cos_full[species] - sum_2_cos_diff[species][ptbin1];
	  sum_2_sin[species][ptbin1] = sum_2_sin_full[species] - sum_2_sin_diff[species][ptbin1];
	  
	  sum_1_cos_reference[species][ptbin1] = sum_1_cos_full[species] - sum_1_cos_diff[species][ptbin1];
          sum_1_sin_reference[species][ptbin1] = sum_1_sin_full[species] - sum_1_sin_diff[species][ptbin1];
          sum_2_cos_reference[species][ptbin1] = sum_2_cos_full[species] - sum_2_cos_diff[species][ptbin1];
          sum_2_sin_reference[species][ptbin1] = sum_2_sin_full[species] - sum_2_sin_diff[species][ptbin1];
	}
	else {
	  sum_1_cos[species][ptbin1] = sum_1_cos_full[species];
          sum_1_sin[species][ptbin1] = sum_1_sin_full[species];
          sum_2_cos[species][ptbin1] = sum_2_cos_full[species];
          sum_2_sin[species][ptbin1] = sum_2_sin_full[species];
	
	  sum_1_cos_reference[species][ptbin1] = sum_1_cos_full[species];
          sum_1_sin_reference[species][ptbin1] = sum_1_sin_full[species];
          sum_2_cos_reference[species][ptbin1] = sum_2_cos_full[species];
          sum_2_sin_reference[species][ptbin1] = sum_2_sin_full[species];
	}
      }
      else {
	sum_1_cos[species][ptbin1] = sum_1_cos_diff[species][ptbin1];
	sum_1_sin[species][ptbin1] = sum_1_sin_diff[species][ptbin1];
	sum_2_cos[species][ptbin1] = sum_2_cos_diff[species][ptbin1];
	sum_2_sin[species][ptbin1] = sum_2_sin_diff[species][ptbin1];
      }

    }

  }
    
}

//----------------------------------------------------------------------
void StFlowDirectCumulantMaker::Combinations(Bool_t integrated, int species, int counter) {
  // set the number of combinatinos to ref_combos_4,3,2,1,0

  ref_combos_4 = 0;
  ref_combos_3 = 0;
  ref_combos_2 = 0;
  ref_combos_1 = 0;
  ref_combos_0 = 0;

  if(integrated==kTRUE) {
    if(species==0) {
      ref_combos_4 = (p_count-pt_bin_counter_p[counter])*(p_count-pt_bin_counter_p[counter]-1)*(p_count-pt_bin_counter_p[counter]-2);
      ref_combos_4 *=(p_count-pt_bin_counter_p[counter]-3);
      ref_combos_3 = (p_count-pt_bin_counter_p[counter])*(p_count-pt_bin_counter_p[counter]-1)*(p_count-pt_bin_counter_p[counter]-2);
      ref_combos_2 = (p_count-pt_bin_counter_p[counter])*(p_count-pt_bin_counter_p[counter]-1);
      ref_combos_1 = (p_count-pt_bin_counter_p[counter]);
      ref_combos_0 = (p_count-pt_bin_counter_p[counter]);
    }
  }
  else {
    if(species==0) {
      ref_combos_4 = pt_bin_counter_p[counter]*(p_count-pt_bin_counter_p[counter])*(p_count-pt_bin_counter_p[counter]-1);
      ref_combos_4 *= (p_count-pt_bin_counter_p[counter]-2);
      ref_combos_3 = pt_bin_counter_p[counter]*(p_count-pt_bin_counter_p[counter])*(p_count-pt_bin_counter_p[counter]-1);
      ref_combos_2 = pt_bin_counter_p[counter]*(p_count-pt_bin_counter_p[counter]);
      ref_combos_1 = pt_bin_counter_p[counter];
      ref_combos_0 = (p_count-pt_bin_counter_p[counter]);
    }
  }

}

//----------------------------------------------------------------------
void StFlowDirectCumulantMaker::CalculateCorrelationsDifferential() {
  // Calculate terms for a fixed pt

  // species=0 == charged_hadron
  // This part puts the component sum terms together to form the relevant correlations

  for(int species=0; species<Flow::SPECIES; species++){
    
    for(int ptbin=0; ptbin<Flow::PTBINS; ptbin++){
      
      Combinations(kFALSE,species,ptbin);

      if(ref_combos_4 == 0) continue;
      if(ref_combos_3 == 0) continue;
      if(ref_combos_2 == 0) continue;
      if(ref_combos_1 == 0) continue;
      if(ref_combos_0 == 0) continue;

      Cumulant_Terms[TERM1][DIF][COS][species][ptbin] = sum_3_cos[species][ptbin]*sum_3_cos_reference[species][ptbin]; 
      Cumulant_Terms[TERM1][DIF][COS][species][ptbin] -= 2*(sum_5_cos[species][ptbin]*sum_2_cos_reference[species][ptbin] - sum_8[species][ptbin]);
      Cumulant_Terms[TERM1][DIF][COS][species][ptbin] += 2*(sum_6_cos_sin[species][ptbin]*sum_2_sin_reference[species][ptbin] - sum_10[species][ptbin]);
      Cumulant_Terms[TERM1][DIF][COS][species][ptbin] /= double(ref_combos_4);
      
      Cumulant_Terms[TERM2][DIF][COS][species][ptbin] = sum_3_cos[species][ptbin]/double(ref_combos_2);
      Cumulant_Terms[TERM2][DIF][SIN][species][ptbin] = sum_3_sin[species][ptbin]/double(ref_combos_2);
      
      Cumulant_Terms[TERM4][DIF][COS][species][ptbin] = sum_1_cos[species][ptbin]/double(ref_combos_1);
      Cumulant_Terms[TERM4][DIF][SIN][species][ptbin] = sum_1_sin[species][ptbin]/double(ref_combos_1);

      Cumulant_Terms[TERM7][DIF][COS][species][ptbin] = ((sum_3_cos[species][ptbin]*sum_2_cos_reference[species][ptbin] - sum_5_cos[species][ptbin]));
      Cumulant_Terms[TERM7][DIF][COS][species][ptbin] -= (sum_3_sin[species][ptbin]*sum_2_sin_reference[species][ptbin] + sum_5_sin[species][ptbin]);
      Cumulant_Terms[TERM7][DIF][COS][species][ptbin] /= double(ref_combos_3);

      Cumulant_Terms[TERM7][DIF][SIN][species][ptbin] = (sum_3_sin[species][ptbin]*sum_2_cos_reference[species][ptbin]) + (sum_3_cos[species][ptbin]*sum_2_sin_reference[species][ptbin]);
      Cumulant_Terms[TERM7][DIF][SIN][species][ptbin] -= (sum_6_sin_cos[species][ptbin] - sum_6_cos_sin[species][ptbin]);
      Cumulant_Terms[TERM7][DIF][SIN][species][ptbin] /= double(ref_combos_3);

      Cumulant_Terms[TERM9][DIF][COS][species][ptbin] = ((sum_3_cos[species][ptbin]*sum_1_cos_reference[species][ptbin] - sum_5_cos[species][ptbin]));
      Cumulant_Terms[TERM9][DIF][COS][species][ptbin] -= (sum_3_sin[species][ptbin]*sum_1_sin_reference[species][ptbin] - sum_5_sin[species][ptbin]);
      Cumulant_Terms[TERM9][DIF][COS][species][ptbin] /= double(ref_combos_3);      

      Cumulant_Terms[TERM9][DIF][SIN][species][ptbin] = (sum_3_sin[species][ptbin]*sum_1_cos_reference[species][ptbin]) + (sum_3_cos[species][ptbin]*sum_1_sin_reference[species][ptbin]);
      Cumulant_Terms[TERM9][DIF][SIN][species][ptbin] -= (sum_6_sin_cos[species][ptbin] + sum_6_cos_sin[species][ptbin]);
      Cumulant_Terms[TERM9][DIF][SIN][species][ptbin] /= double(ref_combos_3);

      Cumulant_Terms[TERM10][DIF][COS][species][ptbin] = sum_11_cos[species][ptbin]/double(ref_combos_2);
      Cumulant_Terms[TERM10][DIF][SIN][species][ptbin] = sum_11_sin[species][ptbin]/double(ref_combos_2);
       
    }
  }
}

//----------------------------------------------------------------------
void StFlowDirectCumulantMaker::CalculateCorrelationsIntegrated() {
  // Calculate terms missing a pt to avoid autocorrelations
    
  // species=0 == charged_hadron
  // Similar to CalculateCorrelationsDifferential but for the integrated flow case

  for(int species=0; species<Flow::SPECIES; species++){

    for(int ptbin=0; ptbin<Flow::PTBINS; ptbin++){

      Combinations(kTRUE,species,ptbin);

      if(ref_combos_4 == 0) continue;
      if(ref_combos_3 == 0) continue;
      if(ref_combos_2 == 0) continue;
      if(ref_combos_1 == 0) continue;
      if(ref_combos_0 == 0) continue;
      
      Cumulant_Terms[TERM1][INT][COS][species][ptbin] = sum_3_cos[species][ptbin]*sum_3_cos[species][ptbin] - 2*sum_4_cos[species][ptbin];
      Cumulant_Terms[TERM1][INT][COS][species][ptbin] -= 4*((sum_5_cos[species][ptbin]*sum_2_cos[species][ptbin] - sum_8[species][ptbin] - sum_7[species][ptbin]) - (sum_6_cos_sin[species][ptbin]*sum_2_sin[species][ptbin] - sum_10[species][ptbin] - sum_9[species][ptbin]));
      Cumulant_Terms[TERM1][INT][COS][species][ptbin] /= double(ref_combos_4);
   
      Cumulant_Terms[TERM2][INT][COS][species][ptbin] = sum_3_cos[species][ptbin]/double(ref_combos_2);
      Cumulant_Terms[TERM2][INT][SIN][species][ptbin] = sum_3_sin[species][ptbin]/double(ref_combos_2);
      Cumulant_Terms[TERM3][INT][COS][species][ptbin] = sum_3_cos[species][ptbin]/double(ref_combos_2);
      Cumulant_Terms[TERM3][INT][SIN][species][ptbin] = sum_3_sin[species][ptbin]/double(ref_combos_2);

      Cumulant_Terms[TERM4][INT][COS][species][ptbin] = sum_1_cos[species][ptbin]/double(ref_combos_1);
      Cumulant_Terms[TERM4][INT][SIN][species][ptbin] = sum_1_sin[species][ptbin]/double(ref_combos_1);

      Cumulant_Terms[TERM5][INT][COS][species][ptbin] = (sum_3_cos[species][ptbin]*sum_2_cos[species][ptbin] - sum_3_sin[species][ptbin]*sum_2_sin[species][ptbin]);
      Cumulant_Terms[TERM5][INT][COS][species][ptbin] -= 2*(sum_5_cos[species][ptbin]);
      Cumulant_Terms[TERM5][INT][COS][species][ptbin] /= double(ref_combos_3);
      Cumulant_Terms[TERM5][INT][SIN][species][ptbin] = (sum_3_cos[species][ptbin]*sum_2_sin[species][ptbin] + sum_3_sin[species][ptbin]*sum_2_cos[species][ptbin]);

      Cumulant_Terms[TERM5][INT][SIN][species][ptbin] += 2*(sum_6_cos_sin[species][ptbin]);
      Cumulant_Terms[TERM5][INT][SIN][species][ptbin] /= double(ref_combos_3);

      Cumulant_Terms[TERM6][INT][COS][species][ptbin] = sum_1_cos[species][ptbin]/double(ref_combos_1);
      Cumulant_Terms[TERM6][INT][SIN][species][ptbin] = sum_1_sin[species][ptbin]/double(ref_combos_1);

      Cumulant_Terms[TERM7][INT][COS][species][ptbin] = Cumulant_Terms[TERM5][INT][COS][species][ptbin];
      Cumulant_Terms[TERM7][INT][SIN][species][ptbin] = Cumulant_Terms[TERM5][INT][SIN][species][ptbin];
  
      Cumulant_Terms[TERM8][INT][COS][species][ptbin] = sum_2_cos[species][ptbin]/double(ref_combos_1);
      Cumulant_Terms[TERM8][INT][SIN][species][ptbin] = sum_2_sin[species][ptbin]/double(ref_combos_1);

      Cumulant_Terms[TERM9][INT][COS][species][ptbin] = ((sum_3_cos[species][ptbin]*sum_1_cos[species][ptbin] - 2*sum_5_cos[species][ptbin]));
      Cumulant_Terms[TERM9][INT][COS][species][ptbin] -= (sum_3_sin[species][ptbin]*sum_1_sin[species][ptbin]);
      Cumulant_Terms[TERM9][INT][COS][species][ptbin] /= double(ref_combos_3);
      Cumulant_Terms[TERM9][INT][SIN][species][ptbin] = ((sum_3_cos[species][ptbin]*sum_1_sin[species][ptbin] - 2*sum_6_cos_sin[species][ptbin]));
      Cumulant_Terms[TERM9][INT][SIN][species][ptbin] += (sum_3_sin[species][ptbin]*sum_1_cos[species][ptbin]);
      Cumulant_Terms[TERM9][INT][SIN][species][ptbin] /= double(ref_combos_3);

      Cumulant_Terms[TERM10][INT][COS][species][ptbin] = sum_11_cos[species][ptbin]/double(ref_combos_2);
      Cumulant_Terms[TERM10][INT][SIN][species][ptbin] = sum_11_sin[species][ptbin]/double(ref_combos_2);
      
    }
  }

}

//----------------------------------------------------------------------
void StFlowDirectCumulantMaker::Fill_Histograms() {
  // Fill term histograms
  
  Event_counter->Fill(pFlowEvent->Centrality());
  if (Mweights[zbin-1][grefmult-1] < 12.2) {
    Event_counterWeighted->Fill(pFlowEvent->Centrality(), Mweights[zbin-1][grefmult-1]);
  }

  for(int ptbin=0; ptbin<Flow::PTBINS; ptbin++){
    for(int species=0; species<Flow::SPECIES; species++){
            
      double the_pt = double(double(ptbin)/10.);  // GeV = bin/10.
      if(ptbin == Flow::PTBINS-1) Combinations(kTRUE,species,ptbin);
      else Combinations(kFALSE,species,ptbin);

      if(ref_combos_4 == 0) continue;
      if(ref_combos_3 == 0) continue;
      if(ref_combos_2 == 0) continue;
      if(ref_combos_1 == 0) continue;
      if(ref_combos_0 == 0) continue;

      if(Mweights[zbin-1][grefmult-1] > 12.2) continue;
      double PF = ref_combos_1*Mweights[zbin-1][grefmult-1]; // prefactor for Y7 centrality bias correction

      for(int phase=0; phase<Flow::PHASES; phase++){
	for(int type=0; type<Flow::TYPES; type++){
	  for(int term=0; term<Flow::TERMS; term++){
	    if(fabs(Cumulant_Terms[term][type][phase][species][ptbin]) < 1.) {
	      Term[term].Type[type].Phase[phase].Species[species].Sum_Singles->Fill(pFlowEvent->Centrality(),the_pt, PF*Cumulant_Terms[term][type][phase][species][ptbin]);
	    
	      Term[term].Type[type].Phase[phase].Species[species].Sum_Squares->Fill(pFlowEvent->Centrality(),the_pt, PF*pow(Cumulant_Terms[term][type][phase][species][ptbin],2));
	    }
	    
	    
	  }
	}
      }
      Entries[species].cent_yields->Fill(pFlowEvent->Centrality(), the_pt, ref_combos_1*Mweights[zbin-1][grefmult-1]);
    }
  }
  
}

//----------------------------------------------------------------------
void StFlowDirectCumulantMaker::ClearTerms() {
  // All the component sum terms need to be cleared twice per event: 1 for the Differential calculation, 1 for the Integrated calculation.
  
  for(int species=0; species<Flow::SPECIES; species++){

    sum_1_cos_full[species]=0;
    sum_1_sin_full[species]=0;
    sum_2_cos_full[species]=0;
    sum_2_sin_full[species]=0;

    for(int ptbin1=0; ptbin1<Flow::PTBINS; ptbin1++){

      for(int ptbin2=0; ptbin2<Flow::PTBINS; ptbin2++){
	
	sum_1_cos_diff[species][ptbin1]=0, sum_1_sin_diff[species][ptbin1]=0;
        sum_2_cos_diff[species][ptbin1]=0, sum_2_sin_diff[species][ptbin1]=0;
	sum_3_cos_diff[species][ptbin1][ptbin2]=0, sum_3_sin_diff[species][ptbin1][ptbin2]=0;
	sum_4_cos_diff[species][ptbin1][ptbin2]=0, sum_4_sin_diff[species][ptbin1][ptbin2]=0;
	sum_5_cos_diff[species][ptbin1][ptbin2]=0, sum_5_sin_diff[species][ptbin1][ptbin2]=0;
	sum_6_cos_sin_diff[species][ptbin1][ptbin2]=0, sum_6_sin_cos_diff[species][ptbin1][ptbin2]=0;
	sum_7_diff[species][ptbin1][ptbin2]=0, sum_8_diff[species][ptbin1][ptbin2]=0, sum_9_diff[species][ptbin1][ptbin2]=0, sum_10_diff[species][ptbin1][ptbin2]=0;
	sum_11_cos_diff[species][ptbin1][ptbin2]=0, sum_11_sin_diff[species][ptbin1][ptbin2]=0;

	sum_1_cos[species][ptbin1]=0, sum_1_sin[species][ptbin1]=0;
        sum_2_cos[species][ptbin1]=0, sum_2_sin[species][ptbin1]=0;
        sum_3_cos[species][ptbin1]=0, sum_3_sin[species][ptbin1]=0;
        sum_4_cos[species][ptbin1]=0, sum_4_sin[species][ptbin1]=0;
        sum_5_cos[species][ptbin1]=0, sum_5_sin[species][ptbin1]=0;
        sum_6_cos_sin[species][ptbin1]=0, sum_6_sin_cos[species][ptbin1]=0;
        sum_7[species][ptbin1]=0, sum_8[species][ptbin1]=0, sum_9[species][ptbin1]=0, sum_10[species][ptbin1]=0;
        sum_11_cos[species][ptbin1]=0, sum_11_sin[species][ptbin1]=0;

      }
      
    }
  }
  
}

//-----------------------------------------------------------------------
Int_t StFlowDirectCumulantMaker::Finish() {
  // Wtite out file of term histograms

  cout << endl << "##### Direct Cumulant Maker:" << endl;
     
  // Write all terms
  const char *flowname = "flow.dirCumulant.root";
  TFile histFile(flowname, "RECREATE");
  
  GetHistList()->Write();
  //GetHistList()->ls();
  
  histFile.Close();
  
  delete pFlowSelect;
  
  return StMaker::Finish();
}

//-----------------------------------------------------------------------
