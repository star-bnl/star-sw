////////////////////////////////////////////////////////////////////////////
//
// $Id: directCumulants_v2.C,v 1.3 2010/06/10 16:33:57 posk Exp $
//
// Authors: Dhevan Gangadharan, UCLA, Jan 2010
//
///////////////////////////////////////////////////////////////////////////////
//
// Description:  Macro to sum the Terms from the output of StFlowDirectCumulantMaker,
//    plot v_2{2] and v_2{4}, and output the graphs to flow.diCrumulant.graphs.root.
//    You can plot them again with plotDirectCumulants_v2pt.C .
//    cent=1 is the most central, cent=0 is the yield weighted average of all the others.
//
////////////////////////////////////////////////////////////////////////////

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <Riostream.h>

#include "TFile.h"
#include "TString.h"
#include "TH1.h"
#include "TH2.h"
#include "TGraphErrors.h"
#include "TArray.h"
#include "TLegend.h"
#include "TStyle.h"

void directCumulants_v2(){

  TCanvas* cOld = (TCanvas*)gROOT->GetListOfCanvases(); // delete old canvas
  if (cOld) cOld->Delete();

  bool ptGraphs = kFALSE;

  //TFile *myfile = new TFile("Y4_test4.root","READ");
  TFile *myfile = new TFile("flow.dirCumulant.root","READ");
  //TFile *myfile = new TFile("~dhevang/test.root","READ");
  const int CENTBINS= 9;
  float x[CENTBINS] = {2.5, 7.5, 15., 25., 35., 45., 55., 65., 75.};
  
  gROOT->SetStyle("Plain");                              // set style
  //gStyle->SetOptDate(0);
  gStyle->SetOptStat(0);
  
  const int TERMS=    10; // terms needed to calculate the cumulants
  const int TYPES=     2; // differential or integrated
  const int PHASES=    2; // sin or cos
  const int SPECIES=   1; // charged particles = 0
  const int PTBINS=   62; // max pt bins
  const int PTINTBINS=19; // max pt bins for pt integration
  int ptOverflow = PTBINS-1;
  int ptCu = PTBINS-2;    // integrated cumulants
  
  double yields[SPECIES][PTBINS]={{0}};
  double cent_yields[SPECIES][CENTBINS][PTBINS]={{{0}}};
  
  // centrality 0 is min bias
  double v2_2[SPECIES][CENTBINS+1][PTBINS]={{{0.}}}; // differential
  double v2_2_e[SPECIES][CENTBINS+1][PTBINS]={{{0.}}};
  double v2_4[SPECIES][CENTBINS+1][PTBINS]={{{0.}}};
  double v2_4_e[SPECIES][CENTBINS+1][PTBINS]={{{0.}}};
  double v2IntPt_2[SPECIES][CENTBINS+1]={{0.}}; // integral
  double v2IntPt_2_e[SPECIES][CENTBINS+1]={{0.}};
  double v2IntPt_4[SPECIES][CENTBINS+1]={{0.}};
  double v2IntPt_4_e[SPECIES][CENTBINS+1]={{0.}};
  double used_cent_yields2[SPECIES][CENTBINS+1]={{0.}};
  double used_cent_yields4[SPECIES][CENTBINS+1]={{0.}};
  double usedInt_cent_yields2[SPECIES][CENTBINS+1]={{0.}};
  double usedInt_cent_yields4[SPECIES][CENTBINS+1]={{0.}};
 
  TString *name;
  TString *name1;
  TString *name2;
  TString *name3;
  TString *name4;
  
  double Terms[TERMS][TYPES][PHASES][SPECIES][CENTBINS][PTBINS]={{{{{{0}}}}}};
  double Terms_e[TERMS][TYPES][PHASES][SPECIES][CENTBINS][PTBINS]={{{{{{0}}}}}};
  
  //////////////////////////////////////////////////////////////////////////////////
  // This part takes all the info out of the input file and puts it into the "Terms" arrays
  
  for(int species=0; species<SPECIES; species++){

    name = new TString("Cent_Pt_yields_");
    *name += species;

    for(int cent=0; cent<CENTBINS; cent++){
      
      for(int ptbin=0; ptbin<PTBINS; ptbin++){
	
        cent_yields[species][cent][ptbin] = ((TH2D*)myfile->Get(name->Data()))->GetBinContent(cent+1,ptbin+1);
        yields[species][ptbin] += cent_yields[species][cent][ptbin];
	
      }
    }
  }
  
  for(int term=0; term<TERMS; term++){

    name1 = new TString("Term_");
    *name1 += term+1;

    for(int type=0; type<TYPES; type++){
      
      name2 = new TString();
      if(type==0) name2->Append("_D_"); // differential
      else name2->Append("_I_"); // integral

      for(int phase=0; phase<PHASES; phase++){

	name3 = new TString();
	if(phase==0) name3->Append("cos_");
	else name3->Append("sin_");
       	for(int species=0; species<SPECIES; species++){

	  name4 = new TString();
	  *name4 += species;

	  for(int cent=0; cent<CENTBINS; cent++){
	    
	    for(int ptbin=0; ptbin<PTBINS; ptbin++){
	      
	      if(cent_yields[species][cent][ptbin] < 1) continue;

	      name = new TString();
	      name->Append(name1->Data());
	      name->Append(name2->Data());
	      name->Append(name3->Data());
	      name->Append(name4->Data());

	      Terms[term][type][phase][species][cent][ptbin] = ((TH2D*)myfile->Get(name->Data()))->GetBinContent(cent+1,ptbin+1)/cent_yields[species][cent][ptbin];

	      name->Append("_Sq");	      
	      Terms_e[term][type][phase][species][cent][ptbin] = ((TH2D*)myfile->Get(name->Data()))->GetBinContent(cent+1,ptbin+1)/cent_yields[species][cent][ptbin];
	      Terms_e[term][type][phase][species][cent][ptbin] -= pow(Terms[term][type][phase][species][cent][ptbin],2);
	      Terms_e[term][type][phase][species][cent][ptbin] /= cent_yields[species][cent][ptbin];
	      Terms_e[term][type][phase][species][cent][ptbin] = sqrt(Terms_e[term][type][phase][species][cent][ptbin]);
	      
	    }
	  }
	}
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////////////////////
  int COS=0, SIN=1, I=1;

  double Cumulant_term[11][TYPES][SPECIES][CENTBINS][PTBINS]={{{{{0}}}}};
  double Cumulant_term_e[11][TYPES][SPECIES][CENTBINS][PTBINS]={{{{{0}}}}};
  double Cumulant_4[TYPES][SPECIES][CENTBINS][PTBINS]={{{{0}}}};
  double Cumulant_4_e[TYPES][SPECIES][CENTBINS][PTBINS]={{{{0}}}};
  double Cumulant_2[TYPES][SPECIES][CENTBINS][PTBINS]={{{{0}}}};
  double Cumulant_2_e[TYPES][SPECIES][CENTBINS][PTBINS]={{{{0}}}};

  // This part reconstructs all the Cumulant terms from "Terms"
  for(int type=0; type<TYPES; type++){
    
    for(int species=0; species<SPECIES; species++){

      for(int cent=0; cent<CENTBINS; cent++){
	
	for(int ptbin=0; ptbin<PTBINS; ptbin++){
	  	  
	  Cumulant_term[0][type][species][cent][ptbin] = Terms[0][type][COS][species][cent][ptbin];
	  Cumulant_term_e[0][type][species][cent][ptbin] = Terms_e[0][type][COS][species][cent][ptbin];

	  Cumulant_term[1][type][species][cent][ptbin] = Terms[1][type][COS][species][cent][ptbin]*Terms[2][I][COS][species][cent][ptbin];
	  Cumulant_term[1][type][species][cent][ptbin] -= Terms[1][type][SIN][species][cent][ptbin]*Terms[2][I][SIN][species][cent][ptbin];
	  Cumulant_term_e[1][type][species][cent][ptbin] = pow(Terms_e[1][type][COS][species][cent][ptbin]*Terms[2][I][COS][species][cent][ptbin],2);
	  Cumulant_term_e[1][type][species][cent][ptbin] += pow(Terms[1][type][COS][species][cent][ptbin]*Terms_e[2][I][COS][species][cent][ptbin],2);
	  Cumulant_term_e[1][type][species][cent][ptbin] += pow(Terms_e[1][type][SIN][species][cent][ptbin]*Terms[2][I][SIN][species][cent][ptbin],2);
          Cumulant_term_e[1][type][species][cent][ptbin] += pow(Terms[1][type][SIN][species][cent][ptbin]*Terms_e[2][I][SIN][species][cent][ptbin],2);
	  Cumulant_term_e[1][type][species][cent][ptbin] = sqrt(Cumulant_term_e[1][type][species][cent][ptbin]);

	  Cumulant_term[2][type][species][cent][ptbin] = Terms[3][type][COS][species][cent][ptbin]*Terms[4][I][COS][species][cent][ptbin];
          Cumulant_term[2][type][species][cent][ptbin] -= Terms[3][type][SIN][species][cent][ptbin]*Terms[4][I][SIN][species][cent][ptbin];
	  
	  Cumulant_term[3][type][species][cent][ptbin] = Terms[5][I][COS][species][cent][ptbin]*Terms[6][type][COS][species][cent][ptbin];
          Cumulant_term[3][type][species][cent][ptbin] -= Terms[5][I][SIN][species][cent][ptbin]*Terms[6][type][SIN][species][cent][ptbin];

	  Cumulant_term[4][type][species][cent][ptbin] = Terms[7][I][COS][species][cent][ptbin]*Terms[8][type][COS][species][cent][ptbin];
          Cumulant_term[4][type][species][cent][ptbin] -= Terms[7][I][SIN][species][cent][ptbin]*Terms[8][type][SIN][species][cent][ptbin];

	  Cumulant_term[5][type][species][cent][ptbin] = Terms[9][type][COS][species][cent][ptbin]*Terms[9][I][COS][species][cent][ptbin];
          Cumulant_term[5][type][species][cent][ptbin] -= Terms[9][type][SIN][species][cent][ptbin]*Terms[9][I][SIN][species][cent][ptbin];

	  Cumulant_term[6][type][species][cent][ptbin] = Terms[3][type][COS][species][cent][ptbin]*Terms[7][I][COS][species][cent][ptbin]*Terms[2][I][COS][species][cent][ptbin];
	  Cumulant_term[6][type][species][cent][ptbin] -= Terms[3][type][SIN][species][cent][ptbin]*Terms[7][I][SIN][species][cent][ptbin]*Terms[2][I][COS][species][cent][ptbin];
	  Cumulant_term[6][type][species][cent][ptbin] -= Terms[3][type][SIN][species][cent][ptbin]*Terms[7][I][COS][species][cent][ptbin]*Terms[2][I][SIN][species][cent][ptbin];
	  Cumulant_term[6][type][species][cent][ptbin] -= Terms[3][type][COS][species][cent][ptbin]*Terms[7][I][SIN][species][cent][ptbin]*Terms[2][I][SIN][species][cent][ptbin];

	  Cumulant_term_e[6][type][species][cent][ptbin] = pow(Terms[3][type][SIN][species][cent][ptbin]*Terms[7][I][SIN][species][cent][ptbin]*Terms_e[2][I][COS][species][cent][ptbin],2);
	  Cumulant_term_e[6][type][species][cent][ptbin] = sqrt(Cumulant_term_e[6][I][species][cent][ptbin]);


	  Cumulant_term[7][type][species][cent][ptbin] = Terms[3][I][COS][species][cent][ptbin]*Terms[7][I][COS][species][cent][ptbin]*Terms[1][type][COS][species][cent][ptbin];
          Cumulant_term[7][type][species][cent][ptbin] -= Terms[3][I][SIN][species][cent][ptbin]*Terms[7][I][SIN][species][cent][ptbin]*Terms[1][type][COS][species][cent][ptbin];
          Cumulant_term[7][type][species][cent][ptbin] -= Terms[3][I][SIN][species][cent][ptbin]*Terms[7][I][COS][species][cent][ptbin]*Terms[1][type][SIN][species][cent][ptbin];
          Cumulant_term[7][type][species][cent][ptbin] -= Terms[3][I][COS][species][cent][ptbin]*Terms[7][I][SIN][species][cent][ptbin]*Terms[1][type][SIN][species][cent][ptbin];

	  Cumulant_term_e[7][type][species][cent][ptbin] = pow(Terms[3][I][SIN][species][cent][ptbin]*Terms[7][I][SIN][species][cent][ptbin]*Terms_e[1][type][COS][species][cent][ptbin],2);
          Cumulant_term_e[7][type][species][cent][ptbin] = sqrt(Cumulant_term_e[7][type][species][cent][ptbin]);


	  Cumulant_term[8][type][species][cent][ptbin] = Terms[3][type][COS][species][cent][ptbin]*Terms[5][I][COS][species][cent][ptbin]*Terms[9][I][COS][species][cent][ptbin];
	  Cumulant_term[8][type][species][cent][ptbin] -= Terms[3][type][SIN][species][cent][ptbin]*Terms[5][I][SIN][species][cent][ptbin]*Terms[9][I][COS][species][cent][ptbin];
	  Cumulant_term[8][type][species][cent][ptbin] -= Terms[3][type][SIN][species][cent][ptbin]*Terms[5][I][COS][species][cent][ptbin]*Terms[9][I][SIN][species][cent][ptbin];
	  Cumulant_term[8][type][species][cent][ptbin] -= Terms[3][type][COS][species][cent][ptbin]*Terms[5][I][SIN][species][cent][ptbin]*Terms[9][I][SIN][species][cent][ptbin];
	  
	  Cumulant_term[9][type][species][cent][ptbin] = Terms[7][I][COS][species][cent][ptbin]*Terms[7][I][COS][species][cent][ptbin]*Terms[9][type][COS][species][cent][ptbin];
          Cumulant_term[9][type][species][cent][ptbin] -= Terms[7][I][SIN][species][cent][ptbin]*Terms[7][I][SIN][species][cent][ptbin]*Terms[9][type][COS][species][cent][ptbin];
          Cumulant_term[9][type][species][cent][ptbin] -= Terms[7][I][SIN][species][cent][ptbin]*Terms[7][I][COS][species][cent][ptbin]*Terms[9][type][SIN][species][cent][ptbin];
          Cumulant_term[9][type][species][cent][ptbin] -= Terms[7][I][COS][species][cent][ptbin]*Terms[7][I][SIN][species][cent][ptbin]*Terms[9][type][SIN][species][cent][ptbin];

	  Cumulant_term[10][type][species][cent][ptbin] = Terms[3][type][COS][species][cent][ptbin]*Terms[5][I][COS][species][cent][ptbin]*Terms[7][I][COS][species][cent][ptbin]*Terms[7][I][COS][species][cent][ptbin];
	  Cumulant_term[10][type][species][cent][ptbin] -= Terms[3][type][SIN][species][cent][ptbin]*Terms[5][I][SIN][species][cent][ptbin]*Terms[7][I][COS][species][cent][ptbin]*Terms[7][I][COS][species][cent][ptbin];
	  Cumulant_term[10][type][species][cent][ptbin] -= Terms[3][type][SIN][species][cent][ptbin]*Terms[5][I][COS][species][cent][ptbin]*Terms[7][I][SIN][species][cent][ptbin]*Terms[7][I][COS][species][cent][ptbin];
	  Cumulant_term[10][type][species][cent][ptbin] -= Terms[3][type][SIN][species][cent][ptbin]*Terms[5][I][COS][species][cent][ptbin]*Terms[7][I][COS][species][cent][ptbin]*Terms[7][I][SIN][species][cent][ptbin];
	  Cumulant_term[10][type][species][cent][ptbin] -= Terms[3][type][COS][species][cent][ptbin]*Terms[5][I][SIN][species][cent][ptbin]*Terms[7][I][SIN][species][cent][ptbin]*Terms[7][I][COS][species][cent][ptbin];
	  Cumulant_term[10][type][species][cent][ptbin] -= Terms[3][type][COS][species][cent][ptbin]*Terms[5][I][SIN][species][cent][ptbin]*Terms[7][I][COS][species][cent][ptbin]*Terms[7][I][SIN][species][cent][ptbin];
	  Cumulant_term[10][type][species][cent][ptbin] -= Terms[3][type][COS][species][cent][ptbin]*Terms[5][I][COS][species][cent][ptbin]*Terms[7][I][SIN][species][cent][ptbin]*Terms[7][I][SIN][species][cent][ptbin];
	  Cumulant_term[10][type][species][cent][ptbin] += Terms[3][type][SIN][species][cent][ptbin]*Terms[5][I][SIN][species][cent][ptbin]*Terms[7][I][SIN][species][cent][ptbin]*Terms[7][I][SIN][species][cent][ptbin];


	  Cumulant_4[type][species][cent][ptbin] = Cumulant_term[0][type][species][cent][ptbin] -2*Cumulant_term[1][type][species][cent][ptbin];
	  Cumulant_4[type][species][cent][ptbin] -= Cumulant_term[2][type][species][cent][ptbin] + Cumulant_term[3][type][species][cent][ptbin];
	  Cumulant_4[type][species][cent][ptbin] -= 2*Cumulant_term[4][type][species][cent][ptbin];
	  Cumulant_4[type][species][cent][ptbin] -= Cumulant_term[5][type][species][cent][ptbin];
	  Cumulant_4[type][species][cent][ptbin] += 4*Cumulant_term[6][type][species][cent][ptbin];
	  Cumulant_4[type][species][cent][ptbin] += 4*Cumulant_term[7][type][species][cent][ptbin];
	  Cumulant_4[type][species][cent][ptbin] += 2*Cumulant_term[8][type][species][cent][ptbin];
	  Cumulant_4[type][species][cent][ptbin] += 2*Cumulant_term[9][type][species][cent][ptbin];
	  Cumulant_4[type][species][cent][ptbin] -= 6*Cumulant_term[10][type][species][cent][ptbin];
	  
	  //////////// Print Cumulant Terms ////////////////////////////
// 	  if (ptbin==ptCu && type==1) { // integrated cumulants
// 	    cout << endl << "centrality= " << cent+1 << endl;
// 	    double noAcc = Cumulant_term[0][type][species][cent][ptbin] -2*Cumulant_term[1][type][species][cent][ptbin];
// 	    cout << "noAcc: " << setprecision(4) << noAcc << endl;
// 	    cout << " 2: " << -Cumulant_term[2][type][species][cent][ptbin] << endl;
// 	    cout << " 3: " << -Cumulant_term[3][type][species][cent][ptbin] << endl;
// 	    cout << " 4: " << -2*Cumulant_term[4][type][species][cent][ptbin] << endl;
// 	    cout << " 5: " << -Cumulant_term[5][type][species][cent][ptbin] << endl;
// 	    cout << " 6: " <<  4*Cumulant_term[6][type][species][cent][ptbin] << endl;
// 	    cout << " 7: " <<  4*Cumulant_term[7][type][species][cent][ptbin] << endl;
// 	    cout << " 8: " <<  2*Cumulant_term[8][type][species][cent][ptbin] << endl;
// 	    cout << " 9: " <<  2*Cumulant_term[9][type][species][cent][ptbin] << endl;
// 	    cout << "10: " << -6*Cumulant_term[10][type][species][cent][ptbin] << endl;
// 	    cout << "Acc/noAcc: " << Cumulant_4[type][species][cent][ptbin] / noAcc << endl;
// 	    if(Cumulant_4[1][0][cent][ptCu]<0 && noAcc<0) {
// 	      cout << "v2NoAcc: " << pow(-noAcc,0.25) << endl;
// 	      cout << "v2Acc: " << pow(-Cumulant_4[1][0][cent][ptCu],0.25) << endl;
// 	      cout << "v2Acc/noAcc: " << pow(-Cumulant_4[1][0][cent][ptCu],0.25) / pow(-noAcc,0.25) << endl;
// 	    }
// 	  }
	  //////////////////////////
	  
	  Cumulant_4_e[type][species][cent][ptbin] = Cumulant_term_e[0][type][species][cent][ptbin]**2;
	  Cumulant_4_e[type][species][cent][ptbin] += 2*(Cumulant_term_e[1][type][species][cent][ptbin])**2;
	  Cumulant_4_e[type][species][cent][ptbin] += 4*(Cumulant_term_e[6][type][species][cent][ptbin])**2;
	  Cumulant_4_e[type][species][cent][ptbin] += 4*(Cumulant_term_e[7][type][species][cent][ptbin])**2;
	  Cumulant_4_e[type][species][cent][ptbin] = sqrt(Cumulant_4_e[type][species][cent][ptbin]);
	  

	  Cumulant_2[type][species][cent][ptbin] = Terms[1][type][COS][species][cent][ptbin];
	  Cumulant_2[type][species][cent][ptbin] -= Terms[3][type][COS][species][cent][ptbin]*Terms[7][type][COS][species][cent][ptbin];
	  
	  Cumulant_2_e[type][species][cent][ptbin] = Terms_e[1][type][COS][species][cent][ptbin]**2;
	  Cumulant_2_e[type][species][cent][ptbin] += (Terms_e[3][type][COS][species][cent][ptbin]*Terms[7][type][COS][species][cent][ptbin])**2;
	  Cumulant_2_e[type][species][cent][ptbin] += (Terms[3][type][COS][species][cent][ptbin]*Terms_e[7][type][COS][species][cent][ptbin])**2;
	  Cumulant_2_e[type][species][cent][ptbin] = sqrt(Cumulant_2_e[type][species][cent][ptbin]);

    	 
	}
      }
    }
  }

  ////////////////////////////////////////////////////////////////////////////////////////
  // each pt bin is calculated for all centralities, then the next pt bin
  int    ptcount[SPECIES]={0}; // index of pt bins
  double used_pt_yields4[CENTBINS+1] = {0.};
  double used_pt_yields2[CENTBINS+1] = {0.};
  double v2IntCu_2[CENTBINS] = {0.};
  double v2IntCu_2Err[CENTBINS] = {0.};
  double v2IntCu_4[CENTBINS] = {0.};
  double v2IntCu_4Err[CENTBINS] = {0.};
  
  // This part reconstructs the elliptic flow from the Cumulant terms
  for(int species=0; species<SPECIES; species++){
    
    cout << endl << "cent: \t v2IntCu_2 +/- err,\t v2IntCu_4 +/- err" << endl;

    for(int ptbin=0; ptbin<PTBINS; ptbin++){
      
      // defaults for min bias set to an unphysical result
      v2_2[species][0][ptcount[species]]   = 10.;
      v2_2_e[species][0][ptcount[species]] = 10.;
      v2_4[species][0][ptcount[species]]   = 10.;
      v2_4_e[species][0][ptcount[species]] = 10.;
      used_pt_yields4[0] = 0.; // reset for each pt and species
      used_pt_yields2[0] = 0.;
      
      for(int cent=0; cent<CENTBINS; cent++){
	if (ptbin==ptCu) { // integrated cumulants
	  if(Cumulant_2[1][0][cent][ptCu]>0) {
	    v2IntCu_2[cent]    = pow(Cumulant_2[1][0][cent][ptCu],0.5);
	    v2IntCu_2Err[cent] = pow((.5/(Cumulant_2[1][0][cent][ptCu])),0.5)*Cumulant_2_e[1][0][cent][ptCu];
	  }
	  if(Cumulant_4[1][0][cent][ptCu]<0) {
	    v2IntCu_4[cent]    = pow(-Cumulant_4[1][0][cent][ptCu],0.25);
	    v2IntCu_4Err[cent] = pow((.25/(-Cumulant_4[1][0][cent][ptCu])),0.75)*Cumulant_4_e[1][0][cent][ptCu];
	  }
	  cout << cent+1 << ": \t" << setprecision(3) << v2IntCu_2[cent]*100. << 
	    " \t+/- " << v2IntCu_2Err[cent]*100. << ", \t" << v2IntCu_4[cent]*100. <<
	    " \t+/- " << v2IntCu_4Err[cent]*100. << endl;
	}

	// defaults for each cent set to an unphysical result
	v2_2[species][cent+1][ptcount[species]]   = 10.;
	v2_2_e[species][cent+1][ptcount[species]] = 10.;
	v2_4[species][cent+1][ptcount[species]]   = 10.;
	v2_4_e[species][cent+1][ptcount[species]] = 10.;
	used_pt_yields4[cent+1] = 0.;
	used_pt_yields2[cent+1] = 0.;

	// 4-particle cumulant v2
	if(Cumulant_4[1][species][cent][ptbin] < 0 && Cumulant_4[0][species][cent][ptbin] < 0) { // both must be negative
	  v2_4[species][cent+1][ptcount[species]] += -cent_yields[species][cent][ptbin]*Cumulant_4[0][species][cent][ptbin]/(pow(-Cumulant_4[1][species][cent][ptbin],.75));
	  v2_4[species][0][ptcount[species]] += v2_4[species][cent+1][ptcount[species]];
	  if(ptbin<PTINTBINS) { // pt integration
	    v2IntPt_4[species][cent+1] += v2_4[species][cent+1][ptcount[species]];
	    usedInt_cent_yields4[species][cent+1] += cent_yields[species][cent][ptbin];
	  }
	  v2_4_e[species][cent+1][ptcount[species]] += pow(cent_yields[species][cent][ptbin]*Cumulant_4_e[0][species][cent][ptbin]/(pow(-Cumulant_4[1][species][cent][ptbin],.75)),2);
	  v2_4_e[species][cent+1][ptcount[species]] += pow(.75*cent_yields[species][cent][ptbin]*Cumulant_4[0][species][cent][ptbin]*Cumulant_4_e[1][species][cent][ptbin]/(pow(-Cumulant_4[1][species][cent][ptbin],1.75)),2);
	  v2_4_e[species][0][ptcount[species]] += v2_4_e[species][cent+1][ptcount[species]];
	  if(ptbin<PTINTBINS) v2IntPt_4_e[species][cent+1] += v2_4_e[species][cent+1][ptcount[species]];

	  used_pt_yields4[cent+1] = cent_yields[species][cent][ptbin];
	  used_cent_yields4[species][cent+1] += cent_yields[species][cent][ptbin];
	  used_pt_yields4[0] += used_pt_yields4[cent+1]; // min bias
	}//if
	
	// 2-particle cumulant v2 
	if(Cumulant_2[1][species][cent][ptbin] > 0) { // must be positive
	  v2_2[species][cent+1][ptcount[species]] += cent_yields[species][cent][ptbin]*Cumulant_2[0][species][cent][ptbin]/(pow(Cumulant_2[1][species][cent][ptbin],.5));
	  v2_2[species][0][ptcount[species]] += v2_2[species][cent+1][ptcount[species]];
	  if(ptbin<PTINTBINS) { // pt integration
	    v2IntPt_2[species][cent+1] += v2_2[species][cent+1][ptcount[species]];
	    usedInt_cent_yields2[species][cent+1] += cent_yields[species][cent][ptbin];
	  }	  
	  v2_2_e[species][cent+1][ptcount[species]] += pow(cent_yields[species][cent][ptbin]*Cumulant_2_e[0][species][cent][ptbin]/(pow(Cumulant_2[1][species][cent][ptbin],.5)),2);
	  v2_2_e[species][cent+1][ptcount[species]] += pow(.5*cent_yields[species][cent][ptbin]*Cumulant_2[0][species][cent][ptbin]*Cumulant_2_e[1][species][cent][ptbin]/(pow(Cumulant_2[1][species][cent][ptbin],1.5)),2);
	  v2_2_e[species][0][ptcount[species]] += v2_2_e[species][cent+1][ptcount[species]];
	  if(ptbin<PTINTBINS) v2IntPt_2_e[species][cent+1] += v2_2_e[species][cent+1][ptcount[species]];
	  
	  used_pt_yields2[cent+1] = cent_yields[species][cent][ptbin];
	  used_cent_yields2[species][cent+1] += cent_yields[species][cent][ptbin];
	  used_pt_yields2[0] += used_pt_yields2[cent+1]; // min bias
	}//if
				    
      }//cent
      
      for(int cent=0; cent<CENTBINS+1; cent++){ // includes cent=0
	
	if(used_pt_yields2[cent] > 0){
	  v2_2[species][cent][ptcount[species]] /= used_pt_yields2[cent];
	  v2_2_e[species][cent][ptcount[species]] = sqrt(v2_2_e[species][cent][ptcount[species]]);
	  v2_2_e[species][cent][ptcount[species]] /= used_pt_yields2[cent];
	}
	if(used_pt_yields4[cent] > 0){
	  v2_4[species][cent][ptcount[species]] /= used_pt_yields4[cent];
	  v2_4_e[species][cent][ptcount[species]] = sqrt(v2_4_e[species][cent][ptcount[species]]);
	  v2_4_e[species][cent][ptcount[species]] /= used_pt_yields4[cent];
	}

      }//cent
      
      ptcount[species]++;
    }//pt

    // For pt integrated v2
    for(int cent=0; cent<CENTBINS+1; cent++){ // includes cent=0
      if(usedInt_cent_yields2[species][cent] > 0){
	v2IntPt_2[species][cent] /= usedInt_cent_yields2[species][cent];
	v2IntPt_2_e[species][cent] = sqrt(v2IntPt_2_e[species][cent]);
	v2IntPt_2_e[species][cent] /= usedInt_cent_yields2[species][cent];
      }
      if(usedInt_cent_yields4[species][cent] > 0){
	v2IntPt_4[species][cent] /= usedInt_cent_yields4[species][cent];
	v2IntPt_4_e[species][cent] = sqrt(v2IntPt_4_e[species][cent]);
	v2IntPt_4_e[species][cent] /= usedInt_cent_yields4[species][cent];
      }
    }//cent

  }//species
  
  cout << endl;
  for(int ptbin=0; ptbin<PTBINS; ptbin++){ // for checking
    //cout << ptbin << ": " << v2_2[0][3][ptbin] << " +/- " << v2_2_e[0][3][ptbin] << endl;
    //cout << ptbin << ": " << cent_yields[0][3][ptbin] << endl;
    //cout << ptbin << ": " << Cumulant_2[1][0][5][ptbin] << endl;
  }

  cout << endl << "cent: \t v2IntPt_2 +/- err,\t v2IntPt_4 +/- err" << endl;
  for(int cent=0; cent<CENTBINS+1; cent++){

    //Throw away last point since its middle-pt value is not well defined.
    //It represents all particles with pt > 6 GeV/c
    //cout << ptOverflow << ": " << v2_2[0][cent][ptOverflow] << endl;
    v2_2[0][cent][ptOverflow]= 10.;
    v2_4[0][cent][ptOverflow]= 10.;
    //Throw away next-to-last point since it contains the integrated cumulant.
    v2_2[0][cent][ptCu]= 10.;
    v2_4[0][cent][ptCu]= 10.;
    //cout << ptOverflow << ": " << v2_2[0][cent][ptOverflow] << endl; // pt = 10?

    // output integrated over pt
    cout << cent << ": \t" << setprecision(3) << v2IntPt_2[0][cent]*100. << " \t+/- " <<
      v2IntPt_2_e[0][cent]*100. << ", \t" << v2IntPt_4[0][cent]*100. << " \t+/- " <<
      v2IntPt_4_e[0][cent]*100. << endl;

  }//cent
  cout << endl;

  ////////////////////////////////////////////////////////////////////////////////////////////
  // plot the results 

  TFile graphFile("flow.dirCumulant.graphs.root", "RECREATE"); // output

  // For v2(pt)

  double middle_pt_points[PTBINS]={0};
  for(int i=0; i<PTBINS; i++){
    middle_pt_points[i]=double((i)/10. + 0.1); // GeV = bin/10.
  }
  double middle_pt_points_e[PTBINS]={0}; // no errors
  
  float v2max = 0.4;
  
  TGraphErrors *charged_v2_2[CENTBINS+1];
  TGraphErrors *charged_v2_4[CENTBINS+1];
  TCanvas* can[CENTBINS+1];
  int canvasWidth = 780, canvasHeight = 600;             // landscape
  for(int cent=0; cent<CENTBINS+1; cent++){
    TString* canName = new TString("centrality_"); 
    *canName += (cent); 
    if(ptGraphs) {
      cout << "plot " << canName->Data() << endl;
      can[cent] = new TCanvas(canName->Data(), canName->Data(), canvasWidth, canvasHeight);
    }
    TLegend *legend = new TLegend(.1,.7,.46,.9,NULL,"brNDC");
    legend->SetFillColor(kWhite);
    TPaveLabel* title = new TPaveLabel(0.7,0.96,0.9,0.99,canName->Data());
    if(ptGraphs) title->Draw();

    TString graphName("v22_"); 
    graphName += (cent); 
    charged_v2_2[cent]= new TGraphErrors(PTBINS,middle_pt_points,v2_2[0][cent],middle_pt_points_e,v2_2_e[0][cent]);
    charged_v2_2[cent]->SetMarkerStyle(20);
    charged_v2_2[cent]->SetMarkerColor(2);
    charged_v2_2[cent]->SetLineColor(2);
    charged_v2_2[cent]->SetMinimum(0.);
    charged_v2_2[cent]->SetMaximum(v2max);
    charged_v2_2[cent]->GetXaxis()->SetTitle("P_{t} (GeV/c)");
    charged_v2_2[cent]->GetYaxis()->SetTitle("v_{2}");
    charged_v2_2[cent]->SetTitle("Elliptic Flow");
    charged_v2_2[cent]->Write(graphName.Data());
    
    TString graphName("v24_"); 
    graphName += (cent); 
    charged_v2_4[cent]= new TGraphErrors(PTBINS,middle_pt_points,v2_4[0][cent],middle_pt_points_e,v2_4_e[0][cent]);
    charged_v2_4[cent]->SetMarkerStyle(20);
    charged_v2_4[cent]->SetMarkerColor(4);
    charged_v2_4[cent]->SetLineColor(4);
    charged_v2_4[cent]->SetMinimum(0.);
    charged_v2_4[cent]->SetMaximum(v2max);
    charged_v2_4[cent]->GetXaxis()->SetTitle("P_{t} (GeV/c)");
    charged_v2_4[cent]->GetYaxis()->SetTitle("v_{2}{4}");
    charged_v2_4[cent]->SetTitle("Elliptic Flow");
    charged_v2_4[cent]->Write(graphName.Data());
    
    if(ptGraphs) charged_v2_2[cent]->Draw("AP");
    legend->AddEntry(charged_v2_2[cent],"charged hadron v_{2}{2}","p");
    if(ptGraphs) charged_v2_4[cent]->Draw("P");
    legend->AddEntry(charged_v2_4[cent],"charged hadron v_{2}{4}","p");    
    if(ptGraphs) legend->Draw("same");
    
    //can[cent]->Print(".pdf");

  }

  //Event_counter histograms

  TH1* Event_counter = dynamic_cast<TH1*>(myfile->Get("Event_counter"));
  if (!Event_counter) {
    cout << "### Can't find hist Event_counter" << endl;
  } else {
    // make the graph page
    gStyle->SetOptStat("e");
    TString* canEvtName = new TString("Event_counter"); 
    cout << "plot " << canEvtName->Data() << endl;
    TCanvas* canEvt = new TCanvas(canEvtName->Data(), canEvtName->Data(), 780, 600);
    Event_counter->SetMinimum(0.);
    Event_counter->GetXaxis()->SetTitle("centrality bin");
    Event_counter->Draw();
    Event_counter->Write(canEvtName->Data());    
  }
  TH1* Event_counterWeighted = dynamic_cast<TH1*>(myfile->Get("Event_counterWeighted"));
  if (!Event_counterWeighted) {
    cout << "### Can't find hist Event_counterWeighted" << endl;
  } else {
    // make the graph page
    //gStyle->SetOptStat("e");
    TString* canEvtWgtName = new TString("Event_counterWeighted"); 
    cout << "plot " << canEvtWgtName->Data() << endl;
    TCanvas* canEvtWgt = new TCanvas(canEvtWgtName->Data(), canEvtWgtName->Data(), 780, 600);
    Event_counterWeighted->SetMinimum(0.);
    Event_counterWeighted->GetXaxis()->SetTitle("centrality bin");
    Event_counterWeighted->Draw("hist");
    Event_counterWeighted->Write(canEvtWgtName->Data());    
  }
  //gStyle->SetOptStat(0);

  // v2 integrated
  
  float v2Intmax = 8.;

  // make the graph page
  TString* canIntName = new TString("v2Int"); 
  cout << "plot " << canIntName->Data() << endl;
  TCanvas* canInt = new TCanvas(canIntName->Data(), canIntName->Data(), 600, 780);
  TLegend* legendInt = new TLegend(0.35,0.1,0.6,0.35);
  legendInt->SetFillColor(kWhite);

  // make a histogram
  TString* histCenName = new TString("v2Int");
  TH1F* histCen = new TH1F(histCenName->Data(), histCenName->Data(), 80, 0., 80.);
  histCen->SetLineColor(kBlack);
  histCen->Draw();

  TGraphErrors *grv2IntPt_2 = new TGraphErrors(CENTBINS);
  TGraphErrors *grv2IntPt_4 = new TGraphErrors(CENTBINS);
  TGraphErrors *grv2IntCu_2 = new TGraphErrors(CENTBINS);
  TGraphErrors *grv2IntCu_4 = new TGraphErrors(CENTBINS);

  int m = 0;
  for(int cent=0; cent<CENTBINS; cent++){
    grv2IntPt_2->SetPoint(m, x[CENTBINS-cent-1], v2IntPt_2[0][cent+1]*100.);
    grv2IntPt_2->SetPointError(m, 0., v2IntPt_2_e[0][cent+1]*100.);
    grv2IntPt_4->SetPoint(m, x[CENTBINS-cent-1], v2IntPt_4[0][cent+1]*100.);
    grv2IntPt_4->SetPointError(m, 0., v2IntPt_4_e[0][cent+1]*100.);
    grv2IntCu_2->SetPoint(m, x[CENTBINS-cent-1], v2IntCu_2[cent]*100.);
    grv2IntCu_2->SetPointError(m, 0., v2IntCu_2Err[cent]*100.);
    grv2IntCu_4->SetPoint(m, x[CENTBINS-cent-1], v2IntCu_4[cent]*100.);
    grv2IntCu_4->SetPointError(m, 0., v2IntCu_4Err[cent]*100.);

    m++;
  }//cent

  grv2IntPt_2->SetMarkerStyle(kFullCircle);
  grv2IntPt_2->SetMarkerColor(kRed);
  grv2IntPt_2->SetLineColor(kRed);
  grv2IntPt_2->SetMinimum(0.);
  grv2IntPt_2->SetMaximum(v2Intmax);
  grv2IntPt_2->SetTitle("Elliptic Flow");
  grv2IntPt_2->Draw("AP");
  legendInt->AddEntry(grv2IntPt_2,"v_{2}{2}(pt)","p");

  grv2IntPt_4->SetMarkerStyle(kFullSquare);
  grv2IntPt_4->SetMarkerColor(kBlue);
  grv2IntPt_4->SetLineColor(kBlue);
  grv2IntPt_4->Draw("P");
  legendInt->AddEntry(grv2IntPt_4,"v_{2}{4}(pt)","p");

  grv2IntCu_2->SetMarkerStyle(kOpenCircle);
  grv2IntCu_2->SetMarkerColor(kRed);
  grv2IntCu_2->SetLineColor(kRed);
  grv2IntCu_2->Draw("P");
  legendInt->AddEntry(grv2IntCu_2,"v_{2}{2}(cu)","p");

  grv2IntCu_4->SetMarkerStyle(kOpenSquare);
  grv2IntCu_4->SetMarkerColor(kBlue);
  grv2IntCu_4->SetLineColor(kBlue);
  grv2IntCu_4->Draw("P");
  legendInt->AddEntry(grv2IntCu_4,"v_{2}{4}(cu)","p");
    
  legendInt->Draw("same");
  // output
  grv2IntPt_2->Write("v22IntPt");
  grv2IntPt_4->Write("v24IntPt");
  grv2IntCu_2->Write("v22IntCu");
  grv2IntCu_4->Write("v24IntCu");
    
  //can[cent]->Print(".pdf");
      
  // label the axes
  TLatex l;
  l.SetNDC();
  l.SetTextSize(0.06);
  l.DrawLatex(0.5,0.02,"% Most Central");
  l.SetTextAngle(90);
  l.DrawLatex(0.05,0.7,"v_{2} (%)" );

  graphFile.Close();
  
}
