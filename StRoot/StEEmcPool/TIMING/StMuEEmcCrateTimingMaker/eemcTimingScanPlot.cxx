#include "eemcTimingScanPlot.h"

#include "TH2.h"
#include "TAxis.h"
#include "TROOT.h"
#include "TKey.h"
#include "TIterator.h"
#include "TFile.h"
#include "TTree.h"
#include "TGraph.h"
#include "TMultiGraph.h"
#include "TSystem.h"
#include "TF1.h"
#include "TCanvas.h"
#include "TStyle.h"
#include "TMath.h"

#include <iostream>
#include <fstream>
#include <iomanip>
#include <map>
#include <set>
#include <string>
#include "StEEmcUtil/EEfeeRaw/EEdims.h"

using namespace std;

// ----------------------------------------------------------------------------
eemcTimingScanPlot::eemcTimingScanPlot() {
  mAxisMin=0.;
  mAxisMax=110.;
}

// ----------------------------------------------------------------------------
Int_t eemcTimingScanPlot::scan(TString directory) {

  /// Graphs for each box
  TGraph *graphs[MaxMapmtCrates][12];
  TString tmpstr,runnumberstring,tmpstr2;
  const Int_t MxMapmtFeeCh= (MaxMapmtCrates* MaxMapmtCrateCh / 16) + 1;
  Float_t channelIntegrals[MxMapmtFeeCh];
  Float_t tmpfloat;
  TString flavor;
  std::vector<std::vector<std::vector<Float_t> > > allRunsVector;  //all runs
  std::vector<std::vector<Float_t> > channelIntegralVector; //all channels
  std::vector<Float_t> tmpvector;  //channels for one crate
  std::vector<Float_t> timingDelay;
  if (!directory) return 0;
  TString dirEntry;
  void *dir = NULL;

  /// Open directory
  if ((dir = gSystem->OpenDirectory(directory.Data())) != NULL) {

    ///
    /// Loop over all runs within the directory
    ///
    const Char_t *dirEntryc;
    while ((dirEntryc = gSystem->GetDirEntry(dir)) != NULL) {
      
      channelIntegralVector.clear();
      dirEntry = dirEntryc;
      /// Process only root files
      if(!dirEntry.Contains(".root")) continue;
      tmpstr2 = directory + "/" + dirEntry;
      /// OPen the file
      TFile *tf = new TFile(tmpstr2.Data());
      /// Process only if open was successful
      if (tf && tf->IsOpen()) {

	/// Determine if we're looking at an MAPMT or TOWER run
        flavor = dirEntry(4,5);

	std::cout << "FLAVOR=" << flavor << std::endl;
	
	/// Access TTree to obtain channel integral and 
	/// timing information
        TTree* ttree = dynamic_cast<TTree*>(tf->Get("ints"));
        ttree->SetBranchAddress("chanint",channelIntegrals);
        ttree->SetBranchAddress("delay",&tmpfloat);
        ttree->GetEvent(0); // only the one event here
	printf("%s %f %f --------\n",flavor.Data(),channelIntegrals,&tmpfloat);

	/// Loop over all MAPMT crates
        for(int i=0; i<MaxMapmtCrates; i++) {
          tmpvector.clear();
          for(int j=0; j<12; j++) {
            tmpvector.push_back(channelIntegrals[i*12 + j]);
          }
          channelIntegralVector.push_back(tmpvector);
        }
//      tmpstr = direntry(25,direntry.Length()-29);
//      sscanf(tmpstr.Data(),"%f",&tmpfloat);
        timingDelay.push_back(tmpfloat);
      }
      delete tf;
      allRunsVector.push_back(channelIntegralVector);
    }
  }



  ///
  /// 
  ///
  Int_t tmpint, sector;  
  for(int icr=0;icr<MaxMapmtCrates;icr++) {


    /*
    //TString tit="sec";
    TString tit="Box";
    sector = icr/4 - 1;
    //if(sector == -1) sector = 12;
    //tit+=sector;
    if(sector == -1) sector = 11;
    tit+=(sector+1);
    tmpint = icr%4 + 1;
    if(tmpint < 4) {
      tit+="S";
      tit+=tmpint;
    } else {
      tit+="P1";
    }
    */

    TString tit = ""; // eh heh, hey beavis!
    sector = -1;
    
    if ( flavor.Contains("tower") ) {
      tit = "Crate";
      tit += icr+1; /// >>>>>>>> DAVE, IS THIS RIGHT ???????  <<<<<<<<<<<<<<
    }
    else {
      tit = "Box";
      sector=icr/4-1; if ( sector==-1 ) sector = 11;
      tit += sector+1;
      tmpint = icr%4 + 1;
      if ( tmpint<4 ) { tit+="S"; tit += tmpint; }
      else tit += "P1";
    }
    


    /// For each MAPMT box, we have 12 channels which
    /// we've measured integrals.  Construct a new 
    /// TGraph for each of them.
    for(int ich=0;ich<12;ich++) {
      TGraph* gr=new TGraph(timingDelay.size());
      gr->SetMarkerStyle(20+ich%7);
      int icol=1+ich%5;
      gr->SetMarkerColor(icol);  
      gr->SetLineColor(icol);  
      if(ich==0)gr->SetName(tit);
      if(ich==0)gr->SetTitle(tit+"; delay(ns)");
      graphs[icr][ich]=gr;
    }
  }


  /// Fill the TGraphs
  Int_t numberofPoints = timingDelay.size();
  Float_t maxima[MaxMapmtCrates];
  for(int icr=0;icr<MaxMapmtCrates;icr++)
    maxima[icr] = 0;

  for(Int_t i=0; i<numberofPoints; i++) {
    for(int icr=0;icr<MaxMapmtCrates;icr++) {
      for(int ich=0; ich<12; ich++) {
        graphs[icr][ich]->SetPoint(i,timingDelay[i],allRunsVector[i][icr][ich]);
        if(allRunsVector[i][icr][ich] > maxima[icr]) maxima[icr] = allRunsVector[i][icr][ich];
      }
    }
  }

  /// Loop over all graphs and sort them
  for ( Int_t i = 0; i < MaxMapmtCrates; i++ ) {
    for ( Int_t j = 0; j < 12; j++ ) {
      graphs[i][j]->Sort();
    }
  }


  /// -------------------------------------------------------------------------
  ///
  /// Create .ps and .gif files for display
  ///


  /// Towers will only be on one page, and the bottom half of it
  /// to be exact...  Also only interested in the first 8 channels
  /// in each crate
  Int_t maxpg= (flavor.Contains("tower"))?1:4;
  Int_t maxch= (flavor.Contains("tower"))?8:12;
  for(int i=0; i<maxpg; i++) {

    ///
    /// Print every set of MAPMT boxen on a different 
    /// canvas
    ///
    TString cname="canvas"; cname += i;  
    TCanvas* c = new TCanvas(cname,"Crate channel plots",0,0,800,600);
    TPad* smallpad[12];
    for(int pp=0; pp<4; pp++) {
      for(int qq=0; qq<3; qq++) {
	c->cd();
	tmpstr = "graph";
	tmpstr += pp*3+qq;
	smallpad[pp*3+qq] = new TPad(tmpstr.Data(),tmpstr.Data(), 0.33*qq, 0.25*pp, 0.33*(qq+1), 0.25*(pp+1));
	smallpad[pp*3+qq]->Draw();
      }
    }
  

    ///
    /// 12 boxes on each page, 4 pages, 48 total boxes
    ///
    for(int icpp=0; icpp<12 ; icpp++) {
      smallpad[icpp]->cd();
      smallpad[icpp]->Clear();
      TMultiGraph* tmg = new TMultiGraph(graphs[i*12 + icpp][0]->GetName(),graphs[i*12 + icpp][0]->GetTitle());
      for(int pcc=0; pcc<maxch ; pcc++) { //channels per crate!	
        tmg->Add(graphs[i*12 + icpp][pcc]);
      }
      //tmg->SetMaximum(maxima[i*12 + icpp] + 0.1);
      tmg->SetMaximum(maxima[i*12 + icpp]+0.02);
      //tmg->SetMinimum(-0.1);
      tmg->SetMinimum(-0.02);
      // graphs[i*12 + icpp][pcc]->SetMaximum(0.15);      
      TH1F *htmp = new TH1F(graphs[i*12 + icpp][0]->GetName(),graphs[i*12 + icpp][0]->GetTitle(),110,mAxisMin,mAxisMax);
      htmp->SetMaximum(maxima[i*12 + icpp]+0.02);
      htmp->SetMinimum(-0.02);
      htmp->Draw();
      tmg->Draw("LP");
    }

    tmpstr="CrateTimingScanPage";
    tmpstr += i;
    tmpstr+=".gif";
    c->Modified();
    c->Update();
    c->Print(tmpstr);
    tmpstr.ReplaceAll("gif","ps");
    c->Print(tmpstr);
  }

  return 0;
}


ClassImp(eemcTimingScanPlot)
