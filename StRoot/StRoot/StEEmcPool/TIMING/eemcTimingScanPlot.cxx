#include "eemcTimingScanPlot.h"

#include "TH2.h"
#include "TAxis.h"
#include "TROOT.h"
#include "TKey.h"
#include "TIterator.h"
#include "TFile.h"
#include "TTree.h"
#include "TGraph.h"
#include "TGraphErrors.h"
#include "TMultiGraph.h"
#include "TSystem.h"
#include "TF1.h"
#include "TCanvas.h"
#include "TStyle.h"
#include "TMath.h"
#include "TLegend.h"

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
  mNormalize=false;
  mNormalizePreshower=false;
  mErrors=false;
  mLegend=false;
  mSuppressZeros=false;
}

// ----------------------------------------------------------------------------
Int_t eemcTimingScanPlot::scan(TString directory) {

  /// Graphs for each box
  TGraph *graphs[MaxMapmtCrates][12];
  TString tmpstr,runnumberstring,tmpstr2;
  const Int_t MxMapmtFeeCh= (MaxMapmtCrates* MaxMapmtCrateCh / 16) + 1;


  Int_t myCrateIds[MaxMapmtCrates][12];
  Int_t myChannelIds[MaxMapmtCrates][12];

  /// Integrals and errorbars (from TTree)
  Float_t channelIntegrals[MxMapmtFeeCh];
  Float_t channelErrors[MxMapmtFeeCh];
  Int_t   channelIds[MxMapmtFeeCh];
  Int_t   crateIds[MxMapmtFeeCh];
  Float_t tmpfloat;

  /// Whether we are dealing with MAPMT's or tower crates
  TString flavor;

  /// Vectors (multidimensional) for storing the integrals
  /// before plotting them
  std::vector<std::vector<std::vector<Float_t> > > allRunsVector;  //all runs
  std::vector<std::vector<Float_t> > channelIntegralVector; //all channels
  std::vector<Float_t> tmpvector;  //channels for one crate
  std::vector<Float_t> timingDelay;

  std::vector<std::vector<std::vector<Int_t> > > allRunsCrateIds;
  std::vector<std::vector<std::vector<Int_t> > > allRunsChannelIds;
  std::vector<std::vector<Int_t> > crateIdsVector;
  std::vector<std::vector<Int_t> > channelIdsVector;
  std::vector<Int_t> tmpCrateIds;
  std::vector<Int_t> tmpChannelIds;
  

  /// Abort if user didn't specify a directory
  if (!directory) return 0;
  TString dirEntry;
  void *dir = NULL;

  /// Open directory
  if ((dir = gSystem->OpenDirectory(directory.Data())) != NULL) {

   
    /// Loop over all files within the directory and
    /// select the root files (important that the directory
    /// not contain any other root files than what we 
    /// are looking at).
    const Char_t *dirEntryc;
    while ((dirEntryc = gSystem->GetDirEntry(dir)) != NULL) {
      
      /// Zero out the entries in the integral vector
      channelIntegralVector.clear();
      crateIdsVector.clear();
      channelIdsVector.clear();
      dirEntry = dirEntryc;

      /// Process only root files
      if(!dirEntry.Contains(".root")) continue;
      tmpstr2 = directory + "/" + dirEntry;

      /// Open the root file
      TFile *tf = new TFile(tmpstr2.Data());

      /// Process only if open was successful
      if (tf && tf->IsOpen()) {

	/// Determine if we're looking at an MAPMT or TOWER run
        flavor = dirEntry(4,5);
	
	/// Access TTree to obtain channel integral, errors
	/// and timing delay
        TTree* ttree = dynamic_cast<TTree*>(tf->Get("ints"));
        ttree->SetBranchAddress("chanint",channelIntegrals);
	ttree->SetBranchAddress("chanerr",channelErrors);
        ttree->SetBranchAddress("delay",&tmpfloat);
	ttree->SetBranchAddress("crateIds",crateIds);      // crate and chan IDs corresponding 
	ttree->SetBranchAddress("channelIds",channelIds);  // to the specified integral
        ttree->GetEvent(0); // only the one event here
	//printf("%s %f %f --------\n",flavor.Data(),channelIntegrals,&tmpfloat);


	/// Loop over all MAPMT boxes.  
        for(int i=0; i<MaxMapmtCrates; i++) {

	  /// Clear temporary vector
          tmpvector.clear();
	  tmpCrateIds.clear();
	  tmpChannelIds.clear();

	  /// Loop over all 12 channels being plotted for
	  /// this box, and add then into consecutive entries
	  /// in the vector.  
          for(int j=0; j<12; j++) {
            tmpvector.push_back(channelIntegrals[i*12 + j]);
	    tmpCrateIds.push_back(crateIds[i*12 + j]);
	    tmpChannelIds.push_back(channelIds[i*12 + j]);
          }
          channelIntegralVector.push_back(tmpvector);
	  crateIdsVector.push_back(tmpCrateIds);
	  channelIdsVector.push_back(tmpChannelIds);
        }
//      tmpstr = direntry(25,direntry.Length()-29);
//      sscanf(tmpstr.Data(),"%f",&tmpfloat);
        timingDelay.push_back(tmpfloat);
      }

      /// Delete/close the root file
      delete tf;

      /// Add the channelIntegralVector to the vector for
      /// all runs/timing delays
      allRunsVector.push_back(channelIntegralVector);
      allRunsCrateIds.push_back(crateIdsVector);
      allRunsChannelIds.push_back(channelIdsVector);

    }

  }



  /// Loop over all crates
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
      graphs[icr][ich]=(TGraph*)gr;
    }


  }//crates


  /// Fill the TGraphs
  Int_t numberofPoints = timingDelay.size();
  Float_t maxima[MaxMapmtCrates];
  Float_t maxCrateChan[MaxMapmtCrates][12];
  for(int icr=0;icr<MaxMapmtCrates;icr++) {
    maxima[icr] = 0;
    for ( Int_t jch=0; jch<12; jch++ ) maxCrateChan[icr][jch]=0.;
  }

  /// Find largest in crate and largest in crate for
  /// specified channel.
  for(Int_t i=0; i<numberofPoints; i++) {    
    /// All crates or boxes
    for(int icr=0;icr<MaxMapmtCrates;icr++) {
      /// Loop over 12 selected channels
      for(int ich=0; ich<12; ich++) {
	  
	/// Determine the maximum channel for this crate
	if (allRunsVector[i][icr][ich] > maxima[icr]) {
	  maxima[icr] = allRunsVector[i][icr][ich];
	  }
	if (allRunsVector[i][icr][ich] > maxCrateChan[icr][ich] ){
	  maxCrateChan[icr][ich] = allRunsVector[i][icr][ich];
	}

      }
    }
  }
  


  
  /// Loop over all points
  for(Int_t i=0; i<numberofPoints; i++) {    
    /// All crates or boxes
    for(int icr=0;icr<MaxMapmtCrates;icr++) {
      /// Loop over 12 selected channels
      for(int ich=0; ich<12; ich++) {
 	/// Set the specified point for this crate and channel,
	/// rescaled to a common maximum for the specified channel
	Float_t scale = 1.;
	if ( mNormalize && maxCrateChan[icr][ich] != 0. ) scale = maxima[icr]/maxCrateChan[icr][ich];
	TString myName=graphs[icr][ich]->GetName();
	if ( myName.Contains("P") && !mNormalizePreshower ) scale = 1.;
	if ( allRunsVector[i][icr][ich] > 0. || !mSuppressZeros )
	  graphs[icr][ich]->SetPoint(i,timingDelay[i],scale*allRunsVector[i][icr][ich]);
	myCrateIds[icr][ich] = allRunsCrateIds[i][icr][ich];
	myChannelIds[icr][ich] = allRunsChannelIds[i][icr][ich];      }

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


  ///
  /// Loop over all pages.  Plot 12 boxes per page.
  ///
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

      /// Create a new multigraph with the name of the 
      /// specified TGraph
      TMultiGraph* tmg = new TMultiGraph(graphs[i*12 + icpp][0]->GetName(),graphs[i*12 + icpp][0]->GetTitle());

      for(int pcc=0; pcc<maxch ; pcc++) { //channels per crate!	
        tmg->Add(graphs[i*12 + icpp][pcc]);
      }
      /// Set maxima for the TGraphs
      tmg->SetMaximum(maxima[i*12 + icpp] * 1.1);
      tmg->SetMinimum(0.0);

      Float_t minX=mAxisMin;
      Float_t maxX=(mLegend)?mAxisMax*1.25:mAxisMax;
      Float_t minY=0.;
      Float_t maxY=1.1*maxima[i*12 + icpp];

      /// Temp histogram to establish sensible X and Y limits
      TH1F *htmp = new TH1F(graphs[i*12 + icpp][0]->GetName(),graphs[i*12 + icpp][0]->GetTitle(),110,minX,maxX);
      htmp->SetMaximum(maxY);
      htmp->SetMinimum(minY);
      htmp->Draw();

      tmg->Draw("LP");

      /// Build a TLegend showing the crate/box ID and channel ID
      if ( !mLegend ) continue;
      TString bxstr = "crate "; bxstr += myCrateIds[i*12 + icpp][1];
      TLegend *legend=new TLegend( 0.77 * maxX, 0.10 * maxY, 0.98 * maxX, 0.98 * maxY, bxstr, "br" );
      legend->SetFillColor(33);
      for(int pcc=0; pcc<maxch ; pcc++) { //channels per crate!	
	TString chstr = bxstr; 
	chstr += ":";
	chstr += myChannelIds[i*12 + icpp][pcc];
	legend->AddEntry(graphs[i*12 + icpp][pcc],chstr,"P");
      }
      legend -> Draw();

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
