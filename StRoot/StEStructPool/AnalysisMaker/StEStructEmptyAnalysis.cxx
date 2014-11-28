/**********************************************************************
 *
 * $Id: StEStructEmptyAnalysis.cxx,v 1.9 2007/05/27 22:43:33 msd Exp $
 *
 * Author: Jeff Porter and Michael Daugherity 
 *
 **********************************************************************
 *
 * Description:  Empty analysis code for testing
 *               Can replace StEStruct2ptCorrelations for an analysis that
 *               reads events, applies all event and track (NOT pair) cuts,
 *               and outputs a few histograms.
 *               Makes Nch distributions for centrality analysis.  
 *
 **********************************************************************/
#include "StEStructEmptyAnalysis.h"
#include "StEStructPool/EventMaker/StEStructEvent.h"
#include "StEStructPool/EventMaker/StEStructTrack.h"
#include "Stiostream.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TFile.h"


ClassImp(StEStructEmptyAnalysis)

StEStructEmptyAnalysis::StEStructEmptyAnalysis(): moutFileName(0) {

  hNEvent = new TH1F("hNEvent","dNevent/dNch - Centrality",1200,0,1200);
  hnt = new TH1F("hnt","dNevent/dNch - Ntrack",1200,0,1200);
  href = new TH1F("href","refMult", 1000,0,1000);
  hnumPrim = new TH1F("hnumPrim", "Number of Good Primary Tracks", 2000, 0, 2000);
  hctb = new TH1D("hctb","CTB Multiplicity", 1500,0,15000);

  hnev14 = new TH1F("hnev14","centrality ^ 1/4",100,0,7);  // I guess round numbers don't matter here... 
  hnt14 = new TH1F("hnt14","Ntrack ^ 1/4",100,0,7); 
  href14 = new TH1F("href14","refMult ^ 1/4", 100,0,7);
  hnumPrim14 = new TH1F("hnumPrim14","Primaries ^ 1/4", 100,0,7);
  hctb14 = new TH1D("hctb14","CTB ^ 1/4", 200,0,15);
    
  hmeanpt = new TH1F("hmeanpt","mean pt",100,0,2);
  hmeanpt14 = new TH1F("hmeanpt14","mean pt ^ 1/4",1000,0,2);
  
  float varxbins[1200];
  varxbins[0]=0;
  for(int i=1; i<1200; i++) varxbins[i]=pow(i,0.25)-0.00001;
  hvar = new TH1F("hvar","var bin",1199,varxbins);

};

bool StEStructEmptyAnalysis::doEvent(StEStructEvent* event){

  if(!event) return false;

  int mult = (int)event->Centrality();
  //cout << " doing event " << event->EventID() << " with mult " << mult << endl; //***

  hNEvent->Fill(mult);
  hnt->Fill(event->Ntrack());
  href->Fill(event->RefMult());
  hnumPrim->Fill(event->NumPrim());
  hctb->Fill(event->ctbMult());


  hnev14->Fill(pow(mult,0.25));
  hvar->Fill(pow(mult,0.25));  
  hnt14->Fill(pow(event->Ntrack(),0.25));
  href14->Fill(pow(event->RefMult(),0.25));
  hnumPrim14->Fill(pow(event->NumPrim(),0.25));
  hctb14->Fill(pow(event->ctbMult(),0.25));


  double ptsum = 0;
  int count = 0;

  StEStructTrackCollection* tcol;
  for(int i=0;i<2;i++){

    if(i==0)tcol=event->TrackCollectionP();
    if(i==1)tcol=event->TrackCollectionM();
    StEStructTrackIterator Iter;
    for(Iter=tcol->begin(); Iter!=tcol->end();++Iter){
      if(*Iter) {
	count++;
	ptsum+=(*Iter)->Pt();
      }
    }
  }

  hmeanpt->Fill( ptsum/count );
  hmeanpt14->Fill( pow(ptsum/count,0.25) );    

  delete event;

  return true;
}

//------------------------------------------------------------------------
void StEStructEmptyAnalysis::finish(){

  if(!moutFileName){
    cout<<" NO OUTPUTFILE TO WRITE TO ..... giving up ...."<<endl;
    return;
  }

  TFile * tf=new TFile(moutFileName,"RECREATE");
  tf->cd();

  for(int i=1; i<=hvar->GetNbinsX(); i++)  hvar->SetBinContent(i, hvar->GetBinContent(i)*4*pow(hvar->GetBinLowEdge(i),3) );

  hNEvent->Write();
  hnt->Write();
  hvar->Write();
  href->Write();
  hnumPrim->Write();
  hctb->Write();

  hnev14->Write();
  hnt14->Write();
  href14->Write();
  hnumPrim14->Write();
  hctb14->Write();

  hmeanpt->Write();
  hmeanpt14->Write();

  tf->Close();

}

/**********************************************************************
 *
 * $Log: StEStructEmptyAnalysis.cxx,v $
 * Revision 1.9  2007/05/27 22:43:33  msd
 * Added new centrality plots to Empty analysis
 *
 * Revision 1.8  2007/01/26 17:09:27  msd
 * Minor bug fix in AnalysisMaker, cleaned up EmptyAnalysis
 *
 * Revision 1.7  2006/04/04 22:05:04  porter
 * a handful of changes:
 *  - changed the StEStructAnalysisMaker to contain 1 reader not a list of readers
 *  - added StEStructQAHists object to contain histograms that did exist in macros or elsewhere
 *  - made centrality event cut taken from StEStructCentrality singleton
 *  - put in  ability to get any max,min val from the cut class - one must call setRange in class
 *
 * Revision 1.6  2005/10/14 13:51:05  msd
 * Yet another fix of code I'm not using
 *
 * Revision 1.5  2005/10/10 16:22:28  msd
 * stability fixes, yet another tweak of output hists
 *
 * Revision 1.4  2005/10/04 16:06:15  msd
 * Finalized centrality plots
 *
 * Revision 1.3  2005/09/29 17:40:30  msd
 * Changed empty analysis to create plots for determining centrality bins
 *
 * Revision 1.2  2004/06/25 03:10:28  porter
 * added a new common statistics output and added electron cut with momentum slices
 *
 * Revision 1.1  2003/10/15 18:20:32  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/
