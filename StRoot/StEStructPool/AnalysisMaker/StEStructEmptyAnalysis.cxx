/**********************************************************************
 *
 * $Id: StEStructEmptyAnalysis.cxx,v 1.6 2005/10/14 13:51:05 msd Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  Empty analysis code for testing
 *               Can replace StEStruct2ptCorrelations for an analysis that
 *               reads events, applies all event and track (NOT pair) cuts,
 *               and outputs a few histograms.  
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

  const char* ctype[]={"Pos","Neg","All"};

  mhm[0]=mhm[1]=0;
  mhm[2]=mhm[3]=0;
  mhm[4]=mhm[5]=1;
  mhm[6]=mhm[7]=2;
  mhm[8]=mhm[9]=3;
  mhm[10]=mhm[11]=4;
  mhm[12]=mhm[13]=4;
 
  for(int i=0;i<3;i++){

    etaMean[i]=new TH1F*[6];
    phiMean[i]=new TH1F*[6];
    ytMean[i]=new TH1F*[6];

    for(int j=0;j<6;j++){
      TString ename("EtaMean");
      ename+=ctype[i]; ename+=j;
      etaMean[i][j]=new TH1F(ename.Data(),ename.Data(),120,-1.2,1.2);
 
      TString pname("PhiMean");
      pname+=ctype[i]; pname+=j;
      phiMean[i][j]=new TH1F(pname.Data(),pname.Data(),120,-3.16,3.16);

      TString yname("YtMean");
      yname+=ctype[i]; yname+=j;
      ytMean[i][j]=new TH1F(yname.Data(),yname.Data(),120,1.0,5.0);    

    }

  }

  hNEvent = new TH1F("hNEvent","dNevent/dNch",1200,0,1200);

  float varxbins[1200];
  varxbins[0]=0;
  for(int i=1; i<1200; i++) varxbins[i]=pow(i,0.25)-0.00001;
  hvar = new TH1F("hvar","var bin",1199,varxbins);

};

bool StEStructEmptyAnalysis::doEvent(StEStructEvent* event){

  if(!event) return false;

  int mult = event->Ntrack();
  //cout << " doing event " << event->EventID() << " with mult " << mult << endl; //***
  hNEvent->Fill(mult);
  hvar->Fill(pow(mult,0.25));
  

  // Below is Jeff's p-p stuff; I'm tempted to just comment all this out...  
    int id=event->Ntrack();
  if(id>=1){

    if(id<=13){
      id=mhm[id];
    } else {
      id=5;
    }

    float npart[2],etaSum[2],phiSum[2],ytSum[2];
    StEStructTrackCollection* tcol;

    for(int i=0;i<2;i++){

      npart[i]=etaSum[i]=phiSum[i]=ytSum[i]=0.;

      if(i==0)tcol=event->TrackCollectionP();
      if(i==1)tcol=event->TrackCollectionM();
      StEStructTrackIterator Iter;
      for(Iter=tcol->begin(); Iter!=tcol->end();++Iter){
	if(*Iter) {
	  npart[i]+=1.0;
	  etaSum[i]+=(*Iter)->Eta();
	  phiSum[i]+=(*Iter)->Phi();
	  ytSum[i]+=(*Iter)->Yt();
	}
      }
      if(npart[i]>0.){
	etaMean[i][id]->Fill(etaSum[i]/npart[i]);
	phiMean[i][id]->Fill(phiSum[i]/npart[i]);
	ytMean[i][id]->Fill(ytSum[i]/npart[i]);
      }

    }

    if( (npart[0]+npart[1])>0.){
	etaMean[2][id]->Fill((etaSum[0]+etaSum[1])/(npart[0]+npart[1]));
	phiMean[2][id]->Fill((phiSum[0]+phiSum[1])/(npart[0]+npart[1]));
	ytMean[2][id]->Fill((ytSum[0]+ytSum[1])/(npart[0]+npart[1]));
    }

  }
  

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

  // Jeff's plots
  for(int i=0;i<3;i++){
    for(int j=0;j<6;j++){
      etaMean[i][j]->Write();
      phiMean[i][j]->Write();
      ytMean[i][j]->Write();
    }
  }
  

  // Centrality plots
  hNEvent->Write();
  
  for(int i=1; i<=hvar->GetNbinsX(); i++)  hvar->SetBinContent(i, hvar->GetBinContent(i)*4*pow(hvar->GetBinLowEdge(i),3) );
  hvar->Write();

  tf->Close();

}

/**********************************************************************
 *
 * $Log: StEStructEmptyAnalysis.cxx,v $
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
