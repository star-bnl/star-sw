/**********************************************************************
 *
 * $Id: StEStructEmptyAnalysis.cxx,v 1.2 2004/06/25 03:10:28 porter Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  Empty analysis code for testing
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

  mhm[2]=mhm[3]=0;
  mhm[4]=mhm[5]=1;
  mhm[6]=mhm[7]=2;
  mhm[8]=mhm[9]=3;
  mhm[10]=mhm[11]=mhm[13]=4;
 

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



};

bool StEStructEmptyAnalysis::doEvent(StEStructEvent* event){


  if(!event) return false;
  //  cout<<" in doEvent "<<endl;

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
	npart[i]+=1.0;
        etaSum[i]+=(*Iter)->Eta();
        phiSum[i]+=(*Iter)->Phi();
        ytSum[i]+=(*Iter)->Yt();
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

  for(int i=0;i<3;i++){
    for(int j=0;j<6;j++){
      etaMean[i][j]->Write();
      phiMean[i][j]->Write();
      ytMean[i][j]->Write();
    }
  }
  tf->Close();
}

/**********************************************************************
 *
 * $Log: StEStructEmptyAnalysis.cxx,v $
 * Revision 1.2  2004/06/25 03:10:28  porter
 * added a new common statistics output and added electron cut with momentum slices
 *
 * Revision 1.1  2003/10/15 18:20:32  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/
