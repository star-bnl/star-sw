// $Id: St2009WlumiMaker.cxx,v 1.1 2009/11/23 23:00:18 balewski Exp $
//
//*-- Author : Ross Corliss, MIT

#include "St2009WMaker.h"

#include "St2009WlumiMaker.h"

ClassImp(St2009WlumiMaker)

//_____________________________________________________________________________
//
St2009WlumiMaker::St2009WlumiMaker(const char *name):StMaker(name){
  wMK=0;muMK=0;HList=0;

}


//_____________________________________________________________________________
//
Int_t St2009WlumiMaker::Init(){
  assert(wMK);
  assert(muMK);
  assert(HList);
  initHistos();
  return StMaker::Init();
}


//_____________________________________________________________________________
//
Int_t St2009WlumiMaker::InitRun  (int runumber){
  towerInfoIsCurrent=false; // make sure we check the tower info on the first event.
  nActiveTowers=0;
  nBHT3=0;
  for (int i=0;i<120;i++)
    nBx[i]=0;

  return 0;
}

//_____________________________________________________________________________
//
Int_t St2009WlumiMaker::FinishRun  (int runnumber){

  printf("Finishing Run %d (lumi)\n",runnumber);

  float activeFraction=nActiveTowers*1.0/4800.0; //portion of the detector that was working.
  float effective_lumi;//pb^-1, effective integrated luminosity, given not all the detector is necessarily working.
  float total_lumi;//pb^-1, total integrated luminosity if the whole detector were working.
  int BHT3prescale=50;//recorded 1/n events:
  int nBHT3triggers;//number of non-background BHT3 events roughly (nBHT3*BHT3prescale);
  float BHT3xs=520000;//pb . 520nb.
  int nAbortGap1, nAbortGap2; //number of counts in the first and second abort gap.

  //try all possible alignments of the abort gaps to find the right relative alignment,
  //then count the events in those two gaps.
  getAbortGapCounts(&nAbortGap1,&nAbortGap2);


  nBHT3triggers=(nBHT3-nAbortGap1*120/11-nAbortGap2*120/9)*BHT3prescale;

  effective_lumi=nBHT3triggers*1.0/BHT3xs;
  //this value doesn't have any factors of activeFraction, because they cancel out:
  //ntotaltriggers=nBHT3triggers/activeFraction
  //total_lumi=ntotaltriggers/bht3xs
  //effective_lumi=total_lumi*activeFraction

  total_lumi=effective_lumi/activeFraction;

  hA[16]->Fill(runnumber,total_lumi);
  hA[17]->Fill(runnumber,activeFraction);
  hA[18]->Fill(runnumber,nAbortGap1*120/11+nAbortGap2*120/9);
  printf("eff_lumi=%f, active=%2.2f, nTowers=%d\n",effective_lumi,activeFraction,nActiveTowers);
  TH1F* temp;
  temp=(TH1F*)(HList->FindObject("muWET"));
  hA[5]->Add(temp,1.0/activeFraction);//yield scaled by the active fraction


  //temp=HList("");
  //repeat for other histos.

return 0;
}

//_____________________________________________________________________________
//
Int_t 
St2009WlumiMaker::Make(){
  //  printf("in %s\n", GetName());
  //hA[0]->Fill("",1.);

  //if we haven't done it yet, calculate the working fraction of the bemc
  if (!towerInfoIsCurrent) getActiveTowers();

  //fill various histograms and arrays with event data
  sortTrigger();

  return kStOK;
}

//_____________________________________________________________________________
//
void 
St2009WlumiMaker::getActiveTowers(){
  //count the number of good towers:
  nActiveTowers=0;
  WeveBEMC *barrel=&(wMK->wEve.bemc);
  for (int i=0;i<4800;i++)
    if (barrel->statTile[0][i]==0)//[0]=kBtow
      nActiveTowers++;
  
  //count good trigger patches?
  //??
  
  towerInfoIsCurrent=true;
}

//_____________________________________________________________________________
//
void 
St2009WlumiMaker::sortTrigger(){
  //printf("sortTrigger()\n");
  //has access to whole W-algo-maker data via pointer 'wMK'
  Wevent2009 *weve=&(wMK->wEve);

  if (weve->l2bitET) {
    hA[0]->Fill("L2W",1);
    hA[0]->Fill("L2Wnormal",1);
  }
  if (weve->l2bitRnd) {
    hA[0]->Fill("L2W",1);
    hA[0]->Fill("L2Wrandom",1);
    nBHT3++;

    nBx[weve->bx7]++;
  }



  return;
}

//_____________________________________________________________________________
//
void 
St2009WlumiMaker::getAbortGapCounts(int *n1, int* n2){
  int separation=80;//the number of bins between the start of the first abort gap and the start of the second
  int width1=11;
  int width2=9;
  int counts1, counts2;
  *n1=nBHT3;
  *n2=nBHT3;
  int total=nBHT3;
  for (int offset=0;offset<120;offset++){
    counts1=0;
    counts2=0;
    for (int i=offset;i<offset+width1;i++)
      counts1+=nBx[i%120];
    for (int i=offset+separation;i<offset+separation+width2;i++)
      counts2+=nBx[i%120];
    if (total>counts1+counts2){
      total=counts1+counts2;
      *n1=counts1;
      *n2=counts2;
    }
  }
  return;
}

// $Log: St2009WlumiMaker.cxx,v $
// Revision 1.1  2009/11/23 23:00:18  balewski
// code moved spin-pool
//
// Revision 1.1  2009/11/23 21:11:18  balewski
// start
//
