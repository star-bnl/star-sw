#include "StMuFcsAnaCheckTrig.h"

ClassImp(StMuFcsAnaCheckTrig)

StMuFcsAnaCheckTrig::StMuFcsAnaCheckTrig()
{
}

StMuFcsAnaCheckTrig::~StMuFcsAnaCheckTrig()
{
}

UInt_t StMuFcsAnaCheckTrig::LoadHists(TFile* file, HistManager* histman, StMuFcsAnaData* anadata)
{
  UInt_t loaded = 0;
  if( histman==0 ){ return loaded; }
  //Below trigger histogram copied from StMuFcsRun22QaMaker
  //@[June 3, 2024] > This is almost all of them as some ids are not in this range but good enough for now
  //@[September 6, 2024] > Learned that for Run 22 only FCS triggers above 890000 are production triggers and can ignore triggers lower than that. Loooking at STAR's RunLog Browser I don't see any triggers larger than 892000. For this reason trigger histogram is only showing these Ids.
  //@[September 13, 2024] > There are a total number of 64 triggers in Run 22. Add an extra bin for any not found
  //@[September 16, 2024] > Note that if loading this histogram from a file the bin names should already be set so there is no need to set them again
  loaded += histman->AddH1F(file,mH1F_MatchFcsTriggers,"H1F_MatchFcsTriggers","Matched FCS Triggers;;",65,0,65);
  int trigsize = anadata->sizeOfFcsTriggers();
  //std::cout << "|trigsize:"<<trigsize << std::endl;
  if( trigsize==64 ){ //Check to make sure all triggers are in the map and it matches the bin size
    for( int i=0; i<trigsize; ++i ){
      //std::cout << "|itrig:"<<i << "|trigname:"<< mFcsTrigMap->triggerName(i) << std::endl;
      mH1F_MatchFcsTriggers->GetXaxis()->SetBinLabel(i+1,anadata->fcsTriggerName(i)); //Bin numbers are offset by 1
    }
  }
  mH1F_MatchFcsTriggers->GetXaxis()->SetBinLabel(65,"NF");  //Last bin is named "NF" for not found which is what nameFromId() returns if trigger is not in the map. This way if no map was loaded but an StFcsRun22TriggerMap was found, searching for triggers will return "NF"
  return loaded;
}

Int_t StMuFcsAnaCheckTrig::DoMake(StMuFcsAnaData* anadata)
{
  //Local copy of needed variables to make things easier
  StMuEvent* MuEvent = anadata->muEvent();

  //Filter with Trigger Information first
  StMuTriggerIdCollection* TrigMuColl = &(MuEvent->triggerIdCollection());
  if( TrigMuColl ){ 
    const StTriggerId& trgIDs = TrigMuColl->nominal();
    Int_t ntrig = trgIDs.triggerIds().size();
    bool hasfcstrigs = (anadata->sizeOfFcsTriggers()>0);
    for( Int_t i=0; i<ntrig; ++i ){
      unsigned int trig = trgIDs.triggerId(i);
      anadata->mTriggers[i] = trig;
      if( hasfcstrigs ){
	std::string thistrig = anadata->fcsTrigNameFromId(trig,MuEvent->runNumber());
	//if( !(anadata->ignoreTrig()) ){
	for( unsigned int j=0; j<anadata->mTargetTrig.size(); ++j ){
	  if( (anadata->mTargetTrig.at(j))==thistrig ){
	    (anadata->mNTrig)++;
	    anadata->mValidTrigFound=true;
	    mH1F_MatchFcsTriggers->Fill( thistrig.c_str(), 1);
	  }
	}
	//}
	if( thistrig=="fcsEM0" || thistrig=="fcsEM1" || thistrig=="fcsEM2" || thistrig=="fcsEM3"
	    || thistrig=="fcsEM0_tpc" || thistrig=="fcsEM1_tpc" || thistrig=="fcsEM2_tpc" || thistrig=="fcsEM3_tpc" )
	  {
	    if( thistrig=="fcsEM0" || thistrig=="fcsEM0_tpc" ){ anadata->mTrigEm0 = 1; }
	    if( thistrig=="fcsEM1" || thistrig=="fcsEM1_tpc" ){ anadata->mTrigEm1 = 2; }
	    if( thistrig=="fcsEM2" || thistrig=="fcsEM2_tpc" ){ anadata->mTrigEm2 = 3; }
	    if( thistrig=="fcsEM3" || thistrig=="fcsEM3_tpc" ){ anadata->mTrigEm3 = 4; }
	    anadata->mEmTrigFound = true;
	  }
      }
      else{ anadata->mNTrig = ntrig; }
    }
    /* Debugging why some events were skipped. The events that were skipped were fcsHad triggered events because I didn't turn them on in the runMuDst.C macro
    if( !(anadata->mValidTrigFound) ){
      //anadata->mNTrig=0; std::cout << "No ValidTrigFound" << std::endl;
      for( Int_t i=0; i<ntrig; ++i ){
	unsigned int trig = trgIDs.triggerId(i);
	std::cout << "|trig:"<<trig << std::endl;
      }
    }
    */
  }
  else{
    LOG_WARN <<"StMuFcsAnaCheckTrig::DoMake() - !TrigMuColl" <<endl;
    anadata->mValidTrigFound=false;
  }

  if( anadata->ignoreTrig() ){
    //If ignoring triggers then set mValidTrigFound to true so event is not skipped
    anadata->mValidTrigFound = true;
  }

  return kStOk;
}

void StMuFcsAnaCheckTrig::PaintMatchTriggers(TCanvas* canvas, const char* savename) const
{
  canvas->Clear();
  mH1F_MatchFcsTriggers->Draw("hist e");
  canvas->Print(savename);
}

			  
