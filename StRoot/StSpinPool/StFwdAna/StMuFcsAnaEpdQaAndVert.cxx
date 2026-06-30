#include "StEnumerations.h"
#include "StEvent/StEvent.h"
#include "StEvent/StFcsCluster.h"
#include "StEvent/StFcsCollection.h"
#include "StEvent/StFcsHit.h"
#include "StEvent/StEventTypes.h"
#include "StFcsDbMaker/StFcsDbMaker.h"
#include "StMessMgr.h"
#include "StMuDSTMaker/COMMON/StMuTypes.hh"
#include "StSpinPool/StFcsRawDaqReader/StFcsRawDaqReader.h"
#include "StThreeVectorF.hh"
#include "Stypes.h"

#include "StMuFcsAnaEpdQaAndVert.h"

ClassImp(StMuFcsAnaEpdQaAndVert)


StMuFcsAnaEpdQaAndVert::StMuFcsAnaEpdQaAndVert()
{
  memset(mH2F_HitEpd_nmipVchkey,0,sizeof(mH2F_HitEpd_nmipVchkey));
  memset(mH2F_HitEpd_tacVadcmip,0,sizeof(mH2F_HitEpd_tacVadcmip));  
}

StMuFcsAnaEpdQaAndVert::~StMuFcsAnaEpdQaAndVert()
{
  delete mH2F_HitEpd_tacVadcmip[0];
  delete mH2F_HitEpd_tacVadcmip[1];
}

UInt_t StMuFcsAnaEpdQaAndVert::LoadHists(TFile* file, HistManager* histman, StMuFcsAnaData* anadata)
{
  if( histman==0 ){ return 0; }
  UInt_t loaded = 0;
  loaded += histman->AddH1F(file,mH1F_Epd_NHits,"H1F_Epd_NHits","Number of Hits from EPD collection;NHits",500,0,500);
  loaded += histman->AddH1F(file,mH1F_Epd_NHits_Cut,"H1F_Epd_NHits_Cut","Number of Hits from EPD collection with nMIP>0.7;NHits",500,0,500);
  loaded += histman->AddH1F(file,mH1F_Epd_NHitsWest,"H1F_Epd_NHitsWest","Number of Hits from EPD collection (West);NHits",300,0,300);
  loaded += histman->AddH1F(file,mH1F_Epd_NHitsWest_Cut,"H1F_Epd_NHitsWest_Cut","Number of Hits from EPD collection (West) with nMIP>0.7;NHits",300,0,300);

  if( mEpdTacAdcOn ){
    if( mH2F_HitEpd_tacVadcmip[0]==0 ){ mH2F_HitEpd_tacVadcmip[0] = new TObjArray(); }//Create new array for east side
    if( mH2F_HitEpd_tacVadcmip[1]==0 ){ mH2F_HitEpd_tacVadcmip[1] = new TObjArray(); }//Create new array for west side
    //EPD has 372 tiles on one side
    loaded += histman->AddH2FArr(file,mH2F_HitEpd_tacVadcmip[0],372,"H2F_HitEpd_tacVadcmip_E","Qt TAC vs. ADC/ADC_1mip;ADC/ADC_1mip;TAC", 50,0,25, 200,0,4000);
    loaded += histman->AddH2FArr(file,mH2F_HitEpd_tacVadcmip[1],372,"H2F_HitEpd_tacVadcmip_W","Qt TAC vs. ADC/ADC_1mip;ADC/ADC_1mip;TAC", 50,0,25, 200,0,4000 );
  }

  loaded += histman->AddH2F(file,mH2F_HitEpd_nmipVchkey[0],"H2F_HitEpd_nmipVchkey_E","NMIP values for East EPD channels by key;key ((pp-1)*31+(tt-1));nmip", 372,0,372, 100,0,25);
  loaded += histman->AddH2F(file,mH2F_HitEpd_nmipVchkey[1],"H2F_HitEpd_nmipVchkey_W","NMIP values for West EPD channels by key;key ((pp-1)*31+(tt-1));nmip", 372,0,372, 100,0,25);
    
  loaded += histman->AddH2F(file,mH2F_Epd_earlywVearlye,"H2F_Epd_earlywVearlye","EPD Earliest TAC West vs. East;Earliest East TAC;Earliest West TAC", 421,-10,4200, 421,-10,4200);
  loaded += histman->AddH2F(file,mH2F_Epd_avgwVavge,"H2F_Epd_avgwVavge","EPD Average TAC West vs. East;Average East TAC;Average West TAC", 200,0,1000, 200,0,1000);
  loaded += histman->AddH2F(file,mH2F_EpdTacDiff_avgVearly,"H2F_EpdTacDiff_avgVearly","Epd TAC difference from Average TAC vs. Early TAC;TacDiff Early;TacDiff Avg",200,-3000,3000, 200,-3000,3000);
    
  loaded += histman->AddH2F(file,mH2F_EpdCut_earlywVearlye,"H2F_EpdCut_earlywVearlye","EPD Earliest TAC West vs. East with 1<nMIP<15;Earliest East TAC;Earliest West TAC", 300,0,4200, 300,0,4200);
  loaded += histman->AddH2F(file,mH2F_EpdCut_avgwVavge,"H2F_EpdCut_avgwVavge","EPD Average TAC West vs. East with 1<nMIP<15;Average East TAC;Average West TAC", 300,0,4200, 300,0,4200);
  loaded += histman->AddH2F(file,mH2F_EpdCutTacDiff_avgVearly,"H2F_EpdCutTacDiff_avgVearly","#splitline{Epd TAC difference from Average TAC vs. Early TAC}{with cuts 1<nMIP<15 & TAC>50};TacDiff Early;TacDiff Avg",200,-3000,3000, 200,-3000,3000);

  return loaded;
}

Int_t StMuFcsAnaEpdQaAndVert::DoMake(StMuFcsAnaData* anadata)
{
  FcsEventInfo* EvtInfo = anadata->getEvtInfo();
  TClonesArray* MuEpdHits = 0;
  StEpdCollection* EpdColl = 0;
  anadata->epdColl(MuEpdHits,EpdColl);
  
  //Int_t npoints = ntotal - anadata->mEvtInfo->mClusterSize;
  unsigned int nepdhits = 0;
  StSPtrVecEpdHit* epdhits = 0;
  if( MuEpdHits!=0 ){ nepdhits = MuEpdHits->GetEntriesFast(); }
  else if( EpdColl!=0 ){
    epdhits = &(EpdColl->epdHits());
    nepdhits = epdhits->size();
  }
  else{
    LOG_ERROR << "StMuFcsAnaEpdQaAndVert::FillEpdinfo() - If you see this error then there is a bug that is setting EPD hits improperly" << endm; return kStErr;
  }
  if( mH1F_Epd_NHits!=0 ){ mH1F_Epd_NHits->Fill(nepdhits); }  
  
  //For processing epd hits
  //Larger TAC values mean earlier times so a tac of 0 means latest possible time
  int earliesttace  = 0;  //Stores the largest TAC value in EPD East
  int earliesttacw  = 0;  //Stores the largest TAC value in EPD West
  int navgtacw   = 0;     //Number of TAC values summed for EPD West
  int navgtace   = 0;     //Number of TAC values summed for EPD East
  double sumtacw = 0;     //Sum of TAC values for EPD West
  double sumtace = 0;     //Sum of TAC values for EPD East

  int cut_navgtacw   = 0;     //Number of TAC values summed for EPD West
  int cut_navgtace   = 0;     //Number of TAC values summed for EPD East
  double cut_sumtacw = 0;     //Sum of TAC values for EPD West
  double cut_sumtace = 0;     //Sum of TAC values for EPD East

  int nhitwest   = 0;
  int nhitscut = 0;
  int nhitswestcut = 0;
  StMuEpdHit* muepdhit = 0;
  StEpdHit* epdhit = 0;
  //std::cout << "|MuEpdHits:"<<MuEpdHits << "|EpdColl:"<<EpdColl << "|epdhits:"<<epdhits << "|nepdhits:"<<nepdhits << std::endl;
  for(unsigned int i=0; i<nepdhits; ++i ){
    if( MuEpdHits!=0 ){ muepdhit = (StMuEpdHit*)MuEpdHits->UncheckedAt(i); } //To match similar in StMuDstMaker->epdHit(int i)
    else if( epdhits!=0 ){ epdhit = (StEpdHit*)((*epdhits)[i]); }
    else{ LOG_ERROR << "IF YOU SEE THIS ERROR THEN THERE IS A VERY SERIOUS BUG IN THE CODE" << endm; return kStErr; } 
    //std::cout << "|i:"<<i << "|muepdhit:"<<muepdhit << "|epdhit:"<<epdhit << std::endl;
    int ew    = muepdhit!=0 ? muepdhit->side()    : epdhit->side();      //east=-1, west=1
    int epdpp = muepdhit!=0 ? muepdhit->position(): epdhit->position();  //Supersector runs [1,12]
    int epdtt = muepdhit!=0 ? muepdhit->tile()    : epdhit->tile();      //Tile number [1,31]
    //int adc = muepdhit!=0 ? muepdhit->adc() : epdhit->adc();
    float nmip = muepdhit!=0 ? muepdhit->nMIP(): epdhit->nMIP();         //The ADC value of the hit divided by the MIP peak position; e.g. if nmip==1 then adc value sits at the MIP peak
    int tac = muepdhit!=0 ? muepdhit->tac() : epdhit->tac();
    //std::cout << "|ew:"<<ew << "|pp:"<<epdpp << "|tt:"<<epdtt << "|adc:"<<adc << "|nmip:"<<nmip <<"tac:"<<tac << std::endl
    if( nmip>0.7 ){ ++nhitscut; }
    if( ew==-1 ){ //This is east side
      if( mEpdTacAdcOn ){ ((TH1*)mH2F_HitEpd_tacVadcmip[0]->UncheckedAt( (epdpp-1)*31+(epdtt-1) ))->Fill(nmip,tac); }
      mH2F_HitEpd_nmipVchkey[0]->Fill((epdpp-1)*31+(epdtt-1),nmip);
      sumtace += tac;
      ++navgtace;
      if( tac > earliesttace ){ earliesttace = tac; }
      if( 1<nmip && nmip<15 && tac>50){
	cut_sumtace += tac;
	++cut_navgtace;
	if( tac > mCutEarliestTacE ){ mCutEarliestTacE = tac; }
      }
    }
    if( ew==1 ){ //This is west side
      if( nmip>0.7 ){ ++nhitswestcut; }
      if( mEpdTacAdcOn ){ ((TH1*)mH2F_HitEpd_tacVadcmip[1]->UncheckedAt( (epdpp-1)*31+(epdtt-1) ))->Fill(nmip,tac); }
      mH2F_HitEpd_nmipVchkey[1]->Fill((epdpp-1)*31+(epdtt-1),nmip);
      sumtacw += tac;
      ++navgtacw;
      ++nhitwest;
      if( tac > earliesttacw ){ earliesttacw = tac; }
      if( 1<nmip && nmip<15 && tac>50 ){
	cut_sumtacw += tac;
	++cut_navgtacw;
	if( tac > mCutEarliestTacW ){ mCutEarliestTacW = tac; }
      }
    }
  }// EPD hit loop
  
  double avgtace = 0;
  double avgtacw = 0;
  if( navgtace>0 )    { avgtace = sumtace/static_cast<double>(navgtace); }
  if( navgtacw>0 )    { avgtacw = sumtacw/static_cast<double>(navgtacw); }
  if( cut_navgtace>0 ){ mCutAvgTacE = cut_sumtace/static_cast<double>(cut_navgtace); }
  if( cut_navgtacw>0 ){ mCutAvgTacW = cut_sumtacw/static_cast<double>(cut_navgtacw); }

  if( mH1F_Epd_NHits_Cut!=0 && nhitscut>0 ){ mH1F_Epd_NHits_Cut->Fill(nhitscut); }
  if( mH1F_Epd_NHitsWest!=0 ){ mH1F_Epd_NHitsWest->Fill(nhitwest); }
  if( mH1F_Epd_NHitsWest_Cut!=0 && nhitswestcut>0 ){ mH1F_Epd_NHitsWest_Cut->Fill(nhitswestcut); }

  //std::cout << "|earlye:"<<earliesttace << "|earlyw:"<<earliesttacw << "|avge:"<<avgtace << "|avgw:"<<avgtacw << std::endl;
  if( mH2F_Epd_avgwVavge!=0 )    { mH2F_Epd_avgwVavge->Fill( avgtace,avgtacw  ); }
  if( mH2F_Epd_earlywVearlye!=0 ){ mH2F_Epd_earlywVearlye->Fill(earliesttace,earliesttacw); }
  if( mH2F_EpdTacDiff_avgVearly!=0 ){ mH2F_EpdTacDiff_avgVearly->Fill( earliesttacw-earliesttace, avgtacw - avgtace ); }

  //std::cout << "|cut_earlye:"<<mCutEarliestTacE << "|cut_earlyw:"<<mCutEarliestTacW << "|cut_avge:"<<mCutAvgTacE << "|cut_avgw:"<<mCutAvgTacW << std::endl;
  if( mH2F_EpdCut_avgwVavge!=0 )    { mH2F_EpdCut_avgwVavge->Fill( mCutAvgTacE,mCutAvgTacW  ); }
  if( mH2F_EpdCut_earlywVearlye!=0 ){ mH2F_EpdCut_earlywVearlye->Fill(mCutEarliestTacE,mCutEarliestTacW); }
  if( mH2F_EpdCutTacDiff_avgVearly!=0 ){ mH2F_EpdCutTacDiff_avgVearly->Fill( mCutEarliestTacW-mCutEarliestTacE, mCutAvgTacW-mCutAvgTacE ); }

  //Fill tree event structure with values
  EvtInfo->mEpdTacEarlyE = mCutEarliestTacE;
  EvtInfo->mEpdTacEarlyW = mCutEarliestTacW;
  EvtInfo->mEpdAvgE = mCutAvgTacE;
  EvtInfo->mEpdAvgW = mCutAvgTacW;
  //For vertex only fill if the average TAC is >50 which is determined by eye for Run 22
  if( mCutAvgTacW>50 && mCutAvgTacE>50 ){
    mEpdVertex = (mCutAvgTacW-mCutAvgTacE) * 0.2475;    
    EvtInfo->mEpdVz = mEpdVertex;
  }
  else{ mEpdVertex = -999; }
  //@[Julye 17, 2024] > Using the same logic as BBC since haven't looked at EpdTacDiff histograms, also because EPD has same 15.6ps/TAC as BBC.
  //@[July 21, 2024]>After looking at TAC differences it seems average with no cuts gives actual results and ADC_Nmip cut tac values do not.
  //@[July 23, 2024]>Looking at TAC diff don't need to subtract 4096 like we do for BBC
  //@[August 29, 2024]>Looking at BBC vs. EPD vertex correlation with EPD vertex scale factor -0.2475 shows negative correlation so make this positive now. My guess is this negative sign has to do with the fact that the BBC time difference needs to be subtracted by 4096 so maybe there you do need the minus sign.
  return kStOk;
}


void StMuFcsAnaEpdQaAndVert::DrawEpdAllQa(TCanvas* canv, const char* savename)
{
  DrawEpdHitQa(canv, savename);
  DrawEpdTacQa( canv, savename);
  DrawEpdTacCutQa( canv, savename);
  DrawEpdTacAdcQa( canv, savename);
}

void StMuFcsAnaEpdQaAndVert::DrawEpdHitQa(TCanvas* canv, const char* savename)
{
  canv->Clear();
  canv->Divide(3,2);
  canv->cd(1);
  if( mH1F_Epd_NHits!=0 ){ mH1F_Epd_NHits->Draw("hist e"); }
  canv->cd(2);
  if( mH1F_Epd_NHits_Cut!=0 ){ mH1F_Epd_NHits_Cut->Draw("hist e"); }
  canv->cd(3)->SetLogz(true);
  if( mH2F_HitEpd_nmipVchkey[0]!=0 ){ mH2F_HitEpd_nmipVchkey[0]->Draw("colz"); }
  canv->cd(4);
  if( mH1F_Epd_NHitsWest!=0 ){ mH1F_Epd_NHitsWest->Draw("hist e"); }
  canv->cd(5);
  if( mH1F_Epd_NHitsWest_Cut!=0 ){ mH1F_Epd_NHitsWest_Cut->Draw("hist e"); }  
  canv->cd(6)->SetLogz(true);
  if( mH2F_HitEpd_nmipVchkey[1]!=0 ){ mH2F_HitEpd_nmipVchkey[1]->Draw("colz"); }
  canv->Print(savename);
}

void StMuFcsAnaEpdQaAndVert::DrawEpdTacQa(TCanvas* canv, const char* savename)
{
  canv->Clear();
  canv->Divide(3,2);
  canv->cd(1)->SetLogz(true);
  if( mH2F_Epd_earlywVearlye!=0 ){ mH2F_Epd_earlywVearlye->Draw("colz"); }
  canv->cd(2)->SetLogz(true);
  if( mH2F_Epd_avgwVavge!=0 ){ mH2F_Epd_avgwVavge->Draw("colz"); }
  if( mH2F_EpdTacDiff_avgVearly!=0 ){
    canv->cd(3);
    mH2F_EpdTacDiff_avgVearly->Draw("colz");
    TH1D* earlytac = ((TH2*)mH2F_EpdTacDiff_avgVearly)->ProjectionX("H1F_EpdTacDiff_early");
    earlytac->SetTitle("Epd TAC difference from Earliest TAC;TacDiff Early");
    TH1D* avgtac   = ((TH2*)mH2F_EpdTacDiff_avgVearly)->ProjectionY("H1F_EpdTacDiff_avg");
    avgtac->SetTitle("Epd TAC difference from Average TAC;TacDiff Avg");
    canv->cd(4);
    earlytac->Draw("hist e");
    canv->cd(5);
    avgtac->Draw("hist e");    
  }
  canv->Print(savename);
}

void StMuFcsAnaEpdQaAndVert::DrawEpdTacCutQa(TCanvas* canv, const char* savename)
{
  canv->Clear();
  canv->Divide(3,2);
  canv->cd(1)->SetLogz(true);
  if( mH2F_EpdCut_earlywVearlye!=0 ){ mH2F_EpdCut_earlywVearlye->Draw("colz"); }
  canv->cd(2)->SetLogz(true);
  if( mH2F_EpdCut_avgwVavge!=0 ){ mH2F_EpdCut_avgwVavge->Draw("colz"); }
  if( mH2F_EpdCutTacDiff_avgVearly!=0 ){
    canv->cd(3);
    mH2F_EpdCutTacDiff_avgVearly->Draw("colz");
    TH1D* earlytac = ((TH2*)mH2F_EpdCutTacDiff_avgVearly)->ProjectionX("H1F_EpdCutTacDiff_early");
    earlytac->SetTitle("Epd TAC difference from Earliest TAC and 1<nMIP<15 and TAC>50;TacDiff Early");
    TH1D* avgtac   = ((TH2*)mH2F_EpdCutTacDiff_avgVearly)->ProjectionY("H1F_EpdCutTacDiff_avg");
    avgtac->SetTitle("Epd TAC difference from Average TAC and 1<nMIP<15 and TAC>50;TacDiff Avg");
    canv->cd(4);
    earlytac->Draw("hist e");
    canv->cd(5);
    avgtac->Draw("hist e");
  }
  //canv->cd(6);
  //mH1F_VertexEpd->Draw("hist e");
  canv->Print(savename);
}

void StMuFcsAnaEpdQaAndVert::DrawEpdTacAdcQa(TCanvas* canv, const char* savename)
{
  if( mEpdTacAdcOn || (mH2F_HitEpd_tacVadcmip[0]!=0 && mH2F_HitEpd_tacVadcmip[1]!=0) ){
    canv->Clear();
    canv->Divide(5,5);
    for( UInt_t i=0; i<2; ++i ){
      for( Int_t ich=0, ipad=1; ich<mH2F_HitEpd_tacVadcmip[i]->GetEntriesFast(); ++ich,++ipad ){
	if( ipad>25 ){ ipad=1; canv->Print(savename); canv->Clear(); canv->Divide(5,5); }
	canv->cd(ipad)->SetLogz(1);
	((TH1*)mH2F_HitEpd_tacVadcmip[i]->UncheckedAt(ich))->Draw("colz");
      }
    }
  }
}
/*
Int_t StMuFcsAnaEpdQaAndVert::LoadGraphsFromFile(TFile* file, TObjArray* graphs )
{
  Int_t gloaded = 0;
  gloaded += StMuFcsAnaData::MakeGraph(file,graphs,mGE_VertexEpd,"GE_VertexEpd","EPD vertex mean (Err=RMS) vs. Run index");
  return gloaded;
}

void StMuFcsAnaEpdQaAndVert::FillGraphs(Int_t irun)
{
  mGE_VertexEpd->SetPoint(irun,irun,mH1F_VertexEpd->GetMean());
  mGE_VertexEpd->SetPointError(irun,0,mH1F_VertexEpd->GetRMS());
}

void StMuFcsAnaEpdQaAndVert::DrawGraphVertex(TCanvas* canv, const char* savename)
{
  canv->Clear();
  canv->cd();

  mGE_VertexEpd->Draw("AL");

  canv->Print(savename);
  
}
*/


