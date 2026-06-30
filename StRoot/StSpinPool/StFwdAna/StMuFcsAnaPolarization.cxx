#include "StMuFcsAnaPolarization.h"

ClassImp(StMuFcsAnaPolarization)

StMuFcsAnaPolarization::StMuFcsAnaPolarization()
{
}

StMuFcsAnaPolarization::~StMuFcsAnaPolarization()
{
}


UInt_t StMuFcsAnaPolarization::LoadHists(TFile* file, HistManager* histman, StMuFcsAnaData* data)
{
  UInt_t loaded = 0;
  if( histman==0 ){ return loaded; }
  
  loaded += histman->AddH1D(file,mH1D_BluePol,     "H1D_BluePol",     "Blue Beam Polarization;%", 1000,0,100 );
  loaded += histman->AddH1D(file,mH1D_YellowPol,   "H1D_YellowPol",   "Yellow Beam Polarization;%", 1000,0,100 );
  loaded += histman->AddH1D(file,mH1D_BluePolErr,  "H1D_BluePolErr",  "Blue Beam Polarization Error;%", 500,0,10 );
  loaded += histman->AddH1D(file,mH1D_YellowPolErr,"H1D_YellowPolErr","Yellow Beam Polarization Error;%", 500,0,10 );

  return loaded;
}

Int_t StMuFcsAnaPolarization::DoMake(StMuFcsAnaData* anadata)
{
  Int_t fillnum = anadata->runInfo()->beamFillNumber(StBeamDirection::east);
  Int_t evttime = anadata->muEvent()->eventInfo().time();
  PolData* poldat = anadata->getPolData(fillnum);
  if( poldat==0 ){ return kStSkip; }
  
  //Divide by 3600 to convert seconds to hours since dP/dT is in %/hour
  Double_t timeelapsed = Double_t(evttime-poldat->mStartTime)/3600.0;
  //Sum because dPdT is already negative
  Double_t polblue      = timeelapsed * poldat->mBluedPdT + poldat->mBlueP0;
  Double_t polyellow    = timeelapsed * poldat->mYellowdPdT + poldat->mYellowP0;
  Double_t polblueerr   = sqrt(timeelapsed*poldat->mBlueErrdPdT*timeelapsed*poldat->mBlueErrdPdT + poldat->mBlueErrP0*poldat->mBlueErrP0);
  Double_t polyellowerr = sqrt(timeelapsed*poldat->mYellowErrdPdT*timeelapsed*poldat->mYellowErrdPdT + poldat->mYellowErrP0*poldat->mYellowErrP0);
  
  mH1D_BluePol->Fill(polblue);
  mH1D_YellowPol->Fill(polyellow);
  mH1D_BluePolErr->Fill(polblueerr);
  mH1D_YellowPolErr->Fill(polyellowerr);
  
  //std::cout  << " + "<<"|eventnum:"<< mH1D_Entries->GetBinContent(1)  <<"|fillnum:"<<fillnum << "|evttime:"<<evttime << "|polblue:"<<polblue << "|polyellow:"<<polyellow << "|totpolblue:"<<mH1D_Entries->GetBinContent(2) << "|totpolyellow:"<<mH1D_Entries->GetBinContent(3) << std::endl;
  return kStOk;

}

void StMuFcsAnaPolarization::PaintPolarization(TCanvas* canv, const char* savename) const
{
  canv->Clear();
  
  canv->Divide(2,2);
  canv->cd(1);
  mH1D_BluePol->Draw("hist e");
  canv->cd(2);
  mH1D_BluePolErr->Draw("hist e");
  canv->cd(3);
  mH1D_YellowPol->Draw("hist e");
  canv->cd(4);
  mH1D_YellowPolErr->Draw("hist e");

  canv->Print(savename);
}


