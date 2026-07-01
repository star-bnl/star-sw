#include "StFwdAnaEcalPairQa.h"

ClassImp(StFwdAnaEcalPairQa)

StFwdAnaEcalPairQa::StFwdAnaEcalPairQa()
{
}

StFwdAnaEcalPairQa::~StFwdAnaEcalPairQa()
{
}

UInt_t StFwdAnaEcalPairQa::LoadHists(TFile* file, HistManager* histman, StFwdAnaData* anadata)
{
  UInt_t loaded = 0;
  if( histman==0 ){ return loaded; }

  loaded += histman->AddH2F(file,mH2F_ClusEnergy_ph1Vph2,"H2F_ClusEnergy_ph1Vph2","Cluster Energy of photon 1 vs. photon 2 no EPD cuts;Energy Highest Photon (GeV);Energy Next Highest (GeV)", 1000,0,200, 1000,0,200);
  loaded += histman->AddH2F(file,mH2F_PointEnergy_ph1Vph2,"H2F_PointEnergy_ph1Vph2","Point Energy of photon 1 vs. photon 2 no EPD cuts;Energy Highest Photon (GeV);Energy Next Highest (GeV)", 1000,0,200, 1000,0,200);

  loaded += histman->AddH1F(file,mH1F_InvMassClusPairs,"H1F_InvMassClusPairs","Invariant mass of all cluster pairs with minimal cuts;Cluster M_{inv} (GeV)", 500,0,10);
  loaded += histman->AddH1F(file,mH1F_InvMassPointPairs,"H1F_InvMassPointPairs","Invariant mass of all point pairs with minimal cuts;Cluster M_{inv} (GeV)", 500,0,10);
  
  return loaded;
}

Int_t StFwdAnaEcalPairQa::DoMake(StFwdAnaData* anadata)
{

  for( int ipair=0; ipair<anadata->getNPhPair(); ++ipair ){
    StFcsPairCandidate* pairc = anadata->getPhPair(ipair);
    StFcsPhotonCandidate* ph1 = anadata->getPhoton(pairc->mPhoton1Idx);
    StFcsPhotonCandidate* ph2 = anadata->getPhoton(pairc->mPhoton2Idx);
    if( pairc->mFromCluster ){
      mH1F_InvMassClusPairs->Fill(pairc->mass());
      mH2F_ClusEnergy_ph1Vph2->Fill(ph1->mEn,ph2->mEn);
    }
    else{
      mH1F_InvMassPointPairs->Fill(pairc->mass());
      mH2F_PointEnergy_ph1Vph2->Fill(ph1->mEn,ph2->mEn);
    }
  }
  
  return kStOk;
}

void StFwdAnaEcalPairQa::PaintPairEnergy(TCanvas* canv, const char* savename) const
{
  canv->Clear();

  canv->Divide(2,2);

  canv->cd(1)->SetLogz();
  mH2F_ClusEnergy_ph1Vph2->Draw("colz");
  canv->cd(2)->SetLogy();
  mH1F_InvMassClusPairs->Draw("hist e p");

  canv->cd(3)->SetLogz();
  mH2F_PointEnergy_ph1Vph2->Draw("colz");
  canv->cd(4)->SetLogy();
  mH1F_InvMassPointPairs->Draw("hist e p");

  canv->Print(savename);
}

			  
