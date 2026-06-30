#include "StMuFcsAnaVertex.h"
#include "StBTofHeader.h"

ClassImp(StMuFcsAnaVertex)

StMuFcsAnaVertex::StMuFcsAnaVertex()
{
}

StMuFcsAnaVertex::~StMuFcsAnaVertex()
{
}

UInt_t StMuFcsAnaVertex::LoadHists(TFile* file, HistManager* histman, StMuFcsAnaData* data)
{
  UInt_t loaded = 0;
  if( histman==0 ){ return loaded; }
  loaded += histman->AddH2F(file,mH2F_VertexPrim_yVx,"H2F_VertexPrim_yVx","Primary Vertex y vs. x;x cm;y cm",200,-2,2,200,-2,2);
  loaded += histman->AddH1F(file,mH1F_VertexPrimZ,"H1F_VertexPrimZ","Primary Vertex (z);cm",201,-100.5,100.5);
  loaded += histman->AddH1F(file,mH1F_VertexVpd,"H1F_VertexVpd","Vpd Vertex (z);cm",50,-200,200);
  loaded += histman->AddH1F(file,mH1F_VertexBbc,"H1F_VertexBbc","Bbc Vertex (z);cm",50,-200,200);
  loaded += histman->AddH1F(file,mH1F_BbcTimeDiff,"H1F_BbcTimeDiff","Bbc Time difference", 200,-5000,5000);
  loaded += histman->AddH1F(file,mH1F_VertexZdc,"H1F_VertexZdc","Zdc Vertex (z);cm",50,-200,200);
  loaded += histman->AddH1F(file,mH1F_VertexEpd,"H1F_VertexEpd","Epd Vertex (z);cm",50,-200,200);

  loaded += histman->AddH2F(file,mH2F_VertexZ_vpdVprim,"H2F_VertexZ_vpdVprim","VPD Vertex (z) vs. Primary Vertex;Primary Vertex (z);VPD Vertex(z)", 50,-200,200, 50,-200,200);
  loaded += histman->AddH2F(file,mH2F_VertexZ_epdVprim,"H2F_VertexZ_epdVprim","EPD Vertex (z) vs. Primary Vertex;Primary Vertex (z);EPD Vertex(z)", 50,-200,200, 50,-200,200);
  loaded += histman->AddH2F(file,mH2F_VertexZ_bbcVprim,"H2F_VertexZ_bbcVprim","BBC Vertex (z) vs. Primary Vertex;Primary Vertex (z);BBC Vertex(z)", 50,-200,200, 50,-200,200);
  loaded += histman->AddH2F(file,mH2F_VertexZ_vpdVepd,"H2F_VertexZ_vpdVepd","VPD vs. Epd Vertex (z);EPD (z) cm;VPD (z) cm", 50,-200,200, 50,-200,200);
  loaded += histman->AddH2F(file,mH2F_VertexZ_zdcVepd,"H2F_VertexZ_zdcVepd","ZDC vs. Epd Vertex (z);EPD (z) cm;ZDC (z) cm", 50,-200,200, 50,-200,200);
  loaded += histman->AddH2F(file,mH2F_VertexZ_bbcVepd,"H2F_VertexZ_bbcVepd","BBC vs. Epd Vertex (z);EPD (z) cm;BBC (z) cm", 50,-200,200, 50,-200,200);
  loaded += histman->AddH2F(file,mH2F_VertexZ_vpdVbbc,"H2F_VertexZ_vpdVbbc","VPD vs. BBC Vertex (z);BBC (z) cm;VPD (z) cm", 50,-200,200, 50,-200,200);
  loaded += histman->AddH2F(file,mH2F_VertexZ_vpdVzdc,"H2F_VertexZ_vpdVzdc","VPD vs. ZDC Vertex (z);ZDC (z) cm;VPD (z) cm", 50,-200,200, 50,-200,200);
  loaded += histman->AddH2F(file,mH2F_VertexZ_zdcVbbc,"H2F_VertexZ_zdcVbbc","ZPD vs. BBC Vertex (z);BBC (z) cm;ZDC (z) cm", 50,-200,200, 50,-200,200);

  loaded += histman->AddH2F(file,mH2F_foundVvertex,"H2F_foundVvertex", "Used vertex bit vs. Vertex that was used in Pi0 Reconstruction;Vertex (cm);found (bit) 0=NA,1=Primary,2=VPD,3=EPD,4=BBC", 50,-200,200, 5,0,5);
  return loaded;
}

Int_t StMuFcsAnaVertex::DoMake(StMuFcsAnaData* anadata)
{
  //Local copy of needed variables to make things easier
  FcsEventInfo* EvtInfo = anadata->getEvtInfo();
  StMuDst* MuDst = anadata->muDst();
  StMuEvent* MuEvent = anadata->muEvent();
  const StTriggerData* TrigData = anadata->trigData();
  //StMuEvent* MuEvent = anadata->muEvent();

  //Vertex Information
  //For vertex one possible function is MuEvent->primaryVertexPosition()
  //To get all possible vertex StMuPrimaryVertex *muprimv = MuDst->primaryVertex(index);
  Double_t primvz = -999;
  if( MuDst->primaryVertex()){EvtInfo->mPrimVertRanking = MuDst->primaryVertex()->ranking();}
  else{EvtInfo->mPrimVertRanking=-1;}
  EvtInfo->mPrimVx = MuEvent->primaryVertexPosition().x();
  EvtInfo->mPrimVy = MuEvent->primaryVertexPosition().y();
  primvz = MuEvent->primaryVertexPosition().z();
  EvtInfo->mPrimVz = primvz;
  mH2F_VertexPrim_yVx->Fill(EvtInfo->mPrimVx,EvtInfo->mPrimVy);
  mH1F_VertexPrimZ->Fill( primvz );

  Double_t vpdvz = -999;
  if( MuDst==0 ){ return kStWarn; }
  if( MuDst->btofHeader() ){ vpdvz = MuDst->btofHeader()->vpdVz(); }
  mH1F_VertexVpd->Fill( vpdvz );
  EvtInfo->mVpdVz = vpdvz;

  //@[April 7, 2021] > No Slewing correction for BBC yet, see StFmsJetMaker2015 in BrightSTAR??
  EvtInfo->mBbcTacDiff = TrigData->bbcTimeDifference() - 4096; //subtract 4096 since 0 means bad event and distribution is Gaussian around 4096
  mH1F_BbcTimeDiff->Fill(EvtInfo->mBbcTacDiff);
  Double_t bbcvz = -999;
  if( fabs(EvtInfo->mBbcTacDiff)>1.e-6 ){ bbcvz = EvtInfo->mBbcTacDiff * -0.2475; } //0.2475 = 0.0165*30/2x
  mH1F_VertexBbc->Fill( bbcvz );
  EvtInfo->mBbcVz = bbcvz;

  //StZdcTriggerDetector& zdc = mMuEvent->zdcTriggerDetector();
  //std::cout <<"|ZdcV:"<<zdc.vertexZ() << std::endl;
  Double_t zdcvz = TrigData->zdcVertexZ();
  EvtInfo->mZdcVz = zdcvz;
  mH1F_VertexZdc->Fill( zdcvz );

  Double_t epdvz = EvtInfo->mEpdVz;     //Should be valid if #StMuFcsAnaEpdQaAndVert was called first otherwise it will be -999
  mH1F_VertexEpd->Fill( epdvz );

  //Correlation Histograms
  mH2F_VertexZ_vpdVprim->Fill(primvz,vpdvz);
  mH2F_VertexZ_epdVprim->Fill(primvz,epdvz);
  mH2F_VertexZ_bbcVprim->Fill(primvz,bbcvz);

  mH2F_VertexZ_vpdVepd->Fill(epdvz,vpdvz);
  mH2F_VertexZ_zdcVepd->Fill(epdvz,zdcvz);
  mH2F_VertexZ_bbcVepd->Fill(epdvz,bbcvz);
  mH2F_VertexZ_vpdVbbc->Fill(bbcvz,vpdvz);
  mH2F_VertexZ_vpdVzdc->Fill(zdcvz,vpdvz);
  mH2F_VertexZ_zdcVbbc->Fill(bbcvz,zdcvz);
  
  //std::cout << this->ClassName()<< "|set vertex" << std::endl;
  if( EvtInfo->mPrimVertRanking>-1 ){ anadata->mFoundVertex = 1; anadata->mUseVertex = EvtInfo->mPrimVz; }
  else if( vpdvz > -998 ){ anadata->mFoundVertex = 2; anadata->mUseVertex = vpdvz; }
  else if( epdvz > -998 ){ anadata->mFoundVertex = 3; anadata->mUseVertex = epdvz; }
  else if( bbcvz > -998 ){ anadata->mFoundVertex = 4; anadata->mUseVertex = bbcvz; }
  else{ anadata->mFoundVertex = 0; anadata->mUseVertex = 0; } //If no vertex found use 0
  EvtInfo->mFoundVertex = anadata->mFoundVertex;
  //std::cout << this->ClassName()<< "|foundvertex:"<<mFoundVertex << "|usevertex:"<<mUseVertex << "|mH2F_foundVvertex:"<<mH2F_foundVvertex << std::endl;
  mH2F_foundVvertex->Fill(anadata->mUseVertex,anadata->mFoundVertex);

  return kStOk;
}

void StMuFcsAnaVertex::DrawVertex(TCanvas* canv, const char* savename) const
{
  canv->Clear();
  canv->Divide(3,3);
  canv->cd(1)->SetLogz(true);
  mH2F_VertexPrim_yVx->Draw("colz");
  canv->cd(2);
  mH1F_VertexPrimZ->Draw("hist e");
  canv->cd(3);
  mH1F_VertexVpd->Draw("hist e");
  canv->cd(4)->SetLogy();
  mH1F_VertexZdc->Draw("hist e");
  canv->cd(5);
  mH1F_VertexEpd->Draw("hist e");
  canv->cd(6);
  mH1F_VertexBbc->Draw("hist e");
  canv->cd(7);
  //canv->cd(6)->SetLogy(1);
  mH1F_BbcTimeDiff->Draw("hist e");

  canv->Print(savename);
}

void StMuFcsAnaVertex::DrawVertexCorrelation(TCanvas* canv, const char* savename) const
{
  canv->Clear();
  canv->Divide(3,3);
  canv->cd(1)->SetLogz(true);
  if( mH2F_VertexZ_vpdVprim!=0 ){ mH2F_VertexZ_vpdVprim->Draw("colz"); }
  canv->cd(2)->SetLogz(true);
  if( mH2F_VertexZ_epdVprim!=0 ){ mH2F_VertexZ_epdVprim->Draw("colz"); }
  canv->cd(3)->SetLogz(true);
  if( mH2F_VertexZ_bbcVprim!=0 ){ mH2F_VertexZ_bbcVprim->Draw("colz"); }
  canv->cd(4)->SetLogz(true);
  if( mH2F_VertexZ_vpdVepd!=0 ){ mH2F_VertexZ_vpdVepd->Draw("colz"); }
  canv->cd(5)->SetLogz(true);
  if( mH2F_VertexZ_zdcVepd!=0 ){ mH2F_VertexZ_zdcVepd->Draw("colz"); }
  canv->cd(6)->SetLogz(true);
  if( mH2F_VertexZ_bbcVepd!=0 ){ mH2F_VertexZ_bbcVepd->Draw("colz"); }
  canv->cd(7)->SetLogz(true);
  if( mH2F_VertexZ_vpdVbbc!=0 ){ mH2F_VertexZ_vpdVbbc->Draw("colz"); }
  canv->cd(8)->SetLogz(true);
  if( mH2F_VertexZ_vpdVzdc!=0 ){ mH2F_VertexZ_vpdVzdc->Draw("colz"); }
  canv->cd(9)->SetLogz(true);
  if( mH2F_VertexZ_zdcVbbc!=0 ){ mH2F_VertexZ_zdcVbbc->Draw("colz"); }
  canv->Print(savename);
}

void StMuFcsAnaVertex::DrawVertexCorrelationNoZdc(TCanvas* canv, const char* savename) const
{
  canv->Clear();
  canv->Divide(2,2);
  canv->cd(1)->SetLogz(true);
  if( mH2F_VertexZ_vpdVepd!=0 ){ mH2F_VertexZ_vpdVepd->SetStats(0); mH2F_VertexZ_vpdVepd->Draw("colz"); }
  canv->cd(2)->SetLogz(true);
  if( mH2F_VertexZ_bbcVepd!=0 ){ mH2F_VertexZ_bbcVepd->SetStats(0); mH2F_VertexZ_bbcVepd->Draw("colz"); }
  canv->cd(3)->SetLogz(true);
  if( mH2F_VertexZ_vpdVbbc!=0 ){ mH2F_VertexZ_vpdVbbc->SetStats(0); mH2F_VertexZ_vpdVbbc->Draw("colz"); }
  canv->Print(savename);
}

			  
