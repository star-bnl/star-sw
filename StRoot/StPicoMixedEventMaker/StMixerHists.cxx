#include "StMixerHists.h"

StMixerHists::StMixerHists(char* fileBaseName):
  mSE_Vtx(NULL), mME_Vtx(NULL), mSE_LS(NULL), mSE_US(NULL),
  mME_LS(NULL), mME_US(NULL)
{
  mSE_Vtx = new TH2F(Form("%s_seVtx",fileBaseName),"Vertex pos;vertex x;vertex y",250,-2.5,2.5,250,-2.5,2.5);
  mME_Vtx = new TH2F(Form("%s_meVtx",fileBaseName),"Vertex pos;vertex x;vertex y",250,-2.5,2.5,250,-2.5,2.5);

  mSE_LS = new TH2F(Form("%s_se_ls_mass",fileBaseName),"Same Event LS pair Invariant mass(K#pi);p_{T}(K#pi)(GeV/c),Mass_{K#pi}(GeV/c^{2})",150,0,15,250,0,2.5);
  mSE_US = new TH2F(Form("%s_se_us_mass",fileBaseName),"Same Event US pair Invariant mass(K#pi);p_{T}(K#pi)(GeV/c),Mass_{K#pi}(GeV/c^{2})",150,0,15,250,0,2.5);
  mME_LS = new TH2F(Form("%s_me_ls_mass",fileBaseName),"Mixed Event LS pair Invariant mass(K#pi);p_{T}(K#pi)(GeV/c),Mass_{K#pi}(GeV/c^{2})",150,0,15,250,0,2.5);
  mME_US = new TH2F(Form("%s_me_us_mass",fileBaseName),"Mixed Event US pair Invariant mass(K#pi);p_{T}(K#pi)(GeV/c),Mass_{K#pi}(GeV/c^{2})",150,0,15,250,0,2.5);
}
StMixerHists::~StMixerHists()
{
  delete mSE_Vtx ;
  delete mME_Vtx ;
  delete mSE_LS ;
  delete mSE_US ;
  delete mME_LS ;
  delete mME_US ;
}
void StMixerHists::closeFile()
{
  mSE_Vtx->Write();
  mME_Vtx->Write();
  mSE_LS->Write();
  mSE_US->Write();
  mME_LS->Write();
  mME_US->Write();
}
