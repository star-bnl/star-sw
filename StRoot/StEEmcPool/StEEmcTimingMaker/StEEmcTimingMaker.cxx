#include "StEEmcTimingMaker.h"

#include <TString.h>
#include <TH1F.h>
#include <TGraphErrors.h>
#include <TF1.h>
#include <TLine.h>
#include <TPaveStats.h>
#include <TTree.h>
#include <vector>

#include "StMessMgr.h"

#include "StEEmcDbMaker/StEEmcDbMaker.h"
#include "StEEmcDbMaker/EEmcDbItem.h"
#include "StEEmcUtil/EEfeeRaw/EEname2Index.h"


#include "StEEmcPool/StEEmcA2EMaker/StEEmcA2EMaker.h"

ClassImp(StEEmcTimingMaker);

// ----------------------------------------------------------------------------
StEEmcTimingMaker::StEEmcTimingMaker(const Char_t *name):StMaker(name)
{

  mTowerMin=25;  // min number of ADC > ped
  mTowerMax=125; // max number of ADC > ped
  mMapmtMin=50;
  mMapmtMax=150;

}
// ----------------------------------------------------------------------------
void StEEmcTimingMaker::setRunNumber( Int_t r )
{
  mRunNumber=r;
}

Int_t StEEmcTimingMaker::InitRun(Int_t run){ 
  assert(run==mRunNumber);
  return kStOK; 
}

Int_t StEEmcTimingMaker::Init()
{

#if 1

  for ( Int_t icrate=0;icrate<MaxTwCrates;icrate++ )
    for ( Int_t ichan=0;ichan<MaxTwCrateCh;ichan++ )
      {
	TString name="hTwCr";name+=icrate+1;name+="Ch";name+=ichan;
	TString title="ADC spectrum crate/chan=";
	title += icrate+1;title+="/";title+=ichan;
	hTower[icrate][ichan]=new TH1F(name,title,512,0.,512.);
      }

  for ( Int_t icrate=0;icrate<MaxMapmtCrates;icrate++ )
    for ( Int_t ichan=0;ichan<MaxMapmtCrateCh;ichan++ )
      {
	TString name="hMaCr";name+=MinMapmtCrateID+icrate;name+="Ch";name+=ichan;
	TString title="ADC spectrum crate/chan=";
	title += MinMapmtCrateID+icrate; title+="/";title+=ichan;
	hMapmt[icrate][ichan]=new TH1F(name,title,512,0.,512.);
      }


  for ( Int_t icrate=0;icrate<MaxTwCrates;icrate++ )
    {
      mTowerCrateYield[icrate]=0;
    }
  for ( Int_t icrate=0;icrate<MaxMapmtCrates;icrate++ )
    {
      mMapmtCrateYield[icrate]=0;
    }

  for ( Int_t icrate=0;icrate<MaxTwCrates;icrate++ )
    for ( Int_t ichan=0;ichan<MaxTwCrateCh;ichan++ )
      {
	mTowerChanYield[icrate][ichan]=0;
      }

  for ( Int_t icrate=0;icrate<MaxMapmtCrates;icrate++ )
    for ( Int_t ichan=0;ichan<MaxMapmtCrateCh;ichan++ )
      {
	mMapmtChanYield[icrate][ichan]=0;
      }
  
      
#endif    

  return kStOk;; /* moves histograms to .hist */
}
// ----------------------------------------------------------------------------
Int_t StEEmcTimingMaker::Make()
{
  
  StEEmcA2EMaker *adcmk = (StEEmcA2EMaker*)GetMaker("AandE");
  if ( !adcmk ) {
    std::cout << "No ADC to E maker" <<std::endl;
    return kStWarn;
  }

  StEEmcDbMaker *dbmk=(StEEmcDbMaker*)GetMaker("eemcdb");
  if ( !dbmk ) {
    std::cout << "No database maker" << std::endl;
    return kStWarn;
  }

  mTotalYield++;

  Int_t ilayer=0;
  for ( Int_t itower=0;itower<720;itower++ )
    {
      StEEmcTower tow = adcmk->tower(itower,ilayer);
      const EEmcDbItem *dbitem=dbmk->getByIndex( EEname2Index(tow.name()) );
      assert(dbitem);
      Int_t crate=dbitem->crate;
      Int_t channel=dbitem->chan;
      hTower[crate-1][channel] -> Fill( tow.raw() );
    }
      
  // pre/postshower layers go into mapmt structures
  for ( ilayer=1;ilayer<4;ilayer++ )
    {
      for ( Int_t itower=0;itower<720;itower++ )
	{
	  StEEmcTower tow = adcmk->tower(itower,ilayer);
	  const EEmcDbItem *dbitem=dbmk->getByIndex( EEname2Index(tow.name()) );
	  assert(dbitem);
	  Int_t crate=dbitem->crate;
	  Int_t channel=dbitem->chan;
	  hMapmt[crate-MinMapmtCrateID][channel]->Fill(tow.raw());
	}
    }

  for ( Int_t isector=0;isector<12;isector++ )
    for ( Int_t iplane=0;iplane<2;iplane++ )
      {
	for ( Int_t index=0;index<288;index++ ) {
	  StEEmcStrip strip=adcmk->strip(isector,iplane,index);
	  const EEmcDbItem *dbitem=dbmk->getByIndex( EEname2Index(strip.name()) );
	  if (!dbitem) continue;
	  Int_t crate=dbitem->crate;
	  Int_t channel=dbitem->chan;
	  hMapmt[crate-MinMapmtCrateID][channel]->Fill(strip.raw());
	}
      }

  return kStOK;
}

// ----------------------------------------------------------------------------
Int_t StEEmcTimingMaker::Finish()
{

  std::vector<TH1F*> histos;

  for ( Int_t icrate=0;icrate<MaxTwCrates;icrate++ )
    for ( Int_t ichan=0;ichan<MaxTwCrateCh;ichan++ )
      {
	if ( hTower[icrate][ichan]->Integral()<10 ) continue;
	histos.push_back( hTower[icrate][ichan] );
      }

  for ( Int_t icrate=0;icrate<MaxMapmtCrates;icrate++ )
    for ( Int_t ichan=0;ichan<MaxMapmtCrateCh;ichan++ )
      {
	if ( hMapmt[icrate][ichan]->Integral()<10 ) continue;
	histos.push_back(hMapmt[icrate][ichan]);
      }

  for ( UInt_t ii=0;ii<histos.size();ii++ )
    {
      TF1 fit("fit","gaus");
      Float_t xmax=(Float_t)histos[ii]->GetMaximumBin();
      fit.SetParameter(0, histos[ii]->Integral() );
      fit.SetParameter(1, xmax );
      fit.SetParLimits(1, xmax-2.0, xmax+2.0 );
      fit.SetParameter(2, 0.90 );
      histos[ii]->GetXaxis()->SetRangeUser(xmax-10.,xmax+10.);
      histos[ii]->Fit(&fit,"RQ","",xmax-10.0,xmax+10.0);
    }


  // towers
  for ( Int_t icrate=0;icrate<MaxTwCrates;icrate++ )
    {

      TString hname="hCrate";hname+=(icrate+1);
      TString htitle="Pedestal-subtracted spectrum for crate ";htitle+=(icrate+1);
      TH1F *hSumTowers=new TH1F(hname,htitle,524,-12.,512.);

      for ( Int_t ichan=0;ichan<MaxTwCrateCh;ichan++ )
	{
	  TH1F *h=hTower[icrate][ichan];
	  TF1  *f=(TF1*)h->GetListOfFunctions()->At(0);
	  if ( !f ) continue;// no fit
	  if ( f->IsA() != TF1::Class() ) continue;// object not a fit
	  Float_t xmean = f->GetParameter(1);
	  Float_t xmin  = xmean+mTowerMin;
	  Float_t xmax  = xmean+mTowerMax;
	  h->GetXaxis()->SetRangeUser(xmin,xmax);
	  Float_t sum=h->Integral();
	  mTowerCrateYield[icrate]+=(Int_t)sum;
	  mTowerChanYield[icrate][ichan]+=(Int_t)sum;
	  h->GetXaxis()->UnZoom();
	  h->Draw();

	  Char_t buf[128];
	  sprintf(buf,"Integral [%5.1f,%5.1f] = %i",xmin,xmax,mTowerChanYield[icrate][ichan]);
	  TString title=h->GetTitle();title+=" ";title+=buf;
	  h->SetTitle(title);

	  TLine *l1=new TLine( xmin, -0.05*h->GetMaximum(), xmin, +0.05*h->GetMaximum() );l1->SetLineColor(2);
	  TLine *l2=new TLine( xmax, -0.05*h->GetMaximum(), xmax, +0.05*h->GetMaximum() );l2->SetLineColor(2);
	  h->GetListOfFunctions()->Add(l1);
	  h->GetListOfFunctions()->Add(l2);

	  for ( Int_t ii=1;ii<=512;ii++ )
	    {
	      hSumTowers->Fill( ((Float_t)ii) - xmean, h->GetBinContent(ii) );
	    }
	  
	}
      AddHist(hSumTowers,".hist");

      std::cout << "crate="<<icrate+1<<" sum="<<mTowerCrateYield[icrate]<<std::endl;
    }


  // mapmt
  for ( Int_t icrate=0;icrate<MaxMapmtCrates;icrate++ )
    {

      TString hname="hCrate";hname+=(icrate+MinMapmtCrateID);
      TString htitle="Pedestal-subtracted spectrum for crate ";htitle+=(icrate+1);
      TH1F *hSumMapmt=new TH1F(hname,htitle,524,-12.,512.);

      for ( Int_t ichan=0;ichan<MaxMapmtCrateCh;ichan++ )
	{
	  TH1F *h=hMapmt[icrate][ichan];
	  TF1  *f=(TF1*)h->GetListOfFunctions()->At(0);
	  if ( !f ) continue;// no fit
	  if ( f->IsA() != TF1::Class() ) continue;// object not a fit
	  Float_t xmean = f->GetParameter(1);
	  Float_t xmin  = xmean+mMapmtMin;
	  Float_t xmax  = xmean+mMapmtMax;
	  h->GetXaxis()->SetRangeUser(xmin,xmax);
	  Float_t sum=h->Integral();
	  mMapmtCrateYield[icrate]+=(Int_t)sum;
	  mMapmtChanYield[icrate][ichan]+=(Int_t)sum;
	  h->GetXaxis()->UnZoom();
	  h->Draw();

	  Char_t buf[128];
	  sprintf(buf,"Integral [%5.1f,%5.1f] = %i",xmin,xmax,mMapmtChanYield[icrate][ichan]);
	  TString title=h->GetTitle();title+=" ";title+=buf;
	  h->SetTitle(title);

	  TLine *l1=new TLine( xmin, -0.05*h->GetMaximum(), xmin, +0.05*h->GetMaximum() );l1->SetLineColor(2);
	  TLine *l2=new TLine( xmax, -0.05*h->GetMaximum(), xmax, +0.05*h->GetMaximum() );l2->SetLineColor(2);
	  h->GetListOfFunctions()->Add(l1);
	  h->GetListOfFunctions()->Add(l2);

	  for ( Int_t ii=1;ii<=512;ii++ )
	    {
	      hSumMapmt->Fill( ((Float_t)ii) - xmean, h->GetBinContent(ii) );
	    }
	  
	}
      AddHist(hSumMapmt,".hist");

      std::cout << "crate="<<icrate+MinMapmtCrateID<<" sum="<<mMapmtCrateYield[icrate]<<std::endl;
    }


  // setup summary TTree
  TTree *tree=new TTree("timing","EEmc timing scan TTree");

  tree->Branch("mRunNumber",&mRunNumber,"mRunNumber/I");
  tree->Branch("mTowerDelay",&mTowerDelay,"mTowerDelay/F");
  tree->Branch("mMapmtDelay",&mMapmtDelay,"mMapmtDelay/F");

  tree->Branch("mTotalYield",&mTotalYield,"mTotalYield/I");

  Int_t nTowerCrates        = MaxTwCrates;
  Int_t nMapmtCrates        = MaxMapmtCrates;
  Int_t nTowerCrateChannels = MaxTwCrateCh * MaxTwCrates;
  Int_t nMapmtCrateChannels = MaxMapmtCrateCh * MaxMapmtCrates;

  tree->Branch("nTowerCrates",&nTowerCrates,"nTowerCrates/I");
  tree->Branch("nMapmtCrates",&nMapmtCrates,"nMapmtCrates/I");
  tree->Branch("nTowerCrateChannels",&nTowerCrateChannels,"nTowerCrateChannels/I");
  tree->Branch("nMapmtCrateChannels",&nMapmtCrateChannels,"nMapmtCrateChannels/I");

  tree->Branch("mTowerCrateYield",mTowerCrateYield,"mTowerCrateYield[nTowerCrates]/I");
  tree->Branch("mMapmtCrateYield",mMapmtCrateYield,"mMapmtCrateYield[nMapmtCrates]/I");

  tree->Branch("mTowerChanYield",mTowerChanYield,"mTowerChanYield[nTowerCrateChannels]/I");
  tree->Branch("mMapmtChanYield",mMapmtChanYield,"mMapmtChanYield[nMapmtCrateChannels]/I");

  tree->Branch("mTowerMin",&mTowerMin,"mTowerMin/I");
  tree->Branch("mTowerMax",&mTowerMax,"mTowerMax/I");
  tree->Branch("mMapmtMin",&mMapmtMin,"mMapmtMin/I");
  tree->Branch("mMapmtMax",&mMapmtMax,"mMapmtMax/I");

  tree->Fill();
  
  AddObj(tree,".hist");

  return kStOK;
}
// ----------------------------------------------------------------------------
void  StEEmcTimingMaker::Clear(Option_t *opts)
{
  return;
}
// ----------------------------------------------------------------------------
void StEEmcTimingMaker::setTiming( Float_t t, Float_t m )
{
  mTowerDelay = t;
  mMapmtDelay = m;
}
// ----------------------------------------------------------------------------
void StEEmcTimingMaker::setTowerCuts(Int_t min, Int_t max)
{
  mTowerMin=min;mTowerMax=max;
  LOG_INFO<<GetName()<<" set tower cuts: [ped+"<<min<<",ped+"<<max<<endm;
}
void StEEmcTimingMaker::setMapmtCuts(Int_t min, Int_t max)
{
  mMapmtMin=min;mMapmtMax=max;
  LOG_INFO<<GetName()<<" set mapmt cuts: [ped+"<<min<<",ped+"<<max<<endm;
}
				    
