#include "StEEmcTimingMaker.h"

#include <TString.h>
#include <TH1F.h>
#include <TGraphErrors.h>
#include <TF1.h>
#include <TLine.h>
#include <TPaveStats.h>
#include <TTree.h>
#include <TPDF.h>
#include <TCanvas.h>
#include <TChain.h>
#include <TFile.h>
#include <TKey.h>

#include <vector>

#include "StMessMgr.h"

#include "StEEmcUtil/database/StEEmcDb.h"
#include "StEEmcUtil/database/EEmcDbItem.h"
#include "StEEmcUtil/EEfeeRaw/EEname2Index.h"

#include "StEEmcPool/StEEmcA2EMaker/StEEmcA2EMaker.h"

#include "StMuDSTMaker/COMMON/StMuDstMaker.h"

ClassImp(StEEmcTimingMaker);

// ----------------------------------------------------------------------------
StEEmcTimingMaker::StEEmcTimingMaker(const Char_t *name):StMaker(name)
{

  mTowerMin=25;  // min number of ADC > ped
  mTowerMax=125; // max number of ADC > ped
  mMapmtMin=50;
  mMapmtMax=150;
  mSupressZero=false;
  mOutputFile="timing.root";

  //  TH1F *hTower[ MaxTwCrates ][ MaxTwCrateCh ];
  //  TH1F *hMapmt[ MaxMapmtCrates ][ MaxMapmtCrateCh ];
  for ( Int_t i=0;i<MaxTwCrates;i++ )
    for ( Int_t j=0;j<MaxTwCrateCh;j++ )
      {
	hTower[i][j]=0;
	mTowerMask[i][j]=0;
      }

  for ( Int_t i=0;i<MaxMapmtCrates;i++ )
    for ( Int_t j=0;j<MaxMapmtCrateCh;j++ )
      {
	hMapmt[i][j]=0;
	mMapmtMask[i][j]=0;
      }


  // init summary arrays to zero
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
	mTowerChanSlope[icrate][ichan]=0.;
      }

  for ( Int_t icrate=0;icrate<MaxMapmtCrates;icrate++ )
    for ( Int_t ichan=0;ichan<MaxMapmtCrateCh;ichan++ )
      {
	mMapmtChanYield[icrate][ichan]=0;
	mMapmtChanSlope[icrate][ichan]=0.;
      }


}

void StEEmcTimingMaker::supressZeroAdc(){ mSupressZero=true; }
// ----------------------------------------------------------------------------
void StEEmcTimingMaker::setRunNumber( Int_t r )
{
  mRunNumber=r;
}

// once DB has been initialized, rename/title the histograms
Int_t StEEmcTimingMaker::InitRun(Int_t run){ 
  assert(run==mRunNumber);

  StEEmcA2EMaker *adcmk = (StEEmcA2EMaker*)GetMaker("AandE");
  if ( !adcmk ) {
    std::cout << "No ADC to E maker" <<std::endl;
    return kStWarn;
  }

  StEEmcDb *dbmk = (StEEmcDb*)this->GetDataSet("StEEmcDb");
  if ( !dbmk ) {
    std::cout << "No database maker" << std::endl;
    return kStFatal;
  }


  Int_t ilayer=0;
  for ( Int_t itower=0;itower<720;itower++ )
    {
      StEEmcTower tow = adcmk->tower(itower,ilayer);
      const EEmcDbItem *dbitem=dbmk->getByIndex( EEname2Index(tow.name()) );
      assert(dbitem);
      Int_t crate=dbitem->crate;
      Int_t channel=dbitem->chan;
      TString twname=dbitem->name;
      TString tbname=dbitem->tube;
      TString title=
	Form("ADC spectrum run=%i, name=%s, cr/chan=%3.3d/%3.3d, tube=%s, ",run,twname.Data(),crate,channel,tbname.Data());
      TString hname="h";hname+=twname;
      hTower[crate-1][channel]->SetTitle(title);
      hTower[crate-1][channel]->SetName(hname);
    }

#if 1
  for ( ilayer=1; ilayer<4;ilayer++ ) {
    for ( Int_t itower=0;itower<720;itower++ )
      {
	StEEmcTower tow = adcmk->tower(itower,ilayer);
	const EEmcDbItem *dbitem=dbmk->getByIndex( EEname2Index(tow.name()) );
	assert(dbitem);
	Int_t crate=dbitem->crate;
	Int_t channel=dbitem->chan;
	TString twname=dbitem->name;
	TString tbname=dbitem->tube;
	TString title=Form("ADC spectrum run=%i, name=%s, cr/chan=%3.3d/%3.3d, tube=%s, ",run,twname.Data(),crate,channel,tbname.Data());
	TString hname="h";hname+=twname;
	hMapmt[crate-MinMapmtCrateID][channel]->SetTitle(title);
	hMapmt[crate-MinMapmtCrateID][channel]->SetName(hname);
    }
  }
#endif

#if 1
  for ( Int_t isector=0;isector<12;isector++ )
    for ( Int_t iplane=0;iplane<2;iplane++ )
      {
	for ( Int_t index=0;index<288;index++ ) {
	  StEEmcStrip strip=adcmk->strip(isector,iplane,index);
	  const EEmcDbItem *dbitem=dbmk->getByIndex( EEname2Index(strip.name()) );
	  if (!dbitem) continue;
	  Int_t crate=dbitem->crate;
	  Int_t channel=dbitem->chan;
	  TString twname=dbitem->name;
	  TString tbname=dbitem->tube;
	  TString title=
	    Form("ADC spectrum run=%i, name=%s, cr/chan=%3.3d/%3.3d, tube=%s, ",run,twname.Data(),crate,channel,tbname.Data());
	  TString hname="h";hname+=twname;
	  hMapmt[crate-MinMapmtCrateID][channel]->SetTitle(title);
	  hMapmt[crate-MinMapmtCrateID][channel]->SetName(hname);
	}
      }
#endif
  LOG_INFO<<GetName()<<"::InitRun("<<run<<") histograms renamed"<<endm;

  return kStOK; 
}

// Initialize histograms.  By default, names will be [Tw or Ma][crate][chn].
// Once DB has been initialized in InitRun, we rename using, eg, h01TA01
// naming convention.  Note that hw channels which map to no detector will
// hold the old naming convention.
Int_t StEEmcTimingMaker::Init()
{

  hCounter=new TH1F("hCounter","Event counter;status",1,0.,1.);
  hCounter->SetBit(TH1::kCanRebin);

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
	mTowerChanSlope[icrate][ichan]=0.;
      }

  for ( Int_t icrate=0;icrate<MaxMapmtCrates;icrate++ )
    for ( Int_t ichan=0;ichan<MaxMapmtCrateCh;ichan++ )
      {
	mMapmtChanYield[icrate][ichan]=0;
	mMapmtChanSlope[icrate][ichan]=0.;
      }
  
      
#endif    

  return kStOk;; /* moves histograms to .hist */
}
// ----------------------------------------------------------------------------
// fill raw ADC for each valid detector
Int_t StEEmcTimingMaker::Make()
{

  hCounter->Fill("Nevents",1.);
  
  StEEmcA2EMaker *adcmk = (StEEmcA2EMaker*)GetMaker("AandE");
  if ( !adcmk ) {
    std::cout << "No ADC to E maker" <<std::endl;
    return kStWarn;
  }

  hCounter->Fill("Nadc2e",1.0);

  StEEmcDb *dbmk = (StEEmcDb*)this->GetDataSet("StEEmcDb");
  if ( !dbmk ) {
    std::cout << "No database maker" << std::endl;
    return kStWarn;
  }

  hCounter->Fill("Ndb",1.0);

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

  static Int_t ncalled=0;
  std::cout << Form("StEEmcTimingMaker::Finish() called for the %i-%s time",ncalled+1,(ncalled)?"nd":"st") << std::endl;
  if ( ncalled ) return kStOK; // been there, done that
  ncalled++;

  std::vector<TH1F*> histos;  

  for ( Int_t icrate=0;icrate<MaxTwCrates;icrate++ )
    for ( Int_t ichan=0;ichan<MaxTwCrateCh;ichan++ )
      {

	if ( !hTower[icrate][ichan] ) continue; // skip null histograms
	if ( hTower[icrate][ichan]->Integral()<10 ) continue;
	histos.push_back( hTower[icrate][ichan] );
      }

  for ( Int_t icrate=0;icrate<MaxMapmtCrates;icrate++ )
    for ( Int_t ichan=0;ichan<MaxMapmtCrateCh;ichan++ )
      {
	if ( !hMapmt[icrate][ichan] ) continue; // skip null histograms
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

#if 0
      std::cout << Form("+ fit %s: ped=%5.2f sig=%5.3f",histos[ii]->GetName(),fit.GetParameter(1),fit.GetParameter(2)) << std::endl;
      if ( ii == 0 ) {
	histos[ii]->GetListOfFunctions()->Print();
	return kStOK;
      }
#endif

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
	  if ( !h ) continue; // skip null channels

	  TString htitle=h->GetTitle(); 
	  LOG_INFO<<"Fitting "<<htitle<<endl;

	  TF1  *f=(TF1*)h->GetListOfFunctions()->At(0);
	  if ( !f ) continue;// no fit
	  if ( f->IsA() != TF1::Class() ) {
	    LOG_WARN<<" somebody inserted something into "<<h->GetName()<<"'s list of functions, and it's messing up the output"<<endm;
#if 0
	    std::cout << "Print out slot 0" << std::endl;
	    f->Print();
	    std::cout << "Print all functions" << std::endl;
	    h->GetListOfFunctions()->Print();
	    return kStOK;
#endif
	    continue;// object not a fit
	  }
	  Float_t xmean = f->GetParameter(1);
	  Float_t xwidth= f->GetParameter(2);
	  Float_t xmin  = xmean+mTowerMin;
	  Float_t xmax  = xmean+mTowerMax;
	  h->GetXaxis()->SetRangeUser(xmin,xmax);
	  Float_t sum=h->Integral();
	  if ( !mTowerMask[icrate][ichan] ) mTowerCrateYield[icrate]+=(Int_t)sum;
	  mTowerChanYield[icrate][ichan]+=(Int_t)sum;
	  
	  h->GetXaxis()->SetRangeUser(xmin-2.0*mTowerMin,xmax+mTowerMin);
	  h->Draw();

	  TLine *l1=new TLine( xmin, -0.05*h->GetMaximum(), xmin, +0.05*h->GetMaximum() );l1->SetLineColor(2);
	  TLine *l2=new TLine( xmax, -0.05*h->GetMaximum(), xmax, +0.05*h->GetMaximum() );l2->SetLineColor(2);
	  h->GetListOfFunctions()->Add(l1);
	  h->GetListOfFunctions()->Add(l2);

	  for ( Int_t ii=1;ii<=512;ii++ )
	    {
	      hSumTowers->Fill( ((Float_t)ii) - xmean, h->GetBinContent(ii) );
	    }

	  TF1 *slope=new TF1("slope","expo",0.,512.);
	  slope->SetLineColor(2); 
	  h->Fit(slope,"RQ+","",xmin,xmax);
	  if ( slope->GetParameter(1) != 0. )
	    mTowerChanSlope[icrate][ichan]=1.0/slope->GetParameter(1);

	  htitle+=Form("ped=%6.2f, ",xmean);
	  htitle+=Form("width=%6.4f, ",xwidth);
	  htitle+=Form("xmin=%5.1f, xmax=%5.1f, Integral=%i, ",xmin,xmax,mTowerChanYield[icrate][ichan]);
	  htitle+=Form("inv. slope=%8.5g+/-%8.5g, ",slope->GetParameter(1),slope->GetParError(1));

	  h->SetTitle(htitle);
	  
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
	  if ( !h ) continue; // skip null channels

	  TString htitle=h->GetTitle();
	  LOG_INFO<<"Fitting "<<htitle<<endl;

	  TF1  *f=(TF1*)h->GetListOfFunctions()->At(0);
	  if ( !f ) continue;// no fit
	  if ( f->IsA() != TF1::Class() ) {
	    LOG_WARN<<" somebody inserted something into "<<h->GetName()<<"'s list of functions, and it's messing up the output"<<endm;
	    continue;// object not a fit
	  }

	  Float_t xmean = f->GetParameter(1);
	  Float_t xwidth = f->GetParameter(2);
	  Float_t xmin  = xmean+mMapmtMin;
	  Float_t xmax  = xmean+mMapmtMax;
	  h->GetXaxis()->SetRangeUser(xmin,xmax);
	  Float_t sum=h->Integral();


	  if ( !mMapmtMask[icrate][ichan] ) mMapmtCrateYield[icrate]+=(Int_t)sum;
	  mMapmtChanYield[icrate][ichan]+=(Int_t)sum;
	  h->GetXaxis()->SetRangeUser(xmin-2.0*mMapmtMin,xmax+mMapmtMin);
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
	  
	  TF1 *slope=new TF1("slope","expo",0.,512.);
	  slope->SetLineColor(2); 
	  h->Fit(slope,"RQ+","",xmin,xmax);
	  if ( slope->GetParameter(1) != 0. )
	    mMapmtChanSlope[icrate][ichan]=1.0/slope->GetParameter(1);

	  htitle+=Form("ped=%6.2f, ",xmean);
	  htitle+=Form("width=%6.4f, ",xwidth);
	  htitle+=Form("xmin=%5.1f, xmax=%5.1f, Integral=%i, ",xmin,xmax,mMapmtChanYield[icrate][ichan]);
	  htitle+=Form("inv. slope=%8.5g+/-%8.5g, ",slope->GetParameter(1),slope->GetParError(1));
	  h->SetTitle(htitle);

	}
      AddHist(hSumMapmt,".hist");

      std::cout << "crate="<<icrate+MinMapmtCrateID<<" sum="<<mMapmtCrateYield[icrate]<<std::endl;
    }


  // setup summary TTree
//  TFile *ff = new TFile(mOutputFile,"RECREATE");
//  ff->cd();
  TTree *tree=new TTree("timing","EEmc timing scan TTree");
  mTree=tree;

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
  //  tree->Branch("mTowerChanSlope",mTowerChanSlope,"mTowerChanSlope[nTowerCrateChannels]/F");
  //  tree->Branch("mMapmtChanSlope",mMapmtChanSlope,"mMapmtChanSlope[nMapmtCrateChannels]/F");

  tree->Branch("mTowerMin",&mTowerMin,"mTowerMin/I");
  tree->Branch("mTowerMax",&mTowerMax,"mTowerMax/I");
  tree->Branch("mMapmtMin",&mMapmtMin,"mMapmtMin/I");
  tree->Branch("mMapmtMax",&mMapmtMax,"mMapmtMax/I");

  tree->Fill();
  
  AddObj(tree,".hist");
//  ff->Write();

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
  LOG_INFO<<GetName()<<" set tower cuts: ped+"<<min<<",ped+"<<max<<endm;
}
void StEEmcTimingMaker::setMapmtCuts(Int_t min, Int_t max)
{
  mMapmtMin=min;mMapmtMax=max;
  LOG_INFO<<GetName()<<" set mapmt cuts: ped+"<<min<<",ped+"<<max<<endm;
}
				    
// ----------------------------------------------------------------------------
void StEEmcTimingMaker::dumpAsciiFile( const Char_t *fname )
{


  LOG_INFO<<GetName()<<"::dumpAsciiFile("<<fname<<");"<<endm;

  ofstream ofile(fname);
  ofile << "Run number: " << mRunNumber << endl;


#if 0
  StMuDstMaker *mumk=(StMuDstMaker*)GetMaker("MuDst");
  if ( mumk ) {
    ofile << "Number of entries: " << mumk->chain()->GetEntries() << endl;
    TObjArray *oa=mumk->chain()->GetListOfFiles();
    Int_t n=oa->GetEntries();
    for ( Int_t i=0;i<n;i++ ) {
      TNamed *named=(TNamed*)oa->At(i);
      if(named)
	ofile << "+ "<<named->GetName()<<endl;
    }
  }
  ofile<<endl;

  ofile<<"Tower cuts: "<<mTowerMin<<" "<<mTowerMax<<endl;
  ofile<<"Mapmt cuts: "<<mMapmtMin<<" "<<mMapmtMax<<endl;
  ofile<<endl;
#endif

#if 1
  const Char_t *sn[]={"01","02","03","04","05","06","07","08","09","10","11","12"};
  const Char_t *ln[]={"T","P","Q","R"};
  const Char_t *bn[]={"A","B","C","D","E"};

  for ( Int_t lay=0;lay<4;lay++ ){
    for ( Int_t sec=0;sec<12;sec++ ){
      for ( Int_t sub=0;sub<5;sub++ ){
	{
	  for ( Int_t eta=0;eta<12;eta++ )
	    {
	      TString tname=sn[sec];tname+=ln[lay];tname+=bn[sub];tname+=sn[eta];
	      TString hname="h";hname+=tname;
	      TH1F *h=(TH1F*)GetHist(hname);   
	      ofile << h->GetTitle() <<endl;
	    }
	}
      }
    }
  }

  const Char_t *pn[]={"U","V"};
  for ( Int_t pln=0;pln<2;pln++ )
    for ( Int_t sec=0;sec<12;sec++ )
      {
	for ( Int_t strip=0;strip<288;strip+=16 ) {
	  for ( Int_t i=0;i<16;i++ ) {
	    Int_t istrip=strip+i;
	    TString tname =sn[sec];
	    tname+=pn[pln];
	    if (istrip+1<100) tname+="0";
	    if (istrip+1<10)  tname+="0";
	    tname+=(istrip+1);
	    TString hname="h";hname+=tname;
	    TH1F *h=(TH1F*)GetHist(hname);	  
	    ofile<<h->GetTitle()<<endl;
	  }
	}
      }

  
#endif

}

// ----------------------------------------------------------------------------
void StEEmcTimingMaker::dumpPDF( const Char_t *fname )
{



  TCanvas *c1=new TCanvas("c1","c1",850,1100);
  c1->Divide(3,4);
  c1->SetLogy();

  const Char_t *sn[]={"01","02","03","04","05","06","07","08","09","10","11","12"};
  const Char_t *ln[]={"T","P","Q","R"};
  const Char_t *bn[]={"A","B","C","D","E"};

  c1->Print(TString(fname)+"("); // print and leave open
  for ( Int_t lay=0;lay<4;lay++ ){
    for ( Int_t sec=0;sec<12;sec++ ){
      for ( Int_t sub=0;sub<5;sub++ ){
	{
	  for ( Int_t eta=0;eta<12;eta++ )
	    {
	      c1->cd(eta+1);
	      c1->cd(eta+1)->SetLogy();
	      TString tname=sn[sec];tname+=ln[lay];tname+=bn[sub];tname+=sn[eta];
	      TString hname="h";hname+=tname;
	      TH1F *h=0;
	      h=(TH1F*)GetHist(hname);	
	      if (h) h->Draw();
	      else {
		hname.ReplaceAll("h","a"); // from L2ped file
		h=(TH1F*)GetHist(hname);
		if ( h ) h->Draw();
	      }
	    }
	  c1->Print(TString(fname)+"("); // print and leave open
	  
	}	
      }
    }
  }

  c1->Clear();
  c1->Divide(4,4);
  c1->SetLogy();

  const Char_t *pn[]={"U","V"};
  for ( Int_t pln=0;pln<2;pln++ )
    for ( Int_t sec=0;sec<12;sec++ )
      {
	for ( Int_t strip=0;strip<288;strip+=16 ) {
	  for ( Int_t i=0;i<16;i++ ) {
	    Int_t istrip=strip+i;
	    c1->cd(i+1);
	    c1->cd(i+1)->SetLogy();
	    TString tname =sn[sec];
	    tname+=pn[pln];
	    if (istrip+1<100) tname+="0";
	    if (istrip+1<10)  tname+="0";
	    tname+=(istrip+1);
	    TString hname="h";hname+=tname;
	    TH1F *h=(TH1F*)GetHist(hname);	  
	    if(h)h->Draw();
	  }
	  c1->Print(TString(fname)+"("); // print and leave open
	}
      }
  c1->Clear();
  c1->Print(TString(fname)+")"); // print and close




#if 0

  TCanvas *c1=new TCanvas("c1","c1",850,1100);
  TPDF *pdf=new TPDF(fname,111);
  c1->Divide(3,4);
  c1->SetLogy();
 
   const Char_t *sn[]={"01","02","03","04","05","06","07","08","09","10","11","12"};
  const Char_t *ln[]={"T","P","Q","R"};
  const Char_t *bn[]={"A","B","C","D","E"};

  // loop ovEr towers and write pdf file
  Int_t i=1;

  for ( Int_t lay=0;lay<4;lay++ )
    for ( Int_t sec=0;sec<12;sec++ )
      for ( Int_t sub=0;sub<5;sub++ )
	for ( Int_t eta=0;eta<12;eta++ )
	  {
	    if ( !(i%12) ) {
	      c1->Update();
	      pdf->NewPage();
	    }
	    TString tname=sn[sec];tname+=ln[lay];tname+=bn[sub];tname+=sn[eta];
	    TString hname="h";hname+=tname;
	    TH1F *h=(TH1F*)GetHist(hname);
	    c1->cd((i++ % 12));
	    if(h) h->Draw();

	  }

  // loop over SMD 
  c1=new TCanvas("c1","c1",850,1100);
  c1->Divide(4,4);
  c1->SetLogy();


  const Char_t *pn[]={"U","V"};

  i=1;
  pdf->NewPage();
  for ( Int_t sec=0;sec<12;sec++ )
    for ( Int_t pln=0;pln<2;pln++ ) 
      for ( Int_t strip=0;strip<288;strip++ )
	{
	  if ( !(i%16) ) {
	    c1->Update();
	    pdf->NewPage();
	  }
	  TString tname =sn[sec];
	  	  tname+=pn[pln];
	  if (strip+1<100) tname+="0";
	  if (strip+1<10)  tname+="0";
	  tname+=(strip+1);
	  TString hname="h";hname+=tname;
	  TH1F *h=(TH1F*)GetHist(hname);
	  c1->cd((i++ % 16));
	  if(h) h->Draw();
	  
	}
#endif


}





// ---------------------------------------------------
void StEEmcTimingMaker::processFromL2( const Char_t *fname, int nevents )
{

  /*
   * Load in the L2ped histogram/root file and set pointers
   * to each of the tower histograms
   *
   * o We rely on the title of the histogram to contain the crate and channel number
   *   (Jan likely gets this from the same place we would... the database).
   *
   * o Once tower histogram pointers are set, we should be able to call finish
   *   which will perform the fitting and yield extractions
   *
   */



  mTotalYield = nevents;
  std::cout << Form("Timing from L2ped file: %s nevents: %i",fname,nevents) << std::endl;
  TFile *f= new TFile(fname);
  assert(f);
  TList *l = f->GetListOfKeys();
  TIter next(l);
  TKey *key = 0;
  while ((key = (TKey*)next()))
    {
      TObject *o = key->ReadObj();
      TString name = o->GetName();
      if ( !name.Contains("T") ) continue; // skip bemc towers
      TH1F *h = (TH1F*)o;
      TString title=h->GetTitle();
      Int_t channel=-1;
      Int_t crate = -1;

      // this line needs to change if L2ped histogram title changes
      sscanf(title.Data(),"%*s cr/ch=%03d/%03d",&crate,&channel);

      std::cout << Form("+ name: %s title: %s crate: %i channel: %i",name.Data(),title.Data(),crate,channel) << std::endl;

      assert(crate>0);                // crates count from 1
      assert(channel>=0);             // channels count from 0
      hTower[crate-1][channel] = h;   // add pointer

    }


}
