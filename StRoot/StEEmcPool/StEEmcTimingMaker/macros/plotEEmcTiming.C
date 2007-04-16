TFile  *file  = 0;
TChain *chain = 0;

#include <vector>

#include "/afs/rhic.bnl.gov/star/packages/DEV/StRoot/StEEmcUtil/EEfeeRaw/EEdims.h"

// summary TTree branches created by StEEmcTimingMaker
Int_t   mRunNumber;
Float_t mTowerDelay;
Float_t mMapmtDelay;
Int_t   mTotalYield;
Int_t   mTowerCrateYield[MaxTwCrates];
Int_t   mMapmtCrateYield[MaxMapmtCrates];
Int_t   mTowerChanYield[MaxTwCrates][MaxTwCrateCh];
Int_t   mMapmtChanYield[MaxMapmtCrates][MaxMapmtCrateCh];
Int_t   mTowerMin;
Int_t   mTowerMax;
Int_t   mMapmtMin;
Int_t   mMapmtMax;

// vectors to hold TGraphs for tower and mapmt crates
Int_t npoints = 0;

TGraphErrors *towerCrateCurves[MaxTwCrates];
TGraphErrors *mapmtCrateCurves[MaxMapmtCrates];

TGraphErrors *towerChanCurves[MaxTwCrates][MaxTwCrateCh];
TGraphErrors *mapmtChanCurves[MaxMapmtCrates][MaxMapmtCrateCh];

// enables printing of output files (gif) for documentation
// figures will appear in same subdirector as input files, 
// specified below.
Bool_t doprint = true;
//Bool_t doprint = false;

void plotEEmcTiming( const Char_t *input_dir="timing_files/")
{  
  
  // chain files
  chainFiles(input_dir);

  // setup branches
  setBranches(input_dir);

  // get total number of points
  Long64_t nruns = chain -> GetEntries();
  npoints=(Int_t)nruns;

  // setup the graphs for each crate and each channel
  initGraphs();

  // loop over all runs
  for ( Long64_t i = 0; i < nruns; i++ ) 
    {
      chain->GetEntry(i);
            
      fillCrates((int)i);
      fillChannels((int)i);

    }

  // draw timing scan curves for tower crates
  drawCrates();
  for ( Int_t ii=0;ii<MaxTwCrates;ii++ ) towerChannels(ii);
  for ( Int_t ii=0;ii<MaxMapmtCrates;ii++ ) mapmtChannels(ii);

  std::cout << "--------------------------------------------------------" << std::endl;
  std::cout << "to view timing curves for any crate" << std::endl;
  std::cout << std::endl;
  std::cout << "towerChannels(icrate) -- icrate = 0-5 for tower crates 1-6"<<std::endl;
  std::cout << "mapmtChannels(icrate) -- icrate = 0-47 for mapmt crates 1-48"<<std::endl;
//  std::cout << "print()               -- make gif files for all crates"<<std::endl;

}
// ----------------------------------------------------------------------------
void print()
{
  drawCrates();
  for ( Int_t ii=0;ii<MaxTwCrates;ii++ ) drawChannels(ii);
  for ( Int_t ii=0;ii<MaxMapmtCrates;ii++ ) drawMapmt(ii);
}

// ----------------------------------------------------------------------------
void drawCrates()
{

  // tower crates first
  TCanvas *towers=new TCanvas("towers","towers",500,400);
  const Char_t *opt[]={"ALP","LP","LP","LP","LP","LP"};

  TLegend *legend=new TLegend(0.125,0.6,0.325,0.85);

  Float_t ymax=0.;
  for ( Int_t icr=0;icr<MaxTwCrates;icr++ )
    {
      towerCrateCurves[icr]->Sort();
      towerCrateCurves[icr]->Draw(opt[icr]);
      TString crname="tw crate ";crname+=icr+1;
      legend->AddEntry( towerCrateCurves[icr], crname, "lp" );
      if ( towerCrateCurves[icr]->GetYaxis()->GetXmax() > ymax )
	ymax=towerCrateCurves[icr]->GetYaxis()->GetXmax();
    }
  towerCrateCurves[0]->SetTitle("EEMC Tower Crate Timing Curves");
  towerCrateCurves[0]->GetXaxis()->SetTitle("TCD phase[ns]");
  TString ytitle=Form("Integral [ped+%i,ped+%i] / N_{events}",mTowerMin,mTowerMax);
  towerCrateCurves[0]->GetYaxis()->SetTitle(ytitle);
  towerCrateCurves[0]->GetYaxis()->SetRangeUser(0.,ymax);
  legend->Draw();

  if(doprint)towers->Print("tower_crates.gif");

  // next mapmt crates, 4 crates per canvas
  TCanvas *mapmt = 0;

  for ( Int_t icr = 0; icr<MaxMapmtCrates; icr+=4 ) 
    {
      
      TString cname="mapmt";cname+=icr+MinMapmtCrateID;
      mapmt=new TCanvas(cname,cname,500,400);
      TString title="EEMC Mapmt Crate Timing Curves ";
      title+=icr+MinMapmtCrateID;
      title+="-";
      title+=icr+MinMapmtCrateID+3;
      legend=new TLegend(0.625,0.12,0.825,0.375);

      Float_t ymax = 0.0;
      for ( Int_t jcr=0;jcr<4;jcr++ ) {
	Int_t index=icr+jcr;
	Int_t crateid=MinMapmtCrateID+index;

	mapmtCrateCurves[index]->Sort();
	mapmtCrateCurves[index]->Draw(opt[jcr]);
	if ( mapmtCrateCurves[index]->GetYaxis()->GetXmax() > ymax )
	  ymax=mapmtCrateCurves[index]->GetYaxis()->GetXmax();
	TString crname="mapmt crate ";crname+=crateid;
	legend->AddEntry(mapmtCrateCurves[index],crname,"lp");

      }
      mapmtCrateCurves[icr]->SetTitle(title);
      mapmtCrateCurves[icr]->GetXaxis()->SetTitle("TCD phase[ns]");
      mapmtCrateCurves[icr]->GetYaxis()->SetTitle("Integral [ped+25,ped+125] / Nevents");
      TString ytitle=Form("Integral [ped+%i,ped+%i] / N_{events}",mMapmtMin,mMapmtMax);
      mapmtCrateCurves[icr]->GetYaxis()->SetTitle(ytitle);
      mapmtCrateCurves[icr]->GetYaxis()->SetRangeUser(0.,ymax*1.05);
      legend->Draw();

      TString oname="mapmt_crates_";
      oname+=icr+MinMapmtCrateID;
      oname+="_";
      oname+=icr+MinMapmtCrateID+4;
      if(doprint)mapmt->Print(oname+".gif");

    }


}
// ----------------------------------------------------------------------------
void mapmtChannels( Int_t crate )
{
  
  static const Int_t stride=16;
  Int_t crateid = MinMapmtCrateID+crate;


  TString fname="mapmt-crate-";fname+=crate+MinMapmtCrateID;fname+=".ps";
  TCanvas *canvas=new TCanvas("canvas","canvas",850/2,1100/2);
  canvas->Divide(1,2);
  Int_t icanvas=0;


  for ( Int_t ich=0;ich<192;ich+=stride ) 
    {

      canvas->cd(1+icanvas%2);
      icanvas++;


      TString pname="crate";
      pname+=crateid;
      pname+="_ch";
      pname+=ich;
      pname+="-";
      pname+=ich+stride-1;

      const Char_t *opts[]={"ALP","LP","LP","LP","LP","LP","LP","LP","LP","LP","LP","LP","LP","LP","LP","LP","LP","LP","LP","LP","LP"};

      // normalize
      Float_t ymax=0.0;
      Double_t sum[stride];for ( Int_t jj=0;jj<stride;jj++ )sum[jj]=0.;
      Double_t max=0.;
      for ( Int_t jch=0;jch<stride;jch++ ) // loop over channels in this one graph
	{
	  Int_t index=ich+jch;
	  Double_t *Y=mapmtChanCurves[crate][index]->GetY();
	  for ( Int_t ipoint=0;ipoint<npoints;ipoint++ ) {
	    if ( Y[ipoint]>ymax ) ymax=Y[ipoint];
	    sum[jch]+=Y[ipoint];
	  }
	  if ( sum[jch]>max ) max=sum[jch];
	}
      if ( max <= 0. ) continue; // meh?

      

      TLegend *legend=new TLegend(0.55,0.11,0.85,0.525);
      for ( Int_t jch=0;jch<stride;jch++ )
	{
	  Int_t index=ich+jch;
	  mapmtChanCurves[crate][index]->SetMarkerSize(0.75);
	  // offset X axis of each of these
	  Double_t *X=mapmtChanCurves[crate][index]->GetX();
	  Double_t *Y=mapmtChanCurves[crate][index]->GetY();
	  Double_t *EY=mapmtChanCurves[crate][index]->GetEY();
	  if ( sum[jch]<= 0. ) continue;
	  //	  std::cout<<"before"<<std::endl;
	  for ( Int_t ip=0;ip<npoints;ip++ ){
	    Float_t shift = 0.5+ ((float)jch) - ((float)stride)/2.0;
	    Double_t yy=Y[ip];
	    X[ip]-= 0.1*shift;
	    Y[ip]*=max/sum[jch];
	    EY[ip]*=max/sum[jch];
	    //	    std::cout << "ip="<<ip<<" y="<<yy<<" y'="<<Y[ip]<<std::endl;
	  }
	  mapmtChanCurves[crate][index]->Sort();
	  if ( !jch ) 
	    mapmtChanCurves[crate][index]->GetXaxis()->SetRangeUser(0.,ymax*1.05);
	  mapmtChanCurves[crate][index]->SetMarkerColor(38+jch);
	  mapmtChanCurves[crate][index]->SetLineColor(38+jch);	  
	  mapmtChanCurves[crate][index]->SetMinimum(0.);	  
	  mapmtChanCurves[crate][index]->Draw(opts[jch]);
	  
	  TString label="crate ";label+=crate+1;label+=" chan ";label+=index;
	  legend->AddEntry(mapmtChanCurves[crate][index],label,"lp");

	}
      legend->Draw();
      canvas->Update();

      //      if(doprint)c->Print(pname+".gif");
      if ( !(icanvas%2) ){
	canvas->Print(fname+"(");
	canvas->Clear();
	canvas->Divide(1,2);
      }
      //      if(doprint)c->Print(pname+".gif");

    }
  canvas->Print(fname+")");
  gSystem->Exec(TString("ps2pdf ")+fname);

}
// ----------------------------------------------------------------------------
void towerChannels( Int_t crate )
{

  static  const  Int_t stride=12;

  TString fname="tower-crate-";fname+=crate+1;fname+=".ps";
  TCanvas *canvas=new TCanvas("canvas","canvas",850/2,1100/2);
  canvas->Divide(1,2);
  Int_t icanvas=0;

  for ( Int_t ich=0;ich<120;ich+=stride )
    {

      canvas->cd(1+icanvas%2);
      icanvas++;

      //    TString aname="crate";aname+=crate;aname+=" channels ";aname+=ich;aname+=" to ";aname+=ich+stride-1;
      //    TCanvas *c = new TCanvas(aname,aname,400,300);

      TString pname="crate";
      pname+=crate+1;
      pname+="_ch";
      pname+=ich;
      pname+="-";
      pname+=ich+stride-1;

      const Char_t *opts[]={"ALP","LP","LP","LP","LP","LP","LP","LP","LP","LP","LP","LP","LP","LP","LP","LP"};
      
      // normalize
      Double_t sum[stride];for ( Int_t jj=0;jj<stride;jj++ )sum[jj]=0.;
      Double_t max=0.;
      for ( Int_t jch=0;jch<stride;jch++ ) // loop over channels in this one graph
	{

	  Int_t index=ich+jch;
	  Double_t *Y=towerChanCurves[crate][index]->GetY();
	  for ( Int_t ipoint=0;ipoint<npoints;ipoint++ ) sum[jch]+=Y[ipoint];
	  if ( sum[jch]>max ) max=sum[jch];
	}
      if ( max <= 0. ) continue; // meh?

      TLegend *legend=new TLegend(0.125,0.15,0.325,0.45);
      for ( Int_t jch=0;jch<stride;jch++ )
	{

	  Int_t index=ich+jch;
	  towerChanCurves[crate][index]->SetMarkerSize(0.75);
	  // offset X axis of each of these
	  Double_t *X=towerChanCurves[crate][index]->GetX();
	  Double_t *Y=towerChanCurves[crate][index]->GetY();
	  Double_t *EY=towerChanCurves[crate][index]->GetEY();
	  if ( sum[jch]<= 0. ) continue;
	  //	  std::cout<<"before"<<std::endl;
	  for ( Int_t ip=0;ip<npoints;ip++ ){
	    Float_t shift = 0.5+ ((float)jch) - ((float)stride)/2.0;
	    Double_t yy=Y[ip];
	    X[ip]-= 0.1*shift;
	    Y[ip]*=max/sum[jch];
	    EY[ip]*=max/sum[jch];
	    //	    std::cout << "ip="<<ip<<" y="<<yy<<" y'="<<Y[ip]<<std::endl;
	  }

	  towerChanCurves[crate][index]->Sort();
	  towerChanCurves[crate][index]->SetMinimum(0.);	  
	  towerChanCurves[crate][index]->SetMarkerColor(38+jch);
	  towerChanCurves[crate][index]->SetLineColor(38+jch);	  
	  towerChanCurves[crate][index]->Draw(opts[jch]);
	  
	  TString label="crate ";label+=crate+1;label+=" chan ";label+=index;
	  legend->AddEntry(towerChanCurves[crate][index],label,"lp");

	}
      legend->Draw();
      canvas->Update();

      //      if(doprint)c->Print(pname+".gif");
      if ( !(icanvas%2) ){
	canvas->Print(fname+"(");
	canvas->Clear();
	canvas->Divide(1,2);
      }

    }
  canvas->Print(fname+")");
  gSystem->Exec(TString("ps2pdf ")+fname);

}



// ----------------------------------------------------------------------------
void fillCrates(Int_t ipoint)
{
#if 1
  // loop over tower crates
  for ( Int_t icr=0;icr<MaxTwCrates;icr++ )
    {
      Float_t yield = (Float_t)mTowerCrateYield[icr];
      Float_t total = (Float_t)mTotalYield;
      if ( total > 10.0 ) {
	Float_t eyield = TMath::Sqrt(yield);
	Float_t etotal = TMath::Sqrt(total);
	Float_t r = yield / total;
	Float_t e1 = (yield>0)? eyield/yield : 0.0;
	Float_t e2 = etotal/total;
	Float_t er = r * TMath::Sqrt( e1*e1 + e2*e2 );
	towerCrateCurves[icr]->SetPoint(ipoint, mTowerDelay, r );
	towerCrateCurves[icr]->SetPointError( ipoint, 0., er );
      }
      else {
	towerCrateCurves[icr]->SetPoint(ipoint, mTowerDelay, -1.0 );
	towerCrateCurves[icr]->SetPointError( ipoint, 0., 0. );
      }
    }
  // loop over mapmt crates
  for ( Int_t icr=0;icr<MaxMapmtCrates;icr++ )
    {
      Float_t yield = (Float_t)mMapmtCrateYield[icr];
      Float_t total = (Float_t)mTotalYield;
      if ( total > 10.0 ) {
	Float_t eyield = TMath::Sqrt(yield);
	Float_t etotal = TMath::Sqrt(total);
	Float_t r = yield / total;
	Float_t e1 = (yield>0)? eyield/yield : 0.0;
	Float_t e2 = etotal/total;
	Float_t er = r * TMath::Sqrt( e1*e1 + e2*e2 );
	mapmtCrateCurves[icr]->SetPoint(ipoint, mMapmtDelay, r );
	mapmtCrateCurves[icr]->SetPointError( ipoint, 0., er );
      }
      else {
	mapmtCrateCurves[icr]->SetPoint(ipoint, mMapmtDelay, -1. );
	mapmtCrateCurves[icr]->SetPointError( ipoint, 0., 0. );	
      }
    }
#endif
}
// ----------------------------------------------------------------------------
void fillChannels(Int_t ipoint)
{

#if 1
  // loop over tower crates
  for ( Int_t icr=0;icr<MaxTwCrates;icr++ )
    {
      for ( Int_t ich=0;ich<MaxTwCrateCh;ich++ ) 
	{

	  Float_t yield = (Float_t)mTowerChanYield[icr][ich];
	  Float_t total = (Float_t)mTotalYield;
	  if ( total > 10.0 ) {
	    Float_t eyield = TMath::Sqrt(yield);
	    Float_t etotal = TMath::Sqrt(total);
	    Float_t r = yield / total;
	    Float_t e1 = (yield>0)? eyield/yield : 0.0;
	    Float_t e2 = etotal/total;
	    Float_t er = r * TMath::Sqrt( e1*e1 + e2*e2 );
	    towerChanCurves[icr][ich]->SetPoint(ipoint, mTowerDelay, r );
	    towerChanCurves[icr][ich]->SetPointError( ipoint, 0., er );
	  }
	  else {
	    towerChanCurves[icr][ich]->SetPoint(ipoint, mTowerDelay, -1.0 );
	    towerChanCurves[icr][ich]->SetPointError( ipoint, 0., 0. );
	  }
	}
    }
#endif
#if 1
  // loop over mapmt crates
  for ( Int_t icr=0;icr<MaxMapmtCrates;icr++ )
    {
      for ( Int_t ich=0;ich<MaxMapmtCrateCh;ich++ ) 
	{

	  Float_t yield = (Float_t)mMapmtChanYield[icr][ich];
	  Float_t total = (Float_t)mTotalYield;
	  if ( total > 10.0 ) {
	    Float_t eyield = TMath::Sqrt(yield);
	    Float_t etotal = TMath::Sqrt(total);
	    Float_t r = yield / total;
	    Float_t e1 = (yield>0)? eyield/yield : 0.0;
	    Float_t e2 = etotal/total;
	    Float_t er = r * TMath::Sqrt( e1*e1 + e2*e2 );
	    mapmtChanCurves[icr][ich]->SetPoint(ipoint, mMapmtDelay, r );
	    mapmtChanCurves[icr][ich]->SetPointError( ipoint, 0., er );
	  }
	  else {
	    mapmtChanCurves[icr][ich]->SetPoint(ipoint, mMapmtDelay, -1.0 );
	    mapmtChanCurves[icr][ich]->SetPointError( ipoint, 0., 0. );
	  }
	}
    }
#endif
}
// ----------------------------------------------------------------------------
void initGraphs()
{

  for ( Int_t i=0;i<MaxTwCrates;i++ ){
    towerCrateCurves[i] =  new TGraphErrors(npoints);
    towerCrateCurves[i]->SetMarkerStyle(20+i);
    towerCrateCurves[i]->SetMarkerColor(i+1);
    towerCrateCurves[i]->SetLineColor(i+1);

    for ( Int_t j=0;j<MaxTwCrateCh;j++ )
      towerChanCurves[i][j]=(TGraphErrors*)towerCrateCurves[i]->Clone();

  }
  for ( Int_t i=0;i<MaxMapmtCrates;i++ ){
    mapmtCrateCurves[i]= new TGraphErrors(npoints);
    mapmtCrateCurves[i]->SetMarkerStyle(20+i%4);
    mapmtCrateCurves[i]->SetMarkerColor(1+i%4);
    mapmtCrateCurves[i]->SetLineColor(1+i%4);

    for ( Int_t j=0;j<MaxMapmtCrateCh;j++ )
      mapmtChanCurves[i][j]=(TGraphErrors*)mapmtCrateCurves[i]->Clone();

  }

  


}
// ----------------------------------------------------------------------------
void chainFiles(const Char_t *path)
{
  chain=new TChain("timing","Timing summary");

  TFile *tfile = 0;
  std::cout << "chaining files in " << path << std::endl;
  TSystemDirectory *dir = new TSystemDirectory("dir",path);
  
  TIter next( dir->GetListOfFiles() );
  TObject *file = 0;
  while ( file = (TObject*)next() )
    {
      TString name=file->GetName();

      // sum the event counter histogram
      if ( name.Contains(".root") ) {
        // open the TFile and
	std::cout << " + " << name << std::endl;
	// tfile = TFile::Open(name);
	chain->Add(name);
      }
    }

}
// ----------------------------------------------------------------------------
void setBranches(const Char_t *dir)
{
  
  chain->SetBranchAddress("mRunNumber",  &mRunNumber );
  chain->SetBranchAddress("mTowerDelay", &mTowerDelay );
  chain->SetBranchAddress("mMapmtDelay", &mMapmtDelay );

  chain->SetBranchAddress("mTotalYield",      &mTotalYield );
  chain->SetBranchAddress("mTowerCrateYield", &mTowerCrateYield );
  chain->SetBranchAddress("mMapmtCrateYield", &mMapmtCrateYield );
  chain->SetBranchAddress("mTowerChanYield",  &mTowerChanYield  );
  chain->SetBranchAddress("mMapmtChanYield",  &mMapmtChanYield  );

  chain->SetBranchAddress("mTowerMin",&mTowerMin);
  chain->SetBranchAddress("mTowerMax",&mTowerMax);
  chain->SetBranchAddress("mMapmtMin",&mMapmtMin);
  chain->SetBranchAddress("mMapmtMax",&mMapmtMax);

}

