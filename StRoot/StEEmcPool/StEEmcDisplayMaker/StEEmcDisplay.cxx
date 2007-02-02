#include "StEEmcDisplay.h"

#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"

#include "TH1F.h"
#include "TH2F.h"
#include "TMarker.h"
#include "TLine.h"
#include "TCanvas.h"
#include "TPaveText.h"
#include "TLatex.h"

#include "StMessMgr.h"

ClassImp(StEEmcDisplay);

StEEmcDisplay::StEEmcDisplay( const Char_t *name, const Char_t *title ) : TNamed(name,title)
{
  memset( mTowerEnergy, 0, sizeof(mTowerEnergy) );
  memset( mTowerStat,   0, sizeof(mTowerStat)   );
  memset( mTowerFail,   0, sizeof(mTowerFail)   );
  memset( mStripEnergy, 0, sizeof(mStripEnergy) );
  memset( mStripStat,   0, sizeof(mStripStat)   );
  memset( mStripFail,   0, sizeof(mStripFail)   );
  eemc=0;
  smdu=0;
  smdv=0;
  pre1=0;
  pre2=0;
  post=0;
  hTowers=0;
  hPre1=0;
  hPre2=0;
  hPost=0;
  geom = new EEmcGeomSimple();
  mEnergy2Mip=1000.0/1.3;
  nClustersU=0;
  nClustersV=0;
  nPoints=0;
  nPi0=0;
  nEta=0;
  mHighTowerEtabin=-1;
  mHighTowerPhibin=-1;
  mHighTowerEnergy=0.;
}

void StEEmcDisplay::clear()
{
  //LOG_INFO<<GetName()<<"::clear() called"<<endm;
  memset( mTowerEnergy, 0, sizeof(mTowerEnergy) );
  memset( mTowerStat,   0, sizeof(mTowerStat)   );
  memset( mTowerFail,   0, sizeof(mTowerFail)   );
  memset( mStripEnergy, 0, sizeof(mStripEnergy) );
  memset( mStripStat,   0, sizeof(mStripStat)   );
  memset( mStripFail,   0, sizeof(mStripFail)   );

  mHighTowerEtabin=-1;
  mHighTowerPhibin=-1;
  mHighTowerEnergy=0.;

  mClusterKey.clear();
  mClusterEnergy.clear();
  mClusterMean.clear();
  mClusterSigma.clear();
  mClusterSector.clear();
  mClusterPlane.clear();
  for ( UInt_t ii=0;ii<mClusterStrips.size();ii++) mClusterStrips[ii].clear();
  mClusterStrips.clear();
  mClusterHisto.clear();
  mClusterStats.clear();
  mClusterColor.clear();
  mClusterFill.clear(); 
  mClusterSplit.clear();

  mPointKey.clear();
  mPointEnergy.clear();
  mPointX.clear();
  mPointY.clear();
  mPointMarker.clear();
  mPointColor.clear();
  mPointStyle.clear();  
  mPointUkey.clear();
  mPointVkey.clear();

  if (hTowers ) delete hTowers;
  hTowers=0;

  if ( hPre1 ) delete hPre1;
  hPre1=0;
  
  if ( hPre2 ) delete hPre2;
  hPre2 = 0;
  
  if ( hPost) delete hPost;
  hPost = 0;

  for ( Int_t sec=0;sec<12;sec++ )
    for ( Int_t plane=0;plane<2;plane++ ){
      if ( hSmds[sec][plane] ) delete hSmds[sec][plane];
      hSmds[sec][plane]=0;
    }
  if (eemc) delete eemc; eemc=0;
  if (smdu) delete smdu; smdu=0;
  if (smdv) delete smdv; smdv=0;

  if (pre1) delete pre1; pre1=0;
  if (pre2) delete pre2; pre2=0;
  if (post) delete post; post=0;

  nClustersU=0;
  nClustersV=0;
  nPoints=0;
  nPi0=0;
  nEta=0;

}

void StEEmcDisplay::add( StEEmcTower tower )
{
  Int_t sec = tower.sector();
  Int_t sub = tower.subsector();
  Int_t eta = tower.etabin();
  Int_t layer=tower.layer();
  if ( layer == 0 ){
    mTowerEnergy[sec][sub][eta]=tower.energy();
    mTowerStat[sec][sub][eta]=tower.stat();
    mTowerFail[sec][sub][eta]=tower.fail();
  }
  else{
    mPrepostEnergy[sec][sub][eta][layer-1]=tower.energy();
    mPrepostStat[sec][sub][eta][layer-1]=tower.stat();
    mPrepostFail[sec][sub][eta][layer-1]=tower.fail();
    return;
  }

  if ( tower.energy() > mHighTowerEnergy )
    {
      mHighTowerEtabin=eta;
      mHighTowerPhibin=tower.phibin();
      mHighTowerEnergy=tower.energy();
    }

}

void StEEmcDisplay::add( StEEmcStrip strip )
{
  Int_t sec = strip.sector();
  Int_t plane = strip.plane();
  Int_t index=strip.index();
  mStripEnergy[sec][plane][index]=strip.energy();
  mStripStat[sec][plane][index]=strip.stat();
  mStripFail[sec][plane][index]=strip.fail();
}

void StEEmcDisplay::add( StEEmcSmdCluster cluster, Int_t color, Int_t fill )
{
  mClusterKey.push_back( cluster.key() );
  mClusterSector.push_back( cluster.sector() );
  mClusterEnergy.push_back( cluster.energy() );
  mClusterPlane.push_back( cluster.plane() );
  std::vector< Int_t > strips;
  for ( Int_t ii=0;ii<cluster.size();ii++ )
    strips.push_back( cluster.strip(ii).index() );
  mClusterStrips.push_back( strips );
  mClusterColor.push_back( color );
  mClusterFill.push_back( fill );
  mClusterMean.push_back( cluster.mean() );
  mClusterSigma.push_back( cluster.sigma() );
  mClusterSplit.push_back( cluster.split() );
  if ( cluster.plane()==0 ) nClustersU++;
  if ( cluster.plane()==1 ) nClustersV++;
}

void StEEmcDisplay::add( StEEmcPoint point, Int_t color, Int_t style )
{
  mPointKey.push_back( point.key() );
  mPointEnergy.push_back( point.energy() );
  TVector3 position=point.position();
  mPointX.push_back( position.X() );
  mPointY.push_back( position.Y() );
  mPointColor.push_back( color );
  mPointStyle.push_back( style );
  mPointUkey.push_back( point.cluster(0).key() );
  mPointVkey.push_back( point.cluster(1).key() );
  nPoints++;
}

// ----------------------------------------------------------------------------

TH2F *StEEmcDisplay::DrawTowers(Option_t *opts)
{

  // generate canvas to display EEMC tower information
  if ( !eemc ) {
    eemc=new TCanvas(TString("eemc-")+GetName(),GetTitle(),600,400);
    eemc->cd();
  }
  // draw and return if it already exists
  if ( hTowers ) {
    hTowers -> Draw(opts);
    return hTowers;
  }

  // create new histogram and fill it
  hTowers = new TH2F(TString("hTowers-")+GetName(),GetTitle(),62,-1.,61.,12,0.,12.);
   
  // loop over all towers and set energy
  for ( Int_t sec=0;sec<kEEmcNumSectors;sec++ )
    for ( Int_t sub=0;sub<kEEmcNumSubSectors;sub++ )
      for ( Int_t eta=0;eta<kEEmcNumEtas;eta++ )
	{

	  Float_t x = 5*sec+sub; x+=0.5;
	  Float_t y = eta; y+=0.5;
	  Int_t bin = hTowers->FindBin ( x, y );

	  if ( !mTowerFail[sec][sub][eta] )
	    hTowers->SetBinContent( bin, mTowerEnergy[sec][sub][eta] );
	  else {
	    failTower(sec,sub,eta, hTowers);
	    /* nada */ 
	  };

	  // wrap right hand edge
	  if ( sec==11 && sub==4 )
	    {
	      x=-0.5;
	      bin = hTowers->FindBin ( x, y );

	      if ( !mTowerFail[sec][sub][eta] )
		hTowers->SetBinContent( bin, mTowerEnergy[sec][sub][eta] );
	      else
		failTower(0,-1,eta,hTowers);

	    }

	  // wrap left hand edge
	  if ( sec==0 && sub==0 )
	    {
              x=60.5;
              bin = hTowers->FindBin ( x, y );
              if ( !mTowerFail[sec][sub][eta] )
                hTowers->SetBinContent( bin, mTowerEnergy[sec][sub][eta] );
	      else
		failTower(12,5,eta,hTowers);
	    }

	}

  // define sector boundaries and set initial boundary
  for ( Float_t x=0.0; x<=60.0; x+=5.0 ) {
    hTowers->GetListOfFunctions()->Add( new TLine( x, 0., x, 12.0 ) );
  }
  hTowers->GetXaxis()->SetRangeUser(0.,59.9);
  
  hTowers->Draw(opts);
  return hTowers;

}






TH2F *StEEmcDisplay::DrawLayer(Int_t layer, Option_t *opts)
{

  
  if ( layer==0 ) return DrawTowers(opts);
  if ( layer > 3 ) {
    LOG_WARN<<" Invalid layer passed to DrawLayer(...)"<<endm;
    return 0;
  }

  TCanvas *mycanvas = 0;//can[layer];

  if ( layer==1 )
    mycanvas=pre1;
  if ( layer==2 )
    mycanvas=pre2;
  if ( layer==3 )
    mycanvas=post;

  const Char_t *cnames[]={"eemc","pre1","pre2","post"};
  TH2F *his[]={hTowers,hPre1,hPre2,hPost};
  TH2F *myhist=his[layer];
  const Char_t *hnames[]={"hTowers","hPre1","hPre2","hPost"};

  // generate canvas to display EEMC tower information
  if ( !mycanvas ) {
    mycanvas=new TCanvas(TString(cnames[layer])+GetName(),GetTitle(),600,400);
    mycanvas->cd();  
    if ( layer==1 )pre1=mycanvas;
    if ( layer==2 )pre2=mycanvas;
    if ( layer==3 )post=mycanvas;
  }


  // draw and return if it already exists
  if ( myhist ) {
    myhist -> Draw(opts);
    return myhist;
  }


  const Char_t *titles[]={"Towers: ","Preshower 1: ", "Preshower 2: ", "Postshower: "};

  // create new histogram and fill it
  myhist = new TH2F(TString(hnames[layer])+GetName(),TString(titles[layer])+GetTitle(),62,-1.,61.,12,0.,12.);
  assert(myhist);
   
  // loop over all towers and set energy
  for ( Int_t sec=0;sec<kEEmcNumSectors;sec++ )
    for ( Int_t sub=0;sub<kEEmcNumSubSectors;sub++ )
      for ( Int_t eta=0;eta<kEEmcNumEtas;eta++ )
	{

	  Float_t x = 5*sec+sub; x+=0.5;
	  Float_t y = eta; y+=0.5;
	  Int_t bin = myhist->FindBin ( x, y );

	  if ( !mPrepostFail[sec][sub][eta][layer-1] )
	    myhist->SetBinContent( bin,1000.* mPrepostEnergy[sec][sub][eta][layer-1] );
	  else {
	    failTower(sec,sub,eta, myhist);
	    /* nada */ 
	  };

	  // wrap right hand edge
	  if ( sec==11 && sub==4 )
	    {
	      x=-0.5;
	      bin = myhist->FindBin ( x, y );

	      if ( !mPrepostFail[sec][sub][eta][layer-1] )
		myhist->SetBinContent( bin, 1000.* mPrepostEnergy[sec][sub][eta][layer-1] );
	      else
		failTower(0,-1,eta,myhist);

	    }

	  // wrap left hand edge
	  if ( sec==0 && sub==0 )
	    {
              x=60.5;
              bin = myhist->FindBin ( x, y );
              if ( !mPrepostFail[sec][sub][eta][layer-1] )
                myhist->SetBinContent( bin, 1000.*mPrepostEnergy[sec][sub][eta][layer-1] );
	      else
		failTower(12,5,eta,myhist);
	    }

	}

  // define sector boundaries and set initial boundary
  for ( Float_t x=0.0; x<=60.0; x+=5.0 ) {
    myhist->GetListOfFunctions()->Add( new TLine( x, 0., x, 12.0 ) );
  }
  myhist->GetXaxis()->SetRangeUser(0.,59.9);
  
  myhist->Draw(opts);
  return myhist;

}






TH2F *StEEmcDisplay::DrawPoints( Option_t *opts )
{

  TH2F *towers = DrawTowers();

  if ( !mPointMarker.size() )
    for ( UInt_t ii=0;ii<mPointKey.size();ii++ )
      {
	
	Float_t x=mPointX[ii];
	Float_t y=mPointY[ii];
	TVector3 position(x,y,kEEmcZSMD);
	
	Int_t sec,sub,eta;
	Float_t dphi,deta;
	if ( !geom->getTower( position, sec, sub, eta, dphi, deta ) ){
	  //LOG_WARN<<"************* tried to add point which missed endcap *************"<<endm;
	  position.Print();
	  return towers;
	}
	Float_t myphi = 0.5 + 5*sec + ((Float_t)sub) + 0.5*dphi;
	Float_t myeta = 0.5 + ((Float_t)eta) + 0.5*deta;
	
	TMarker *m=new TMarker( myphi, myeta, mPointStyle[ii] );
	m->SetMarkerColor( mPointColor[ii] );
	mPointMarker.push_back(m);

	TLatex *tex=new TLatex(  myphi+0.05, myeta, Form("uid=%i vid=%i",mPointUkey[ii],mPointVkey[ii]) );
	tex->SetTextColor( mPointColor[ii] );
	towers->GetListOfFunctions()->Add(tex);
	
      }

  for ( UInt_t ii=0;ii<mPointMarker.size();ii++ )
    {
      mPointMarker[ii]->Draw();
    }

  return towers;


}


// ----------------------------------------------------------------------------

TH1F *StEEmcDisplay::DrawSmd( Int_t sector, Int_t plane, Option_t *opts )
{

  const Char_t *name_planes[]={"smdu","smdv"};
  const Char_t *name_uv[]={"U","V"};

  // generate canvas to draw smd information
  TCanvas *mycanvas = (plane==0)? smdu : smdv;
  if ( !mycanvas )
    {
      mycanvas = new TCanvas(TString(name_planes[plane])+"-"+GetName(),GetTitle(),600,400);
      if ( plane==0 ) smdu=mycanvas; else smdv=mycanvas;
    }
  mycanvas->cd();


  // draw histogram and exit if it exists
  if ( hSmds[sector][plane] ) 
    {
      hSmds[sector][plane]->Draw(opts);
      return hSmds[sector][plane];
    }


  // create histogram 
  TString hname="h";
  if ( sector+1<10 ) hname+="0";
  hname+=(sector+1);
  hname+=name_uv[plane];
  hname+="::";
  hname+=GetName();
  TH1F *histo=new TH1F(hname,TString(GetTitle())+";index;Nmips",288,0.,288.);
  hSmds[sector][plane]=histo;

  for ( Int_t strip=0;strip<kEEmcNumStrips;strip++ )
    {
      Float_t energy = mStripEnergy[sector][plane][strip];
      if ( !mStripFail[sector][plane][strip] )
	histo->SetBinContent( strip+1, energy*mEnergy2Mip );
      else { /* nada */ }
    }

  hSmds[sector][plane]->Draw(opts);

  return hSmds[sector][plane];

}

// ----------------------------------------------------------------------------

TH1F *StEEmcDisplay::DrawClusters( Int_t sector, Int_t plane, Option_t *opts )
{

  Bool_t draw_stats = false;
  TString myopts=opts;
  if ( myopts.Contains("stats") )
    {
      myopts.ReplaceAll("stats","");
      opts=myopts.Data();
      draw_stats=true;
    }

  TH1F *hsmd = DrawSmd(sector, plane, opts );

  for ( UInt_t ii=0;ii<mClusterKey.size();ii++ )
    {
      if ( sector == mClusterSector[ii] &&
	   plane  == mClusterPlane[ii] )
	{
	  DrawCluster(ii,"same");
	}
    }

  
  if ( draw_stats )
    for ( UInt_t ii=0;ii<mClusterKey.size();ii++ )
      {
	if ( sector == mClusterSector[ii] &&
	     plane  == mClusterPlane[ii] )
	  {
	    
	    Int_t   key     = mClusterKey[ii];
	    Float_t energy  = mClusterEnergy[ii];
	    Float_t nmips   = energy * mEnergy2Mip;
	    Float_t mean    = mClusterMean[ii];
	    Float_t sigma   = mClusterSigma[ii];
	    Int_t   sector  = mClusterSector[ii];
	    Int_t   plane   = mClusterPlane[ii];
	    Int_t   color   = mClusterColor[ii];
	    Int_t   fill    = mClusterFill[ii];
	    TCanvas *c = (plane==0)? smdu : smdv;
	    c->cd();

	    Int_t imean=(Int_t)mean;
	    Float_t Iright = hSmds[sector][plane]->Integral( imean+6,imean+10 );
	    Float_t Ileft  = hSmds[sector][plane]->Integral( imean-10, imean-6);

	    Float_t xpave = mean + 6.0;
	    Float_t ypave = hSmds[sector][plane]->GetBinContent(imean+1);
	    if ( Iright > 0.4 * nmips && Ileft < 0.4 * nmips )
	      {
		xpave = mean - 25.0;
	      }

	    TPaveText *text=new TPaveText( xpave, ypave*0.7, xpave+20, ypave      );
	    text->SetFillColor( color );
	    text->SetFillStyle( fill );
	    text->AddText(Form("cluster id = %i",key));
	    text->AddText(Form("energy = %5.2f MeV",energy*1000.0));
	    text->AddText(Form("mean   = %5.1f", mean));
	    text->AddText(Form("sigma  = %5.3f", sigma));
	    if ( mClusterSplit[ii] )
	      text->AddText(Form("split cluster"));
	    //	    mClusterStats.push_back(text);
	    // text->Draw("same");
	    hsmd->GetListOfFunctions()->Add( text );
	    text->Draw();

	  }
      }


  return hsmd;

}

// ----------------------------------------------------------------------------


TH1F *StEEmcDisplay::DrawCluster( Int_t icluster, Option_t *opts )
{

  //Int_t sector = mClusterSector[ icluster ];
  Int_t plane  = mClusterPlane [ icluster ];

  assert( (plane==0)?smdu:smdv );

  if ( plane==0 ) smdu->cd();
  if ( plane==1 ) smdv->cd();

  // if cluster histograms are available then draw the specified histogram
  // and return
  if ( mClusterHisto.size() )
    {
      mClusterHisto[ icluster ]->Draw(opts);
      return mClusterHisto[icluster];
    }

  // create cluster histograms
  const Char_t *name_uv[]={"U","V"};
  for ( UInt_t ii=0;ii<mClusterKey.size();ii++ )
    {

      Int_t sector = mClusterSector[ ii ];
      Int_t plane  = mClusterPlane[ ii ];
      TString UV=name_uv[plane];

      LOG_INFO <<"+ cluster "
	       << ( (sector<9)?"0":"" )
	       << (sector+1) 
	       << UV
	       << " key=" << mClusterKey[ii]
	       << " col=" << mClusterColor[ii]
	       << " fill="<< mClusterFill[ii]
	       << endm;

      TString hname="h";
      if ( sector+1<10 ) hname+="0";
      hname+=(sector+1);
      hname+=name_uv[plane];
      hname+="key";
      hname+=mClusterKey[ii];
      hname+="::";
      hname+=GetName();
      hname+=ii;

      TH1F *histo = new TH1F(hname,GetTitle(),kEEmcNumStrips,0.,(Float_t)kEEmcNumStrips);
      //LOG_INFO<<"sec=" << sector<<" pl="<<plane<<" nstrips="<< mClusterStrips[ii].size()<<endm;
      for ( UInt_t jj=0;jj<mClusterStrips[ii].size();jj++ )
	{
	  Int_t index=mClusterStrips[ii][jj];
	  Float_t energy = mStripEnergy[ sector ][ plane ][ index ];
	  histo->SetBinContent( index+1, energy*mEnergy2Mip );
	}

      histo->SetFillColor( mClusterColor[ii] );
      histo->SetFillStyle( mClusterFill[ii] );

      mClusterHisto.push_back( histo );

    }


  mClusterHisto[icluster]->Draw(opts);

  return mClusterHisto[icluster];


}

// ----------------------------------------------------------------------------
void StEEmcDisplay::failTower( Int_t sec, Int_t sub, Int_t eta, TH2F *histo )
{
  Float_t x1=5*sec+sub;
  Float_t x2=x1+1.0;
  Float_t y1=eta;
  Float_t y2=eta+1;
  TLine *l1=new TLine(x1,y1,x2,y2);l1->SetLineColor(2);
  TLine *l2=new TLine(x2,y1,x1,y2);l2->SetLineColor(2);
  histo->GetListOfFunctions()->Add(l1);
      histo->GetListOfFunctions()->Add(l2);
}

// ----------------------------------------------------------------------------
Float_t StEEmcDisplay::sumEnergy(Int_t layer)
{

  if ( layer > 3 ) {
    LOG_WARN<<" sumEnergy not yet implemented for SMD'ss"<<endm;
    return -1.0;
  }

  Float_t sum = 0.;
  for ( Int_t sec=0;sec<12;sec++ )
    for ( Int_t sub=0;sub<5;sub++ )
      for ( Int_t eta=0;eta<12;eta++ )
	{
	  if ( layer==0 ) {
	    sum += mTowerEnergy[sec][sub][eta];
	  }
	  else {
	    sum += mPrepostEnergy[sec][sub][eta][layer-1];
	  }
	}

  if ( layer > 0 ) sum *= 1000.0;
  return sum;
  
}

Int_t StEEmcDisplay::hitMultiplicity(Int_t layer, Float_t threshold)
{

  if ( layer > 3 ) {
    LOG_WARN<<" sumEnergy not yet implemented for SMD'ss"<<endm;
    return -1;
  }

  Int_t sum = 0;
  for ( Int_t sec=0;sec<12;sec++ )
    for ( Int_t sub=0;sub<5;sub++ )
      for ( Int_t eta=0;eta<12;eta++ )
	{
	  if ( layer==0 ) {
	    if ( mTowerEnergy[sec][sub][eta] > threshold ) sum++;
	  }
	  else {
	    if ( mPrepostEnergy[sec][sub][eta][layer-1] *1000.0 > threshold ) sum++;
	  }
	}

  return sum;
  
}
