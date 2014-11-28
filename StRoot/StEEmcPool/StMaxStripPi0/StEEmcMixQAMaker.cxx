#include "StEEmcMixQAMaker.h"
#include "StEEmcMixMaker.h"
#include "StEEmcPointMaker.h"
#include "TH1F.h"
#include "TH2F.h"
#include <stdio.h>
#include <iostream>

ClassImp(StEEmcMixQAMaker);

// ----------------------------------------------------------------------------
StEEmcMixQAMaker::StEEmcMixQAMaker(const Char_t *name):StMaker(name)
{

  /// Default cuts
  maxPerSector  = 100; // eg no cut
  maxPerEvent   = 100; // eg no cut
  maxPerCluster = 2;   // default number of points associated with 6-18 towers 
                       // which make up the pi0 candidate's energy

  zVertexMin=-150.0;
  zVertexMax=  50.0;

  /// pT binning for mass spectra
  mBins.push_back(0.);  
  mBins.push_back(1.0);
  mBins.push_back(2.0);
  mBins.push_back(3.0);
  mBins.push_back(4.0);
  mBins.push_back(6.0);
  mBins.push_back(8.0);
  mBins.push_back(10.0);
  mBins.push_back(15.0);
  mBins.push_back(20.0);  
  mBins.push_back(200.0);

  mBackground = false;

}

// ----------------------------------------------------------------------------
Int_t StEEmcMixQAMaker::Init()
{


  hNcandidates = new TH1F("hNcandidates","Number of pairs",30,0.,30.);

  hMassRall=new TH1F("hMassRall","Dipoint invariant mass, integrated",360,0.,3.6);
  hZvertexRall=new TH1F("hZvertexRall","Event vertex",150,-150.,150.);

  /// Book histograms for Y vs X of pi0, gammas and E1 vs E2
  for ( Int_t sec=0;sec<13;sec++ )
    {
      TString name="hYXpair";name+=sec+1;
      TString title="Y vs X [cm] of stable pi0, sector ";title+=sec+1;
      hYXpair.push_back(new TH2F(name,title,250,-250.,250.,250,-250.,250.));
      name="hYXhigh";name+=sec+1;
      title="Y vs X [cm] of higher energy gamma, sector ";title+=sec+1;
      hYXhigh.push_back(new TH2F(name,title,250,-250.,250.,250,-250.,250.));
      name.ReplaceAll("high","low");
      title.ReplaceAll("high","low");
      hYXlow.push_back(new TH2F(name,title,250,-250.,250.,250,-250.,250.));
      name="hE1E2sec";name+=sec+1;
      title="Energy point1 vs Energy point2 [GeV], sector ";title+=sec+1;
      hE1E2.push_back(new TH2F(name,title,100,0.,50.,100,0.,50.));
    }



  /// Book histograms for pi0 kinematics binned in pT for each sector
  for ( Int_t sec=0;sec<13;sec++ )
    {
      
      /// init our dynamic array of histograms
      std::vector<TH1F *> tmp;
      hMassR.push_back( tmp );
      hZvertexR.push_back( tmp );
      hZggR.push_back( tmp );
      hPhiggR.push_back( tmp );
      hEnergyR.push_back( tmp );

      TString sec_name = "-sec";sec_name+=sec+1;
      for ( UInt_t ptbin=0;ptbin<mBins.size()-1;ptbin++ )
	{
	  
	  TString bin_name = "-bin";bin_name+=ptbin;
	  
	  TString hname="hMassR";hname+=sec_name;hname+=bin_name;
	  TString htitle="Dipoint invariant mass, sector=";htitle+=sec+1;
	  htitle+=", ptbin="; htitle+=(Int_t)ptbin;
	  hMassR[sec].push_back( new TH1F(hname,htitle,360,0.,3.60) );

	  hname="hZvertexR";hname+=sec_name;hname+=bin_name;
	  htitle="Event vertex";htitle+=sec+1;
	  htitle+=", ptbin="; htitle+=(Int_t)ptbin;
	  hZvertexR[sec].push_back(new TH1F(hname,htitle,150,-150.,150.));

	  hname="hZggR";hname+=sec_name;hname+=bin_name;
	  htitle="Zgg = |E1-E2|/E, sector= ";htitle+=sec+1; htitle+=", ptbin=";htitle+=(Int_t)ptbin;
	  hZggR[sec].push_back(new TH1F(hname,htitle,50,0.,1.));	  	 
	  
	  hname="hPhiggR";hname+=sec_name;hname+=bin_name;
	  htitle="Opening angle, sector=";htitle+=sec+1;htitle+=", ptbin=";htitle+=(Int_t)ptbin;
	  hPhiggR[sec].push_back(new TH1F(hname,htitle,50,0.,0.1));	 

	  hname="hEnergyR";hname+=sec_name;hname+=bin_name;
	  htitle+="Energy, sector=";htitle+=sec+1;htitle+=", ptbin=";htitle+=(Int_t)ptbin;
	  hEnergyR[sec].push_back(new TH1F(hname,htitle,50,0.,50.));
	  
	}

      /// one bin integrated over full pt range
      TString hname="hMassR";hname+=sec_name;hname+="-unbinned";
      TString htitle="Dipoint invariant mass, sector=";htitle+=sec+1;
      hMassR[sec].push_back( new TH1F(hname,htitle,360,0.,3.6) );
			     
      hname="hZvertexR";hname+=sec_name;hname+="-unbinned";
      htitle="Event vertex, sector=";htitle+=sec+1;
      hZvertexR[sec].push_back(new TH1F(hname,htitle,150,-150.,150.));

      hname="hZggR";hname+=sec_name;hname+="-unbinned";
      htitle="Zgg = |E1-E2|/E, sector=";htitle+=sec+1;
      hZggR[sec].push_back(new TH1F(hname,htitle,50,0.,1.));
      
      hname="hPhiggR";hname+=sec_name;hname+="-unbinned";
      htitle="Opening angle, sector=";htitle+=sec+1;
      hPhiggR[sec].push_back(new TH1F(hname,htitle,50,0.,0.1));

      hname="hEnergyR";hname+=sec_name;hname+="-unbinned";
      htitle+="Energy, sector=";htitle+=sec+1;
      hEnergyR[sec].push_back(new TH1F(hname,htitle,50,0.,50.));
    }


      
  return StMaker::Init();
}

// ----------------------------------------------------------------------------
Int_t StEEmcMixQAMaker::Make()
{

  ///
  /// Sort pairs by sector
  ///
  std::vector< StEEmcPairVec_t > pairs;
  for ( Int_t sec=0;sec<12;sec++ )
    {
      StEEmcPairVec_t tmp;
      pairs.push_back(tmp);
    }

  hNcandidates -> Fill( mEEmixer -> numberOfCandidates() );


  ///
  /// Use standard list of real points
  ///
  if ( !mBackground ) 
    {

      /// Maximum pairs per event
      if ( mEEmixer -> numberOfCandidates() > maxPerEvent ) return kStOK;

      for ( Int_t i=0;i<mEEmixer->numberOfCandidates();i++ )
	{
	  
	  StEEmcPair p = mEEmixer -> candidate(i);
	  pairs[ p.point(0).sector() ].push_back(p);
	  
	}

    }
  ///
  /// Use mixed combinatoric points
  ///
  else
    {

      /// Maximum pairs per event
      if ( mEEmixer -> numberOfMixedCandidates() > maxPerEvent ) return kStOK;

      for ( Int_t i=0;i<mEEmixer->numberOfMixedCandidates();i++ )
	{
	  
	  StEEmcPair p = mEEmixer -> mixedCandidate(i);
	  pairs[ p.point(0).sector() ].push_back(p);
	  
	}

    }
  




  ///
  /// Loop over all sectors and fill QA histograms
  ///
  for ( UInt_t sec=0;sec<12;sec++ )
    {

      /// Maximum pairs per sector
      if ( pairs[sec].size() > (UInt_t)maxPerSector ) continue;


      for ( UInt_t i=0;i<pairs[sec].size();i++ )
	{
	  
	  StEEmcPair pair = pairs[sec][i];

	  /// Maximum points per cluster
	  if ( !twoBodyCut( pair ) ) continue;

	  /// Find pt bin
	  Int_t bin = ptbin( pair );
	  std::cout << "sector=" << sec << " mass=" << pair.mass() << " pt = " << pair.pt() << " bin=" << bin << std::endl;
	  if ( bin < 0 ) continue;

	  /// verify vertex cut
	  if ( pair.vertex().Z() < zVertexMin ||
	       pair.vertex().Z() > zVertexMax ) continue;

	  /// Fill mass
	  hMassR[sec][bin] -> Fill( pair.mass() );
	  hMassR[ 12][bin] -> Fill( pair.mass() );
	  hMassRall -> Fill( pair.mass() );
	  
	  hMassR[sec].back() -> Fill( pair.mass() ); // unbinned
	  hMassR[ 12].back() -> Fill( pair.mass() ); // int over sectors

	  /// Gated quantities
	  if ( pair.mass() > mMin && pair.mass() <= mMax )
	    {

	      /// pt binned
	      hZvertexR[sec][bin] -> Fill( pair.vertex().Z() );
	      hZvertexR[ 12][bin] -> Fill( pair.vertex().Z() );
	      hZvertexRall -> Fill( pair.vertex().Z() );
	      
	      hZvertexR[sec].back() -> Fill( pair.vertex().Z() ); // unbinned
	      hZvertexR[ 12].back() -> Fill( pair.vertex().Z() ); // int sect

	      /// pt integrated

	      StEEmcPoint point1=pair.point(0);
	      StEEmcPoint point2=pair.point(1);

	      TVector3 pos1 = point1.position();
	      TVector3 pos2 = point2.position();

	      Float_t e1=point1.energy();
	      Float_t e2=point2.energy();
	      
	      TVector3 posp = ( e1 * pos1 + e2 * pos2 ) * ( 1.0/(e1+e2) );

	      hYXpair[sec] -> Fill( posp.X(), posp.Y() );
	      hYXhigh[sec] -> Fill( pos1.X(), pos1.Y() );
	      hYXlow[sec]  -> Fill( pos2.X(), pos2.Y() );
	      hE1E2[sec]   -> Fill( e2, e1 );

	      hYXpair[ 12] -> Fill( posp.X(), posp.Y() );
	      hYXhigh[ 12] -> Fill( pos1.X(), pos1.Y() );
	      hYXlow[ 12]  -> Fill( pos2.X(), pos2.Y() );
	      hE1E2[ 12]   -> Fill( e2, e1 );
	      
	    }
	  
	}

    }

  return kStOK;
}


// ----------------------------------------------------------------------------
Int_t StEEmcMixQAMaker::ptbin( StEEmcPair pair )
{
  for ( UInt_t i=0;i<mBins.size()-1;i++ )
    {
      if ( pair.pt() > mBins[i] && pair.pt() <= mBins[i+1] ) return (Int_t)i;
    }
  return -1;
}

// ----------------------------------------------------------------------------
void StEEmcMixQAMaker::mixer(const Char_t *name, Float_t min, Float_t max)
{
  mEEmixer=(StEEmcMixMaker *)GetMaker(name);
  assert(mEEmixer);// please specify a valid mix maker
  mMin=min;
  mMax=max;
  assert(max>min && max>0.); // you gotta be kidding, right?
}

// ----------------------------------------------------------------------------
void StEEmcMixQAMaker::points(const Char_t *name)
{
  mEEpoints=(StEEmcPointMaker *)GetMaker(name);
  assert(mEEpoints);
}

// ----------------------------------------------------------------------------
Bool_t StEEmcMixQAMaker::twoBodyCut( StEEmcPair pair )
{

  /// Obtain the 6-18 towers which contribute energy to this pair
  
  Bool_t towers[720]; for (Int_t i=0;i<720;i++ ) towers[i]=false; 
  StEEmcTower t1 = pair.point(0).tower(0);
  StEEmcTower t2 = pair.point(1).tower(0);
  
  for ( Int_t i=0;i<t1.numberOfNeighbors();i++ ) {
    StEEmcTower mytow=t1.neighbor(i);
    towers[ mytow.index() ] = true;
  }
  for ( Int_t i=0;i<t2.numberOfNeighbors();i++ ) {
    StEEmcTower mytow=t2.neighbor(i);
    towers[ t2.neighbor(i).index() ] = true;
  }


  towers[ t1.index() ] = true;
  towers[ t2.index() ] = true;

  /// NOTE: we could consider adding the next-nearest neighbors as well
  
  /// Loop over all points in the endcap and count the number which
  /// match one of the specified towers


  Int_t count = 0;
  for ( Int_t i=0;i<mEEpoints->numberOfPoints();i++ )
    {
      StEEmcPoint p=mEEpoints->point(i);
      StEEmcTower t=p.tower(0);      
      if ( towers[ t.index() ] ) count++;
    }

  return ( count <= maxPerCluster );

}
