/**
 * \class StEEmcQAMaker
 * \brief Example of QA histograming using the StEEmcA2EMaker
 *
 * This maker produces some useful QA histograms using StEEmcA2EMaker
 *
 * \author Jason C. Webb
 * $Date: 2010/08/26 22:49:21 $
 * $Revision: 1.4 $
 *
 */
 
#include "StEEmcQAMaker.h"
ClassImp(StEEmcQAMaker);

#include "TH1F.h"
#include "TH2F.h"
#include "TString.h"

#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h" 
#include "StMuDSTMaker/COMMON/StMuTriggerIdCollection.h"

#include "StEEmcA2EMaker.h"

// ----------------------------------------------------------------------------
StEEmcQAMaker::StEEmcQAMaker( const Char_t *name ) : StMaker(name)
{
  nVertexMin =    1;
  nVertexMax =  999; 
  zVertexMin = -150;
  zVertexMax = +150;
  mSamplingFractionT=0.04;
  mSamplingFractionU=0.007;
  mSamplingFractionV=0.007;
  mSoftTrig = 0.;
}

// ----------------------------------------------------------------------------
Int_t StEEmcQAMaker::Init()
{
  hEventCounter = new TH1F("hEventCounter","Event counts",10,0.,10.);
  hEventCounter -> GetXaxis() -> SetBinLabel(1,"raw event");
  hEventCounter -> GetXaxis() -> SetBinLabel(2,"triggered");
  hEventCounter -> GetXaxis() -> SetBinLabel(3,"|vertex|<cut");
  hEventCounter -> GetXaxis() -> SetBinLabel(4,"track cuts");
  hEventCounter -> GetXaxis() -> SetBinLabel(5,"eemc response");


  hFrequencyT=new TH2F("hFrequencyT","Tower frequency",60,0.,60.,12,0.,12.);
  hFrequencyP=new TH2F("hFrequencyP","Pre1 frequency",60,0.,60.,12,0.,12.);
  hFrequencyQ=new TH2F("hFrequencyQ","Pre2 frequency",60,0.,60.,12,0.,12.);
  hFrequencyR=new TH2F("hFrequencyR","Post frequency",60,0.,60.,12,0.,12.);

  for ( UInt_t trig=0;trig<mTriggerList.size();trig++ )
    {
	TString hname="hTrigId";hname+=mTriggerList[trig];
	TString htitle="Triggers analysed per sector [0,12)";
	hTriggers.push_back(new TH1F(hname,htitle,12,0.,12.));

	hTriggersHard.push_back(new TH1F(hname+"hard",htitle,12,0.,12.));
    } 
  //    UInt_t trig=mTriggerList.size();
  hTriggers.push_back(new TH1F("hTrigOr","Any trigger in trigger list [0,12)",12,0.,12.));



  const Char_t *secname[]={
    "[0]","[1]","[2]","[3]","[4]","[5]","[6]","[7]","[8]","[9]",
    "[10]","[11]","Full","Cut"}; 


	    
  for ( Int_t sec=0;sec<14;sec++ )
    {
      
      TString hname="hEnergyDeposit";
      TString htitle="EM Energy [GeV] summed over towers in sector ";

      htitle+=secname[sec];
      hEnergyDepositT.push_back(new TH1F(hname+"T"+secname[sec],htitle,60,0.,60.));
      htitle="Energy deposit [MeV] summed over towers in sector ";
      hEnergyDepositP.push_back(new TH1F(hname+"P"+secname[sec],htitle,50,0.,200.));
      hEnergyDepositQ.push_back(new TH1F(hname+"Q"+secname[sec],htitle,50,0.,200.));
      hEnergyDepositR.push_back(new TH1F(hname+"R"+secname[sec],htitle,50,0.,200.));
      htitle.ReplaceAll("towers","smd strips"); 
      hEnergyDepositU.push_back(new TH1F(hname+"U"+secname[sec],htitle,50,0.,400.));
      hEnergyDepositV.push_back(new TH1F(hname+"V"+secname[sec],htitle,50,0.,400.));
      

      hname.ReplaceAll("EnergyDeposit","Multiplicity");
      htitle="Sector multiplicity ";htitle+=secname[sec]; 

      hMultiplicityT.push_back(new TH1F(hname+"T"+secname[sec],htitle,60,0.,60.)); 
      hMultiplicityP.push_back(new TH1F(hname+"P"+secname[sec],htitle,60,0.,60.)); 
      hMultiplicityQ.push_back(new TH1F(hname+"Q"+secname[sec],htitle,60,0.,60.)); 
      hMultiplicityR.push_back(new TH1F(hname+"R"+secname[sec],htitle,60,0.,60.)); 
      hMultiplicityU.push_back(new TH1F(hname+"U"+secname[sec],htitle,60,0.,60.)); 
      hMultiplicityV.push_back(new TH1F(hname+"V"+secname[sec],htitle,60,0.,60.)); 
      

      hname="hAdc"; 
      htitle="ADC-ped in ";htitle+=secname[sec]; 

      hAdcT.push_back(new TH1F(hname+"T"+secname[sec],htitle,512,0.,512.)); 
      hAdcP.push_back(new TH1F(hname+"P"+secname[sec],htitle,512,0.,512.)); 
      hAdcQ.push_back(new TH1F(hname+"Q"+secname[sec],htitle,512,0.,512.)); 
      hAdcR.push_back(new TH1F(hname+"R"+secname[sec],htitle,512,0.,512.)); 
      hAdcU.push_back(new TH1F(hname+"U"+secname[sec],htitle,512,0.,512.)); 
      hAdcV.push_back(new TH1F(hname+"V"+secname[sec],htitle,512,0.,512.)); 
      

      hname="hNglobal";hname+=secname[sec]; 
      hNglobal.push_back(new TH1F(hname,"Number of global tracks",50,0.,50.)); 
      hname="hNprimary";hname+=secname[sec]; 
      hNprimary.push_back(new TH1F(hname,"Number of primary tracks",50,0.,50.)); 
      hname="hNvertex";hname+=secname[sec];
      hNvertex.push_back(new TH1F(hname,"Number of primary vertices",10,0.,10.));
      hname="hZvertex";hname+=secname[sec];
      hZvertex.push_back(new TH1F(hname,"Z of 1st primary vertex",200,-200.,200.));	
      hname="hZvertexErr";hname+=secname[sec];
      hZvertexErr.push_back(new TH1F(hname,"Z err of 1st primary vertex",200,-200.,200.));	
      hname="hRankVertex";hname+=secname[sec];
      hRankVertex.push_back(new TH1F(hname,"Ranking of 1st primary vertex",100,0.,100.)); 
      hname="hNtrackVertex";hname+=secname[sec];  
      hNtrackVertex.push_back(new TH1F(hname,"Number of tracks used 1st prim vert",25,0.,25.)); 
      hname="hNtrackVertexEE";hname+=secname[sec];  
      hNtrackVertexEE.push_back(new TH1F(hname,"Number of tracks used 1st prim vert macthed to eemc",25,0.,25.)); 
      hname="hPTsumVertex";hname+=secname[sec];
      hPTsumVertex.push_back(new TH1F(hname,"Scalar pT sum of tracks used 1st prim vertex [GeV]",100,0.,100.)); 

    }


  return StMaker::Init();
}

// ----------------------------------------------------------------------------
Int_t StEEmcQAMaker::Make()
{

  /// We hav an event
  hEventCounter -> Fill( 0 );

  /// Verify that event is on the list of triggers
  if ( !CheckTriggers() ) return kStOK;
  hEventCounter -> Fill( 1 );

  /// Verify that the event is w/in vertex cuts
  if ( !CheckVertex() ) return kStOK;
  hEventCounter -> Fill( 2 );

  /// Do any track checking here
  if ( !CheckTracks() ) return kStOK;
  hEventCounter -> Fill( 3 );

  /// Handle EEmc response
  if ( !EEmcResponse() ) return kStOK;
  hEventCounter -> Fill ( 4 );


  return StMaker::Make();
}

// ----------------------------------------------------------------------------
void StEEmcQAMaker::analysis(const Char_t *name)
{
    mEEanalysis=(StEEmcA2EMaker *)GetMaker(name);
    assert(mEEanalysis);
}

// ----------------------------------------------------------------------------
void StEEmcQAMaker::mudst(const Char_t *name)
{
    mMuDst=(StMuDstMaker*)GetMaker(name);
    assert(mMuDst); 
} 

// ----------------------------------------------------------------------------
Bool_t StEEmcQAMaker::CheckTriggers()
{

    /// Get the event
    StMuEvent *event = mMuDst -> muDst() -> event(); 
    assert(event);

    mSectorTrigger = -1;

    /// Get the trigger id collection 
    StMuTriggerIdCollection tic = event -> triggerIdCollection();
    StTriggerId l1trig = tic.l1();

    /// Get the highest tower in the event
    /*
    StEEmcTower ht = mEEanalysis->hightower(); 
    if ( ht.fail() ) return false; 
    */
    StEEmcTower ht;
    Bool_t got_ht = false;
    for ( Int_t i=0;i<mEEanalysis->numberOfHitTowers(0);i++ )
      {
	StEEmcTower tow=mEEanalysis->hittower(i,0);
	if ( tow.et() > ht.et() && !(tow.fail()) ) 
	  {
	    ht=tow;
	    got_ht = true;
	  }
      }
    if ( !got_ht ) return false;

    /// Scan trigger list for high tower 
    Bool_t go=false;                

    if ( mTriggerList.size() == 0 ) go = true; // no triggers requested 
    for ( UInt_t i=0;i<mTriggerList.size();i++ )
    {
	Int_t myId = mTriggerList[i]; 
	if ( l1trig.isTrigger( myId ) && ht.et() > mSoftTrig ) {
	   go=true;
	   hTriggers[i] -> Fill ( ht.sector() );  
	   mSectorTrigger = ht.sector();
	} 
	if ( l1trig.isTrigger( myId ) ) {
	  hTriggersHard[i]->Fill( ht.sector() );
	}
    } 
    if ( go ) hTriggers[ mTriggerList.size() ] -> Fill( ht.sector() ); 

    return go; 
} 






// ----------------------------------------------------------------------------
Bool_t StEEmcQAMaker::CheckVertex()
{


    StMuEvent *event = mMuDst -> muDst() -> event();
    assert(event);
    Bool_t go = false;

    /// Verify that we had a trigger
    if ( mSectorTrigger < 0 ) return false;

    /// Get the highest tower in the event
    StEEmcTower ht = mEEanalysis->hightower();
    if ( ht.fail() ) return false;


    Int_t numberOfVertices=mMuDst -> muDst() -> numberOfPrimaryVertices(); 
    hNvertex[ mSectorTrigger ] -> Fill( numberOfVertices );
    hNvertex[ 12 ] -> Fill( numberOfVertices ); 
    if ( numberOfVertices < nVertexMin || 
         numberOfVertices > nVertexMax ) return false; 
    hNvertex[ 13 ] -> Fill( numberOfVertices ); 

    /// If we reach this point with no vertices, the user has specified
    /// a minimum number of vertices == 0.  
    if ( !numberOfVertices ) return true;  

    /// Get the first primary vertex and check against cut  
    StMuPrimaryVertex *vertex = mMuDst -> muDst() -> primaryVertex(0); 
    Float_t zvertex=vertex->position().z(); 
    hZvertex[ mSectorTrigger ] -> Fill ( zvertex );
    hZvertex[ 12 ] -> Fill( zvertex );
    if ( zvertex < zVertexMin || zvertex > zVertexMax ) return false; 
    hZvertex[ 13 ] -> Fill( zvertex );


    Float_t ezvertex = vertex->posError().z();
    hZvertexErr[ mSectorTrigger ] -> Fill( ezvertex );
    hZvertexErr[ 12          ] -> Fill( ezvertex );


    /// Vertex "likelyhood" ranking 
    Float_t rank = vertex -> ranking();
    hRankVertex[ mSectorTrigger ] -> Fill ( rank );
    hRankVertex[ 12 ] -> Fill ( rank ); 


   /// Number of tracks associated with vertex 
    Int_t ntrack = vertex -> nTracksUsed();
    hNtrackVertex[ mSectorTrigger ] -> Fill( ntrack );
    hNtrackVertex[ 12 ] -> Fill( ntrack );


    ntrack = vertex -> nEEMCMatch();
    hNtrackVertexEE[ mSectorTrigger ] -> Fill( ntrack );
    hNtrackVertexEE[ 12 ] -> Fill( ntrack ); 


   /// Scalar pT sum
    Float_t ptsum = vertex -> sumTrackPt();
    hPTsumVertex[ mSectorTrigger ] -> Fill( ptsum );
    hPTsumVertex[ 12 ] -> Fill( ptsum ); 

    return go; 
}


// ----------------------------------------------------------------------------
Bool_t StEEmcQAMaker::CheckTracks()
{

    /// Get the highest tower in the event
    StEEmcTower ht = mEEanalysis->hightower();
    if ( ht.fail() ) return false;

    if ( mSectorTrigger < 0 ) return false;

    /// Number of global tracks
    Int_t nGlobal=mMuDst->muDst()->numberOfGlobalTracks();
    hNglobal[ mSectorTrigger ] -> Fill( nGlobal );
    hNglobal[ 12          ] -> Fill( nGlobal ); 

    /// Number of primary tracks
    Int_t nPrimary=mMuDst->muDst()->numberOfPrimaryTracks(); 
    hNprimary[ mSectorTrigger ] -> Fill( nPrimary );
    hNprimary[ 12          ] -> Fill( nPrimary ); 

    return true; 
} 






// ----------------------------------------------------------------------------
Bool_t StEEmcQAMaker::EEmcResponse()
{
 
  Int_t mysector = mSectorTrigger;
       
    /// Get the highest tower in the event
    StEEmcTower ht = mEEanalysis->hightower();
    if ( ht.fail() ) return false;

    if ( mysector < 0 ) return false;

    //    Int_t ht_sec = ht.sector();
    Int_t ht_phi = ht.phibin();
    Int_t ht_eta = ht.etabin();

    /// high tower frequency 
    hFrequencyT -> Fill( ht_phi, ht_eta ); 

    StEEmcTower pre1 = mEEanalysis->hightower(1);
    StEEmcTower pre2 = mEEanalysis->hightower(2);
    StEEmcTower post = mEEanalysis->hightower(3);
    hFrequencyP -> Fill ( pre1.phibin(), pre1.etabin() );
    hFrequencyQ -> Fill ( pre2.phibin(), pre2.etabin() );
    hFrequencyR -> Fill ( post.phibin(), post.etabin() );

    /// Compute energy deposit in 6 layers of the endcap 
    Float_t energy_deposit[6];
    for ( Int_t i=0;i<6;i++ )
    {
	energy_deposit[i] = mEEanalysis->energy( mysector, i );
    } 
    /// Correct for sampling fraction 
    //    energy_deposit[0] *= mSamplingFractionT * 1000.0;
    for ( Int_t i=1;i<6;i++ )
      energy_deposit[i] *= 1000.0;

    hEnergyDepositT[mysector]->Fill( energy_deposit[0] ); 
    hEnergyDepositP[mysector]->Fill( energy_deposit[1] ); 
    hEnergyDepositQ[mysector]->Fill( energy_deposit[2] ); 
    hEnergyDepositR[mysector]->Fill( energy_deposit[3] ); 
    hEnergyDepositU[mysector]->Fill( energy_deposit[4] ); 
    hEnergyDepositV[mysector]->Fill( energy_deposit[5] ); 

    hMultiplicityT[mysector]->Fill( mEEanalysis->numberOfHits(mysector,0) ); 
    hMultiplicityP[mysector]->Fill( mEEanalysis->numberOfHits(mysector,1) ); 
    hMultiplicityQ[mysector]->Fill( mEEanalysis->numberOfHits(mysector,2) ); 
    hMultiplicityR[mysector]->Fill( mEEanalysis->numberOfHits(mysector,3) ); 
    hMultiplicityU[mysector]->Fill( mEEanalysis->numberOfHits(mysector,4) ); 
    hMultiplicityV[mysector]->Fill( mEEanalysis->numberOfHits(mysector,5) ); 

    for ( Int_t sub=0;sub<5; sub++ )
	for ( Int_t eta=0;eta<12;eta++ )
	{
	    StEEmcTower t=mEEanalysis->tower(mysector,sub,eta,0);
	    if ( t.adc()>0. && !t.fail() ) hAdcT[ mysector ] -> Fill ( t.adc() );
	    t=mEEanalysis->tower(mysector,sub,eta,1);
	    if ( t.adc()>0. && !t.fail() ) hAdcP[ mysector ] -> Fill ( t.adc() );
	    t=mEEanalysis->tower(mysector,sub,eta,2);
	    if ( t.adc()>0. && !t.fail() ) hAdcQ[ mysector ] -> Fill ( t.adc() );
	    t=mEEanalysis->tower(mysector,sub,eta,3);
	    if ( t.adc()>0. && !t.fail() ) hAdcR[ mysector ] -> Fill ( t.adc() );
	}
    for (Int_t hit=0;hit<mEEanalysis->numberOfHitStrips(mysector,0);hit++ )
    {
	StEEmcStrip strip=mEEanalysis->hitstrip(mysector,0,hit); 
	if ( strip.fail() || strip.energy()<=0. ) continue;
	hAdcU[mysector]->Fill( strip.adc() );
    }

    for (Int_t hit=0;hit<mEEanalysis->numberOfHitStrips(mysector,1);hit++ )
    {
	StEEmcStrip strip=mEEanalysis->hitstrip(mysector,1,hit); 
	if ( strip.fail() || strip.energy()<=0. ) continue;
	hAdcV[mysector]->Fill( strip.adc() );
    }


    return true;
} 
