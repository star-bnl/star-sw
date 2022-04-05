/**
 
\class StEEmcIUPi0Analysis
\brief A maker for creating pi0 histograms
\\Weihong He
    The StEEmcIUPi0Analysis takes as input the list of pi0 candiates 
provided by StEEmcMixMaker, filters out pairs depending on user-specified
cuts, then spin sorts the events into a number of histograms.  These
histograms are stored in TDirectory's underneath the .hist dataset. 
The user may specify cuts in the SpinCuts object (accessible through
a call to cuts().) 

*/
// For real data analyses, you need to switch off #define EFFICIENCY.
//#define EFFICIENCY
#include "StEEmcIUPi0Analysis.h"
#include "StEEmcIUMixMaker.h"
#include "StEEmcIUPointMaker.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TH2D.h"
#include "TTree.h" 
#include "TFile.h" 
#include <stdio.h>
#include <iostream>
#include "SpinCutsIU.h" 
#include "StEEmcPool/StEEmcPointMaker/EEmcSectorFit.h" 
#include "StEEmcUtil/StEEmcSmd/EEmcSmdGeom.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
#include "StEEmcPool/StRFEmcTrigger/StRFEmcTrigMaker.h"
#include "StEEmcPool/StEEmcA2EMaker/StEEmcA2EMaker.h" 
#include "StMcOutputMaker.h"
ClassImp(StEEmcIUPi0Analysis);

// ----------------------------------------------------------------------------
StEEmcIUPi0Analysis::StEEmcIUPi0Analysis(const Char_t *name):StMaker(name)
{
    mCuts=new SpinCutsIU(); 
    mTrigSim = 0;
    mTrigSimThreshold = 0;
    mEEgeom=new EEmcGeomSimple();
    mSpinSort = true;
    
}

// ----------------------------------------------------------------------------
Int_t StEEmcIUPi0Analysis::Init()
{

  // book histograms for pi0 analysis
  mHistograms[ kAny  ] = new SpinIUHistos("Any", "Pi0 spectra, unsorted");
  // book histograms for pi0 analysis
  mBackgrounds[ kAny  ] = new SpinIUHistos("AnyB", "combinatoric spectra, unsorted");
  hPi0Mass=new TH1F("hMass","invariant mass in windows [0.08,0.18]Gev and DeltaR<0.04",120,0.,1.2);
  hPTvsPiNum = new TH1F("ReconPt",  "Recon PT",50,0.,25. );
  hMcEta = new TH1F("McEta", "Mc Eta",50,0.,2.5 );
  hEEmcEta = new TH1F("EEmcEta", "EEmc Eta",50,0.,2.5 );
  hMcPhi = new TH1F("McPhi", "Mc Phi",360,-180.,180. );
  hMcPt = new TH1F("McPt", "Mc Pt",50,0.,25. );
  hMcEnergy=new TH1F("McEnergy","Mc energy for recon pi0",80,0.,80.);
  hREtaPhi=new TH2F("REEmcEtaPhi", "Reconstructed pi0 track; Recon phi / deg;ReconEta",360,-180.,180,20,1.,2.);
  hReconEta = new TH1F("ReconEta", "Reconstructed Eta",50,0.,2.5 );
  hReconPhi = new TH1F("ReconPhi", "Recon Phi",360,-180.,180. );
  hRecoEnergy = new TH1F("ReconEnergy","Reconstructed Pi0 Energy",60,0.,60.);
  hResoEta = new TH1F("ResoEta", "Resolution of Eta: ReconEta - EEmcEta",10,-1.,1. );
  hResoPhi = new TH1F("ResoPhi", "Resolution of Phi: ReconPhi - McPhi",20,-20.,20. );
  hResoPt = new TH1F("ResoPt", "Resolution of Pt",160,-4.,4. );
  hResoEnergy=new TH1F("ResoEnergy", "Resolution of Energy",200,-10.,10.);
  hResoPtvsG = new TH2F("ResoPtvsG", "Resolution of Pt; mcPt-ReconPt;mcPt",80,-4.,4.,250,0.,25.);
  hResoEnergyvsG = new TH2F("ResoEnergyvsG", "Resolution of Energy; mcEnergy-ReconEnergy;mcEnergy",200,-10.,10.,80,0.,80.);
  hRealMcPD= new TH1F("RealMcdistance","Distance between real and MC pi0",100,0.,0.5);
  hRealMcPR= new TH1F("RRealMcdistance","Distance between real and MC pi0 in the EEmc detector",100,0.,0.5);
  mHistograms[1]=new SpinIUHistos("PP","Pi0 spectra, PP, spin4=dec5");
  mHistograms[2]=new SpinIUHistos("PN","Pi0 spectra, PN, spin4=dec6");
  mHistograms[3]=new SpinIUHistos("NP","Pi0 spectra, NP, spin4=dec9");
  mHistograms[4]=new SpinIUHistos("NN","Pi0 spectra, NN, spin4=dec10");
  EventCounter=new TH1F("EventCounter","Event counter",10,0.,10.);
  hEventCounter=new TH1F("hEventCounter","Event counts",10,0.,10.);
  hEventCounter -> GetXaxis() -> SetBinLabel(1,"raw");
  hEventCounter -> GetXaxis() -> SetBinLabel(2,"minb");
  hEventCounter -> GetXaxis() -> SetBinLabel(3,"hw trg");
  hEventCounter -> GetXaxis() -> SetBinLabel(5,"sw trg");
  hEventCounter -> GetXaxis() -> SetBinLabel(5,"sw trg[3,6]");
  hEventCounter -> GetXaxis() -> SetBinLabel(6,"vertex");
  hPairCounter  = new TH1F("hPairCounter","Pair counts",10,0.,10.);
  hPi0Counter  = new TH1F("hPi0Counter","Pi0 counts",10,0.,10.);
  hFillPatternI = new TH1F("hFillPatternI","Intended fill pattern:bunch X-ing @ STAR IP:n runs",120,0.,120.);
  hSpin4        = new TH1F("hSpin4","Spin 4:spin4 state",11,0.,11.);
  hSpin4mb      = new TH1F("hSpin4mb","Spin 4:spin4 state",11,0.,11.);
  hBx7          = new TH1F("hBx7","7-bit bunch crossing:bx7",120,0.,120.);
  hBx48         = new TH1F("hBx48","48-bit bunch crossing:bx48",120,0.,120.);
  hBx7diffBx48  = new TH2F("hBx7diffBx48","bx1=(bx7-off7)%120, bx2=(bx48-off48)%120;bx7;bx1-bx2",120,0.,120.,21,-10.5,10.5);
  hBxStar       = new TH1F("hBxStar","Beam x-ing at STAR IP;star bunch x-ing",120,0.,120.); 
  hBxStarPi0    = new TH1F("hBxStarPi0","Beam x-ing at STAR IP with pi0 detected;star bunch x-ing",120,0.,120.); 
  hMassBx=new TH2F("hMassBx","Beam x-ing vs Mass;M [GeV];STAR bunch xing",120,0.,1.2,120,0.,120.); 
  hZvertBx=new TH2F("hZvertBx","Beam x-ing vs Z vertex [0.1,0.18]",150,-150.,150.,120,0.,120.); 
  hZggBx=new TH2F("hZggBx","Beam x-ing vs Zgg",100,0.,1.,120,0.,120.);
  hEtaBx=new TH2F("hEtaBx","Beam x-ing vs eta",120,0.8,2.2,120,0.,120.); 
  DEtaMass=new TH2F("DEtaMass","Delta Eta of the two points vs Mass of the reconstructed pi0",60,0.,0.6,20,0.,0.1);
  DPhiMass=new TH2F("DPhiMass","Delta Phi of the two points vs Mass of the reconstructed pi0",60,0.,0.6,20,0.,0.1);
  dUvsdV=new TH2D("dUvsdV","Seed distance between smd clusters; dU; dV",10,0,10,10,0,10);
  dUvsdVGood=new TH2D("dUvsdVGood","Good Seed distance between smd clusters; dU; dV",10,0,10,10,0,10);
  dUvsRatio=new TH2F("dUvsRatio","Ucluster ratio along with seed distance;dU;Ratio",20,0.,20.,10,0.,1.);
  dVvsRatio=new TH2F("dVvsRatio","Vcluster ratio along with seed distance;dV;Ratio",20,0.,20.,10,0.,1.);
  dUvsRatioGood=new TH2F("dUvsRatioGood","Good Ucluster ratio along with seed distance;dU;Ratio",20,0.,20.,10,0.,1.);
  dVvsRatioGood=new TH2F("dVvsRatioGood","Good Vcluster ratio along with seed distance;dV;Ratio",20,0.,20.,10,0.,1.);
  dUVzeroE=new TH1F("dUVzeroE","Pi0 energy with dU or dV=0",60,0.,60);
  dUVzeroEta=new TH1F("dUVzeroEta","Pi0 Eta with dU or dV=0",20,1.,2.);
  GoodPiGeo=new TH2F("GoodPiGeo","Pi0 distribution in EEMC with mass range [0.08,0.18]Gev;x;y",120,-240.,240.,120,-240.,240.);
  GoodPiPt=new TH1F("GoodPiPt",  "Recon PT",50,0.,25.);
  mRealEvent=new StEEmcIUMixEvent();
  mMixedEvent=new StEEmcIUMixEvent(); 
  mRealTree=new TTree("mRealTree","Real Events");
  mRealTree->Branch("MixEvent","StEEmcIUMixEvent",&mRealEvent);
  mMixedTree=new TTree("mMixedTree","Mixed Events");
  mMixedTree->Branch("MixEvent","StEEmcIUMixEvent",&mMixedEvent); 

  AddObj( mRealTree, ".hist" );
  AddObj( mMixedTree, ".hist" ); 

  return StMaker::Init(); 
}
// ----------------------------------------------------------------------------
Int_t StEEmcIUPi0Analysis::InitRun(Int_t run)
{
  
  mSpinDb->print(0); // 0=short, 1=huge
  Info("InitRun","run number = %d",run);
  const Int_t *spin8bits=mSpinDb->getSpin8bits();
  for(int bx=0;bx<120;bx++){
    Bool_t isFilled=(spin8bits[bx] & 0x11)==0x11;
    if(isFilled) hFillPatternI->Fill(bx);
  }

  mRunNumber = run;
  
  return StMaker::InitRun(run);

}
// ----------------------------------------------------------------------------
Int_t StEEmcIUPi0Analysis::Make()
{

  /// Determine spin state.
  EventCounter->Fill(0.,1.);
  Int_t spin4 = -1;
  Int_t bxStar = -1; 
  if ( mSpinDb -> isValid() && mSpinDb -> isPolDirLong() ) 
    spin4=getSpinState( mMuDst->muDst(), bxStar );


  hBxStar -> Fill( bxStar ); 

  mRealEvent -> setEvent( mMuDst->muDst()->event() ); 
  mRealEvent -> setSpin4( spin4 ); 
  mMixedEvent -> setEvent( mMuDst->muDst()->event() ); 
  mMixedEvent -> setSpin4( spin4 ); 

  for ( Int_t ii=0;ii<720;ii++ ) {
    StEEmcTower tower=mEEanalysis->tower(ii);
    if ( tower.fail() ) continue;
    mRealEvent->mADC[ii]=(60./4096.)*tower.adc();///meh
    mRealEvent->mStat[ii]=tower.stat();
  }


  /// Check trigger 
  
  if ( !accept( mMuDst->muDst() ) ) goto END_OF_MAKE;
  EventCounter->Fill(2.,1.);
  hPairCounter -> Fill( kEvent ); 
 
  
  /// map spin decimal bits to histograms
  static Int_t mymap[]={0,0,0,0,0,1,2,0,0,3,4};

  /// event energy sums
  mRealEvent -> mTotalEnergyT = mEEanalysis -> energy(0); 
  mRealEvent -> mTotalEnergyP = mEEanalysis -> energy(1); 
  mRealEvent -> mTotalEnergyQ = mEEanalysis -> energy(2); 
  mRealEvent -> mTotalEnergyR = mEEanalysis -> energy(3); 
  mRealEvent -> mTotalEnergyU = mEEanalysis -> energy(4); 
  mRealEvent -> mTotalEnergyV = mEEanalysis -> energy(5); 

  /// Loop over all candidate pairs
  numcutCan=0;
  numoacceptedpair=0;

  
  
  for ( Int_t i=0;i<mEEmixer->numberOfCandidates();i++ )
    {
     
      StEEmcIUPair pair = mEEmixer->candidate(i);
      hPairCounter -> Fill( kPair ); 
     
      if ( !accept( pair ) ) continue; 
     
      mRealEvent -> addPair( pair ); 
      numoacceptedpair++;
      
      //std::cout << "spin4=" << spin4 << " ";
      //pair.print();
     
      mHistograms[ kAny ] -> Fill( pair );
      //assert(mySputMk);
      StEEmcIUPoint point1=pair.point(0);
      StEEmcIUPoint point2=pair.point(1);
      //int sector=point1.sector(); 
      TVector3 point1pos=point1.position();
      TVector3 point2pos=point2.position();
      float deta=fabs(point1pos.Eta()-point2pos.Eta());
      float dphi=fabs(point1pos.Phi()-point2pos.Phi());
      DEtaMass->Fill(pair.mass(),deta);
      DPhiMass->Fill(pair.mass(),dphi);
      //calculate detector Eta
      float Rxy=TMath::Sqrt(pair.vertex().x()*pair.vertex().x()+pair.vertex().y()*pair.vertex().y());
      float hHeight=pair.pt()*(270.0-pair.vertex().Z())/pair.pz()+Rxy;
      float etatheta=TMath::ATan(hHeight/270.0);
      //printf("accept pz=%f\n",pair.pz());
      float mideta=tan(etatheta/2.0);
      float eemceta=-log(mideta);
      hEEmcEta->Fill(eemceta);

      float EsmallU=(point1.cluster(0).energy()<point2.cluster(0).energy())?point1.cluster(0).energy():point2.cluster(0).energy();
      float ElargeU=(point1.cluster(0).energy()>point2.cluster(0).energy())?point1.cluster(0).energy():point2.cluster(0).energy();
      float EsmallV=(point1.cluster(1).energy()<point2.cluster(1).energy())?point1.cluster(1).energy():point2.cluster(1).energy();
      float ElargeV=(point1.cluster(1).energy()>point2.cluster(1).energy())?point1.cluster(1).energy():point2.cluster(1).energy();
      float dU=abs(point1.cluster(0).strip(0).index()-point2.cluster(0).strip(0).index());
      float dV=abs(point1.cluster(1).strip(0).index()-point2.cluster(1).strip(0).index());
      if(pair.mass()<0.06) {
	dUvsdV->Fill(dU,dV);
	
	if(dU==0||dV==0)
	  { 
	    dUVzeroE->Fill(pair.energy());
	    dUVzeroEta->Fill(pair.momentum().Eta());
	  }
	if(dU==0)
	  {
	    dVvsRatio->Fill(dV,EsmallV/ElargeV);
	  }
	if(dV==0)
	  {
	    dUvsRatio->Fill(dU,EsmallU/ElargeU);
	  }
      }//end of mass cut 0.06
      if(pair.mass()>=0.08 && pair.mass()<=0.18){
	Float_t e1 = pair.point(0).energy();
	Float_t e2 = pair.point(1).energy();
	
	TVector3 pp = (e1*point1pos + e2*point2pos) * ( 1/(e1+e2) );
	GoodPiGeo->Fill(pp.X(),pp.Y());
	GoodPiPt->Fill(pair.pt());
	dUvsdVGood->Fill(dU,dV);

	if(dU==0)
	  {
	    dVvsRatioGood->Fill(dV,EsmallV/ElargeV); 
	  }

	if(dV==0)
	  {
	    dUvsRatioGood->Fill(dU,EsmallU/ElargeU);
	  }	
      }//end of good mass cut
#ifdef EFFICIENCY
     
      //start to calculate pi0 reconstruction efficiency
      StMcOutputMaker *mkMc=(StMcOutputMaker *)GetMaker("mcRead");
      assert(mkMc);
     
      float rPt=0;
      float rEta=0;
      float rPhi=0;
      float rE=0;
    
      if(pair.mass()>=0.08 && pair.mass()<=0.18)
	{
	  
	  numcutCan++;
	  rPt=pair.pt();
	  rEta=pair.momentum().Eta();
	  rPhi=pair.momentum().Phi();
	  rE=pair.energy();
	  int size=0;
	  size=mkMc->gTr.size();
	  float minD=10000000;
	  float minR=10000000;
	  //printf("ana size=%d\n",size);
	  for ( Int_t j=0;j<size;j++ )
	  {
	      StMcTrack *gt=mkMc->gTr[j];
	      float eemceta=mkMc->geemcEta[j];
	      StLorentzVectorF p4=gt->fourMomentum();
	      float etaMc=p4.pseudoRapidity();
	      float phiMc=p4.phi();
	      //printf("rPhi=%f phiMc=%f rEta=%f etaMc=%f\n",rPhi,phiMc,rEta,etaMc);
	      
	      float deltaphi=fabs(rPhi-phiMc);
	      if(deltaphi>=3.14159265)
		{deltaphi=6.283-deltaphi;}
	      float deltaeta=rEta-etaMc;
	      float Rdeltaeta=rEta-eemceta;
	      float deltad=TMath::Sqrt(deltaphi*deltaphi+deltaeta*deltaeta);
	      float deltaR=TMath::Sqrt(deltaphi*deltaphi+Rdeltaeta*Rdeltaeta);
	      if(deltad<=minD) 
		{
		  //printf("dphi=%f deta=%f\n",deltaphi,deltaeta);
		  minD=deltad;
		}
	      if(deltaR<=minR) 
		{
		  minR=deltaR;
		}
	  }
	  hRealMcPD->Fill(minD);
	  hRealMcPR->Fill(minR);
	  for ( Int_t j=0;j<size;j++ )
	  {
	      StMcTrack *gt=mkMc->gTr[j];
	      float eemceta=mkMc->geemcEta[j];
	      //float eeeta=mkMc->geeEta[j];
	      StLorentzVectorF p4=gt->fourMomentum();
	      float etaMc=p4.pseudoRapidity();
	      //printf("etaMc=%f eemceta=%f\n",etaMc,eemceta);
	      float phiMc=p4.phi();
	      //printf("rPhi=%f phiMc=%f rEta=%f etaMc=%f\n",rPhi,phiMc,rEta,etaMc);
	      float ptMc=p4.perp();
	      float energyMc=p4.e();
	      //printf("energyMc=%f\n",energyMc);
	      float deltaphi=fabs(rPhi-phiMc);
	      if(deltaphi>=3.14159265)
		{deltaphi=6.283-deltaphi;}
	      float deltaeta=rEta-etaMc;
	      float Rdeltaeta=rEta-eemceta;
	      float deltad=TMath::Sqrt(deltaphi*deltaphi+deltaeta*deltaeta);
	      float deltaR=TMath::Sqrt(deltaphi*deltaphi+Rdeltaeta*Rdeltaeta);
	   
		  
	    
	      //deltad for pythia deltaR for simple MC	      
	      if(deltaR<=0.04)
		{
		  hPi0Mass->Fill(pair.mass());
		  hPTvsPiNum->Fill(rPt);
		  hREtaPhi->Fill(rPhi/3.1416*180,rEta);
		  hReconEta->Fill(rEta);
		  hReconPhi->Fill(rPhi/3.1416*180);
		  hRecoEnergy->Fill(rE);
		  
		  hMcEta->Fill(etaMc);
		  hEEmcEta->Fill(eemceta);
		  hMcPhi->Fill(phiMc/3.1416*180);
		  hMcPt->Fill(ptMc);
		  hResoPt->Fill(rPt-ptMc);
		  hResoPtvsG->Fill(rPt-ptMc,ptMc);
		  hResoEta->Fill(rEta-etaMc);
		  hResoPhi->Fill((rPhi-phiMc)/3.1416*180);
		  hMcEnergy->Fill(energyMc);
		  hResoEnergyvsG->Fill(rE-energyMc,energyMc);
		  hResoEnergy->Fill(rE-energyMc);
		//printf("rE=%f mcE=%f\n",rE,energyMc);
		//hZgg->Fill(pair.zgg());
	
		 
		//printf("zvertex=%f\n",pair.vertex().Z());
		  //McEvsMass->Fill(energyMc,pair.mass());
		}
	      
	  }
	 
	}//end of efficiency calculation
      
#endif
      
      /// If spin sorting has been disabled
      if ( !mSpinSort ) {
	std::cout << "Problem detected, spin sorting disabled" << std::endl;
	continue;
      }
      
      spin4=getSpinState( mMuDst->muDst(),bxStar );
      if ( spin4>=0 )
      if ( mymap[spin4] ) {
	mHistograms[ mymap[spin4] ] -> Fill( pair );
      }

      hMassBx->Fill(pair.mass(),bxStar); 
      if ( pair.mass()>0.1 && pair.mass() < 0.18 ) {
	  hBxStarPi0->Fill(bxStar); 
	  hZvertBx->Fill(pair.vertex().Z(),bxStar);
	  hZggBx->Fill(pair.zgg(),bxStar);
	  hEtaBx->Fill(pair.momentum().Eta(),bxStar); 
      }
      

    }
  hPi0Counter->Fill(numoacceptedpair);
  //printf("numcutCan=%d\n",numcutCan);
  //}//end of mix candidate 6
  /// Repeat for backgrounds
  for ( Int_t i=0;i<mEEmixer->numberOfMixedCandidates();i++ )
    {

      StEEmcIUPair pair = mEEmixer->mixedCandidate(i);
      if ( !accept( pair, false ) ) continue;
      mBackgrounds[ kAny ] -> Fill( pair );
      mMixedEvent -> addPair( pair ); 

    }

 END_OF_MAKE:
  mRealTree->Fill();
  mMixedTree->Fill(); 
  return kStOK;
}

// ----------------------------------------------------------------------------
void StEEmcIUPi0Analysis::mixer(const Char_t *name)
{
  mEEmixer=(StEEmcIUMixMaker *)GetMaker(name);
  assert(mEEmixer);// please specify a valid mix maker
}

// ----------------------------------------------------------------------------
void StEEmcIUPi0Analysis::points(const Char_t *name)
{
  mEEpoints=(StEEmcIUPointMaker *)GetMaker(name);
  assert(mEEpoints);
}

// ----------------------------------------------------------------------------
void StEEmcIUPi0Analysis::mudst(const Char_t *name)
{
    mMuDst=(StMuDstMaker*)GetMaker(name);
    assert(mMuDst); 
}
// ----------------------------------------------------------------------------
void StEEmcIUPi0Analysis::analysis(const Char_t *name)
{
    mEEanalysis=(StEEmcA2EMaker*)GetMaker(name);
    assert(mEEanalysis); 
} 
// ----------------------------------------------------------------------------
void StEEmcIUPi0Analysis::spin(const Char_t *name) 
{
  mSpinDb=(StSpinDbMaker*)GetMaker(name);
  /// no assert, null pointer expected if running on MC
}
// ----------------------------------------------------------------------------
Bool_t StEEmcIUPi0Analysis::twoBodyCut( StEEmcIUPair &pair )
{

  static const Int_t   maxPerCluster = 4;
  static const Float_t minTowerFrac = 0.;
  
  /// Obtain the 6-18 towers which contribute energy to this pair

  Bool_t towers[720]; for (Int_t i=0;i<720;i++ ) towers[i]=false; 
  StEEmcTower t1 = pair.point(0).tower(0);
  StEEmcTower t2 = pair.point(1).tower(0);

  Float_t Etowers=0.;
  Etowers += t1.energy();


  towers[ t1.index() ] = true;
  towers[ t2.index() ] = true;

  for ( Int_t i=0;i<t1.numberOfNeighbors();i++ ) {
    StEEmcTower mytow=t1.neighbor(i);
    towers[ mytow.index() ] = true;    
    Etowers += mytow.energy();
  }
  for ( Int_t i=0;i<t2.numberOfNeighbors();i++ ) {
    StEEmcTower mytow=t2.neighbor(i);
    if ( !towers[mytow.index()] ) Etowers += mytow.energy();
    towers[ mytow.index() ] = true;
  }



  /// NOTE: we could consider adding the next-nearest neighbors as well
  
  /// Loop over all points in the endcap and count the number which
  /// match one of the specified towers as above

  Int_t count = 0;
  Float_t pe1=pair.point(0).energy();
  Float_t pe2=pair.point(1).energy();
  Float_t min2=(pe1<=pe2)?pe1:pe2;
  Bool_t Most2E=true;

  for ( Int_t i=0;i<mEEpoints->numberOfPoints();i++ )
    {
      StEEmcIUPoint p=mEEpoints->point(i);
      Float_t ppe=p.energy();
      StEEmcTower t=p.tower(0);      
      if ( towers[ t.index() ] ){
	count++;
	if(t.index()!=t1.index() && t.index()!=t2.index() && ppe>min2){
	  Most2E=false;
	}
      }
    }
  //off most energetic two points
  Most2E=true;

  Float_t Epoints = pair.energy();

  return ( count <= maxPerCluster && Most2E && Epoints > minTowerFrac * Etowers );

}

// ----------------------------------------------------------------------------
// mudst based cuts
Bool_t StEEmcIUPi0Analysis::accept( StMuDst *mudst )
{

    // verify that the trigger is in the trigger list 
    StMuEvent *event=mudst->event();
    assert(event);

    Bool_t good_trigger = false; 

    // -------------------------------------------------------
    //
    // Trigger from real data
    //

    StMuTriggerIdCollection tic = event -> triggerIdCollection();
    StTriggerId l1trig = tic.l1();
    // if no triggers are specified, always return true 
    if ( mTriggerList.size() <=0 ) {
	good_trigger = true; 
    } 

    Int_t go=0;
    std::vector<Int_t>::iterator iter=mTriggerList.begin();
    while ( iter != mTriggerList.end() ) {
      	go = l1trig.isTrigger( (*iter) );
	good_trigger |= go; 	
    	iter++;
    }


    // -------------------------------------------------------
    //
    // Trigger from sim data
    //
    if ( mTrigSim ) 
      {
	good_trigger = mTrigSim -> getEEMCtrigHT( mTrigSimThreshold );
	good_trigger &= mTrigSim -> getBBCtrig();
      }


    if ( l1trig.isTrigger( mMinBias ) )
      hEventCounter -> Fill( kMinBias );

    // must have a valid trigger to proceed 
    if ( good_trigger ) 
      {
	
	hEventCounter -> Fill( kTrig );
      } 
    else 
      {        
	
        return false; 
    }

    // loop over all hit towers and verify that one tower
    // exceeds the software threshold in mCuts
    StEEmcTower ht;
    Bool_t sw36=0; 
    
    for ( Int_t i=0;i<mEEanalysis->numberOfHitTowers(0);i++ )
    {
	StEEmcTower t=mEEanalysis->hittower(i,0);
	if ( !t.fail() && t.et() > ht.et() ) {
	    ht=t; 
	    if ( ht.et()>=mCuts->tower_et_cut &&
		 ht.sector()>=2&&ht.sector()<=5 ) sw36=true; 
	}
    } 
    //printf("ht.et=%f\n",ht.et());
    if ( ht.et() < mCuts->tower_et_cut ) {
	good_trigger = false; 
	//printf("false\n");
	return false; 
    } 
    else 
    {
	hEventCounter -> Fill( kSoftTrig );
    } 
    if ( sw36 ) hEventCounter -> Fill( kSoftTrig36 ); 
    
    
    StThreeVectorF vert=event->primaryVertexPosition();
    if ( vert.z() < mCuts->z_vertex_min) {
      // std::cout << "No vertex" << std::endl;
      return false;
    }
    if ( vert.z() > mCuts->z_vertex_max ) {
      // std::cout << "No vertex" << std::endl;
      return false; 
    }
    
    hEventCounter -> Fill( kVertex ); 


    return true; 

} 

Bool_t StEEmcIUPi0Analysis::accept( StEEmcIUPair pair, Bool_t fill )
{
 
    /// Pair must be isolated, i.e. only two points sharing tower energy 
  if ( !twoBodyCut(pair) ) return false; 
  if (fill) hPairCounter -> Fill( kTwoBody ); 
    /// One gamma must be associated with a tower above trigger threshold
  StEEmcIUPoint point1=pair.point(0);
  StEEmcIUPoint point2=pair.point(1); 
  StEEmcTower tower1=pair.point(0).tower(0);
  StEEmcTower tower2=pair.point(1).tower(0);
  TVector3 pointpos1=point1.position();
  TVector3 pointpos2=point2.position();
  Float_t peta1=pointpos1.Eta();
  Float_t peta2=pointpos2.Eta();
  Float_t pphi1=pointpos1.Phi();
  Float_t pphi2=pointpos2.Phi();
  Int_t mod_phi1=int(pphi1*180./3.14159265+180.)%6;
  Int_t mod_phi2=int(pphi2*180./3.14159265+180.)%6;
  pointpos1=pointpos1.Unit();
  pointpos2=pointpos2.Unit();

  Float_t pet1 = (point1.energy()*pointpos1).Perp();
  Float_t pet2 = (point2.energy()*pointpos2).Perp();
 
  //require both points to be above 1.5 GeV because of fluctuation   
  if(pet1<=1.5 || pet2<=1.5) return false;
 
  if ( tower1.adc() < mCuts->adc_cut && tower2.adc() < mCuts->adc_cut ) return false; 

  if ( fill) hPairCounter -> Fill( kAdcTow ); 

    /*
    if ( tower1.et()  < mCuts->tower_et_cut &&
	 tower2.et()  < mCuts->tower_et_cut ) return false;
    */
  TVector3 d1 = mEEgeom -> getTowerCenter( (UInt_t)tower1.sector(), (UInt_t)tower1.subsector(), (UInt_t)tower1.etabin() );
  TVector3 d2 = mEEgeom -> getTowerCenter( (UInt_t)tower2.sector(), (UInt_t)tower2.subsector(), (UInt_t)tower2.etabin() );
 
  TVector3 dd1(d1.x(),d1.y(),d1.z()-pair.vertex().z());
  TVector3 dd2(d2.x(),d2.y(),d2.z()-pair.vertex().z());

 
  d1=d1.Unit();
  d2=d2.Unit();
  dd1=dd1.Unit();
  dd2=dd2.Unit();
  //Float_t et1 = (tower1.energy()*d1).Perp();
  //Float_t et2 = (tower2.energy()*d2).Perp();
  Float_t et11 = (tower1.energy()*dd1).Perp();
  Float_t et22 = (tower2.energy()*dd2).Perp();  

  //place setting bivariate Gaussian tower et cut according to point positions
  Float_t ueta1,ueta2,tower_et_cut1=3,tower_et_cut2=3;
  if(peta1>=1.901 && peta1<=2) ueta1=1.932;
  if(peta1>=1.807 && peta1<1.901) ueta1=1.845;
  if(peta1>=1.718 && peta1<1.807) ueta1=1.761;
  if(peta1>=1.633 && peta1<1.718) ueta1=1.664;
  if(peta1>=1.552 && peta1<1.633) ueta1=1.585;
  if(peta1>=1.476 && peta1<1.552) ueta1=1.507;
  if(peta1>=1.403 && peta1<1.476) ueta1=1.4374;
  if(peta1>=1.334 && peta1<1.403) ueta1=1.36;
  if(peta1>=1.268 && peta1<1.334) ueta1=1.294;
  if(peta1>=1.205 && peta1<1.268) ueta1=1.2315;
  if(peta1>=1.146 && peta1<1.205) ueta1=1.174;
  if(peta1>=1.089 && peta1<1.146) ueta1=1.1265;

  if(peta2>=1.901 && peta2<=2) ueta2=1.932;
  if(peta2>=1.807 && peta2<1.901) ueta2=1.845;
  if(peta2>=1.718 && peta2<1.807) ueta2=1.761;
  if(peta2>=1.633 && peta2<1.718) ueta2=1.664;
  if(peta2>=1.552 && peta2<1.633) ueta2=1.585;
  if(peta2>=1.476 && peta2<1.552) ueta2=1.507;
  if(peta2>=1.403 && peta2<1.476) ueta2=1.4374;
  if(peta2>=1.334 && peta2<1.403) ueta2=1.36;
  if(peta2>=1.268 && peta2<1.334) ueta2=1.294;
  if(peta2>=1.205 && peta2<1.268) ueta2=1.2315;
  if(peta2>=1.146 && peta2<1.205) ueta2=1.174;
  if(peta2>=1.089 && peta2<1.146) ueta2=1.1265;
  float toweretcut=mCuts->tower_et_cut;
  tower_et_cut1=toweretcut*(exp(-0.5*((peta1-ueta1)*(peta1-ueta1)/pow(0.035,2)+(mod_phi1-0.0)*(mod_phi1-0.0)/pow(2.3,2))));
  tower_et_cut2=toweretcut*(exp(-0.5*((peta2-ueta2)*(peta2-ueta2)/pow(0.035,2)+(mod_phi2-0.0)*(mod_phi2-0.0)/pow(2.3,2))));

  if ( et11 < tower_et_cut1 && et22 < tower_et_cut2 ) return false;
  


  if ( fill ) hPairCounter -> Fill( kEtTow ); 

    /// Pair must be w/in eta range

  float Rxy=TMath::Sqrt(pair.vertex().x()*pair.vertex().x()+pair.vertex().y()*pair.vertex().y());
  float hHeight=pair.pt()*(270.0-pair.vertex().Z())/pair.pz()+Rxy;
  float etatheta=TMath::ATan(hHeight/270.0);
  //printf("accept pz=%f\n",pair.pz());
  float mideta=tan(etatheta/2.0);
  float eta=-log(mideta);
  if ( eta < mCuts->eta_min || eta > mCuts->eta_max ) return false;
  if ( fill ) hPairCounter->Fill( kKinematics ); 


  
  
  
    
  return true;

} 

// ----------------------------------------------------------------------------
Int_t StEEmcIUPi0Analysis::getSpinState( StMuDst *mudst, Int_t &bxStar )
{

  StMuEvent   *event = mudst -> event();
  StL0Trigger *trig  =&(event->l0Trigger());

  StMuTriggerIdCollection tic = event -> triggerIdCollection();
  StTriggerId l1trig = tic.l1();


  Int_t bx48 = trig->bunchCrossingId();
  Int_t bx7  = trig->bunchCrossingId7bit( mRunNumber );  

  //bxStar = mSpinDb->BXstarUsingBX48(bx48);
  bxStar = mSpinDb->BXyellowUsingBX48(bx48); 

  mRealEvent->bx48 = bx48;
  mRealEvent->bx7  = bx7;
  mRealEvent->bxStar = bxStar; 

  mMixedEvent->bx48=bx48;
  mMixedEvent->bx7 =bx7;
  mMixedEvent->bxStar=bxStar; 

  //////////////////////////////////////////////////////////////////////
  /// HARDCODED KLUDGE 
  if ( bx7 == 0 || bx7 == 119 ) return -1; // kick 0 and 119 out of mix
  if ( bx7 == 14 || bx7 == 54 ) return -1; 
  //////////////////////////////////////////////////////////////////////

  hBx7->Fill( bx7 );
  hBx48->Fill( bx48 );
  hBx7diffBx48->Fill( bx7, mSpinDb->offsetBX48minusBX7(bx48,bx7) );

  //$$$::cout << "bx7=" << bx7 << " bx48=" << bx48 << std::endl;

  if ( mSpinDb -> isMaskedUsingBX48(bx48) ) return -1;  // return an error flag
  if ( mSpinDb->offsetBX48minusBX7(bx48,bx7)!=0 ) std::cout << "BUNCH CROSSINGS INCONSISTENT" << std::endl;

  //  assert(mSpinDb->offsetBX48minusBX7(bx48,bx7)==0); // scaler boards were not in sync
  //  mSpinSort = (mSpinDb->offsetBX48minusBX7(bx48,bx7)==0);
  // disable spin sorting and clear histograms
  if ( mSpinDb->offsetBX48minusBX7(bx48,bx7)!=0 )
    {
      mSpinSort = false; 
      for ( Int_t ii=1;ii<=4;ii++ ) mHistograms[ii]->Clear();
    }


  Int_t spin4 = mSpinDb->spin4usingBX48(bx48);
  hSpin4->Fill(spin4);

  if ( l1trig.isTrigger(96011) ) hSpin4mb -> Fill(spin4); 

  
  return spin4;

}
// ----------------------------------------------------------------------------
void StEEmcIUPi0Analysis::Clear(Option_t *opts)
{
    mRealEvent -> Clear();
    mMixedEvent -> Clear(); 
} 
