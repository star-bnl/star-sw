#include "StEvent/StEnumerations.h"
#include "StEvent/StEpdHit.h"
#include "StEvent/StEvent.h"
#include "StEvent/StFcsCluster.h"
#include "StEvent/StFcsCollection.h"
#include "StEvent/StFcsHit.h"
#include "StEvent/StEventTypes.h"
#include "StFcsDbMaker/StFcsDbMaker.h"
#include "StMessMgr.h"
#include "StMuDSTMaker/COMMON/StMuTypes.hh"
#include "StSpinPool/StFcsQaMaker/StFcsQaMaker.h"
#include "StSpinPool/StFcsRawDaqReader/StFcsRawDaqReader.h"
#include "StRoot/StEpdUtil/StEpdGeom.h"
#include "StThreeVectorF.hh"
#include "Stypes.h"

#include "StFwdAnaEpdFcsMixedEvent.h"

ClassImp(StFwdAnaEpdFcsMixedEvent)

StFwdAnaEpdFcsMixedEvent::StFwdAnaEpdFcsMixedEvent()
{
  mMixedPhArr  = new TClonesArray("StFcsPhotonCandidate");
}

StFwdAnaEpdFcsMixedEvent::~StFwdAnaEpdFcsMixedEvent()
{
  mMixedPhArr->Clear("C");
  delete mMixedPhArr;
}

UInt_t StFwdAnaEpdFcsMixedEvent::LoadHists( TFile* file, HistManager* histman, StFwdAnaData* data )
{
  UInt_t loaded = 0;
  if( histman==0 ){ return loaded; }
  //std::cout << "StFwdAnaEpdFcsMixedEvent::LoadHists()" << std::endl;

  loaded += histman->AddH2F(file,mH2F_PointProj_nmipValldx,"H2F_PointProj_nmipValldx","nMIP vs. FCS projected point to EPD x minus EPD x of all hits;dX (cm);nmip", 200,-100,100, 70,0,7);
  loaded += histman->AddH2F(file,mH2F_PointProj_nmipValldy,"H2F_PointProj_nmipValldy","nMIP vs. FCS projected point to EPD y minus EPD y of all hits;dY (cm);nmip", 200,-100,100, 70,0,7);
  loaded += histman->AddH2F(file,mH2F_PointProj_nmipValldr,"H2F_PointProj_nmipValldr","nMIP vs. FCS projected point to EPD, polar distance to all other hits;dR (cm);nmip", 200,-100,100, 70,0,7);
  loaded += histman->AddH2F(file,mH2F_PointProj_nmipValldphi,"H2F_PointProj_nmipValldphi","nMIP vs. FCS projected point to EPD, angle difference to all other hits;d#phi;nmip", 90,0,TMath::Pi(), 70,0,7);
  loaded += histman->AddH2F(file,mH2F_MixedPointProj_nmipValldr,"H2F_MixedPointProj_nmipValldr","Mixed event nMIP vs. FCS projected point to EPD, polar distance to all other hits;dR (cm);nmip", 200,-100,100, 70,0,7);
  loaded += histman->AddH2F(file,mH2F_MixedPointProj_nmipValldphi,"H2F_MixedPointProj_nmipValldphi","Mixed event nMIP vs. FCS projected point to EPD, angle difference to all other hits;d#phi;nmip", 90,0,TMath::Pi(), 70,0,7);

  loaded += histman->AddH2F(file,mH2F_PointProj_nmipVtiledx,"H2F_PointProj_nmipVtiledx","nMIP vs. FCS projected point to EPD x minus EPD x of proj tile;dX (cm);nmip", 80,-20,20, 70,0,7);
  loaded += histman->AddH2F(file,mH2F_PointProj_nmipVtiledy,"H2F_PointProj_nmipVtiledy","nMIP vs. FCS projected point to EPD y minus EPD y of proj tile;dY (cm);nmip", 80,-20,20, 70,0,7);
  loaded += histman->AddH2F(file,mH2F_PointProj_nmipVtiledr,"H2F_PointProj_nmipVtiledr","nMIP vs. FCS projected point to EPD, polar distance to projected hit;dR (cm);nmip", 80,-20,20, 70,0,7);
  loaded += histman->AddH2F(file,mH2F_PointProj_nmipVtiledphi,"H2F_PointProj_nmipVtiledphi","nMIP vs. FCS projected point to EPD, angle difference to proj tile;d#phi;nmip", 90,0,TMath::Pi()/5.0, 70,0,7);
  loaded += histman->AddH2F(file,mH2F_MixedPointProj_nmipVtiledr,"H2F_MixedPointProj_nmipVtiledr","Mixed event nMIP vs. FCS projected point to EPD, polar distance to projected hit;dR (cm);nmip", 80,-20,20, 70,0,7);
  loaded += histman->AddH2F(file,mH2F_MixedPointProj_nmipVtiledphi,"H2F_MixedPointProj_nmipVtiledphi","Mixed event nMIP vs. FCS projected point to EPD, angle difference to proj tile;d#phi;nmip", 90,0,TMath::Pi()/5.0, 70,0,7);

  loaded += histman->AddH2F(file,mH2F_PointProj_LowMult_nmipValldr,"H2F_PointProj_LowMult_nmipValldr","Low Mult nMIP vs. FCS projected point to EPD, polar distance to all other hits;dR (cm);nmip", 200,-100,100, 70,0,7);
  loaded += histman->AddH2F(file,mH2F_PointProj_LowMult_nmipValldphi,"H2F_PointProj_LowMult_nmipValldphi","Low Mult nMIP vs. FCS projected point to EPD, angle difference to all other hits;d#phi;nmip", 90,0,TMath::Pi(), 70,0,7);
  loaded += histman->AddH2F(file,mH2F_MixedPointProj_LowMult_nmipValldr,"H2F_MixedPointProj_LowMult_nmipValldr","Low Mult Mixed event nMIP vs. FCS projected point to EPD, polar distance to projected hit;dR (cm);nmip", 200,-100,100, 70,0,7);
  loaded += histman->AddH2F(file,mH2F_MixedPointProj_LowMult_nmipValldphi,"H2F_MixedPointProj_LowMult_nmipValldphi","Low Mult Mixed event nMIP vs. FCS projected point to EPD, angle difference to proj all;d#phi;nmip", 90,0,TMath::Pi(), 70,0,7);

  return loaded;
}

//----------------------
Int_t StFwdAnaEpdFcsMixedEvent::DoMake(StFwdAnaData* anadata)
{
  //std::cout << this->ClassName() << "|Start Make" << std::endl;
  TClonesArray* PhArr = anadata->getPhArr();
  StEpdGeom* EpdGeom = anadata->epdGeom();
  Double_t usevertex = anadata->getEvtData()->mUseVertex;
  Double_t vertexcutlow = anadata->vertexCutLow();
  Double_t vertexcuthigh = anadata->vertexCutHigh();
  TClonesArray* MuEpdHits = 0;
  StEpdCollection* EpdColl = 0;
  anadata->epdColl(MuEpdHits,EpdColl);  

  //Check photon candidates if they have any hits in the EPD. Use a separate loop so that this information could be used in the pi0 checking loop if needed. In future may also want to check against FCS preshower (EPD) hits
  //Int_t npoints = ntotal - anadata->mEvtInfo->mClusterSize;
  unsigned int nepdhits = 0;
  StSPtrVecEpdHit* epdhits = 0;
  if( MuEpdHits!=0 ){ nepdhits = MuEpdHits->GetEntriesFast(); }
  else if( EpdColl!=0 ){
    epdhits = &(EpdColl->epdHits());
    nepdhits = epdhits->size();
  }
  else{ LOG_ERROR << "StFwdAnaEpdFcsMixedEvent::DoMake() - If you see this error then there is a bug that is setting EPD hits improperly" << endm; return kStErr; }

  //std::cout << this->ClassName() << "|Start:Make_CheckAndSetEpdHit" << std::endl;
  //Check photon candidates if they have any hits in the EPD. Use a separate loop so that this information could be used in the pi0 checking loop if needed. In future may also want to check against FCS preshower (EPD) hits
  Int_t noldhits = mMixedPhArr->GetEntriesFast();
  Int_t nnewhits = PhArr->GetEntriesFast();
  Int_t npoints = nnewhits - anadata->getEvtData()->mClusterSize;
  Int_t ntotal = noldhits+nnewhits;
  Int_t nepdwesthits = 0;
  for( Int_t iph = 0; iph<ntotal; ++iph ){
    //std::cout << "|iph:"<<iph << "|iphnew:"<<iph-noldhits << std::endl;
    StFcsPhotonCandidate* ph = 0;
    if( iph>=noldhits ){
      ph = (StFcsPhotonCandidate*) PhArr->UncheckedAt(iph-noldhits);
    }
    else{
      ph = (StFcsPhotonCandidate*) mMixedPhArr->UncheckedAt(iph);
    }
    if( ph==0 ){ std::cout << "==========I=CANNOT=BE=ZERO==========" << std::endl; return kStErr; }
    std::vector<Double_t> epdproj;
    if( iph>=noldhits ){
      epdproj = StFwdAnaData::ProjectToEpd(ph->mX,ph->mY,ph->mZ,usevertex);
    }
    else{
      //iph<noldhits
      epdproj = StFwdAnaData::ProjectToEpd(ph->mX,ph->mY,ph->mZ,mOldVertex);
      //StFwdAnaEpdMatch::CheckInsideEpdTile(EpdGeom, ph,epdproj.at(0),epdproj.at(1));  //Do this check for 
    }
    //This will be taken care of already if StFwdAnaEpdMatch is called before this Ana class
    //if( iph>=noldhits ){
      //Only fill for the points from current event
      //mH2F_EpdProjHitMap->Fill( epdproj.at(0),epdproj.at(1) );
      //if( vertexcutlow<=usevertex && usevertex<=vertexcuthigh ){ mH2F_EpdProjHitMap_Vcut->Fill(epdproj.at(0),epdproj.at(1)); }
      //std::cout << " + |phx:"<<ph->mX << "|phy:"<<ph->mY << "|phz:"<<ph->mZ << "|v:"<<usevertex << std::endl;
      //std::cout << " + |epdx:"<<epdproj.at(0) << "|epdy:"<<epdproj.at(1) << "|epdz:"<<epdproj.at(2) << std::endl;
      //std::cout << " ** |iph:"<< iph-noldhits << std::endl;
      //Only need to do this for the new points
      //StFwdAnaEpdMatch::CheckInsideEpdTile(EpdGeom, ph,epdproj.at(0),epdproj.at(1));  //Function that will check which EPD tiles photon candidate overlaps with and sets the appropriate variables for it
      //CheckInsideEpdTile(ph,epdproj.at(0),epdproj.at(1));  //Function that will check which EPD tiles photon candidate overlaps with and sets the appropriate variables for it
      /*
      if( ph->mEpdMatch[0]==0 ){ //If no intersection found it would be -1 so now check all the CCW adjacencies
	std::cout << "     ** |iph:"<<iph-noldhits <<"|projx:"<<epdproj.at(0) << "|projy:"<<epdproj.at(1) << "|nmip:"<< ph->mEpdHitNmip[0] << "|epdkey:"<<ph->mEpdMatch[0] << std::endl;
	}*/
    //}
    //loop over all hits and if an nmip value exists set for the point
    StMuEpdHit* muepdhit = 0;
    StEpdHit* epdhit = 0;
    for(unsigned int i=0; i<nepdhits; ++i ){
      if( MuEpdHits!=0 ){ muepdhit = (StMuEpdHit*)MuEpdHits->UncheckedAt(i); } //To match similar in StMuDstMaker->epdHit(int i)
      else if( epdhits!=0 ){ epdhit = (StEpdHit*)((*epdhits)[i]); }
      else{ LOG_ERROR << "IF YOU SEE THIS ERROR THEN THERE IS A VERY SERIOUS BUG IN THE CODE" << endm; return kStErr; } 
      //std::cout << "|i:"<<i << "|muepdhit:"<<muepdhit << "|epdhit:"<<epdhit << std::endl;
      int ew    = muepdhit!=0 ? muepdhit->side()    : epdhit->side();      //east=-1, west=1
      if( ew==-1 ){ continue; }
      if( iph==noldhits){ ++nepdwesthits; }
      int epdpp = muepdhit!=0 ? muepdhit->position(): epdhit->position();  //Supersector runs [1,12]
      int epdtt = muepdhit!=0 ? muepdhit->tile()    : epdhit->tile();      //Tile number [1,31]
      float nmip = muepdhit!=0 ? muepdhit->nMIP(): epdhit->nMIP();         //The ADC value of the hit divided by the MIP peak position; e.g. if nmip==1 then adc value sits at the MIP peak
      TVector3 epdhitxyz = EpdGeom->TileCenter(epdpp,epdtt,ew);
      Double_t dx = epdproj.at(0)-epdhitxyz[0];
      Double_t dy = epdproj.at(1)-epdhitxyz[1];
      double rpoint = sqrt(epdproj.at(0)*epdproj.at(0) + epdproj.at(1)*epdproj.at(1));
      double rhit = sqrt(epdhitxyz[0]*epdhitxyz[0] + epdhitxyz[1]*epdhitxyz[1]);
      Double_t pointdottile = epdproj.at(0)*epdhitxyz[0] + epdproj.at(1)*epdhitxyz[1];
      //Dot product for angle difference
      Double_t CosTheta = pointdottile / (rpoint*rhit);
      Double_t diffphi = TMath::ACos( CosTheta );
      //std::cout << "|epdz:"<<epdhitxyz[2] << std::endl;
      if( ! ph->mFromCluster ){
	//if( mTrigEm2==3 && mTrigEm0<0 && mTrigEm1<0 ){
	  if( iph>=noldhits ){
	    if( vertexcutlow<=usevertex && usevertex<=vertexcuthigh ){
	      mH2F_PointProj_nmipValldx->Fill(dx,nmip);
	      mH2F_PointProj_nmipValldy->Fill(dy,nmip);
	      mH2F_PointProj_nmipValldr->Fill(rpoint-rhit,nmip);
	      mH2F_PointProj_nmipValldphi->Fill(diffphi,nmip);
	      if( npoints<=5 ){
		mH2F_PointProj_LowMult_nmipValldr->Fill(rpoint-rhit,nmip);
		mH2F_PointProj_LowMult_nmipValldphi->Fill(diffphi,nmip);
	      }
	    }
	  }
	  else{
	    //iph<noldhits
	    if( vertexcutlow<=mOldVertex && mOldVertex<=vertexcuthigh ){
	      mH2F_MixedPointProj_nmipValldr->Fill(rpoint-rhit,nmip);
	      mH2F_MixedPointProj_nmipValldphi->Fill(diffphi,nmip);
	      /*Int_t overflowbin = mH2F_MixedPointProj_nmipValldr->GetBin(201,71);
		if( mH2F_MixedPointProj_nmipValldr->GetBinContent(overflowbin)!=0 ){
		std::cout << "Filled overflow bin:(" << overflowbin << "," << mH2F_MixedPointProj_nmipValldr->GetBinContent(overflowbin)<<")";
		std::cout << "|rpoint:"<<rpoint << "|rhit:"<<rhit << "|nmip:"<<nmip;
		std::cout << "|rpoint-rhit:"<<rpoint-rhit;
		std::cout << "|vert:"<<mOldVertex<< "|("<<epdproj.at(0) << ","<<epdproj.at(1) << ","<<epdproj.at(2)<<")";
		std::cout << std::endl;
		exit(0);
		}*/
	      if( mNOldPoints<=5 ){
		mH2F_MixedPointProj_LowMult_nmipValldr->Fill(rpoint-rhit,nmip);
		mH2F_MixedPointProj_LowMult_nmipValldphi->Fill(diffphi,nmip);
	      }
	    }
	  }
	  //}
      }
      if( ph->mEpdMatch[0] == (100*epdpp+epdtt)  ){  //Check match above
	if( iph>=noldhits ){
	  if( vertexcutlow<=usevertex && usevertex<=vertexcuthigh ){
	    if( ! ph->mFromCluster ){
	      mH2F_PointProj_nmipVtiledx->Fill(dx,nmip);
	      mH2F_PointProj_nmipVtiledy->Fill(dy,nmip);
	      mH2F_PointProj_nmipVtiledr->Fill(rpoint-rhit,nmip);
	      mH2F_PointProj_nmipVtiledphi->Fill(diffphi,nmip);	    
	    }
	  }
	  //int adc = muepdhit!=0 ? muepdhit->adc() : epdhit->adc();
	  //ph->mEpdHitNmip[0] = nmip;
	  //std::cout << "|epdpp:"<<epdpp <<"|epdtt:"<<epdtt <<"|nmip:"<<nmip << std::endl;
	}
	else{
	  //iph<noldhits
	  if( vertexcutlow<=mOldVertex && mOldVertex<=vertexcuthigh ){
	    if( ! ph->mFromCluster ){
	      mH2F_MixedPointProj_nmipVtiledr->Fill(rpoint-rhit,nmip);
	      mH2F_MixedPointProj_nmipVtiledphi->Fill(diffphi,nmip);
	    }
	  }
	}
      }
    }
    //if( iph>=noldhits ){ mH2F_EpdNmip->Fill(ph->mFromCluster,ph->mEpdHitNmip[0]); }
  }
  //std::cout << "|nold:"<<noldhits << "|nnew:"<<nnewhits << "|ntotal:"<<ntotal << "|oldvert:"<<mOldVertex << "|newvert:"<<usevertex<< "|nepdhits:"<<nepdwesthits <<"|noldpoints:"<<mNOldPoints << "|npoints:"<<npoints << std::endl;

  //std::cout << "|clustersize:"<<clustersize << "|ncandidates:"<<ncandidates << "|npoints:"<<npoints << std::endl;

  //Do this here since clear gets called before Make so that MixedPhArr doesn't get filled with junk before first pass
  mNOldPoints = PhArr->GetEntriesFast();
  mOldVertex = usevertex;  //Set old vertex for mixed event analysis on next event
  mMixedPhArr->Clear("C");
  //mMixedPhArr->AbsorbObjects(PhArr); //@[September 4, 2025] > This may giving a memory leak adding my own for loop to copy

  for( int i=0; i<PhArr->GetEntriesFast(); ++i ){
    StFcsPhotonCandidate* mixedph = (StFcsPhotonCandidate*)mMixedPhArr->ConstructedAt(i);
    StFcsPhotonCandidate* ph = (StFcsPhotonCandidate*)PhArr->ConstructedAt(i);
    ph->Copy(*mixedph); //Copy function copies ph into mixedph
    //ph->Print();
    //mixedph->Print();
  }

  return kStOk;
}

void StFwdAnaEpdFcsMixedEvent::PaintEpdAllDistQa(TCanvas* canv, const char* savename) const
{
  canv->Clear();
  canv->Divide(3,2);
  canv->cd(1)->SetLogz();
  mH2F_PointProj_nmipValldx->Draw("colz");
  mH2F_PointProj_nmipValldx->GetYaxis()->SetRangeUser(0,3);
  canv->cd(4)->SetLogz();
  mH2F_PointProj_nmipValldy->Draw("colz");
  mH2F_PointProj_nmipValldy->GetYaxis()->SetRangeUser(0,3);
  canv->cd(2)->SetLogz();
  mH2F_PointProj_nmipValldr->Draw("colz");
  mH2F_PointProj_nmipValldr->GetYaxis()->SetRangeUser(0,3);
  canv->cd(3)->SetLogz();
  mH2F_PointProj_nmipValldphi->Draw("colz");
  mH2F_PointProj_nmipValldphi->GetYaxis()->SetRangeUser(0,3);

  canv->cd(5)->SetLogz();
  mH2F_MixedPointProj_nmipValldr->Draw("colz");
  mH2F_MixedPointProj_nmipValldr->GetYaxis()->SetRangeUser(0,3);
  canv->cd(6)->SetLogz();
  mH2F_MixedPointProj_nmipValldphi->Draw("colz");
  mH2F_MixedPointProj_nmipValldphi->GetYaxis()->SetRangeUser(0,3);

  canv->Print(savename);
}

void StFwdAnaEpdFcsMixedEvent::PaintEpdDistAnaQa(TCanvas* canv, const char* savename) const
{
  canv->Clear();
  canv->Divide(3,2);

  TH1* pointprojdr = ((TH2*)mH2F_PointProj_nmipValldr)->ProjectionX("pointprojdr",1,mH2F_PointProj_nmipValldr->GetNbinsY(),"e");
  TH1* pointprojdphi = ((TH2*)mH2F_PointProj_nmipValldphi)->ProjectionX("pointprojdphi",1,mH2F_PointProj_nmipValldphi->GetNbinsY(),"e");
  TH1* mixedpointprojdr = ((TH2*)mH2F_MixedPointProj_nmipValldr)->ProjectionX("mixedpointprojdr",1,mH2F_MixedPointProj_nmipValldr->GetNbinsY(),"e");
  TH1* mixedpointprojdphi = ((TH2*)mH2F_MixedPointProj_nmipValldphi)->ProjectionX("mixedpointprojdphi",1,mH2F_MixedPointProj_nmipValldphi->GetNbinsY(),"e");
  canv->cd(1);
  pointprojdr->DrawCopy("hist e");
  canv->cd(4);
  mixedpointprojdr->Draw("hist e");
  canv->cd(2);
  pointprojdphi->DrawCopy("hist e");
  canv->cd(5);
  mixedpointprojdphi->Draw("hist e");
  
  pointprojdr->Divide(mixedpointprojdr);
  pointprojdphi->Divide(mixedpointprojdphi);
  
  canv->cd(3);
  pointprojdr->Draw("hist e");
  pointprojdr->GetYaxis()->SetRangeUser(0.95,1.15);
  canv->cd(6);
  pointprojdphi->Draw("hist e");

  canv->Print(savename);
  
}

void StFwdAnaEpdFcsMixedEvent::PaintEpdAllDistQaLowMult(TCanvas* canv, const char* savename) const
{
  canv->Clear();
  canv->Divide(3,2);

  canv->cd(1)->SetLogz();
  mH2F_PointProj_LowMult_nmipValldr->Draw("colz");
  mH2F_PointProj_LowMult_nmipValldr->GetYaxis()->SetRangeUser(0,3);
  canv->cd(2)->SetLogz();
  mH2F_PointProj_LowMult_nmipValldphi->Draw("colz");
  mH2F_PointProj_LowMult_nmipValldphi->GetYaxis()->SetRangeUser(0,3);
  canv->cd(4)->SetLogz();
  mH2F_MixedPointProj_LowMult_nmipValldr->Draw("colz");
  mH2F_MixedPointProj_LowMult_nmipValldr->GetYaxis()->SetRangeUser(0,3);
  canv->cd(5)->SetLogz();
  mH2F_MixedPointProj_LowMult_nmipValldphi->Draw("colz");
  mH2F_MixedPointProj_LowMult_nmipValldphi->GetYaxis()->SetRangeUser(0,3);

  TH1* pointprojlowmultdr = ((TH2*)mH2F_PointProj_LowMult_nmipValldr)->ProjectionX("pointprojlowmultdr",1,mH2F_PointProj_LowMult_nmipValldr->GetNbinsY(),"e");
  TH1* pointprojlowmultdphi = ((TH2*)mH2F_PointProj_LowMult_nmipValldphi)->ProjectionX("pointprojlowmultdphi",1,mH2F_PointProj_LowMult_nmipValldphi->GetNbinsY(),"e");
  TH1* mixedpointprojlowmultdr = ((TH2*)mH2F_MixedPointProj_LowMult_nmipValldr)->ProjectionX("mixedpointprojlowmultdr",1,mH2F_MixedPointProj_LowMult_nmipValldr->GetNbinsY(),"e");
  TH1* mixedpointprojlowmultdphi = ((TH2*)mH2F_MixedPointProj_LowMult_nmipValldphi)->ProjectionX("mixedpointprojlowmultdphi",1,mH2F_MixedPointProj_LowMult_nmipValldphi->GetNbinsY(),"e");
  /*
  canv->cd(5);
  pointprojdr->DrawCopy("hist e");
  canv->cd(6);
  mixedpointprojdr->Draw("hist e");
  canv->cd(7);
  pointprojdphi->DrawCopy("hist e");
  canv->cd(8);
  mixedpointprojdphi->Draw("hist e");
  */

  pointprojlowmultdr->Divide(mixedpointprojlowmultdr);
  pointprojlowmultdphi->Divide(mixedpointprojlowmultdphi);
  
  canv->cd(3);
  pointprojlowmultdr->Draw("hist e");
  pointprojlowmultdr->GetYaxis()->SetRangeUser(0.85,0.95);
  canv->cd(6);
  pointprojlowmultdphi->Draw("hist e");


  canv->Print(savename);

}

void StFwdAnaEpdFcsMixedEvent::PaintEpdTileDistQa(TCanvas* canv, const char* savename) const
{
  canv->Clear();
  canv->Divide(3,2);
  canv->cd(1)->SetLogz();
  mH2F_PointProj_nmipVtiledx->Draw("colz");
  mH2F_PointProj_nmipVtiledx->GetXaxis()->SetRangeUser(-20,20);
  mH2F_PointProj_nmipVtiledx->GetYaxis()->SetRangeUser(0,3);
  canv->cd(4)->SetLogz();
  mH2F_PointProj_nmipVtiledy->Draw("colz");
  mH2F_PointProj_nmipVtiledy->GetXaxis()->SetRangeUser(-20,20);
  mH2F_PointProj_nmipVtiledy->GetYaxis()->SetRangeUser(0,3);
  canv->cd(2)->SetLogz();
  mH2F_PointProj_nmipVtiledr->Draw("colz");
  mH2F_PointProj_nmipVtiledr->GetXaxis()->SetRangeUser(-20,20);
  mH2F_PointProj_nmipVtiledr->GetYaxis()->SetRangeUser(0,3);
  canv->cd(3)->SetLogz();
  mH2F_PointProj_nmipVtiledphi->Draw("colz");
  mH2F_PointProj_nmipVtiledphi->GetXaxis()->SetRangeUser(-0.3,0.3);
  mH2F_PointProj_nmipVtiledphi->GetYaxis()->SetRangeUser(0,3);
  canv->cd(5)->SetLogz();
  mH2F_MixedPointProj_nmipVtiledr->Draw("colz");
  mH2F_MixedPointProj_nmipVtiledr->GetXaxis()->SetRangeUser(-20,20);
  mH2F_MixedPointProj_nmipVtiledr->GetYaxis()->SetRangeUser(0,3);
  canv->cd(6)->SetLogz();
  mH2F_MixedPointProj_nmipVtiledphi->Draw("colz");
  mH2F_MixedPointProj_nmipVtiledphi->GetXaxis()->SetRangeUser(-0.3,0.3);
  mH2F_MixedPointProj_nmipVtiledphi->GetYaxis()->SetRangeUser(0,3);

  canv->Print(savename);
}


