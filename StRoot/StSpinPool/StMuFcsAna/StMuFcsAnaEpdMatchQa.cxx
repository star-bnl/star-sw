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

#include "StMuFcsAnaEpdMatchQa.h"

ClassImp(StMuFcsAnaEpdMatchQa)

StMuFcsAnaEpdMatchQa::StMuFcsAnaEpdMatchQa()
{
}

StMuFcsAnaEpdMatchQa::~StMuFcsAnaEpdMatchQa()
{
  for( auto itr=mEpdTileMap.begin(); itr!=mEpdTileMap.end(); ++itr ){
    delete itr->second;
  }
  mEpdTileMap.clear();
  for( auto itr=mEpdCcwLines.begin(); itr!=mEpdCcwLines.end(); ++itr ){
    delete itr->second;
  }
  mEpdCcwLines.clear();
}

UInt_t StMuFcsAnaEpdMatchQa::LoadHists( TFile* file, HistManager* histman, StMuFcsAnaData* data )
{
  UInt_t loaded = 0;
  if( histman==0 ){ return loaded; }
  //std::cout << "StMuFcsAnaEpdMatchQa::LoadHists()" << std::endl;

  //loaded += histman->AddH2F(file,mH2F_PointProj_nmipVdrtile,"H2F_PointProj_nmipVdrtile","nMIP vs. FCS projected point to EPD, polar distance to all other tiles;dR (cm);nmip", 200,-100,100, 70,0,7);
  //loaded += histman->AddH2F(file,mH2F_PointProj_nmipVdrhit,"H2F_PointProj_nmipVdrhit","nMIP vs. FCS projected point to EPD, polar distance to all other hits;dR (cm);nmip", 200,-100,100, 70,0,7);
  //loaded += histman->AddH2F(file,mH2F_PointProj_nmipVdphitile,"H2F_PointProj_nmipVdphitile","nMIP vs. FCS projected point to EPD, angle difference to all other tiles;d#phi;nmip", 100,-TMath::Pi(),TMath::Pi(), 70,0,7);
  //loaded += histman->AddH2F(file,mH2F_PointProj_nmipVdphihit,"H2F_PointProj_nmipVdphihit","nMIP vs. FCS projected point to EPD, angle difference to all other hits;d#phi;nmip", 100,-TMath::Pi(),TMath::Pi(), 70,0,7);

  //loaded += histman->AddH1F(file,mH1F_PointProjDistToEpdTiles,"H1F_PointProjDistToEpdTiles","Distance of FCS projected point to all EPD tiles;D (cm)", 200,0,200);
  //loaded += histman->AddH1F(file,mH1F_PointProjDistToEpdHits,"H1F_PointProjDistToEpdHits","Distance of FCS projected point to all EPD Hits;D (cm)", 200,0,200);

  loaded += histman->AddH2F(file,mH2F_ClusProjEpdAdj_maxVsum,"H2F_ClusProjEpdAdj_maxVsum","FCS projected cluster to EPD tile max vs. sum of adjacent tiles;Sum;Max", 70,0,7 , 70,0,7);
  loaded += histman->AddH2F(file,mH2F_PointProjEpdAdj_maxVsum,"H2F_PointProjEpdAdj_maxVsum","FCS projected point to EPD tile max vs. sum of adjacent tiles;Sum;Max", 70,0,7 , 70,0,7);

  loaded += histman->AddH2F(file,mH2F_ClusProjEpdAdj_maxVtile,"H2F_ClusProjEpdAdj_maxVtile","FCS projected cluster to EPD adjacent tiles nmip max vs. nmip of intersecting tile;Found;Max", 70,0,7 , 70,0,7);
  loaded += histman->AddH2F(file,mH2F_PointProjEpdAdj_maxVtile,"H2F_PointProjEpdAdj_maxVtile","FCS projected point to EPD adjacent tiles nmip max vs. nmip of intersecting tile;Found;Max", 70,0,7 , 70,0,7);

  loaded += histman->AddH1F(file,mH1F_ClusProjEpdAdjRedMip,"H1F_ClusProjEpdAdjRedMip","FCS projected cluster to EPD adjacent tiles max nmip divided by sum of adjacent;max/sum", 70,0,7);
  loaded += histman->AddH1F(file,mH1F_PointProjEpdAdjRedMip,"H1F_PointProjEpdAdjRedMip","FCS projected point to EPD adjacent tiles max nmip divided by sum of adjacent;max/sum", 70,0,7);

  loaded += histman->AddH1F(file,mH1F_ClusProjEpdAdjRedMax,"H1F_ClusProjEpdAdjRedMax","FCS projected cluster to EPD adjacent tiles nmip of intersecting tile divided by max nmip of adjacent tile;Found/Max", 70,0,7 );
  loaded += histman->AddH1F(file,mH1F_PointProjEpdAdjRedMax,"H1F_PointProjEpdAdjRedMax","FCS projected point to EPD adjacent tiles nmip of intersecting tile divided by max nmip of adjacent tile;Found/Max", 70,0,7);

  return loaded;
}

//----------------------
Int_t StMuFcsAnaEpdMatchQa::DoMake(StMuFcsAnaData* anadata)
{
  memset(mAllEpdNmip,0,sizeof(mAllEpdNmip));
  //std::cout << this->ClassName() << "|Start Make" << std::endl;
  TClonesArray* PhArr = anadata->getPhArr();
  StEpdGeom* EpdGeom = anadata->epdGeom();
  Double_t usevertex = anadata->mUseVertex;
  Double_t vertexcutlow = anadata->mVertexCutLow;
  Double_t vertexcuthigh = anadata->mVertexCutHigh;
  if( !(vertexcutlow<=usevertex && usevertex<=vertexcuthigh) ){ return kStOk; }
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
  else{ LOG_ERROR << "StMuFcsAnaEpdMatchQa::DoMake() - If you see this error then there is a bug that is setting EPD hits improperly" << endm; return kStErr; }

  //std::cout << this->ClassName() << "|Start:Make_CheckAndSetEpdHit" << std::endl;
  //Check photon candidates if they have any hits in the EPD. Use a separate loop so that this information could be used in the pi0 checking loop if needed. In future may also want to check against FCS preshower (EPD) hits
  Int_t npoints = PhArr->GetEntriesFast();
  //Int_t nepdwesthits = 0;
  for( Int_t iph = 0; iph<npoints; ++iph ){
    //std::cout << "|iph:"<<iph << std::endl;
    FcsPhotonCandidate* ph = (FcsPhotonCandidate*) PhArr->UncheckedAt(iph);
    if( ph==0 ){ std::cout << "==========I=CANNOT=BE=ZERO==========" << std::endl; return kStErr; }

    std::vector<Double_t> epdproj = StMuFcsAnaData::ProjectToEpd(ph->mX,ph->mY,ph->mZ,usevertex);
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
      int epdpp = muepdhit!=0 ? muepdhit->position(): epdhit->position();  //Supersector runs [1,12]
      int epdtt = muepdhit!=0 ? muepdhit->tile()    : epdhit->tile();      //Tile number [1,31]
      float nmip = muepdhit!=0 ? muepdhit->nMIP(): epdhit->nMIP();         //The ADC value of the hit divided by the MIP peak position; e.g. if nmip==1 then adc value sits at the MIP peak
      mAllEpdNmip[epdpp-1][epdtt-1] = nmip;
      /*
      TVector3 epdhitxyz = EpdGeom->TileCenter(epdpp,epdtt,ew);
      double rpoint = sqrt(epdproj.at(0)*epdproj.at(0) + epdproj.at(1)*epdproj.at(1));
      double rhit = sqrt(epdhitxyz[0]*epdhitxyz[0] + epdhitxyz[1]*epdhitxyz[1]);
      Double_t phipoint = TMath::ATan2(epdproj.at(1),epdproj.at(0));
      Double_t phihit = TMath::ATan2(epdhitxyz[1],epdhitxyz[0]);
      Double_t diffphi = phipoint-phihit;
      if( diffphi>TMath::Pi() ){ diffphi = diffphi - TMath::Pi(); }
      if( diffphi<(-1.0*TMath::Pi()) ){ diffphi = diffphi + TMath::Pi(); }
      Double_t dist = sqrt( (epdproj.at(0)-epdhitxyz[0])*(epdproj.at(0)-epdhitxyz[0]) + (epdproj.at(1)-epdhitxyz[1])*(epdproj.at(1)-epdhitxyz[1]) );
      //std::cout << "|epdz:"<<epdhitxyz[2] << std::endl;
      if( vertexcutlow<=usevertex && usevertex<=vertexcuthigh ){
	mH2F_PointProj_nmipVdrhit->Fill(rpoint-rhit,nmip);
	mH2F_PointProj_nmipVdphihit->Fill(diffphi,nmip);
	if( nmip>0.7 ){ mH1F_PointProjDistToEpdHits->Fill(dist); }
      }
      */
    }
    CheckInsideEpdTile(EpdGeom, ph,epdproj.at(0),epdproj.at(1));

    //loop over all west epd tiles after hit loop since we can use the saved nmip information from the hit loop
    /*
    for(int i_pp=1; i_pp<=12; ++i_pp){     //Supersector runs [1,12]
      for( int i_tt=1; i_tt<=31; ++i_tt ){ //Tile number [1,31]
	TVector3 epdhitxyz = EpdGeom->TileCenter(i_pp,i_tt,1);  //Only project to west side
	double rpoint = sqrt(epdproj.at(0)*epdproj.at(0) + epdproj.at(1)*epdproj.at(1));
	double rhit = sqrt(epdhitxyz[0]*epdhitxyz[0] + epdhitxyz[1]*epdhitxyz[1]);
	Double_t phipoint = TMath::ATan2(epdproj.at(1),epdproj.at(0));
	Double_t phihit = TMath::ATan2(epdhitxyz[1],epdhitxyz[0]);
	Double_t diffphi = phipoint-phihit;
	if( diffphi>TMath::Pi() ){ diffphi = diffphi - TMath::Pi(); }
	if( diffphi<(-1.0*TMath::Pi()) ){ diffphi = diffphi + TMath::Pi(); }
	Double_t dist = sqrt( (epdproj.at(0)-epdhitxyz[0])*(epdproj.at(0)-epdhitxyz[0]) + (epdproj.at(1)-epdhitxyz[1])*(epdproj.at(1)-epdhitxyz[1]) );
	if( vertexcutlow<=usevertex && usevertex<=vertexcuthigh ){
	  mH2F_PointProj_nmipVdrtile->Fill(rpoint-rhit,mAllEpdNmip[i_pp-1][i_tt-1]);
	  mH2F_PointProj_nmipVdphitile->Fill(diffphi,mAllEpdNmip[i_pp-1][i_tt-1]);
	  mH1F_PointProjDistToEpdTiles->Fill(dist);
	}
      }
    }
    
    if( mCanvas!=0 ){
      //if( mCanvSaveName.Length()==0 ){
      std::stringstream ss_savename;
      ss_savename << "FcsAna_PointEpdDistQa_"<<ph->mFromCluster << "_Ph"<<iph << "_Evt"<<mEvent << ".png";
      //std::cout << ss_savename.str() << std::endl;
      mCanvSaveName = ss_savename.str().c_str();
      //}
      //PaintPointEpdDistQa(mCanvas,mCanvSaveName.Data());
      //PaintPointEpdDistQaProj(mCanvas,mCanvSaveName.Data());
      //PaintPointEpdDist(mCanvas,mCanvSaveName.Data());
      //mCanvas->PaintTextBox();
    }
    */
  }
  
  //++mEvent;
  return kStOk;
}

void StMuFcsAnaEpdMatchQa::CheckInsideEpdTile(StEpdGeom* epdgeo, FcsPhotonCandidate* photon, Double_t projx, Double_t projy )
{
  //loop over all west epd tiles so that even if no hit recorded can use as a veto
  for(int i_pp=1; i_pp<=12; ++i_pp){     //Supersector runs [1,12]
    for( int i_tt=1; i_tt<=31; ++i_tt ){ //Tile number [1,31]
      if( epdgeo->IsInTile(i_pp,i_tt, 1, projx,projy) ){ //Only care about west EPD tiles; hence the '1'
	//TPolyLine* epd_ccw = EpdCCWOuterCorner(i_pp,i_tt);
	photon->mEpdHitNmip[0] = 0;
	photon->mEpdMatch[0] = 100*i_pp + i_tt;
	//std::cout << " + |projx:"<<projx << "|projy:"<<projy << "|nmip:"<< photon->mEpdHitNmip[0] << "|epdkey:"<<photon->mEpdMatch[0] << std::endl;
	break; //Inside match should be unique
      }
    }
  }

  if( photon->mEpdMatch[0]==0 ){ //If no intersection found it would be -1 so now check all the CCW adjacencies
    //std::cout << "   - |projx:"<<projx << "|projy:"<<projy << "|nmip:"<< photon->mEpdHitNmip[0] << "|epdkey:"<<photon->mEpdMatch[0] << std::endl;
    int ccwcounter = 1; //Should be 1 but Hack to check the drawing of the projections
    for(int i_pp=1; i_pp<=12; ++i_pp){     //Supersector runs [1,12]
      for( int i_tt=1; i_tt<=31; ++i_tt ){ //Tile number [1,31]
	Int_t epdkey = 100*i_pp+i_tt;
	TPolyLine* epd_outerccw = 0;
	auto itr = mEpdCcwLines.find(epdkey);
	if( itr!=mEpdCcwLines.end() ){ epd_outerccw = itr->second; }
	else{
	  epd_outerccw = StMuFcsAnaEpdMatch::EpdCCWOuterCorner(epdgeo,i_pp,i_tt);
	  auto result = mEpdCcwLines.emplace(epdkey,epd_outerccw);
	  if( !(result.second) ){ std::cout << "If you see this ERROR,, something went totally wrong and For some reason you tried to insert an EPD Tile Outer CCW that already exists" << std::endl; }
	}
	if( TMath::IsInside( projx, projy, epd_outerccw->GetN(), epd_outerccw->GetX(), epd_outerccw->GetY() ) ){
	  //TPolyLine* epd_ccw = EpdCCWOuterCorner(i_pp,i_tt);
	  photon->mEpdHitNmip[ccwcounter] = 0;
	  photon->mEpdMatch[ccwcounter] = epdkey;
	  //std::cout << "     - |ccwcounter:"<<ccwcounter << "|nmip:"<< photon->mEpdHitNmip[ccwcounter] << "|epdkey:"<<photon->mEpdMatch[ccwcounter] << std::endl;
	  ++ccwcounter;
	}
      }
    }
  }

  int ncorners = 0;
  for( int icorner=0; icorner<5; ++icorner ){
    //std::cout << " + |projx:"<<projx << "|projy:"<<projy << "|epdkey:"<<photon->mEpdHitNmip[icorner] << std::endl;
    if( photon->mEpdMatch[icorner]!=0 ){ ++ncorners; }
  }
  
  if( ncorners>1 ){
    int bestcorner = 0;
    Double_t mindist = 999; //Pick some large distance so that the minimum will get set with first loop
    for( int icorner=0; icorner<5; ++icorner ){
      //Pick the best corner and set it to 0 value since the algorithm above only cares about the match in 0
      //std::cout << " + |projx:"<<projx << "|projy:"<<projy << "|nmip:"<< photon->mEpdHitNmip[icorner] << "|epdkey:"<<photon->mEpdMatch[icorner];
      if( photon->mEpdMatch[icorner]!=0 ){
	int epdpp = photon->mEpdMatch[icorner]/100;
	int epdtt = photon->mEpdMatch[icorner] - epdpp*100;
	TVector3 epdhitxyz = epdgeo->TileCenter(epdpp,epdtt,1);//1 for west
	Double_t distx = projx-epdhitxyz.x();
	Double_t disty = projy-epdhitxyz.y();
	Double_t dist = TMath::Sqrt(distx*distx+disty*disty);
	//std::cout << "|("<<epdhitxyz.x() << ","<<epdhitxyz.y() <<")|dx:"<<distx << "|dy:"<< disty << "|dist:"<<dist;
	if( dist<mindist ){ bestcorner = icorner; }
      }
      //else{std::cout << "|("<<0 << ","<<0 <<")"; }
      //std::cout << std::endl;
    }
    //std::cout << "   + |ncorners:"<<ncorners << std::endl;
    photon->mEpdMatch[0] = photon->mEpdMatch[bestcorner];
    photon->mEpdHitNmip[0] = 0;
  }
  
  /*
  //For all tiles except 1, 2, or 3 only check Outer CCW since this will cover the gap for all tiles except 1, 2, or 3
  TPolyLine* epd_outerccw = EpdCCWOuterCorner(i_pp,i_tt);
  if( TMath::IsInside( projx, projy, epd_outerccw->GetN(), epd_outerccw->GetX(), epd_outerccw->GetY() ) ){
  photon->mEpdHitNmip[1] = 0;
  photon->mEpdMatch[1] = 100*i_pp + i_tt;
  }
  //else if( i_tt==1 || i_tt==2 || i_tt==3 ){
  //For tiles 1, 2, and 3 need to check other than the outer CCW because of the pentagonal structure of tile 1 means that inner CCW, inner CW, and outer CW have a weird overlap that needs checking
  TPolyLine* epd_innerccw = EpdCCWInnerCorner(i_pp,i_tt);
  if( TMath::IsInside( projx, projy, epd_innerccw->GetN(), epd_innerccw->GetX(), epd_innerccw->GetY() ) ){
  photon->mEpdHitNmip[2] = 0;
  photon->mEpdMatch[2] = 100*i_pp + i_tt;
  }
  TPolyLine* epd_innercw = EpdCWInnerCorner(i_pp,i_tt);
  if( TMath::IsInside( projx, projy, epd_innercw->GetN(), epd_innercw->GetX(), epd_innercw->GetY() ) ){
  photon->mEpdHitNmip[3] = 0;
  photon->mEpdMatch[3] = 100*i_pp + i_tt;
  }
  TPolyLine* epd_outercw = EpdCWOuterCorner(i_pp,i_tt);
  if( TMath::IsInside( projx, projy, epd_outercw->GetN(), epd_outercw->GetX(), epd_outercw->GetY() ) ){
  photon->mEpdHitNmip[4] = 0;
  photon->mEpdMatch[4] = 100*i_pp + i_tt;
  }
  } //for i_tt
  }   //for i_pp
  */
  
  //Set nmip since this is called after hit loop above so the nmip array should be filled
  int found_pp = 0;
  int found_tt = 0;
  if( photon->mEpdMatch[0]!=0 ){
    found_pp = photon->mEpdMatch[0]/100;
    found_tt = photon->mEpdMatch[0] - found_pp*100;
    photon->mEpdHitNmip[0] = mAllEpdNmip[found_pp-1][found_tt-1];
  }
  
  if( found_pp==0 || found_tt==0 ){ return; }
  //Grab all adjacencies then check their mip values
  const int MAX_ADJ = 8;
  int adj_pp[MAX_ADJ];
  int adj_tt[MAX_ADJ];
  StMuFcsAnaEpdMatch::GetEpdTileOuter(    found_pp, found_tt, adj_pp[0], adj_tt[0] );
  StMuFcsAnaEpdMatch::GetEpdTileOuterCCW( found_pp, found_tt, adj_pp[1], adj_tt[1] );
  StMuFcsAnaEpdMatch::GetEpdTileCCW(      found_pp, found_tt, adj_pp[2], adj_tt[2] );
  StMuFcsAnaEpdMatch::GetEpdTileInnerCCW( found_pp, found_tt, adj_pp[3], adj_tt[3] );
  StMuFcsAnaEpdMatch::GetEpdTileInner(    found_pp, found_tt, adj_pp[4], adj_tt[4] );
  StMuFcsAnaEpdMatch::GetEpdTileInnerCW(  found_pp, found_tt, adj_pp[5], adj_tt[5] );
  StMuFcsAnaEpdMatch::GetEpdTileCW(       found_pp, found_tt, adj_pp[6], adj_tt[6] );
  StMuFcsAnaEpdMatch::GetEpdTileOuterCW(  found_pp, found_tt, adj_pp[7], adj_tt[7] );

  float nmiptile = mAllEpdNmip[found_pp-1][found_tt-1];
  //Start with intersecting tile's values
  float nmipmax = nmiptile;
  float nmipsum = nmiptile;
  //std::cout << "  + |pp:"<<found_pp << "|tt:"<<found_tt <<"|nmip:"<< nmipmax << "|projx:"<<projx <<"|projy:"<<projy << std::endl;
  for(int iadj=0; iadj<MAX_ADJ; ++iadj ){
    if( adj_pp[iadj]==0 || adj_tt[iadj]==0 ){ continue; }
    float nmip_adj = mAllEpdNmip[adj_pp[iadj]-1][adj_tt[iadj]-1];
    //TVector3 epdhitxyz = epdgeo->TileCenter(adj_pp[iadj],adj_tt[iadj],1);
    //std::cout << "    - |iadj:"<<iadj << "|pp:"<<adj_pp[iadj] << "|tt:"<<adj_tt[iadj] << "|nmip:"<<nmip_adj << std::endl;
    //std::cout << "    - |iadj:"<<iadj << "|pp:"<<adj_pp[iadj] << "|tt:"<<adj_tt[iadj] << "|nmip:"<<nmip_adj <<"|x:"<<epdhitxyz[0] <<"|y:"<<epdhitxyz[1] << std::endl;
    if( nmip_adj>nmipmax ){ nmipmax=nmip_adj; }
    nmipsum += nmip_adj;
    //mH1F_PointProjEpdTile_nmipVadj->Fill(iadj,nmip_adj);
  }

  if( nmipmax>nmipsum ){ std::cout << "   * |nmiptile:"<<nmiptile << "|nmipmax:"<<nmipmax << "|nmipsum:"<<nmipsum << std::endl; }

  if( photon->mFromCluster ){ mH2F_ClusProjEpdAdj_maxVsum->Fill(nmipsum,nmipmax); }
  else{ mH2F_PointProjEpdAdj_maxVsum->Fill(nmipsum,nmipmax); }
  if( photon->mFromCluster ){ mH2F_ClusProjEpdAdj_maxVtile->Fill(nmiptile,nmipmax); }
  else{ mH2F_PointProjEpdAdj_maxVtile->Fill(nmiptile,nmipmax); }

  float reduced_nmip = nmipsum>0 ? nmipmax/nmipsum : 0;  //If sum is zero all adjacent nmips must be zero as well
  //std::cout << "   * |nmipmax:"<<nmipmax << "|nmipsum:"<<nmipsum <<"|red_nmip:"<<nmipmax/nmipsum << std::endl;
  if( photon->mFromCluster ){ mH1F_ClusProjEpdAdjRedMip->Fill(reduced_nmip); }
  else{ mH1F_PointProjEpdAdjRedMip->Fill(reduced_nmip); }

  float reduced_max = nmipmax>0 ? nmiptile/nmipmax : 0;  //If max is 0 then that means even the found nmip was zero
  if( photon->mFromCluster ){ mH1F_ClusProjEpdAdjRedMax->Fill(reduced_max); }
  else{ mH1F_PointProjEpdAdjRedMax->Fill(reduced_max); }
  
}

void StMuFcsAnaEpdMatchQa::PaintPointEpdDistQa(TCanvas* canv, const char* savename) const
{
  canv->Clear();
  canv->Divide(2,2);
  canv->cd(1)->SetLogz();
  mH2F_PointProj_nmipVdrtile->Draw("colz");
  mH2F_PointProj_nmipVdrtile->GetYaxis()->SetRangeUser(0,3);
  canv->cd(3)->SetLogz();
  mH2F_PointProj_nmipVdrhit->Draw("colz");
  mH2F_PointProj_nmipVdrhit->GetYaxis()->SetRangeUser(0,3);
  
  canv->cd(2)->SetLogz();
  mH2F_PointProj_nmipVdphitile->Draw("colz");
  mH2F_PointProj_nmipVdphitile->GetYaxis()->SetRangeUser(0,3);
  canv->cd(4)->SetLogz();
  mH2F_PointProj_nmipVdphihit->Draw("colz");
  mH2F_PointProj_nmipVdphihit->GetYaxis()->SetRangeUser(0,3);

  canv->Print(savename);
}

void StMuFcsAnaEpdMatchQa::PaintPointEpdDistQaProj(TCanvas* canv, const char* savename) const
{
  canv->Clear();
  canv->Divide(3,2);

  TH1* pointprojdrtile = ((TH2*)mH2F_PointProj_nmipVdrtile)->ProjectionX("pointprojdrtile",1,mH2F_PointProj_nmipVdrtile->GetNbinsY(),"e");
  TH1* pointprojdrhit = ((TH2*)mH2F_PointProj_nmipVdrhit)->ProjectionX("pointprojdrhit",1,mH2F_PointProj_nmipVdrhit->GetNbinsY(),"e");
  TH1* pointprojdphitile = ((TH2*)mH2F_PointProj_nmipVdphitile)->ProjectionX("pointprojdphitile",1,mH2F_PointProj_nmipVdphitile->GetNbinsY(),"e");
  TH1* pointprojdphihit = ((TH2*)mH2F_PointProj_nmipVdphihit)->ProjectionX("pointprojdphihit",1,mH2F_PointProj_nmipVdphihit->GetNbinsY(),"e");

  canv->cd(1);
  pointprojdrtile->DrawCopy("hist c");
  canv->cd(4);
  pointprojdrhit->DrawCopy("hist c");
  canv->cd(2);
  pointprojdphitile->DrawCopy("hist c");
  canv->cd(5);
  pointprojdphihit->DrawCopy("hist c");
  
  pointprojdrhit->Divide(pointprojdrtile);
  pointprojdphihit->Divide(pointprojdphitile);
  
  canv->cd(3);
  pointprojdrhit->DrawCopy("hist c");
  pointprojdrhit->GetYaxis()->SetRangeUser(0.95,1.15);
  canv->cd(6);
  pointprojdphihit->DrawCopy("hist c");

  canv->Print(savename);

  delete pointprojdrtile;
  delete pointprojdrhit;
  delete pointprojdphitile;
  delete pointprojdphihit;
  
}

void StMuFcsAnaEpdMatchQa::StMuFcsAnaEpdMatchQa::PaintPointEpdDist(TCanvas* canv, const char* savename) const
{
  canv->Clear();
  canv->Divide(2,2);

  canv->cd(1);
  mH1F_PointProjDistToEpdTiles->Draw("hist c");
  
  canv->cd(2);
  mH1F_PointProjDistToEpdHits->Draw("hist c");

  canv->cd(3);
  TH1* h1f_subtract = new TH1F("h1f_subtract","h1f_subtract",200,0,200);
  h1f_subtract->Add(mH1F_PointProjDistToEpdHits,mH1F_PointProjDistToEpdTiles,-1,1);
  h1f_subtract->Draw("hist c");
  
  canv->Print(savename);

  delete h1f_subtract;
}

void StMuFcsAnaEpdMatchQa::PaintProjEpdAdjQa(TCanvas* canv, const char* savename) const
{
  canv->Clear();
  canv->Divide(3,3);
  
  canv->cd(1)->SetLogz();
  mH2F_ClusProjEpdAdj_maxVsum->Draw("colz");
  canv->cd(4)->SetLogz();
  mH2F_PointProjEpdAdj_maxVsum->Draw("colz");

  canv->cd(2)->SetLogz();
  mH2F_ClusProjEpdAdj_maxVtile->Draw("colz");
  canv->cd(5)->SetLogz();
  mH2F_PointProjEpdAdj_maxVtile->Draw("colz");

  canv->cd(3);
  mH1F_ClusProjEpdAdjRedMip->Draw("hist e");
  canv->cd(6);
  mH1F_PointProjEpdAdjRedMip->Draw("hist e");

  canv->cd(7)->SetLogy();
  mH1F_ClusProjEpdAdjRedMax->Draw("hist e");
  canv->cd(8)->SetLogy();
  mH1F_PointProjEpdAdjRedMax->Draw("hist e");
  
  canv->Print(savename);
}

Int_t StMuFcsAnaEpdMatchQa::DrawEpdProjection(StMuFcsAnaData* anadata, TCanvas* canvas, const char* savename)
{
  if( anadata==0 ){ return 0; }

  Double_t usevertex = anadata->mUseVertex;
  Double_t vertexcutlow = anadata->mVertexCutLow;
  Double_t vertexcuthigh = anadata->mVertexCutHigh;
  if( !(vertexcutlow<=usevertex && usevertex<=vertexcuthigh) ){ return 0; }
  
  if( canvas==0  ){ return 0; }
  TClonesArray* PhArr = anadata->getPhArr();
  if( PhArr==0 ){ return 0; }
  StFcsDb* FcsDb = anadata->fcsDb();
  if( FcsDb==0 ){ return 0; }
  StEpdGeom* EpdGeom = anadata->epdGeom();
  if( EpdGeom==0 ){ return 0; }
  Double_t EpdNmipCut = anadata->epdNmipCut();

  canvas->Clear();
  canvas->cd();
  canvas->DrawFrame(-100,-100,100,100);

  //loop over all west epd tiles and make the polygon need to do this every time otherwise root does not draw the fill correctly
  //Don't need to loop over hits to get nmip since that should be stored in mEpdAllNmip
  for(int i_pp=1; i_pp<=12; ++i_pp){     //Supersector runs [1,12]
    for( int i_tt=1; i_tt<=31; ++i_tt ){ //Tile number [1,31]
      std::map<Int_t,TPolyLine*>::iterator epdhitit = mEpdTileMap.find(100*i_pp+i_tt);
      TPolyLine* polyline = 0;
      if( epdhitit==mEpdTileMap.end() ){
	polyline = StMuFcsAnaEpdMatch::EpdTilePoly(EpdGeom,i_pp,i_tt);
	std::pair<std::map<Int_t,TPolyLine*>::iterator,bool> itr = mEpdTileMap.emplace(100*i_pp+i_tt,polyline);
	if( ! (itr.second) ){ std::cout << "Could not add polyline to map" << std::endl; }
      }
      else{ polyline = epdhitit->second; }
      //std::cout << "|i_pp:"<<i_pp<<"|i_tt:"<<i_tt<<"|key:"<<100*i_pp+i_tt << std::endl;
      if( polyline==0 ){ std::cout << "Could not create polyline map element" << std::endl; continue; }
      polyline->SetLineWidth(1);
      polyline->SetLineColor(kBlack);
      polyline->SetFillColorAlpha(kWhite,0);
      float nmip = mAllEpdNmip[i_pp-1][i_tt-1];
      if( 0==nmip ){
	polyline->SetFillColorAlpha(kWhite,0);
      }
      else if( 0<nmip && nmip<EpdNmipCut ){
	polyline->SetFillColorAlpha(kBlue,0.2);
	//std::cout << "  <nmip|ihit:"<<i << "|epdpp:"<<epdpp << "|epdtt:"<<epdtt <<"|nmip:"<<nmip << std::endl;
      }
      else if( nmip>EpdNmipCut ){
	//Int_t color = kOrange+nmip;
	//if( nmip>10 ){ color = kOrange+10; }
	//epdhitit->second->SetFillColorAlpha(kRed,0.2);
	Int_t color = kOrange;
	polyline->SetFillColor(color);
	//std::cout << "  >nmip|ihit:"<<i << "|epdpp:"<<epdpp << "|epdtt:"<<epdtt <<"|nmip:"<<nmip << std::endl;
      }
      else{
	polyline->SetFillColor(kGray);
      }
    }
  }

  //Create boxes for all FCS towers
  std::vector<TBox*> FcsTowers;
  /*
  double zepd = 375.0;
  //double zfcs=710.0+13.90+15.0;
  //double zr = zepd/zfcs;
  //float yoff[kFcsNDet]={100.0,100.0,-100.0,-100.0,100.0,100.0};
  //float xoff[kFcsNDet]={  0.0,  0.0,   0.0,   0.0,  0.0,  0.0};
  for( int idet=0; idet<kFcsNDet; idet++ ){
    if(idet<=kFcsEcalSouthDetId){ //ecal & hcal
      for(int id=0; id<FcsDb->maxId(idet); id++){
	StThreeVectorF xyz = FcsDb->getStarXYZ(idet,id);
	double wx=FcsDb->getXWidth(idet);
	double wy=FcsDb->getYWidth(idet);
	double zr = zepd/xyz.z();
	TBox* cell=new TBox(zr*(xyz.x()-wx/2.0), zr*(xyz.y()-wy/2.0), 
			    zr*(xyz.x()+wx/2.0), zr*(xyz.y()+wy/2.0));
	cell->SetFillColorAlpha(kWhite,0);
	cell->SetLineColorAlpha(kGray,0.5);
	cell->SetLineWidth(1);
	FcsTowers.push_back(cell);
      }
    }
  }
  */ 
  /*std::map<Int_t,bool> fcsepdhits;
  for( unsigned int ihit=mMuFcsColl->indexOfFirstHit(4); ihit<mMuFcsColl->numberOfHits(); ++ihit ){
    StMuFcsHit* fcshit = mMuFcsColl->getHit(ihit);
    unsigned short det = fcshit->detectorId();
    unsigned short id = fcshit->id();
    int pp=0;
    int tt=0;
    FcsDb->getEPDfromId(det,id,pp,tt);
    int fcsepdkey = 100*pp+tt;
    //std::cout << "|det:"<<det <<"|id:"<<id << "|fcsepdkey:"<<fcsepdkey << std::endl;
    if( fcsepdkey!=0 ){ fcsepdhits[fcsepdkey] = false; }
    //fcshit->energy()
  }*/

  //loop over all photon candidates
  std::vector<TMarker*> FcsMarkers;
  std::vector<TEllipse*> FcsPointEllipse;
  for( Int_t iph = 0; iph<PhArr->GetEntriesFast(); ++iph ){
    //std::cout << "|iph:"<<iph << "|iphnew:"<<iph-noldhits << std::endl;
    FcsPhotonCandidate* ph = (FcsPhotonCandidate*) PhArr->UncheckedAt(iph);
    if( ph==0 ){ std::cout << "==========I=CANNOT=BE=ZERO==========" << std::endl; return 0; }
    std::vector<Double_t> epdproj = StMuFcsAnaData::ProjectToEpd(ph->mX,ph->mY,ph->mZ,usevertex);
    //std::cout << "|i:"<<iph <<"|x:"<<epdproj.at(0) << "|y:"<<epdproj.at(1) << std::endl;
    TMarker* fcsmarker = 0;
    TEllipse* ellipse = 0;
    if( ph->mFromCluster ){
      fcsmarker = new TMarker(epdproj.at(0),epdproj.at(1),33);
      fcsmarker->SetMarkerSize(1.5);
    }
    else{
      fcsmarker = new TMarker(epdproj.at(0),epdproj.at(1),30);
      fcsmarker->SetMarkerSize(2);
      //ellipse = new TEllipse(epdproj.at(0),epdproj.at(1),10,10);
      int linesize=1;
      //A bit hacky but should work since size and energy is linearly increasing
      if( ph->mEn>10 ){ linesize=2; }
      if( ph->mEn>20 ){ linesize=3; }
      if( ph->mEn>30 ){ linesize=4; }
      if( ph->mEn>40 ){ linesize=5; }
      if( ph->mEn>50 ){ linesize=6; }
      if( ph->mEn>60 ){ linesize=7; }
      if( ph->mEn>70 ){ linesize=8; }
      if( ph->mEn>80 ){ linesize=9; }
      if( ph->mEn>90 ){ linesize=10; }
      if( ellipse!=0 ){
	ellipse->SetLineWidth(linesize);
	FcsPointEllipse.push_back(ellipse);
      }
    }
    fcsmarker->SetMarkerColor(kBlack);
    
    //loop over all west epd tiles so that even if no hit recorded can use as a veto
    //Supersector runs [1,12]
    //Tile number [1,31]
    for(int i_pp=1; i_pp<=12; ++i_pp){
      for( int i_tt=1; i_tt<=31; ++i_tt ){
	if( ph->mEpdMatch[0] == (100*i_pp+i_tt) ){
	  //if( mEpdGeo->IsInTile(i_pp,i_tt,1,epdproj.at(0),epdproj.at(1)) ){
	  //std::cout << " + |iph:"<<iph << "|nmip:"<<ph->mEpdHitNmip[0] << "|epdkey:"<<100*i_pp+i_tt << std::endl;
	  std::map<Int_t,TPolyLine*>::iterator epdhitit = mEpdTileMap.find(100*i_pp+i_tt);
	  if( epdhitit!=mEpdTileMap.end() ){
	    if( ph->mEpdHitNmip[0]==0 ){
	      epdhitit->second->SetLineColor(kGreen+2);
	      epdhitit->second->SetFillColorAlpha(kGreen,0.5);
	      epdhitit->second->SetLineWidth(2);
	      fcsmarker->SetMarkerColor(kGreen+2);
	      if( ellipse!=0 ){ ellipse->SetLineColor(kGreen+2); }
	    }
	    else if( 0<ph->mEpdHitNmip[0] && ph->mEpdHitNmip[0]<EpdNmipCut ){
	      epdhitit->second->SetLineColor(kBlue);
	      epdhitit->second->SetFillColorAlpha(kBlue,0.5);
	      epdhitit->second->SetLineWidth(2);
	      fcsmarker->SetMarkerColor(kBlue);
	      if( ellipse!=0 ){ ellipse->SetLineColor(kBlue); }
	      //std::cout << "<nmip|iph:"<<iph << "|i_pp:"<<i_pp << "|i_tt:"<<i_tt << "|nmip"<<ph->mEpdHitNmip[0] << std::endl;
	    }
	    else if( ph->mEpdHitNmip[0]>=EpdNmipCut ){
	      epdhitit->second->SetLineColor(kRed);
	      epdhitit->second->SetFillColorAlpha(kRed,0.5);
	      epdhitit->second->SetLineWidth(2);
	      fcsmarker->SetMarkerColor(kRed);
	      if( ellipse!=0 ){ ellipse->SetLineColor(kRed); }
	    }
	    else{
	      epdhitit->second->SetLineColor(kGray);
	      fcsmarker->SetMarkerColor(kGray);
	      if( ellipse!=0 ){ ellipse->SetLineColor(kGray); }
	    }
	  }
	  else{
	    std::cout << "Not in map!" << std::endl;
	  }
	}
      }
    }
    FcsMarkers.push_back(fcsmarker);
  }

  //Draw Epd tiles first so they appear in the background
  for( std::map<Int_t,TPolyLine*>::iterator polyitr=mEpdTileMap.begin(); polyitr!=mEpdTileMap.end(); ++polyitr ){
    //std::cout << "|id:"<<polyitr->first << "|polyline:"<<polyitr->second << std::endl;
    polyitr->second->Draw("f");
    polyitr->second->Draw();
  }
  //Draw photon candidates next on top of the epd tiles
  for( unsigned int i=0; i<FcsMarkers.size(); ++i ){
    FcsMarkers.at(i)->Draw();
  }
  for( unsigned int i=0; i<FcsPointEllipse.size(); ++i ){
    FcsPointEllipse.at(i)->SetFillColorAlpha(kWhite,0);
    FcsPointEllipse.at(i)->Draw();
  }
  for( unsigned int i=0; i<FcsTowers.size(); ++i ){
    FcsTowers.at(i)->Draw("l");
  }

  /*for( std::map<Int_t,bool>::iterator fcsepditr=fcsepdhits.begin(); fcsepditr!=fcsepdhits.end(); ++fcsepditr ){
    if( fcsepditr->second==false ){ std::cout << "|NOHIT:"<<fcsepditr->first << std::endl; }
    }*/

  canvas->Print(savename);
  //Clean memory
  for( unsigned int i=0; i<FcsMarkers.size(); ++i ){
    delete FcsMarkers.at(i);
  }
  for( unsigned int i=0; i<FcsPointEllipse.size(); ++i ){
    delete FcsPointEllipse.at(i);
  }
  for( unsigned int i=0; i<FcsTowers.size(); ++i ){
    delete FcsTowers.at(i);
  }
  return 1;
}

