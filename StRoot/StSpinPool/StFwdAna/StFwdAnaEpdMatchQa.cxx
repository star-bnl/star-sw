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

#include "StFwdAnaEpdMatchQa.h"

ClassImp(StFwdAnaEpdMatchQa)

StFwdAnaEpdMatchQa::StFwdAnaEpdMatchQa()
{
}

StFwdAnaEpdMatchQa::~StFwdAnaEpdMatchQa()
{
  for( auto itr=mEpdTileMap.begin(); itr!=mEpdTileMap.end(); ++itr ){
    delete itr->second;
  }
  mEpdTileMap.clear();
}

UInt_t StFwdAnaEpdMatchQa::LoadHists( TFile* file, HistManager* histman, StFwdAnaData* data )
{
  UInt_t loaded = 0;
  if( histman==0 ){ return loaded; }

  //Turn this on if you ever need to plot the mH2F_ClusBadProjVcut_yVx or mH2F_PointBadProjVcut_yVx histograms with the EPD tiles
  for(int i_pp=1; i_pp<=12; ++i_pp){     //Supersector runs [1,12]
    for( int i_tt=1; i_tt<=31; ++i_tt ){ //Tile number [1,31]
      std::map<Int_t,TPolyLine*>::iterator epdhitit = mEpdTileMap.find(100*i_pp+i_tt);
      TPolyLine* polyline = 0;
      if( epdhitit==mEpdTileMap.end() ){
	polyline = StFwdAnaEpdMatch::EpdTilePoly(data->epdGeom(),i_pp,i_tt);
	polyline = new TPolyLine(polyline->GetN(),polyline->GetX(),polyline->GetY());
	std::pair<std::map<Int_t,TPolyLine*>::iterator,bool> itr = mEpdTileMap.emplace(100*i_pp+i_tt,polyline);
	if( ! (itr.second) ){ std::cout << "Could not add polyline to map" << std::endl; }
      }
      else{
	//polyline = epdhitit->second;
    polyline = epdhitit->second;
      }
      //std::cout << "|i_pp:"<<i_pp<<"|i_tt:"<<i_tt<<"|key:"<<100*i_pp+i_tt << std::endl;
      if( polyline==0 ){ std::cout << "Could not create polyline map element" << std::endl; continue; }
      polyline->SetLineWidth(1);
      polyline->SetLineColor(kBlack);
      polyline->SetFillColorAlpha(kWhite,0);
    }
  }
  
  //std::cout << "StFwdAnaEpdMatchQa::LoadHists()" << std::endl;

  //loaded += histman->AddH2F(file,mH2F_PointProj_nmipVdrtile,"H2F_PointProj_nmipVdrtile","nMIP vs. FCS projected point to EPD, polar distance to all other tiles;dR (cm);nmip", 200,-100,100, 70,0,7);
  //loaded += histman->AddH2F(file,mH2F_PointProj_nmipVdrhit,"H2F_PointProj_nmipVdrhit","nMIP vs. FCS projected point to EPD, polar distance to all other hits;dR (cm);nmip", 200,-100,100, 70,0,7);
  //loaded += histman->AddH2F(file,mH2F_PointProj_nmipVdphitile,"H2F_PointProj_nmipVdphitile","nMIP vs. FCS projected point to EPD, angle difference to all other tiles;d#phi;nmip", 100,-TMath::Pi(),TMath::Pi(), 70,0,7);
  //loaded += histman->AddH2F(file,mH2F_PointProj_nmipVdphihit,"H2F_PointProj_nmipVdphihit","nMIP vs. FCS projected point to EPD, angle difference to all other hits;d#phi;nmip", 100,-TMath::Pi(),TMath::Pi(), 70,0,7);

  //loaded += histman->AddH1F(file,mH1F_PointProjDistToEpdTiles,"H1F_PointProjDistToEpdTiles","Distance of FCS projected point to all EPD tiles;D (cm)", 200,0,200);
  //loaded += histman->AddH1F(file,mH1F_PointProjDistToEpdHits,"H1F_PointProjDistToEpdHits","Distance of FCS projected point to all EPD Hits;D (cm)", 200,0,200);
  
  loaded += histman->AddH2F(file,mH2F_EpdTilesNmip_yVx,"H2F_EpdTilesNmip_yVx","West EPD tile y vs. x weighted by nmip+1 (STAR coordinates);x (cm);y (cm)", 100,-100,100, 100,-100,100);
  loaded += histman->AddH2F(file,mH2F_EpdTilesNmip_rVphi,"H2F_EpdTilesNmip_rVphi","West EPD tile r vs. phi in STAR coordinates;#phi;r", 96,-TMath::Pi(),TMath::Pi(),100,0,100);  //Use 96 bins in phi since opening angle is 7.5 degrees so this is double that in number of bins
  //loaded += histman->AddH2F(file,mH2F_EpdHits_phiVr,"H2F_EpdHits_phiVr","west EPD hits phi vs. r in STAR coordinates;r;#phi", 100,0,100, 100,-TMath::Pi(),TMath::Pi());
  //loaded += histman->AddH2F(file,mH2F_EpdHitsCut_phiVr,"H2F_EpdHitsCuts_phiVr","west EPD hits with nmip cut phi vs. r in STAR coordinates;r;#phi", 100,0,100, 100,-TMath::Pi(),TMath::Pi());
  loaded += histman->AddH1F(file,mH1F_EpdAdjNmip,"H1F_EpdAdjNmip","nmip vs. EPD adjacency",9,0,9);
  std::vector<std::string> adjnames;
  adjnames.push_back("O");
  adjnames.push_back("OCCW");
  adjnames.push_back("CCW");
  adjnames.push_back("ICCW");
  adjnames.push_back("tile");
  adjnames.push_back("I");
  adjnames.push_back("ICW");
  adjnames.push_back("CW");
  adjnames.push_back("OCW");
  for( int i=0; i<9; ++i ){
    mH1F_EpdAdjNmip->GetXaxis()->SetBinLabel(i+1,adjnames.at(i).c_str());
  }
  
  loaded += histman->AddH1F(file,mH1F_ClusNBadEpdProj,"H1F_ClusNBadEpdProj","Number of clusters in an event that did not project back to a valid EPD tile;;",15,0,15);
  loaded += histman->AddH1F(file,mH1F_ClusNBadEpdProjVcut,"H1F_ClusNBadEpdProjVcut","Number of clusters in an event that did not project back to a valid EPD tile with |vertex|<150;;",15,0,15);

  loaded += histman->AddH1F(file,mH1F_PointNBadEpdProj,"H1F_PointNBadEpdProj","Number of points in an event that did not project back to a valid EPD tile;;",15,0,15);
  loaded += histman->AddH1F(file,mH1F_PointNBadEpdProjVcut,"H1F_PointNBadEpdProjVcut","Number of points in an event that did not project back to a valid EPD tile with |vertex|<150;;",15,0,15);

  loaded += histman->AddH2F(file,mH2F_ClusBadProjVcut_yVx,"H2F_ClusBadProjVcut_yVx","Position of clusters that did not project back to a valid EPD tile with vertex cut;x (cm);y (cm)",100,-100,100, 100,-100,100);
  loaded += histman->AddH2F(file,mH2F_PointBadProjVcut_yVx,"H2F_PointBadProjVcut_yVx","Position of points that did not project back to a valid EPD tile with vertex cut;x (cm);y (cm)",100,-100,100, 100,-100,100);

  loaded += histman->AddH2F(file,mH2F_ClusProjEpdAdj_maxVsum,"H2F_ClusProjEpdAdj_maxVsum","FCS projected cluster to EPD tile max vs. sum of adjacent tiles;Sum;Max", 70,0,7 , 70,0,7);
  loaded += histman->AddH2F(file,mH2F_PointProjEpdAdj_maxVsum,"H2F_PointProjEpdAdj_maxVsum","FCS projected point to EPD tile max vs. sum of adjacent tiles;Sum;Max", 70,0,7 , 70,0,7);
  loaded += histman->AddH2F(file,mH2F_ClusProjEpdAdj_tileVsum,"H2F_ClusProjEpdAdj_tileVsum","FCS projected cluster to EPD tile nmip vs. sum of adjacent tiles;Sum;Found", 70,0,7 , 70,0,7);
  loaded += histman->AddH2F(file,mH2F_PointProjEpdAdj_tileVsum,"H2F_PointProjEpdAdj_tileVsum","FCS projected point to EPD tile nmip vs. sum of adjacent tiles;Sum;Found", 70,0,7 , 70,0,7);

  loaded += histman->AddH2F(file,mH2F_ClusProjEpdAdj_maxVtile,"H2F_ClusProjEpdAdj_maxVtile","FCS projected cluster to EPD adjacent tiles nmip max vs. nmip of intersecting tile;Found;Max", 70,0,7 , 70,0,7);
  loaded += histman->AddH2F(file,mH2F_PointProjEpdAdj_maxVtile,"H2F_PointProjEpdAdj_maxVtile","FCS projected point to EPD adjacent tiles nmip max vs. nmip of intersecting tile;Found;Max", 70,0,7 , 70,0,7);

  loaded += histman->AddH1F(file,mH1F_ClusProjEpdAdjRedMip,"H1F_ClusProjEpdAdjRedMip","FCS projected cluster to EPD adjacent tiles max nmip divided by sum of adjacent;max/sum", 70,0,7);
  loaded += histman->AddH1F(file,mH1F_PointProjEpdAdjRedMip,"H1F_PointProjEpdAdjRedMip","FCS projected point to EPD adjacent tiles max nmip divided by sum of adjacent;max/sum", 70,0,7);

  loaded += histman->AddH1F(file,mH1F_ClusProjEpdAdjRedMax,"H1F_ClusProjEpdAdjRedMax","FCS projected cluster to EPD adjacent tiles nmip of intersecting tile divided by max nmip of adjacent tile;Found/Max", 70,0,7 );
  loaded += histman->AddH1F(file,mH1F_PointProjEpdAdjRedMax,"H1F_PointProjEpdAdjRedMax","FCS projected point to EPD adjacent tiles nmip of intersecting tile divided by max nmip of adjacent tile;Found/Max", 70,0,7);

  loaded += histman->AddH1F(file,mH1F_ClusEpdFoundRegion,"H1F_ClusEpdFoundRegion","FCS cluster found region of intersection with EPD tile", 10, -2.5,7.5);
  mH1F_ClusEpdFoundRegion->GetXaxis()->SetBinLabel(1,"NF");
  mH1F_ClusEpdFoundRegion->GetXaxis()->SetBinLabel(2,"tile");
  mH1F_ClusEpdFoundRegion->GetXaxis()->SetBinLabel(3,"O");
  mH1F_ClusEpdFoundRegion->GetXaxis()->SetBinLabel(4,"OCCW");
  mH1F_ClusEpdFoundRegion->GetXaxis()->SetBinLabel(5,"CCW");
  mH1F_ClusEpdFoundRegion->GetXaxis()->SetBinLabel(6,"ICCW");
  mH1F_ClusEpdFoundRegion->GetXaxis()->SetBinLabel(7,"I");
  mH1F_ClusEpdFoundRegion->GetXaxis()->SetBinLabel(8,"ICW");
  mH1F_ClusEpdFoundRegion->GetXaxis()->SetBinLabel(9,"CW");
  mH1F_ClusEpdFoundRegion->GetXaxis()->SetBinLabel(10,"OCW");
  loaded += histman->AddH1F(file,mH1F_PointEpdFoundRegion,"H1F_PointEpdFoundRegion","FCS point found region of intersection with EPD tile", 10, -2.5,7.5);
  mH1F_PointEpdFoundRegion->GetXaxis()->SetBinLabel(1,"NF");
  mH1F_PointEpdFoundRegion->GetXaxis()->SetBinLabel(2,"tile");
  mH1F_PointEpdFoundRegion->GetXaxis()->SetBinLabel(3,"O");
  mH1F_PointEpdFoundRegion->GetXaxis()->SetBinLabel(4,"OCCW");
  mH1F_PointEpdFoundRegion->GetXaxis()->SetBinLabel(5,"CCW");
  mH1F_PointEpdFoundRegion->GetXaxis()->SetBinLabel(6,"ICCW");
  mH1F_PointEpdFoundRegion->GetXaxis()->SetBinLabel(7,"I");
  mH1F_PointEpdFoundRegion->GetXaxis()->SetBinLabel(8,"ICW");
  mH1F_PointEpdFoundRegion->GetXaxis()->SetBinLabel(9,"CW");
  mH1F_PointEpdFoundRegion->GetXaxis()->SetBinLabel(10,"OCW");
  
  loaded += histman->AddH1F(file,mH1F_ClusChargeId, "H1F_ClusChargeId","FCS clusters identified as charge vs. neutral", 2, -0.5,1.5);
  mH1F_ClusChargeId->GetXaxis()->SetBinLabel(1,"Neutral");
  mH1F_ClusChargeId->GetXaxis()->SetBinLabel(2,"Charged");
  loaded += histman->AddH1F(file,mH1F_PointChargeId, "H1F_PointChargeId","FCS points identified as charge vs. neutral", 2, -0.5,1.5);
  mH1F_PointChargeId->GetXaxis()->SetBinLabel(1,"Neutral");
  mH1F_PointChargeId->GetXaxis()->SetBinLabel(2,"Charged");

  return loaded;
}

//----------------------
Int_t StFwdAnaEpdMatchQa::DoMake(StFwdAnaData* anadata)
{
  mH2F_EpdTilesNmip_yVx->Reset();
  mH2F_EpdTilesNmip_rVphi->Reset();
  //mH2F_EpdHits_phiVr->Reset();
  //mH2F_EpdHitsCut_phiVr->Reset();
  
  //std::cout << this->ClassName() << "|Start Make" << std::endl;
  TClonesArray* PhArr = anadata->getPhArr();
  StEpdGeom* EpdGeom = anadata->epdGeom();
  Double_t usevertex = anadata->mUseVertex;
  Double_t vertexcutlow = anadata->mVertexCutLow;
  Double_t vertexcuthigh = anadata->mVertexCutHigh;
  //if( !(vertexcutlow<=usevertex && usevertex<=vertexcuthigh) ){ return kStOk; }
  Double_t epdnmipcut = anadata->mEpdNmipCut;
  TClonesArray* MuEpdHits = 0;
  StEpdCollection* EpdColl = 0;
  anadata->epdColl(MuEpdHits,EpdColl);

  //Loop over all West EPD tiles only for good vertex events to get its distance
  if( vertexcutlow<=usevertex && usevertex<=vertexcuthigh ){
    for(int i_pp=1; i_pp<=12; ++i_pp){     //Supersector runs [1,12]
      for( int i_tt=1; i_tt<=31; ++i_tt ){ //Tile number [1,31]
	TVector3 epdhitxyz = EpdGeom->TileCenter(i_pp,i_tt,1);  //Only project to west side
	//loop over epd hits for distance checking
	Double_t rhit = sqrt(epdhitxyz[0]*epdhitxyz[0] + epdhitxyz[1]*epdhitxyz[1]);
	Double_t phihit = TMath::ATan2(epdhitxyz[1],epdhitxyz[0]);
	((TH2*)mH2F_EpdTilesNmip_yVx)->Fill( epdhitxyz[0], epdhitxyz[1], StFwdAnaEpdMatch::epdNmip(i_pp,i_tt)+1 ); //Scale up by 1 to see empty tiles
	((TH2*)mH2F_EpdTilesNmip_rVphi)->Fill( phihit,rhit, StFwdAnaEpdMatch::epdNmip(i_pp,i_tt)+1 );              //Scale up by 1 to see empty tiles
      }
    }
  }
  /*unsigned int nepdhits = 0;
  StSPtrVecEpdHit* epdhits = 0;
  if( MuEpdHits!=0 ){ nepdhits = MuEpdHits->GetEntriesFast(); }
  else if( EpdColl!=0 ){
    epdhits = &(EpdColl->epdHits());
    nepdhits = epdhits->size();
  }
  else{ LOG_ERROR << "StFwdAnaEpdMatchQa::DoMake() - If you see this error then there is a bug that is setting EPD hits improperly" << endm; return kStErr; }
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
    TVector3 epdhitxyz = EpdGeom->TileCenter(epdpp,epdtt,ew);
    Double_t rhit = sqrt(epdhitxyz[0]*epdhitxyz[0] + epdhitxyz[1]*epdhitxyz[1]);
    Double_t phihit = TMath::ATan2(epdhitxyz[1],epdhitxyz[0]);
    ((TH2*)mH2F_EpdHits_phiVr)->Fill(rhit,phihit,nmip);
    if( nmip > (anadata->epdNmipCut()) ){
      ((TH2*)mH2F_EpdHitsCut_phiVr)->Fill(rhit,phihit,nmip);
    }
  }*/
  
  if( mCanvas!=0 ){
    if( mCanvSaveName.Length()==0 ){
      std::stringstream ss_savename;
      ss_savename << "FcsAna_DistQa_Evt"<<anadata->getEventNum() << ".png";
      //std::cout << ss_savename.str() << std::endl;
      PaintEpdTileHitDistQa(mCanvas,ss_savename.str().c_str());
      ss_savename.str("");
      ss_savename << "FcsAna_DistQaFcs_Evt"<<anadata->getEventNum() << ".png";
      DrawEpdTileHitDistWithFcs(anadata,mCanvas,ss_savename.str().c_str());
    }
    else{
      PaintEpdTileHitDistQa(mCanvas,mCanvSaveName.Data());
    }
  }
  
  //std::cout << this->ClassName() << "|Start:Make_CheckAndSetEpdHit" << std::endl;
  //Check photon candidates if they have any hits in the EPD. Use a separate loop so that this information could be used in the pi0 checking loop if needed. In future may also want to check against FCS preshower (EPD) hits
  Int_t n_clusnoepdproj = 0;
  Int_t n_clusnoepdproj_vcut = 0;
  Int_t n_pointnoepdproj = 0;
  Int_t n_pointnoepdproj_vcut = 0;
  for( Int_t iph = 0; iph<PhArr->GetEntriesFast(); ++iph ){
    //std::cout << "|iph:"<<iph << std::endl;
    StFcsPhotonCandidate* ph = (StFcsPhotonCandidate*) PhArr->UncheckedAt(iph);
    if( ph==0 ){ std::cout << "==========I=CANNOT=BE=ZERO==========" << std::endl; return kStErr; }
    /*if( mCanvas!=0 ){
      mH1F_EpdAdjNmip->Reset();
      int found_pp = ph->mEpdMatch[0]/100;
      int found_tt = ph->mEpdMatch[0] - found_pp*100;
      const int MAX_ADJ = 8;
      int adj_pp[MAX_ADJ];
      int adj_tt[MAX_ADJ];
      StFwdAnaEpdMatch::GetEpdTileOuter(    found_pp, found_tt, adj_pp[0], adj_tt[0] );
      StFwdAnaEpdMatch::GetEpdTileOuterCCW( found_pp, found_tt, adj_pp[1], adj_tt[1] );
      StFwdAnaEpdMatch::GetEpdTileCCW(      found_pp, found_tt, adj_pp[2], adj_tt[2] );
      StFwdAnaEpdMatch::GetEpdTileInnerCCW( found_pp, found_tt, adj_pp[3], adj_tt[3] );
      StFwdAnaEpdMatch::GetEpdTileInner(    found_pp, found_tt, adj_pp[4], adj_tt[4] );
      StFwdAnaEpdMatch::GetEpdTileInnerCW(  found_pp, found_tt, adj_pp[5], adj_tt[5] );
      StFwdAnaEpdMatch::GetEpdTileCW(       found_pp, found_tt, adj_pp[6], adj_tt[6] );
      StFwdAnaEpdMatch::GetEpdTileOuterCW(  found_pp, found_tt, adj_pp[7], adj_tt[7] );

      mH1F_EpdAdjNmip->SetBinContent(1, StFwdAnaEpdMatch::epdNmip(adj_pp[0],adj_tt[0]) );
      mH1F_EpdAdjNmip->SetBinContent(2, StFwdAnaEpdMatch::epdNmip(adj_pp[1],adj_tt[1]) );
      mH1F_EpdAdjNmip->SetBinContent(3, StFwdAnaEpdMatch::epdNmip(adj_pp[2],adj_tt[2]) );
      mH1F_EpdAdjNmip->SetBinContent(4, StFwdAnaEpdMatch::epdNmip(adj_pp[3],adj_tt[3]) );
      mH1F_EpdAdjNmip->SetBinContent(5, ph->mEpdHitNmip[0]);
      mH1F_EpdAdjNmip->SetBinContent(6, StFwdAnaEpdMatch::epdNmip(adj_pp[4],adj_tt[4]) );
      mH1F_EpdAdjNmip->SetBinContent(7, StFwdAnaEpdMatch::epdNmip(adj_pp[5],adj_tt[5]) );
      mH1F_EpdAdjNmip->SetBinContent(8, StFwdAnaEpdMatch::epdNmip(adj_pp[6],adj_tt[6]) );
      mH1F_EpdAdjNmip->SetBinContent(9, StFwdAnaEpdMatch::epdNmip(adj_pp[7],adj_tt[7]) );

      std::stringstream ss_savename;
      ss_savename << "FcsAna_EpdAdjPh_Evt"<<anadata->getEventNum()<< "_Ph"<<iph << ".png";
      mCanvas->Clear();
      mH1F_EpdAdjNmip->Draw("hist e");
      mCanvas->Print(ss_savename.str().c_str());
      }*/
    
    std::vector<Double_t> epdproj = StFwdAnaData::ProjectToEpd(ph->mX,ph->mY,ph->mZ,usevertex);
    
    //Check how many clusters/points didn't have a match
    if( ph->mEpdMatch[0]==0 ){
      if( ph->mFromCluster ){
	++n_clusnoepdproj;
	if( vertexcutlow<=usevertex && usevertex<=vertexcuthigh ){
	  ++n_clusnoepdproj_vcut;
	  //std::cout << "===== EVT:"<<anadata->getEventNum() << " == iPh:"<<iph << " =====" << std::endl;
	  //ph->Print("epd");
	  //std::cout << "    + |projx:"<<epdproj.at(0) << "|projy:"<<epdproj.at(1) << std::endl;
	  mH2F_ClusBadProjVcut_yVx->Fill(epdproj.at(0),epdproj.at(1));
	}
      }
      else{
	++n_pointnoepdproj;
	if( vertexcutlow<=usevertex && usevertex<=vertexcuthigh ){
	  ++n_pointnoepdproj_vcut;
	  mH2F_PointBadProjVcut_yVx->Fill(epdproj.at(0),epdproj.at(1));
	}
      }
    }

    if( ph->mEpdMatch[0]!=0 ){
      //For below QA only use events with a good vertex since it QA's the cut criteria algorithm
      if( vertexcutlow<=usevertex && usevertex<=vertexcuthigh ){
	float nmiptile = ph->mEpdHitNmip[1];
	float nmipmax = ph->mEpdHitAdjMax;
	float nmipsum = ph->mEpdHitNmipSum;
	float reduced_nmip = nmipsum>0 ? nmipmax/nmipsum : 0;  //If sum is zero all adjacent nmips must be zero as well
	float reduced_max = nmipmax>0 ? nmiptile/nmipmax : 0;  //If max is 0 then that means even the found nmip was zero
	//std::cout << "   * |nmipmax:"<<nmipmax << "|nmipsum:"<<nmipsum <<"|red_nmip:"<<nmipmax/nmipsum << std::endl;
	if( ph->mFromCluster ){
	  mH2F_ClusProjEpdAdj_maxVsum->Fill(nmipsum,nmipmax);
	  mH2F_ClusProjEpdAdj_tileVsum->Fill(nmipsum,nmiptile);
	  mH2F_ClusProjEpdAdj_maxVtile->Fill(nmiptile,nmipmax);
	  mH1F_ClusProjEpdAdjRedMip->Fill(reduced_nmip);
	  mH1F_ClusProjEpdAdjRedMax->Fill(reduced_max);
	  mH1F_ClusEpdFoundRegion->Fill(ph->mEpdFoundRegion);
	  if( ph->mEpdHitNmip[0]>epdnmipcut){ mH1F_ClusChargeId->Fill(1); }
	  else{ mH1F_ClusChargeId->Fill(0); }
	}
	else{
	  mH2F_PointProjEpdAdj_maxVsum->Fill(nmipsum,nmipmax);
	  mH2F_PointProjEpdAdj_tileVsum->Fill(nmipsum,nmiptile);
	  mH2F_PointProjEpdAdj_maxVtile->Fill(nmiptile,nmipmax);
	  mH1F_PointProjEpdAdjRedMip->Fill(reduced_nmip);
	  mH1F_PointProjEpdAdjRedMax->Fill(reduced_max);
	  mH1F_PointEpdFoundRegion->Fill(ph->mEpdFoundRegion);
	  if( ph->mEpdFoundRegion==-2 ){
	    ph->Print("epd");
	  }
	  if( ph->mEpdHitNmip[0]>epdnmipcut){ mH1F_PointChargeId->Fill(1); }
	  else{ mH1F_PointChargeId->Fill(0); }
	}
      }
    }

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
      ss_savename << "FcsAna_PointEpdDistQa_"<<ph->mFromCluster << "_Ph"<<iph << "_Evt"<<amadata->getEventNum() << ".png";
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

  mH1F_ClusNBadEpdProj->Fill(n_clusnoepdproj);
  mH1F_ClusNBadEpdProjVcut->Fill(n_clusnoepdproj_vcut);

  mH1F_PointNBadEpdProj->Fill(n_pointnoepdproj);
  mH1F_PointNBadEpdProjVcut->Fill(n_pointnoepdproj_vcut);

  return kStOk;
}

void StFwdAnaEpdMatchQa::PaintBadProjections(TCanvas* canv, const char* savename) const
{
  canv->Clear();

  //canv->Divide(2,2);
  canv->Divide(3,2);

  canv->cd(1)->SetLogy();
  mH1F_ClusNBadEpdProj->Draw("hist e");
  canv->cd(3)->SetLogy();
  mH1F_ClusNBadEpdProjVcut->Draw("hist e");
  canv->cd(5);
  mH2F_ClusBadProjVcut_yVx->Draw("colz");
  for(int i_pp=1; i_pp<=12; ++i_pp){     //Supersector runs [1,12]
    for( int i_tt=1; i_tt<=31; ++i_tt ){ //Tile number [1,31]
      std::map<Int_t,TPolyLine*>::const_iterator epdhitit = mEpdTileMap.find(100*i_pp+i_tt);
      if( epdhitit!=mEpdTileMap.end() ){
	epdhitit->second->Draw("f");
	epdhitit->second->Draw();
	//pad6->PaintPolyLine(polyline->GetN(),polyline->GetX(),polyline->GetY());
      }
      else{ std::cout << "no polyline" << std::endl; }
    }
  }
  
  canv->cd(2)->SetLogy();
  mH1F_PointNBadEpdProj->Draw("hist e");
  canv->cd(4)->SetLogy();
  mH1F_PointNBadEpdProjVcut->Draw("hist e");
  canv->cd(6);
  mH2F_PointBadProjVcut_yVx->Draw("colz");
  for(int i_pp=1; i_pp<=12; ++i_pp){     //Supersector runs [1,12]
    for( int i_tt=1; i_tt<=31; ++i_tt ){ //Tile number [1,31]
      std::map<Int_t,TPolyLine*>::const_iterator epdhitit = mEpdTileMap.find(100*i_pp+i_tt);
      if( epdhitit!=mEpdTileMap.end() ){
	epdhitit->second->Draw("f");
	epdhitit->second->Draw();
	//pad6->PaintPolyLine(polyline->GetN(),polyline->GetX(),polyline->GetY());
      }
      else{ std::cout << "no polyline" << std::endl; }
    }
  }
  
  canv->Print(savename);
}

void StFwdAnaEpdMatchQa::PaintPointEpdDistQa(TCanvas* canv, const char* savename) const
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

void StFwdAnaEpdMatchQa::PaintPointEpdDistQaProj(TCanvas* canv, const char* savename) const
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

void StFwdAnaEpdMatchQa::PaintEpdTileHitDistQa(TCanvas* canv, const char* savename) const
{
  canv->Clear();

  canv->Divide(2,2);

  canv->cd(1);
  mH2F_EpdTilesNmip_yVx->SetStats(0);
  mH2F_EpdTilesNmip_yVx->SetMinimum(1);
  mH2F_EpdTilesNmip_yVx->SetMaximum(5);
  mH2F_EpdTilesNmip_yVx->Draw("colz");
  canv->cd(2);
  //mH2F_EpdHits_phiVr->Draw("colz");
  //mH2F_EpdTilesNmip_yVx->Draw("pol colz");
  mH2F_EpdTilesNmip_rVphi->SetStats(0);
  mH2F_EpdTilesNmip_rVphi->SetMinimum(1);
  mH2F_EpdTilesNmip_rVphi->SetMaximum(5);
  mH2F_EpdTilesNmip_rVphi->Draw("colz");
  //canv->cd(3);
  //mH2F_EpdHitsCut_phiVr->Draw("colz");

  canv->Print(savename);
}

Int_t StFwdAnaEpdMatchQa::DrawEpdTileHitDistWithFcs(StFwdAnaData* anadata, TCanvas* canvas, const char* savename)
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

  //anadata->getEvtInfo()->Print();

  canvas->Clear();
  //mH2F_EpdTilesNmip_yVx->SetStats(0);
  //mH2F_EpdTilesNmip_yVx->SetMinimum(1);
  //mH2F_EpdTilesNmip_yVx->SetMaximum(5);
  //mH2F_EpdTilesNmip_yVx->Draw("colz2");
  mH2F_EpdTilesNmip_rVphi->SetStats(0);
  mH2F_EpdTilesNmip_rVphi->SetMinimum(1);
  mH2F_EpdTilesNmip_rVphi->SetMaximum(5);
  mH2F_EpdTilesNmip_rVphi->Draw("colz2");
  //((TH2*)mH2F_EpdHits_phiVr)->SetMinimum(0);
  //((TH2*)mH2F_EpdHits_phiVr)->SetMaximum(4);
  //mH2F_EpdHits_phiVr->Draw("colz");
  //loop over all west epd tiles and make the polygon need to do this every time otherwise root does not draw the fill correctly
  //Don't need to loop over hits to get nmip since that should be stored in mEpdAllNmip
  for(int i_pp=1; i_pp<=12; ++i_pp){     //Supersector runs [1,12]
    for( int i_tt=1; i_tt<=31; ++i_tt ){ //Tile number [1,31]
      std::map<Int_t,TPolyLine*>::iterator epdhitit = mEpdTileMap.find(100*i_pp+i_tt);
      TPolyLine* polyline = 0;
      TPolyLine* polyline_rVphi = 0;
      if( epdhitit==mEpdTileMap.end() ){
	polyline = StFwdAnaEpdMatch::EpdTilePoly(EpdGeom,i_pp,i_tt);
	Int_t nvals = polyline->GetN();
	Double_t* xvals = polyline->GetX();
	Double_t* yvals = polyline->GetY();
	Double_t philine[6] = {0};  //Max number of corners in EPD is 5 so need one more than that
	Double_t rline[6] = {0};
	for( int i=0; i<nvals; ++i ){
	  philine[i] = TMath::ATan2(yvals[i],xvals[i]);
	  rline[i] = sqrt( xvals[i]*xvals[i] + yvals[i]*yvals[i] );	  
	}
	polyline_rVphi = new TPolyLine(nvals,philine,rline);
	//std::pair<std::map<Int_t,TPolyLine*>::iterator,bool> itr = mEpdTileMap.emplace(100*i_pp+i_tt,polyline);
	std::pair<std::map<Int_t,TPolyLine*>::iterator,bool> itr = mEpdTileMap.emplace(100*i_pp+i_tt,polyline_rVphi);
	if( ! (itr.second) ){ std::cout << "Could not add polyline to map" << std::endl; }
	delete polyline;
	polyline = 0;

      }
      else{
	//polyline = epdhitit->second;
	polyline_rVphi = epdhitit->second;
      }
      //std::cout << "|i_pp:"<<i_pp<<"|i_tt:"<<i_tt<<"|key:"<<100*i_pp+i_tt << std::endl;
      if( polyline_rVphi==0 ){ std::cout << "Could not create polyline map element" << std::endl; continue; }
      polyline_rVphi->SetLineWidth(1);
      polyline_rVphi->SetLineColor(kBlack);
      polyline_rVphi->SetFillColorAlpha(kWhite,0);
    }
  }

  //loop over all photon candidates
  std::vector<TMarker*> FcsMarkers;
  std::vector<int> EpdTilesToDraw;
  for( Int_t iph = 0; iph<PhArr->GetEntriesFast(); ++iph ){
    //std::cout << "|iph:"<<iph << "|iphnew:"<<iph-noldhits << std::endl;
    StFcsPhotonCandidate* ph = (StFcsPhotonCandidate*) PhArr->UncheckedAt(iph);
    if( ph==0 ){ std::cout << "==========I=CANNOT=BE=ZERO==========" << std::endl; return 0; }
    //Draw all "matched" tiles
    for( short i=1; i<5; ++i ){
      if( ph->mEpdMatch[i]!=0 ){
	EpdTilesToDraw.push_back(ph->mEpdMatch[i]);
      }
      auto itr = mEpdTileMap.find(ph->mEpdMatch[0]);
      if( itr!=mEpdTileMap.end() ){
	if( ph->mEpdHitNmip[0]>anadata->mEpdNmipCut ){ itr->second->SetLineColor(kRed); }
	else{ itr->second->SetLineColor(kBlue); }
      }
    }
    std::vector<Double_t> epdproj = StFwdAnaData::ProjectToEpd(ph->mX,ph->mY,ph->mZ,usevertex);
    //Checking with BBC vertex
    //std::vector<Double_t> epdproj = StFwdAnaData::ProjectToEpd(ph->mX,ph->mY,ph->mZ,anadata->getEvtInfo()->mBbcVz);
    //StFwdAnaEpdMatch::CheckInsideEpdTile( EpdGeom, ph, epdproj.at(0), epdproj.at(1) );
    //std::cout << "|i:"<<iph <<"|x:"<<epdproj.at(0) << "|y:"<<epdproj.at(1) << std::endl;
    Double_t rphoton = sqrt( (epdproj.at(0)*epdproj.at(0)) + (epdproj.at(1)*epdproj.at(1)) );
    Double_t phiphoton = TMath::ATan2(epdproj.at(1),epdproj.at(0));
    TMarker* fcsmarker = 0;
    if( ph->mFromCluster ){
      fcsmarker = new TMarker(phiphoton,rphoton,33);
      //fcsmarker = new TMarker(epdproj.at(0),epdproj.at(1),33);
      //fcsmarker = new TMarker(ph->mX,ph->mY,33);
      fcsmarker->SetMarkerSize(1.5);
    }
    else{
      fcsmarker = new TMarker(phiphoton,rphoton,30);
      //fcsmarker = new TMarker(epdproj.at(0),epdproj.at(1),30);
      //fcsmarker = new TMarker(ph->mX,ph->mY,30);
      fcsmarker->SetMarkerSize(2);
    }
    fcsmarker->SetMarkerColor(kBlack);
    //ph->Print("epd");
    //Set colors based on best nmip
    //Double_t RedMip= ph->mEpdHitNmip[0];
    Double_t RedMip= ph->mEpdHitAdjMax;
    //if( ph->mEpdHitAdjMax>0.00001 ){
    //if( ph->mEpdHitNmipSum>0.00001 ){
      //RedMip = ph->mEpdHitAdjMax/ph->mEpdHitNmipSum;
      //RedMip = ph->mEpdHitNmip[0]/ph->mEpdHitAdjMax;
      //RedMip = ph->mEpdHitNmip[0]/ph->mEpdHitNmipSum;
      //RedMip = ph->mEpdHitNmip[0];
    //}
    if( RedMip < EpdNmipCut  ){
      //fcsmarker->SetMarkerColor(kGreen+2);
    }
    else{
      //fcsmarker->SetMarkerColor(kRed);
    }
    
    FcsMarkers.push_back(fcsmarker);
  }

  //Draw Epd tiles first so they appear in the background
  /*for( std::map<Int_t,TPolyLine*>::iterator polyitr=mEpdTileMap.begin(); polyitr!=mEpdTileMap.end(); ++polyitr ){
    //std::cout << "|id:"<<polyitr->first << "|polyline:"<<polyitr->second << std::endl;
    polyitr->second->Draw("f");
    polyitr->second->Draw();
  }*/
  for( unsigned int i=0; i<EpdTilesToDraw.size(); ++i ){
    auto itr = mEpdTileMap.find( EpdTilesToDraw.at(i) );
    if( itr!=mEpdTileMap.end() ){
      itr->second->Draw("f");
      itr->second->Draw();
    }
  }

  //Draw photon candidates next on top of the epd tiles
  canvas->cd();
  for( unsigned int i=0; i<FcsMarkers.size(); ++i ){
    FcsMarkers.at(i)->Draw();
  }

  /*for( std::map<Int_t,bool>::iterator fcsepditr=fcsepdhits.begin(); fcsepditr!=fcsepdhits.end(); ++fcsepditr ){
    if( fcsepditr->second==false ){ std::cout << "|NOHIT:"<<fcsepditr->first << std::endl; }
    }*/
    
  canvas->Print(savename);
  //Clean memory
  for( unsigned int i=0; i<FcsMarkers.size(); ++i ){
    delete FcsMarkers.at(i);
  }
  return 1;
}

void StFwdAnaEpdMatchQa::PaintPointEpdDist(TCanvas* canv, const char* savename) const
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

void StFwdAnaEpdMatchQa::PaintProjEpdAdjQa(TCanvas* canv, const char* savename) const
{
  canv->Clear();
  canv->Divide(3,4);
  
  canv->cd(1)->SetLogz();
  mH2F_ClusProjEpdAdj_maxVsum->Draw("colz");
  canv->cd(4)->SetLogz();
  mH2F_PointProjEpdAdj_maxVsum->Draw("colz");

  canv->cd(2)->SetLogz();
  mH2F_ClusProjEpdAdj_tileVsum->Draw("colz");
  canv->cd(5)->SetLogz();
  mH2F_PointProjEpdAdj_tileVsum->Draw("colz");

  canv->cd(3)->SetLogz();
  mH2F_ClusProjEpdAdj_maxVtile->Draw("colz");
  canv->cd(6)->SetLogz();
  mH2F_PointProjEpdAdj_maxVtile->Draw("colz");

  canv->cd(7)->SetLogy();
  mH1F_ClusProjEpdAdjRedMip->Draw("hist e");
  canv->cd(10)->SetLogy();
  mH1F_PointProjEpdAdjRedMip->Draw("hist e");

  canv->cd(8)->SetLogy();
  mH1F_ClusProjEpdAdjRedMax->Draw("hist e");
  canv->cd(11)->SetLogy();
  mH1F_PointProjEpdAdjRedMax->Draw("hist e");
  
  canv->Print(savename);
}

void StFwdAnaEpdMatchQa::PaintFoundChargeId(TCanvas* canv, const char* savename) const
{
  canv->Clear();
  canv->Divide(2,2);

  canv->cd(1);
  mH1F_ClusEpdFoundRegion->Draw("hist e");
  canv->cd(2);
  mH1F_ClusChargeId->Draw("hist e");

  canv->cd(3);
  mH1F_PointEpdFoundRegion->Draw("hist e");
  canv->cd(4);
  mH1F_PointChargeId->Draw("hist e");

  canv->Print(savename);
}

Int_t StFwdAnaEpdMatchQa::DrawEpdProjection(StFwdAnaData* anadata, TCanvas* canvas, const char* savename)
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
	polyline = StFwdAnaEpdMatch::EpdTilePoly(EpdGeom,i_pp,i_tt);
	std::pair<std::map<Int_t,TPolyLine*>::iterator,bool> itr = mEpdTileMap.emplace(100*i_pp+i_tt,polyline);
	if( ! (itr.second) ){ std::cout << "Could not add polyline to map" << std::endl; }
      }
      else{ polyline = epdhitit->second; }
      //std::cout << "|i_pp:"<<i_pp<<"|i_tt:"<<i_tt<<"|key:"<<100*i_pp+i_tt << std::endl;
      if( polyline==0 ){ std::cout << "Could not create polyline map element" << std::endl; continue; }
      polyline->SetLineWidth(1);
      polyline->SetLineColor(kBlack);
      polyline->SetFillColorAlpha(kWhite,0);
      float nmip = StFwdAnaEpdMatch::epdNmip(i_pp,i_tt);
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
  for( unsigned int ihit=mFwdColl->indexOfFirstHit(4); ihit<mFwdColl->numberOfHits(); ++ihit ){
    StFwdHit* fcshit = mFwdColl->getHit(ihit);
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
    StFcsPhotonCandidate* ph = (StFcsPhotonCandidate*) PhArr->UncheckedAt(iph);
    if( ph==0 ){ std::cout << "==========I=CANNOT=BE=ZERO==========" << std::endl; return 0; }
    std::vector<Double_t> epdproj = StFwdAnaData::ProjectToEpd(ph->mX,ph->mY,ph->mZ,usevertex);
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

