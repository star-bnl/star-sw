#include "StEnumerations.h"
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

#include "StFwdAnaEpdMatch.h"

ClassImp(StFwdAnaEpdMatch)

std::map<Int_t,TPolyLine*> StFwdAnaEpdMatch::mEpdCcwLines;
float StFwdAnaEpdMatch::mAllEpdNmip[12][31] = {0};

StFwdAnaEpdMatch::StFwdAnaEpdMatch()
{
}

StFwdAnaEpdMatch::~StFwdAnaEpdMatch()
{
  for( auto itr=mEpdCcwLines.begin(); itr!=mEpdCcwLines.end(); ++itr ){
    delete itr->second;
  }
  mEpdCcwLines.clear();
}

UInt_t StFwdAnaEpdMatch::LoadHists(TFile* file, HistManager* histman, StFwdAnaData* data)
{
  UInt_t loaded = 0;
  if( histman==0 ){ return loaded; }

  loaded += histman->AddH2F(file,mH2F_EpdProjHitMap,"H2F_EpdProjHitMap","Distribution of x,y projections of photon candidates onto STAR EPD plane;x (cm);y (cm)", 300,-150,150, 200,-100,100);
  loaded += histman->AddH2F(file,mH2F_EpdProjHitMap_Vcut,"H2F_EpdProjHitMap_Vcut","Distribution of x,y projections of photon candidates onto STAR EPD plane with |vertex|<150cm;x (cm);y (cm)", 300,-150,150, 200,-100,100);
  loaded += histman->AddH2F(file,mH2F_EpdNmip,"H2F_EpdNmip","Distribution of nmip values from matching projected clusters and points;;Nmip",2,0,2, 70,0,7);
  mH2F_EpdNmip->GetXaxis()->SetBinLabel(1,"Clusters");
  mH2F_EpdNmip->GetXaxis()->SetBinLabel(2,"Points");

  return loaded;
}

Int_t StFwdAnaEpdMatch::DoMake(StFwdAnaData* anadata)
{
  memset(mAllEpdNmip,0,sizeof(mAllEpdNmip));
  
  TClonesArray* PhArr = anadata->getPhArr();
  StEpdGeom* EpdGeom = anadata->epdGeom();
  Double_t usevertex = anadata->mUseVertex;
  Double_t vertexcutlow = anadata->mVertexCutLow;
  Double_t vertexcuthigh = anadata->mVertexCutHigh;
  TClonesArray* MuEpdHits = 0;
  StEpdCollection* EpdColl = 0;
  anadata->epdColl(MuEpdHits,EpdColl);  

  //Check photon candidates if they have any hits in the EPD. Use a separate loop so that this information could be used in the pi0 checking loop if needed. In future may also want to check against FCS preshower (EPD) hits
  //Int_t npoints = ntotal - anadata->mEvtInfo->mClusterSize;
  //First loop over all hits and set values of global nmip array
  unsigned int nepdhits = 0;
  StSPtrVecEpdHit* epdhits = 0;
  if( MuEpdHits!=0 ){ nepdhits = MuEpdHits->GetEntriesFast(); }
  else if( EpdColl!=0 ){
    epdhits = &(EpdColl->epdHits());
    nepdhits = epdhits->size();
  }
  else{ LOG_ERROR << "StFwdAnaEpdMatch::FillEpdinfo() - If you see this error then there is a bug that is setting EPD hits improperly" << endm; return kStErr; }
  StMuEpdHit* muepdhit = 0;
  StEpdHit* epdhit = 0;
  Int_t nepdwesthits = 0;
  for(unsigned int i=0; i<nepdhits; ++i ){
    if( MuEpdHits!=0 ){ muepdhit = (StMuEpdHit*)MuEpdHits->UncheckedAt(i); } //To match similar in StMuDstMaker->epdHit(int i)
    else if( epdhits!=0 ){ epdhit = (StEpdHit*)((*epdhits)[i]); }
    else{ LOG_ERROR << "IF YOU SEE THIS ERROR THEN THERE IS A VERY SERIOUS BUG IN THE CODE" << endm; return kStErr; } 
    //std::cout << "|i:"<<i << "|muepdhit:"<<muepdhit << "|epdhit:"<<epdhit << std::endl;
    int ew    = muepdhit!=0 ? muepdhit->side()    : epdhit->side();      //east=-1, west=1
    if( ew==-1 ){ continue; }                                            //Skip hits in the east since FCS is only on the west
    ++nepdwesthits;
    int epdpp = muepdhit!=0 ? muepdhit->position(): epdhit->position();  //Supersector runs [1,12]
    int epdtt = muepdhit!=0 ? muepdhit->tile()    : epdhit->tile();      //Tile number [1,31]
    //int adc = muepdhit!=0 ? muepdhit->adc() : epdhit->adc();
    float nmip = muepdhit!=0 ? muepdhit->nMIP(): epdhit->nMIP();         //The ADC value of the hit divided by the MIP peak position; e.g. if nmip==1 then adc value sits at the MIP peak
    mAllEpdNmip[epdpp-1][epdtt-1] = nmip;
    //std::cout << "|epdpp:"<<epdpp <<"|epdtt:"<<epdtt <<"|nmip:"<<nmip << std::endl;
    //std::cout << "|epdz:"<<epdhitxyz[2] << std::endl;
  }

  //Loop over all photon candidates and run #CheckInsideEpdTile() algorithm which relies on nmip values from hit loop
  Int_t ntotal = PhArr->GetEntriesFast();
  for( Int_t iph = 0; iph<ntotal; ++iph ){
    //std::cout << "|iph:"<<iph << "|iphnew:"<<iph-noldhits << std::endl;
    StFcsPhotonCandidate* ph = (StFcsPhotonCandidate*) PhArr->UncheckedAt(iph);
    if( ph==0 ){ std::cout << "==========I=CANNOT=BE=ZERO==========" << std::endl; return kStErr; }
    std::vector<Double_t> epdproj = StFwdAnaData::ProjectToEpd(ph->mX,ph->mY,ph->mZ,usevertex);
    
    mH2F_EpdProjHitMap->Fill( epdproj.at(0),epdproj.at(1) );
    if( vertexcutlow<=usevertex && usevertex<=vertexcuthigh ){ mH2F_EpdProjHitMap_Vcut->Fill(epdproj.at(0),epdproj.at(1)); }
    //std::cout << " + |phx:"<<ph->mX << "|phy:"<<ph->mY << "|phz:"<<ph->mZ << "|v:"<<usevertex << std::endl;
    //std::cout << " + |epdx:"<<epdproj.at(0) << "|epdy:"<<epdproj.at(1) << "|epdz:"<<epdproj.at(2) << std::endl;
    //std::cout << " ** |iph:"<< iph-noldhits << std::endl;
    StFwdAnaEpdMatch::CheckInsideEpdTile(EpdGeom, ph,epdproj.at(0),epdproj.at(1));  //Function that will check which EPD tiles photon candidate overlaps with and sets the appropriate variables for it
    //std::cout << "=====DEEPDEBUG=====:"<<((ph->mFromCluster)?0:1) << "|" << ph->mEpdHitNmip[0] << std::endl;
    //std::cout << "=====DEEPDEBUG=====:"<< ((ph->mFromCluster)?0:1) << "|" << ph->mEpdHitNmip[0] << std::endl;
    mH2F_EpdNmip->Fill( ((ph->mFromCluster)?0:1),ph->mEpdHitNmip[0] );
  }
  //std::cout << "|nold:"<<noldhits << "|nnew:"<<nnewhits << "|ntotal:"<<ntotal << "|oldvert:"<<mOldVertex << "|newvert:"<<mUseVertex<< "|nepdhits:"<<nepdwesthits <<"|noldpoints:"<<mNOldPoints << "|npoints:"<<npoints << std::endl;

  //std::cout << "|clustersize:"<<clustersize << "|ncandidates:"<<ncandidates << "|npoints:"<<npoints << std::endl;
  return kStOk;
}

void StFwdAnaEpdMatch::CheckInsideEpdTile(StEpdGeom* epdgeo, StFcsPhotonCandidate* photon, Double_t projx, Double_t projy )
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
	TPolyLine* epd_outerccw = EpdCCWOuterCorner(epdgeo,i_pp,i_tt);
	if( TMath::IsInside( projx, projy, epd_outerccw->GetN(), epd_outerccw->GetX(), epd_outerccw->GetY() ) ){
	  //TPolyLine* epd_ccw = EpdCCWOuterCorner(i_pp,i_tt);
	  photon->mEpdHitNmip[ccwcounter] = 0;
	  photon->mEpdMatch[ccwcounter] = 100*i_pp+i_tt;
	  //std::cout << "     - |ccwcounter:"<<ccwcounter << "|nmip:"<< photon->mEpdHitNmip[ccwcounter] << "|epdkey:"<<photon->mEpdMatch[ccwcounter] << std::endl;
	  ++ccwcounter;
	}
      }
    }
  }
  
  int ncorners = 0;
  for( int icorner=1; icorner<5; ++icorner ){
    //std::cout << " + |projx:"<<projx << "|projy:"<<projy << "|epdkey:"<<photon->mEpdHitNmip[icorner] << std::endl;
    if( photon->mEpdMatch[icorner]!=0 ){ ++ncorners; }
  }

  if( ncorners==1 ){
    //If only 1 corner found take that as the found corner, note that if a in match tile was found ncorners would be zero since OuterCCW would never get checked
    photon->mEpdMatch[0] = photon->mEpdMatch[1];
    photon->mEpdHitNmip[0] = photon->mEpdHitNmip[1];
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
	photon->mEpdHitNmip[icorner] = mAllEpdNmip[epdpp-1][epdtt-1];
	//std::cout << "|("<<epdhitxyz.x() << ","<<epdhitxyz.y() <<")|dx:"<<distx << "|dy:"<< disty << "|dist:"<<dist;
	if( dist<mindist ){ bestcorner = icorner; }
      }
      //else{std::cout << "|("<<0 << ","<<0 <<")"; }
      //std::cout << std::endl;
    }
    //std::cout << "   + |ncorners:"<<ncorners << std::endl;
    if( bestcorner!=0 ){
      photon->mEpdMatch[0] = photon->mEpdMatch[bestcorner];
      photon->mEpdHitNmip[0] = 0;  //0 means intersection found
    }
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
  /*else{
    std::cout << "---------- EpdMatch=0 ----------" << std::endl;
    photon->Print("epd");
    }*/
  if( found_pp!=0 && found_tt!=0 ){
    //Grab all adjacencies then check their mip values
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
    //if( nmipmax>nmipsum ){ std::cout << "   * |nmiptile:"<<nmiptile << "|nmipmax:"<<nmipmax << "|nmipsum:"<<nmipsum << std::endl; }

    photon->mEpdHitNmipSum = nmipsum;
    photon->mEpdHitAdjMax = nmipmax;

    //Part 2 of the algorithm is to pick the maximum based on some intersecting region
    //projx,projy dot with tilex,tiley
    TVector3 epdhitxyz = epdgeo->TileCenter(found_pp,found_tt,1); //Only care about west tiles
    Double_t tilex = epdhitxyz.x();
    Double_t tiley = epdhitxyz.y();
    Double_t rpoint = sqrt( projx*projx + projy*projy );
    Double_t rtile = sqrt( tilex*tilex + tiley*tiley );
    Double_t rdiff = rpoint - rtile;                              //Difference in r that needs to be checked
    Double_t pointdottile = projx*tilex + projy*tiley;
    //Dot product for angle difference
    Double_t CosTheta = pointdottile / (rpoint*rtile);
    Double_t phidiff = TMath::ACos( CosTheta );                   //Need possible positive, negative
    //Cross product for angle direction, both vectors lie in plane so their z-components are zero therefore only need z-component of cross product. By using tile X point; a positive z-component means you need to go CCW from the tile to get to the point, and negative means go CW to go from the tile to the point. This was chosen to match CCW and CW adjacency rules for EPD
    Double_t phidir = (tilex*projy) - (tiley*projx);
    if( phidir<0 ){ phidiff = -1.0*phidiff; } //If negative cross product make angle difference negative for checks below
    
    //According to the #StEpdGeom::GetCorners() the opening angle for tile corners is 7.5 degrees so tile center is presumably half that and so take half of that to get the minimum angle as 1.875 degrees. Convert to radians to match radian angle unit above
    Double_t phidiffmin = -7.5/4.0 * TMath::Pi()/180.0;  //minimum phi difference between point and tile center to consider the photon in tile
    Double_t phidiffmax = 7.5/4.0 * TMath::Pi()/180.0;   //maximum phi difference between point and tile center to consider the photon in tile
    //Tiles 1, 2, 3 in the EPD have an r gap of 4.4 cm and the others have a gap of 5.53 cm so take a quarter of that to form the +- range to check
    Double_t rdiffmin = found_tt<=3 ? -4.4/4.0 : -5.53/4.0;  //minimum r difference between point and tile center to consider photon in tile
    Double_t rdiffmax = found_tt<=3 ? 4.4/4.0 : 5.53/4.0;    //maximum r difference between point and tile center to consider photon in tile
    //std::cout << "|rpoint:"<<rpoint << "|rtile:"<<rtile << "|phipoint:"<<TMath::ATan2(projy,projx)*180.0/TMath::Pi() << "|phitile:"<<TMath::ATan2(tiley,tilex)*180.0/TMath::Pi() << std::endl;
    //std::cout << "  + |rdiff:"<<rdiff << "|phidiff:"<<phidiff*180.0/TMath::Pi() << "|phidir:"<<phidir << std::endl;
    //std::cout << "  + |rmin:"<<rdiffmin << "|rmax:"<<rdiffmax << "|phimin:"<<phidiffmin*180.0/TMath::Pi() << "|phimax:"<<phidiffmax*180.0/TMath::Pi() << std::endl;
    //Do this so the logic is separated from the action and thus code is easier to read
    short foundregion = -2;   //-1=tile, 0=outer, 1=outerccw, 2=ccw, 3=innerccw, 4=inner, 5=innercw, 6=cw, 7=outercw; chosen to match array above
    if( rdiffmin<=rdiff && rdiff<=rdiffmax && phidiffmin<=phidiff && phidiff<=phidiffmax ){ foundregion = -1; }  //In tile
    else if( rdiffmax<rdiff && phidiffmin<=phidiff && phidiff<=phidiffmax )               { foundregion = 0; }   //Outer adjacency
    else if( rdiffmax<rdiff && phidiffmax<phidiff )                                       { foundregion = 1; }   //CCW is positive in this convention so want phidiff larger than maximum
    else if( rdiffmin<=rdiff && rdiff<=rdiffmax && phidiffmax<phidiff )                   { foundregion = 2; }   //Take the CCW tile
    else if( rdiffmin>rdiff && phidiffmax<phidiff )                                       { foundregion = 3; }   //Take InnerCCW, CCW, Inner
    else if( rdiffmin>rdiff && phidiffmin<=phidiff && phidiff<=phidiffmax )               { foundregion = 4; }   //Take Inner
    else if( rdiffmin>rdiff && phidiffmin>phidiff )                                       { foundregion = 5; }   //Take inner CW
    else if( rdiffmin<=rdiff && rdiff<=rdiffmax && phidiffmin>phidiff )                   { foundregion = 6; }   //Take CW
    else if( rdiffmax<rdiff && phidiffmin>phidiff )                                       { foundregion = 7; }   //Outer CW
    else{ foundregion = -2; }
    photon->mEpdFoundRegion = foundregion;
    //Take max of nmip between intersecting and outer tile (set ncorner 0 to max, corner 1 with nmip of found, corner 2 with outer tile nmip, the rest zero
    //if( rmax<rdiff && phimaxrad<phidiff ) //take max nmip of outerCCW, CCW, and outer
    //if( rmin<rdiff && rdiff<rmax && phimaxrad<phidiff )  // take the CCW tile
    //if( rmin>rdiff && phidiff<phiminrad ) //Take InnerCCW, CCW, Inner
    //if( rmin>rdiff && phiminrad<phidiff && phidiff<phimaxrad ) //Take Inner
    //if( rmin>rdiff && phiminrad>phidiff ) //Take InnerCW, CW, and Inner
    //if( rmin<rdiff && rdiff<rmax && phiminrad>phidiff ) //Take CW
    //if( rmax<rdiff && phiminrad>phidiff ) //Take OuterCW, CW, and Outer
    //
    switch( foundregion ){
    case -1:{
      photon->mEpdMatch[0]=100*found_pp+found_tt;
      photon->mEpdHitNmip[0] = nmiptile;
      photon->mEpdMatch[1]=100*found_pp+found_tt;
      photon->mEpdHitNmip[1] = nmiptile;
      for(short i=2; i<5; ++i ){ photon->mEpdMatch[i]=0; photon->mEpdHitNmip[i]=-1; }
      break;}
    case 0:{
      float nmipouter = epdNmip(adj_pp[0],adj_tt[0]);  //index 0 is the outer adjacency as defined above
      if( nmiptile>=nmipouter ){
	photon->mEpdMatch[0] = 100*found_pp + found_tt;
	photon->mEpdHitNmip[0] = nmiptile;
      }
      else{
	photon->mEpdMatch[0]=100*adj_pp[0]+adj_tt[0];
	photon->mEpdHitNmip[0] = nmipouter;
      }
      photon->mEpdMatch[1]=100*found_pp+found_tt;
      photon->mEpdHitNmip[1] = nmiptile;
      photon->mEpdMatch[2]=100*adj_pp[0]+adj_tt[0];
      photon->mEpdHitNmip[2] = nmipouter;
      for(short i=3; i<5; ++i ){ photon->mEpdMatch[i]=0; photon->mEpdHitNmip[i]=-1; }
      break;}
    case 1:{
      float nmipouter = epdNmip(adj_pp[0],adj_tt[0]);     //index 0 is the outer adjacency as defined above
      float nmipouterccw = epdNmip(adj_pp[1],adj_tt[1]);  //index 1 is the outer CCW adjacency as defined above
      float nmipccw = epdNmip(adj_pp[2],adj_tt[2]);       //index 2 is the CCW adjacency as defined above
      if( nmiptile>=nmipouter && nmiptile>=nmipouterccw && nmiptile>=nmipccw ){
	photon->mEpdMatch[0] = 100*found_pp+found_tt;
	photon->mEpdHitNmip[0] = nmiptile;
      }
      else if( nmipouter>=nmiptile && nmipouter>=nmipouterccw && nmipouter>=nmipccw ){
	photon->mEpdMatch[0] = 100*adj_pp[0]+adj_tt[0];
	photon->mEpdHitNmip[0] = nmipouter;
      }
      else if( nmipouterccw>=nmiptile && nmipouterccw>=nmipouter && nmipouterccw>=nmipccw ){
	photon->mEpdMatch[0] = 100*adj_pp[1]+adj_tt[1];
	photon->mEpdHitNmip[0] = nmipouterccw;
      }
      else{
	photon->mEpdMatch[0] = 100*adj_pp[2]+adj_tt[2];
	photon->mEpdHitNmip[0] = nmipccw;
      }	
      photon->mEpdMatch[1]=100*found_pp+found_tt;
      photon->mEpdHitNmip[1] = nmiptile;
      photon->mEpdMatch[2]=100*adj_pp[0]+adj_tt[0];
      photon->mEpdHitNmip[2] = nmipouter;
      photon->mEpdMatch[3]=100*adj_pp[1]+adj_tt[1];
      photon->mEpdHitNmip[3] = nmipouterccw;
      photon->mEpdMatch[4]=100*adj_pp[2]+adj_tt[2];
      photon->mEpdHitNmip[4] = nmipccw;
      break;}
    case 2:{
      float nmipccw = epdNmip(adj_pp[2],adj_tt[2]);
      if( nmiptile>=nmipccw ){
	photon->mEpdMatch[0] = 100*found_pp + found_tt;
	photon->mEpdHitNmip[0] = nmiptile;
      }
      else{
	photon->mEpdMatch[0]=100*adj_pp[2]+adj_tt[2];
	photon->mEpdHitNmip[0] = nmipccw;
      }
      photon->mEpdMatch[1]=100*found_pp+found_tt;
      photon->mEpdHitNmip[1] = nmiptile;
      photon->mEpdMatch[2]=100*adj_pp[2]+adj_tt[2];
      photon->mEpdHitNmip[2] = nmipccw;
      for(short i=3; i<5; ++i ){ photon->mEpdMatch[i]=0; photon->mEpdHitNmip[i]=-1; }
      break;}
    case 3:{
      float nmipccw = epdNmip(adj_pp[2],adj_tt[2]);       //index 2 is the CCW adjacency as defined above
      float nmipinnerccw = epdNmip(adj_pp[3],adj_tt[3]);  //index 3 is the inner CCW adjacency as defined above
      float nmipinner = epdNmip(adj_pp[4],adj_tt[4]);     //index 4 is the inner adjacency as defined above      
      if( nmiptile>=nmipccw && nmiptile>=nmipinnerccw && nmiptile>=nmipinner ){
	photon->mEpdMatch[0] = 100*found_pp+found_tt;
	photon->mEpdHitNmip[0] = nmiptile;
      }
      else if( nmipccw>=nmiptile && nmipccw>=nmipinnerccw && nmipccw>=nmipinner ){
	photon->mEpdMatch[0] = 100*adj_pp[2]+adj_tt[2];
	photon->mEpdHitNmip[0] = nmipccw;
      }
      else if( nmipinnerccw>=nmiptile && nmipinnerccw>=nmipccw && nmipinnerccw>=nmipinner ){
	photon->mEpdMatch[0] = 100*adj_pp[3]+adj_tt[3];
	photon->mEpdHitNmip[0] = nmipinnerccw;
      }
      else{
	photon->mEpdMatch[0] = 100*adj_pp[4]+adj_tt[4];
	photon->mEpdHitNmip[0] = nmipinner;
      }
      photon->mEpdMatch[1]=100*found_pp+found_tt;
      photon->mEpdHitNmip[1] = nmiptile;
      photon->mEpdMatch[2]=100*adj_pp[2]+adj_tt[2];
      photon->mEpdHitNmip[2] = nmipccw;
      photon->mEpdMatch[3]=100*adj_pp[3]+adj_tt[3];
      photon->mEpdHitNmip[3] = nmipinnerccw;
      photon->mEpdMatch[4]=100*adj_pp[4]+adj_tt[4];
      photon->mEpdHitNmip[4] = nmipinner;
      break;}
    case 4:{
      float nmipinner = epdNmip(adj_pp[4],adj_tt[4]);
      if( nmiptile>=nmipinner ){
	photon->mEpdMatch[0] = 100*found_pp + found_tt;
	photon->mEpdHitNmip[0] = nmiptile;
      }
      else{
	photon->mEpdMatch[0]=100*adj_pp[4]+adj_tt[4];
	photon->mEpdHitNmip[0] = nmipinner;
      }
      photon->mEpdMatch[1]=100*found_pp+found_tt;
      photon->mEpdHitNmip[1] = nmiptile;
      photon->mEpdMatch[2]=100*adj_pp[4]+adj_tt[4];
      photon->mEpdHitNmip[2] = nmipinner;
      for(short i=3; i<5; ++i ){ photon->mEpdMatch[i]=0; photon->mEpdHitNmip[i]=-1; }
      break;}
    case 5:{
      float nmipinner = epdNmip(adj_pp[4],adj_tt[4]);    //index 4 is the inner adjacency as defined above
      float nmipinnercw = epdNmip(adj_pp[5],adj_tt[5]);  //index 5 is the inner CW adjacency as defined above
      float nmipcw = epdNmip(adj_pp[6],adj_tt[6]);       //index 6 is the inner adjacency as defined above      
      if( nmiptile>=nmipinner && nmiptile>=nmipinnercw && nmiptile>=nmipcw ){
	photon->mEpdMatch[0] = 100*found_pp+found_tt;
	photon->mEpdHitNmip[0] = nmiptile;
      }
      else if( nmipinner>=nmiptile && nmipinner>=nmipinnercw && nmipinner>=nmipcw ){
	photon->mEpdMatch[0] = 100*adj_pp[4]+adj_tt[4];
	photon->mEpdHitNmip[0] = nmipinner;
      }
      else if( nmipinnercw>=nmiptile && nmipinnercw>=nmipinner && nmipinnercw>=nmipcw ){
	photon->mEpdMatch[0] = 100*adj_pp[5]+adj_tt[5];
	photon->mEpdHitNmip[0] = nmipinnercw;
      }
      else{
	photon->mEpdMatch[0] = 100*adj_pp[6]+adj_tt[6];
	photon->mEpdHitNmip[0] = nmipcw;
      }
      photon->mEpdMatch[1]=100*found_pp+found_tt;
      photon->mEpdHitNmip[1] = nmiptile;
      photon->mEpdMatch[2]=100*adj_pp[4]+adj_tt[4];
      photon->mEpdHitNmip[2] = nmipinner;
      photon->mEpdMatch[3]=100*adj_pp[5]+adj_tt[5];
      photon->mEpdHitNmip[3] = nmipinnercw;
      photon->mEpdMatch[4]=100*adj_pp[6]+adj_tt[6];
      photon->mEpdHitNmip[4] = nmipcw;
      break;}
    case 6:{
      float nmipcw = epdNmip(adj_pp[6],adj_tt[6]);
      if( nmiptile>=nmipcw ){
	photon->mEpdMatch[0] = 100*found_pp + found_tt;
	photon->mEpdHitNmip[0] = nmiptile;
      }
      else{
	photon->mEpdMatch[0]=100*adj_pp[6]+adj_tt[6];
	photon->mEpdHitNmip[0] = nmipcw;
      }
      photon->mEpdMatch[1]=100*found_pp+found_tt;
      photon->mEpdHitNmip[1] = nmiptile;
      photon->mEpdMatch[2]=100*adj_pp[6]+adj_tt[6];
      photon->mEpdHitNmip[2] = nmipcw;
      for(short i=3; i<5; ++i ){ photon->mEpdMatch[i]=0; photon->mEpdHitNmip[i]=-1; }
      break;}
    case 7:{
      float nmipcw = epdNmip(adj_pp[6],adj_tt[6]);          //index 6 is the CW adjacency as defined above
      float nmipoutercw = epdNmip(adj_pp[7],adj_tt[7]);     //index 7 is the outer CW adjacency as defined above
      float nmipouter = epdNmip(adj_pp[0],adj_tt[0]);       //index 0 is the outer adjacency as defined above      
      if( nmiptile>=nmipcw && nmiptile>=nmipoutercw && nmiptile>=nmipouter ){
	photon->mEpdMatch[0] = 100*found_pp+found_tt;
	photon->mEpdHitNmip[0] = nmiptile;
      }
      else if( nmipcw>=nmiptile && nmipcw>=nmipoutercw && nmipcw>=nmipouter ){
	photon->mEpdMatch[0] = 100*adj_pp[6]+adj_tt[6];
	photon->mEpdHitNmip[0] = nmipcw;
      }
      else if( nmipoutercw>=nmiptile && nmipoutercw>=nmipcw && nmipoutercw>=nmipouter ){
	photon->mEpdMatch[0] = 100*adj_pp[7]+adj_tt[7];
	photon->mEpdHitNmip[0] = nmipoutercw;
      }
      else{
	photon->mEpdMatch[0] = 100*adj_pp[0]+adj_tt[0];
	photon->mEpdHitNmip[0] = nmipouter;
      }
      photon->mEpdMatch[1]=100*found_pp+found_tt;
      photon->mEpdHitNmip[1] = nmiptile;
      photon->mEpdMatch[2]=100*adj_pp[6]+adj_tt[6];
      photon->mEpdHitNmip[2] = nmipcw;
      photon->mEpdMatch[3]=100*adj_pp[7]+adj_tt[7];
      photon->mEpdHitNmip[3] = nmipoutercw;
      photon->mEpdMatch[4]=100*adj_pp[0]+adj_tt[0];
      photon->mEpdHitNmip[4] = nmipouter;
      break;}
    default:{
      photon->Print("epd");
      //std::cout << "==========" << std::endl;
      //for(short i=0; i<5; ++i ){ photon->mEpdMatch[i]=0; photon->mEpdHitNmip[i]=-1; }
      break;}
    }
    //Check if maximum from adjacency check matches check from max adjacency?
    //photon->Print("epd");
    //std::cout << "==========" << std::endl;
  }//if( found_pp!=0 && found_tt!=0 )
  /*else{
    std::cout << "---------- No Match Found!----------" << std::endl;
    photon->Print();
    }*/
}

std::vector<Int_t> StFwdAnaEpdMatch::GetAdjacentEpdIds(Int_t pp,Int_t tt)
{
  std::vector<Int_t> adjtiles;
  //Start from top tile and go counterclockwise
  Int_t adjpp = 0;
  Int_t adjtt = 0;
  GetEpdTileOuter(pp,tt,adjpp,adjtt);
  if( adjpp==0 && adjtt==0 ){ adjtiles.push_back(100*adjpp+adjtt); }
  GetEpdTileOuterCCW(pp,tt,adjpp,adjtt);
  if( adjpp==0 && adjtt==0 ){ adjtiles.push_back(100*adjpp+adjtt); }
  GetEpdTileCCW(pp,tt,adjpp,adjtt);
  if( adjpp==0 && adjtt==0 ){ adjtiles.push_back(100*adjpp+adjtt); }
  GetEpdTileInnerCCW(pp,tt,adjpp,adjtt);
  if( adjpp==0 && adjtt==0 ){ adjtiles.push_back(100*adjpp+adjtt); }
  GetEpdTileInner(pp,tt,adjpp,adjtt);
  if( adjpp==0 && adjtt==0 ){ adjtiles.push_back(100*adjpp+adjtt); }
  GetEpdTileInnerCW(pp,tt,adjpp,adjtt);
  if( adjpp==0 && adjtt==0 ){ adjtiles.push_back(100*adjpp+adjtt); }
  GetEpdTileCW(pp,tt,adjpp,adjtt);
  if( adjpp==0 && adjtt==0 ){ adjtiles.push_back(100*adjpp+adjtt); }
  GetEpdTileOuterCW(pp,tt,adjpp,adjtt);
  if( adjpp==0 && adjtt==0 ){ adjtiles.push_back(100*adjpp+adjtt); }

  return adjtiles;
}

void StFwdAnaEpdMatch::GetEpdTileOuter(Int_t pp, Int_t tt, Int_t& newpp, Int_t& newtt)
{
  newpp = 0; newtt=0;    //Catch all just in case
  if( 2<=tt && tt<=29 ){
    newpp = pp;
    newtt = tt+2;      //Outer tile is tt+2 for all but extreme cases
  }
  else{
    if( tt==30 ){ newpp=0; newtt=0; }  //Outermost tile
    if( tt==31 ){ newpp=0; newtt=0; }  //Outermost tile
    if( tt==1  ){ newpp=pp; newtt=2; }  ////tile going out is 2 (old:Nothing going "out" but OuterCCW and OuterCW will return 2 and 3)
  }
}

void StFwdAnaEpdMatch::GetEpdTileOuterCCW(Int_t pp, Int_t tt, Int_t& newpp, Int_t& newtt)
{
  newpp = 0; newtt=0;    //Catch all just in case
  if( 2<=tt && tt<=29 ){
    if( tt%2==0 ){
      newpp = pp+1;
      if(newpp==13){ newpp=1; }  //loop back to pp 1 for pp 12
      newtt = tt+3;              //above CCW for even is tt+3 and pp+1
    }
    else{
      newpp = pp;
      newtt = tt+1;              //above CCW for odd is tt+1
    }
  }
  else{
    if( tt==30 ){ newpp=0; newtt=0; }  //Outermost tile
    if( tt==31 ){ newpp=0; newtt=0; }  //Outermost tile
    if( tt==1  ){ newpp=pp+1;if(newpp==13){newpp=1;} newtt=3; } //Outer and going CCW is tile 3 and one over (old:2)
  }
}

void StFwdAnaEpdMatch::GetEpdTileCCW(Int_t pp, Int_t tt, Int_t& newpp, Int_t& newtt)
{
  newpp = 0; newtt=0;    //Catch all just in case
  if( 2<=tt && tt<=31 ){
    if( tt%2==0 ){
      newpp = pp+1;
      if(newpp==13){ newpp=1; }  //loop back to pp 1 for pp 12
      newtt = tt+1;              //CCW  tile for even is tt+1 and pp+1
    }
    else{
      newpp = pp;
      newtt = tt-1;  //CCW tile for odd is tt-1
    }
  }
  else{
    if( tt==1 ){ newpp=pp+1; if(newpp==13){ newpp=1; } newtt=1; }
  }
}
  
void StFwdAnaEpdMatch::GetEpdTileInnerCCW(Int_t pp, Int_t tt, Int_t& newpp, Int_t& newtt)
{
  newpp = 0; newtt=0;    //Catch all just in case
  if( 4<=tt && tt<=31 ){
    if( tt%2==0 ){
      newpp = pp+1;
      if(newpp==13){ newpp=1; }  //loop back to pp 1 for pp 12
      newtt = tt-1;              //Inner CCW tile for even is tt-1 and pp+1
    }
    else{
      newpp = pp;
      newtt=tt-3;                  //Inner CCW for odd is tt-3
    }
  }
  else{
    if( tt==3 ){ newpp=0;  newtt=0; }                               //There is no inner CCW (old:Inner CCW for tile 3 is tile in this supersector because it is the "most" CCW)
    if( tt==2 ){ newpp = pp+1; if(newpp==13){ newpp=1; }  newtt=1; } //Inner CCW for tile 2 is next supersector over  tile 1 since it forms a nice corner there
    if( tt==1 ){ newpp=0;  newtt=0; }  //Innermost tile
  }
}

void StFwdAnaEpdMatch::GetEpdTileInner(Int_t pp, Int_t tt, Int_t& newpp, Int_t& newtt)
{
  newpp = 0; newtt=0;    //Catch all just in case
  if( 4<=tt && tt<=31 ){
    newpp = pp;
    newtt = tt-2;
  }
  else{
    if( tt==3 ){ newpp=0;  newtt=0; }  //Nothing more inner but will be handled by InnerCCW and InnerCW
    if( tt==2 ){ newpp=pp;  newtt=1; }  //to match Outer (old:Nothing more inner but will be handled by InnerCCW and InnerCW)
    if( tt==1 ){ newpp=0;  newtt=0; }  //Innermost tile
  }
}

void StFwdAnaEpdMatch::GetEpdTileInnerCW(Int_t pp, Int_t tt, Int_t &newpp, Int_t& newtt)
{
  newpp = 0; newtt=0;    //Catch all just in case
  if( 4<=tt && tt<=31 ){
    if( tt%2==0 ){
      newpp = pp;
      newtt = tt-1;        //Inner CW for even is tt-1
    }
    else{
      newpp = pp-1;
      if( newpp==0 ){ newpp=12; }  //loop back to 12 for pp 1
      newtt = tt-3;                //Inner CW for odd is tt-3 and pp-1
    }
  }
  else{
    if( tt==3 ){ newpp = pp-1; if( newpp==0 ){ newpp=12; } newtt=1; }  //InnerCW is tile 1 of next supersector over since it forms a nice corner
    if( tt==2 ){ newpp = 0; newtt = 0; }                              //No inner CW (old:InnerCW is tile 1 of this supersector since it is "most" clockwise)
    if( tt==1 ){ newpp=0;  newtt=0; }  //Innermost tile
  }
}

void StFwdAnaEpdMatch::GetEpdTileCW(Int_t pp, Int_t tt, Int_t& newpp, Int_t& newtt)
{
  newpp = 0; newtt=0;    //Catch all just in case
  if( 2<=tt && tt<=31 ){
    if( tt%2==0 ){
      newpp = pp;
      newtt = tt+1;     //CW for even is tt+1
    }
    else{
      newpp = pp-1;
      if( newpp==0 ){ newpp=12; } //loop back to 12 for pp 1
      newtt = tt-1;        //CW tile for odd is tt-1 and pp-1
    }
  }
  else{
    if( tt==1 ){ newpp = pp-1; if( newpp==0 ){ newpp=12; } newtt = 1; }
  }
}

void StFwdAnaEpdMatch::GetEpdTileOuterCW(Int_t pp, Int_t tt, Int_t& newpp, Int_t& newtt)
{
  newpp = 0; newtt=0;    //Catch all just in case
  if( 2<=tt && tt<=29 ){
    if( tt%2==0 ){
      newpp = pp;
      newtt = tt+3;     //Outer CW for even is tt+3
    }
    else{
      newpp = pp-1;
      if( newpp==0 ){ newpp=12; } //loop back to 12 for pp 1
      newtt = tt+1;        //Outer CW tile for odd is tt+1 and pp-1
    }
  }
  else{
    if( tt==30 ){ newpp=0; newtt=0; }  //Outermost tile
    if( tt==31 ){ newpp=0; newtt=0; }  //Outermost tile
    if( tt==1 ){  newpp=pp; newtt=3; }  //Outmost tile going CW is 3
  }
}

void StFwdAnaEpdMatch::GetEpdPPandTTFromId(Int_t id, Int_t& pp, Int_t& tt)
{
  pp = id/100;  //Automatically should floor to lowest positive integer in implicity conversion
  tt = id%100;  
}


TPolyLine* StFwdAnaEpdMatch::EpdTilePoly(StEpdGeom* epdgeo, short pp, short tt)
{
  if( epdgeo==0 ){ return 0; }
  double x[5] = {0};
  double y[5] = {0};
  int ncorners = 0;
  short eastwest = 1;   //1 means EPD west
  epdgeo->GetCorners(pp,tt,eastwest,&ncorners,x,y);
  if( ncorners==0 ){ return 0; }
  std::vector<double> xvals;
  std::vector<double> yvals;
  for( int i=0; i<ncorners; ++i ){
    xvals.emplace_back(x[i]);
    yvals.emplace_back(y[i]);
    if( i==ncorners-1 ){
      xvals.emplace_back(x[0]);
      yvals.emplace_back(y[0]);
    }
  }
  //std::cout << "|pp:"<<pp << "|tt:"<<tt << "|n:"<<ncorners << "|";
  /*for( unsigned int j=0; j<xvals.size(); ++j ){
    std::cout << "("<<xvals.at(j) << ","<<yvals.at(j) << ")|";
    }*/
  //std::cout << std::endl;
  TPolyLine* polyline = new TPolyLine(xvals.size(),xvals.data(),yvals.data());  //Equal sizes so shouldn't matter which is used
  return polyline;
}


//Makes the 2x2 
TPolyLine* StFwdAnaEpdMatch::EpdCCWOuterCorner(StEpdGeom* epdgeo, short pp, short tt)
{
  Int_t epdkey = 100*pp+tt;
  auto itr = mEpdCcwLines.find(epdkey);
  if( itr!=mEpdCcwLines.end() ){ return itr->second; }  //return if already exists in map
  
  //std::vector<Int_t> adjaenttiles = StFwdAnaEpdMatch::GetAdjacentEpdIds(pp,tt);
  //std::vector<TPolyLine*> alllines;
  //alllines.push_back(EpdTilePoly(pp,tt)); //Start with center tile
  TPolyLine* main = EpdTilePoly(epdgeo, pp,tt);
  Int_t nmain = main->GetN();
  Double_t* xmain = main->GetX();
  Double_t* ymain = main->GetY();
  std::list<double> xvals;
  std::list<double> yvals;
  for( int i=0; i<nmain-1; ++i ){
    xvals.push_back(xmain[i]);
    yvals.push_back(ymain[i]);
  }
  std::list<double>::iterator xitr = xvals.begin();
  std::list<double>::iterator yitr = yvals.begin();
  /*
  for( ; xitr!=xvals.end() && yitr!=yvals.end(); ++xitr, ++yitr ){
    std::cout << " + |x:"<< *xitr << "|y:"<<*yitr << std::endl;
    }*/
  bool CCWIstt1 = false;
  xitr = xvals.begin();
  yitr = yvals.begin();
  Int_t adjpp = 0;
  Int_t adjtt = 0;
  StFwdAnaEpdMatch::GetEpdTileCCW(pp,tt,adjpp,adjtt);
  //std::cout << "CCW|adjpp:"<<adjpp << "|adjtt:"<<adjtt << std::endl;
  if( adjpp!=0 && adjtt!=0 ){
    //Know there is something in the counter clockwise direction
    TPolyLine* adjline = EpdTilePoly(epdgeo,adjpp,adjtt);
    Int_t nadj = adjline->GetN();
    Double_t* adjxvals = adjline->GetX();
    Double_t* adjyvals = adjline->GetY();
    int lastcorner = 3;  //For rectangular tiles this is the index to use
    //std::cout << "|nadj:"<<nadj << std::endl;
    if( nadj==6 ){
      //For tile pp1
      CCWIstt1 = true;
      lastcorner = 4;  //For tt1 which is pentagonal this is the index to use
    }
    //if( nadj==5 ){  //5 is actually 4 corners since last element is the same as the start
      //std::cout << "HERECCW:"<<adjxvals[0] <<"|"<<adjyvals[0] << std::endl;
      //Since "insert" will insert before the iterator advance to second corner which is the outer CCW corner
      std::advance(xitr,1);
      std::advance(yitr,1);
      //Want to add CW inner edge first and then inner CCW edge
      xvals.insert(xitr,adjxvals[lastcorner]);
      yvals.insert(yitr,adjyvals[lastcorner]);
      xvals.insert(xitr,adjxvals[0]);
      yvals.insert(yitr,adjyvals[0]);
      xvals.insert(xitr,adjxvals[1]);
      yvals.insert(yitr,adjyvals[1]);
      xvals.insert(xitr,adjxvals[2]);
      yvals.insert(yitr,adjyvals[2]);
      if( CCWIstt1 ){
	//Add extra corner for tt1
	xvals.insert(xitr,adjxvals[3]);
	yvals.insert(yitr,adjyvals[3]);
      }
      //}
    delete adjline;
    
    /*for( std::list<double>::iterator xit=xvals.begin(), yit=yvals.begin(); xit!=xvals.end() && yit!=yvals.end(); ++xit, ++yit ){
      std::cout << " + |x:"<< *xit << "|y:"<<*yit << std::endl;
      }*/
  }
  StFwdAnaEpdMatch::GetEpdTileOuter(pp,tt,adjpp,adjtt);
  //std::cout << "Outer|adjpp:"<<adjpp << "|adjtt:"<<adjtt << std::endl;
  if( adjpp!=0 && adjtt!=0 ){
    //Erase point on this corner
    //std::cout << "  - |x:"<<*xitr << "|y:"<<*yitr << std::endl;
    xitr = xvals.erase(xitr); //Gets set to next element (corner)
    yitr = yvals.erase(yitr);
    //std::cout << "AFTERERASE" << std::endl;
    /*for( std::list<double>::iterator xit=xvals.begin(), yit=yvals.begin(); xit!=xvals.end() && yit!=yvals.end(); ++xit, ++yit ){
      std::cout << " + |x:"<< *xit << "|y:"<<*yit << std::endl;
      }*/
    --xitr;   //Go back to previous corner
    --yitr;
    //std::cout << "  - |x:"<<*xitr << "|y:"<<*yitr << std::endl;
    //Know there is something above so add the points accordingly
    TPolyLine* adjline = EpdTilePoly(epdgeo,adjpp,adjtt);
    Int_t nadj = adjline->GetN();
    Double_t* adjxvals = adjline->GetX();
    Double_t* adjyvals = adjline->GetY();
    if( nadj==5 ){
      //std::cout << "HEREOUTER:"<<adjxvals[0] <<"|"<<adjyvals[0] << std::endl;
      //Since "insert" will insert before the iterator advance to third corner of original tile
      std::advance(xitr,1);
      std::advance(yitr,1);
      xvals.insert(xitr,adjxvals[0]);
      yvals.insert(yitr,adjyvals[0]);
      xvals.insert(xitr,adjxvals[1]);
      yvals.insert(yitr,adjyvals[1]);
      xvals.insert(xitr,adjxvals[2]);
      yvals.insert(yitr,adjyvals[2]);
      xvals.insert(xitr,adjxvals[3]);
      yvals.insert(yitr,adjyvals[3]);
    }
    else{ std::cout << "MAJOR ERROR:TILE1 CANNOT BE AN OUTER TILE" << std::endl; }
    delete adjline;
    /*
    for( std::list<double>::iterator xit=xvals.begin(), yit=yvals.begin(); xit!=xvals.end() && yit!=yvals.end(); ++xit, ++yit ){
      std::cout << " + |x:"<< *xit << "|y:"<<*yit << std::endl;
      }*/
  }
  StFwdAnaEpdMatch::GetEpdTileOuterCCW(pp,tt,adjpp,adjtt);
  //std::cout << "OuterCCW|adjpp:"<<adjpp << "|adjtt:"<<adjtt << std::endl;
  if( adjpp!=0 && adjtt!=0 ){
    //Know there is something in the outer CCW position
    TPolyLine* adjline = EpdTilePoly(epdgeo,adjpp,adjtt);
    Int_t nadj = adjline->GetN();
    Double_t* adjxvals = adjline->GetX();
    Double_t* adjyvals = adjline->GetY();
    if( nadj==5 ){
      //std::cout << "HEREOUTERCCW:"<<adjxvals[0] <<"|"<<adjyvals[0] << std::endl;
      xitr = xvals.begin();
      yitr = yvals.begin();
       //Get to the points related to the inner corner and remove them
      std::advance(xitr,4);
      std::advance(yitr,4);
      //std::cout << "  - |x:"<<*xitr << "|y:"<<*yitr << std::endl;
      xitr = xvals.erase(xitr); //Gets set to next element (corner)
      yitr = yvals.erase(yitr);
      //std::cout << "AFTERERASE1" << std::endl;
      //std::cout << "  - |x:"<<*xitr << "|y:"<<*yitr << std::endl;
      xitr = xvals.erase(xitr);
      yitr = yvals.erase(yitr);
      if( CCWIstt1 ){
	//Delete extra corner when tt1 was added CCW
	xitr = xvals.erase(xitr);
	yitr = yvals.erase(yitr);
      }
      //std::cout << "AFTERERASE2" << std::endl;
      //std::cout << "  - |x:"<<*xitr << "|y:"<<*yitr << std::endl;
      /*for( std::list<double>::iterator xit=xvals.begin(), yit=yvals.begin(); xit!=xvals.end() && yit!=yvals.end(); ++xit, ++yit ){
	std::cout << " + |x:"<< *xit << "|y:"<<*yit << std::endl;
	}*/
      //Iterator has moved to next element which also needs to be deleted
      //Since "insert" will insert before the iterator it is now pointing to correct "top left corner"
      //Want to add clockwise edge first
      xvals.insert(xitr,adjxvals[0]);
      yvals.insert(yitr,adjyvals[0]);
      xvals.insert(xitr,adjxvals[1]);
      yvals.insert(yitr,adjyvals[1]);
      xvals.insert(xitr,adjxvals[2]);
      yvals.insert(yitr,adjyvals[2]);
      //xvals.push_back(adjxvals[3]); //Don't insert last point which is now inside the shape
      //yvals.push_back(adjyvals[3]);
    }
    delete adjline;
  }

  std::vector<double> newxvals;
  std::vector<double> newyvals;
  xitr=xvals.begin();
  yitr=yvals.begin();
  //std::cout << "=====final=====" << std::endl;
  for( ; xitr!=xvals.end() && yitr!=yvals.end(); ++xitr, ++yitr ){
    newxvals.push_back(*xitr);
    newyvals.push_back(*yitr);
    //std::cout << " + |x:"<< *xitr << "|y:"<<*yitr << std::endl;
  }
  //Add first element back in to close the polygon
  xitr = xvals.begin();
  yitr = yvals.begin();
  newxvals.push_back(*xitr);
  newyvals.push_back(*yitr);
    
  delete main;
  TPolyLine* newline = new TPolyLine(newxvals.size(),newxvals.data(),newyvals.data());  //Equal sizes so shouldn't matter which is used
  auto result = mEpdCcwLines.emplace(epdkey,newline);
  if( !(result.second) ){ std::cout << "If you see this ERROR,, something went totally wrong and For some reason you tried to insert an EPD Tile Outer CCW that already exists" << std::endl; }
  return newline;
}

TPolyLine* StFwdAnaEpdMatch::EpdCWOuterCorner(StEpdGeom* epdgeo, short pp, short tt)
{
  int newtt = 0;
  int newpp = 0;
  StFwdAnaEpdMatch::GetEpdTileCW(pp,tt,newpp,newtt);
  //std::cout << "|newpp:"<<newpp << "|newtt:"<<newtt << std::endl;
  if( newtt!=0 && newpp!=0 ){ return EpdCCWOuterCorner(epdgeo, newpp,newtt); }
  else{ return 0; }
}

TPolyLine* StFwdAnaEpdMatch::EpdCCWInnerCorner(StEpdGeom* epdgeo, short pp, short tt)
{
  int newtt = 0;
  int newpp = 0;
  StFwdAnaEpdMatch::GetEpdTileInner(pp,tt,newpp,newtt);
  //std::cout << "|pp:"<<pp << "|tt:"<<tt << "|newpp:"<<newpp << "|newtt:"<<newtt << std::endl;
  if( newpp!=0 && newtt!=0 ){ return EpdCCWOuterCorner(epdgeo,newpp,newtt); }
  else{
    if( tt==3 ){
      //Special for tile 3 to group tile 1, 2 and 3 together
      TPolyLine* tt1 = EpdTilePoly(epdgeo,pp,1);
      TPolyLine* tt2 = EpdTilePoly(epdgeo,pp,2);
      TPolyLine* tt3 = EpdTilePoly(epdgeo,pp,3);
      Double_t* xtt1 = tt1->GetX();
      Double_t* ytt1 = tt1->GetY();
      Double_t* xtt2 = tt2->GetX();
      Double_t* ytt2 = tt2->GetY();
      Double_t* xtt3 = tt3->GetX();
      Double_t* ytt3 = tt3->GetY();
      std::vector<double> xvals;
      std::vector<double> yvals;
      //Manually adding corners
      xvals.push_back(xtt1[0]);
      yvals.push_back(ytt1[0]);
      xvals.push_back(xtt1[1]);
      yvals.push_back(ytt1[1]);
      xvals.push_back(xtt2[0]);
      yvals.push_back(ytt2[0]);
      xvals.push_back(xtt2[1]);
      yvals.push_back(ytt2[1]);
      xvals.push_back(xtt2[2]);
      yvals.push_back(ytt2[2]);
      xvals.push_back(xtt3[1]);
      yvals.push_back(ytt3[1]);
      xvals.push_back(xtt3[2]);
      yvals.push_back(ytt3[2]);
      xvals.push_back(xtt3[3]);
      yvals.push_back(ytt3[3]);
      xvals.push_back(xtt1[3]);
      yvals.push_back(ytt1[3]);
      xvals.push_back(xtt1[4]);
      yvals.push_back(ytt1[4]);
      //Add last points back in to close the polygon
      xvals.push_back(xtt1[0]);
      yvals.push_back(ytt1[0]);
      
      delete tt1;
      delete tt2;
      delete tt3;
      TPolyLine* newline = new TPolyLine(xvals.size(),xvals.data(),yvals.data());  //Equal sizes so shouldn't matter which is used
      return newline;      
    }
    if( tt==1 ){
      //For tile 1 only add CCW tile
      TPolyLine* main = EpdTilePoly(epdgeo,pp,tt);
      Int_t nmain = main->GetN();
      Double_t* xmain = main->GetX();
      Double_t* ymain = main->GetY();
      std::list<double> xvals;
      std::list<double> yvals;
      for( int i=0; i<nmain-1; ++i ){
	xvals.push_back(xmain[i]);
	yvals.push_back(ymain[i]);
      }
      std::list<double>::iterator xitr = xvals.begin();
      std::list<double>::iterator yitr = yvals.begin();
      xitr = xvals.begin();
      yitr = yvals.begin();
      bool CCWIstt1 = false;
      Int_t adjpp = 0;
      Int_t adjtt = 0;
      StFwdAnaEpdMatch::GetEpdTileCCW(pp,tt,adjpp,adjtt);
      if( adjpp!=0 && adjtt!=0 ){
	//Know there is something in the counter clockwise direction
	TPolyLine* adjline = EpdTilePoly(epdgeo,adjpp,adjtt);
	Int_t nadj = adjline->GetN();
	Double_t* adjxvals = adjline->GetX();
	Double_t* adjyvals = adjline->GetY();
	int lastcorner = 3;  //For rectangular tiles this is the index to use
	//std::cout << "|nadj:"<<nadj << std::endl;
	if( nadj==6 ){
	  //For tile pp1
	  CCWIstt1 = true;
	  lastcorner = 4;  //For tt1 which is pentagonal this is the index to use
	}
	std::advance(xitr,1);
	std::advance(yitr,1);
	//Want to add CW inner edge first and then inner CCW edge
	xvals.insert(xitr,adjxvals[lastcorner]);
	yvals.insert(yitr,adjyvals[lastcorner]);
	xvals.insert(xitr,adjxvals[0]);
	yvals.insert(yitr,adjyvals[0]);
	xvals.insert(xitr,adjxvals[1]);
	yvals.insert(yitr,adjyvals[1]);
	xvals.insert(xitr,adjxvals[2]);
	yvals.insert(yitr,adjyvals[2]);
	if( CCWIstt1 ){
	  //Add extra corner for tt1
	  xvals.insert(xitr,adjxvals[3]);
	  yvals.insert(yitr,adjyvals[3]);
	}
	delete adjline;
      }
      std::vector<double> newxvals;
      std::vector<double> newyvals;
      xitr=xvals.begin();
      yitr=yvals.begin();
      //std::cout << "=====final=====" << std::endl;
      for( ; xitr!=xvals.end() && yitr!=yvals.end(); ++xitr, ++yitr ){
	newxvals.push_back(*xitr);
	newyvals.push_back(*yitr);
	//std::cout << " + |x:"<< *xitr << "|y:"<<*yitr << std::endl;
      }
      //Add first element back in to close the polygon
      xitr = xvals.begin();
      yitr = yvals.begin();
      newxvals.push_back(*xitr);
      newyvals.push_back(*yitr);
    
      delete main;
      TPolyLine* newline = new TPolyLine(newxvals.size(),newxvals.data(),newyvals.data());  //Equal sizes so shouldn't matter which is used
      return newline;
    }
  }
  return 0;
}

TPolyLine* StFwdAnaEpdMatch::EpdCWInnerCorner(StEpdGeom* epdgeo, short pp, short tt)
{
  int newtt = 0;
  int newpp = 0;
  StFwdAnaEpdMatch::GetEpdTileInnerCW(pp,tt,newpp,newtt);
  //std::cout << "|pp:"<<pp << "|tt:"<<tt << "|newpp:"<<newpp << "|newtt:"<<newtt << std::endl;
  if( newpp!=0 && newtt!=0 ){ return EpdCCWOuterCorner(epdgeo,newpp,newtt); }
  else{
    if( tt==2 ){
      //Special for tile 2 to group tile 1, 2 and 3 together
      TPolyLine* tt1 = EpdTilePoly(epdgeo,pp,1);
      TPolyLine* tt2 = EpdTilePoly(epdgeo,pp,2);
      TPolyLine* tt3 = EpdTilePoly(epdgeo,pp,3);
      Double_t* xtt1 = tt1->GetX();
      Double_t* ytt1 = tt1->GetY();
      Double_t* xtt2 = tt2->GetX();
      Double_t* ytt2 = tt2->GetY();
      Double_t* xtt3 = tt3->GetX();
      Double_t* ytt3 = tt3->GetY();
      std::vector<double> xvals;
      std::vector<double> yvals;
      //Manually adding corners
      xvals.push_back(xtt1[0]);
      yvals.push_back(ytt1[0]);
      xvals.push_back(xtt1[1]);
      yvals.push_back(ytt1[1]);
      xvals.push_back(xtt2[0]);
      yvals.push_back(ytt2[0]);
      xvals.push_back(xtt2[1]);
      yvals.push_back(ytt2[1]);
      xvals.push_back(xtt2[2]);
      yvals.push_back(ytt2[2]);
      xvals.push_back(xtt3[1]);
      yvals.push_back(ytt3[1]);
      xvals.push_back(xtt3[2]);
      yvals.push_back(ytt3[2]);
      xvals.push_back(xtt3[3]);
      yvals.push_back(ytt3[3]);
      xvals.push_back(xtt1[3]);
      yvals.push_back(ytt1[3]);
      xvals.push_back(xtt1[4]);
      yvals.push_back(ytt1[4]);
      //Add last points back in to close the polygon
      xvals.push_back(xtt1[0]);
      yvals.push_back(ytt1[0]);
      
      delete tt1;
      delete tt2;
      delete tt3;
      TPolyLine* newline = new TPolyLine(xvals.size(),xvals.data(),yvals.data());  //Equal sizes so shouldn't matter which is used
      return newline;      
    }
    if( tt==1 ){
      //For tile 1 only add CC tile
      TPolyLine* main = EpdTilePoly(epdgeo,pp,tt);
      Int_t nmain = main->GetN();
      Double_t* xmain = main->GetX();
      Double_t* ymain = main->GetY();
      std::list<double> xvals;
      std::list<double> yvals;
      for( int i=0; i<nmain-1; ++i ){
	xvals.push_back(xmain[i]);
	yvals.push_back(ymain[i]);
      }
      std::list<double>::iterator xitr = xvals.end();
      std::list<double>::iterator yitr = yvals.end();
      xitr = xvals.end();
      yitr = yvals.end();
      Int_t adjpp = 0;
      Int_t adjtt = 0;
      StFwdAnaEpdMatch::GetEpdTileCW(pp,tt,adjpp,adjtt);
      if( adjpp!=0 && adjtt!=0 ){
	//Know there is something in the clockwise direction and it is tt 1 with 5 corners based on adjacent mapping
	TPolyLine* adjline = EpdTilePoly(epdgeo,adjpp,adjtt);
	//Int_t nadj = adjline->GetN();
	Double_t* adjxvals = adjline->GetX();
	Double_t* adjyvals = adjline->GetY();
	//advance to last corner
	--xitr;
	--yitr;
	//Want to add CCW outer edge first and then outer CW edge and so on
	xvals.insert(xitr,adjxvals[1]);
	yvals.insert(yitr,adjyvals[1]);
	xvals.insert(xitr,adjxvals[2]);
	yvals.insert(yitr,adjyvals[2]);
	xvals.insert(xitr,adjxvals[3]);
	yvals.insert(yitr,adjyvals[3]);
	xvals.insert(xitr,adjxvals[4]);
	yvals.insert(yitr,adjyvals[4]);
	xvals.insert(xitr,adjxvals[0]);
	yvals.insert(yitr,adjyvals[0]);
	delete adjline;
      }
      std::vector<double> newxvals;
      std::vector<double> newyvals;
      xitr=xvals.begin();
      yitr=yvals.begin();
      //std::cout << "=====final=====" << std::endl;
      for( ; xitr!=xvals.end() && yitr!=yvals.end(); ++xitr, ++yitr ){
	newxvals.push_back(*xitr);
	newyvals.push_back(*yitr);
	//std::cout << " + |x:"<< *xitr << "|y:"<<*yitr << std::endl;
      }
      //Add first element back in to close the polygon
      xitr = xvals.begin();
      yitr = yvals.begin();
      newxvals.push_back(*xitr);
      newyvals.push_back(*yitr);
    
      delete main;
      TPolyLine* newline = new TPolyLine(newxvals.size(),newxvals.data(),newyvals.data());  //Equal sizes so shouldn't matter which is used
      return newline;
    }
  }
  return 0;
}

void StFwdAnaEpdMatch::PaintEpdProjections(TCanvas* canv, const char* savename)   const
{
  canv->Clear();
  
  canv->Divide(2,2);
  canv->cd(1)->SetLogz();
  mH2F_EpdProjHitMap->Draw("colz");
  canv->cd(2)->SetLogz();
  mH2F_EpdProjHitMap_Vcut->Draw("colz");
  canv->cd(3)->SetLogz();
  mH2F_EpdNmip->Draw("colz");

  canv->Print(savename);
}
			  
