#include "StFcsShowerAnaMaker.h"

#include "StEnumerations.h"
#include "StMessMgr.h"
#include "Stypes.h"
#include "StEventTypes.h"
#include "StMuDSTMaker/COMMON/StMuTypes.hh"
#include "StThreeVectorF.hh"
#include "tables/St_g2t_vertex_Table.h"

#include "StFcsDbMaker/StFcsDb.h"
#include "StFcsDbMaker/StFcsDbMaker.h"
#include "StFcsCollection.h"
#include "StFcsCluster.h"
#include "StFcsPoint.h"

//#include "/star/u/dkap7827/Tools2/Tools/MyTools/inc/Ctools.h"
//#include "/star/u/dkap7827/Tools2/Tools/MyTools/inc/Rtools.h"
ClassImp(StFcsShowerAnaMaker)

Double_t StFcsShowerAnaMaker::DistStThreeVecD(StThreeVectorD &vec1, StThreeVectorD &vec2)
{
  Double_t xdiff = vec1.x()-vec2.x();
  Double_t ydiff = vec1.y()-vec2.y();
  Double_t zdiff = vec1.z()-vec2.z();
  return sqrt(xdiff*xdiff+ydiff*ydiff+zdiff*zdiff);
}

Double_t StFcsShowerAnaMaker::TaxiDistStThreeVecD(StThreeVectorD &vec1, StThreeVectorD &vec2)
{
  Double_t xdiff = fabs(vec1.x()-vec2.x());
  Double_t ydiff = fabs(vec1.y()-vec2.y());
  Double_t zdiff = fabs(vec1.z()-vec2.z());
  return xdiff+ydiff+zdiff;
}

Double_t StFcsShowerAnaMaker::ChebDistStThreeVecD(StThreeVectorD &vec1, StThreeVectorD &vec2)
{
  Double_t xdiff = fabs(vec1.x()-vec2.x());
  Double_t ydiff = fabs(vec1.y()-vec2.y());
  Double_t zdiff = fabs(vec1.z()-vec2.z());
  return std::max({xdiff,ydiff,zdiff});
}


StThreeVectorD StFcsShowerAnaMaker::starXYZtoLocal(int det, const StThreeVectorD &xyz, double showermaxz)
{
  if( showermaxz<0 ){ showermaxz =  mFcsDb->getShowerMaxZ(det); }  //when negative use default showermax
  if( xyz.x()<0 ){ if( det%2!=0 ){ std::cout << "Warning wrong detector id" << std::endl; } }
  if( xyz.x()>0 ){ if( det%2!=1 ){ std::cout << "Warning wrong detector id" << std::endl; } }
  double detangle = mFcsDb->getDetectorAngle(det)*M_PI/180.0;
  if( det%2==1 ){ detangle *= -1.0; } //South side use negative angle for rotation, not sure why but it works
  StThreeVectorD xyzoff = mFcsDb->getDetectorOffset(det,showermaxz);
  //Set y value to be topmost part of detector. This is important in checking the bounds of the returned vector
  xyzoff.setY( xyzoff.y() - (mFcsDb->getYWidth(det)/2.0) + (double(mFcsDb->nRow(det)) / 2.0 * mFcsDb->getYWidth(det)) );
  StThreeVectorD localxyz = xyz - xyzoff;
  localxyz.rotateY(detangle);
  //This change of coordinates is to the inside edge near the beam pipe. So the new origin is the top most inside edge of the detector, with positive x-axis facing south and positive y-axis going up
  if( det%2==1 ){
    //For south side more x-positive is good, negative is outside FCS
    if( localxyz.x()<0 || localxyz.y()>0 ){ localxyz.set(0,0,0); } //x is negative hence outside the coverage of the South detectors
    else{ localxyz.setY(-localxyz.y()); } //make y positive since the local coordinate (0,0,0) is top inner edge of detector
  }
  else{
    //For north side more x-negative is good, positive is outside FCS
    if( localxyz.x()>0 || localxyz.y()>0 ){ localxyz.set(0,0,0); } //x is positive hence outside the coveratge of the North detectors
    else{ localxyz.setX(-localxyz.x()); localxyz.setY(-localxyz.y()); } //make x,y positive to compensate for odd (0,0,0) position
  }
  return localxyz;
}

void StFcsShowerAnaMaker::starXYZtoColRow(int det, const StThreeVectorD &xyz, Int_t &col, Int_t &row, double showermaxz)
{
  StThreeVectorD localxyz = starXYZtoLocal(det,xyz,showermaxz);
  //Use ceiling since column, row counting starts at 1 not 0
  col = ceil(localxyz.x()/mFcsDb->getXWidth(det));
  row = ceil(localxyz.y()/mFcsDb->getYWidth(det));
  if( col>mFcsDb->nColumn(det) ){ col=0; }
  if( row>mFcsDb->nRow(det) ){ row=0; }
}

StFcsShowerAnaMaker::StFcsShowerAnaMaker(const char* name):StMaker(name)
{
}

StFcsShowerAnaMaker::~StFcsShowerAnaMaker()
{
  delete mDataTree;
  delete mHistMan;  //Delete last since contains pointer to TFile
}

Int_t StFcsShowerAnaMaker::Init()
{
  /*if( mFileName.Length()!=0 ){
    mTestFile = new ofstream();
    TString fname(mFileName);
    fname.ReplaceAll(".root",".out");
    mTestFile->open(fname.Data());
    if( !(mTestFile->is_open()) ){ std::cout <<"file '"<< fname << "' not open" << std::endl; }
    }*/

  mHistMan = new HistManager();
  if( mFileName.Length()==0 ){ mHistMan->InitFile("ShowerAna.root","RECREATE"); }
  else{ mHistMan->InitFile(mFileName.Data(), "RECREATE"); }
  //std::cout << "|nloaded:"<<nloaded << std::endl;
  UInt_t nhists = LoadHistograms(0,mHistMan);  //Use zero to create new histograms
  std::cout << "StFcsShowerAnaMaker::Init made "<<nhists << " histograms" << std::endl;
  
  if( mDataTree==0 ){
    mDataTree    = new TTree("ShowerAnaTree","");
    mFcsHitsArr  = new TClonesArray("StFcsPicoHit");
    mFcsClusArr  = new TClonesArray("StFcsPicoCluster");
    mFcsPointArr = new TClonesArray("StFcsPicoPoint");
    mG2tPrimArr  = new TClonesArray("StPicoG2tTrack");
    mG2tParArr   = new TClonesArray("StPicoG2tTrack");
    mDataTree->Branch("FcsHit",     &mFcsHitsArr);
    mDataTree->Branch("FcsCluster", &mFcsClusArr);
    mDataTree->Branch("FcsPoint",   &mFcsPointArr);
    mDataTree->Branch("G2tPrim",    &mG2tPrimArr);
    mDataTree->Branch("G2tParent",  &mG2tParArr);
  }
  else{ std::cout << "WARNING StFcsShowerAnaMaker::Init - DataTree Exists" << std::endl; }
  
  return kStOk;
}

Int_t StFcsShowerAnaMaker::InitRun(Int_t runnumber)
{
  if( !mFcsDb ){ mFcsDb = static_cast<StFcsDb*>(GetDataSet("fcsDb")); }
  //mFcsDb->setDbAccess(0);
  if (!mFcsDb) {
    LOG_ERROR << "StFcsShowerAnaMaker::Init Failed to get StFcsDb" << endm;
    return kStFatal;
  }
  return kStOk;
}

Int_t StFcsShowerAnaMaker::Finish()
{
  mHistMan->Write();
  if( mDataTree!=0 ){ mDataTree->Write(); }
  return kStOk;
}

UInt_t StFcsShowerAnaMaker::LoadHistograms(TFile* file, HistManager* histman)
{
  UInt_t nloaded = 0;

  nloaded += histman->AddH1F(file,mH1F_PointXLocal,"H1F_PointXLocal","",100,0,1);
  nloaded += histman->AddH1F(file,mH1F_PointYLocal,"H1F_PointYLocal","",100,0,1);
  nloaded += histman->AddH2F(file,mH2F_PointLocalyVx,"H2F_PointLocalyVx",";Point Local X;Point Local Y", 100,0,1, 100,0,1);
  nloaded += histman->AddH1F(file,mH1F_Chi2Ndf1Photon,"H1F_Chi2Ndf1Photon","Single Photon Fit;Chi^2/NDF",100,0,100);
  nloaded += histman->AddH1F(file,mH1F_Chi2Ndf2Photon,"H1F_Chi2Ndf2Photon","Two Photon Fit;Chi^2/NDF",100,0,100);
  nloaded += histman->AddH1F(file,mH1F_ClusSigMax,"H1F_ClusSigMax","",200,0,2);
  nloaded += histman->AddH1F(file,mH1F_ClusSigMin,"H1F_ClusSigMin","",200,0,2);
  nloaded += histman->AddH2F(file,mH2F_PointXProjX,"H2F_PointXProjX","",160,-160,160, 160,-160,160);
  nloaded += histman->AddH2F(file,mH2F_PointYProjY,"H2F_PointYProjY","",160,-160,160, 160,-160,160);
  nloaded += histman->AddH2F(file,mH2F_ClusSigMaxEn,"H2F_ClusSigMaxEn","",100,0,100,200,0,2);
  nloaded += histman->AddH2F(file,mH2F_ClusSigMinEn,"H2F_ClusSigMinEn","",100,0,100,200,0,2);
  
  nloaded += histman->AddH1F(file,mH1F_primid,"H1F_primid",";GEANT ID", 11,-0.5,10.5);
  nloaded += histman->AddH1F(file,mH1F_parentid,"H1F_parentid",";GEANT ID", 11,-0.5,10.5);
  nloaded += histman->AddH1F(file,mH1F_NParClusPhotons,"H1F_NParClusPhotons",";Number of parent cluster photons", 5,-0.5,4.5);
  nloaded += histman->AddH2F(file,mH2F_npoiVnclus,"H2F_npoiVnclus",";NClusters;NPoints", 7,-0.5,6.5, 7,-0.5,6.5);
  nloaded += histman->AddH2F(file,mH2F_cluseVlore,"H2F_cluseVlore",";Cluster Lorentz E;Cluster E", 100,0,50, 100,0,50);
  nloaded += histman->AddH2F(file,mH2F_trkeVpoie,"H2F_trkeVpoie",";point E;trk E", 300,0,30, 300,0,30);
  nloaded += histman->AddH1F(file,mH1F_invmasspoi,"H1F_invmasspoi",";inv mass point (GeV)", 100,0,0.5);
  nloaded += histman->AddH1F(file,mH1F_invmasstrk,"H1F_invmasstrk",";inv mass g2trk (GeV)", 100,-0.5,0.5);
  nloaded += histman->AddH1F(file,mH1F_dpoitrk,"H1F_dpoitrk",";r (cm)", 100,0,10);
  nloaded += histman->AddH2F(file,mH2F_massVdgg,"H2F_massVdgg",";d_{gg} point (cm);inv masss point (GeV)", 100,0,50, 100,0,1);
  nloaded += histman->AddH2F(file,mH2F_trkmassVdgg,"H2F_trkmassVdgg",";d_{gg} point (cm);inv masss track (GeV)", 100,0,50, 100,0,1);
  
  nloaded += histman->AddH2F(file,mH2F_parprojyVprojx,"H2F_parprojyVprojx",";proj x cm;proj y cm", 160,-160,160, 160,-160,160 );
  nloaded += histman->AddH1F(file,mH1F_NPrimTrks,"H1F_NPrimTrks",";Number of Primary Tracks", 5,-0.5,4.5);
  nloaded += histman->AddH1F(file,mH1F_NParTrks,"H1F_NParTrks",";Number of Parent Tracks", 5,-0.5,4.5);

  nloaded += histman->AddH2F(file,mH2F_hiteVtrkdist,"H2F_hiteVtrkdist",";track distance (cm);hit energy (GeV)", 100,0,100, 500,0,50 );
  nloaded += histman->AddH2F(file,mH2F_hiteVtrktaxid,"H2F_hiteVtrktaxid",";track Taxicab distance (cm);hit energy (GeV)", 100,0,100, 500,0,50 );
  nloaded += histman->AddH2F(file,mH2F_hiteVtrkchebd,"H2F_hiteVtrkchebd",";track Chebyshev distance (cm);hit energy (GeV)", 100,0,100, 500,0,50 );
  nloaded += histman->AddH2F(file,mH2F_TrkhitfracVdist,"H2F_TrkhitfracVdist",";track distance (cm);fraction of energy (GeV)", 50,0,100, 100,0,1 );

  nloaded += histman->AddH2F(file,mH2F_clusmeanyVx,"H2F_clusmeanyVx",";Cluster Mean X (cm);Cluster Mean Y (cm)", 160,-160,160, 160,-160,160);
  nloaded += histman->AddH1F(file,mH1F_ClusMeanDTrk,"H1F_ClusMeanDTrk",";Cluster Mean Distance to Track(cm);", 100,0,50);

  nloaded += histman->AddH3F(file,mH3F_Showers,"H3F_Showers",";HitLocalX;HitLocalY;HitEn", 100,-10,10, 100,-10,10, 100,0,1);
  nloaded += histman->AddH2F(file,mH2F_NhitsPerClus,"H2F_NhitsPerClus",";NhitsPerClus", 6,0,6, 50,0,50);

  if( mHC2F_PointLocalyVx==0 ){ mHC2F_PointLocalyVx = new TObjArray(); }
  nloaded += histman->AddH2FArr(file,mHC2F_PointLocalyVx,8*6,"PointLocalyVx_PhiEta",";Point Local X;Point Local Y",100,0,1, 100,0,1); //8 bins in phi, 6 in eta

  return nloaded;
}

Int_t StFcsShowerAnaMaker::Make()
{
  StEvent* event = (StEvent*)GetInputDS("StEvent");
  if (!event) {
    LOG_ERROR << "StFcsShowerAnaMaker::Make did not find StEvent" << endm;
    return kStErr;
  }
  mFcsColl = event->fcsCollection();
  if (!mFcsColl) {
    LOG_ERROR << "StFcsShowerAnaMaker::Make did not find StEvent->StFcsCollection" << endm;
    return kStErr;
  }

  int totalhits = 0;   //Running counter for all hits for all detectors
  int totalclus = 0;   //Running counter for all clusters for all detectors
  int totalpoints = 0; //Running counter for all points for all detectors
  
  //std::cout << "|HitsArr:"<<mFcsHitsArr << "|ClusArr:"<<mFcsClusArr << "|PointArr:"<<mFcsPointArr << "|G2tPrimArr:"<<mG2tPrimArr << "|G2tParArr:"<<mG2tParArr << std::endl;
  for( int det=0; det<kFcsNDet; det++ ){
    StSPtrVecFcsHit& hits = mFcsColl->hits(det);
    int nh = mFcsColl->numberOfHits(det);
    //std::cout << "|det:"<<det << "|nh:"<<nh << std::endl;
    for( int ihit=0; ihit<nh; ++ihit ){
      StFcsHit* hit=hits[ihit];
      StFcsPicoHit* picohit = (StFcsPicoHit*)mFcsHitsArr->ConstructedAt(totalhits++);
      //std::cout << "|det:"<<det<<"|ihit:"<<ihit << "|hit:"<<hit << "|picohit:"<<picohit << std::endl;
      picohit->mDetId  = hit->detectorId();
      picohit->mChId   = hit->id();
      picohit->mAdcSum = hit->adcSum();
      picohit->mEnergy = hit->energy();
      picohit->mClusterId = -1; //@[February 20, 2024]>Set it explicityly to -1 since for some reason was being intialized to 0 in some cases and throwing off the cluster to hit associations
      
      if( det<=kFcsHcalSouthDetId ){ //ECAL and HCAL
	StThreeVectorF xyz = mFcsDb->getStarXYZ(hit);
	picohit->mXstar = xyz.x();
	picohit->mYstar = xyz.y();
	picohit->mZstar = xyz.z();
	mFcsDb->getLocalXYinCell(hit, picohit->mXLocal, picohit->mYLocal);
      }
      else if(det==kFcsPresNorthDetId || det==kFcsPresSouthDetId){//EPD as Pres
	picohit->mZstar = 375.0; //Taken from StFcsEventDisplay for zepd this is in cm
      }
      //std::cout << "|i:"<<ihit<<"|th:"<<totalhits<<"("<<picohit->mXstar<<","<<picohit->mYstar<<","<<picohit->mEnergy<<")"<<std::endl;
    }

    StSPtrVecFcsCluster& clusters = mFcsColl->clusters(det);
    int nc = mFcsColl->numberOfClusters(det);
    //std::cout << "|det:"<<det << "|nc:"<<nc << std::endl;
    for( int iclus=0; iclus<nc; ++iclus ){
      StFcsCluster* cluster = clusters[iclus];
      StFcsPicoCluster* picoclus = (StFcsPicoCluster*)mFcsClusArr->ConstructedAt(totalclus++);
      picoclus->mId             = cluster->id();
      picoclus->mDetId          = cluster->detectorId();
      picoclus->mCategory       = cluster->category();
      picoclus->mNTowers        = cluster->nTowers();
      picoclus->mNNeighbor      = cluster->nNeighbor();
      picoclus->mNPoints        = cluster->nPoints();
      picoclus->mEnergy         = cluster->energy();
      picoclus->mX              = cluster->x();
      picoclus->mY              = cluster->y();
      picoclus->mSigmaMin       = cluster->sigmaMin();
      picoclus->mSigmaMax       = cluster->sigmaMax();
      picoclus->mTheta          = cluster->theta();
      picoclus->mChi2Ndf1Photon = cluster->chi2Ndf1Photon();
      picoclus->mChi2Ndf2Photon = cluster->chi2Ndf2Photon();
      mH1F_Chi2Ndf1Photon->Fill(cluster->chi2Ndf1Photon());
      mH1F_Chi2Ndf2Photon->Fill(cluster->chi2Ndf2Photon());
      //Lorentz 4 momentum of cluster
      StLorentzVectorD clusp = cluster->fourMomentum();
      picoclus->mPx = clusp.px();
      picoclus->mPy = clusp.py();
      picoclus->mPz = clusp.pz();
      picoclus->mE  = clusp.e();

      mH2F_cluseVlore->Fill(clusp.e(),cluster->energy());
      if( cluster->energy() > mEnCut ){
	mH1F_ClusSigMax->Fill(cluster->sigmaMax());
	mH1F_ClusSigMin->Fill(cluster->sigmaMin());
	mH2F_ClusSigMaxEn->Fill(cluster->energy(),cluster->sigmaMax());
	mH2F_ClusSigMinEn->Fill(cluster->energy(),cluster->sigmaMin());
      }
      UInt_t nhits = cluster->nTowers();
      StPtrVecFcsHit& clushits = cluster->hits();
      if( GetDebug()==3 ){ std::cout << "|clusid:"<<cluster->id() << "|nTowers:"<<nhits << "|size:"<<clushits.size() << std::endl; }
      //(*mTestFile) << "|det:"<<det << "|Cid:"<<cluster->id() << "|Cdet:"<<cluster->detectorId() << "|Cx:"<<cluster->x() <<"|Cy:"<<cluster->y() << "|Ce:"<<cluster->energy() << "|Cnhit:"<<nhits /*<<"|Csize:"<<clushits.size()*/ << std::endl;
      for( UInt_t ihit=0; ihit<nhits; ++ihit ){
	StFcsHit* hit = clushits[ihit];
	if( GetDebug()==3 ){ std::cout << " + |detid:"<<hit->detectorId() << "|id:"<<hit->id() << std::endl; }
	StFcsPicoHit* picohit = 0;
	for( Int_t phit=0; phit<mFcsHitsArr->GetEntriesFast(); ++phit ){
	  picohit = (StFcsPicoHit*)mFcsHitsArr->At(phit);
	  if( picohit->mDetId==hit->detectorId() && picohit->mChId==hit->id() ){ picohit->mClusterId = cluster->id(); break; }
	}
	if( det<2 ){
	  mH2F_NhitsPerClus->Fill(det,nhits);
	  Float_t hlocalx = 0;
	  Float_t hlocaly = 0;
	  mFcsDb->getLocalXYinCell(hit, hlocalx, hlocaly);
	  ((TH3F*)mH3F_Showers)->Fill( hlocalx-cluster->x(), hlocaly-cluster->y(), hit->energy()/cluster->energy() );
	  //(*mTestFile) << "  * |Hdet:"<<hit->detectorId() << "|Hid:"<<hit->id() << "|Hx:"<<hlocalx << "|Hy:"<<hlocaly << "|He:"<<hit->energy() << std::endl;
	  //(*mTestFile) << "  + |Pdet:"<<picohit->mDetId << "|Pid:"<<picohit->mChId << "|Px:"<<picohit->mXLocal <<"|Py:"<<picohit->mYLocal << "|Pe:"<<picohit->mEnergy << "|Pcid:"<<picohit->mClusterId << std::endl;
	  }
      }
    }
    
    StSPtrVecFcsPoint& points = mFcsColl->points(det);
    int np = mFcsColl->numberOfPoints(det);
    //std::cout << "|det:"<<det << "|np:"<<np << std::endl;
    for( int ipoint=0; ipoint<np; ++ipoint ){
      StFcsPoint* point=points[ipoint];
      //std::cout << "|det:"<<det<<"|ipoint:"<<ipoint << "|point:"<<point << std::endl;
      if( point->energy() > mEnCut ){
	float xfull = point->x();
	float xwhole = floor(xfull);
	float xpart = xfull-xwhole;
	mH1F_PointXLocal->Fill( xpart ); //Only store fractional part
	float yfull = point->y();
	float ywhole = floor(yfull);
	float ypart = yfull-ywhole;
	mH1F_PointYLocal->Fill( ypart ); //Only store fractional part
	mH2F_PointLocalyVx->Fill(xpart,ypart);
	StThreeVectorF pointxyz = point->xyz();
	int phi_index = pointxyz.phi() / (3.1415/4.0); //int conversion will floor to correct value and we have 8 phi bins
	if( pointxyz.phi() < 0 ){ phi_index = 7 + phi_index; }//Since we have 8 bins and are counting from zero
	int psuedorap_index = floor((pointxyz.pseudoRapidity() - 2.4)/0.3); //Bin width in psuedorapidity is 0.3
	if( psuedorap_index < 0 ){ psuedorap_index = 0; }
	if( psuedorap_index > 4.2 ){ psuedorap_index = 5; } //Since we have 6 psuedorapidity bins
	int index = 8*psuedorap_index + phi_index; //Since we have 8 phi indicies
	((TH2*)mHC2F_PointLocalyVx->At(index))->Fill(xpart,ypart);

	StFcsCluster* pointclus = point->cluster();
	g2t_track_st* g2ttrk = 0;
	g2t_vertex_st* g2tvert = 0;
	St_g2t_track* trackTable = static_cast<St_g2t_track*>(GetDataSet("g2t_track"));
	St_g2t_vertex* vertexTable = static_cast<St_g2t_vertex*>(GetDataSet("g2t_vertex"));
	double speedperc = 0;
	if( !trackTable ){ std::cout<< "g2t_track Table not found" << std::endl; continue; }
	else{
	  const int nTrk = trackTable->GetNRows();
	  if( GetDebug()>0 && GetDebug()<3 ){ std::cout << "g2t_track table has "<< nTrk << " tracks" << std::endl; }
	  if( nTrk>0 ){
	    g2ttrk = trackTable->GetTable();
	    if( !g2ttrk ) { std::cout << " g2t_track GetTable failed" << std::endl; continue; }
	  }
	}
	if( !vertexTable ){ std::cout<< "g2t_vertex Table not found" << std::endl; continue; }
	else{
	  const int nVertex = vertexTable->GetNRows();
	  if( GetDebug()>0 && GetDebug()<3 ){ std::cout << "g2t_vertex table has "<< nVertex << " vertices" << std::endl; }
	  if( nVertex>0 ){
	    g2tvert = vertexTable->GetTable();
	    if( !g2tvert) { std::cout << " g2t_vertex GetTable failed" << std::endl; continue; }
	  }
	}
	if( g2ttrk && g2tvert ){
	  StFcsPicoPoint* picopoint = (StFcsPicoPoint*)mFcsPointArr->ConstructedAt(totalpoints);
	  picopoint->mDetId                 = point->detectorId();
	  picopoint->mEnergy                = point->energy();
	  picopoint->mXlocal                = point->x();
	  picopoint->mYlocal                = point->y();
	  picopoint->mParentClusterId       = point->parentClusterId();
	  picopoint->mNParentClusterPhotons = point->nParentClusterPhotons();
	  picopoint->mClusterIndex          = mFcsClusArr->GetEntriesFast() - nc + point->parentClusterId(); //The correct index to use is old cluster size (current size - number of clusters added(nc)) + parentclusterid
	  //STAR xyx
	  //StThreeVectorF pointxyz = point->xyz();
	  //std::cout << "|ipoint:"<<ipoint << "|E:"<<point->energy() << "|x:"<<pointxyz.x() << "|y:"<<pointxyz.y() << "|z:"<<pointxyz.z() << std::endl;
	  picopoint->mXstar = pointxyz.x();
	  picopoint->mYstar = pointxyz.y();
	  picopoint->mZstar = pointxyz.z();
	  //Lorentz 4 momentum of point
	  StLorentzVectorD pointp = point->fourMomentum();
	  picopoint->mPx = pointp.px();
	  picopoint->mPy = pointp.py();
	  picopoint->mPz = pointp.pz();
	  picopoint->mE  = pointp.e();

	  mH1F_NParClusPhotons->Fill(picopoint->mNParentClusterPhotons);

	  StThreeVectorD clusmean = mFcsDb->getStarXYZfromColumnRow( det, pointclus->x(), pointclus->y() ); //Z is shower max z by default
	  //std::cout << "|ClusMean(X,Y,Z):("<<clusmean.x()<<","<<clusmean.y() << ","<< clusmean.z() << ")" << std::endl;
	  mH2F_clusmeanyVx->Fill(clusmean.x(),clusmean.y());

	  float frac=0;
	  int ntrk=0;
	  //const g2t_track_st* parenttrk = mFcsDb->getParentG2tTrack(pointclus,g2ttrk,frac,ntrk);
	  //std::cout << "|parenttrk|Id:"<<parenttrk->id << "|Pid:"<<parenttrk->ge_pid << "|E:"<<parenttrk->e << "|eta:"<<parenttrk->eta << "|frac:"<<frac << "|ntrk:"<<ntrk << std::endl;
	  const g2t_track_st* primtrk = mFcsDb->getPrimaryG2tTrack(pointclus,g2ttrk,frac,ntrk);
	  //const g2t_track_st* primtrk = mFcsDb->getParentG2tTrack(pointclus,g2ttrk,frac,ntrk);
	  StPicoG2tTrack* picotrk = (StPicoG2tTrack*)mG2tPrimArr->ConstructedAt(totalpoints);
	  StThreeVectorD projshowerxyz1 = mFcsDb->projectTrackToEcal(primtrk,g2tvert);
	  StThreeVectorD projshowerxyz2 = mFcsDb->projectTrackToEcalSMax(primtrk,g2tvert);
	  //std::cout << "|picotrk:"<<picotrk << "|primtrk:"<<primtrk << std::endl;
	  picotrk->id = primtrk->id;
	  picotrk->ge_pid = primtrk->ge_pid;
	  picotrk->mPx = primtrk->p[0];
	  picotrk->mPy = primtrk->p[1];
	  picotrk->mPz = primtrk->p[2];
	  picotrk->mE  = primtrk->e;
	  picotrk->mEta = primtrk->eta;
	  picotrk->mXProj = projshowerxyz2.x();
	  picotrk->mYProj = projshowerxyz2.y();
	  picotrk->mZProj = projshowerxyz2.z();

	  mH1F_primid->Fill(primtrk->ge_pid);

	  const g2t_track_st* parenttrk = mFcsDb->getParentG2tTrack(pointclus,g2ttrk,frac,ntrk);
	  StPicoG2tTrack* picoparent = (StPicoG2tTrack*)mG2tParArr->ConstructedAt(totalpoints++);
	  StThreeVectorD projparentxyz = mFcsDb->projectTrackToEcalSMax(parenttrk,g2tvert);

	  picoparent->id = parenttrk->id;
	  picoparent->ge_pid = parenttrk->ge_pid;
	  picoparent->mPx = parenttrk->p[0];
	  picoparent->mPy = parenttrk->p[1];
	  picoparent->mPz = parenttrk->p[2];
	  picoparent->mE  = parenttrk->e;
	  picoparent->mEta = parenttrk->eta;
	  picoparent->mXProj = projparentxyz.x();
	  picoparent->mYProj = projparentxyz.y();
	  picoparent->mZProj = projparentxyz.z();

	  mH1F_parentid->Fill(parenttrk->ge_pid);
	  mH2F_PointXProjX->Fill(pointxyz.x(), projparentxyz.x());
	  mH2F_PointYProjY->Fill(pointxyz.y(), projparentxyz.y());
	  mH2F_trkeVpoie->Fill( point->energy(), parenttrk->e );
	  double xdiff = picopoint->mXstar - picoparent->mXProj;
	  double ydiff = picopoint->mYstar - picoparent->mYProj;
	  mH1F_dpoitrk->Fill( sqrt(xdiff*xdiff + ydiff*ydiff) );
	  mH2F_parprojyVprojx->Fill( picoparent->mXProj, picoparent->mYProj );

	  Double_t dist = DistStThreeVecD(clusmean,projparentxyz);
	  mH1F_ClusMeanDTrk->Fill(dist);

	  StPtrVecFcsHit& clushits = pointclus->hits();
	  //if( clushits.size() != pointclus->nTowers() ){ std::cout << "|nTowers:"<<pointclus->nTowers() << "|HitSize:"<<clushits.size() << std::endl; }
	  //bool longdist = false;
	  for( UInt_t ihit=0; ihit<clushits.size(); ++ihit ){
	    StFcsHit* hit = clushits[ihit];
	    //int hitcol = mFcsDb->getColumnNumber(hit->detectorId(),hit->id());
	    //int hitrow = mFcsDb->getRowNumber(hit->detectorId(),hit->id());
	    float hitlocalx = 0;
	    float hitlocaly = 0;
	    mFcsDb->getLocalXYinCell(hit,hitlocalx,hitlocaly);
	    //std::cout << "|detid:"<<hit->detectorId() << "|id:"<<hit->id() << "|p_cluster:"<<hit->cluster()<<"|pointclus:"<<pointclus << std::endl;
	    //const g2t_track_st* hitparenttrk = mFcsDb->getParentG2tTrack(hit,g2ttrk,frac,ntrk);
	    const g2t_track_st* hitparenttrk = mFcsDb->getPrimaryG2tTrack(hit,g2ttrk,frac,ntrk);
	    //StThreeVectorD projhitparentxyz = mFcsDb->projectTrackToEcalSMax(hitparenttrk,g2tvert);
	    StThreeVectorD projhitparentxyz = mFcsDb->projectTrackToEcalSMax(hitparenttrk,g2tvert);
	    StThreeVectorD hitxyz = mFcsDb->getStarXYZ(hit);
	    Double_t hitdist = DistStThreeVecD(hitxyz,projhitparentxyz);
	    //if( hitdist>50 ){ longdist = true; }
	    int trackcol = 0;
	    int trackrow = 0;
	    int trackdet = projhitparentxyz.x()>=0 ? 1:0;
	    StThreeVectorD tracklocalxyz = starXYZtoLocal(trackdet,projhitparentxyz);
	    starXYZtoColRow(trackdet,projhitparentxyz,trackcol,trackrow);
	    //int trackid = mFcsDb->getId(trackdet,trackrow,trackcol);
	    //std::cout << " + |ihit:"<<ihit << "|hitdist:"<<hitdist << "|hitparentid:"<<hitparenttrk->id << std::endl;
	    //std::cout << "    - |Hit(X,Y,Z):("<<hitxyz.x()<<","<<hitxyz.y() << ","<< hitxyz.z() << ")" << "|det:"<<hit->detectorId() << "|col:"<<hitcol << "|row:"<<hitrow << "|id:"<<hit->id() << "|E:"<<hit->energy() << "|Local(X,Y,X):("<<hitlocalx*mFcsDb->getXWidth(det)<<","<<hitlocaly*mFcsDb->getYWidth(det)<<","<<0<<")"<< std::endl;
	    //std::cout << "    - |Trk(X,Y,Z):("<<projhitparentxyz.x()<<","<<projhitparentxyz.y() << ","<< projhitparentxyz.z() << ")" << "|det:"<<trackdet << "|col:"<<trackcol <<"|row:"<<trackrow <<"|id:"<<trackid << "|E:"<<hitparenttrk->e << "|Local(X,Y,X):("<<tracklocalxyz.x()<<","<<tracklocalxyz.y()<<","<<tracklocalxyz.z()<<")"<< std::endl;//"Trk" short for Track but use 3 characters to match number of characters in "Hit"
	    //std::cout << "    - |Trk(PX,PY,PZ):("<<hitparenttrk->p[0]<<","<<hitparenttrk->p[1]<<","<<hitparenttrk->p[2] << ")"<< std::endl;
	    
	    mH2F_hiteVtrkdist->Fill(hitdist,hit->energy());
	    mH2F_hiteVtrktaxid->Fill( TaxiDistStThreeVecD(hitxyz,projhitparentxyz), hit->energy() );
	    mH2F_hiteVtrkchebd->Fill( ChebDistStThreeVecD(hitxyz,projhitparentxyz), hit->energy() );
	    mH2F_TrkhitfracVdist->Fill(hitdist,frac);
	  }
	  /*
	  if( longdist ){
	    std::cout << "LONGDIST" << std::endl;
	    for( UInt_t ihit=0; ihit<clushits.size(); ++ihit ){
	      StFcsHit* hit = clushits[ihit];
	      const g2t_track_st* hitparenttrk = mFcsDb->getPrimaryG2tTrack(hit,g2ttrk,frac,ntrk);
	      StThreeVectorD projhitparentxyz = mFcsDb->projectTrackToEcalSMax(hitparenttrk,g2tvert);
	      StThreeVectorD hitxyz = mFcsDb->getStarXYZ(hit);
	      Double_t hitdist = DistStThreeVecD(hitxyz,projhitparentxyz);
	      int vertind = hitparenttrk->start_vertex_p - 1;
	      std::cout << " + |detid:"<<hit->detectorId() << "|id:"<<hit->id() << "|hitdist:"<<hitdist << "|hitparentid:"<<hitparenttrk->id << std::endl;
	      std::cout << "    - |ihit:"<<ihit<<"|Hit(X,Y,Z):("<<hitxyz.x()<<","<<hitxyz.y() << ","<< hitxyz.z() << ")" << std::endl;
	      std::cout << "    - |Track(X,Y,Z):("<<projhitparentxyz.x()<<","<<projhitparentxyz.y() << ","<< projhitparentxyz.z() << ")" << std::endl;
	      std::cout << "    - |Track["<<hitparenttrk->ge_pid<<"]"<<"(PX,PY,PZ,E):("<<hitparenttrk->p[0]<<"," <<hitparenttrk->p[1] << ","<<hitparenttrk->p[2] <<","<<hitparenttrk->e <<")"<< std::endl;
	      std::cout << "    - |vertind:"<<vertind<< "|TrackVert(X,Y,Z):("<<g2tvert[vertind].ge_x[0] << ","<<g2tvert[vertind].ge_x[1] << ","<< g2tvert[vertind].ge_x[2] << ")" << std::endl;
	    }
	    std::cout << "END" << std::endl;
	  }
	  */
	  //double phi = atan2(primtrk->p[1],primtrk->p[0]);
	  //double theta = 2.0*atan(exp(-1.0*primtrk->eta));
	  //double mass = sqrt(primtrk->e*primtrk->e - primtrk->ptot*primtrk->ptot);
	  if( GetDebug()==2 ){
	    std::cout << "|primtrk|Id:"<<primtrk->id << "|Pid:"<<primtrk->ge_pid << "|E:"<<primtrk->e
		      << "|px:"<<primtrk->p[0] << "|py:"<<primtrk->p[1] << "|pz:"<<primtrk->p[2] << "|pt:"<<primtrk->pt << "|ptot:"<<primtrk->ptot
		      << "|eta:"<<primtrk->eta //<< "|theta:"<<theta << "|phi:"<<phi << "|mass:"<< mass
		      << "|point:("<<pointxyz.x() <<","<<pointxyz.y()<<","<<pointxyz.z() <<")"
		      << "|projE:("<<projshowerxyz1.x() <<","<<projshowerxyz1.y()<<","<<projshowerxyz1.z() <<")"
		      << "|projS:("<<projshowerxyz2.x() <<","<<projshowerxyz2.y()<<","<<projshowerxyz2.z() <<")"
		      << "|start_vertex:"<<primtrk->start_vertex_p << "|stop_vertex:"<<primtrk->stop_vertex_p
		      << "|frac:"<<frac << "|ntrk:"<<ntrk
		      << std::endl;
	  }
	  /*
	  std::cout << "|primtrk|Id:"<<picotrk->id << "|Pid:"<<picotrk->ge_pid << "|E:"<<picotrk->mE
		    << "|px:"<<picotrk->mPx << "|py:"<<picotrk->mPy << "|pz:"<<picotrk->mPz
		    << "|pt:"<<picotrk->pt()<<"|g2t:"<<primtrk->pt << "|ptot:"<<picotrk->ptot() << "|g2t:"<<primtrk->ptot
		    << "|eta:"<<picotrk->mEta <<"|g2t:"<<primtrk->eta << "|theta:"<<picotrk->theta() << "|phi:"<<picotrk->phi()
		    << "|point:("<<pointxyz.x() <<","<<pointxyz.y()<<","<<pointxyz.z() <<")"
		    << "|projE:("<<projshowerxyz1.x() <<","<<projshowerxyz1.y()<<","<<projshowerxyz1.z() <<")"
		    << "|projS:("<<projshowerxyz2.x() <<","<<projshowerxyz2.y()<<","<<projshowerxyz2.z() <<")"
		    << "|frac:"<<frac << "|ntrk:"<<ntrk << std::endl;
	  */
	  if( GetDebug()==2 ){
	    int totalntrk = ntrk;
	    for( int itrk=0; itrk<totalntrk; ++itrk ){
	      const g2t_track_st* sectrk = mFcsDb->getPrimaryG2tTrack(pointclus,g2ttrk,frac,ntrk,itrk);
	      if( sectrk==0 ){ continue; }
	      double phi = atan2(sectrk->p[1],sectrk->p[0]);
	      double theta = 2.0*atan(exp(-1.0*sectrk->eta));
	      double mass = sqrt(sectrk->e*sectrk->e - sectrk->ptot*sectrk->ptot);
	      double diff = speedperc  - sectrk->ptot/sectrk->e;
	      std::cout << " + |sectrk:"<<itrk<<"|Id:"<<sectrk->id << "|Pid:"<<sectrk->ge_pid << "|E:"<<sectrk->e
			<< "|px:"<<sectrk->p[0] << "|py:"<<sectrk->p[1] << "|pz:"<<sectrk->p[2] << "|pt:"<<sectrk->pt << "|ptot:"<<sectrk->ptot
			<< "|eta:" << sectrk->eta << "|theta:"<<theta << "|phi:"<<phi << "|mass:"<< mass << "|beta:"<<sectrk->ptot/sectrk->e
			<< "|diff:"<< diff
			<< "|start_vertex:"<<sectrk->start_vertex_p << "|stop_vertex:"<<sectrk->stop_vertex_p
			<< "|frac:"<<frac << "|ntrk:"<<ntrk
			<< std::endl;
	    }
	    for( int i=0; i<vertexTable->GetNRows(); ++i ){
	      double xdiff = g2tvert[i].ge_x[0] - g2tvert[0].ge_x[0];
	      double ydiff = g2tvert[i].ge_x[1] - g2tvert[0].ge_x[1];
	      double zdiff = g2tvert[i].ge_x[2] - g2tvert[0].ge_x[2];
	      //double dist = sqrt( g2tvert[i].ge_x[0]*g2tvert[i].ge_x[0] + g2tvert[i].ge_x[1]*g2tvert[i].ge_x[1] + g2tvert[i].ge_x[2]*g2tvert[i].ge_x[2]  );
	      double dist = sqrt( xdiff*xdiff + ydiff*ydiff + zdiff*zdiff );
	      double speed = (dist/g2tvert[i].ge_tof) / 100.0; //convert to meters/sec
	      speedperc = speed/299792458.0;
	      double masspi0 = 0.1349768;
	      //double masspi0 = 0.135031;
	      double MyE = masspi0 * sqrt( (1.0)/(1.0-speedperc*speedperc) );
	      double MyP = sqrt( MyE*MyE - masspi0*masspi0 );
	      double MyM = sqrt( MyE*MyE - MyP*MyP );
	      std::cout << "|g2tvert|i:"<<i << "|dp:"<<(g2tvert[i].daughter_p)+1 << "|eg_label:"<<g2tvert[i].eg_label << "|eg_proc:" << g2tvert[i].eg_proc
			<< "|event_p:"<<g2tvert[i].event_p << "|id:"<< g2tvert[i].id << "|is_itrmd:"<<g2tvert[i].is_itrmd << "|parent_p:"<<g2tvert[i].parent_p
			<< "|eg_tof:"<<g2tvert[i].eg_tof << "|eg_x:"<<g2tvert[i].eg_x[0] <<","<< g2tvert[i].eg_x[1]<<","<<g2tvert[i].eg_x[2]
			<< "|ge_tof:"<<g2tvert[i].ge_tof << "|ge_x:"<<g2tvert[i].ge_x[0]<<","<<g2tvert[i].ge_x[1]<<","<<g2tvert[i].ge_x[2]
			<< "|ge_vz:"<<speedperc << "|ge_MyE:"<< MyE << "|ge_MyP:"<< MyP << "|ge_MyM:"<< MyM
			<< std::endl;
	    }
	  }
	}
      }
    }
    
    if( mFcsPointArr->GetEntriesFast()>=2 ){
      TLorentzVector lv_p0;   //Lorentz vector of 1 point
      TLorentzVector lv_p1;   //Lorentz vector of other point
      TLorentzVector lv_trk0; //Lorentz vector of 1 g2t track
      TLorentzVector lv_trk1; //Lorentz vector of other g2t track
      
      StFcsPicoPoint* poi0 = (StFcsPicoPoint*)mFcsPointArr->At(0);
      lv_p0.SetPx(poi0->mPx);
      lv_p0.SetPy(poi0->mPy);
      lv_p0.SetPz(poi0->mPz);
      lv_p0.SetE(poi0->mE);
      StFcsPicoPoint* poi1 = (StFcsPicoPoint*)mFcsPointArr->At(1);
      lv_p1.SetPx(poi1->mPx);
      lv_p1.SetPy(poi1->mPy);
      lv_p1.SetPz(poi1->mPz);
      lv_p1.SetE(poi1->mE);
      //}
      //if( partrks->GetEntriesFast()>=2 ){
      StPicoG2tTrack* trk0 = (StPicoG2tTrack*)mG2tParArr->At(0);
      lv_trk0.SetPx(trk0->mPx);
      lv_trk0.SetPy(trk0->mPy);
      lv_trk0.SetPz(trk0->mPz);
      lv_trk0.SetE (trk0->mE);
      StPicoG2tTrack* trk1 = (StPicoG2tTrack*)mG2tParArr->At(1);
      lv_trk1.SetPx(trk1->mPx);
      lv_trk1.SetPy(trk1->mPy);
      lv_trk1.SetPz(trk1->mPz);
      lv_trk1.SetE (trk1->mE);
      //point invariant mass
      TLorentzVector poiinv = lv_p0+lv_p1;
      double xdiff = poi0->mXstar - poi1->mXstar;
      double ydiff = poi0->mYstar - poi1->mYstar;
      double zdiff = poi0->mZstar - poi1->mZstar;
      double dgg = sqrt( xdiff*xdiff + ydiff*ydiff + zdiff*zdiff );
      mH1F_invmasspoi->Fill(poiinv.M());
      mH2F_massVdgg->Fill(dgg,poiinv.M());
      //Only compute invariant mass for tracks that are not the same particle
      if( trk0->id != trk1->id) {
	//track invariant mass
	TLorentzVector trkinv = lv_trk0+lv_trk1;
	double xdiff = trk0->mXProj - trk1->mXProj;
	double ydiff = trk0->mYProj - trk1->mYProj;
	double zdiff = trk0->mZProj - trk1->mZProj;
	double dgg = sqrt( xdiff*xdiff + ydiff*ydiff + zdiff*zdiff );
	mH1F_invmasstrk->Fill(trkinv.M());
	mH2F_trkmassVdgg->Fill(dgg,trkinv.M());
      }
    }
  }

  mH1F_NPrimTrks->Fill(mG2tPrimArr->GetEntriesFast());
  mH1F_NParTrks->Fill(mG2tParArr->GetEntriesFast());
  mH2F_npoiVnclus->Fill(totalclus,totalpoints);

  mDataTree->Fill();
  mFcsHitsArr->Clear();
  mFcsClusArr->Clear();
  mFcsPointArr->Clear();
  mG2tPrimArr->Clear();
  mG2tParArr->Clear();
  
  return kStOk;
}

