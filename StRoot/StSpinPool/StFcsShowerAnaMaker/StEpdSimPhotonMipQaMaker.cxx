#include "StEnumerations.h"
#include "StMessMgr.h"
#include "StEvent/StEvent.h"
#include "StEvent/StEventTypes.h"
#include "StMuDSTMaker/COMMON/StMuTypes.hh"
#include "Stypes.h"
#include "StThreeVectorF.hh"
#include "StEvent/StEpdCollection.h"
#include "StEpdUtil/StEpdGeom.h"
#include "tables/St_g2t_vertex_Table.h"
#include "tables/St_g2t_track_Table.h"

#include "StEpdSimPhotonMipQaMaker.h"

//#include "/star/u/dkap7827/Tools2/Tools/MyTools/inc/Ctools.h"
//#include "/star/u/dkap7827/Tools2/Tools/MyTools/inc/Rtools.h"
ClassImp(StEpdSimPhotonMipQaMaker)


StEpdSimPhotonMipQaMaker::StEpdSimPhotonMipQaMaker(const char* name):StMaker(name)
{
}

StEpdSimPhotonMipQaMaker::~StEpdSimPhotonMipQaMaker()
{
  delete mHistsMan;  
}

Int_t StEpdSimPhotonMipQaMaker::Init()
{
  if( !mEpdGeo ){ mEpdGeo = new StEpdGeom(); }
  //mFcsDb->setDbAccess(0);

  mHistsMan = new HistManager();
  if( mFileName.Length()==0 ){ mHistsMan->InitFile("EpdSimPhoton.root", "RECREATE"); }
  else{ mHistsMan->InitFile(mFileName.Data(),"RECREATE"); }
  
  UInt_t nmade = LoadHistograms(0,mHistsMan);
  std::cout << "StEpdSimPhotonMipQaMaker::Init made "<<nmade << " histograms" << std::endl;
    
  return kStOk;
}

Int_t StEpdSimPhotonMipQaMaker::Finish()
{
  mHistsMan->Write();
  return kStOk;
}

UInt_t StEpdSimPhotonMipQaMaker::LoadHistograms( TFile* file, HistManager* histman )
{
  UInt_t nloaded = 0;

  nloaded += histman->AddH1F(file,mH1F_NumberOfWestHits, "H1F_NumberOfWestHits", ";NumberofWestHits",500,0,500);
  nloaded += histman->AddH1F(file,mH1F_NumberOfMips,"H1F_NumberOfMips",";NumberOfMips",100,0,100);
  nloaded += histman->AddH2F(file,mH2F_Hit_yVx,"H2F_Hit_yVx",";Hit X;Hit Y", 300,-150,150, 200,-100,100);

  nloaded += histman->AddH1F(file,mH1F_NTracks,"H1F_NTracks",";NTracks", 11,-0.5,10.5);
  nloaded += histman->AddH1F(file,mH1F_GeantId,"H1F_GeantId",";GEANT ID", 11,-0.5,10.5);
  nloaded += histman->AddH1F(file,mH1F_NVertex,"H1F_NVertex",";NVertex", 11,-0.5,10.5);

  return nloaded;
}

Int_t StEpdSimPhotonMipQaMaker::Make()
{
  StEvent* event = (StEvent*)GetInputDS("StEvent");
  if (!event) {
    LOG_ERROR << "StEpdSimPhotonMipQaMaker::Make did not find StEvent" << endm;
    return kStErr;
  }
  mEpdHitMkr = (StEpdHitMaker*)GetMaker("epdHit");
  if( mEpdHitMkr==0 ){ LOG_WARN << "StMuFcsPi0TreeMaker::Make - No StEpdHitMaker(\"epdHit\")" << endm; }
  else{ mEpdColl = mEpdHitMkr->GetEpdCollection(); }

  int nhits = 0;   //Running counter for EPD west hits
  int nhitsnMIP = 0;  //Running counter for number of hits larger than nMIP cut in the west EPD
  
  StSPtrVecEpdHit* epdhits = 0;
  int nepdhits = 0;
  if( mMuEpdHits!=0 ){ nepdhits = mMuEpdHits->GetEntriesFast(); }
  else if( mEpdColl!=0 ){
    epdhits = &(mEpdColl->epdHits());
    nepdhits = epdhits->size();
  }
  else{ LOG_ERROR << "StMuEpdRun22QaMaker::FillEpdinfo() - If you see this error then there is a bug that is setting EPD hits improperly" << endm; return kStErr; }
  StMuEpdHit* muepdhit = 0;
  StEpdHit* epdhit = 0;
  for(int i=0; i<nepdhits; ++i ){
    if( mMuEpdHits!=0 ){ muepdhit = (StMuEpdHit*)mMuEpdHits->UncheckedAt(i); } //To match similar in StMuDstMaker->epdHit(int i)
    else if( epdhits!=0 ){ epdhit = (StEpdHit*)((*epdhits)[i]); }
    else{ LOG_ERROR << "IF YOU SEE THIS ERROR THEN THERE IS A VERY SERIOUS BUG IN THE CODE" << endm; return kStErr; } 
    //std::cout << "|i:"<<i << "|muepdhit:"<<muepdhit << "|epdhit:"<<epdhit << std::endl;
    int ew    = muepdhit!=0 ? muepdhit->side()    : epdhit->side();      //east=-1, west=1
    if( ew==-1 ){ continue; }
    ++nhits;
    int epdpp = muepdhit!=0 ? muepdhit->position(): epdhit->position();  //Supersector runs [1,12]
    int epdtt = muepdhit!=0 ? muepdhit->tile()    : epdhit->tile();      //Tile number [1,31]
    float nmip = muepdhit!=0 ? muepdhit->nMIP(): epdhit->nMIP();         //The ADC value of the hit divided by the MIP peak position; e.g. if nmip==1 then adc value sits at the MIP peak
    TVector3 epdhitxyz = mEpdGeo->TileCenter(epdpp,epdtt,ew);
    if( nmip>mnMIPCut ){
      ++nhitsnMIP;
      mH2F_Hit_yVx->Fill(epdhitxyz.x(),epdhitxyz.y());
    }
  }
  mH1F_NumberOfMips->Fill(nhitsnMIP);

  g2t_track_st* g2ttrk = 0;
  g2t_vertex_st* g2tvert = 0;
  St_g2t_track* trackTable = static_cast<St_g2t_track*>(GetDataSet("g2t_track"));
  St_g2t_vertex* vertexTable = static_cast<St_g2t_vertex*>(GetDataSet("g2t_vertex"));
  double speedperc = 0;
  if( !trackTable ){ std::cout<< "g2t_track Table not found" << std::endl; }
  else{
    const int nTrk = trackTable->GetNRows();
    if( GetDebug()>0 && GetDebug()<3 ){ std::cout << "g2t_track table has "<< nTrk << " tracks" << std::endl; }
    if( nTrk>0 ){
      g2ttrk = trackTable->GetTable();
      if( !g2ttrk ) { std::cout << " g2t_track GetTable failed" << std::endl; }
    }
  }
  if( !vertexTable ){ std::cout<< "g2t_vertex Table not found" << std::endl; }
  else{
    const int nVertex = vertexTable->GetNRows();
    if( GetDebug()>0 && GetDebug()<3 ){ std::cout << "g2t_vertex table has "<< nVertex << " vertices" << std::endl; }
    if( nVertex>0 ){
      g2tvert = vertexTable->GetTable();
      if( !g2tvert) { std::cout << " g2t_vertex GetTable failed" << std::endl; }
    }
  }
  if( g2ttrk && g2tvert ){
    mH1F_NTracks->Fill(trackTable->GetNRows());
    int totalntrk = trackTable->GetNRows();
    for( int itrk=0; itrk<totalntrk; ++itrk ){
      g2t_track_st* sectrk = (g2ttrk+itrk);//mFcsDb->getPrimaryG2tTrack(pointclus,g2ttrk,frac,ntrk,itrk);
      if( sectrk==0 ){ continue; }
      double phi = atan2(sectrk->p[1],sectrk->p[0]);
      double theta = 2.0*atan(exp(-1.0*sectrk->eta));
      double mass = sqrt(sectrk->e*sectrk->e - sectrk->ptot*sectrk->ptot);
      double diff = speedperc  - sectrk->ptot/sectrk->e;
      mH1F_GeantId->Fill(sectrk->ge_pid);
      if( GetDebug()==2 ){
	std::cout << " + |sectrk:"<<itrk<<"|Id:"<<sectrk->id << "|Pid:"<<sectrk->ge_pid << "|E:"<<sectrk->e
		  << "|px:"<<sectrk->p[0] << "|py:"<<sectrk->p[1] << "|pz:"<<sectrk->p[2] << "|pt:"<<sectrk->pt << "|ptot:"<<sectrk->ptot
		  << "|eta:" << sectrk->eta << "|theta:"<<theta << "|phi:"<<phi << "|mass:"<< mass << "|beta:"<<sectrk->ptot/sectrk->e
		  << "|diff:"<< diff
		  << "|start_vertex:"<<sectrk->start_vertex_p << "|stop_vertex:"<<sectrk->stop_vertex_p
		  << std::endl;
	}
    }
    mH1F_NVertex->Fill(vertexTable->GetNRows());
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
      if( GetDebug()==2 ){
	std::cout << "|g2tvert|i:"<<i << "|dp:"<<(g2tvert[i].daughter_p)+1 << "|eg_label:"<<g2tvert[i].eg_label << "|eg_proc:" << g2tvert[i].eg_proc
		  << "|event_p:"<<g2tvert[i].event_p << "|id:"<< g2tvert[i].id << "|is_itrmd:"<<g2tvert[i].is_itrmd << "|parent_p:"<<g2tvert[i].parent_p
		  << "|eg_tof:"<<g2tvert[i].eg_tof << "|eg_x:"<<g2tvert[i].eg_x[0] <<","<< g2tvert[i].eg_x[1]<<","<<g2tvert[i].eg_x[2]
		  << "|ge_tof:"<<g2tvert[i].ge_tof << "|ge_x:"<<g2tvert[i].ge_x[0]<<","<<g2tvert[i].ge_x[1]<<","<<g2tvert[i].ge_x[2]
		  << "|ge_vz:"<<speedperc << "|ge_MyE:"<< MyE << "|ge_MyP:"<< MyP << "|ge_MyM:"<< MyM
		  << std::endl;
	}
    }
  }
  
  //reconstruct photons for invariant mass?
  
  return kStOk;
}

void StEpdSimPhotonMipQaMaker::PaintEpd(TCanvas* canvas, const char* savename)
{
  canvas->Clear();
  canvas->Divide(2,2);
  canvas->cd(1);
  mH1F_NumberOfWestHits->Draw("hist e");
  canvas->cd(2);
  mH1F_NumberOfMips->Draw("hist e");
  canvas->cd(3);
  mH2F_Hit_yVx->Draw("colz");
  canvas->Print(savename);
}

void StEpdSimPhotonMipQaMaker::PaintSimEpd(TCanvas* canvas, const char* savename)
{
  canvas->Clear();
  canvas->Divide(2,2);
  canvas->cd(1);
  mH1F_NTracks->Draw("hist e");
  canvas->cd(2);
  mH1F_GeantId->Draw("hist e");
  canvas->cd(3);
  mH1F_NVertex->Draw("colz");
  canvas->Print(savename);
}




