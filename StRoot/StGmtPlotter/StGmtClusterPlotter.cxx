/////
#include "StGmtClusterPlotter.h"
#include "StRoot/StEvent/StGmtCollection.h"
#include "StRoot/StEvent/StGmtHitCollection.h"
#include "StRoot/StEvent/StGmtHit.h"
#include "StRoot/StGmtUtil/geometry/StGmtGeom.h"
#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StEvent/StEventInfo.h"
#include <TH2D.h>
#include <TH1D.h>
#include <TROOT.h>
#include <TStyle.h>
#include <TCanvas.h>
#include "StRoot/StGmtUtil/geometry/StGmtGeom.h"
#include "StRoot/StEvent/StEvent.h"
#include "StEvent/StEventTypes.h"
#include "StTpcDb/StTpcDb.h"
#include "StGmtUtil/geometry/StGmtGeom.h"
#include "StEvent/StEventTypes.h"
#include <TPolyMarker3D.h>
#include "StDetectorDbMaker/StGmtSurveyC.h"
#include "TGeoMatrix.h"
ClassImp(StGmtClusterPlotter);

Int_t StGmtClusterPlotter::Make()
{
  cout <<" making cluster plotter " <<endl;
  Int_t ierr = kStOk;
  StEvent* eventPtr = 0;
  eventPtr = (StEvent*)GetInputDS("StEvent");
  
  if( !eventPtr ) {
    LOG_ERROR << "Error getting pointer to StEvent from '" << ClassName() << "'" << endm;
    ierr = kStErr;
  };
  
  mGmtCollectionPtr = 0;
  
  if( eventPtr ) {
    mGmtCollectionPtr=eventPtr->gmtCollection();
  };
  
  if( !mGmtCollectionPtr) {
    LOG_ERROR << "Error getting pointer to StGmtCollection from '" << ClassName() << "'" << endm;
    ierr = kStErr;
  };
  Int_t realEvtNr=-1;
  if(eventPtr)
    {
      if(eventPtr->info())
	realEvtNr=eventPtr->info()->id();
    }
  StThreeVectorD ooo[32];
  StThreeVectorD ppp[32];
  Double_t R = 85.606 * 2.54; // inches => cm, from 2012-10-31 email of W.J. Llope to stargmt-l
  LOG_INFO <<"In plotter make " << endm;
  const TGeoHMatrix& tpc2Glob = gStTpcDb->Tpc2GlobalMatrix();
  for(int module=0;module<  kGmtNumModules;module++)    {
    TGeoHMatrix GmtModuleOnTpc = StGmtOnTpc::instance()->GetMatrix(module);
    TGeoHMatrix GmtOnModule    = StGmtOnModule::instance()->GetMatrix(module);
    TGeoHMatrix GmtOnTpc       = GmtModuleOnTpc * GmtOnModule;
    TGeoHMatrix GmtOnGlob      = tpc2Glob * GmtOnTpc;
    
    StGmtHitCollection* clusterCol=mGmtCollectionPtr->getHitCollection(module); if(!clusterCol) continue;
    const StSPtrVecGmtHit &hitVec=clusterCol->getHitVec(); if(!hitVec.size()) continue;
    int nGmtHits =0;
    Double_t tpclocal[3];
    Double_t tpcglobal[3];
    LOG_INFO <<"Got GMT collection, looking at " << hitVec.size() << " cluster ..  " <<endm;
    StSPtrVecGmtHitConstIterator hitIter;
    for(hitIter=hitVec.begin();hitIter != hitVec.end();hitIter++)
      {
	if (Debug()) (*hitIter)->Print("");
	//get XY hit
	Double_t X=(*hitIter)->getLocalX();
	Double_t Y=(*hitIter)->getLocalY();
	
	//store Amplitude for future cut
	Double_t AX=(*hitIter)->getAdcX();
	Double_t AY=(*hitIter)->getAdcY();
	
	hGmtAmpX->Fill(AX);
	hGmtAmpY->Fill(AY);
	
	if(AX<100 || AY<100) continue; //noise cut
	
	if(AX>AY)
	  hGmtAmpRatio->Fill(AY/AX);
	else
	  hGmtAmpRatio->Fill(AX/AY);
	
	hGmtHitXY[module]->Fill(X,Y);
	hGmtHitXYGlob->Fill(X,Y);
	hNGMThits->Fill(module);
	Double_t gmtCoord[3] = {X, Y, 0};
	Double_t gmtLocalTpc[3];
	GmtOnTpc.LocalToMaster(gmtCoord,gmtLocalTpc);
	GmtOnTpc.LocalToMaster(gmtCoord,ppp[nGmtHits].xyz());
	//translate into TPC coordinate
	Double_t Z = StGmtGeom::getModuleZ(module);
	double Phi = StGmtGeom::getModulePhi(module)+ TMath::Sign(Y/ R,Z);
	tpclocal[0] = R*TMath::Cos(Phi);
	tpclocal[1] = R*TMath::Sin(Phi);
	tpclocal[2] = Z+ TMath::Sign(X,Z);
	
	//test
	LOG_INFO << "TEST PROJ "<< module << endm;
	TVector3 lol(tpclocal[0],tpclocal[1],tpclocal[2]);
	TVector3 loldet(0.,0.,0.);
	ProjOnModule(module,lol,loldet);
	LOG_INFO << "X,Y=" << X <<", "<< Y << endm; 
	LOG_INFO << "LOC=" << lol.x()<<", " << lol.y()<<", " << lol.z()<< endm; 
	LOG_INFO << "DET=" << loldet.x() <<", " <<loldet.y()<<", "<< loldet.z() << endm; 
	
	tpc2Glob.LocalToMaster(tpclocal,tpcglobal);
	ooo[nGmtHits].set(tpcglobal[0],tpcglobal[1],tpcglobal[2]);
	TVector3 v(tpcglobal[0],tpcglobal[1],tpcglobal[2]);
	//if(module == 6) 
	vhit.push_back(v);
	nGmtHits++;
	//if (nGmtHits==32) break;
      }
    if (! nGmtHits) return kStOK;
    StSPtrVecTrackNode& theNodes = eventPtr->trackNodes();
    UInt_t nnodes = theNodes.size();
    
    for (UInt_t i=0; i<nnodes; i++) {
      for (UInt_t j=0; j<theNodes[i]->entries(global); j++) {
	StTrack* gTrack = (StTrack *)theNodes[i]->track(global,j);
	if (!gTrack) continue;
	if (gTrack->fitTraits().numberOfFitPoints() < 20 ) continue; //cut on track quality
	hTrFitPts->Fill(gTrack->fitTraits().numberOfFitPoints());
	StTrackGeometry* gTrackGeom = gTrack->outerGeometry();
	StPhysicalHelixD hh = gTrackGeom->helix();
	pairD sTof = hh.pathLength(R);	
	StThreeVector<double> locR(-999,-999,-999);
	if(sTof.first > 0 || sTof.second > 0) {
	  Double_t rTof =  (sTof.first < 0 || sTof.second < 0) 
	    ? max(sTof.first, sTof.second) : min(sTof.first, sTof.second); 
	  StThreeVector<double> pos;
	  if(rTof>0) pos = hh.at(rTof);
	  GmtOnGlob.MasterToLocal(pos.xyz(), locR.xyz());
	}
	if (TMath::Abs(locR.x()) > 20 || TMath::Abs(locR.z()) > 20) continue;
	for (UInt_t k=0;k<(UInt_t)nGmtHits;k++) 	    {
	  StThreeVectorD nGMTloc(0.,0.,1.);
	  StThreeVectorD nGMTglob;
	  GmtOnGlob.LocalToMasterVect(nGMTloc.xyz(),nGMTglob.xyz());
	  StThreeVectorD xGMTloc(0.,0.,0.);
	  StThreeVectorD xGMTglob;
	  GmtOnGlob.LocalToMaster(xGMTloc.xyz(),xGMTglob.xyz());
	  Double_t pathlenGMT = hh.pathLength(xGMTglob,nGMTglob);
	  StThreeVectorD predGMTglob = hh.at(pathlenGMT);
	  StThreeVectorD predGMTloc;
	  GmtOnGlob.MasterToLocal(predGMTglob.xyz(), predGMTloc.xyz());
	  //Calulation of a normal vector to the plane in a very clear fashion
	  double phi = StGmtGeom::getModulePhi(module);
	  Double_t loc[3], glo[3], hitg[3], hitl[3]; 
	  hitg[0]= ooo[k].x();
	  hitg[1]= ooo[k].y();
	  hitg[2]= ooo[k].z();
	  
	  // LOG_INFO << "hit in glob coor : (" << hitg[0] << ", "   << hitg[1] << ", " <<   hitg[2] << ") " << endm;
	  tpc2Glob.MasterToLocal(hitg,hitl); // hit in local coor
	  //LOG_INFO << "hit in Local coor : (" << hitl[0] << ", "   << hitl[1] << ", " <<   hitl[2] << ") " << endm;
	  //tpc2Glob.LocalToMaster(hitl,hitg); // hit in local coor
	  //LOG_INFO << "hit in Glob again coor : (" << hitg[0] << ", "   << hitg[1] << ", " <<   hitg[2] << ")\n\n\n" << endm;
	  
	  loc[0] = TMath::Cos(phi);
	  loc[1] = TMath::Sin(phi);
	  loc[2] = 0;
	  StThreeVectorD n;
	  tpc2Glob.LocalToMaster(loc,n.xyz());   
	  //get the pathlength at the intersection between the helix and the plane define by n and ooo[k]
	  Double_t pathlen = hh.pathLength(ooo[k],n);
	  //get da position in global coor
	  StThreeVectorD dcapt = hh.at(pathlen);
	  glo[0]=dcapt.x(); glo[1]=dcapt.y(); glo[2]=dcapt.z();
	  TVector3 v(glo[0],glo[1],glo[2]);
	  //go back in detector coordinate
	  tpc2Glob.MasterToLocal(glo,loc);
	  Double_t dist =  sqrt((loc[0]-hitl[0])* (loc[0]-hitl[0])+ (loc[1]-hitl[1])* (loc[1]-hitl[1]) +  (loc[2]-hitl[2])* (loc[2]-hitl[2])) ;
	  //LOG_INFO << "LOCAL when saved : (" << hitl[0] << ", "   << hitl[1] << ", " <<   hitl[2] << ")" << endm;
	  //LOG_INFO << "L_TPC when saved : (" << loc[0] << ", "   << loc[1] << ", " <<   loc[2] << ")" << endm;
	  //LOG_INFO << " Distance " << dist << endm;
	  //save
	  
	  //LOG_INFO << "Module "<< module <<" x0,y0 :  "<< X0 << ", " << Y0 << ")\n\n\n\n\n" << endm;
	  if( dist < 50){//to avoid tracks that are far away from everything
	    TVector3 v3tpc(loc[0],loc[1],loc[2]);
	    TVector3 v3tpcDet(0.,0.,0.);
	    ProjOnModule(module,v3tpc,v3tpcDet);
	    
	    TVector3 v3hit(hitl[0],hitl[1],hitl[2]);
	    TVector3 v3hitDet(0.,0.,0.);
	    ProjOnModule(module,v3hit,v3hitDet);
	    
	    hTpcXY[module]->Fill(v3tpcDet.x(),v3tpcDet.y());
	    hTpcXYGlob->Fill(v3tpcDet.x(),v3tpcDet.y());
	    if(v3tpcDet.x() > 0 && v3tpcDet.x() < 10 && 
	       v3tpcDet.y()> 0 && v3tpcDet.y() < 10) //in GMT acceptance 
	      {
		hNTPChits->Fill(module);
		//}
		if(mHel[module]==NULL) //store helix to draw for fun
		  mHel[module]=new StPhysicalHelixD(hh);
	      }
	    //if(i==0 && j==0){//everything GMT related.
	    hGmtXY[module]->Fill(v3hitDet.x(),v3hitDet.y());
	    hGmtXYGlob->Fill(v3hitDet.x(),v3hitDet.y());
	    //}
	    
	    hResX[module]->Fill(v3tpcDet.x()-v3hitDet.x());
	    hResXGlob->Fill(v3tpcDet.x()-v3hitDet.x());
	    hResYGlob->Fill(v3tpcDet.y()-v3hitDet.y());
	    hResY[module]->Fill(v3tpcDet.y()-v3hitDet.y());
	    //correlation
	    hGmtCorXX->Fill(v3tpcDet.x(),v3hitDet.x());
	    hGmtCorXY->Fill(v3tpcDet.x(),v3hitDet.y());
	    hGmtCorYY->Fill(v3tpcDet.y(),v3hitDet.y());
	    hGmtCorYX->Fill(v3tpcDet.y(),v3hitDet.x());
	    
	    if( dist < 15)vhitTPC.push_back(v);
	  }
	}
      }
    }
  }//end for modules
  runningEvtNr++;
  return ierr;
};

void StGmtClusterPlotter::ProjOnModule(const int module, const TVector3 &vloc, TVector3 &vdet){ //convert tpc local coordinate into gmt detector coordinate
  Double_t R = 85.606 * 2.54; // inches => cm, from 2012-10-31 email of W.J. Llope to stargmt-l
  
  //Vector base calculation
  TVector3 xx, yy, oo; 
  double X=0, Y=0;
  Double_t Z = StGmtGeom::getModuleZ(module);
  double Phi = StGmtGeom::getModulePhi(module)+ TMath::Sign(Y/ R,Z);
  oo.SetX(R*TMath::Cos(Phi));
  oo.SetY(R*TMath::Sin(Phi));
  oo.SetZ(Z+ TMath::Sign(X,Z));//origin
  
  X=1; Y=0;
  Phi = StGmtGeom::getModulePhi(module)+ TMath::Sign(Y/ R,Z);
  xx.SetX(R*TMath::Cos(Phi));
  xx.SetY(R*TMath::Sin(Phi));
  xx.SetZ(Z+ TMath::Sign(X,Z));//x axis
  xx=xx-oo;
  
  X=0; Y=1;
  Phi = StGmtGeom::getModulePhi(module)+ TMath::Sign(Y/ R,Z);
  yy.SetX(R*TMath::Cos(Phi));
  yy.SetY(R*TMath::Sin(Phi));
  yy.SetZ(Z+ TMath::Sign(X,Z));
  yy=yy-oo;
  
  vdet = vloc-oo; //vector from the origin of the module to the point to transform in det. corrdinate
  double xd = vdet*xx; //Projection in X axis of the detector coordinate system
  double yd = vdet*yy; //same for Y
  vdet.SetX(xd);
  vdet.SetY(yd);
  vdet.SetZ(0.);
}

Int_t StGmtClusterPlotter::Finish(){
  myRootFile->cd();
  DrawBoxes(); 
  myRootFile->Write();
  myRootFile->Close();
  Int_t ierr = kStOk; 
  return ierr;
};


/**
   construct histograms
   
*/
Int_t StGmtClusterPlotter::Init(){
  
  myRootFile=new TFile("clusterPlotter.root","RECREATE");
  h3Dhits = new TH3D("h3Dhits","h3Dhits", 100, 50,250, 100,-250,250, 100,-250,250);
  hGmtXY=new TH2D*[kGmtNumModules];
  hGmtXYGlob = new  TH2D("XY_All","XY_All",400,-20,20,400,-20,20);
  hGmtHitXY=new TH2D*[kGmtNumModules];
  hGmtHitXYGlob = new  TH2D("XY_Hit_All","XY_Hit_All",200,0,10,200,0,10);
  hTpcXY=new TH2D*[kGmtNumModules];
  hTpcXYGlob = new  TH2D("XY_Tpc_All","XY_Tpc_All",400,-50,50,400,-50,50);
  hResX= new TH1D*[kGmtNumModules];
  hResXGlob= new TH1D("hResX","hResX",1000,-5,5);
  hResY= new TH1D*[kGmtNumModules];
  hResYGlob= new TH1D("hResY","hResY",1000,-5,5);
  
  hNGMThits = new TH1D ("nGMThits","nGMThits",kGmtNumModules,0,kGmtNumModules);
  hNTPChits = new TH1D ("nTPChits","nTPChits",kGmtNumModules,0,kGmtNumModules);
  
  mHel=new StPhysicalHelixD*[kGmtNumModules];
  //correlations
  hGmtCorXX = new  TH2D("Corr_TpcX_GMTX","Corr_TpcX_GMTX",400,-20,20,400,-5,15);
  hGmtCorXY = new  TH2D("Corr_TpcX_GMTY","Corr_TpcX_GMTY",400,-20,20,400,-5,15);
  hGmtCorYY = new  TH2D("Corr_TpcY_GMTY","Corr_TpcY_GMTY",400,-20,20,400,-5,15);
  hGmtCorYX = new  TH2D("Corr_TpcY_GMTX","Corr_TpcY_GMTX",400,-20,20,400,-5,15);
  
  //For cuts
  hTrFitPts = new TH1D("Tracks_N_Fitted_points","Tracks_N_Fitted_points",250,0,250);
  hGmtAmpX = new TH1D("GmtAmpX","GmtAmpX",1000,0,10000);
  hGmtAmpY = new TH1D("GmtAmpY","GmtAmpY",1000,0,10000);
  hGmtAmpRatio = new TH1D("GmtAmpRatio","GmtAmpRatio",200,0,1);
  
  for(int iD=0;iD<kGmtNumModules;iD++)
    {
      char buffer[200];
      sprintf(buffer,"GmtClusterXY_Mod%d",iD);
      hGmtXY[iD]=new TH2D(buffer,buffer,400,-20,20,400,-20,20);
      sprintf(buffer,"GmtClusterHitXY_Mod%d",iD);
      hGmtHitXY[iD]=new TH2D(buffer,buffer,200,0,10,200,0,10);
      sprintf(buffer,"TpcClusterXY_Mod%d",iD);
      hTpcXY[iD]=new TH2D(buffer,buffer,400,-50,50,400,-50,50);
      sprintf(buffer,"ResX_Mod%d",iD);
      hResX[iD]=new TH1D(buffer,buffer,1000,-5,5);
      sprintf(buffer,"ResY_Mod%d",iD);
      hResY[iD]=new TH1D(buffer,buffer,1000,-5,5);
      
      mHel[iD]=NULL;
    }
  
  Int_t ierr = kStOk;
  return ierr;
};
