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
  Double_t R = 85.606 * 2.54; // inches => cm, from 2012-10-31 email of W.J. Llope to stargmt-l
  LOG_INFO <<"In plotter make " << endm;
  for(int module=0;module<  kGmtNumModules;module++)
    {
      
      StGmtHitCollection* clusterCol=mGmtCollectionPtr->getHitCollection(module); if(!clusterCol) continue;
      const StSPtrVecGmtHit &hitVec=clusterCol->getHitVec(); if(!hitVec.size()) continue;
      int nGmtHits =0;
      Double_t tpclocal[3];
      Double_t tpcglobal[3];
      const TGeoHMatrix& tpc2Glob = gStTpcDb->Tpc2GlobalMatrix();
      LOG_INFO <<"Got GMT collection, looking at " << hitVec.size() << " cluster ..  " <<endm;
      StSPtrVecGmtHitConstIterator hitIter;
      for(hitIter=hitVec.begin();hitIter != hitVec.end();hitIter++)
	{
	  //get XY hit
	  Double_t X=(*hitIter)->getLocalX();
	  Double_t Y=(*hitIter)->getLocalY();
	  
	  //store Amplitude for future cut
	  Double_t AX=(*hitIter)->getAdcX();
	  Double_t AY=(*hitIter)->getAdcY();
	  
	  if(AX<200 || AY<200) continue; //noise cut
	  
 	  hGmtAmpX->Fill(AX);
	  hGmtAmpY->Fill(AY);
	  if(AX>AY)
	    hGmtAmpRatio->Fill(AY/AX);
	  else
	    hGmtAmpRatio->Fill(AX/AY);
	  
	  hGmtHitXY[module]->Fill(X,Y);
	  hGmtHitXYGlob->Fill(X,Y);
	  hNGMThits->Fill(module);
	  
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
      
      StSPtrVecTrackNode& theNodes = eventPtr->trackNodes();
      unsigned int nnodes = theNodes.size();
      
      for (unsigned int i=0; i<nnodes; i++) {
	for (unsigned int j=0; j<theNodes[i]->entries(global); j++) {
	  StTrack* tri = (StTrack *)theNodes[i]->track(global,j);
	  if (!tri) continue;
	  if (tri->fitTraits().numberOfFitPoints() < 20 ) continue; //cut on track quality
     	  hTrFitPts->Fill(tri->fitTraits().numberOfFitPoints());
	  StTrackGeometry* triGeom = tri->outerGeometry();
	  StPhysicalHelixD hh = triGeom->helix();
	  for (unsigned int k=0;k<(unsigned int)nGmtHits;k++) 
	    {
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
		
	      loc[0] = TMath::Cos(phi) + hitl[0];
	      loc[1] = TMath::Sin(phi) + hitl[1];
	      loc[2] = 0 + hitl[2];              //hit + 1 in local coor
	      tpc2Glob.LocalToMaster(loc,glo);   
	      StThreeVectorD n;
	      n.set(glo[0]-hitg[0],glo[1]-hitg[1], glo[2]-hitg[2]);
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

StGmtClusterPlotter::StGmtClusterPlotter( const Char_t* name): StMaker( name ),runningEvtNr(0)
{
  
  
};

StGmtClusterPlotter::~StGmtClusterPlotter()
{
  //delete histogram arrays
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


void StGmtClusterPlotter::DrawBoxes(){ // to draw the GMT boxes in 3D for event display
  LOG_INFO << "IN StGmtClusterPlotter::DrawBoxes(){" << endm;
  Double_t R = 85.606 * 2.54; // inches => cm, from 2012-10-31 email of W.J. Llope to stargmt-l
  const TGeoHMatrix& tpc2Glob = gStTpcDb->Tpc2GlobalMatrix();
  TCanvas * c = new TCanvas("c3d","c3d");
  h3Dhits->Draw();
  for(int module=0;module<kGmtNumModules;module++){
    pl = new TPolyLine3D(5);
    Double_t tpclocal[3];
    Double_t tpcglobal[3];  
    int a=0;
    double X=0, Y=0;
    Double_t Z = StGmtGeom::getModuleZ(module);
    double Phi = StGmtGeom::getModulePhi(module)+ TMath::Sign(Y/ R,Z);
    tpclocal[0] = R*TMath::Cos(Phi);
    tpclocal[1] = R*TMath::Sin(Phi);
    tpclocal[2] = Z+ TMath::Sign(X,Z);
    tpc2Glob.LocalToMaster(tpclocal,tpcglobal); //for (int i=0;i<3;i++)tpcglobal[i] = tpclocal[i];
    pl->SetPoint(a,tpcglobal[0],tpcglobal[1],tpcglobal[2]);a++;

    X=10, Y=0;
    Z = StGmtGeom::getModuleZ(module);
    Phi = StGmtGeom::getModulePhi(module)+ TMath::Sign(Y/ R,Z);
    tpclocal[0] = R*TMath::Cos(Phi);
    tpclocal[1] = R*TMath::Sin(Phi);
    tpclocal[2] = Z+ TMath::Sign(X,Z);
    tpc2Glob.LocalToMaster(tpclocal,tpcglobal);//for (int i=0;i<3;i++)tpcglobal[i] = tpclocal[i];
    pl->SetPoint(a,tpcglobal[0],tpcglobal[1],tpcglobal[2]);a++;

    X=10, Y=10;
    Z = StGmtGeom::getModuleZ(module);
    Phi = StGmtGeom::getModulePhi(module)+ TMath::Sign(Y/ R,Z);
    tpclocal[0] = R*TMath::Cos(Phi);
    tpclocal[1] = R*TMath::Sin(Phi);
    tpclocal[2] = Z+ TMath::Sign(X,Z);
    tpc2Glob.LocalToMaster(tpclocal,tpcglobal);//for (int i=0;i<3;i++)tpcglobal[i] = tpclocal[i];
    pl->SetPoint(a,tpcglobal[0],tpcglobal[1],tpcglobal[2]);a++;

    X=0, Y=10;
    Z = StGmtGeom::getModuleZ(module);
    Phi = StGmtGeom::getModulePhi(module)+ TMath::Sign(Y/ R,Z);
    tpclocal[0] = R*TMath::Cos(Phi);
    tpclocal[1] = R*TMath::Sin(Phi);
    tpclocal[2] = Z+ TMath::Sign(X,Z);
    tpc2Glob.LocalToMaster(tpclocal,tpcglobal);//for (int i=0;i<3;i++)tpcglobal[i] = tpclocal[i];
    pl->SetPoint(a,tpcglobal[0],tpcglobal[1],tpcglobal[2]);a++;

    
    X=0, Y=0;
    Z = StGmtGeom::getModuleZ(module);
    Phi = StGmtGeom::getModulePhi(module)+ TMath::Sign(Y/ R,Z);
    tpclocal[0] = R*TMath::Cos(Phi);
    tpclocal[1] = R*TMath::Sin(Phi);
    tpclocal[2] = Z+ TMath::Sign(X,Z);
    tpc2Glob.LocalToMaster(tpclocal,tpcglobal);//for (int i=0;i<3;i++)tpcglobal[i] = tpclocal[i];
    pl->SetPoint(a,tpcglobal[0],tpcglobal[1],tpcglobal[2]);a++;
    
    LOG_INFO <<"GMT BOX MODULE :"<< module << " = " << tpcglobal[0]<< ", " << tpcglobal[1]<< ", "<< tpcglobal[2] << endm;
    
    pl->SetLineColor(kBlue);
    pl->Draw("same");  

    //Draw helixes for the lol
    if(true) continue;
    if(mHel[module]==NULL) continue;
    LOG_INFO << "Helix for module " << module << " found." << endm;
    const int nHpt = 50;
    TPolyMarker3D * mH = new TPolyMarker3D(nHpt);
    //GET NORMAL PLANE AND PATHLENGTH
    double phi = StGmtGeom::getModulePhi(module);
    Double_t loc[3], glo[3]; 
    loc[0] = TMath::Cos(phi) + tpclocal[0];
    loc[1] = TMath::Sin(phi) + tpclocal[1];
    loc[2] = 0 + tpclocal[2];              //hit + 1 in local coor
    tpc2Glob.LocalToMaster(loc,glo);   
    StThreeVectorD n;
    n.set(glo[0]-tpcglobal[0],glo[1]-tpcglobal[1], glo[2]-tpcglobal[2]);
   
    int i=0;
    for (double iPath=0.; iPath<nHpt; iPath=iPath+1)
      {
 	tpcglobal[0] = tpcglobal[0] + tpcglobal[0]*n[0];
 	tpcglobal[1] = tpcglobal[1] + tpcglobal[1]*n[1];
 	tpcglobal[2] = tpcglobal[2] + tpcglobal[2]*n[2];
 	Double_t pathlen = mHel[module]->pathLength(tpcglobal,n);
 	StThreeVectorD dcapt = mHel[module]->at(TMath::Sign(iPath,pathlen));
	
 	mH->SetPoint(i,dcapt.x(),dcapt.y(),dcapt.y());
 	i++;}  
    mH->SetMarkerStyle(6);
    mH->SetMarkerColor(kBlack);
    mH->Draw("same");
  }
  
  //GMT HITS
  TPolyMarker3D * pm = new TPolyMarker3D(vhit.size());
  for (unsigned int i=0;i<vhit.size();i++)
    {
      pm->SetPoint(i,vhit.at(i).x(),vhit.at(i).y(),vhit.at(i).z());
      /*
      // this part is to plot the normal vector  
      double phi = StGmtGeom::getModulePhi(6);
      Double_t loc[3], glo[3], hitg[3], hitl[3]; 
      hitg[0]= vhit.at(i).x();
      hitg[1]= vhit.at(i).y();
      hitg[2]= vhit.at(i).z();
      tpc2Glob.MasterToLocal(hitg,hitl);
      loc[0] = TMath::Cos(phi) + hitl[0];
      loc[1] = TMath::Sin(phi) + hitl[1];
      loc[2] = 0 + hitl[2];
      tpc2Glob.LocalToMaster(loc,glo);
      TVector3 n(glo[0],glo[1], glo[2]);
      n = n - vhit.at(i);//difference between the 2points +> vector
      TPolyLine3D * pmn = new TPolyLine3D(2);
      pmn->SetPoint(0,vhit.at(i).x(),vhit.at(i).y(),vhit.at(i).z());
      pmn->SetPoint(1,n.x(),n.y(),n.z());
      pmn->Draw("same");
      */
    }
  pm->SetMarkerStyle(7);
  pm->SetMarkerColor(kBlue);
  pm->Draw("same");
  
  //PROJECTED HELIXES
  TPolyMarker3D * pm2 = new TPolyMarker3D(vhitTPC.size());
  for (unsigned int i=0;i<vhitTPC.size();i++)
    {
      pm2->SetPoint(i,vhitTPC.at(i).x(),vhitTPC.at(i).y(),vhitTPC.at(i).z());
    }
  pm2->SetMarkerStyle(7);
  pm2->SetMarkerColor(kRed);
  pm2->Draw("same");
  
  c->Write();
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
ClassImp(StGmtClusterPlotter);
