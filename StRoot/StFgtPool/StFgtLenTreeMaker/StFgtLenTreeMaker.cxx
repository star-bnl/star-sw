#include "StFgtLenTreeMaker.h"
#include "StRoot/StEvent/StFgtCollection.h"
#include "StRoot/StEvent/StFgtHitCollection.h"
#include "StRoot/StEvent/StFgtHit.h"
#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"
#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StEvent/StEventInfo.h"
#include <TH2D.h>
#include <TROOT.h>
#include <TStyle.h>
#include <TCanvas.h>


Int_t StFgtLenTreeMaker::Make()
{

   StEvent* eventPtr = 0;
   eventPtr = (StEvent*)GetInputDS("StEvent");
   (*outTxtFile) <<endl<<endl<<" ------new event: " << eventPtr->info()->id() << "----------------------" << " running nr: " << runningEvtNr << "------" << endl;
   
   Bool_t flag=false;
   for(int iDx=0;iDx<kFgtNumDiscs;iDx++)
     {
       Ncl[iDx]=0;
       for(int iCl=0;iCl<10;iCl++)
	 {
	   cl_geoId[iDx][iCl]=-1;
	   cl_quad[iDx][iCl]=-1;
	   cl_z[iDx][iCl]=0.;cl_ez[iDx][iCl]=0.;
	   cl_phi[iDx][iCl]=0;cl_ephi[iDx][iCl]=0;
	   cl_r[iDx][iCl]=-1;cl_er[iDx][iCl]=-1;
	   cl_charge[iDx][iCl]=-1;cl_echarge[iDx][iCl]=-1;
	   cl_numStrips[iDx][iCl]=-1;
	   cl_tStrip[iDx][iCl]=-1;
	   cl_layer[iDx][iCl]=' ';
	   cl_key[iDx][iCl]=-1;
	   maxadc[iDx][iCl]=-1;seedadc[iDx][iCl]=-1;
	 }
     };
   Int_t ierr = StFgtQaMaker::Make();
   cout <<"in plotter make " << endl;
   Float_t x;
   Float_t y;
   Int_t prvGeoId=-1;
   for(int iDx=0;iDx<kFgtNumDiscs;iDx++)
     {
       vector<float> vPhi[4];
       vector<float> vR[4];
       
       vector<float> vPhiCharge[4];
       vector<float> vRCharge[4];
	 
	 StFgtStripCollection& strips=*(mFgtCollectionPtr->getStripCollection( iDx ));
	 for( StSPtrVecFgtStripIterator it=strips.getStripVec().begin();it!=strips.getStripVec().end();++it)
	   {
	     Short_t quad, disc, strip;
	     Char_t layer; 
	     if(((*it)->getGeoId()-prvGeoId)>1 && prvGeoId>=0)
	       {
		 (*outTxtFile) <<endl;
	       }
	     Bool_t stripDead=false;
	     prvGeoId=(*it)->getGeoId();
	     StFgtGeom::decodeGeoId((*it)->getGeoId(),disc, quad, layer, strip);
	     if(layer!='R')
	       continue;
	     switch((*it)->getClusterSeedType())
	       {
	       case kFgtSeedType1:
		 (*outTxtFile) <<"#";
		 break;
	       case kFgtSeedType2:
		 (*outTxtFile) <<"#";
		 break;
	       case kFgtSeedType3:
		 (*outTxtFile) <<"#";
		 break;
	       case kFgtClusterPart:
		 (*outTxtFile) <<"*";
		 break;
	       case kFgtClusterEndUp:
		 (*outTxtFile) <<"*";
		 break;
	       case kFgtClusterEndDown:
		 (*outTxtFile) <<"*";
		 break;
	       case kFgtDeadStrip:
		 if(((it-1))>=strips.getStripVec().begin() && (it+1)<strips.getStripVec().end())
		   {
		     if((*(it-1))->getClusterSeedType()>kFgtDeadStrip&& (*(it+1))->getClusterSeedType()>kFgtDeadStrip)
		       (*outTxtFile) <<"*";
		     else
		       (*outTxtFile) <<"x";
		   }
		 else
		   (*outTxtFile) <<"x";
		 break;

	       default:
		 (*outTxtFile) <<"x";
	       }
	     if((*it)->getClusterSeedType()==kFgtDeadStrip) 
	       stripDead=true;
	     else
	       stripDead=false;


	     for( Int_t timebin = 0; timebin < kFgtNumTimeBins-2; ++timebin )
	       {

		 if(stripDead)
		   (*outTxtFile) << setw(4) << " ?  "<< "    ";
		 else
		   {
		     float numSig=(*it)->getAdc(timebin)/(*it)->getPedErr();
		     if(numSig >1)
		       (*outTxtFile) << setw(4) << (*it)->getAdc(timebin)<< "    ";
		     else
		       (*outTxtFile) << setw(4) << " .  "<< "    ";
		   }
	       }

	     (*outTxtFile) << " : charge: " << (*it)->getCharge()<<" +- " << (*it)->getChargeUncert() <<" location: "<<StFgtGeom::encodeGeoName(disc,quad,layer,strip);
	     (*outTxtFile) <<"/"<<(*it)->getGeoId();
	     (*outTxtFile) << " ped: " << (*it)->getPed() <<" +- " << (*it)->getPedErr();
	     (*outTxtFile) << " run evtNr " << runningEvtNr;
	     //		     (*outTxtFile)  <<" t0: " << (*it)->getFitParamT0() <<" fit chi2/ndf: " << (*it)->getFitChi2();
	     if((*it)->getClusterSeedType()==kFgtSeedType1)
	       {
		 (*outTxtFile) << " ---> seed w/ 3 high strips";
	       }
	     if((*it)->getClusterSeedType()==kFgtSeedType2)
	       (*outTxtFile) << " ---> seed w/ 2 high strips";
	     if((*it)->getClusterSeedType()==kFgtSeedType3)
	       (*outTxtFile) << " ---> seed w/ 1 high strip";
	     
	     if((*it)->getClusterSeedType()==kFgtClusterPart)
	       (*outTxtFile) << " ---> Part of cluster";
	     if((*it)->getClusterSeedType()==kFgtDeadStrip) 
	       (*outTxtFile) <<" ---> Strip is marked dead";
	     if((*it)->getClusterSeedType()==kFgtClusterEndUp) 
	       (*outTxtFile) <<" ---> End of a cluster";
	     if((*it)->getClusterSeedType()==kFgtClusterEndDown) 
	       (*outTxtFile) <<" ---> Beginning of a cluster";

	     (*outTxtFile) <<endl;
	   }

     cout <<"trying to get clusters in disc " << iDx << endl;
     StFgtHitCollection* clusterCol=mFgtCollectionPtr->getHitCollection(iDx);
     
     if(clusterCol)
       {
	 flag=true;
	 Int_t iCl=0;
	 cout <<"got collection, looking at hits ..  " <<endl;
	 const StSPtrVecFgtHit &hitVec=clusterCol->getHitVec();
	 StSPtrVecFgtHitConstIterator hitIter;
	 for(hitIter=hitVec.begin();hitIter != hitVec.end();hitIter++)
	   {
	     Int_t iq=(*hitIter)->getQuad();
	     Float_t phi=(*hitIter)->getPositionPhi();
	     Float_t ephi=(*hitIter)->getErrorPhi();
	     Float_t r=(*hitIter)->getPositionR();
	     Float_t er=(*hitIter)->getErrorR();
	     Float_t z=(*hitIter)->getPositionZ();
	     Float_t ez=(*hitIter)->getErrorZ();
	     Int_t geoId=(*hitIter)->getCentralStripGeoId();
	     Float_t charge=(*hitIter)->charge();
	     Float_t echarge=(*hitIter)->getChargeUncert();
	     Int_t numStrips=(*hitIter)->getStripWeightMap().size();
	     ///check if at least one of the strips has a valid seed)
	     Short_t quad, disc, strip;
	     Char_t layer; 

	     Bool_t containsSeed=false;
	     Int_t maxA=0;
	     Int_t seedA=0;
	     for(stripWeightMap_t::iterator it=(*hitIter)->getStripWeightMap().begin();it!=(*hitIter)->getStripWeightMap().end();it++)
	       {
		 if(it->first->getClusterSeedType()==kFgtSeedType1 || it->first->getClusterSeedType()==kFgtSeedType2 ||it->first->getClusterSeedType()==kFgtSeedType3) //require  1,2,3 strips
		   {
		     containsSeed=true;		     
		     if(seedA < it->first->getMaxAdc())seedA=it->first->getMaxAdc();
		   }
		 if(maxA < it->first->getMaxAdc())maxA=it->first->getMaxAdc();
	       }
	     if(iCl<10){
	       maxadc[iDx][iCl]=maxA;
	       seedadc[iDx][iCl]=seedA;
	     };
	     if(containsSeed)
	       {
		 //		 (*outTxtFile) <<" looking at Cluster for geoId: " << (*hitIter)->getCentralStripGeoId() << " layer: " << (*hitIter)->getLayer() <<endl;
		 //Int_t maxA=0;
		 //for(stripWeightMap_t::iterator it=(*hitIter)->getStripWeightMap().begin();it!=(*hitIter)->getStripWeightMap().end();it++)
		 //{
		 //  StFgtGeom::decodeGeoId(it->first->getGeoId(),disc, quad, layer, strip);
		 //  if(maxA < it->first->getMaxAdc())maxA=it->first->getMaxAdc();
		     //if(it->first->getClusterSeedType()==kFgtSeedType1)
		     //{
			 //		     (*outTxtFile) << " ---> seed with 3 high strips";
		     //}
		 //}
		 
		 if((*hitIter)->getLayer()!='R')
		   {
		     continue;///go to next cluster
		   }
		 
		 Short_t tDisc, tQuad,tStrip;
		 Char_t tLayer;
		 //get strip id from the geo id
		 StFgtGeom::decodeGeoId(geoId,tDisc,tQuad,tLayer,tStrip);     
		 //	     cout <<"filling charge with phi: " << phi << " r : " << r << " charge: " << charge <<" numStrips: " << numStrips<< " strip nr: " << tStrip<<endl;
		 //	     cout<<"disc nr: " << iDx <<" quad: " << iq << " numquads: " << kFgtNumQuads <<endl;
		 if(iCl<10){
		   cl_geoId[iDx][iCl]=geoId;
		   cl_quad[iDx][iCl]=iq;
		   cl_z[iDx][iCl]=z;cl_ez[iDx][iCl]=ez;
		   cl_phi[iDx][iCl]=phi;cl_ephi[iDx][iCl]=ephi;
		   cl_r[iDx][iCl]=r;cl_er[iDx][iCl]=er;
		   cl_charge[iDx][iCl]=charge;cl_echarge[iDx][iCl]=echarge;
		   cl_numStrips[iDx][iCl]=numStrips;
		   cl_tStrip[iDx][iCl]=tStrip;
		   cl_layer[iDx][iCl]=(*hitIter)->getLayer();
		   cl_key[iDx][iCl]=(*hitIter)->getKey();
		 }
		 iCl++;
	       }
	   }
	 Ncl[iDx]=iCl;
       }
     }
   iEvt=runningEvtNr;
   if(flag){
     printf("*************** FILLING THE TREE **********************\n");
     tCl->Fill();
   };
   runningEvtNr++; 
   return ierr;
};
 
 StFgtLenTreeMaker::StFgtLenTreeMaker( const Char_t* name):runningEvtNr(0)
{
  StFgtQaMaker( name, 0,0, "qName" );
  fname="hFgt";
};

StFgtLenTreeMaker::~StFgtLenTreeMaker()
{
  //delete histogram arrays
};


Int_t StFgtLenTreeMaker::Finish(){
  outTxtFile->close();
  gStyle->SetPalette(1);
  cout <<"cluster tree maker finish funciton " <<endl;
   Int_t ierr = kStOk;

  tCl->Print();
  fFgt->cd();
  ierr=tCl->Write();
  fFgt->Close();
  
  return ierr;
};

Int_t StFgtLenTreeMaker::Init(){
  
  myRootFile=new TFile("clusterPlotter.root","RECREATE");
  outTxtFile=new ofstream;
  outTxtFile->open("clusters.txt");


   Int_t ierr = kStOk;
   ierr=InitTree();
   
   hClusterCharge=new TH1D("clusterCharge","clusterCharge",100, 0, 1000);
   char buffer[100];
   hCChargePosSpacePhi=new TH2D*[kFgtNumDiscs*kFgtNumQuads];
   hCChargePosSpaceR=new TH2D*[kFgtNumDiscs*kFgtNumQuads];
   hClusSizePhi=new TH2D*[kFgtNumDiscs*kFgtNumQuads];
   hClusSizeR=new TH2D*[kFgtNumDiscs*kFgtNumQuads];
   hCChargeElecSpace=new TH2D*[kFgtNumDiscs*kFgtNumQuads];
   hClusSizeElecSpace=new TH2D*[kFgtNumDiscs*kFgtNumQuads];

   radioPlots=new TH2D*[kFgtNumDiscs];
   corrPlots=new TH2D*[kFgtNumDiscs*kFgtNumQuads];


   for(int iD=0;iD<kFgtNumDiscs;iD++)
     {
       for(int iQ=0;iQ<kFgtNumQuads;iQ++)
	 {
	   sprintf(buffer,"clusterChargeDisk%d_Quad%d_phi",iD,iQ);
	   hCChargePosSpacePhi[iD*kFgtNumQuads+iQ]=new TH2D(buffer,buffer,200,-4,4, 200, 0, 2000);
	   sprintf(buffer,"clusterChargeDisk%d_Quad%d_R",iD,iQ);
	   hCChargePosSpaceR[iD*kFgtNumQuads+iQ]=new TH2D(buffer,buffer,200,10,35, 200, 0, 2000);
	   sprintf(buffer,"clusterSizeDisk%d_Quad%d_phi",iD,iQ);
	   hClusSizePhi[iD*kFgtNumQuads+iQ]=new TH2D(buffer,buffer,1000,-4,4, 20, 0, 20);
	   sprintf(buffer,"clusterSizeDisk%d_Quad%d_R",iD,iQ);
	   hClusSizeR[iD*kFgtNumQuads+iQ]=new TH2D(buffer,buffer,1000,10,35, 20, 0, 20);
	   sprintf(buffer,"clusterSizeDisk%d_Quad%d_ElecSpace",iD,iQ);
	   hClusSizeElecSpace[iD*kFgtNumQuads+iQ]=new TH2D(buffer,buffer,100,0,1000, 20, 0, 20);
	   sprintf(buffer,"clusterChargeDisk%d_Quad%d_ElecSpace",iD,iQ);
	   hCChargeElecSpace[iD*kFgtNumQuads+iQ]=new TH2D(buffer,buffer,100,0,1000, 100, 0, 2000);
	   sprintf(buffer,"radioDisk%d_Quad_%d",iD,iQ);
	   radioPlots[iD]=new TH2D(buffer,buffer,100,-50,50,100,-50,50);
	   sprintf(buffer,"r_phi_ChargeCorr%d_Quad_%d",iD,iQ);
	   corrPlots[iD*kFgtNumQuads+iQ]=new TH2D(buffer,buffer,100,0,2000,100,0,2000);
	 }
     }
   return ierr;
};

Int_t StFgtLenTreeMaker::InitTree(){

  Int_t ierr = kStOk;
  if(fname.CompareTo("")==0){
    LOG_ERROR << "No output file name given for the TTree" << endm;
    ierr = kStErr;    
  }
  else{
    TString outName=fname+".tree.root";
    fFgt = new TFile(outName,"recreate");    
    tCl = new TTree("tCl","TTree for FGT cluster analysis");
    tCl->Branch("iEvt",&iEvt,"iEvt/I");
    tCl->Branch("Ncl",Ncl,"Ncl[6]/I");   
    tCl->Branch("cl_geoId",cl_geoId,"cl_geoId[6][10]/I");
    tCl->Branch("cl_quad",cl_quad,"cl_quad[6][10]/I");
    tCl->Branch("cl_z",cl_z,"cl_z[6][10]/F");
    tCl->Branch("cl_ez",cl_ez,"cl_ez[6][10]/F");
    tCl->Branch("cl_phi",cl_phi,"cl_phi[6][10]/F");  
    tCl->Branch("cl_ephi",cl_ephi,"cl_ephi[6][10]/F");
    tCl->Branch("cl_r",cl_r,"cl_r[6][10]/F");
    tCl->Branch("cl_er",cl_er,"cl_er[6][10]/F");
    tCl->Branch("cl_charge",cl_charge,"cl_charge[6][10]/F");
    tCl->Branch("cl_echarge",cl_echarge,"cl_echarge[6][10]/F");
    tCl->Branch("cl_numStrips",cl_numStrips,"cl_numStrips[6][10]/I");
    tCl->Branch("cl_tStrip",cl_tStrip,"cl_tStrip[6][10]/I");
    tCl->Branch("cl_layer",cl_layer,"cl_layer[6][10]/B");
    tCl->Branch("cl_key",cl_key,"cl_key[6][10]/I");
    tCl->Branch("maxadc",maxadc,"maxadc[6][10]/I");
    tCl->Branch("seedadc",seedadc,"seedadc[6][10]/I");
  };
  return ierr;
};

ClassImp(StFgtLenTreeMaker);
