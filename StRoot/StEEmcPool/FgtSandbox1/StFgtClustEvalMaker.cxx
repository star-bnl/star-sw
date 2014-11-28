// *-- Author : J.Balewski
// 
// $Id: StFgtClustEvalMaker.cxx,v 1.2 2011/04/11 19:35:38 fisyak Exp $

#include <TVector3.h>
#include <TH2.h>
#include <TF1.h>
#include <TFile.h>
#include <TLine.h>
#include <TPolyLine.h>
#include <TCrown.h>
#include <TRandom3.h>
#include "TMath.h"
#include "StFgtClustEvalMaker.h"
#include "StFgtSlowSimuMaker.h"
#include "StFgtClustFindMaker.h"

ClassImp(StFgtClustEvalMaker)

//--------------------------------------------
StFgtClustEvalMaker::StFgtClustEvalMaker(const char *name):StMaker(name){
  /// Class Constructor.  
  setHList(0);
  memset(hA,0,sizeof(hA));
  geom=new StFgtGeom();
  par_minDelRad=0.07; //(cm) radial match cut off ,default=0.07
  par_minRdPhi=0.02; //(cm)  match cut off ,default=0.02
}

//--------------------------------------------
void 
StFgtClustEvalMaker::Clear(Option_t *) {
   StMaker::Clear();
}

//--------------------------------------------
StFgtClustEvalMaker::~StFgtClustEvalMaker(){

}

//_______________________________________________
//________________________________________________
void
StFgtClustEvalMaker::saveHisto(TString fname){
  TString outName=fname+".hist.root";
  TFile f( outName,"recreate");
  assert(f.IsOpen());
  printf("%d histos are written  to '%s' ...\n",HList->GetEntries(),outName.Data());

  HList->Write();
  f.Close();

}


//--------------------------------------------
//--------------------------------------------
//--------------------------------------------
Int_t 
StFgtClustEvalMaker::Finish(){
  LOG_INFO<<"::Finish() \n"<<  endm; 


  return StMaker::Finish();
}

//--------------------------------------------
//--------------------------------------------
//--------------------------------------------
Int_t 
StFgtClustEvalMaker::Make(){
  mInpEve++;
  
  LOG_INFO <<"::Make() inpEve="<<  mInpEve<<endm;
  StFgtSlowSimuMaker *ssMk=(StFgtSlowSimuMaker *) GetMaker("FgtSlowSimu");
  assert(ssMk);
  StFgtClustFindMaker *cfMk=(StFgtClustFindMaker *) GetMaker("FgtClustFind");
  assert(cfMk);
  
  printf(" INPUT: \n disk  #g2t_tracks  #Rad_clust   #Phi_clust \n");


  // .... match 1D clusters in every plane of all disks
  for(int iDisk=0;iDisk<kFgtMxDisk;iDisk++) {
    int nG=0;
    for(int iQuad=0; iQuad<kFgtMxQuad;iQuad++) nG+=ssMk->mG2tHitList[iDisk][iQuad].size();

    int nrR= cfMk->mRadClustList[iDisk].size();
    int nrP= cfMk->mPhiClustList[iDisk].size();

    int nmR=matchRadClust1D( ssMk->mG2tHitList[iDisk],cfMk->mRadClustList[iDisk]);
    int nmP=matchPhiClust1D( ssMk->mG2tHitList[iDisk],cfMk->mPhiClustList[iDisk]);
    // printf("match Disk=%d  nG=%d   nrR=%d->%d  nrP=%d->%d\n",iDisk+1,nG,nrR,nmR, nrP,nmP);

    hA[0]->Fill(10*iDisk+0,nG);
    hA[0]->Fill(10*iDisk+1,nrR);
    hA[0]->Fill(10*iDisk+2,nrP);
    hA[0]->Fill(10*iDisk+3,nmR);
    hA[0]->Fill(10*iDisk+4,nmP);
    if(nrR>nG) printf("#OV Rad %d %d\n",nrR,nG);
    if(nrR>nmR) printf("#MI Rad %d %d\n",nrR,nmR);
    if(nrP>nG) printf("#OV Phi %d %d\n",nrP,nG);
    if(nrP>nmP) printf("#MI Phi %d %d\n",nrP,nmP);
  }
 return kStOK;
}

 
//--------------------------------------------
//--------------------------------------------
//--------------------------------------------
int
StFgtClustEvalMaker::matchRadClust1D( vector<fgt_g2t_auxil> *g2tTrL,  vector<fgt_cluster1D> &clL){

  /* brute force used,
     unnecessary scans over all clusters in one disk, could do it by quadrant
  */

  int nM=0;
  for(int iQuad=0; iQuad<kFgtMxQuad;iQuad++)     
    for(UInt_t ih=0;ih<g2tTrL[iQuad].size();ih++) {
      double gR=g2tTrL[iQuad][ih].Rlab.Perp(); // radial distance from Z-axis
      double minDelR=par_minDelRad;
      int jc=-1;// best match
      for(UInt_t ic=0;ic<clL.size();ic++) {
	if(clL[ic].matched) continue; //match only once
	double delR=clL[ic].position - gR;
	if(TMath::Abs(minDelR)<TMath::Abs(delR)) continue;
	minDelR=delR;// better match found
	jc=ic;
      }
      if(jc>=0) { 
	hA[5]->Fill(minDelR*1e4); // in um
	hA[7]->Fill(gR,minDelR*1e4); // in um
	hA[9]->Fill(g2tTrL[iQuad][ih].Rlab.x(),g2tTrL[iQuad][ih].Rlab.y());
	//	printf("match Rad: iQ=%d gT=%d gR=%.2f  match  jc=%d  minDelR=%.3f\n",iQuad, ih,gR,jc,minDelR);
	nM++;
	clL[jc].matched=true;
      } //else	printf("nomatch ih=%d gR=%.2f\n", ih,gR);

    }
  return nM;
}

//--------------------------------------------
//--------------------------------------------
//--------------------------------------------
int
StFgtClustEvalMaker::matchPhiClust1D( vector<fgt_g2t_auxil> *g2tTrL,  vector<fgt_cluster1D> &clL){
  const double pi=2.*acos(0.),  dpi=2.*pi; // double pi

  /* brute force used,
     unnecessary scans over all clusters in one disk, could do it by quadrant
  */

  int nM=0;
  for(int iQuad=0; iQuad<kFgtMxQuad;iQuad++)     
    for(UInt_t ih=0;ih<g2tTrL[iQuad].size();ih++) {
      double gR=g2tTrL[iQuad][ih].Rlab.Perp(); // radial distance from Z-axis
      double gPhi=g2tTrL[iQuad][ih].Rlab.Phi(); // radial distance from Z-axis
      while (gPhi<-pi) gPhi+=dpi;
      while (gPhi>pi) gPhi-=dpi; // angle in [0,2pi]

      double minRdPhi=par_minRdPhi;
      int jc=-1;// best match
      for(UInt_t ic=0;ic<clL.size();ic++) {
	if(clL[ic].matched) continue; //match only once
	double rPhi=clL[ic].position;
	while (rPhi<-pi) rPhi+=dpi;
	while (rPhi>pi) rPhi-=dpi;// angle in [0,2pi]
	double delPhi=gPhi-rPhi;
	if(delPhi>pi) delPhi-=dpi;
	if(delPhi<-pi) delPhi+=dpi;
	double RdelPhi=gR*delPhi;
	//printf("phi1,2: g r=%.3f phi/deg=%.2f, r phi/deg=%.2f,  delPR/cm=%.3f  \n",gR,gPhi/3.1416*180.,rPhi/3.1416*180.,RdelPhi);

	if(TMath::Abs(minRdPhi)<TMath::Abs(RdelPhi)) continue;
	minRdPhi=RdelPhi;// better match found
	jc=ic;
      }
      if(jc>=0) { 
	hA[6]->Fill(minRdPhi*1e4); // in um
	hA[8]->Fill(gR,minRdPhi*1e4); // in um
	hA[10]->Fill(g2tTrL[iQuad][ih].Rlab.x(),g2tTrL[iQuad][ih].Rlab.y());
	//printf("match Phi iQ=%d gT=%d gR=%.2f gPhi/deg=%.2f  match  jc=%d  minRdPhi=%.3f\n",iQuad, ih,gR,gPhi/3.1417*180.,jc,minRdPhi);
	nM++;
	clL[jc].matched=true;
      } //else	printf("nomatch ih=%d gR=%.2f gPhi/deg=%.2f\n", ih,gR,gPhi/3.1417*180.);

    }
  return nM;
}

//--------------------------------------------
//--------------------------------------------
//--------------------------------------------
Int_t 
StFgtClustEvalMaker::Init(){
  assert(HList);
  mInpEve=0;
//  int i;

  hA[0]=new TH1F("ev_Stat1D","Eval clust matching; x=10*disk",100, -0.5,99.5);

  double Yrange=2*geom->radStrip_pitch()*1e4;
  
  hA[5]= new TH1F("ev_errRad","error in reco  R (track-recoClust); R_err (um)",100,-par_minDelRad*1e4,par_minDelRad*1e4);
  hA[6]= new TH1F("ev_RerrPhi","error in reco phi*R   (track-recoClust); R*#Phi_err (um)",100,-par_minRdPhi*1e4,par_minRdPhi*1e4);



  hA[7]=new TH2F("ev_dRad_R","#DeltaR    accuracy of reco cluster ;thrown R (cm); #DeltaR   (um)",45,0.,45.,50,-Yrange,Yrange);
  hA[8]=new TH2F("ev_RdPhi_R","R*#Delta#Phi   accuracy of reco cluster ;thrown R (cm);R*#Delta#Phi (um)",45,0.,45.,200,-Yrange,Yrange);


  char text[1000];
  sprintf(text,"gen clust X-Y , R-matched (#DeltaR<%.3fmm);Lab  X (cm); Lab Y (cm); ",par_minDelRad*10);
  hA[9]=new TH2F("ev_RokXY",text,90,-45.,45.,90,-45.,45.);

  sprintf(text,"gen clust X-Y , #Phi-matched (R#Delta#Phi<%.3fmm);Lab  X (cm); Lab Y (cm); ",par_minRdPhi*10);
  hA[10]=new TH2F("ev_PhiokXY",text,90,-45.,45.,90,-45.,45.);


  //..............add gadgets to histos.....................
  TList *Lx; TLine *ln;    TPolyLine *pln;  

  //... strip size for cluster error plots: delRad
  double x[100],y[100];

   Lx=hA[7]->GetListOfFunctions(); // delR
   double y1=par_minDelRad*1e4;
   ln=new TLine(0,y1,45,y1); ln->SetLineColor(kGreen);Lx->Add(ln);
   ln=new TLine(0,-y1,45,-y1); ln->SetLineColor(kGreen);Lx->Add(ln);
   x[0]=x[9]=geom->Rin();  y[0]=y[9]=0;
   x[1]=x[8]=x[0];         y[1]=geom->radStrip_pitch()*1e4/2; y[8]=-y[1];
   x[2]=x[7]=geom->Rmid(); y[2]=y[1];                y[7]=-y[2];
   x[3]=x[6]=x[2];         y[3]=geom->radStrip_pitch()*1e4/2; y[6]=-y[3];
   x[4]=x[5]=geom->Rout(); y[4]=y[3];                y[5]=-y[4];
   pln=new TPolyLine(10,x,y);
   pln->SetLineColor(kRed); Lx->Add(pln);


   Lx=hA[8]->GetListOfFunctions(); // R*delPhi
   y1=par_minRdPhi*1e4;
   ln=new TLine(0,y1,45,y1); ln->SetLineColor(kGreen);Lx->Add(ln);
   ln=new TLine(0,-y1,45,-y1); ln->SetLineColor(kGreen);Lx->Add(ln);
   double hl=geom->phiStrip_pitch()*1e4/2 * geom->Rin();
   double hh=2*hl;
   double hm=hh*geom->Rmid()/geom->Rmid();
   x[0]=x[12]=x[11]=geom->Rin();  y[0]=y[12]=hl;  y[11]=-y[0];
   x[1]=x[10]=geom->Rmid();  y[1]=hh;     y[10]=-y[1];
   x[2]=x[9]=x[1];           y[2]=hm;     y[9]=-y[2];
   x[3]=x[8]=geom->Rmid(); y[3]=hh;     y[8]=-y[3];
   x[4]=x[7]=x[3];           y[4]=hl;     y[7]=-y[4];
   x[5]=x[6]=geom->Rout();   y[5]=hh;     y[6]=-y[5];
   pln=new TPolyLine(13,x,y);  pln->SetLineColor(kRed);Lx->Add(pln);


  for(int i=0;i<mxH;i++) 
    if(hA[i]) HList->Add(hA[i]);
  
  LOG_INFO<<Form("::Init params match limit (cm) delR=%.3f  R*delPhi=%.3f", par_minDelRad,par_minRdPhi)<<  endm; 


  return StMaker::Init();
}


/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

// $Log: StFgtClustEvalMaker.cxx,v $
// Revision 1.2  2011/04/11 19:35:38  fisyak
// Replace uint by UInt_t, use TMath
//
// Revision 1.1  2011/04/07 19:31:22  balewski
// start
//


 


