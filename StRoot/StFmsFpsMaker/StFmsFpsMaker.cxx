// \class StFmsFpsMaker
// \author Akio Ogawa
//
//  $Id: StFmsFpsMaker.cxx,v 1.2 2015/09/02 16:12:29 akio Exp $
//  $Log: StFmsFpsMaker.cxx,v $
//  Revision 1.2  2015/09/02 16:12:29  akio
//  fix typo
//
//  Revision 1.1  2015/09/02 14:56:12  akio
//  Initial version of FMS-FPS correation analysis
//

#include "StFmsFpsMaker.h"

#include "StMessMgr.h"
#include "Stypes.h"

#include "StFmsDbMaker/StFmsDbMaker.h"
#include "StEventTypes.h"
#include "StEvent/StEvent.h"
#include "StEvent/StFmsCollection.h"
#include "StEvent/StFmsHit.h"
#include "StEvent/StFmsPoint.h"
#include "StMuDSTMaker/COMMON/StMuTypes.hh"

#include "TFile.h"
#include "TH1F.h"
#include "TH2F.h"

static const float E1=5.0, E2=4.0, M1=0.05, M2=0.20;
static const int FMSNHIT=99;
static const int FPSNHIT=99;

inline float project(float x, float z, float zp, float vz){
  return x/(z-vz)*(zp-vz);
}

//find minimal distance between a point (x,y) and a rectange with center (x0,y0) and width (xw,yw)
//return negative distance if its inside, positive outside
float distance(float x, float y, float x0, float y0, float xw, float yw){
  float xx=x-x0;
  float yy=y-y0;
  float dx1=xx-xw/2.0, adx1=fabs(dx1);
  float dx2=xx+xw/2.0, adx2=fabs(dx2);
  float dy1=yy-yw/2.0, ady1=fabs(dy1);
  float dy2=yy+yw/2.0, ady2=fabs(dy2);
  float adx=(adx1<adx2) ? adx1 : adx2;
  float ady=(ady1<ady2) ? ady1 : ady2;
  float ad =(adx <ady ) ? adx  : ady ; 
  float oix=dx1*dx2;
  float oiy=dy1*dy2;
  if(oix<0.0 && oiy<0.0) return -ad; //inside
  if(oix<0.0 && oiy>0.0) return ady; //outside in y direction
  if(oix>0.0 && oiy<0.0) return adx; //outside in x direction
  return sqrt( pow(fabs(xx)-xw/2.0,2.0) + pow(fabs(yy)-yw/2.0,2.0) ); //outside both ways, return dist from corner
}

ClassImp(StFmsFpsMaker);

StFmsFpsMaker::StFmsFpsMaker(const Char_t* name):StMaker(name),mQA(false),mFilename((char *)"fmsfps.root"),mReadMuDST(0){}

StFmsFpsMaker::~StFmsFpsMaker(){}

Int_t StFmsFpsMaker::Init(){  
  mFmsDbMaker=static_cast<StFmsDbMaker*>(GetMaker("fmsDb"));  
  if(!mFmsDbMaker){
    LOG_ERROR  << "StFmsFpsMaker::InitRun Failed to get StFmsDbMaker" << endm;
    return kStFatal;
  }
  if(!mQA) return kStOK;

  char c[100];
  mFile=new TFile(mFilename,"RECREATE");
  static const int pm[kFpsNQuad][kFpsNLayer]={{1,1,1},{1,-1,-1},{-1,1,1},{-1,-1,-1}};
  for(int cut=0; cut<NCUT; cut++){
    for(int q=0; q<kFpsNQuad; q++){
      for(int l=0; l<kFpsNLayer; l++){
	float min,max;
	if(pm[q][l]==1){min=-10.0; max=110;}
	else           {min=-110.0; max=10.0;} 
	sprintf(c,"FMSFPS_Q%1dL%1d_c%1d",q+1,l+1,cut);
	mH2[q][l][cut]=new TH2F(c,c,120,min,max,21,0.5,21.5);
	sprintf(c,"FMSFPSd_Q%1dL%1d_c%1d",q+1,l+1,cut);
	mHd2[q][l][cut]=new TH2F(c,c,100,min,max,100,-50.0,50.0);
	sprintf(c,"FMS-FPS_Q%1dL%1d_c%1d",q+1,l+1,cut);
	mHd[q][l][cut]=new TH1F(c,c,100,-50.0,50.0);
      }
    }
    sprintf(c,"NP_c%d",cut);  mNp  [cut]=new TH1F(c,c, 30,0.0,30.0);
    sprintf(c,"e_c%d",cut);   mHene[cut]=new TH1F(c,c,100,0.0,100.0);
    sprintf(c,"pt_c%d",cut);  mHpt [cut]=new TH1F(c,c,100,0.0,10.0);
    sprintf(c,"ept_c%d",cut); mHept[cut]=new TH2F(c,c,100,0.0,100.0,100,0.0,10.0);
    sprintf(c,"eta_c%d",cut); mHeta[cut]=new TH1F(c,c,100,2.5,4.3);
    sprintf(c,"phi_c%d",cut); mHphi[cut]=new TH1F(c,c,100,-3.141592654,3.141592654);
    sprintf(c,"x_c%d",cut);   mHx  [cut]=new TH1F(c,c,100,-100.0,100.0);
    sprintf(c,"y_c%d",cut);   mHy  [cut]=new TH1F(c,c,100,-100.0,100.0);
    sprintf(c,"xy_c%d",cut);  mHxy [cut]=new TH2F(c,c,100,-100.0,100.0,100,-100.0,100.0);
    sprintf(c,"m1_c%d",cut);  mHm1 [cut]=new TH1F(c,c,100,0.0,1.0);
    sprintf(c,"m2_c%d",cut);  mHm2 [cut]=new TH1F(c,c,100,0.0,4.0);
    sprintf(c,"pid_c%d",cut); mHpid[cut]=new TH1F(c,c,7,0.0,7.0);
    sprintf(c,"pid2_c%d",cut);mHpid2[cut]=new TH1F(c,c,7,0.0,7.0);
  }
  return kStOK;
}

Int_t StFmsFpsMaker::Finish(){
  if(mQA){
    mFile->Write();
    mFile->Close();
  }
  return kStOK;
}

Int_t StFmsFpsMaker::Make(){
  StEvent* event = (StEvent*)GetInputDS("StEvent");
  if(!event) {LOG_ERROR << "StFmsFpsMaker::Make did not find StEvent"<<endm; return kStErr;}
  mFmsColl = event->fmsCollection();
  if(!mFmsColl) {LOG_ERROR << "StFmsFpsMaker::Make did not find StEvent->FmsCollection"<<endm; return kStErr;}
  if(mReadMuDST) readMuDST();
  //makeHitArray();
  mFmsColl->fillFpsSlat();        //fill StFpsSlat in StFmsCollection
  corrFmsFps();                   //find FMS-FPS correlation, add fps slats info to StFmsPoint 
  mFmsColl->fillFpsAssociation(); //fill StFpsSlat with association info to FmsPoint
  pid(0);                         //find PID, add to FMS point
  print();
  if(mQA) fmsFpsAlignment();  
  return kStOk;
}

//Correlation between FMS and FPS
void StFmsFpsMaker::corrFmsFps(){
  int npoint=mFmsColl->numberOfPoints();
  StSPtrVecFmsPoint& points = mFmsColl->points(); 

  //loop over FMS points
  for(int i=0; i<npoint; i++) { 
    float x=points[i]->XYZ().x();
    float y=points[i]->XYZ().y();
    float z=points[i]->XYZ().z();
    StLorentzVectorF v1=points[i]->fourMomentum();
    points[i]->resetFps();
    //loop over FPS layers to and project 
    for(int l=1; l<=kFpsNLayer; l++){
      int nFpsFound=0;
      //loop pver FPS quad and slats
      for(int q=1; q<=kFpsNQuad; q++){	
	for(int s=1; s<=kFpsNSlat; s++) {
	  int slatid = mFmsDbMaker->fpsSlatId(q,l,s);	  
	  if(slatid<0) continue;
	  float xyz[3],width[3];
	  mFmsDbMaker->fpsPosition(slatid,xyz,width);	 
	  static const float mergine=2.0; //add a mergine to both side to get close hit
	  static const float zvertex=0.0;
	  float projx=project(x,z,xyz[2],zvertex);
	  float projy=project(y,z,xyz[2],zvertex);	
	  float d = distance(projx,projy,xyz[0],xyz[1],width[0],width[1]); 
	  if(d<mergine){
	    float mip=mFmsColl->fps(slatid)->mip();
	    unsigned short status=mFmsDbMaker->fpsStatus(slatid);
	    if(status != 0) mip=-9.0; //check FPS status from DB
	    //printf("%3d proj=%5.1f %5.1f E=%5.1f Q%1dL%1dS%02d %6.1f %6.1f %6.1f %6.1f mip=%5.1f d=%4.1f n=%d zvtx=%4.0f\n",
	    //       i,projx,projy,e,q,l,s,x1,x2,y1,y2,mip,d,nFpsFound,zvertex[j]);
	    points[i]->setFps(l,mip,slatid,d);
	    nFpsFound++;
	  } // if this hit a fps slat
	} //loop over fps slat
      } //loop over fps quad
      if(nFpsFound==0){
	// printf("%3d proj=%5.1f %5.1f E=%5.1f  L%1d not found!!!\n",i,x,y,e,l);
      }else if(nFpsFound>kFpsNCandidate){
	LOG_WARN << Form("found %d FPS slats assosicated with FmsPoint, which is more than %d!!!",nFpsFound,kFpsNCandidate) <<endm;
      }
    } //loop over fps layer
    //points[i]->orderFpsCandidates();
  }
}

void StFmsFpsMaker::pid(int opt){  //opt: 0=take closest only, 1=closest + 2nd closest, 2=closest, if mip=0 at closest, take 2nd if available
  int npoint=mFmsColl->numberOfPoints();
  StSPtrVecFmsPoint& points = mFmsColl->points();

  for(int i=0; i<npoint; i++) {
    int pid=StFmsPoint::kFpsPidNoFps;
    int i1=0,i2=0,i3=0;
    if(opt==0 || opt==2){
      i1=int(points[i]->fpsMip(1,0) + 0.5);
      i2=int(points[i]->fpsMip(2,0) + 0.5); 
      i3=int(points[i]->fpsMip(3,0) + 0.5);
    }else if(opt==1){
      i1=int(points[i]->fpsMip(1,kFpsNCandidate)+0.5);
      i2=int(points[i]->fpsMip(2,kFpsNCandidate)+0.5);
      i3=int(points[i]->fpsMip(3,kFpsNCandidate)+0.5);
    }
    if(opt==2){
      if(i1<=0) i1=int(points[i]->fpsMip(1,1) + 0.5);
      if(i2<=0) i2=int(points[i]->fpsMip(2,1) + 0.5);
      if(i3<=0) i3=int(points[i]->fpsMip(3,1) + 0.5);
    }      
    if(i1<-5 || i2<-5 || i3<-5) {
      pid=StFmsPoint::kFpsPidBad;  //bad FPS status
    }else if(i1<0 || i2<0 || i3<0) {
      pid=StFmsPoint::kFpsPidNoFps;  //no FPS coverage 
    }else{
      //total 0
      if     (i1==0 && i2==0 && i3==0) pid=StFmsPoint::kFpsPidGamma1;   // golden gamma which didn't convert in PbC
      //total 1 hits
      else if(i1>=1 && i2==0 && i3==0) pid=StFmsPoint::kFpsPidGamma3;   // gamma + accidental?
      else if(i1==0 && i2>=1 && i3==0) pid=StFmsPoint::kFpsPidGamma4;   // gamma + accidental?
      else if(i1==0 && i2==0 && i3>=1) pid=StFmsPoint::kFpsPidGamma2;   // golden gamma
      //total 2 hits
      else if(i1>=1 && i2>=1 && i3==0) pid=StFmsPoint::kFpsPidUnknown;  // ???
      else if(i1>=1 && i2==0 && i3>=1) pid=StFmsPoint::kFpsPidGamma5;   // gamma + accidental? 
      else if(i1==0 && i2>=1 && i3>=1) pid=StFmsPoint::kFpsPidGamma6;   // gamma + accidental? 
      //total 3 hits
      else if(i1>=1 && i2>=1 && i3<=4) pid=StFmsPoint::kFpsPidMip;      // MIP
      else if(i1==1 && i2==1 && i3>=5) pid=StFmsPoint::kFpsPidElectron1;// golden e+e-
      else if(i1==1 && i2>=2 && i3>=5) pid=StFmsPoint::kFpsPidElectron2;// e+e-
      else if(i1>=2 && i2==1 && i3>=5) pid=StFmsPoint::kFpsPidElectron3;// e+e-
      else if(i1>=2 && i2>=2 && i3>=5) pid=StFmsPoint::kFpsPidGamma7;   // gamma converted to e+e- pair?
      else                             LOG_WARN << Form("Leaking selection : %1d %1d %1d\n",i1,i2,i3)<<endm;
    }    
    points[i]->setFpsPid(pid);
  } // loop over FMS points
}

void StFmsFpsMaker::print(){
  int npoint=mFmsColl->numberOfPoints();
  StSPtrVecFmsPoint& points = mFmsColl->points();
  StSPtrVecFpsSlat& slats = mFmsColl->fpsSlats();
  mFmsColl->print(3);
  for(int i=0; i<npoint; i++) {
    printf("FMS %2d E=%5.1f xyz=%5.1f %5.1f %5.1f ",i,points[i]->energy(),points[i]->XYZ().x(),points[i]->XYZ().y(),points[i]->XYZ().z());
    for(int l=1; l<=kFpsNLayer; l++){
      for(int j=0; j<kFpsNCandidate; j++){
	int slatid=points[i]->fpsSlatId(l,j);
	if(slatid>=0){
	  int qq,ll,ss;
	  mFmsDbMaker->fpsQLSfromSlatId(slatid,&qq,&ll,&ss);	 	  
	  printf("Q%1dL%1dS%02d %4.1f (%4.1f) ",qq,ll,ss,points[i]->fpsMip(l,j),points[i]->fpsDistance(l,j));
	}else{
	  printf("                    ");
	}	
      }
    }
    int i1=points[i]->fpsMip(1,0)+0.5; if(i1>9) i1=9;
    int i2=points[i]->fpsMip(2,0)+0.5; if(i2>9) i2=9;
    int i3=points[i]->fpsMip(3,0)+0.5; if(i3>9) i3=9;
    int ii=i1*100+i2*10+i3;
    printf("MMM=%03d ",ii);
    int pid=points[i]->fpsPid();
    if(pid==StFmsPoint::kFpsPidBad)            printf("Bad ");
    else if(pid==StFmsPoint::kFpsPidGamma1)    printf("Ga1 ");
    else if(pid==StFmsPoint::kFpsPidGamma2)    printf("Ga2 ");
    else if(pid==StFmsPoint::kFpsPidGamma3)    printf("Ga3 ");
    else if(pid==StFmsPoint::kFpsPidGamma4)    printf("Ga4 ");
    else if(pid==StFmsPoint::kFpsPidGamma5)    printf("Ga5 ");
    else if(pid==StFmsPoint::kFpsPidGamma6)    printf("Ga6 ");
    else if(pid==StFmsPoint::kFpsPidMip)       printf("Mip ");
    else if(pid==StFmsPoint::kFpsPidElectron1) printf("El1 ");
    else if(pid==StFmsPoint::kFpsPidElectron2) printf("El2 ");
    else if(pid==StFmsPoint::kFpsPidElectron3) printf("El2 ");
    else if(pid==StFmsPoint::kFpsPidUnknown)   printf("Unk ");
    else                           printf("??? ");
    
    StLorentzVectorF v1=points[i]->fourMomentum();
    for(int j=0; j<npoint; j++) { //loop over 2nd FMS point
      if(j==i) continue;
      StLorentzVectorF v2=points[j]->fourMomentum();
      StLorentzVectorF v3=v1+v2;
      float m=v3.m();
      if(m>M1 && m<M2) printf("Pi0 m%d%d=%4.2f ",i,j,m);
    }
    printf("\n");    
  }

  for(int l=1; l<=kFpsNLayer; l++){
    printf("NMIP      FPS layer%1d ",l);
    for(int q=1; q<=kFpsNQuad; q++){
      printf(" Q%1d ",q);
      for(int s=1; s<=kFpsNSlat; s++){
	int slatid = mFmsDbMaker->fpsSlatId(q,l,s);
	float mip=slats[slatid]->mip();
	int n=int(mip+0.5);
	if(n>9) n=9;
	if(slatid>=0){printf("%1d",n);}
	else         {printf("  ");}
      }      
    }
    printf("\n");
    printf("NFmsPoint FPS layer%1d ",l);
    for(int q=1; q<=kFpsNQuad; q++){
      printf(" Q%1d ",q);
      for(int s=1; s<=kFpsNSlat; s++){
	int slatid = mFmsDbMaker->fpsSlatId(q,l,s);
	int n=slats[slatid]->nPoint(0);
	if(n>9) n=9;
	if(slatid>=0){printf("%1d",n);}
	else         {printf(" ");}
      }      
    }
    printf("\n");
  }
}

//QA for Correlation between FMS and FPS
void StFmsFpsMaker::fmsFpsAlignment(){
  //if(!fp) {LOG_INFO<<"StFmsFpsMaker::corrFmsFps No FmsPoint found"<<endm; return;}
  int cut[NCUT], n[NCUT], npoint[kFpsNQuad], nfps[kFpsNQuad][kFpsNLayer];
  static const int MAXNP=100;
  float mass[MAXNP][MAXNP];
  int pids[MAXNP][MAXNP];
  memset(cut,0,sizeof(cut));
  memset(n,0,sizeof(n));
  memset(npoint,0,sizeof(npoint));
  memset(nfps,0,sizeof(nfps));
  memset(mass,0,sizeof(mass));
  memset(pids,0,sizeof(pids));
  
  StSPtrVecFmsPoint& points = mFmsColl->points(); 
  StSPtrVecFpsSlat& slats = mFmsColl->fpsSlats(); 
  int np=mFmsColl->numberOfPoints();
  int ns=slats.size();
  if(np>MAXNP) {
    LOG_INFO<<"StFmsFpsMaker::corrFmsFps too many FmsPoint found "<<np<<" > "<<MAXNP<< endm; 
    return;
  }

  //Get # of FMS point per quad 
  for(int i=0; i<np; i++) {
    float x=points[i]->XYZ().x();
    float y=points[i]->XYZ().y();
    int q=0;
    if     (x>=0.0 && y>=0.0) {q=1;}
    else if(x>=0.0 && y< 0.0) {q=2;}
    else if(x< 0.0 && y>=0.0) {q=3;}
    else if(x< 0.0 && y< 0.0) {q=4;}
    npoint[q-1]++;
  }
  
  //Get # of FPS slats hitted per quad/layer
  for(int i=0; i<ns; i++) {
    if(slats[i]->mip()>0.5){
      int slatid=slats[i]->slatId();
      int q,l,s;
      mFmsDbMaker->fpsQLSfromSlatId(slatid,&q,&l,&s);
      nfps[q-1][l-1]++;
    }
  }
  
  //Loop over FMS points to set up cuts
  cut[0]=1; //no cut
  for(int i=0; i<np; i++) {
    float e=points[i]->energy();
    float x=points[i]->XYZ().x();
    float y=points[i]->XYZ().y();
    float z=points[i]->XYZ().z();
    int q=0;
    if     (x>=0.0 && y>=0.0) {q=1;}
    else if(x>=0.0 && y< 0.0) {q=2;}
    else if(x< 0.0 && y>=0.0) {q=3;}
    else if(x< 0.0 && y< 0.0) {q=4;}
      /*printf("ID=%3d Det=%2d E=%6.2f Type=%d x=%6.1f y=%6.1f Quad=%1d\n",
	points[i]->id(),points[i]->detectorId(),points[i]->energy(),points[i]->nParentClusterPhotons(),
	x,y,q);*/           
    if(q==0) {LOG_INFO<<"StFmsFpsMaker::corrFmsFps found bad fmsPoint"<<endm; continue;}
    StLorentzVectorF v1=points[i]->fourMomentum();

    if(npoint[q-1]<=FMSNHIT || nfps[q-1][0]<=FPSNHIT ||  nfps[q-1][1]<=FPSNHIT ||  nfps[q-1][2]<=FPSNHIT){
      cut[1]=1;
      if(e<E1) {cut[2]=1;}
      else     {cut[3]=1;}      
    }
    int he=0, pi0=0;      
    for(int j=0; j<np; j++) { //loop over 2nd FMS point
      if(j==i) continue;
      float e2=points[j]->energy();
      if(e2>e) continue;
      StLorentzVectorF v2=points[j]->fourMomentum();
      StLorentzVectorF v3=v1+v2;
      float m=v3.m();
      mass[i][j]=m;
      if(e2>E2){
	he=1;	
	if(m>M1 && m<M2) pi0=1;
	int pid1=0, pid2=0;
	int p1=points[i]->fpsPid();
	int p2=points[j]->fpsPid();
	int cc=0;
	pid1=p1/10;
	pid2=p2/10;
	if     (pid1==1 && pid2==1) cc=0;                           //gg
	else if(pid1==2 && pid2==2) cc=1;                           //hh
	else if(pid1==3 && pid2==3) cc=2;                           //ee
	else if((pid1==1 && pid2==2) || (pid1==2 && pid2==1)) cc=3; //gh
	else if((pid1==1 && pid2==3) || (pid1==3 && pid2==1)) cc=4; //ge
	else if((pid1==2 && pid2==3) || (pid1==3 && pid2==2)) cc=5; //eh
	else cc=6;                                                  //others
	cut[cc+6]=1;
	pids[i][j]=cc;
      }
    }
    if(cut[1]==1 && cut[3]==1){
      if(he==1)  {cut[4]=1;}
      if(pi0==1) {cut[5]=1;}
    }
    
    //finished all cuts... counting and filling
    for(int c=0; c<NCUT; c++){      
      if(cut[c]==0) continue;
      n[c]++;  
      mHene[c]->Fill(e);        
      mHpt[c]->Fill(v1.perp());
      mHept[c]->Fill(e,v1.perp());
      mHeta[c]->Fill(v1.pseudoRapidity());    
      mHphi[c]->Fill(v1.phi());
      mHx[c]->Fill(x);
      mHy[c]->Fill(y);
      mHxy[c]->Fill(x,y);
      mHpid[c]->Fill(float(points[i]->fpsPid()));
    }
    for(int j=0; j<np; j++) { //loop over 2nd FMS point
      if(j==i) continue;
      float m=mass[i][j];
      if(m==0.0) continue;
      float e2=points[j]->energy();
      int pp=pids[i][j];
      for(int c=0; c<NCUT; c++){
	if(cut[c]==0) continue;
	if(c==4 && e2<E2) continue;
	if(c==5 && (e2<E2 || m<M1 || m>M2)) continue;
	if(c==6 && (e2<E2 || pp!=0)) continue;
	if(c==7 && (e2<E2 || pp!=1)) continue;
	if(c==8 && (e2<E2 || pp!=2)) continue;
	if(c==9 && (e2<E2 || pp!=3)) continue;
	if(c==10&& (e2<E2 || pp!=4)) continue;
	if(c==11&& (e2<E2 || pp!=5)) continue;
	if(c==12&& (e2<E2 || pp!=6)) continue;
        mHm1[c]->Fill(m);
	mHm2[c]->Fill(m);
	mHpid2[c]->Fill(float(pp));
      }
    }
    //Now FPS
    for(int l=1; l<=kFpsNLayer; l++) {
      for(int s=1; s<=kFpsNSlat; s++) {
	float xyz[3],width[3];	  
	mFmsDbMaker->fpsPosition(q,l,s,xyz,width);
	//printf("Q%1dL%1dS%02d hit=%6.3f xyz=%6.2f %6.2f %6.2f\n",q,l,s,mHit[q-1][l-1][s-1],xyz[0],xyz[1],xyz[2]);
	int slatid = mFmsDbMaker->fpsSlatId(q,l,s);
	if(slats[slatid]->mip()>0.5){
	  for(int c=0; c<NCUT; c++){
	    if(cut[c]==0) continue;	    
	    static const float zvertex=0.0;
	    if(l==1){ 
	      //inline float project(float x, float z, float zp, float vz){
	      float xx=project(x,z,xyz[2],zvertex);
	      mH2[q-1][l-1][c]->Fill(xx,float(s));
	      mHd[q-1][l-1][c]->Fill(xx-xyz[0]);
	      mHd2[q-1][l-1][c]->Fill(xx,xx-xyz[0]);
	    }else{ 
	      float yy=project(y,z,xyz[2],zvertex);
	      mH2[q-1][l-1][c]->Fill(yy,float(s)); 
	      mHd[q-1][l-1][c]->Fill(yy-xyz[1]);
	      mHd2[q-1][l-1][c]->Fill(yy,y-xyz[1]);
	    }
	  }  //loop over cuts
	}  //if FPS has mip
      }  //loop slat
    }  //loop layer
  }  //loop over FMS point
  for(int c=0; c<NCUT; c++){
    mNp[c]->Fill(float(n[c]));
  }
}

//Read MuDST if available, and update FPS hits in StEvent using current DB values
void StFmsFpsMaker::readMuDST(){  
  int nh=mFmsColl->numberOfHits();
  StSPtrVecFmsHit& hits = mFmsColl->hits();
  int nfpshit=0;
  for(int j=0; j<nh; j++){ //count fps hits in StEvent->StFmsCollection->Hit
    if(hits[j]->detectorId()==kFpsDetId) nfpshit++; 
  }
  StMuDst* mudst = (StMuDst*)GetInputDS("MuDst");
  if(!mudst){ LOG_INFO<<"StFmsFpsMaker::readMuDST found no Mudst"<<endm; return;}
  StMuFmsCollection* muFmsColl = StMuDst::muFmsCollection();
  if(!muFmsColl){ LOG_INFO<<"StFmsFpsMaker::readMuDST found no StMuFmsCollection in MuDST"<<endm; return;}
  int nfps=0;
  int nhits = muFmsColl->numberOfHits();  
  for(int i=0; i<nhits; i++){
    StMuFmsHit* h = muFmsColl->getHit(i);
    if(h->detectorId()==kFpsDetId){ //only updating FPS hits... StFmsHitMaker deal with the rest
      int flag=0;
      int ch=h->channel();	  
      float gain=mFmsDbMaker->fpsGain(ch);
      float nmip=h->adc()/gain;
      //printf("ch=%3d adc=%4d gain=%6.2f nmip=%6.2f\n",ch,h->adc(),gain,nmip);
      if(nfpshit>0){ //only if there were FPS hits in StEnvent...
	for(int j=0; j<nh; j++){ //loop over fmsHits in StEvent and updating energy with new calibration
	  if(hits[j]->detectorId()==kFpsDetId && hits[j]->channel()==ch){
	    if(h->adc()!= hits[j]->adc()) {
	      LOG_ERROR << "StFmsFpsMaker::readMuDst Found inconsistent FPS hit" <<endm;
	      h->print();
	      hits[j]->print();
	      break;
	    }
	    //LOG_INFO<<"StFmsFpsMaker::readMuDST found matching hits in StEvent->StFmsCollection, updating energy with new DB value "<<endm;
	    hits[j]->setEnergy(nmip);
	    flag=1;
	    break;
	  }
	}
      }
      if(flag==0){ //found no correspinding hit in StEvent->FmsCollection, so adding it
	StFmsHit* hit = new StFmsHit(h->detectorId(),h->channel(),
				     h->qtCrate(),h->qtSlot(),h->qtChannel(),
				     h->adc(), h->tdc(), nmip);
	mFmsColl->addHit(hit);
	//LOG_INFO<<"StFmsFpsMaker::readMuDST did not find matching hits in StEvent->StFmsCollection. Creating and adding"<<endm;
      }
      nfps++;
    }
  }
  LOG_INFO<<"StFmsFpsMaker::readMuDST Found "<<nhits<<" FMS hits in MuDst, updated "<<nfps<<" FPS hits"<<endm;
}

