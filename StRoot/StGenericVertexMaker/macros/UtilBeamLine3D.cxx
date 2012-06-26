#include <stdio.h>
#include <math.h>
#include <assert.h>
#include <TObjArray.h>
#include <TH1.h>
#include <TH2.h>
#include <TLine.h>
#include <TList.h>

#include "UtilBeamLine3D.h"

//========================
//========================
UtilBeamLine3D::UtilBeamLine3D() {
  track.clear(); // just on case;
  fcnCount=0;
  // input tracks
  cut_maxRxy=1.5;
  cut_maxZ=100; 
  cut_minChi2=0.4;
  cut_maxChi2=2.;
  cut_minP=1.2; // GeV/c 
  // minimization:
  cut_Dmax=1.0; // likelhood cut-off  
  par_filter=0; // 0=all, 1: West-only = +Z & +eta
  printf("Params of UtilBeamLine3D\n  INPUT tracks: maxRxy=%.1fcm, maxZ=%.1fcm, Chi2=[%.1f,%.1f]  minP=%.1f GeVC/c \n  Minimization: Dmax=%.1fcm, WestOnly=%d \n\n",
	 cut_maxRxy,cut_maxZ,cut_minChi2, cut_maxChi2,cut_minP,cut_Dmax,par_filter);
}

//========================
//========================
void 
UtilBeamLine3D:: initHisto( TObjArray * HList){
  TList *L=0;  TLine *ln=0; float yMax=1e6; TH1* h=0;

  memset(hA,0,sizeof(hA));
  printf("helper:initHist\n");

  hA[0]=h=new TH1F("trStat","statistics for tracks",20,0.5,20.5);
  h->GetXaxis()->SetTitleOffset(0.4);  h->GetXaxis()->SetLabelSize(0.06);  h->GetXaxis()->SetTitleSize(0.05); h->SetMinimum(0.8);
  h->SetLineColor(kBlue);h->SetLineWidth(2);
  h->SetMarkerSize(2);//<-- large text

  hA[1]=h=new TH1F("trCh2","input track chi2; chi2/dof",100,0,10);
  ln=new TLine(cut_minChi2,0,cut_minChi2,yMax);
  ln->SetLineColor(kRed); ln->SetLineStyle(2);
  L= h->GetListOfFunctions();  L->Add(ln);
  ln=new TLine(cut_maxChi2,0,cut_maxChi2,yMax);
  ln->SetLineColor(kRed); ln->SetLineStyle(2);
  L= h->GetListOfFunctions();  L->Add(ln);
 
  hA[2]=h=new TH1F("trZ","input track Z @DCA; Z(cm)",50,-200,200);
  ln=new TLine(cut_maxZ,0,cut_maxZ,yMax);
  ln->SetLineColor(kRed); ln->SetLineStyle(2);
  L= h->GetListOfFunctions();  L->Add(ln);
  ln=new TLine(-cut_maxZ,0,-cut_maxZ,yMax);
  ln->SetLineColor(kRed); ln->SetLineStyle(2);
  L= h->GetListOfFunctions();  L->Add(ln);

  hA[3]=h=new TH1F("trX","input track X @DCA; X(cm)",100,-4,4);
  ln=new TLine(cut_maxRxy,0,cut_maxRxy,yMax);
  ln->SetLineColor(kRed); ln->SetLineStyle(2);
  L= h->GetListOfFunctions();  L->Add(ln);
  ln=new TLine(-cut_maxRxy,0,-cut_maxRxy,yMax);
  ln->SetLineColor(kRed); ln->SetLineStyle(2);
  L= h->GetListOfFunctions();  L->Add(ln);

  hA[4]=h=new TH1F("trY","input track Y @DCA; Y(cm)",100,-4,4);  
  ln=new TLine(cut_maxRxy,0,cut_maxRxy,yMax);
  ln->SetLineColor(kRed); ln->SetLineStyle(2);
  L= h->GetListOfFunctions();  L->Add(ln);
  ln=new TLine(-cut_maxRxy,0,-cut_maxRxy,yMax);
  ln->SetLineColor(kRed); ln->SetLineStyle(2);
  L= h->GetListOfFunctions();  L->Add(ln);

  hA[5]=h=new TH1F("trP","input track momentum P @DCA; P (GeV/c)",100,0,10);
  ln=new TLine(cut_minP,0,cut_minP,yMax);
  ln->SetLineColor(kRed); ln->SetLineStyle(2);
  L= h->GetListOfFunctions();  L->Add(ln);

  hA[6]=new TH1F("trPt","input track momentum PT @DCA; PT (GeV/c)",100,0,10);
  //free 7..12

  int nB=30;
  hA[13]=h= new TH2D("fcnChiXY","ln(3D likelihood), STAR X-Y plane;  X-axis (cm); Y-axis(cm)",nB,-.3,.3,nB,-.3,.3);
  ln=new TLine(-10,0,10,0);  ln->SetLineColor(kMagenta); 
  L= h->GetListOfFunctions();  L->Add(ln);
  ln=new TLine(0,-10,0,10);  ln->SetLineColor(kMagenta); 
  L= h->GetListOfFunctions();  L->Add(ln);
 
  hA[14]=h= new TH2D("fcnChiXnX","ln(3D likelihood), STAR X-n_{X} plane;  X-axis (cm); slope X ",nB,0,1,nB,-2e-3,2e-3);
  ln=new TLine(0,-10,0,10);  ln->SetLineColor(kMagenta); 
  L= h->GetListOfFunctions();  L->Add(ln);
  ln=new TLine(-10,0,10,0);  ln->SetLineColor(kMagenta); 
  L= h->GetListOfFunctions();  L->Add(ln);

  hA[15]=h= new TH2D("fcnChiYnY","ln(3D likelihood),  STAR Y-n_{Y} plane;  Y-axis (cm); slope Y ",nB,0,1,nB,-2e-3,2e-3);
  ln=new TLine(0,-10,0,10);  ln->SetLineColor(kMagenta); 
  L= h->GetListOfFunctions();  L->Add(ln);
  ln=new TLine(-10,0,10,0);  ln->SetLineColor(kMagenta); 
  L= h->GetListOfFunctions();  L->Add(ln);
  // free 16..18

  hA[19]=h=new TH1F("bmSol","Beam line params (3D fit); value",4,1,1);
  h->SetMarkerStyle(24);
  float mY=0.3;
  h->SetMaximum(mY);  h->SetMinimum(-mY);
  h->GetXaxis()->SetTitleOffset(0.4);  h->GetXaxis()->SetLabelSize(0.06);  h->GetXaxis()->SetTitleSize(0.05);
  h->SetLineColor(kBlue);h->SetLineWidth(2);
  // h->SetMarkerSize(2);//<-- large text

  ln=new TLine(0,0,6,0);    L= h->GetListOfFunctions();  L->Add(ln);

  //20-30: reserved for chi2 monitoring
  hA[20]=new TH1F("fcnDet","FCN: determinant from DCA solution",200,0,1.01);
  hA[21]=h=new TH1F("fcnDca","FCN: 3D dca length ...; DCA (cm)",100,0,5.);
  ln=new TLine(cut_Dmax,0,cut_Dmax,yMax);
  ln->SetLineColor(kRed); ln->SetLineStyle(2);
  L= h->GetListOfFunctions();  L->Add(ln);

  hA[22]=h=new TH2F("fcnDcaZ","FCN: 3D dca length; Z (cm);DCA (cm)",50,-200,200.,50,0,5.);
  ln=new TLine(-200,cut_Dmax,200,cut_Dmax);
  ln->SetLineColor(kRed); ln->SetLineStyle(2);
  L= h->GetListOfFunctions();  L->Add(ln);
  hA[23]=h=new TH1F("fcnNtr","FCN: # of tracks with 3D.DCA<Dmax ; Z (cm)",50,-200,200.);
  h->SetLineColor(kBlue);   h->SetFillColor(kBlue); 

  for(int i=0;i<mxH; i++) if(hA[i]) HList->Add(hA[i]);
  printf("track QA params:  maxRxy<%.1f cm, maxZ<%.1f cm, chi2/dof=[%.1f, %.1f],  Dmax=%.2f cm track filter=%d\n", cut_maxRxy, cut_maxZ, cut_minChi2,cut_maxChi2,cut_Dmax,par_filter);

}


//========================
//========================
int 
UtilBeamLine3D::qaTracks(){
  printf("qaTRacks start=%d\n",(int)track.size());
  int nOK=0;
  vector<TrackStump>::iterator it;
  for (it=track.begin() ; it < track.end(); it++ ) {
    TrackStump *t= &(*it);
    hA[0]->Fill(1);
    
    hA[1]->Fill(t->chi2);
    if( t->chi2<cut_minChi2 ||  t->chi2>cut_maxChi2)   continue;
    hA[0]->Fill(2);
    
    hA[2]->Fill(t->r.z());
    if(fabs(t->r.z()) >cut_maxZ)  continue;
    hA[0]->Fill(3);

    hA[3]->Fill(t->r.x());
    if(fabs(t->r.x()) >cut_maxRxy) continue;
    hA[0]->Fill(4);

    hA[4]->Fill(t->r.y());
    if(fabs(t->r.y()) >cut_maxRxy) continue;
    hA[0]->Fill(5);
    
    hA[5]->Fill(t->P);
    if(t->P<cut_minP) continue;
    hA[0]->Fill(6);

    if(par_filter==1) {
      if(t->r.z()< 0 || t->p.z() <0)  continue;
    }
    hA[0]->Fill(7);

    
    //..... track has been accepted ....
    t->bad=0;
    nOK++;
    hA[6]->Fill(t->Pt);
  }
  printf("qaTRacks end=%d\n",nOK);
  return nOK;
}


//========================
//========================
void 
UtilBeamLine3D::print(int k) {
  printf("UtilBeamLine3D::print(%d) size=%d\n",k,(int)track.size());
  for(int i=0;i<(int)track.size();i++) {
    track[i].print();
    if(i>=k) break;
  }
}

//========================
//========================
void 
UtilBeamLine3D::scanChi2(double *par, int mode){

  // mode:  //0=x-y, 1=x-nx , 2= y-ny
  printf("scan chi2, mode=%d ....\n",mode);   
  // working variables for likelihod fcn
  int npar,iflag=0;
  double *grad=0;
  double fcnval;
  double wrkPar[mxPar];

  // 2D scan in X-Y plane
  // copy fixed Ux,Uy

  TH2F *h2=0;
  if(mode==0) h2=(TH2F*)hA[13];
  if(mode==1) h2=(TH2F*)hA[14];
  if(mode==2) h2=(TH2F*)hA[15];
  TAxis *xax=h2->GetXaxis();
  TAxis *yax=h2->GetYaxis();
  
  if(mode==0) { // adjust only if too much displaced
    if(fabs(par[0])>0.2 ||fabs(par[1])>0.2 ){
      xax->Set(xax->GetNbins(),par[0]-0.3,par[0]+0.3); 
      yax->Set(yax->GetNbins(),par[1]-0.3,par[1]+0.3); 
    }
  }
  if(mode>0) { // always adjust
    xax->Set(xax->GetNbins(),par[0]-0.3,par[0]+0.3); 
    yax->Set(yax->GetNbins(),par[2]-9e-3,par[2]+9e-3);  
  }

  // fill chi2 values
  for(int bx=1;bx<=xax->GetNbins();bx++){
    double x=xax->GetBinCenter(bx);
    for(int by=1;by<=yax->GetNbins();by++){
      double y=yax->GetBinCenter(by);
      if(mode==0) { // X-Y plane
	wrkPar[0]=x;	   wrkPar[1]=y;
	wrkPar[2]=par[2];  wrkPar[3]=par[3];
      }
      if(mode==1) { // X-nX
	wrkPar[0]=x;	  wrkPar[1]=par[1];
	wrkPar[2]=y;      wrkPar[3]=par[3];
      }

      if(mode==2) { // X-nX
	wrkPar[0]=par[0];      wrkPar[1]=x;
	wrkPar[2]=par[2];      wrkPar[3]=y;
      }

      beamLineLike3D(npar,grad,fcnval, wrkPar,iflag);
      //printf("  %d %f %f  fcn=%.1f\n",by,x,y,fcnval);
      h2->Fill(x,y,fcnval);
    }
  }

}

//========================
void 
UtilBeamLine3D::evalSolution(double *par){
  // working variables for likelihod fcn
  int npar,iflag=0;
  double *grad=0;
  double fcnval;
  //  double wrkPar[mxPar];
  fcnMon1=1; // activate chi2 monitor for the 1st pass
  beamLineLike3D(npar,grad,fcnval, par,iflag);
}


//========================
//========================
void 
UtilBeamLine3D::readTracks(const TString fnameT){
  const char *fname=fnameT.Data();
  printf("Read fname=%s=\n",fname);
  FILE *fd=fopen(fname,"r"); assert(fd);
  char text[1000];
  TrackStump t;
  float x,y,z,px,py,pz,ery,eryz,erz;
  track.clear();
  int mxSkip=2; // max # consecutive of lines allowed to be wrong
  int nSkip=mxSkip;
  char buf[3000];
  while(1) { 
    char * retc=fgets(buf,3000,fd);    
    if(retc==0) break;    
    //if(buf[0]=='#') continue;    
    //printf("=%s=\n",buf);    
    int ret=sscanf(buf ,"%s %f %f %f   %f %f %f   %f %f %f   %d %f  %f %d,",text,&x,&y,&z,&px,&py,&pz,&ery,&eryz,&erz,&t.nFitP,&t.chi2,&t.z0,&t.eveId);
    //printf("%d %s %f %f %f   %f %f %f   %f %f %f   %d %f  %.1f %d \n",ret,text,x,y,z,px,py,pz,t.cyy,t.czy,t.czz,t.nFitP,t.chi2,t.z0,t.eveId);
    if(ret!=14) {
      printf("bad ret=%d  nSkip=%d  size=%d\n",ret,nSkip,(int)track.size());
      nSkip--;
      if(nSkip) continue; // allow this line as bad ;
      else break; // do not read from input any more
    }
    nSkip=mxSkip;
    // add gentle slope in x & y
    //x=0.0002*z; y=-0.0003*z;
    t.r.SetXYZ(x,y,z);
    t.p.SetXYZ(px,py,pz);
    t.P=t.p.Mag();
    t.Pt=t.p.Pt();
    t.p=t.p.Unit();
    t.ery2=ery*ery;
    t.eryz=eryz;
    t.erz2=erz*erz;
    t.bad=1; // assume all bad, track QA will decide
    track.push_back(t);
    //if(track.size()>19) break;
  }
  printf("found %d tracks\n",(int)track.size());
  fclose(fd);
}
