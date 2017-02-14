#include <string.h>
#include <assert.h>
#include <stdio.h>
#include <StMessMgr.h>

#include <TH1.h>
#include <TH2.h>
#include <TObjArray.h>
#include <TList.h>
#include <TLine.h>
#include <TArrow.h>
#include <TEllipse.h>

#include <StEvent/StGlobalTrack.h>

#include <StBFChain/StBFChain.h> // for geant vertex
#include <tables/St_g2t_vertex_Table.h> //for geant vertex

#include "Vertex3D.h"

namespace StEvPPV {

//==========================================================
//==========================================================
Vertex3D::Vertex3D() {
}

//==========================================================
//==========================================================
void
Vertex3D::initRun(){
  LOG_INFO <<Form("Vertex3D::initRun() params:track glob %.1f<PT(GeV/c)<%.1f, sigY<%.1fcm, nTrack>%d ", cut_pT1, cut_pT2, cut_sigY, cut_numTrack)<<endm;
 }

//==========================================================
//==========================================================
Vertex3D::~Vertex3D(){
}

//==========================================================
//==========================================================
void
Vertex3D::clearEvent(){
  hA[0]->Fill(1);
  isFound=false;
  track.clear();
  //  printf("ver3DclearEvt\n");
}


//==========================================================
//==========================================================
void
Vertex3D::clearTracks(){
  hA[0]->Fill(2);
  track.clear();
  // printf("ver3DclearTrk\n");
}


//==========================================================
//==========================================================
void Vertex3D::addTrack(TrackData* trk)
{
  if(isValid()) return; // block adding new track after 1st vertex is found
  if(  !trk->mTpc && !trk->mBtof && !trk->mCtb && !trk->mBemc && trk->mEemc ) return;
  hA[0]->Fill(3);
  // track is matched to any fast detector

  float pt=trk->dcaTrack.gP.Pt();
  hA[2]->Fill(pt);
  if (pt <cut_pT1) return;
  hA[0]->Fill(4);
  if (pt >cut_pT2) return;
  hA[0]->Fill(5);

  float sY=trk->dcaTrack.sigYloc; //track extrapolation error in transverse direction
  hA[3]->Fill(sY);
  if(sY>cut_sigY) return;
  hA[0]->Fill(6);

  hA[4]->Fill(trk->dcaTrack.sigZ); 
  track.push_back(trk); 
}

//==========================================================
//==========================================================
void Vertex3D::doExtrapolation()
{ // study track cov matrix for individual tracks using my macro plTrCov.C 
#if 0 // Looks like this function is doing nothing
  for(unsigned int i=0;i<track.size();i++) {
     const StiKalmanTrack* tr=track[i]->mother;
     hA[5]->Fill(1);
     if( fabs(tr->getChi2()-1.)>0.8) continue;
     hA[5]->Fill(2);
     if( tr->getFitPointCount()<20) continue;
     hA[5]->Fill(3);
     if( fabs(tr->getPseudoRapidity())>0.5) continue;
     hA[5]->Fill(4);
     if( fabs(tr->getPt())<1.) continue;
     hA[5]->Fill(5);
      
     printf("TTT global phi/rad=%.3f PT=%.2f curv=%f Rxy=%.1f nFitP=%d eta=%.2f  chi2/dof=%.1f\n",tr->getPhi(),tr->getPt(),tr->getCurvature(),tr->getPoint().perp(),tr->getFitPointCount(),tr->getPseudoRapidity(),tr->getChi2());
     printf("T: track position along nodes, N=%d\n",tr->getFitPointCount());
     StiKTNIterator tNode = tr->rbegin();
     StiKTNIterator eNode = tr->rend();
     int iN=0;
     float minLx=9999, maxLx=0.; // local coordinates of track
     for (;tNode!=eNode;++tNode){
       StiKalmanTrackNode *node = &(*tNode);
       if(!node->isValid())      continue;
     
       StiHit *stiHit = node->getHit();
       if (!stiHit)              continue;
	 
       if (node->getChi2()>1000) continue;
       if (!node->isFitted())    continue;
       iN++;
       float Lx=node->x();
       float Ly=node->y();
       float Lz=node->z();
       float sLy=sqrt(node->fitErrs()._cYY);
       float sLz=sqrt(node->fitErrs()._cZZ);
       float Rxy=sqrt(Lx*Lx+Ly*Ly);
       if(minLx>Lx) minLx=Lx;
       if(maxLx<Lx) maxLx=Lx;
       printf("iN=%d Lx=%.2f  Ly=%.2f %.2f   Lz=%.2f %.2f  Rxy=%.1f  gX=%.2f gY=%.2f \n",iN,Lx,Ly,sLy,Lz,sLz,Rxy,node->x_g(),node->y_g());
       	 printf("TTm %.2f    %.2f %.3f   %.2f %.3f  %f\n",node->x_g(),node->y_g(),sLy,node->z_g(),sLz,node->getAlpha());

     } // end of nodes w/ hits
 
     for(int iDir=0;iDir<2;iDir++) {
       if(iDir==0)  printf("extrapolate track toward vertex, innLx=%.1f\n",minLx);
       if(iDir==1)  printf("extrapolate track toward BSMD, outLx=%.1f\n",maxLx);       
       for(int iI=0;iI<17;iI++) {
	 float Lx=100.;StiKalmanTrackNode* node=0; int ret=-2;
	 StiKalmanTrack track2=*tr; // clone track to allow extrapolation
	 if(iDir==0) {// toward vertex
	   Lx=minLx-iI*5;
	   node=track2.getInnerMostNode();
	   ret=node->propagate(Lx,kCylindrical,kOutsideIn);
	 } else {
	   Lx=maxLx+iI*2; if(Lx>250) break;
	   node=track2.getOuterMostNode();
	   ret=node->propagate(Lx,kCylindrical,kInsideOut);
	 }
	 if(iI==0)cout<<"TT track extrap node:"<< *node;
	 if(ret<0) {printf("prop in x=%f ret=%d, skip\n",Lx,ret); continue;}
	 float Ly=node->y();
	 float Lz=node->z();
	 if(fabs(Ly)>40.) break; // to not cross sector boundary ??
	 if(iDir==1 && fabs(Ly)>36.) break; // to not cross sector boundary ??
	 node->propagateError();
	 float sLy=sqrt(node->fitErrs()._cYY);
	 float sLz=sqrt(node->fitErrs()._cZZ);
	 printf("%diN=%d Lx=%.2f  Ly=%.2f %.2f   Lz=%.2f %.2f gX=%.2f gY=%.2f\n",iDir,iI,Lx,Ly,sLy,Lz,sLz,node->x_g(),node->y_g());
	 printf("TT%d %.2f   %.2f %.3f   %.2f %.3f  %f\n",iDir,node->x_g(),node->y_g(),sLy,node->z_g(),sLz,node->getAlpha());
       }
     }
     
  }  
#endif
}
//head -n 4224 Lda83-618 | tail -n 150 | grep TT

//==========================================================
//==========================================================
void
Vertex3D::study(TVector3 r, int eveID){
  float Z0=r.z();
  
  hA[0]->Fill(10);
  hA[1]->Fill(track.size());
  dumpPrimTracks4beamLine(Z0, eveID);
  trackChi2QA(Z0);
  // doExtrapolation(); /* for cov-matrix study, activate by hand */
  if (track.size()<cut_numTrack)  return;
  hA[0]->Fill(11);

  // fill event plots .........
  if(nHE<mxHE) {
    TList *Lyx= hYX[nHE]->GetListOfFunctions(); 
    TList *Lyz= hYZ[nHE]->GetListOfFunctions(); 
    TArrow *ar=0;
    //printf("Z0=%f, eveID=%d\n",Z0,eveID);
    float D=2.5; // cm half length of tracklet
    float mxPt=0;
    for(unsigned int i=0;i<track.size();i++) {
      DcaTrack  tr=track[i]->dcaTrack;
      float sig=tr.sigYloc;
      float x=tr.R.x();
      float y=tr.R.y();
      float z=tr.R.z();
      float px=tr.gP.x();
      float py=tr.gP.y();
      float pz=tr.gP.z();
      float pt=tr.gP.Pt();
      float pyz=sqrt(py*py+pz*pz);
      float ux = D*px/pt; //formula uses unit vector in Y_Z plane
      float uy = D*py/pt;
      float vz = D*pz/pyz;//... in Y-Z plane
      float vy = D*py/pyz;
      float head=pt/100.;
      int width=int(3.* (0.2*0.2)/sig/sig);
      if(mxPt<pt) mxPt=pt;
      // printf("tr PT=%f  x=%f dx=%f y=%f dy=%f z=%f vz=%f head=%f width=%d sig=%f\n",pt, x,ux,y,uy,z,vz,head, width,sig);

      ar=new TArrow(x-ux,y-uy,x+ux,y+uy,head,"|>");  
      ar->SetLineColor(kBlue); ar->SetLineWidth(width); Lyx->Add(ar);

      ar=new TArrow(z-vz,y-vy,z+vz,y+vy,head,"|>"); 
      ar->SetLineColor(kMagenta);ar->SetLineWidth(width); Lyz->Add(ar);
    }

    // draw reco vertex 3D , dashed
    TEllipse *el=0;
    float Rel=0.3;
    el=new TEllipse(r.x(),r.y(),Rel,Rel); el->SetLineColor(kRed);el->SetLineStyle(2);
    Lyx->Add(el);
    el=new TEllipse(r.z(),r.y(),Rel,Rel); el->SetLineColor(kBlack);el->SetLineStyle(2);
    Lyz->Add(el);
    
    // try to add geant vertex to the plot
    St_DataSet *gds=StMaker::GetChain()->GetDataSet("geant");
    if(gds) {
      St_g2t_vertex  *g2t_ver=( St_g2t_vertex *)gds->Find("g2t_vertex");
      if( g2t_ver) {
	g2t_vertex_st *GVER= g2t_ver->GetTable();
	float *GV=GVER->ge_x;
	printf("#GGVER  x=%.1f y=%.1f z=%.1f ",GV[0],GV[1],GV[2]); 
	TEllipse *el=0;
	float Rel=0.25;
	el=new TEllipse(GV[0],GV[1],Rel,Rel); el->SetLineColor(kRed);
	Lyx->Add(el);
	
	el=new TEllipse(GV[2],GV[1],Rel,Rel); el->SetLineColor(kBlack);
	Lyz->Add(el);
      }
    }
    
    // change title of Y_X
    char tt[1000]; sprintf(tt,"%s, mxPt=%.1f GeV/c",hYX[nHE]->GetTitle(), mxPt);
    hYX[nHE]->SetTitle(tt);
    
    // change title of Y_Z
    sprintf(tt,"%s, eveID=%d",hYZ[nHE]->GetTitle(), eveID);
    hYZ[nHE]->SetTitle(tt);

    // change Z_range for Y_Z
    hYZ[nHE]->GetXaxis()->Set(2,Z0-3,Z0+3);
    nHE++;
  }
}


//==========================================================
void Vertex3D::dumpPrimTracks4beamLine(float z0, int eveID) 
{
if (z0 || eveID) {}
#if 0
  for(unsigned int i=0;i<track.size();i++) {
      DcaTrack  tr=track[i]->dcaTrack;
      StiNodeErrs *er=&(tr.fitErr);
      float x=tr.R.x();
      float y=tr.R.y();
      float z=tr.R.z();
      float px=tr.gP.x();
      float py=tr.gP.y();
      float pz=tr.gP.z();
      printf("track4beamLine %f %f %f   %f %f %f   %f %f %f   %d %f  %.1f %d \n",x,y,z,px,py,pz,er->_cYY,er->_cZY,er->_cZZ , tr.nFitPoint,tr.gChi2,z0,eveID);
  }
#endif
}


//==========================================================
void
Vertex3D::trackChi2QA(float z0) {
  for(unsigned int i=0;i<track.size();i++) {
      DcaTrack  tr=track[i]->dcaTrack;
       float chi2dof=tr.gChi2;
      hA[10]->Fill(chi2dof);
      hA[11]->Fill(chi2dof,1./tr.gP.Pt());
      hA[12]->Fill(chi2dof,tr.gP.Eta());
      hA[13]->Fill(chi2dof,z0);
      hA[14]->Fill(chi2dof,tr.nFitPoint);
      hA[15]->Fill(chi2dof,tr.gP.Phi());
  }
}


//==========================================================
//==========================================================
void
Vertex3D:: initHisto (TObjArray*HList){
  TList *L=0;  TLine *ln=0; float yMax=1e6; TH1* h=0;
  
  memset(hA,0,sizeof(hA));
 
  assert(HList);
  hA[0]=new TH1F("v3D_myStat","3DVF my statistics;cases",15,0.5,15.5); // count cases
  hA[1]=h=new TH1F("v3D_inTr","# of input tracks, 3DVF; # of tracks",30,-0.5,29.5);
  ln=new TLine(cut_numTrack-0.5,0,cut_numTrack-0.5,yMax);
  ln->SetLineColor(kRed); ln->SetLineStyle(2);
  L= h->GetListOfFunctions();  L->Add(ln);

  hA[2]=h=new TH1F("v3D_inPt","PT , input tracks  3DVF; global PT (GeV) ",100,0,25);
  L= h->GetListOfFunctions(); 
  ln=new TLine(cut_pT1,0,cut_pT1,yMax);
  ln->SetLineColor(kRed); ln->SetLineStyle(2); L->Add(ln);
  ln=new TLine(cut_pT2,0,cut_pT2,yMax);
  ln->SetLineColor(kRed); ln->SetLineStyle(2); L->Add(ln);

  hA[3]=h=new TH1F("v3D_inSigY","sig(Yloc) @DCA , input tracks  3DVF; sig(Ylocal) (cm) ",30,0,1.2);
  ln=new TLine(cut_sigY,0,cut_sigY,yMax);
  ln->SetLineColor(kRed); ln->SetLineStyle(2);
  L= h->GetListOfFunctions();  L->Add(ln);


  hA[4]=new TH1F("v3D_inSigZ","sig(Z) @DCA , input tracks  3DVF; sig(Z) (cm) ",30,0,1.5);

  hA[5]=new TH1F("v3D_myStat2","3DVF track extrapolaton statistics;cases",15,0.5,15.5); // count cases
  // free:6-9

  hA[10]=new TH1F("v3D_chi1","#chi2/DOF best prim track candidates; #chi^{2}/DOF", 100,0,5);
  hA[11]=new TH2F("v3D_chi2","#chi2(1/PT) best prim track candidates; #chi^{2}/DOF; 1GeV/PT ", 30,0,3,30,0,2);
  hA[12]=new TH2F("v3D_chi3","#chi2(eta) best prim track candidates; #chi^{2}/DOF; eta ", 30,0,3,30,-2,2);
  hA[13]=new TH2F("v3D_chi4","#chi2(zVert) best prim track candidates; #chi^{2}/DOF; zVertex(cm) ", 30,0,3,30,-150,150);  
  hA[14]=new TH2F("v3D_chi5","#chi2(nFitPoints) best prim track candidates; #chi^{2}/DOF; nFitPoints ", 30,0,3,25,0,50);
  hA[15]=new TH2F("v3D_chi6","#chi2(phi) best prim track candidates; #chi^{2}/DOF; #phi (rad) ", 30,0,3,30,-3.2,3.2);

  for(int i=0;i<mxHA;i++) 
    if(hA[i]) HList->Add(hA[i]);


  // initialize event histograms..............
  memset(hYX,0,sizeof(hYX));
  memset(hYZ,0,sizeof(hYZ));

  nHE=0; // clear counter
  float mxR1=3; // cm
  for(int i=0;i<mxHE;i++) {
    char tt1[100], tt2[1000];
    sprintf(tt1,"v3D_eve%dYX",i);
    sprintf(tt2,"Y-X glob tracks at DCA, ieve=%d; X (cm); Y(cm); ",i);
    hYX[i]=new TH2F(tt1,tt2,2,-mxR1,mxR1,2,-mxR1,mxR1);
    HList->Add(hYX[i]);

    sprintf(tt1,"v3D_eve%dYZ",i);
    sprintf(tt2,"Y-Z glob tracks at DCA, ieve=%d; Z (cm); Y(cm); ",i);
    hYZ[i]=new TH2F(tt1,tt2,2,-mxR1,mxR1,2,-mxR1,mxR1);
    HList->Add(hYZ[i]);

  }
}
}// end namespace StEvPPV

