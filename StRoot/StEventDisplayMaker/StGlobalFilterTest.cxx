// $Id: StGlobalFilterTest.cxx,v 1.5 2004/11/16 04:33:05 perev Exp $
#include "TError.h"
#include "TSystem.h"
#include "TCanvas.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TCL.h"
#include "StGlobalFilterTest.h"
#include "TObjArray.h"
#include "StEventHelper.h"
#include "StThreeVectorD.hh"
#include "StThreeVectorF.hh"
#include "StPhysicalHelixD.hh"
#include "THelixTrack.h"
#include "StContainers.h"
#include "StHit.h"
#include "THack.h"


ClassImp(StGlobalFilterTest)
//_____________________________________________________________________________
StGlobalFilterTest::StGlobalFilterTest(): StGlobalFilterABC("Test","Test")
{
   memset(fHRes,0,sizeof(fHRes));
   fCanvas[0] = new TCanvas("RESID","Track Residuals",1);
   fHRes[0] = new TH1F("RESH0","max resuduals    ",100,0,2.);
   fHRes[1] = new TH1F("RESH1","max resuduals BAD",100,0,2.);
   fHRes[2] = new TH1F("RESH2","ave resuduals    ",100,0,2.);
   fHRes[3] = new TH1F("RESH3","ave resuduals BAD",100,0,2.);
   fHRes[4] = new TH1F("RESH4","big resuduals    ",100,-3.,3.);
   int nhres = sizeof(fHRes)/sizeof(void*);
   fCanvas[0]->Divide(1,nhres);
   for (int i=0;i<nhres;i++) {
     fCanvas[0]->cd(i+1); fHRes[i]->Draw();fHRes[i]->StatOverflows();}

   fCanvas[1] = new TCanvas("RESDE","Residuals:dE",1);
   fPlot[0]= new TH2F("RESDE1","Residuals:dE ",100,0,10.,100,0,5.);
   fPlot[0]->Draw();
}
//_____________________________________________________________________________
void StGlobalFilterTest::Filter(TObjArray *eArr,int flag)
{
  static int nKount=0;
  int kind,nTot=0,nSel=0,iSel=0;;
  TObject *to;
  
  int n = eArr->GetLast()+1;
  for (int ioj=0;ioj<n-1;ioj++)
  {
    to = eArr->At(ioj);
    if (!to) 		continue;
    kind = StEventHelper::Kind(to);
    if (!(kind&kTRK)) 	continue;
    StTrack *trk = (StTrack *)to;
    to = eArr->At(ioj+1);
    if (!to) 		continue;
    kind = StEventHelper::Kind(to);
    if (!(kind&kHRR)) 		{ eArr->AddAt(0,ioj);continue;}
    StPtrVecHit *hr = (StPtrVecHit *)to;
    int nhits = hr->size();
    if (nhits<10) 		{(*eArr)[ioj]=0;(*eArr)[ioj+1]=0;continue;}
    nTot++;
    StTrackHelper th(trk);
    double len = th.GetLength();
    double mom  = th.GetMom().mag();
    if (mom<0.3) 		{(*eArr)[ioj]=0;(*eArr)[ioj+1]=0;continue;}
    if (th.GetFlag()<0) 	{(*eArr)[ioj]=0;(*eArr)[ioj+1]=0;continue;}
//   Only bad tracks
    
    iSel=0;
    

    double curv1,curv2,dE;

    StPhysicalHelixD *hlx[2] ={0,0};
    THelixTrack myHlx[2];
    double myBeg[2][3],myDir[2][3];
    for (int i=0;i<2;i++) { 
       nKount++;
       hlx[i]=th.GetHelix(i);
       StEventHelper::MyHelix(myHlx+i,hlx[i]);
       if (i) myHlx[i].Backward();
       myHlx[i].Step(0.,myBeg[i],myDir[i]);}
    curv1 = hlx[0]->curvature();
    curv2 = hlx[1]->curvature();
    dE = 1.e+10;
    if (curv1>1.e-10 && curv2>1.e-10) {
      dE = (1./curv1-1./curv2)/len/(1.+pow(0.135/mom,2));
    }
    
    double maxRes[2] = {0,0};
    double aveRes[2] = {0,0};
    int    ncount[2] = {0,0};
    int svt=0;
    for (int ih=0;ih<nhits;ih++) {
      StHit *hit = hr->at(ih);
      int bad = hit->flag()!=0;
      if (hit->detector()==kSvtId) svt++;
      StHitHelper hh(hit);
      if (!hh.IsFit()) continue;
      StThreeVectorD pnt = hh.GetPoint();
      double dist ;
      double myHit[3],myClose[2][3],myDlose[2][3],myDist[3][3],s[3];
      s[2] = hlx[0]->pathLength(pnt);
      double per = hlx[0]->period();
      while (s[2]<-per) s[2]+=per;
      while (s[2]> per) s[2]-=per;

      myHit[0]=pnt.x();myHit[1]=pnt.y(); myHit[2]=pnt.z();
      int iSel=0;
      for (int i=0;i<2;i++) {
	s[i]=myHlx[i].Step(myHit,myClose[i],myDlose[i]);
        if(s[i]>1000) iSel |= 16;
	if(s[i]<-1)   iSel |= 32; 
	if (iSel) { break;}
        TCL::vsub(myHit,myClose[i],myDist[i],3);
        double tmp = sqrt(TCL::vdot(myDist[i],myDist[i],3));
        int noProblem = (i || tmp>2. || fabs(s[0]-s[2])< 0.1 );
        if (!noProblem) {
	  printf("***Problem***	tmp=%g,s[0]=%g, s[2]=%g\n",tmp,s[0],s[2]);
	  printf("MyHit	= %g %g %g\n",myHit[0],myHit[1],myHit[2]);
          myHlx[0].Print();
//	Assert(noProblem);
        }
      }
      double wt0 = pow(s[0],3);
      double wt1 = pow(s[1],3);
      double wtn = wt0+wt1;
      wt0/=wtn;wt1/=wtn;
      TCL::vlinco(myDist[0],wt1,myDist[1],wt0,myDist[2],3);
      dist = sqrt(TCL::vdot(myDist[2],myDist[2],3));
      fHRes[4]->Fill(log10(dist));
      ncount[bad]++;
      aveRes[bad]+= dist*dist;
      if(dist>maxRes[bad]) maxRes[bad] = dist;
    }

    if (flag&kHIT){
      for (int ib=0;ib<2;ib++) {
        if(!ncount[ib]) continue;
        aveRes[ib] = sqrt(aveRes[ib]/ncount[ib]);
        fHRes[ib+0]->Fill(maxRes[ib]);
        fHRes[ib+2]->Fill(aveRes[ib]);
      }
      fPlot[0]->Fill(mom,maxRes[0]);

    }
    if (maxRes[0]>0.6) iSel|=64;
    if (iSel) 	{nSel++;continue;}
    (*eArr)[ioj]=0;(*eArr)[ioj+1]=0;
  }
   THack::PadRefresh(fCanvas[0]);
   THack::PadRefresh(fCanvas[1]);
   printf("\nStGlobalFilterTest: %d tracks %d were selected\n",nTot,nSel);
}
//_____________________________________________________________________________
void StGlobalFilterTest::NewEvent(int nrun,int nevt)
{
   fHRes[0]->Reset();
   fHRes[1]->Reset();
   fHRes[2]->Reset();
}
