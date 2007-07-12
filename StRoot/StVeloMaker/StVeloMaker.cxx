//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StVeloMaker class for Makers                                         //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include "StVeloMaker.h"
#include "StMessMgr.h"
#include "TSystem.h"
#include "TH1.h"
#include "TH1K.h"
#include "TH2.h"
#if ROOT_VERSION_CODE < 331013
#include "TCL.h"
#else
#include "TCernLib.h"
#endif
#include "TTableSorter.h"
#include "TCanvas.h"
#include "TMath.h"
#include "TRandom.h"
#include "StDstPointChair.h"
#include "StTclHitChair.h"
#include "StG2TTpcChair.h"
#include "THelixTrack.h"

const double RTPCMAX = 200.;
const double RTPCMIN =  60.;

const double RTRKMIN = 500.; //Possible min=386.
const double RTRKMAX = 10000.;
const double XYERR = 0.1;
const double ZERR  = 0.2;
const double ERR2FACT  = 16.4;
const double MAXDZDS  = 5.;
const double MAXZ0    = 250.;
const double MAXZH    = 200.;
const int    NSTD   = 2;
const int    MAXACC = 9;
const int    i1REG  = 0;
const int    i2REG  = MAXACC/2;

const int    MAXINSECT = 50;
#define RREG(ireg) (RTPCMAX - (ireg)*(RTPCMAX-RTPCMIN)/MAXACC)


//_____________________________________________________________________________
class MyTCL {
public:
  MyTCL(){};
 ~MyTCL(){};
  static double *trsequ(double *smx, int m=3, double *b=0, int n=1);

  static double *vscale(double *a, double scale, double *b, int n)
    	{ for (int i=0;i<n;i++){b[i]=scale*a[i];}; return b;}


  static double *vlinco(double *a, double fa, double *b, double fb,double *x, int n)
    	{ for (int i=0;i<n;i++){x[i]=a[i]*fa+b[i]*fb;}; return x;}

//  x = G*c                                                
  static double *vmatl(double *g, double *c, double *x, int n=3,int m=3);
//  x = c*G                                                
  static double *vmatr(double *c, double *g, double *x, int n=3,int m=3);

                                               
};

//_____________________________________________________________________________
double *MyTCL::trsequ(double *smx, int m, double *b, int n)
{
   double *mem = new double[(m*(m+1))/2+m];
   double *v = mem;
   double *s = v+m;
   if (!b) n=0;
   TCL::trpck (smx,s    ,m);
   TCL::trsinv(s  ,s,    m);
   
   for (int i=0;i<n;i++) {
     TCL::trsa  (s  ,b+i*m, v, m, 1);
     TCL::ucopy (v  ,b+i*m, m);}
   TCL::trupck(s  ,smx,  m);
   delete [] mem;
   return b;
}

//_____________________________________________________________________________
  double *MyTCL::vmatl(double *g, double *c, double *x, int n,int m)
{
  for (int i=0; i<n; i++) {
    double sum = 0;
    for (int j=0; j<m; j++) sum += g[j + m*i]*c[j];
    x[i] = sum; }  
  return x;
}      

//_____________________________________________________________________________
  double *MyTCL::vmatr(double *c, double *g, double *x, int n,int m)
{
  for (int j=0; j<m; j++) {
    double sum = 0;
    for (int i=0; i<n; i++) sum += g[j + n*i]*c[i];
    x[j] = sum; }  
  return x;
}      
      
class MyHit {
  public:
  float x; float y; float z; float r; short s; short u;
};
class MyTrk {
  public:
  double x[3]; double d[3]; double rho; int s;
};

double anglim(MyHit *hit,int region = -1);
double anfGate(double rho,double rxy0,double rxy);
void padRefresh(TPad *pad,int flag=0);
void padReset(TPad *pad);

inline double sLen(double rtrk,double rho) 
{     double s = 0.5*(rtrk*rho);
      return (s < 0.1)? rtrk*(1.+s*s/6.) : 2 * asin(s)/rho;
}
//_____________________________________________________________________________
class TTTableSorter : public TTableSorter
{
  public:
  TTTableSorter(float *arr, int narr):TTableSorter(arr,narr){}
  Int_t BinarySearch(Float_t fkey)
  {return TMath::BinarySearch(fNumberOfRows,(const Float_t *)fSortIndex,fkey);}
  void Randomize(Int_t jl,Int_t jr); 
};
//_____________________________________________________________________________
void TTTableSorter::Randomize(Int_t jl,Int_t jr)
{
  static TRandom *tr=0;
  if (!tr) tr = new TRandom();

  if (!fSortIndex) 	return;

  if (jl < 0)			jl = 0;
  if (jl > fNumberOfRows-1) 	jl = fNumberOfRows-1;
  if (jr > fNumberOfRows-1) 	jr = fNumberOfRows-1;
  if (jl+1>=jr)		return;
  for (int i=jl; i<jr; i++) {
    int sht = (int)((jr-i+1)*tr->Rndm());
    if (!sht) 	continue;
    void *v = fSortIndex[i]; fSortIndex[i] = fSortIndex[i+sht];
    fSortIndex[i+sht] = v;
  }
}
//_____________________________________________________________________________
class myMesh 
{
public: 
    myMesh(int jl,int jr);
   ~myMesh(){};
 Int_t  Next();

 Int_t  fLeft;
 UInt_t fMax;
 UInt_t fCurr;
};
//_____________________________________________________________________________
myMesh::myMesh(int jl,int jr)
{
  fCurr  = 0;
  fMax = jr - jl;
  fLeft  = jl;
}

//_____________________________________________________________________________
Int_t myMesh::Next()
{
  while(2001) {
    UInt_t uu=0;
    UInt_t umax = fMax;
    UInt_t u=fCurr++;
    for (; umax; umax>>=1) {uu<<=1; if (u&1) uu|=1;u>>=1;}
    if (uu<=fMax) return fLeft + uu;
  }

}//_____________________________________________________________________________
ClassImp(StVeloMaker)
  
//_____________________________________________________________________________
StVeloMaker::StVeloMaker(const char *name): StMaker(name)
{
  hAll = &hZHit;
  memset(hAll,0,NTHIST*sizeof(void*));

  fC1=0; fC2=0; fC3=0;

  //		Histograms     
  
  hZHit    = new TH1K("HitZ","Z dist of fHits",400,-200.,200.);
  hZBadHit = new TH1K("BadHZ","Z bad fHits"   ,400,-200.,200.);

  hZVtx[0] = new TH1K("Zvtx+ ","Zvertex +Side   ",2*400,-200.,200.);
  hZVtx[1] = new TH1K("Zvtx- ","Zvertex -Side   ",2*400,-200.,200.);
  hZVtx[2] = new TH1F("Zvtx+C","Zvertex +Cleared",2*400,-200.,200.);
  hZVtx[3] = new TH1F("Zvtx+Zoom","Zvertex +ZOOM",40,-2.,2.);
  hZVtx[4] = new TH1F("Zvtx-Zoom","Zvertex -ZOOM",40,-2.,2.);
  hZVtx[5] = new TH1F("Rvtx+Zoom","Rvertex +ZOOM",40,0.,4.);
  hZVtx[6] = new TH1F("Rvtx-Zoom","Rvertex -ZOOM",40,0.,4.);
  hPlot = new TH2C("PlotZR","",80,-200.,200.,40,0.,200);

  hZHit->SetDirectory(0);
  hZBadHit->SetDirectory(0);
  hPlot->SetDirectory(0);
  for (int i=0;i<6;i++) hZVtx[i]->SetDirectory(0);

  fC1 = fC2 = fC3 = fC4 = 0;
  




}
//_____________________________________________________________________________
StVeloMaker::~StVeloMaker() {}

//_____________________________________________________________________________

Int_t StVeloMaker::Init() {

  hDZ[0] = new TH1F("dZ "    ," ZV+ - ZV- ", 2*20,-.5,.5);
  hDZ[1] = new TH1F("dZ/Err "," dZ / dzErr", 2*20,-5.,5.);
  hDZ[2] = new TH1F("zErr ","zErr"         , 40,0.,1.);
  hDZ[3] = new TH1F("ZV "," ZV", 40,-200.,200.);
 hZdZPlot = new TH2C("PlotZdZ","PlotZdZ",80,-200.,200.,40,-2.,2.);


  if (m_Mode&1) {
    fC1 = new TCanvas("C1");
    fC1->Divide(1,9);
    fC1->cd(1); hZHit->Draw();
    fC1->cd(2); hZBadHit->Draw();
    for (int i=0;i<7;i++) {fC1->cd(3+i);hZVtx[i]->Draw();}

    fC2 = new TCanvas("C2");
    hPlot->Draw();

    fC3 = new TCanvas("dZetc");
    fC3->Divide(1,4);
    for (int i=0; i<4; i++) {fC3->cd(i+1); hDZ[i]->Draw();}

    fC4 = new TCanvas("ZdZetc");
    hZdZPlot->Draw();

  }

  return StMaker::Finish();
  
  return StMaker::Init();
}

//_____________________________________________________________________________

Int_t StVeloMaker::Make() 
{
  int doUpdate=0;
  fHitChair=0;
  TDataSet *tphit = 0;
  if (!fHitChair) {
     tphit = GetDataSet("tphit");
     doUpdate = (tphit!=0);
     if (tphit) fHitChair = new StTclHitChair  ((St_tcl_tphit  *)tphit);
  }
  if (!fHitChair) {
     tphit = GetDataSet("dst/point");
     if (tphit) fHitChair = new StDstPointChair((St_dst_point  *)tphit);
  }
  if (!fHitChair) {
     tphit = GetDataSet("g2t_tpc_hit");
     if (tphit) fHitChair = new StG2TTpcChair  ((St_g2t_tpc_hit*)tphit);
  }



  if (!fHitChair) return kStErr;

  fNHits =  fHitChair->GetNRows(); 


  fGate = new float[fNHits];
  fHits = new MyHit[fNHits];
  int nacc = 0;
  double dz,zz;
  for(int ihit = 0; ihit < fNHits; ihit++) {
      if (fHitChair->DetectorId(ihit)!=kTpcIdentifier) continue;
      if (fHitChair->PadRow(ihit)==13) continue;		//***bad 13 padrow
      fHits[nacc].x = fHitChair->GetX(ihit);
      fHits[nacc].y = fHitChair->GetY(ihit);
      zz = fHitChair->GetZ(ihit);
      fHits[nacc].z = fHitChair->GetZ(ihit);
      fHits[nacc].s = (short)fHitChair->Sector(ihit);
      fHits[nacc].r = ::sqrt(::pow(fHits[nacc].x,2)+::pow(fHits[nacc].y,2));
      fHits[nacc].u = 0;
      fGate[nacc] = anglim(fHits+nacc);

      hZHit->Fill( fHits[nacc].z );
      if (fNHits<3000 || 
         (fHits[nacc].s==1 || fHits[nacc].s==13)) 
         hPlot->Fill(fHits[nacc].z,fHits[nacc].r);
      float sec = fHits[nacc].s-12.5;
      if (sec*fHits[nacc].z>0) {
        hZBadHit->Fill( fHits[nacc].z );
        continue;
      }
      nacc++;

  }
  fNHits = nacc;
  fNTrks = 0;
  MyTrk trks[24*MAXINSECT];
  fTrks = trks;
  if (fC1) {::padRefresh(fC1); ::padRefresh(fC2);}

  Int_t res;
  res = Make00();
  if (res) 	goto RETN;  

  res = Make01();
  if (res) 	goto RETN;  

  res = Make02();
  if (res) 	goto RETN;  

  if (!doUpdate) 		goto RETN;	
  dz = fV[3][2];
  if (dz*dz/fG[3][8] < 3.*3.)	goto RETN;

//VP  for(int ihit = 0; ihit < fNHits; ihit++) {
//VP    if(fHits[ihit].s <=12) fHits[ihit].z -= dz;
//VP    else                   fHits[ihit].z += dz;  
//VP  }


//		UPdate tphit
  nacc = 0;  
  fNHits =  fHitChair->GetNRows(); 
  for(int ihit = 0; ihit < fNHits; ihit++) {
    if (fHitChair->DetectorId(ihit)!=kTpcIdentifier) continue;
    zz = fHitChair->GetZ(ihit);
    int s = fHitChair->Sector(ihit);
    if(s <=12) zz -= dz; else zz += dz;  
    fHitChair->SetZ(zz,ihit);
  }

RETN:
  delete [] fGate;
  delete [] fHits;
  delete    fHitChair;

  return res;
}

//
//_____________________________________________________________________________


Int_t StVeloMaker::Make00() 
{

  int track[MAXACC];
  int i1sect,iReg,nmods=0;
  Int_t jhit;
  Int_t i1hit,j1hit,k1hit,n1hit;
  Int_t i2hit,j2hit,k2hit,n2hit;
  Int_t i3hit,j3hit,k3hit,n3hit;

  double x1,y1,z1,r1,rr1,xr1,yr1,s1;
  double x2,y2,z2,r2,rr2,xr2,yr2,s2;
  double x3,y3,z3,r3,rr3,        s3;
  double tmp;
  double rho,rc,xc,yc,z0,dzds,zmax,zmin;
  float dmore,dless;



  TTTableSorter mysorter(fGate,fNHits);

  for (i1sect = 1; i1sect<=24; i1sect++) {	//SectLoop
    iReg = i1REG;
    float beg1Key = i1sect*10+iReg;
    float end1Key = i1sect*10+iReg+1;
    k1hit = mysorter.BinarySearch(beg1Key)+1;
    n1hit = mysorter.BinarySearch(end1Key)+0;
    int inSect = 0;
    myMesh mesh1(k1hit,n1hit);

    for (j1hit=k1hit;j1hit<=n1hit;j1hit++)	// 1st hit
    {
      if (inSect >= MAXINSECT)			break;
      jhit = mesh1.Next();
      i1hit = mysorter.GetIndex(jhit);
      if (fHits[i1hit].u )			continue;
      assert(fGate[i1hit] <= end1Key && fGate[i1hit]>=beg1Key);
      track[i1REG] = i1hit;
      x1 = fHits[i1hit].x;
      y1 = fHits[i1hit].y;
      z1 = fHits[i1hit].z;
      r1 = fHits[i1hit].r;
      rr1 = r1*r1;
      xr1 = x1/rr1; yr1 = y1/rr1;
      rc = RTRKMIN;

      float beg2Key = anglim(fHits+i1hit,i2REG);
      float end2Key = beg2Key;
      dless = anfGate(1./RTRKMIN,r1,RREG(i2REG+1));
      dmore = anfGate(1./RTRKMIN,r1,RREG(i2REG+0));
      if (dmore < -dless) dmore = -dless;
      beg2Key -= dmore;
      end2Key += dmore;
      k2hit = mysorter.BinarySearch(beg2Key)+1;
      n2hit = mysorter.BinarySearch(end2Key)+0;
      myMesh mesh2(k2hit,n2hit);

      for (j2hit=k2hit;j2hit<=n2hit;j2hit++)		// 2nd hit
      {
        if (inSect >= MAXINSECT)		break;
        jhit = mesh2.Next();
	i2hit = mysorter.GetIndex(jhit);
        if (fHits[i2hit].u )			continue;
        assert(fGate[i2hit] <= end2Key && fGate[i2hit]>=beg2Key);
        track[i2REG] = i2hit;
	z2 = fHits[i2hit].z;
	x2 = fHits[i2hit].x;
	y2 = fHits[i2hit].y;
	r2 = fHits[i2hit].r;
	rr2 = r2*r2;
	dzds = (z2-z1)/(r2-r1);
	z0   = z1 - dzds*r1;
	if (fabs(z0) > MAXZ0) 			continue;

	xr2 = x2/rr2; yr2 = y2/rr2;
	xc = -(yr2-yr1); yc = (xr2-xr1);
	tmp = ::sqrt(xc*xc+yc*yc); xc = xc/tmp; yc = yc/tmp;
	rho = xc*(xr1+xr2)+yc*(yr1+yr2);
	if (fabs(rho) > 1./RTRKMIN)  		continue;
	if (rho < 0) {xc = -xc; yc = -yc; rho = -rho;}
	rc = (rho > 1./RTRKMAX)?  rc = 1./rho : RTRKMAX;

        s1 = sLen(r1,rho);
        s2 = sLen(r2,rho);

	dzds = (z2-z1)/(s2-s1);
	z0 = (z1 - dzds * s1);
	if (fabs(z0) > MAXZ0 ) 			continue;


	int weDidIt=0,weFailed=0; 
	for (iReg=i1REG+1;iReg<MAXACC;iReg++) {//find intermediate
//		Update limits
          if (iReg == i2REG) 	continue;
          float beg3Key = anglim(fHits+i2hit,iReg);
	  zmax = z0 + dzds*RREG(iReg+0);
	  zmin = z0 + dzds*RREG(iReg+1);
	  if (zmin>zmax) { tmp = zmin; zmin = zmax; zmax = tmp;}
          float end3Key = beg3Key;
          dmore = anfGate(rho,r2,RREG(iReg+0));
          dless = anfGate(rho,r2,RREG(iReg+1));
          if (x2*yc-y2*xc > 0.) { beg3Key +=dless; end3Key +=dmore;}
          else                  { beg3Key -=dmore; end3Key -=dless;}
	  k3hit = mysorter.BinarySearch(beg3Key)+1;
	  n3hit = mysorter.BinarySearch(end3Key)+0;
          myMesh mesh3(k3hit,n3hit);
	  weDidIt=0;
          track[iReg] = -1;
	  for (j3hit=k3hit;j3hit<=n3hit;j3hit++)
	  {
            jhit = mesh3.Next();
	    i3hit = mysorter.GetIndex(jhit);
//            assert(fGate[i3hit] <= end3Key && fGate[i3hit]>=beg3Key);
	    z3 = fHits[i3hit].z;
            if (z3>zmax || z3 < zmin)		continue;
            if (fHits[i3hit].u )		continue;
	    x3 = fHits[i3hit].x;
	    y3 = fHits[i3hit].y;
	    r3 = fHits[i3hit].r;
	    rr3 = r3*r3;
            double tmpx = fabs((xc*x3+yc*y3-0.5*rr3*rho))/(NSTD*XYERR);
            if (tmpx > 1.)			continue;
            s3 = sLen(r3,rho);
            double tmpz = fabs((z0+dzds*s3) - z3)/ZERR/NSTD;
            tmp = (tmpz*tmpz)/(1.+dzds*dzds);
            if (tmp > 1.)	 		continue;
            weDidIt = 1;			
            track[iReg] = i3hit;
            break;
	  }
          weFailed += !weDidIt;
	  if (weFailed>2) break; 			//unsuccess       
	}// end inter

	if (weFailed>2) continue; 			//unsuccess       
        inSect++; 
        z0 = MakeTrack(track);

           
	if (i1sect > 12) hZVtx[1]->Fill(z0);
	else             hZVtx[0]->Fill(z0);
	if (fC1 && nmods++ && (nmods)%100==0) ::padRefresh(fC1);
        break;
      }// end 2nd hit
    }//end 1st hit
  }//end SectLoop
  padRefresh(fC1);  
  return kStOK;
}
//_____________________________________________________________________________
double StVeloMaker::MakeTrack(int *tr)
{
   int ihit,lv,nacc=0;
   double pnts[MAXACC][3];

   for (lv =0; lv < MAXACC; lv++) 
   {
     ihit = tr[lv];
     if (ihit <0 ) 	continue;
     pnts[nacc][0] = fHits[ihit].u=1;  
     pnts[nacc][0] = fHits[ihit].x;  
     pnts[nacc][1] = fHits[ihit].y;  
     pnts[nacc][2] = fHits[ihit].z;  
     nacc++;
   }

   //double r1 = ::sqrt(::pow(pnts[i1REG][0],2)+::pow(pnts[i1REG][1],2));
   //double r2 = ::sqrt(::pow(pnts[i2REG][0],2)+::pow(pnts[i2REG][1],2));
   //double z0 = (pnts[i1REG][2]*r2- pnts[i2REG][2]*r1)/(r2-r1);

   THelixTrack hel(pnts[0],nacc);
   TCL::ucopy(hel.GetXYZ(),fTrks[fNTrks].x,3);
   TCL::ucopy(hel.GetDir(),fTrks[fNTrks].d,3);
   fTrks[fNTrks].rho = hel.GetRho();
   fTrks[fNTrks].s = fHits[tr[0]].s;

   fNTrks++;

   return fTrks[fNTrks-1].x[2];
}   
//_____________________________________________________________________________

Int_t StVeloMaker::Make01() 
{
  const double VTXWIDTH = 5.;

  const char *ve[] = {"+ve","-ve"};
  double step = hZVtx[0]->GetBinWidth(1);
  int    nbin = hZVtx[0]->GetNbinsX();

//	Clearing
    int ih = 0; 
    int jh = (ih+1)&1;
    double thri = hZVtx[ih]->GetEntries()/nbin;
    double thrj = hZVtx[jh]->GetEntries()/nbin;
    int maxShift = (int)(VTXWIDTH/step);
    int bestShift=0,iShift;
    double bestCorr=0;

    for (iShift = -maxShift; iShift<maxShift; iShift++) //shift loop
    {
      int nacc = 0;
      double corr=0,averi=0,aver2i=0,averj=0,aver2j=0;
    
      for (int ibin=0; ibin < nbin; ibin++) {	//LoopIBins
	int jbin = ibin + iShift;
	if (jbin<1 || jbin>nbin)	continue;
	double vali = hZVtx[ih]->GetBinContent(ibin);
	if(vali<thri) 	continue;
	double valj = hZVtx[jh]->GetBinContent(jbin);
        if (valj < thrj) 		continue;
	nacc++;
	averi += vali; averj += valj;
	aver2i += vali*vali; aver2j += valj*valj;
	corr += vali*valj;
      }//end LoopIBins
      if (!nacc) 			continue;
      corr /= nacc; 
      averi  /= nacc; averj  /= nacc;
      aver2i /= nacc; aver2j /= nacc;
      corr -= averi*averj;
      aver2i -= averi*averi;
      aver2j -= averj*averj;
      corr   /= ::sqrt(aver2i*aver2j);
      if (corr < bestCorr) continue;
      bestCorr = corr; bestShift=iShift;

    }//end shift loop

    if (!bestCorr) return kStErr;


    for (int ibin=0; ibin < nbin; ibin++) {	//FillBins
      int jbin = ibin + bestShift;
      if (jbin<1 || jbin>nbin)	continue;
      double vali = hZVtx[ih]->GetBinContent(ibin);
      if (vali < thri) 		continue;
      double valj = hZVtx[jh]->GetBinContent(jbin);
      if (valj < thrj) 		continue;
      hZVtx[ih+2]->SetBinContent(ibin,vali*valj);
    }//end fill Bins

  padRefresh(fC1);

  int nerr=0;
    double zmin = +MAXZ0;
    double zmax = -MAXZ0;
    
    TH1 *h = hZVtx[2];
    int    jmax = h->GetMaximumBin();
    double bmax = h->GetBinContent(jmax);
    int j;
    for (j = jmax+1; j <=nbin; j++) {if (h->GetBinContent(j)<0.1*bmax)break;}
    zmax = h->GetBinLowEdge(j+1);
    for (j = jmax-1; j >0    ; j--) {if (h->GetBinContent(j)<0.1*bmax)break;}
    zmin = h->GetBinLowEdge(j);
    fZLim[0][0] = zmin;                fZLim[0][1] = zmax;
    fZLim[1][0] = zmin+bestShift*step; fZLim[1][1] = zmax+bestShift*step;


    for (int iz=0;iz<2;iz++) {
      zmin= fZLim[iz][0] ;  zmax = fZLim[iz][1];
      double w = zmax-zmin;
      printf("StVeloMaker: %s TPC limits %5.0f  %5.0f, width = %4.0f"
           ,ve[iz],zmin,zmax,w);
    if (w > 2*VTXWIDTH) { nerr++; printf(" *** TOO BAD ***");}
    printf("\n");
    fZLim[iz][0] = zmin; fZLim[iz][1] = zmax;
  }
  return (nerr) ? kStWarn: kStOK;   

}
//_____________________________________________________________________________

Int_t StVeloMaker::Make02() 
{
  int j,is;
  

  FitVtx(0);
  printf("StVelo: +ZV = %12.7g %12.7g \n",fV[0][2],::sqrt(fG[0][8]));
  FitVtx(1);
  printf("StVelo: -ZV = %12.7g %12.7g \n",fV[1][2],::sqrt(fG[1][8]));
  double c[3][9],v[3][3];  
  for (is=0;is<2;is++) {
    TCL  ::ucopy(fG[is],c[is],9);
    MyTCL::trsequ(c[is],3);
    MyTCL::vmatl(c[is],fV[is],v[is]);
  }
    TCL::vadd(c[0],c[1],c[2],9);
  MyTCL::trsequ(c[2]);
    TCL::ucopy(c[2],fG[2],9);
    TCL::vadd(v[0],v[1],v[2],3);
  MyTCL::vmatl(c[2],v[2],fV[2]);

  MyTCL::vlinco(fV[0],+0.50,fV[1],-0.50,fV[3],3);
  MyTCL::vscale(c[2] ,+0.25,fG[3]            ,9);  

  for (is=0;is<4;is++){
    printf("Vtx+-Err = ");
    for (j=0;j<3;j++)printf("%8.3f(+-%5.3f) ",fV[is][j],::sqrt(fG[is][j*4]));   
    printf("\n");
  }
    hDZ[0]->Fill(fV[3][2]);
    hDZ[1]->Fill(fV[3][2]/::sqrt(fG[3][8]));
    hDZ[2]->Fill(::sqrt(fG[3][8]));
    hDZ[3]->Fill(fV[2][2]);
    hZdZPlot->Fill(fV[2][2],fV[3][2]);
    padRefresh(fC3);padRefresh(fC4);


    return 0;
}
//_____________________________________________________________________________
void StVeloMaker::FitVtx(int is) 
{
  const double WT0 = 0.1;
  int i,j,jlist;
  double *x,*d,A[3],BB[3][3],ABAB[3],z0;
  TCL::vzero(A,3);
  TCL::vzero(ABAB,3);
  TCL::vzero(BB[0],3*3);
  int N = 0;   

  for (jlist=0;jlist<fNTrks; jlist++) {//listLoop
    int isect = fTrks[jlist].s;
    if (is  != (isect > 12)) 	continue;
    z0 = fTrks[jlist].x[2];
    if (z0 < fZLim[is][0]) 	continue;     
    if (z0 > fZLim[is][1]) 	continue;     
    N++;
    x = fTrks[jlist].x;
    d = fTrks[jlist].d;
    double ba = TCL::vdot(x,d,3);
    for (i=0;i<3;i++) {
      A[i] += x[i];
      ABAB[i] += x[i] - ba*d[i];
      for (j=0;j<3;j++) {BB[i][j] -= d[i]*d[j];}
    }
  }
  BB[2][2] -= WT0*1.;
    
  for (i=0;i<3;i++) {
    A[i] /= N; ABAB[i] /= (N+WT0);
    for (j=0;j<3;j++) {BB[i][j] /= (N+WT0);}
    BB[i][i] += 1.;
  }
  MyTCL::trsequ(BB[0],3, ABAB);
  TCL::ucopy(ABAB,fV[is],3);
  double err2Fact = ERR2FACT/MAXACC/N;
  MyTCL::vscale(BB[0],err2Fact,fG[is],9);
  
}


//_____________________________________________________________________________
void StVeloMaker::PrintInfo() {

  if (Debug()) StMaker::PrintInfo();
}

//-----------------------------------------------------------------------

Int_t StVeloMaker::Finish()
{
  padRefresh(fC1);
  padRefresh(fC2);
  padRefresh(fC3);
  padRefresh(fC4);
  return StMaker::Finish();
}
//-----------------------------------------------------------------------

void StVeloMaker::Clear(const char *)
{
//	Clear histos
  padReset(fC2);
  padReset(fC1);
}

//----------------------------------------------------------------------
double anfGate(double rho,double rxy0,double rxy)
{
  double dfi = 0.5*(rxy-rxy0)*rho;
  double err = NSTD*XYERR/rxy;
  if (dfi>0.) dfi+=err; else dfi-=err;
  return 0.1*dfi;

}
//----------------------------------------------------------------------
double anglim(MyHit *hit, int region)
{
  double ang = atan2(hit->y,hit->x)+M_PI ;
  if (region == -1) {
    region = (int)((RTPCMAX - hit->r)/(RTPCMAX-RTPCMIN)*MAXACC);
  }

  return (hit->s)*10 + region +0.1*ang;

}
#if 0
//----------------------------------------------------------------------
double anglim(MyHit *hit, int region)
{
  double ang = (hit->z/hit->r)/(MAXZH/RTRKMIN)+1. ;
  if (region == -1) {
    region = (int)((RTPCMAX - hit->r)/(RTPCMAX-RTPCMIN)*MAXACC);
  }

  return (hit->s)*10 + region +0.1*ang;

}
#endif
//----------------------------------------------------------------------
void padRefresh(TPad *pad,int flag)
{
  if (!pad) return;
  pad->Modified();
  pad->Update();
  TList *tl = pad->GetListOfPrimitives();
  if (!tl) return;
  TListIter next(tl);
  TObject *to;
  while ((to=next())) {
    if (to->InheritsFrom(TPad::Class())) padRefresh((TPad*)to,1);}
  if (flag) return;
  gSystem->ProcessEvents();

}
//----------------------------------------------------------------------
void padReset(TPad *pad)
{
  if (!pad) return;
  TList *tl = pad->GetListOfPrimitives();
  if (!tl) return;
  TListIter next(tl);
  TObject *to;
  while ((to=next())) {
    if (to->InheritsFrom(TH1::Class())) ((TH1*)to)->Reset();
    if (to->InheritsFrom(TPad::Class())) padReset((TPad*)to);}

}

