// $Id: StMCStepping2Hist.cxx,v 1.7.6.1 2021/05/08 21:59:07 perev Exp $
//
//
// Class StMCStepping2Hist
// ------------------
// 

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "TROOT.h"
#include "TColor.h"
#include "TVirtualMC.h"
#include "StMCStepping2Hist.h"
#include "StTGeoProxy.h"
#include "TObjArray.h"
#include "TNamed.h"
#include "TH1F.h"
#include "THStack.h"

#include "TGeoManager.h"
#include "TGeoVolume.h"
#include "TGeoShape.h"
#include "TGeoBBox.h"
#include "TGeoTube.h"
#include "TProfile.h"
#include "TProfile2D.h"
#include "TLegend.h"
#include "TStyle.h"
#include "TCanvas.h"
#include "TSystem.h"
#include "StTProfile2D.h"
#include "StiELossTrk.h"

void Break(int ii) {
static int myII=-1946;
if (ii != myII) return;
printf ("Break %d\n",ii);
}

StMCStepping2Hist *StMCStepping2Hist::fThis=0;

static TH1D *Convert(const TProfile *tp,const char *suff="th1d")
{
     int nx = tp->GetXaxis()->GetNbins(); 
  double xl = tp->GetXaxis()->GetXmin();
  double xu = tp->GetXaxis()->GetXmax();

  TString ts(tp->GetName());ts += "_"; ts+=suff;
  TH1D  *hh = new TH1D(ts,tp->GetTitle(),nx,xl,xu);
  double ents=0;
  for (int i=1;i<=nx;i++) {
    double cont = tp->GetBinContent(i);
    hh->SetBinContent(i, cont);
    ents += cont;
  }
  hh->SetEntries(ents);
  hh->SetFillColor(tp->GetFillColor());
  return hh; 
}

//_____________________________________________________________________________
class My2Hist
{
enum {kMAXNODE=1000,kMAXMODU=20};
public:
class Node_t {
public:
TString name;
double  radL;
double  ort2;
double  rxy;
double  z;
double  len;
double  mcs[3];
   int  ihit;

};

private:
class From_t {
public:
  From_t(){ ort2=0;radL=0;nTims=0;}

public:
TString name;
double  radL;
double  ort2;
int     nTims;
};

class Modu_t {
public:
Modu_t() {memset(mybeg,0,myend-mybeg+1);}
From_t *GetFrom(const char *name);
void   Add2From(const char *name,double radl,double ort2);
public:
TString name;
char    mybeg[1];
double  radL;
double  ort2;
double  rMax;
double  zMax;
int     ihit;
int     kolo;
TProfile *prof[4];
int     nfrom;
char    myend[1];
From_t  from[kMAXMODU];
};

public:
   My2Hist(const char *tit);
const char *GetName() const {return mName.Data();}
void SetEta(double eta){ mEta = eta;}
Node_t &AddNode();


void Fill(double rxy0,double rxy1,double z0,double z1,double radL);
void Update();
void Paint();
void Save();
void PrintFrom();
Modu_t &GetModu(const TString &name);
void Clear();
void PadClean(TPad *pad);

private:
char mFist[1];
int mNModu;  	//number of modules used
int mNNode;     //number of nodees  used
int mNBins;	//number of bins in profile
double mLimt[2];//limits of profile
double mEta;


THStack    *mStk[3][2];
TCanvas    *mC[3][2],*mC2,*mCt;
TLegend    *mL[3][2],*mL2;

StTProfile2D *mP2;
TH1D         *mHt;
double mDelta2Z;
double mDelta2R;
char mLast[1];

Node_t	mNode[kMAXNODE];
Modu_t 	mModu[kMAXMODU];
TString	mName; 		//Name

};

//_____________________________________________________________________________
My2Hist::From_t *My2Hist::Modu_t::GetFrom(const char *name)   
{
  int i;
  for (i=0;i<nfrom;i++) {if (from[i].name == name) return from+i;}
  
  nfrom = i+1;
  from[i].name=name;
  return from+i;
}
//_____________________________________________________________________________
void My2Hist::Modu_t::Add2From(const char *name,double radl,double ort2)
{
   From_t *fr = GetFrom(name);
   fr->nTims++;
   fr->radL +=radl;
   fr->ort2 +=ort2;
}
//_____________________________________________________________________________
My2Hist::My2Hist(const char *tit)   
{
   gStyle->SetPalette(1);
   if (!gROOT->GetColor(1000)) new TColor(1000,1,1,1);


   memset(mFist,0,mLast-mFist);
   mNBins = 60;
   mLimt[0]= -6;
   mLimt[1]=  6;
   mName = tit;
   TString ts("P2_");ts+=mName; ts+="_ZR";
   double zLow=-2000,zUpp=2000;
   double rLow=0   ,rUpp=500;
   int nZ=400,nR=250;
   mDelta2Z = (zUpp-zLow)/nZ;
   mDelta2R = (rUpp-rLow)/nR;
   mP2 = new StTProfile2D(ts,"invX0(Z,Rxy)",nZ,zLow,zUpp,nR,rLow,rUpp);
   mHt = new TH1D("OldStar","TrackLen(Rxy)",100,1./200,1./60);
}   

//_____________________________________________________________________________
My2Hist::Node_t  &My2Hist::AddNode()
{
   return mNode[mNNode++];
}
//_____________________________________________________________________________
void My2Hist::Fill(double r0,double r1,double z0,double z1,double radL)
{
  if (radL <=0.) radL = 3e33;
  double dR = r1-r0,dZ = z1 - z0;
  double Delta2R = (dR<0) ? -mDelta2R:mDelta2R;
  double Delta2Z = (dZ<0) ? -mDelta2Z:mDelta2Z;
  double tau,dau;
  double dQ = sqrt(dR*dR+dZ*dZ);
  double rA=r0,zA=z0;
  while(1) {
    double rB = int((rA+Delta2R)/mDelta2R)*mDelta2R;
    tau = (r1-rA)/dR;
    dau = (rB-rA)/dR;
    if (tau>dau) tau = dau;

    double zB = int((zA+Delta2Z)/mDelta2Z)*mDelta2Z;
    dau = (z1 -zA)/dZ;
    if (tau>dau) tau = dau;
    dau = (zB  -zA)/dZ;
    if (tau>dau) tau = dau;
    if (tau<1e-6) break;
    rB = rA + dR*tau;
    zB = zA + dZ*tau;
    double dL = dQ*tau;
    mP2->Fill(0.5*(zA+zB),0.5*(rA+rB),1./radL,dL);
    if (fabs(z0) <200 && fabs(z1) <200 && r1<200) { mHt->Fill(1/(0.5*(rA+rB)),dL);}
    rA = rB; zA = zB;
  }

}
//_____________________________________________________________________________
void My2Hist::Update()
{
  if (!mNNode) return;
  int idxLst;
  for (idxLst=mNNode-1;idxLst>=0;idxLst--) 	{if(mNode[idxLst].ihit) break;}
  if (idxLst<0) {mNNode=0; Clear(); return;}

  TString ts(mNode[idxLst].name);
  for (;idxLst>=0;idxLst--) 	{if(mNode[idxLst].name!=ts) break;}
  if (idxLst<0) {mNNode=0; Clear(); return;}


  for (int idx=0;idx<=idxLst;idx++) {
    Modu_t &modu = GetModu(mNode[idx].name);
    modu.radL += mNode[idx].radL;
    assert(modu.radL<100);
    modu.ihit += mNode[idx].ihit;
    if (modu.rMax< mNode[idx].rxy) modu.rMax = mNode[idx].rxy;
    if (modu.zMax< mNode[idx].z  ) modu.zMax = mNode[idx].z  ;
  } 

  for (int idx=0;idx<mNModu;idx++) {
    if (mModu[idx].radL <=0) 	continue;
    assert(mModu[idx].radL<100);
    mModu[idx].prof[0]->Fill(mEta,mModu[idx].radL);
  } 

  TString prevName("****");
  for (int ihi=0;ihi<mNNode;ihi++) {
    if (!mNode[ihi].ihit) 	continue;
    double radL[2]={0},ort2[2]={0};
    TString hitName(mNode[ihi].name);
    double hitLen = mNode[ihi].len;
    Modu_t &hitm = GetModu(mNode[ihi].name);

    for (int idx=0;idx<ihi;idx++) {
      radL[0]+= mNode[idx].radL;
      ort2[0] = mNode[idx].ort2;
      double ort2m = StiELossTrk::GetOrt2(mNode[idx].mcs,hitLen);
      hitm.Add2From(mNode[idx].name,mNode[idx].radL,ort2m);
      if (mNode[idx].name == mNode[ihi].name) continue;
      radL[1]+= mNode[idx].radL;
      ort2[1] = mNode[idx].ort2;
      hitm.prof[1]->Fill(mEta,radL[1]);
      hitm.prof[2]->Fill(mEta,sqrt(ort2[1]));
  } }

  Clear();

}

//_____________________________________________________________________________
void My2Hist::Paint()
{
  enum {nKols=12};
// Magenta= red anilin (sirenevyj)
// Cian   = light blue (goluboj)
// Spring = light green
// Teal   = Blue Green
//	Sort by aver rxy
  Modu_t swap;

  do { swap.prof[0]=0;
    for (int i=1;i<mNModu;i++) {
      if (mModu[i].rMax >= mModu[i-1].rMax) continue; 
      swap = mModu[i]; mModu[i]=mModu[i-1]; mModu[i-1]=swap;
    }
  } while(swap.prof[0]);

  for (int i=0,kol=-1;i<mNModu;i++) {
    int nEnt0 = (int)mModu[i].prof[0]->GetEntries();
    int nEnt1 = (int)mModu[i].prof[1]->GetEntries();
    if (nEnt0+nEnt1<=0) continue;
    kol++;
    mModu[i].kolo = gStyle->GetColorPalette((kol*23));
    const char *cens = (mModu[i].ihit)? "*":" ";
    printf (" Modu=%s%s \tRxy=%g \tradL=%g \tEnt=%d/%d \tColor=%d\n"
    ,(const char*)mModu[i].name,cens
    ,mModu[i].rMax
    ,mModu[i].prof[0]->GetMaximum()
    ,nEnt0,nEnt1
    ,mModu[i].kolo
    );
  }
static const char *ParaName[3]={"RadL" ,"RadL","Ort"};  
static const char *SmallBig[2]={"Small","Big" };  
static const char *FromInto[3]={"ThisToAll" ,"AllToThis","AllToThis"};  
static const double   myMax[3]={    3  ,3     ,    3 };
  for (int jk=0;jk<3; jk++) {

    for (int sb=0;sb<2; sb++) {
      TString namS("HS_"); namS+=ParaName[jk];namS+=SmallBig[sb]; namS+=FromInto[jk];
      TString namH(namS); namH.Replace(1,1,"1");
      TString tit(SmallBig[sb]);  tit+=" "; tit+=ParaName[jk]; 
      tit+="(Eta) contribution "; tit+=FromInto[jk];
      THStack *ths = new THStack(namS,tit);
      TLegend *tl  = new TLegend(0.6,0.6,0.9,0.9);
      mL[jk][sb] = tl;
      tl->SetFillStyle(4050);
      mStk[jk][sb] = ths;
      for (int ih = 0;ih<mNModu; ih++) {
	if (!mModu[ih].prof[jk]->GetEntries()) 		continue;
	if ((sb) != (mModu[ih].prof[jk]->GetMaximum()>myMax[jk])) continue;
    //    assert(kol<nKols);
	mModu[ih].prof[jk]->SetFillColor(mModu[ih].kolo);
	ths->Add(Convert(mModu[ih].prof[jk],namH));
        tl->AddEntry(mModu[ih].prof[jk],"","f");      
      }// end ih
      if (!ths->GetHists()) continue;
      TString ts("C_"); ts+=mName; ts+=ParaName[jk];
      ts+=SmallBig[sb] ; ts +=FromInto[jk];

      mC[jk][sb] = new TCanvas(ts,tit,600,800);
      ths->Draw(); tl->Draw();
      mC[jk][sb]->Update();

    }//end sb
  }//end jk

   mP2->SupressZeros(1e-5);
   TString ts("C_"); ts+=mName; ts+="_ZR";
   mCt = new TCanvas("OldStar","TrakLen(Rxy)" ,600,800);
   mHt->Draw();
   mCt->Modified(); mCt->Update();

   mC2 = new TCanvas(ts       ,"invX0(Z,Rxy)" ,600,800);
   mC2->Divide(1,3);
   mC2->cd(1); mP2->Draw("colz");
   ts="P_"; ts+=mName; ts+="_Z";
   TProfile *pz = mP2->ProfileX(ts,1,-1); 
   pz->SetFillStyle(3026);
   pz->SetLineColor(2);
   pz->SetLineWidth(3);

   ts="P_"; ts+=mName; ts+="_R";
   TProfile *pr = mP2->ProfileY(ts,1,-1);
   pr->SetFillStyle(3026);
   pr->SetLineColor(2);
   pr->SetLineWidth(3);

   mC2->cd(2); pz->Draw("");
   mC2->cd(3); pr->Draw("");

   mC2->Modified(); mC2->Update();
}
//_____________________________________________________________________________
void My2Hist::Save()
{
   for (int i=0;i<7;i++) {
     if(!mC[0][i]) continue;
     mC[0][i]->Update();
     mC[0][i]->Print(".png");
     PadClean(mC[0][i]);
     mC[0][i]->Print(".C");
     if (i>5) continue;
     TString ts(mC[0][i]->GetName());ts.Replace(0,1,"HTM");
//     TQtCanvas2Html WebSite(mC[0][i],1.8,ts.Data());
//        char cc[500];
//        sprintf(cc,"TQtCanvas2Html WebSite((TCanvas*)%p,1.8,\"%s\");"
//               ,(void*)mC[0][i],mC[0][i]->GetName());
// 
//        gROOT->ProcessLine(cc);
   }
}
//_____________________________________________________________________________
My2Hist::Modu_t &My2Hist::GetModu(const TString &name)
{
static int idx = -1;
  if (idx>=0 && name == mModu[idx].name) return mModu[idx];
  
//search by name
  for (idx=0; idx < mNModu ; idx++) {if (name==mModu[idx].name) break;}
   
  if (idx==mNModu) { 
    mModu[mNModu].name = name; 
    TString ts(name); ts+="_";
    mModu[mNModu].prof[0] = new TProfile(name,name,mNBins,mLimt[0],mLimt[1]);  
    ts+="_";    
    mModu[mNModu].prof[1] = new TProfile(ts  ,name,mNBins,mLimt[0],mLimt[1]);  
    ts+="_";    
    mModu[mNModu].prof[2] = new TProfile(ts  ,name,mNBins,mLimt[0],mLimt[1]);  
    mNModu++; assert(mNModu<100);
  }
  return mModu[idx];
}
//_____________________________________________________________________________
void My2Hist::Clear()
{
  mNNode = 0;
  for (int idx=0;idx<mNModu;idx++) {
    mModu[idx].ihit=0;
    mModu[idx].radL=0;
  }
}
//_____________________________________________________________________________
void My2Hist::PadClean(TPad *pad)
{
  static int nCall=0; nCall++;
  if (!pad) return;
  TList *tl = pad->GetListOfPrimitives();
  if (!tl) return;
  TObject *to;
  TObjLink *lnkNex = tl->FirstLink(),*lnk=0;
  while (lnkNex) {
    lnk = lnkNex; lnkNex = lnk->Next();
    to = lnk->GetObject(); if (!to) continue;
    if (to->InheritsFrom(TPad::Class())) { PadClean((TPad*)to); continue;}
    if (to->InheritsFrom(TH1F::Class())) { tl->Remove(lnk);/* delete to;*/} 
  }

}
//_____________________________________________________________________________
void My2Hist::PrintFrom()
{
#define QWE(x) (int(x*1000)/1000.)
  TString ts(GetName()); ts+=".tab";
  FILE *ftab = fopen(ts.Data(),"w");
  assert (ftab);


  fprintf(ftab,"\n\n   PrintFrom() %s contributions\n\n",GetName());

  for (int im=0;im<mNModu; im++) {
    int nfr = mModu[im].nfrom;
    if (!nfr) continue;
    From_t *fr = mModu[im].from;
    double radL=0,ort2=0;
    for (int jm=0;jm<nfr;jm++) {
      fr[jm].radL/=fr[jm].nTims;
      fr[jm].ort2/=fr[jm].nTims;
      radL+=fr[jm].radL;
      ort2+=fr[jm].ort2;
      fr[jm].ort2=sqrt(fr[jm].ort2);
    }
    ort2 = sqrt(ort2);
    fprintf(ftab,"%s(%6.4g  ) =\t",mModu[im].name.Data(),QWE(radL));
    for (int jm=0;jm<nfr;jm++) {
      if (fr[jm].ort2<0.1*ort2) continue;
      fprintf(ftab,"%s(%6.4g  ) \t"
             ,fr[jm].name.Data()
             ,QWE(fr[jm].radL));
    }
    fprintf(ftab,"\n%s(%6.4gcm) =\t",mModu[im].name.Data(),QWE(ort2));
    for (int jm=0;jm<nfr;jm++) {
      if (fr[jm].ort2<0.1*ort2) continue;
      fprintf(ftab,"%s(%6.4gcm) \t"
            ,fr[jm].name.Data()
            ,QWE(fr[jm].ort2));
    }     
    fprintf(ftab,"\n\n");  
  }  
  fclose(ftab);
}



//_____________________________________________________________________________
//_____________________________________________________________________________
ClassImp(StMCStepping2Hist)

//_____________________________________________________________________________
StMCStepping2Hist::StMCStepping2Hist(const char *name,const char *tit)
  : StMCStepping(name,tit)
{
   gStyle->SetPalette(1);
   assert(!fThis);
   memset(fFist,0,fLast-fFist);
   TString tsName(GetName());
   int yf = tsName.Contains(".DEV2");
   tsName = gSystem->BaseName(tsName.Data());
   tsName.ReplaceAll(".C","");
   tsName.ReplaceAll("Geometry.","");
   if (yf) tsName += "yf";
   fThis = this;
   fMy2Hist  = new My2Hist(tsName);
   fELossTrk[0] = new StiELossTrk;
   fELossTrk[1] = new StiELossTrk;
}   
//_____________________________________________________________________________
StMCStepping2Hist::~StMCStepping2Hist()
{
   fThis=0;
}
//_____________________________________________________________________________
void StMCStepping2Hist::Print(const Option_t*) const
{
   StTGeoProxy::Instance()->Print(KazeAsString(fKaze));
   printf("RadLen=%g fCurrentLength=%g Rxy=%g Z=%g\n\n"
         , fTotRadL,fCurrentLength,fCurrentPosition.Perp(),fCurrentPosition.Z());
}		
//_____________________________________________________________________________
int StMCStepping2Hist::Fun()
{
static int nCall = 0;
nCall++;

if (!fHitShape)  fHitShape = StTGeoProxy::Instance()->GetHitShape();

  const TGeoVolume *modu = 0;     TString ts,modName;
  Case();
// Sensitive volume
   
  if (fSensMaxZ < fabs(fCurrentPosition.Z())) fSensMaxZ = fabs(fCurrentPosition.Z());
  if (fSensMaxR < fCurrentPosition.Perp()   ) fSensMaxR = fCurrentPosition.Perp();
//

  assert(fCurrentLength< 10000);
  assert(fEnterLength  < 10000);
  
//   StTGeoProxy::Instance()->Print(KazeAsString(fKaze));
//   printf("fEnterLength=%g fCurrentLength=%g Rxy=%g Z=%g\n\n"
//         , fEnterLength, fCurrentLength,fCurrentPosition.Perp(),fCurrentPosition.Z());
SWITCH: int myKaze = fKaze;
if (GetDebug()) {printf("%d - ",nCall); Print();}
if (strcmp(fVolume->GetName(),"TPAD")==0) Break(1);


  switch (fKaze) {
    case kNEWtrack:;
      fELossTrk[0]->Reset();
      fMy2Hist->Update();
      fTotRadL=0;
      fModName=""; 

    case kENTERtrack:;
         if (strcmp(fVolume->GetName(),"HALL")==0) fKaze=kENDEDtrack;
         if (fHitShape->Outside(fCurrentPosition.Z(),fCurrentPosition.Perp()))
	                                           fKaze=kENDEDtrack;
    break;
    
    case kCONTINUEtrack:
    case kIgnore:
    break;
    
    case kOUTtrack:
    case kENDEDtrack:
      fMy2Hist->Update();
      TVirtualMC::GetMC()->StopTrack();
      break;

    case kEXITtrack:
    {
      fVolHits = (StTGeoProxy::Instance()->IsSensitive(fVolume))? 1:0;
      if (fX0<=0) break;
      double dL = fCurrentLength-fEnterLength;
      fVolRadL = fabs(dL)/fX0;
      fTotRadL    += fVolRadL;
//??      assert(fTotRadL<100);
      modu = StTGeoProxy::Instance()->GetModu();   
      fModName = (modu)? modu->GetName(): "CAVE";
      fModName = Alias(fModName);
      fELossTrk[0]->Add(dL,fX0);
      fELossTrk[1]->Reset();;
      fELossTrk[1]->Add(dL,fX0);
      fTotOrt2 = fELossTrk[0]->GetOrt2();
      FillHist(1);

    }
    break;

    default:
     Error("Case","Unexpected case %x == %s",fCase,fCasName.Data());
     assert(0);
  }
  if (fKaze!=myKaze) goto SWITCH;

  return 0;
}		

//_____________________________________________________________________________
void StMCStepping2Hist::Finish(const char *)
{
  printf("\nMaxSensVolu: Rxy=%g aZ=%g\n\n",fSensMaxR,fSensMaxZ);


  fMy2Hist->Paint();
  fMy2Hist->Save();
  fMy2Hist->PrintFrom();
}

//_____________________________________________________________________________
void StMCStepping2Hist::FillHist(int flag)
{
   
   switch(flag) {
   
     case 0: break;
   
     case 1: 
     {
       double r1  = fEnterPosition.Perp();
       double r2  = fCurrentPosition.Perp();
       double eta = fCurrentMomentum.Eta();
       double phi = fCurrentMomentum.Phi()/M_PI*180; if(phi){}
       double z1  = fEnterPosition[2];
       double z2  = fCurrentPosition[2];

       fMy2Hist->SetEta(eta);
       My2Hist::Node_t &node = fMy2Hist->AddNode();
       node.name = fModName;
       node.len	 = fEnterLength;
       node.radL	=fVolRadL;
       node.z		=z2;
       node.ort2	=fTotOrt2;
       node.rxy		=r2;
       node.ihit	=fVolHits;
       fELossTrk[1]->GetCoef(node.mcs);
       fMy2Hist->Fill(r1,r2,z1,z2,fX0);
       break;
     }
  }

}
//_____________________________________________________________________________
const char *StMCStepping2Hist::Alias(const char *modu)
{
  const char *inp[] = {"IBSH","IBSG","IBSF","IBSE","IBSD"
                      ,"IBSC","IBCC","IBAC","IBSB","IBSA"
		      ,"IBEM"
		      ,"FTMO"
		      ,"FBOX","FBO1","FBO2",0};
  
  const char *out[] = {"VPDD","VPDD","VPDD","VPDD","VPDD"
                      ,"VPDD","VPDD","VPDD","VPDD","VPDD"
		      ,"VPDD"
		      ,"FTPC"
		      ,"FPDM","FPDM","FPDM",0};

  for (int i=0;inp[i];i++) {
    if (strcmp(modu,inp[i])==0) return out[i]; 
  }
  return modu;
}
