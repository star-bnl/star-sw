// $Id: StMCSteppingHist.cxx,v 1.3 2009/10/13 17:19:35 perev Exp $
//
//
// Class StMCSteppingHist
// ------------------
// Base class for Magnetic field calculation

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "StMCSteppingHist.h"
#include "TObjArray.h"
#include "TNamed.h"
#include "TH1F.h"

#include "TGeoManager.h"
#include "TGeoVolume.h"
#include "TGeoShape.h"
#include "TGeoBBox.h"
#include "TGeoTube.h"
#include "TProfile.h"
#include "TProfile2D.h"
#include "StTProfile2D.h"
#include "TStyle.h"
#include "TCanvas.h"
#include "TSystem.h"

StMCSteppingHist *StMCSteppingHist::fThis=0;
int Kount[10]={0,0,0,0,0,0,0,0,0,0};
double hMin[4] = {99,99,99,99};
double hMax[4] = {0};
class MyHolder : public TNamed
{
public:
   MyHolder(const char *name,const char *tit):TNamed(name,tit)
   {memset(&fEps,0,(char*)&fLast-(char*)&fEps);}

   double fEps;
   double fEpz;
   int    fEnt;
   int    fTot;
   double fS[2];
   double fSA[2];
   double fSR[2];
   
   TH1F  *fH[2];
   int    fLast;
};



ClassImp(StMCSteppingHist)

//_____________________________________________________________________________
StMCSteppingHist::StMCSteppingHist(const char *name,const char *tit)
  : StMCStepping(name,tit)
{
   gStyle->SetPalette(1);
   assert(!fThis);
   memset(&fFist,0,&fLast-&fFist);
   TString tsName(GetName());
   int yf = tsName.Contains(".DEV2");
   tsName = gSystem->BaseName(tsName.Data());
   tsName.ReplaceAll(".C","");
   tsName.ReplaceAll("Geometry.","");
   if (yf) tsName += "yf";
   fThis = this;
   fVols   = new TObjArray();  
   fMats   = new TObjArray();  
   const char *hNam[]=	{"Star_dRadL_1","Star_dRadL_2","Star_dRadL_3"
		        ,"Tpce_dRadL_1","Tpce_dRadL_2","Tpce_dRadL_3"
   			,"Star__RadL_1","Star__RadL_2","Star__RadL_3"
		        ,"Tpce__RadL_1","Tpce__RadL_2","Tpce__RadL_3",0};

   const char *hTit[]=	{"STAR dRadL(eta,rxy)"
                       	,"STAR dRadL(phi,rxy)"
		       	,"STAR dRadL(phi,eta)"
                       	,"TPC  dRadL(eta,rxy)"
                       	,"TPC  dRadL(phi,rxy)"
		       	,"TPC  dRadL(phi,eta)"
  			,"STAR  RadL(eta,rxy)"
                       	,"STAR  RadL(phi,rxy)"
		       	,"STAR  RadL(phi,eta)"
                       	,"TPC   RadL(eta,rxy)"
                       	,"TPC   RadL(phi,rxy)"
		       	,"TPC   RadL(phi,eta)",0};

const double hPar[][6]= {{60,  0, 3, 110,0,220}
			,{30,-15,15, 110,0,220}
			,{30,-15,15,  60,0,  3}
			,{60,  0, 3, 110,0,220}
			,{30,-15,15, 110,0,220}
			,{30,-15,15,  60,0,  3}
			,{60,  0, 3, 110,0,220}
			,{30,-15,15, 110,0,220}
			,{30,-15,15,  60,0,  3}
			,{60,  0, 3, 110,0,220}
			,{30,-15,15, 110,0,220}
			,{30,-15,15,  60,0,  3}};

   rStep = int(hPar[0][5]/hPar[0][3]+0.0001);
   
   for (int i=0;hNam[i];i++) {
     TString tsn(tsName); tsn+="_"; tsn+=hNam[i];
     TString tst(tsName); tst+=":"; tst+=hTit[i];
     
     mH[i] = new StTProfile2D(tsn.Data(),tst.Data()
                            ,(int)hPar[i][0],hPar[i][1],hPar[i][2]
			    ,(int)hPar[i][3],hPar[i][4],hPar[i][5]);
     mC[i] = new TCanvas(tsn.Data(),tst.Data(),600,800);
     mC[i]->Divide(1,3);
     mC[i]->cd(1); gPad->SetLogz();mH[i]->Draw("colZ");
   }
   


}   
//_____________________________________________________________________________
StMCSteppingHist::~StMCSteppingHist()
{
   fVols  ->Delete(); delete fVols;   fVols  =0;	
   fMats  ->Delete(); delete fMats;   fMats  =0;	
   fThis=0;
}
//_____________________________________________________________________________
void StMCSteppingHist::Print(const Option_t*) const
{
   fVols->ls();
   fMats->ls();
}		
//_____________________________________________________________________________
int StMCSteppingHist::Fun()
{
  Case();
  assert(fCurrentLength< 10000);
  assert(fEnterLength  < 10000);
  switch (fKaze) {
    case kNEWtrack:;
      FillVolMat();
      mRadL=0;
    case kENTERtrack:;
      TestTGeo();   
    break;
    
    case kCONTINUEtrack:
    case kOUTtrack:
    case kIgnore:
    break;
    
    case kEXITtrack:
    case kENDEDtrack:
    {
      SummArr();
      FillHist();
    }
    break;

    default:
     Error("Case","Unexpected case %x == %s",fCase,fCasName.Data());
     assert(0);
  }
  return 0;
}		
//_____________________________________________________________________________
void StMCSteppingHist::Fill(TObjArray *arr)
{
    
    MyHolder *mh = 0;
    int n = arr->GetSize();
    for (int idx=1;idx<n;idx++) {
      mh = (MyHolder*)arr->At(idx);
      if (!mh)		continue; 
      if (!mh->fS[0])	continue; 
      mh->fEnt++;
      for (int j=0;j<2;j++) {
         double val = mh->fS[j];
         mh->fS[j]=0;
         mh->fSA[j]+=val;
         mh->fSR[j]+=(val*val);
      }
    }
}               
//_____________________________________________________________________________
void StMCSteppingHist::Sort(TObjArray *arr)
{
  arr->Compress();
  arr->Sort();
  int n = arr->GetSize();
  for (int i=0;i<n-1; i++) {
    TNamed *t0=(TNamed*)arr->At(i+0);
    if (!t0) 			break;
    TNamed *t1=(TNamed*)arr->At(i+1);
    if (!t1) 			break;
    const char *n0 = t0->GetName();
    const char *n1 = t1->GetName();
    if (*n0 != *n1) 		continue;
    int l0 = strcspn(n0,"#");
    int l1 = strcspn(n1,"#");
    if (l0!=l1) 		continue;
    if (strncmp(n0,n1,l0))	continue;
    TString ts(n0);ts.Insert(l0,"_");
    t0->SetName(ts.Data());
  }
  arr->Sort();
}

//_____________________________________________________________________________
void StMCSteppingHist::SummArr()
{
   for (int iarr=0;iarr<2;iarr++) {
     TObjArray *arr = (&fVols)[iarr];

     int n = arr->GetSize();
     int id = ((iarr==0)? fVolume->GetNumber():fMaterial->GetIndex());
     MyHolder *mh=0;
     if (id < n) mh = (MyHolder*)arr->At(id);
     if (!mh) { 
       TString ts = (!iarr)? fVolume->GetName():fMedium->GetName();
       ts+="#"; ts+=id;
       mh = new MyHolder(ts.Data(),fMedium->GetName());
        for (int i=0;i<2;i++) {
          TH1F *h = new TH1F(ts.Data(), "", 100, 0., 1.);
#if  ROOT_VERSION_CODE < 395523
          h->SetBit(TH1::kCanRebin);
#endif
          h->SetDirectory(0);
          mh->fH[i] = h;
        }
       arr->AddAtAndExpand(mh,id);
     }
     mh->fTot++;
     double eps = fPrevEps+fMedium->GetParam(kEpsil);
     mh->fEps+=eps;
     double cl=fCurrentLength; 
     double el=fEnterLength; 
     double s  = cl-el;
     double sz = s*(cl*cl+cl*el+el*el);
     mh->fEpz += 3.*(cl*cl*fMedium->GetParam(kEpsil)+el*el*fPrevEps);
     mh->fS[0] += s;   
     mh->fS[1] += sz;   
   }//end of for
   fPrevEps = fMedium->GetParam(kEpsil);
}	

//_____________________________________________________________________________
void StMCSteppingHist::FillVolMat()
{
   Fill(fVols);
   Fill(fMats);
   fPrevEps=0;
}

//_____________________________________________________________________________
void StMCSteppingHist::Finish(const char *opt)
{
// *** MinMax[0] = 1.01188e-08 82.2315
// *** MinMax[1] = 1.01188e-08 1.38347
// *** MinMax[2] = 6.65619e-06 269.674
// *** MinMax[3] = 0.00645718 10.036
//  Print(opt);

  for (int i=0;i<4;i++) {
    printf (" *** MinMax[%d] = %g %g\n",i,hMin[i],hMax[i]);
  }

  double myMin[4]={1e-6,1e-6,5e-6,5e-3};
  double myMax[4]={1e+2,5e+0,1e+2,1e+1};
  TString ts;
  int idx;
  for (int i=0;mH[i];i++) {
    mH[i]->SetMinimum(myMin[i/3]);
    mH[i]->SetMaximum(myMax[i/3]);
    TProfile *hx = mH[i]->ProfileX();
    ts = hx->GetTitle();
    idx = ts.Index(",");
    ts.Replace(idx,4,"");
    hx->SetTitle(ts);
    TProfile *hy = mH[i]->ProfileY();
    ts = hy->GetTitle();
    idx = ts.Index(",");
    ts.Replace(idx-3,4,"");
    hy->SetTitle(ts);
    mC[i]->cd(2);hx->Draw("logy");
    mC[i]->cd(3);hy->Draw("logy");
    mC[i]->Modified(); mC[i]->Update();
  }

#if 0
  static const char* ext[]={".volu",".mate",0};
  FillVolMat();
  Sort(fVols);
  Sort(fMats);
  for (int iar=0;iar<2; iar++)  {
    TObjArray *arr = (&fVols)[iar];
    TString file(GetName()); file +="MCStep"; file += ext[iar];

    FILE *f = fopen(file.Data(),"w");

    int n = arr->GetSize();
    for (int i=0; i<n; i++) {
      MyHolder *mh = (MyHolder*)arr->At(i);
      if(!mh) 		continue;
      int nent = mh->fEnt;
      if (nent<10) 	continue;
      double aver =  mh->fSA[0]/nent;
      double rms  =  (mh->fSR[0]/nent-aver*aver);
      rms = sqrt(rms/(nent-1));
      double eps  = mh->fEps/nent;
      rms = rms + eps;
      double fak = (nent/fTrackNumber)*(4./3)*3.141592;
      eps  = mh->fEpz/nent;
      double sz  = mh->fSA[1]/nent;
      double sze = mh->fSR[1]/nent-sz*sz;
      sz  = (sz*fak);
      sze = (sqrt(sze/(nent-1))+eps)*fak;
      sz /= 1000.; sze /=1000.; 
      fprintf(f,"%20s.%s \tnent=%d \taver=%g +- %g \tsize= %g +- %g\n"
             ,mh->GetName(),GetName(),nent,aver,rms,sz,sze);
    }
  }
#endif //0
//  printf("Kount[0-2] = %d %d %d\n",Kount[0],Kount[1],Kount[2]);

  while(!gSystem->ProcessEvents()){}; 
  for (int i=0;mC[i];i++) {mC[i]->Print(".C");}
  for (int i=0;mC[i];i++) {mC[i]->Print(".png");}

}

//_____________________________________________________________________________
void StMCSteppingHist::TestTGeo()
{
#if 0
  double par[9];
  if (!gGeoManager) return;
  TGeoVolume *gv = gGeoManager->GetCurrentVolume();
  assert(gv);
  assert(fVolName == gv->GetName());
  if (fMatName != "TPCE_SENSITIVE_GAS") return;

  TGeoShape *sh = gv->GetShape();
  if (strcmp("TGeoTube",sh->GetName())== 0) {
  TGeoTube *tube = (TGeoTube *)sh;
     par[0] = tube->GetRmin();
     par[1] = tube->GetRmax();
     par[2] = tube->GetDz();}
  
  else if (strcmp("TGeoBBox",sh->GetName())== 0) {
  TGeoBBox *bbox = (TGeoBBox *)sh;
     par[0] = bbox->GetDX();
     par[1] = bbox->GetDY();
     par[2] = bbox->GetDZ();
   } else { return;}

  printf("TestTGeo: %s::%s %g %g %g\n",gv->GetName(),sh->GetName(),par[0],par[1],par[2]);
#endif //0
}
//_____________________________________________________________________________
void StMCSteppingHist::FillHist()
{
   int inTPC=0;
   for (int i=1;1;i++) {
     TGeoNode *n = gGeoManager->GetMother(i); 
     if(!n) 				break;
     TGeoVolume *v=n->GetVolume();
     if (!v) 				continue;
     if (strcmp(v->GetName(),"TPCE"))  	continue;
     inTPC=1; break;
   }
   
   double r1 = fEnterPosition.Perp();
   double r2 = fCurrentPosition.Perp();
   double eta = fCurrentMomentum.Eta();
   double phi = fCurrentMomentum.Phi()/M_PI*180;
//   printf("vol=%s.%s radL=%g\n",fVolume->GetName(),fMaterial->GetName(),fMaterial->GetRadLen());
   double radl = fMaterial->GetRadLen(); if (radl<=0) return;
   radl = fabs(fCurrentLength-fEnterLength)/radl;
   for (int iDet=0;iDet<=inTPC*3; iDet+=3) {
     double jl = r1,jr;
     while(1) {
       jr=int(jl+rStep); if (jr >r2) jr=r2;
       double dr = jr-jl; 
       double myRadLen = radl*dr/(r2-r1);
       assert(myRadLen>=0);
       mH[iDet+0]->Fill(eta,jr ,myRadLen);
       mH[iDet+1]->Fill(phi,jr ,myRadLen);
       mH[iDet+2]->Fill(phi,eta,myRadLen);

       mRadL += 0.5*myRadLen;
       mH[iDet+6]->Fill(eta,jr ,mRadL);
       mH[iDet+7]->Fill(phi,jr ,mRadL);
       mH[iDet+8]->Fill(phi,eta,mRadL);
       mRadL += 0.5*myRadLen;
       if (myRadLen > 1e-8) {
	 int jDet = iDet/3;
	 if (hMin[jDet]>myRadLen) hMin[jDet]=myRadLen;
	 if (hMax[jDet]<myRadLen) hMax[jDet]=myRadLen;
	 jDet +=2;
	 if (hMin[jDet]>mRadL) hMin[jDet]=mRadL;
	 if (hMax[jDet]<mRadL) hMax[jDet]=mRadL;
       }

       jl = jr; if (jl>=r2) break;
     }
   }
}






