// $Id: StMCSteppingHist.cxx,v 1.1.1.1 2004/07/17 20:02:55 perev Exp $
//
//
// Class StMCSteppingHist
// ------------------
// Base class for Magnetic field calculation

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "StMCSteppingHist.h"
#include "TObjArray.h"
#include "TNamed.h"
#include "TH1F.h"

#include "TGeoManager.h"
#include "TGeoVolume.h"
#include "TGeoShape.h"
#include "TGeoBBox.h"
#include "TGeoTube.h"

StMCSteppingHist *StMCSteppingHist::fThis=0;
int Kount[10]={0,0,0,0,0,0,0,0,0,0};

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
   assert(!fThis);
   memset(&fFist,0,&fLast-&fFist);
   fThis = this;
   fVols   = new TObjArray();  
   fMats   = new TObjArray();  
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
    }
    break;

    default:
     Error("Case","Unexpected case %x == %s",fCase,fCasName.Data());
     Assert(0);
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
     int id = ((iarr==0)? fVolId:fMatId);
     MyHolder *mh=0;
     if (id < n) mh = (MyHolder*)arr->At(id);
     if (!mh) { 
       TString ts = (!iarr)? fVolName:fMedName;
       ts+="#"; ts+=id;
       mh = new MyHolder(ts.Data(),fMedName.Data());
        for (int i=0;i<2;i++) {
          TH1F *h = new TH1F(ts.Data(), "", 100, 0., 1.);
          h->SetBit(TH1::kCanRebin);
          h->SetDirectory(0);
          mh->fH[i] = h;
        }
       arr->AddAtAndExpand(mh,id);
     }
     mh->fTot++;
     double eps = fPrevEps+fMedi.epsil;
     mh->fEps+=eps;
     double cl=fCurrentLength; 
     double el=fEnterLength; 
     double s  = cl-el;
     double sz = s*(cl*cl+cl*el+el*el);
     mh->fEpz += 3.*(cl*cl*fMedi.epsil+el*el*fPrevEps);
     mh->fS[0] += s;   
     mh->fS[1] += sz;   
   }//end of for
   fPrevEps = fMedi.epsil;
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
//  Print(opt);
  static const char* ext[]={".volu",".mate",0};
  fVolId=0; FillVolMat();
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

//  printf("Kount[0-2] = %d %d %d\n",Kount[0],Kount[1],Kount[2]);


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












		
