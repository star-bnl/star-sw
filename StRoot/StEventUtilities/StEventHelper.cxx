#include <stdio.h>
#include <stdlib.h>
#include "TROOT.h"
#include "TClass.h"
#include "TBaseClass.h"
#include "TDataMember.h"
#include "TMethod.h"
#include "TMethodArg.h"
#include "TDataType.h"
#include "Api.h"
#include "TMemberInspector.h"
#include "TExMap.h"
#include "TCollection.h"
#include "TRegexp.h"
#include "TRandom.h"
#include "TError.h"
#include "TPoints3DABC.h"
#include "TSystem.h"
#include "TPad.h"
#include "TView.h"

#include "StEvent.h"
#include "StHit.h"
#include "StTrack.h"
#include "StVertex.h"
#include "StTrackGeometry.h"
#include "StTrackDetectorInfo.h"
// For L3 filter
#include "StarClassLibrary/BetheBloch.h"
#include "StEvent/StDedxPidTraits.h"

#include "StEventHelper.h"

void Break(){printf("InBreak\n");}


class StEventInspector : public TMemberInspector {
public:
StEventInspector(TExMap *map,Int_t &count,const char *opt="");
virtual ~StEventInspector(){delete fSkip;};
virtual void Inspect(TClass* cl, const char* parent, const char* name, const void* addr);
        void CheckIn(TObject *obj,const char *bwname="");

Int_t &fCount;
TExMap *fMap;
TRegexp *fSkip;
TString fOpt;

};      

//______________________________________________________________________________
StEventInspector::StEventInspector(TExMap *map,Int_t &count,const char *opt):fCount(count)
{  
  fMap = map;
  fSkip = 0;
  fOpt = opt;
  if (fOpt.Length()) fSkip = new TRegexp(fOpt.Data());
}
//______________________________________________________________________________
void StEventInspector::Inspect(TClass* kl, const char* tit , const char* name, const void* addr)
{
  if(tit && strchr(tit,'.'))	return ;

  TString ts;

  if (!kl) return;
  if (name[0] == '*') name++;
  int ln = strcspn(name,"[ ");
  TString iname(name,ln);
  const char *iName=iname.Data();
  if (iName[1]=='P' && strcmp(iName,"fParent"       )==0) 	return;
  if (iName[0]=='G' && strcmp(iName,"G__virtualinfo")==0)	return;

  G__ClassInfo *classInfo = kl->GetClassInfo();  	
  if (!classInfo)		return;
  G__ClassInfo &cl = *classInfo;


// 		Browse data members
  G__DataMemberInfo m(cl);
  int found=0;
  const char *mName=0;
  while (m.Next()) {	// MemberLoop
     mName = m.Name();
     if (mName[1] != iName[1])	continue;
     if (strcmp(mName,iName) ) 	continue;
     found = 1; break;
  }     
  assert(found);

  // we skip: non TObjects
  //  - the member G__virtualinfo inserted by the CINT RTTI system

  long prop = m.Property() | m.Type()->Property();
  if (prop & G__BIT_ISFUNDAMENTAL) 	return;
  if (prop & G__BIT_ISSTATIC) 		return;
  if (prop & G__BIT_ISENUM) 		return;
  if (strcmp(m.Type()->Fullname(),"TObject") && !m.Type()->IsBase("TObject"))
  					return;

  int  size = sizeof(void*);
  if (!(prop&G__BIT_ISPOINTER)) size = m.Type()->Size(); 

  int nmax = 1;
  if (prop & G__BIT_ISARRAY) {
    for (int dim = 0; dim < m.ArrayDim(); dim++) nmax *= m.MaxIndex(dim);
  }

  for(int i=0; i<nmax; i++) {
    char *ptr = (char*)addr + i*size;
    TObject *obj = (prop&G__BIT_ISPOINTER) ? *((TObject**)ptr) : (TObject*)ptr;
    if (!obj) 		continue;
    const char *bwname = obj->ClassName();
    if (!bwname[0] || strcmp(bwname,obj->ClassName())==0) {
      bwname = name;
	int l = strcspn(bwname,"[ ");
	if (bwname[l]=='[') {
          char cbuf[12]; sprintf(cbuf,"[%02d]",i);
          ts.Replace(0,999,bwname,l);
          ts += cbuf;
          bwname = (const char*)ts;
      }  
    }  

    CheckIn(obj,bwname);

  }

}    
//______________________________________________________________________________
void StEventInspector::CheckIn(TObject *obj,const char *bwname)
{
  if (!obj) return;
  TObject *inobj=0;
  if (obj->InheritsFrom(StRefArray::Class())) return;  
  if (obj->InheritsFrom( StObjLink::Class())) return;  
  int n;
  if (fSkip && (fSkip->Index(obj->ClassName(),&n)>=0))	return;   

  if (obj->InheritsFrom(TCollection::Class())){
    TCollection *tcol = (TCollection*)obj;  
    TIter next(tcol);
    while ((inobj=next())) {CheckIn(inobj);}  
    return;
  }

  if (obj->InheritsFrom(StXRef::Class())){

     Long_t &inmap = (*fMap)(TMath::Hash(&obj,sizeof(void*)),(Long_t)obj);
     if (inmap) return;
     inmap = 1;fCount++;
  }

  if (obj->InheritsFrom(StStrArray::Class())){
//     if (obj->IsA()==StSPtrVecTrackNode::Class()) Break();
     Long_t &inmap = (*fMap)(TMath::Hash(&obj,sizeof(void*)),(Long_t)obj);
     if (inmap) return;
     inmap = 2; fCount++;
     int vecobj = ( obj->IsA() == StSPtrVecObject::Class());  
//     printf("%8d %p %s::%s\n",fLevel,obj,obj->GetName(),bwname);
     StStrArray *arr = (StStrArray*)obj; 
     int sz = arr->size();
     for (int idx=0;idx<sz; idx++) {
       inobj = arr->at(idx);
       Int_t count = fCount;
       CheckIn(inobj);
       if (count==fCount && !vecobj) break;  //if no action was made, no sense to continue
     }
     return;
  }

  char cbuf[1000];*cbuf=0;
  StEventInspector insp(fMap,fCount);
  obj->ShowMembers(insp,cbuf);
}
   


//______________________________________________________________________________
ClassImp(StEventHelper)
//______________________________________________________________________________
StEventHelper::StEventHelper(const TObject *evt,const char *opt)
{
   fMap = new TExMap();
   fObject = 0;
   Reset(evt,opt);
}
//______________________________________________________________________________
StEventHelper::~StEventHelper()
{
   delete fMap; fMap=0;
   Clear();
}
//______________________________________________________________________________
void StEventHelper::Clear(Option_t *opt)
{
}
//______________________________________________________________________________
void StEventHelper::Reset(const TObject *evt,const char *opt)
{
   if (fObject == evt) return;
   fObject = (TObject *)evt;
   Clear();
   fMap->Delete();
   if (!fObject) return;
   int kount=0;
   StEventInspector insp(fMap,kount,opt);
   char cbuf[1024];
   
   fObject->ShowMembers(insp,cbuf);
}
//______________________________________________________________________________
void StEventHelper::ls(Option_t* option) const
{
   typedef struct { int nb; int sz; const char *tenant; } QWE;
   QWE *qwe=0;

   TExMap map;
   TExMapIter  it(fMap); 
   Long_t key,val;
   while( it.Next(key,val) ) {
     if (val != 2) continue;
     StStrArray *a = (StStrArray *)key;
     Long_t &cnt = map((Long_t)a->IsA());
//     printf("%s %p\n",a->ClassName(),a);
     if (!cnt) {
       qwe = new QWE;
       cnt = (Long_t)qwe; 
       qwe->nb=0; qwe->sz=0;qwe->tenant=0;
     }
     qwe = (QWE*)cnt;
     qwe->nb++; qwe->sz += a->size();
     if (qwe->tenant==0 && a->size()) {
        TObject *to = a->front();
        if (to) qwe->tenant = to->ClassName();
     } 
 
   }
   TExMapIter  itt(&map);
   printf("\n      StEvent(%p)\n",fObject);

   while( itt.Next(key,val) ) {
     TObject *kl = (TObject *)key;
     qwe = (QWE*)val;
     printf ("%8d(%8d) - %s (%s)\n",qwe->nb,qwe->sz,kl->GetName(),qwe->tenant);
     delete qwe;
   }
   printf("\n");

}
//______________________________________________________________________________
TObjArray *StEventHelper::SelConts(const char *sel)
{
  TObjArray *tarr = new TObjArray;
  TRegexp reg(sel);

  TExMapIter  it(fMap); 
  Long_t key,val;
  while( it.Next(key,val) ) {
     if (val == 1) 	continue;
     StStrArray *a = (StStrArray *)key;
     if(a->size()==0)				continue;
     int n =0;
     if (reg.Index(a->ClassName(),&n)<0)	continue;   
     tarr->Add(a);
  }
  return tarr;   
}   
//______________________________________________________________________________
TObjArray *StEventHelper::SelTracks(Int_t th)
{
// 		th == needTrack + 2*needHit

  TObjArray *conts = SelConts("^StSPtrVecTrack$");
  TObjArray *traks = new TObjArray();
  Int_t ilast = conts->GetLast();
  for (int idx=0;idx<=ilast;idx++) {
    StObjArray *arr = (StObjArray *)conts->At(idx);
    if (!arr)		continue;
    if (!arr->size())	continue;
    StTrack *trk = (StTrack*)arr->front();
    if (!trk)	continue;
    if (trk->IsZombie()) continue;
    Assert(trk->InheritsFrom(StTrack::Class()));
    if (th&1) {
      StTrackPoints *trp = new StTrackPoints(trk);
      if (trp->IsZombie()) {delete trp; trk->makeZombie(); continue;}
      traks->Add(trp);
    }

    if (!(th&2)) 	continue;
    StTrackDetectorInfo *tdi = trk->detectorInfo();
    if (!tdi)		continue;
    StPtrVecHit *hits = &tdi->hits();
    if (!hits) 		continue;
    if (!hits->size())	continue;
    StHitPoints *hip = new StHitPoints(hits);
    traks->Add(hip);
  }         
  delete conts;
  return traks;
}
//______________________________________________________________________________
TObjArray *StEventHelper::SelHits(const char *RegEx, Int_t un)
{
//		un == used +2*nonused

  TObjArray *conts = SelConts(RegEx);
  TObjArray *hits = new TObjArray();
  Int_t ilast = conts->GetLast();
  
  for (int idx=0;idx<=ilast;idx++) {
    StObjArray *arr = (StObjArray *)conts->At(idx);
    if (!arr)		continue;
    int sz = arr->size();
    if (!sz)		continue;
    if (strcmp("StSPtrVecEmcRawHit",arr->ClassName())==0) continue; //VP Hack
    for(int ih=0;ih<sz; ih++) {
      StHit *hit = (StHit*)arr->at(ih);
      if (!hit) 	continue;
      Assert(hit->InheritsFrom(StHit::Class()));
      int used = (hit->trackReferenceCount()!=0);
      if (used==1 && (un&1)==0) continue;
      if (used==0 && (un&2)==0) continue;
      StHitPoints *hip = new StHitPoints(hit);
      hits->Add(hip);
    }
  }         
  delete conts;
  return hits;
}
//______________________________________________________________________________
TObjArray *StEventHelper::SelVertex(const char *sel,Int_t thFlag)
{
  char title[100],name[100];

  if (thFlag==0)		   thFlag  =1;
  
  StTrack *trk; StTrackPoints *trp;StVertexPoints *vxp;

  TObjArray *conts = SelConts(sel);
  TObjArray *traks = new TObjArray();
  Int_t ilast = conts->GetLast();
  int nvtx =0;
  for (int idx=0;idx<=ilast;idx++) {
    StObjArray *arr = (StObjArray *)conts->At(idx);
    if (!arr)	continue;
    int sz = arr->size();
    if (!sz)	continue;
    for (int ivx=0; ivx<sz; ivx++) {
      StVertex *vx = (StVertex*)arr->at(ivx);
      if (!vx) 	continue;
      nvtx++;
      vxp = new StVertexPoints(vx);
      trk = vx->parent();
      if (trk ) {
        Assert(trk->InheritsFrom(StTrack::Class()));
         
        

        strcpy(name,trk->ClassName());
        sprintf(title,"parent%d",nvtx);
        trp = new StTrackPoints(trk,name,title);
        traks->Add(trp);}
        
      int nd = vx->numberOfDaughters();
      for (int id=0;id<nd;id++) {
        trk = vx->daughter(id);
        if (!trk)	continue;
        Assert(trk->InheritsFrom(StTrack::Class()));
        strcpy(name,trk->ClassName());
        sprintf(title,"daughter%d",id);
        if (thFlag&1) { 
          trp = new StTrackPoints(trk,name,title);
          traks->Add(trp);
        }
        if (thFlag&2) { 
          StTrackDetectorInfo *tdi = trk->detectorInfo();
          if (!tdi)		continue;
          StPtrVecHit *hits = &tdi->hits();
          if (!hits) 		continue;
          if (!hits->size())	continue;
          StHitPoints *hip = new StHitPoints(hits);
          traks->Add(hip);
        }
      }
    }
  }         
  delete conts;
  return traks;
}

//______________________________________________________________________________
ClassImp(StPoints3DABC)
void StPoints3DABC::Add(StPoints3DABC *add)
{
  int n = add->fSize + fSize;
  if (n > fN) {
    if (n < fN*2) n = fN*2;
    Float_t *arr = new Float_t[n*3];
    memcpy(arr, fXYZ, fSize*3*sizeof(Float_t));
    delete [] fXYZ; fXYZ = arr; fN = n;
  }
  memcpy(fXYZ+fSize*3,add->fXYZ,add->fSize*3*sizeof(Float_t));
  fSize+=add->fSize;
}
//______________________________________________________________________________
ClassImp(StTrackPoints)
//______________________________________________________________________________
StTrackPoints::StTrackPoints(StTrack *st,const char *name,const char *title)
:StPoints3DABC(name,title,st),fTrack((StTrack*&)fObj)
{
  Init();
}
//______________________________________________________________________________
void StTrackPoints::Init() 
{  
  if (fXYZ) return;
  float len = fTrack->length();   
  if (len <= 0.0001) {
    Warning("Init","Zero length %s(%p), IGNORED",fObj->ClassName(),fObj);
    MakeZombie();
    return;
  }
  
  StTrackGeometry *geo = fTrack->geometry(); 
  Assert(geo);
  double curva = fabs(geo->curvature());
  double dip   = geo->dipAngle();
  fSize = abs(int(len*cos(dip)*curva*20))+2;   
  fN = fSize;
  fXYZ = new Float_t[fN*3];
  StPhysicalHelixD hel = geo->helix();
  Double_t step = len/(fSize-1);
  Double_t ss = 0;
  
  for (int i =0;i<fSize;i++,ss+=step)
  {
     fXYZ[i*3+0] = hel.x(ss); 
     fXYZ[i*3+1] = hel.y(ss); 
     fXYZ[i*3+2] = hel.z(ss); 
  }
}
//______________________________________________________________________________
 Int_t StTrackPoints::DistancetoPrimitive(Int_t px, Int_t py)
{
//*-*-*-*-*-*-*Compute distance from POINT px,py to a 3-D points *-*-*-*-*-*-*
//*-*          =====================================================
//*-*
//*-*  Compute the closest distance of approach from POINT px,py to each SEGMENT
//*-*  of the polyline.
//*-*  Returns when the distance found is below DistanceMaximum.
//*-*  The distance is computed in pixels units.
//*-*
//*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

   enum {inaxis = 7,mindist=10};
   Float_t dist = 999999;

   Int_t puxmin = gPad->XtoAbsPixel(gPad->GetUxmin());
   Int_t puymin = gPad->YtoAbsPixel(gPad->GetUymin());
   Int_t puxmax = gPad->XtoAbsPixel(gPad->GetUxmax());
   Int_t puymax = gPad->YtoAbsPixel(gPad->GetUymax());

   TView *view = 0;
//*-*- return if POINT is not in the user area
   if (px < puxmin - inaxis) goto END;
   if (py > puymin + inaxis) goto END;
   if (px > puxmax + inaxis) goto END;
   if (py < puymax - inaxis) goto END;

   view = gPad->GetView();
   if (!view) 						goto END;
   
   {Int_t i;
   Float_t /*dpoint,*/alfa;
   Float_t xndc[3];
   Int_t x1,y1,x0,y0;
   Int_t pointSize = fN*3;
   view->WCtoNDC(fXYZ, xndc);
   x0 = gPad->XtoAbsPixel(xndc[0]);
   y0 = gPad->YtoAbsPixel(xndc[1]);

   float dif[2],difdif,cur[2],curcur,difcur;
   for (i=3;i<pointSize;i+=3) {
      view->WCtoNDC(fXYZ+i, xndc);
      x1     = gPad->XtoAbsPixel(xndc[0]);
      y1     = gPad->YtoAbsPixel(xndc[1]);
      dif[0] = x1-x0; dif[1]=y1-y0;
      cur[0] = x0-px; cur[1]=y0-py;
      difdif = (dif[0]*dif[0]+dif[1]*dif[1]);
      difcur = (dif[0]*cur[0]+dif[1]*cur[1]);
      curcur = cur[0]*cur[0]+cur[1]*cur[1];
      if (difdif<mindist*mindist) {
	if ((i+3)<pointSize) 				continue;
	dist = curcur; 					break;
      }
      alfa = -difcur/difdif;

      if (alfa<0.) {dist =  curcur; 			break;}

      x0=x1; y0=y1;
      if (alfa > 1.) {
	if (i+3 < pointSize) 				continue; 
        dist = (px-x1)*(px-x1) + (py-y1)*(py-y1);	break;
      }
      dist = curcur+alfa*(2*difcur+difdif*alfa);
       						      	break;
   }}
END:
   dist =  TMath::Sqrt(dist);
   if (dist <= mindist) { dist = 0; gPad->SetSelected(fObj);}

   return Int_t(dist);
}

  
//______________________________________________________________________________
ClassImp(StVertexPoints)
//______________________________________________________________________________
StVertexPoints::StVertexPoints(StVertex *sv,const char *name,const char *title)
:StPoints3DABC(name,title,sv),fVertex((StVertex*&)fObj)
{
  fVertex = sv;
  fSize = 1; fN =1;
  fXYZ = new Float_t[3];
  fXYZ[0] = fVertex->position().x(); 
  fXYZ[1] = fVertex->position().y(); 
  fXYZ[2] = fVertex->position().z(); 
}
   
   
ClassImp(StHitPoints)
//______________________________________________________________________________
StHitPoints::StHitPoints(StHit *sh,const char *name,const char *title)
:StPoints3DABC(name,title,sh)
{
  fSize = 1; fN =1;
  fObj = sh;
  Init();
}
//______________________________________________________________________________
StHitPoints::StHitPoints(StRefArray *ar,const char *name,const char *title)
:StPoints3DABC(name,title,ar)
{
  fSize = ar->size();
  fN = fSize;
  if (!fSize) return;
  fObj = (fSize==1) ? ar->front() : ar;
  Init();
}
//______________________________________________________________________________
void StHitPoints::Init() 
{  
  if (fXYZ) return;
  fXYZ = new Float_t[fN*3];
  
  for (int i =0;i<fSize;i++)
  {
     StHit *hit= (fSize==1) ? (StHit*)fObj: (StHit*)((StRefArray*)fObj)->at(i);
     StThreeVectorF v3 = hit->position();
     fXYZ[i*3+0] = v3.x(); 
     fXYZ[i*3+1] = v3.y(); 
     fXYZ[i*3+2] = v3.z(); 
  }
}
   
//______________________________________________________________________________
ClassImp(StFilterABC)

int StFilterABC::fgDial=0;
//______________________________________________________________________________
StFilterABC::StFilterABC(const char *name,bool active):TNamed(name,""),fActive(active)
{
#ifdef OLDDISPLAY
   char cbuf[200];
   sprintf(cbuf,"__StEventControlPanel__.AddFilter((TObject*)%p);",this);
   gROOT->ProcessLine(cbuf);
#endif
}
//______________________________________________________________________________
void StFilterABC::SetDefs()
{
  for (int i=0; GetNams()[i]; i++) {GetPars()[i]=GetDefs()[i];}
}
//______________________________________________________________________________
void   StFilterABC::Update()
{
#ifdef OLDDISPLAY
   char cbuf[200];
   float       *pars    = GetPars();
   const float *defs    = GetDefs();
   const char **namval  = GetNams();
   int flagg = 2001;
   if (!fgDial++) gROOT->LoadMacro("FilterDialog.C");
   sprintf(cbuf
  ,"new FilterDialog((char*)%p,(char**)%p,(float*)%p,(float*)%p,(int*)%p);"
                    ,GetName(),    namval,      defs,      pars,  &flagg);
   printf("%s\n",cbuf);
   void *dial = (void*)gROOT->ProcessLineFast(cbuf);
   printf("StFilterABC::Update() Waiting for update\n");
   while(flagg) {gSystem->DispatchOneEvent(1);}
   sprintf(cbuf,"delete ((FilterDialog*)%p);",dial);
   printf("StFilterABC::Update: %s\n",cbuf);
   gROOT->ProcessLine(cbuf);
   printf("StFilterABC::Update() update finished\n");
#endif
}
   
//______________________________________________________________________________
ClassImp(StFilterDef)
StFilterDef::StFilterDef(const char *name,bool active):StFilterABC(name,active)
{
  SetDefs();
  
}
//______________________________________________________________________________
const char  **StFilterDef::GetNams() const
{
  static const char *nams[] = {
   "  RandomSelect ",
   "  RxyMin       ",
   "  RxyMax       ",
   "  ZMin         ",
   "  ZMax         ",
   "  PhiMin       ",
   "  PhiMax       ",
   "  LenMin       ",
   "  LenMax       ",
   "  PtMin        ",
   "  PtMax        ",
   "  QMin         ",
   "  QMax         ", 
   "  EncodedMethod",
   0};
  return nams;
}
//______________________________________________________________________________
const float  *StFilterDef::GetDefs() const
{
  static const float defs[] = {
   /*  RandomSelect=*/    1.00,
   /*  RxyMin      =*/    0.00,
   /*  RxyMax      =*/  900.00,
   /*  ZMin        =*/ -900.00,
   /*  ZMax        =*/ +900.00,
   /*  PhiMin      =*/ -180.01,
   /*  PhiMax      =*/ +181.01,
   /*  LenMin      =*/   +0.00,
   /*  LenMax      =*/ +999.00,
   /*  PtMin       =*/    0.00,
   /*  PtMax       =*/  999.00,
   /*  QMin        =*/   -1   ,
   /*  QMax        =*/   +1   ,
   /* Encoded method*/   -1   ,     // The default value -1 menas all

   0};
  return defs;   
}   
//______________________________________________________________________________
Int_t StFilterDef::Accept(StPoints3DABC *pnt) 
{
   static TRandom rrr;
   float x,y,z,r2xy,phid,len,pt,q;
   TObject *to;
   StTrack *trk;
   
   int cut = 1;
   if (fRandomSelect < 1. && fRandomSelect < rrr.Rndm())return 0;


   z = pnt->GetZ(0);				
   cut++;
   if (fZMin >z || z > fZMax)				goto SKIP;	

   x = pnt->GetX(0);
   y = pnt->GetY(0);
   r2xy = x*x+y*y;
   cut++;
   if (fRxyMin*fRxyMin > r2xy || r2xy > fRxyMax*fRxyMax)goto SKIP;
   phid = atan2(y,x)*(180./M_PI);
   cut++;
   if (fPhiMin > phid || phid > fPhiMax) 		goto SKIP;
   to = pnt->GetObject();
   if (!to) 						return 1;
   if (!to->InheritsFrom(StTrack::Class()))		return 1;
   trk = (StTrack*)to;
   len = trk->length();
   cut++;
   if (fLenMin >len || len > fLenMax)			goto SKIP;	
   pt = trk->geometry()->momentum().perp();
   cut++;
   if (fPtMin >pt || pt > fPtMax)			goto SKIP;	
   q = trk->geometry()->charge();
   cut++;
   if (fQMin >q || q > fQMax)				goto SKIP;	
   cut++;
   if ( (int(fEncodedMethod) != -1) && (trk->encodedMethod() != int(fEncodedMethod)) )
      goto SKIP;
   return 1;   

SKIP: return 0;

}


//______________________________________________________________________________
ClassImp(StMuDstFilterHelper)
StMuDstFilterHelper::StMuDstFilterHelper(const char *name,bool active):StFilterABC(name,active)
{
  mBB = new BetheBloch();
  SetDefs();
  
}
//______________________________________________________________________________
StMuDstFilterHelper::~StMuDstFilterHelper()
{ delete mBB;}
//______________________________________________________________________________
const char  **StMuDstFilterHelper::GetNams() const
{
  static const char *nams[] = {
    " pCutHigh            ",  
    " nHitsCutHighP       ",
    " pCutLow             ",
    " nHitsCutLowP        ",    
    " chargeForLowP       ",
    " dEdxMassCutHigh     ",
    " dEdxFractionCutHigh ",
    " dEdxMassCutLow      ",
    " dEdxFractionCutLow  ",
    0
  };
  return nams;
}
//______________________________________________________________________________
const float  *StMuDstFilterHelper::GetDefs() const
{
  static const float defs[] = {
    /* pCutHigh            */ 2.0,    // high momentum cut for RICH/Upsilon candidates 
    /* nHitsCutHighP       */ 10,     // nHits cut for all tracks
    /* pCutLow             */ 0.2,    // low momentum cut
    /* nHitsCutLowP        */ 15,    
    /* chargeForLowP       */ -1,     // charge for tracks with pCutLow < p < pCutHigh, set to 0 for all tracks
    /* dEdxMassCutHigh     */ 0.939,  // cut below BetheBloch(p/dEdxMassCutHigh), e.g. proton-band
    /* dEdxFractionCutHigh */ 0.6,    // cut fraction of dEdx-band, i.e. dEdxFractionCut * BetheBloch(p/dEdxMassCut)
    /* dEdxMassCutLow      */ 0.494,  // cut above BetheBloch(p/dEdxMassCutLow), e.g. kaon-band
    /* dEdxFractionCutLow  */ 1.1,
    0
  };
  return defs;
}
//______________________________________________________________________________
Int_t StMuDstFilterHelper::Accept(const StTrack* track) {
  
  float pCutHigh        = fpCutHigh;    // high momentum cut for RICH/Upsilon candidates 
  int   nHitsCutHighP   = int(fnHitsCutHighP);     // nHits cut for all tracks

  // following cuts apply only for tracks with pCutLow < p <pHigh
  float pCutLow             = fpCutLow;            // low momentum cut
  int   nHitsCutLowP        = int(fnHitsCutLowP);    
  int   chargeForLowP       = int(fchargeForLowP); // charge for tracks with pCutLow < p < fpCutHigh, set to 0 for all tracks
  float dEdxMassCutHigh     = fdEdxMassCutHigh;    // cut below BetheBloch(p/dEdxMassCutHigh), e.g. proton-band
  float dEdxFractionCutHigh = fdEdxFractionCutHigh;// cut fraction of dEdx-band, i.e. dEdxFractionCut * BetheBloch(p/dEdxMassCut)
  float dEdxMassCutLow      = fdEdxMassCutLow;     // cut above BetheBloch(p/dEdxMassCutLow), e.g. kaon-band
  float dEdxFractionCutLow  = fdEdxFractionCutLow;

  int iret = 0;
  int chargeOK = 0;
  int dedxOK = 0;

  float magnitude = track->geometry()->momentum().magnitude();
  int   nPoints   = track->detectorInfo()->numberOfPoints();

  if  (   magnitude > pCutHigh && nPoints >= nHitsCutHighP)   iret = 1;
  else {
     if ( magnitude > pCutLow  && nPoints >= nHitsCutLowP ) 
     {
        // check charge
        if (chargeForLowP==0) 
           chargeOK = 1;
        else if (track->geometry()->charge() == chargeForLowP) 
           chargeOK = 1;

        // check dEdx
        //	      if (mBB==0) mBB = new BetheBloch();
        float dedxHigh = dEdxFractionCutHigh * mBB->Sirrf(magnitude/dEdxMassCutHigh);
        float dedxLow  = dEdxFractionCutLow  * mBB->Sirrf(magnitude/dEdxMassCutLow);
        float dedx     = 0;

        // get track dEdx
        const StSPtrVecTrackPidTraits& traits = track->pidTraits();
        StDedxPidTraits* dedxPidTr;
        for (unsigned int itrait = 0; itrait < traits.size(); itrait++){
           dedxPidTr = 0;
           if (traits[itrait]->detector() == kTpcId) {
              StTrackPidTraits* thisTrait = traits[itrait];
              dedxPidTr = dynamic_cast<StDedxPidTraits*>(thisTrait);
              if (dedxPidTr && dedxPidTr->method() == kTruncatedMeanId) {
                 // adjust L3 dE/dx by a factor of 2 to match offline
                 dedx = 2 * dedxPidTr->mean();
              }
           }
        }
        if (dedx > dedxHigh && dedx > dedxLow) 
           dedxOK = 1;
        // final answer
        iret = chargeOK * dedxOK;
     } // if (pCutLow && nHitsCutLowP)
  }
  return iret;
}

//______________________________________________________________________________
Int_t StMuDstFilterHelper::Accept(StPoints3DABC *pnt) 
{
   TObject *to;
   StTrack *trk;
   to = pnt->GetObject();
   if (!to) 						return 1;
   if (!to->InheritsFrom(StTrack::Class()))		return 1;
   trk = (StTrack*)to;
   return Accept(trk);
}
