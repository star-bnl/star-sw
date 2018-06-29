#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "TROOT.h"
#include "TClass.h"
#if ROOT_VERSION_CODE < 331013
#include "TCL.h"
#else
#include "TCernLib.h"
#endif
#include "TMath.h"
#include "TBaseClass.h"
#include "TDataMember.h"
#include "TMethod.h"
#include "TMethodArg.h"
#include "TDataType.h"
#if ROOT_VERSION_CODE < ROOT_VERSION(5,34,29)
#include "Api.h"
#endif
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
#include "StObject.h"
#include "StHit.h"
#include "StTrack.h"
#include "StGlobalTrack.h"
#include "StTrackNode.h"
#include "StVertex.h"
#include "StTrackGeometry.h"
#include "StTrackDetectorInfo.h"
// For L3 filter
#if 0
#include "StarClassLibrary/BetheBloch.h"
#else
#include "StBichsel/Bichsel.h"
#endif 
#include "StEvent/StDedxPidTraits.h"
// For coloring filter
#include "StEventTypes.h"
#include "StProbPidTraits.h"
#include "StTpcDedxPidAlgorithm.h"
#include "THelixTrack.h"

#define  __EVENTHELPER_ONLY__
#include "StEventHelper.h"
#include "StEventHitIter.h"
#undef   __EVENTHELPER_ONLY__
#include <map>

#include "StRnDHitCollection.h"
#include "StEtrHitCollection.h"

void Break(){printf("InBreak\n");}

std::map<long,long>  myMap;
typedef std::pair <long,long> MyPair;
std::map <long,long> :: const_iterator myFinder;

#if ROOT_VERSION_CODE >= ROOT_VERSION(5,34,29)
typedef TMemberInspector StEventInspector;
#else

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

  G__ClassInfo *classInfo = (G__ClassInfo *)kl->GetClassInfo();  	
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
     LongKey_t &inmap = (*fMap)(TMath::Hash(&obj,sizeof(void*)),(Long_t)obj);
     myFinder = myMap.find((long)obj);
     assert((inmap==0) == (myFinder==myMap.end()));

     if (inmap) return;
     myMap.insert(MyPair((long)obj,1));     
     inmap = 1;fCount++;
  }

  if (obj->InheritsFrom(StStrArray::Class())){
//     if (obj->IsA()==StSPtrVecTrackNode::Class()) Break();
    if (((StStrArray*)obj)->size()) {


       LongKey_t &inmap = (*fMap)(TMath::Hash(&obj,sizeof(void*)),(Long_t)obj);
       myFinder = myMap.find((long)obj);
       assert((inmap==0) == (myFinder==myMap.end()));
       if (inmap) return;
       myMap.insert(MyPair((long)obj,2));     

       inmap = 2; fCount++;
       int vecobj = ( obj->IsA() == StSPtrVecObject::Class());  
  //     printf("%8d %p %s::%s\n",fLevel,(void*)obj,obj->GetName(),bwname);
       StStrArray *arr = (StStrArray*)obj; 
       int sz = arr->size();
       for (int idx=0;idx<sz; idx++) {
	 inobj = arr->at(idx);
	 Int_t count = fCount;
	 CheckIn(inobj);
	 if (count==fCount && !vecobj) break;  //if no action was made, no sense to continue
       }
       return;
  }  }
  StEventInspector insp(fMap,fCount);
#if ROOT_VERSION_CODE < 334597
  char cbuf[1000];*cbuf=0;
  obj->ShowMembers(insp,cbuf);
#else
  obj->ShowMembers(insp);
#endif
}
#endif /* ROOT_VERSION_CODE >= ROOT_VERSION(5,34,29) */
//______________________________________________________________________________
ClassImp(StEventHelper)
//______________________________________________________________________________
StEventHelper::StEventHelper(const TObject *evt,const char *opt)
{
   fMap = new TExMap(10000);
   myMap.clear();
   fObject = 0;
   Reset(evt,opt);
}
//______________________________________________________________________________
StEventHelper::~StEventHelper()
{
   myMap.clear();
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
   fObject = (TObject *)evt;
   Clear();
   myMap.clear();
   fMap->Delete();
#if ROOT_VERSION_CODE < ROOT_VERSION(5,34,29)
   if (!fObject) return;
   int kount=0;
   StEventInspector insp(fMap,kount,opt);
#if ROOT_VERSION_CODE < 334597
   char cbuf[1024];
   fObject->ShowMembers(insp,cbuf);
#else
   fObject->ShowMembers(insp);
#endif
#endif
}
//______________________________________________________________________________
int StEventHelper::Kind(const TObject *to)
{
  static TClass *klass=0;
  static int who=0;
  int kind = 0;
  TClass *myClass=to->IsA(); 
  if (myClass!=klass) {
     klass = myClass; who = 0;
     if      (klass->InheritsFrom(      StHit::Class())) { who=kHIT;}
     else if (klass->InheritsFrom(    StTrack::Class())) { who=kTRK;}
     else if (klass->InheritsFrom(StPtrVecHit::Class())) { who=kHRR;}
     else if (klass->InheritsFrom(   StVertex::Class())) { who=kVTX;}
     else if (klass->InheritsFrom(  TObjArray::Class())) { who=kTRR;}
  }
  kind = who;
  if (kind==kHIT) {
    StHitHelper hh((StHit*)to);
    if (hh.IsUsed()) {kind|=kUSE;} else {kind|=kUNU;}
    if (hh.IsFit ())  kind|=kFIT;
    return kind;
  }
  return kind;
}
		
//______________________________________________________________________________
void StEventHelper::ls(Option_t* option) const
{
   typedef struct { int nb; int sz; const char *tenant; } QWE;
   QWE *qwe=0;

   TExMap map;
   TExMapIter  it(fMap); 
   LongKey_t key,val;
   while( it.Next(key,val) ) {
     if (val != 2) continue;
     StStrArray *a = (StStrArray *)(key);
     LongKey_t &cnt = map((Long_t)a->IsA());
//     printf("%s %p\n",a->ClassName(),(void*)a);
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
   printf("\n      StEvent(%p)\n",(void*)fObject);

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
   LongKey_t key,val;
  while( it.Next(key,val) ) {
     if (val == 1) 	continue;
     StStrArray *a = (StStrArray *)(key);
     if(a->size()==0)				continue;
     int n =0;
     if (reg.Index(a->ClassName(),&n)<0)	continue;   
     tarr->Add(a);
  }
  return tarr;   
}   
//______________________________________________________________________________
TObjArray *StEventHelper::SelTracks(const char*,int flag)
{
int trackTypes[]= {global, primary, tpt, secondary, estGlobal, estPrimary,-1};    

  TObjArray *conts = SelConts("^StSPtrVecTrackNode$");
  TObjArray *traks = new TObjArray();
  Int_t ilast = conts->GetLast();
  for (int idx=0;idx<=ilast;idx++) {
    StObjArray *arr = (StObjArray *)conts->At(idx);
    if (!arr)			continue;
    int ntrk = arr->size();
    if (!ntrk)			continue;
    for (int itrk=0;itrk<ntrk;itrk++) {
      StTrackNode *tn = (StTrackNode*)arr->at(itrk);
      if (!tn) 			continue;
      StTrack *trk = 0;int ity; int bty=kTGB;
      for (int jty=0;(ity=trackTypes[jty])>=0;jty++,bty<<=1){
        if (!(flag&bty))	continue;
        trk=(StTrack *)tn->track(ity);
	if (!trk)		continue;
	if (trk->IsZombie())  	continue;
   // See:  StRoot/St_base/StObject.h also
	if ((flag&kMark2Draw) && !trk->TestBit(kMark2Draw)) continue;
	traks->Add(trk);	
      }//end track types
    }//end StTrackNode's
  }// end  StSPtrVecTrackNode's        
  delete conts;
  return traks;
}
//______________________________________________________________________________
TObjArray *StEventHelper::SelHits(const char *RegEx, Int_t un, Int_t flag)
{
//		un == used +2*nonused

  TObjArray *conts = SelConts(RegEx);
  TObjArray *hits  = new TObjArray();
  Int_t ilast = conts->GetLast();
  
  for (int idx=0;idx<=ilast;idx++) {
    StObjArray *arr = (StObjArray *)conts->At(idx);
    if (!arr)		continue;
    int sz = arr->size();
    if (!sz)		continue;
    if (!arr->at(0)->InheritsFrom(StHit::Class())) continue;
    for(int ih=0;ih<sz; ih++) {
      StHit *hit = (StHit*)arr->at(ih);
      if (!hit) 		continue;
      if (hit->IsZombie())	continue;
      // See:  StRoot/St_base/StObject.h also
      if ((flag&kMark2Draw) && !hit->TestBit(kMark2Draw)) continue;
      int used = (hit->trackReferenceCount()!=0);
      int take = 0;
      if ( used && (un&kUSE)) take++;
      if (!used && (un&kUNU)) take++;
      if (take) hits->Add(hit);
    }
  }         
  delete conts;
  return hits;
}
//______________________________________________________________________________
TObjArray *StEventHelper::SelVertex(const char *sel,Int_t flag)
{
  
  TObjArray *conts = SelConts(sel);
  TObjArray *verts = new TObjArray();
  Int_t ilast = conts->GetLast();
  int nvtx =0;
  for (int idx=0;idx<=ilast;idx++) {
    StObjArray *arr = (StObjArray *)conts->At(idx);
    if (!arr)	continue;
    int sz = arr->size();
    if (!sz)	continue;
    for (int ivx=0; ivx<sz; ivx++) {
      StVertex *vx = (StVertex*)arr->at(ivx);
      if (!vx) 			continue;
      if (vx->IsZombie())	continue;
      // See:  StRoot/St_base/StObject.h also
      if ((flag&kMark2Draw) && !vx->TestBit(kMark2Draw)) continue;
      verts->Add(vx);nvtx++;
    }
  }
  delete conts;
  return verts;
}
//______________________________________________________________________________
TObjArray *StEventHelper::ExpandAndFilter(const TObject *eObj, int flag, TObjArray *out)
{
  // eobj - TObject of StEvent objects (StHit,StTrack,StVertex or TObjArray of above)
  if (!out) {out = new TObjArray;}
  TObject *eobj = (TObject*)eObj;
  
  int kind = Kind(eobj);
  if (kind&kHIT) {// input StHit
    if (!(flag&kHIT)) return out;
    int take=kind & (kUSE|kUNU|kFIT) &flag;
    if (take) out->Add(eobj);
    return out;
  }//endif  StHit

//-------------------------------------------------------------
  if (kind&kTRK) {// input StTrack
    if (flag&kTRK)  out->Add(eobj);
    if (!(flag&kHRR)) return out; 
    StTrack *trk = (StTrack*)eobj;
    StTrackHelper trkh(trk);
    out->Add((TObject*)trkh.GetHits());
    return out;
  }//endif StTrack

//-------------------------------------------------------------
  if (kind&kVTX) {// input StVertex
    if (flag&kVTX) out->Add(eobj);
    StVertex *vtx = (StVertex*)eobj;
    if (!(flag&(kTRK|kHRR))) return out;
    StVertexHelper  vtxh(vtx);
    int n = vtxh.GetNTracks();
    for (int i=-1;i<n;i++) {
      const TObject *to = vtxh.GetTrack(i);
      if (!to) continue;
      ExpandAndFilter(to,flag,out);
    }
    return out;
  }//endif StVertex
//-------------------------------------------------------------

  if (kind&kTRR) {  // input TObjArray
    TObjArray *inp = (TObjArray *)eobj;
    inp->Compress();
    int nbjs = inp->GetLast()+1;   
    if (!nbjs)	return 0;
    for (int i=0;i<nbjs;i++) {
      ExpandAndFilter(inp->At(i),flag,out);
    }
    return out;
  }// endif TObjArray

//-------------------------------------------------------------
  if (kind&kHRR) {  // input HitsArray
    if (!(flag&kHRR)) return out;
    out->Add(eobj);
    return out;
  }// endif HitsArray
  return 0;
}	
//______________________________________________________________________________
TObjArray *StEventHelper::MakePoints(TObjArray *inp, int flag)
{
static const Color_t plitra[]={kRed,kGreen,kBlue,kMagenta, kCyan};
static const int     nlitra  = sizeof(plitra)/sizeof(Color_t);
             int     ilitra=0;
  inp->Compress();
  int nbjs = inp->GetLast()+1;   
  if (!nbjs)	return 0;
  TObjArray *out = new TObjArray;out->SetOwner();
  StPoints3DABC  *p[3] ; int np=0;
  for (int i=0;i<nbjs;i++) {
    TObject *to = inp->At(i);
    int kind = Kind(to);
    if (!(kind&kHRR)) { ilitra++; ilitra = ilitra%nlitra; }
    int take = (kind&flag);
    if (!take) 	continue;
//??    if (take&kHIT) take -=kHIT;
    if (!take) 	continue;

    np = 0;
    if      (kind&kHIT) {np=1;p[0] = new StHitPoints   ((StHit      *)to  );}
    else if (kind&kHRR && ((StPtrVecHit*)to)->size()) 
	                {np=1;p[0] = new StHitPoints   ((StPtrVecHit*)to  );}

    else if (kind&kTRK) {np=3;p[0] = new StTrackPoints ((StTrack    *)to  );
                              p[1] = new StInnOutPoints((StTrack    *)to,0);
                              p[2] = new StInnOutPoints((StTrack    *)to,1);}

    else if (kind&kVTX) {np=1;p[0] = new StVertexPoints((StVertex   *)to  );}

    for (int j=0;j<np;j++){p[j]->SetUniqueID(plitra[ilitra]); out->Add(p[j]);}
  }// 

  return out;
}	
//______________________________________________________________________________
void StEventHelper::Break(int kase)
{
  fprintf(stderr,"Break(%d)\n",kase);
}
//______________________________________________________________________________
void StEventHelper::Remove(StEvent *ev,const char *className)
{
StSPtrVecObject& V = ev->content();
  int n = V.size();
  for (int i=0; i<n; i++) {
    StObject *to = V[i];
    if (!to) continue;
    if (!strstr(to->ClassName(),className)) continue;
    V[i] = 0; delete to;
  }
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
StTrackPoints::StTrackPoints(const StTrack *st,const char *name,const char *title)
:StPoints3DABC(name,title,st)
{
  Init();
}
//______________________________________________________________________________
void StTrackPoints::Init() 
{  
  if (fXYZ) return;
  StTrack *trk = ((StTrack*)fObj); 
  StTrackHelper th(trk);
  fXYZ = th.GetPoints(fSize);
  fN = fSize;
  if (!fSize) { MakeZombie(); return;}
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
//VP   if (dist <= mindist) { dist = 0; gPad->SetSelected(fObj);}
   if (dist <= mindist) { dist = 0; gPad->SetSelected(this);}

   return Int_t(dist);
}

  
//______________________________________________________________________________
ClassImp(StVertexPoints)
//______________________________________________________________________________
StVertexPoints::StVertexPoints(const StVertex *sv,const char *name,const char *title)
:StPoints3DABC(name,title,sv)
{
  SetBit(1);
  fSize = 1; fN =1;
  fXYZ = new Float_t[3];
  fXYZ[0] = ((StVertex*)fObj)->position().x(); 
  fXYZ[1] = ((StVertex*)fObj)->position().y(); 
  fXYZ[2] = ((StVertex*)fObj)->position().z(); 
}
//______________________________________________________________________________
ClassImp(StVertexPoints)
//______________________________________________________________________________
StInnOutPoints::StInnOutPoints(const StTrack *st,int innout,const char *name,const char *title)
:StPoints3DABC(name,title,st)
{
  fSize = 1; fN =1; fInnOut=innout;
  const StTrackGeometry *geo = (fInnOut==0) ? st->geometry():st->outerGeometry();
  fXYZ = new Float_t[3];
  fXYZ[0] = geo->origin().x(); 
  fXYZ[1] = geo->origin().y(); 
  fXYZ[2] = geo->origin().z(); 
}
   
   
ClassImp(StHitPoints)
//______________________________________________________________________________
StHitPoints::StHitPoints(const StHit *sh,const char *name,const char *title)
:StPoints3DABC(name,title,sh)
{
  fSize = 1; fN =1;
  Init();
}
//______________________________________________________________________________
StHitPoints::StHitPoints(const StRefArray *ar,const char *name,const char *title)
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
  
  int n=0;
  for (int i =0;i<fSize;i++)
  {
     StHit *hit= (fSize==1) ? (StHit*)fObj: (StHit*)((StRefArray*)fObj)->at(i);
     if (fSize>1 && !hit->trackReferenceCount()) continue;
     if (fSize>1 && !hit->usedInFit()) 		 continue;
     StThreeVectorF v3 = hit->position();
     fXYZ[n*3+0] = v3.x(); 
     fXYZ[n*3+1] = v3.y(); 
     fXYZ[n*3+2] = v3.z(); 
     n++;
  }
  fN=n; fSize=n;
}
   
//______________________________________________________________________________
ClassImp(StFilterABC)

int StFilterABC::fgDial=0;
//______________________________________________________________________________
StFilterABC::StFilterABC(const char *name,bool active):TNamed(name,""),fActive(active)
{
}
//______________________________________________________________________________
void StFilterABC::SetDefs()
{
  for (int i=0;GetNams() && GetNams()[i]; i++) {GetPars()[i]=GetDefs()[i];}
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
   "  PseudoMin    ",
   "  PseudoMax    ",
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
   /*  PseudoMin   =*/ -999.00,
   /*  PseudoMax   =*/  999.00,
   /*  QMin        =*/   -1   ,
   /*  QMax        =*/   +1   ,
   /* Encoded method*/   -1   ,     // The default value -1 menas all

   0};
  return defs;   
}   

//______________________________________________________________________________
Int_t StFilterDef::Accept(StPoints3DABC *pnt,Color_t &color, Size_t&, Style_t&)
{
   static TRandom rrr;
   float x,y,z,r2xy,phid,len,pt,ps,q;
   const TObject *to;
   const StTrack *trk;
    // set default color for tracks
   color = (((color-kRed)+1)%6)+kRed;

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

   // set default color for tracks
   // color = (((color-kRed)+1)%6)+kRed;

   trk = (StTrack*)to;
   len = trk->length();
   cut++;
   if (fLenMin >len || len > fLenMax)			goto SKIP;	
   pt = trk->geometry()->momentum().perp();
   cut++;
   if (fPtMin >pt || pt > fPtMax)			goto SKIP;	
   ps = trk->geometry()->momentum().pseudoRapidity();
   cut++;
   if (fPsMin >ps || ps > fPsMax)			goto SKIP;	
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
#if 0
  mBB = new BetheBloch();
#endif
  SetDefs();
  
}
//______________________________________________________________________________
StMuDstFilterHelper::~StMuDstFilterHelper()
#if 0
{ delete mBB;}
#else
{}
#endif
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
#if 0
        // check dEdx
        //	      if (mBB==0) mBB = new BetheBloch();
        float dedxHigh = dEdxFractionCutHigh * mBB->Sirrf(magnitude/dEdxMassCutHigh);
        float dedxLow  = dEdxFractionCutLow  * mBB->Sirrf(magnitude/dEdxMassCutLow);
#else
        float dedxHigh = dEdxFractionCutHigh * Bichsel::Instance()->GetI70M(TMath::Log10(magnitude/dEdxMassCutHigh));
        float dedxLow  = dEdxFractionCutLow  * Bichsel::Instance()->GetI70M(TMath::Log10(magnitude/dEdxMassCutLow));
#endif
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
   const TObject *to;
   const StTrack *trk;
   to = pnt->GetObject();
   if (!to) 						return 1;
   if (!to->InheritsFrom(StTrack::Class()))		return 1;
   trk = (StTrack*)to;
   return Accept(trk);
}

//______________________________________________________________________________
ClassImp(StColorFilterHelper)
StColorFilterHelper::StColorFilterHelper(const char *name,bool active):StFilterABC(name,active)
{
   fPidAlgorithm = new StTpcDedxPidAlgorithm();;
   fElectron     = StElectron::instance();
   fPion         = StPionPlus::instance();
   fKaon         = StKaonPlus::instance();
   fProton       = StProton::instance();

   SetDefs();
  
}
//______________________________________________________________________________
StColorFilterHelper::~StColorFilterHelper()
{ delete fPidAlgorithm;}
//______________________________________________________________________________
const char  **StColorFilterHelper::GetNams() const
{
  static const char *nams[] = {
    " Electron sigma ",  
    " Electron color ",
    " Pion sigma     ",  
    " Pion color     ",
    " Kaon sigma     ",  
    " Kaon color     ",
    " Proton sigma   ",  
    " Proton color   ",
    " others sigma   ",  
    " others color   ",
    0
  };
  return nams;
}
//______________________________________________________________________________
const float  *StColorFilterHelper::GetDefs() const
{
  static const float defs[] = {
    /* fNSigmaElectron*/ 1 ,   // nSigma cut for electron 
    /* fNColorElectron*/ 2 ,   // the color index for electron
    /* fNSigmaPion    */ 1 ,   // nSigma cut for electron
    /* fNColorPion    */ 3 ,   // the color index for pion
    /* fNSigmaKaon    */ 1 ,   // nSigma cut for pion
    /* fNColorKaon    */ 4 ,   // the color index for kaon 
    /* fNSigmaProton  */ 1 ,   // nSigma cut for kaon
    /* fNColorProton  */ 6 ,   // the color index kaon
    
    /* fNSigmaOther*/   -1,     // nSigma cut for other types
    /* fNColorOther*/    0,     // the color index for other types
    0
  };
  return defs;
}
//______________________________________________________________________________
Int_t StColorFilterHelper::Accept(const StTrack* track, Color_t &color, Size_t&size, Style_t&) {
  
float   sigmaElectron =  fNSigmaElectron ;         // nSigna cut for electron 
Color_t colorElectron = (Color_t)fNColorElectron ; // the color index for electron

float   sigmaPion     = fNSigmaPion ;              // nSigna cut for electron
Color_t colorPion     = (Color_t)fNColorPion ;     // the color index for pion

float   sigmaKaon     = fNSigmaKaon ;              // nSigna cut for pion
Color_t colorKaon     = (Color_t)fNColorKaon ;     // the color index for kaon 

float   sigmaProton   = fNSigmaProton ;            // nSigna cut for kaon
Color_t colorProton   = (Color_t)fNColorProton ;   // the color index kaon
    
// float   sigmaOther    = fNSigmaOther ;             // nSigna cut for other types
Color_t colorOther    = (Color_t)fNColorOther ;    // the color index for other types

// Fisyak's color schema

/* const StParticleDefinition* pd = */ track->pidTraits(*fPidAlgorithm);

  color = colorOther;
  size  = 1;

 if (TMath::Abs(fPidAlgorithm->numberOfSigma(fElectron)) < sigmaElectron) 
    { color = colorElectron; size = 2; }

 if (TMath::Abs(fPidAlgorithm->numberOfSigma(fKaon))     < sigmaKaon)
    { color = colorKaon; size = 4; }
 
 if (TMath::Abs(fPidAlgorithm->numberOfSigma(fPion))     < sigmaPion)
    { color = colorPion; size = 5; }
 
 if (TMath::Abs(fPidAlgorithm->numberOfSigma(fProton))   < sigmaProton)
    { color = colorProton; size = 3; }

  return 1;
}

//______________________________________________________________________________
Int_t StColorFilterHelper::Accept(StPoints3DABC *pnt, Color_t&color, Size_t&size, Style_t&style) 
{
   const TObject *to;
   const StTrack *trk;
   to = pnt->GetObject();
   if (!to) 						return 1;
   if (!to->InheritsFrom(StTrack::Class()))		return 1;
   trk = (StTrack*)to;
   return Accept(trk,color,size,style);
}
//______________________________________________________________________________
//______________________________________________________________________________
//______________________________________________________________________________
ClassImp(StVertexHelper)
StVertexHelper::StVertexHelper(const StVertex *vtx)
{ SetVertex(vtx);} 
StVertexHelper::StVertexHelper(const StEvent *evt)
{ SetVertex(evt->primaryVertex(0));} 
//______________________________________________________________________________
void StVertexHelper::SetVertex(const StVertex *vtx){fVtx = vtx;}    
int  StVertexHelper::GetType()               {return (int)fVtx->type();}
int  StVertexHelper::GetFlag()               {return fVtx->flag();};
int  StVertexHelper::GetNTracks()            {return fVtx->numberOfDaughters();}	
//______________________________________________________________________________
const StThreeVectorF &StVertexHelper::GetPoint()
{
  return fVtx->position();
}  
//______________________________________________________________________________   
const StTrack *StVertexHelper::GetTrack(int idx)  	// -1=parent track  
{
  if (idx==-1) return (const StTrack *) fVtx->parent();
  if (idx>= GetNTracks()) return 0;
  return (const StTrack *) fVtx->daughter((UInt_t)idx);
}
//______________________________________________________________________________   
const float *StVertexHelper::GetErrMtx()  	
{
/// return error matrix in form xx,yx,yy,zx,zy,zz

  StMatrixF mxF = fVtx->covariantMatrix();
  int jj=0;
  for (int i=0;i< 3;i++) {
  for (int j=0;j<=i;j++) {
    fErrMtx[jj++] = mxF(i+1,j+1);}}
  return fErrMtx;
}
//______________________________________________________________________________
//______________________________________________________________________________
//______________________________________________________________________________
ClassImp(StTrackHelper)
StTrackHelper::StTrackHelper(const StTrack *trk) : 
      StHelixHelper(trk->geometry()->helix(),
                    trk->outerGeometry()->helix(),trk->length())
      , fTrk(trk), fHits(0)
{    GetNHits();  }
StTrackHelper::StTrackHelper(const StGlobalTrack *trk) : 
      StHelixHelper(trk->dcaGeometry()->helix(),
                    trk->outerGeometry()->helix(),trk->length())
      , fTrk(trk), fHits(0)
{    GetNHits();  }

StTrackHelper::~StTrackHelper()
{  }
int  StTrackHelper::GetType()  			const	{return fTrk->type();}
        int     StTrackHelper::GetFlag()	const 	{return fTrk->flag();}
        int     StTrackHelper::GetCharge() 	const	{return fTrk->geometry()->charge();}
const StVertex *StTrackHelper::GetParent() 	const  	{return fTrk->vertex();}	 
      float     StTrackHelper::GetImpact() 	const 	{return fTrk->impactParameter();}
      float     StTrackHelper::GetCurv() 	const  	{return GetTHelix(0)->GetRho() ;}
const StThreeVectorF &StTrackHelper::GetFirstPoint() const {return fTrk->geometry()->origin();}
const StThreeVectorF &StTrackHelper::GetLastPoint()  const {return fTrk->outerGeometry()->origin();}
const StThreeVectorF &StTrackHelper::GetMom()        const {return fTrk->geometry()->momentum();}
	
//______________________________________________________________________________
const StPtrVecHit *StTrackHelper::GetHits() const
{
  if (fHits) 	return fHits;
  const StTrackDetectorInfo *tdi = fTrk->detectorInfo();
  if (!tdi)	return 0;
  fHits = &tdi->hits();
  return fHits;
}
//______________________________________________________________________________
int StTrackHelper::GetNHits() const
{ 
  if (fHits) return fHits->size(); 
  GetHits();
  return (fHits)? fHits->size():0; 
}
//______________________________________________________________________________
const StHit *StTrackHelper::GetHit(int idx) const
{
  if (idx<0) 		return 0;
  if (idx>=GetNHits()) 	return 0;
  if (!fHits)          	return 0;
  return fHits->at(idx);
}
//______________________________________________________________________________
int StTrackHelper::numberOfFitPoints(int det) const
{
   const StTrackFitTraits& trait = fTrk->fitTraits();
   return (det)? trait.numberOfFitPoints((StDetectorId)det): trait.numberOfFitPoints();
}
//______________________________________________________________________________
StMCTruth StTrackHelper::GetTruth(int byNumb,double rXYMin,double rXYMax) const
{
   StMCPivotTruth pivo(1);
   int nHits = GetNHits();
   int nUsed=0;
   for (int jh=0;jh<nHits;jh++) {
     const StHit *hit = GetHit(jh);
     double r = sqrt(pow(hit->position().x(),2)+pow(hit->position().y(),2));
     if (r<rXYMin)		continue;	
     if (r>rXYMax) 		continue;	
     int idTruth=hit->idTruth();
     int wtTruth=hit->qaTruth();
     if (!wtTruth) wtTruth=1;
//     if (!idTruth || !wtTruth)	{
//       Warning("GetTruth","idTruth,wtTruth= %d %d",idTruth,wtTruth);
//       				continue;}
     nUsed++; pivo.Add(idTruth,wtTruth);	
   }		
   if (!nUsed) return 0;
   return pivo.Get(byNumb);
}		

//______________________________________________________________________________
//______________________________________________________________________________
//______________________________________________________________________________
ClassImp(StHitHelper)
      StHitHelper::StHitHelper(const StHit *hit){fHit = hit;}
void  StHitHelper::SetHit(const StHit *hit)	{fHit = hit;}    
int   StHitHelper::GetDetId()			{return fHit->detector();}
int   StHitHelper::GetFlag() 			{return fHit->flag();}
float StHitHelper::GetCharge()			{return fHit->charge();}
int   StHitHelper::IsUsed()			{return fHit->trackReferenceCount();}
int   StHitHelper::IsFit()			{return fHit->usedInFit();}
const StThreeVectorF &StHitHelper::GetPoint()	{return fHit->position();}
//______________________________________________________________________________
//______________________________________________________________________________
//______________________________________________________________________________

ClassImp(StErrorHelper)
//_____________________________________________________________________________
StErrorHelper::StErrorHelper()
{
  fNErr=0; fNTot=0; fKErr=0;
  fMap = new TExMap;
  fArr = new TArrayI;
}
//_____________________________________________________________________________
StErrorHelper::~StErrorHelper()
{
  delete fMap;
  delete fArr;
}
//_____________________________________________________________________________
void StErrorHelper::Add(int errn)   
{
  fNTot++;
  if(!errn) return;
  fNErr++;
  (*fMap)(errn)++;
}
//_____________________________________________________________________________
void StErrorHelper::MakeArray()   
{
  if (!fNErr) return;
  fKErr = fMap->GetSize();
  fArr->Set(fKErr*3);
  TExMapIter it(fMap);
  LongKey_t lerr,lnum;

  int idx=0;
  while(it.Next(lerr,lnum)) {
    (*fArr)[idx+    0] = lnum;
    (*fArr)[idx+fKErr] = lerr;
    idx++;
  }
  
  TMath::Sort(fKErr, fArr->GetArray(), fArr->GetArray()+2*fKErr);
}
//_____________________________________________________________________________
void StErrorHelper::Print(const char* txt) const
{
  StErrorHelper *This = (StErrorHelper *)this;
  This->MakeArray();
  if (!txt) txt="";
  printf("StEvent Error Summary:%s\n",txt);
  
  printf("%4d -%8d(%4d)\n",0,0,fNTot-fNErr);
  int *nrr=fArr->GetArray();
  int *krr=nrr+fKErr;
  int *idx=krr+fKErr;
  for (int i=0;i<fKErr;i++) {
    int j = idx[i];
    printf("%4d -%8d(%4d) //%s\n",i+1,krr[j],nrr[j],Say(krr[j]).Data());
  }
}
//_____________________________________________________________________________

TString StErrorHelper::Say(int ierr,const char *klass)
{
  static const char *TabErr[] = 
  {
 "StTrack"		,"mFlag"		,"1","2","is Negative",
 "StTrack"		,"mFlag"		,"1","3","is Zero",

 "StTrack"		,"mImpactParameter"	,"2","1","is NaN",
 "StTrack"		,"mImpactParameter"	,"2","2","is huge",

 "StTrack"		,"mLength"		,"3","1","is NaN",
 "StTrack"		,"mLength"		,"3","2","is huge",
 "StTrack"		,"mLength"		,"3","3","is too small",
 "StTrack"		,"mLength"		,"3","4","contradicts to In/Out distance",
 "StTrack"		,"mLength"		,"3","5","helix out of Zmax",
 "StTrack"		,"mLength"		,"3","6","helix out of Rmax",

 "StTrack"		,"mGeometry"		,"4","2","iz zero",
 "StTrack"		,"mGeometry"		,"4","0","StTrackGeometry",

 "StTrack"		,"mOuterGeometry"	,"5","2","iz zero",
 "StTrack"		,"mOuterGeometry"	,"5","0","StTrackGeometry",

 "StTrack"		,"mDetectorInfo"	,"6","2","iz zero",
 "StTrack"		,"mDetectorInfo"	,"6","0","StTrackDetectorInfo",

 "StTrackGeometry"	,"Helix"	        ,"1","0","StPhysicalHelixD",
 "StTrackGeometry"	,"Helix"	        ,"1","2","out of zMax",
 "StTrackGeometry"	,"Helix"	        ,"1","3","out of rMax",

 "StTrackDetectorInfo"	,"mFirstPoint"	        ,"1","0","StThreeVectorF",
 "StTrackDetectorInfo"	,"mFirstPoint"	        ,"1","2","out of zMax",
 "StTrackDetectorInfo"	,"mFirstPoint"	        ,"1","3","out of rMax",

 "StTrackDetectorInfo"	,"mLastPoint"	        ,"2","0","StThreeVectorF",
 "StTrackDetectorInfo"	,"mLastPoint"	        ,"2","2","out of zMax",
 "StTrackDetectorInfo"	,"mFLastPoint"	        ,"2","3","out of rMax",


 "StPhysicalHelixD"	,"mDipAngle"	        ,"1","1","is NaN",
 "StPhysicalHelixD"	,"mDipAngle"	        ,"1","2",">  Py/2",
 "StPhysicalHelixD"	,"mDipAngle"	        ,"1","3","== Py/2",

 "StPhysicalHelixD"	,"mCurvature"	        ,"2","1","is NaN",
 "StPhysicalHelixD"	,"mCurvature"	        ,"2","2","too big",
 "StPhysicalHelixD"	,"mCurvature"	        ,"2","3","is Negaive",

 "StPhysicalHelixD"	,"mOrigin"	        ,"3","0","StThreeVectorD",
 "StPhysicalHelixD"	,"mH"	        	,"4","2","!= 1 or -1",

 "StThreeVectorD"	,"mX1"	        	,"1","1","is NaN",
 "StThreeVectorD"	,"mX1"	        	,"1","2","too big",
 "StThreeVectorD"	,"mX2"	        	,"2","1","is NaN",
 "StThreeVectorD"	,"mX2"	        	,"2","2","too big",
 "StThreeVectorD"	,"mX3"	        	,"3","1","is NaN",
 "StThreeVectorD"	,"mX3"	        	,"3","2","too big",


 "StThreeVectorF"	,"mX1"	        	,"1","1","is NaN",
 "StThreeVectorF"	,"mX1"	        	,"1","2","too big",
 "StThreeVectorF"	,"mX2"	        	,"2","1","is NaN",
 "StThreeVectorF"	,"mX2"	        	,"2","2","too big",
 "StThreeVectorF"	,"mX3"	        	,"3","1","is NaN",
 "StThreeVectorF"	,"mX3"	        	,"3","2","too big",
 0};
TString ts;

  int jmm = ierr%10;
  int jrr = (ierr/10)%10;
  for (const char **jt=TabErr;*jt;jt+=5) {
    if (strcmp(klass,*jt) ) 	continue;
    if (atoi(jt[2]) != jmm) 	continue;
    if (atoi(jt[3]) != jrr) 	continue;
    ts+=jt[1]; 
    if (jrr) { ts+=": "; ts+=jt[4];}
    else     { ts+=".";  ts+=Say(ierr/100,jt[4]);}
    return ts;
  }
  ts="***Unknown***";
  return ts;
}
