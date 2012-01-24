#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "StEvent.h"
#include "StHit.h"
#include "StEventHitIter.h"
#include "StTpcHitCollection.h"
#include "StTpcSectorHitCollection.h"
#include "StTpcPadrowHitCollection.h"
#include "StFtpcHitCollection.h"
#include "StFtpcPlaneHitCollection.h"
#include "StSsdHitCollection.h"
#include "StSvtHitCollection.h"
#include "StRnDHitCollection.h"
#include "StEtrHitCollection.h"
#include "StTofCollection.h"


//________________________________________________________________________________
//________________________________________________________________________________
//________________________________________________________________________________
//..............................................................................
class StTpcHitCollection;
class StTpcHitIter : public StHitIter {
public:
  StTpcHitIter(const StTpcHitCollection *hc):StHitIter(hc){}
  StTpcHitIter(const StEvent            *ev){Reset(ev);}
  StDetectorId DetectorId() const {return kTpcId;}
  const void *GetContainer(const StEvent *ev) const;

protected:
const void *GetObj(const void *cont,int lev,int idx) const; 
      int  GetSize(const void *cont,int lev        ) const; 
};

//..............................................................................
class StSvtHitCollection;
class StSvtHitIter : public StHitIter {
public:
  StSvtHitIter(const StEvent            *ev) {Reset(ev);}
  StSvtHitIter(const StSvtHitCollection *hc):StHitIter(hc){}
  StDetectorId DetectorId() const {return kSvtId;}
  const void *GetContainer(const StEvent *ev) const;
protected:
const void *GetObj(const void *cont,int lev,int idx) const; 
      int  GetSize(const void *cont,int lev        ) const; 
};


//..............................................................................
class StSsdHitCollection;
class StSsdHitIter : public StHitIter {
public:
  StSsdHitIter(const StEvent            *ev){Reset(ev);}
  StSsdHitIter(const StSsdHitCollection *hc):StHitIter(hc){}
  StDetectorId DetectorId() const {return kSsdId;}
  const void *GetContainer(const StEvent *ev) const;
protected:
const void *GetObj(const void *cont,int lev,int idx) const; 
      int  GetSize(const void *cont,int lev        ) const; 
};


//..............................................................................
class StFtpcHitCollection;
class StFtpcHitIter : public StHitIter {
public:
  StFtpcHitIter(const StEvent             *ev){Reset(ev);}
  StFtpcHitIter(const StFtpcHitCollection *hc):StHitIter(hc){}
  StDetectorId DetectorId() const {return kFtpcWestId;}
  const void *GetContainer(const StEvent *ev) const;
protected:
const void *GetObj(const void *cont,int lev,int idx) const; 
      int  GetSize(const void *cont,int lev        ) const; 
};


//..............................................................................
class StRnDHitCollection;
class StRnDHitIter : public StHitIter {
public:
  StRnDHitIter(const StEvent*ev,StDetectorId id){fId=id;Reset(ev);}
  StRnDHitIter(const StRnDHitCollection *hc,StDetectorId id):StHitIter(hc){fId=id;}
  StDetectorId DetectorId() const {return fId;}
  const void *GetContainer(const StEvent *ev) const;
protected:
const void *GetObj(const void *cont,int lev,int idx) const; 
      int  GetSize(const void *cont,int lev        ) const; 
private:
  StDetectorId fId;
};

//..............................................................................
class StTofCollection;
class StTofHitIter : public StHitIter {
public:
  StTofHitIter(const StEvent *ev){Reset(ev);}
  StTofHitIter(const StTofCollection *hc):StHitIter(hc){}
  StDetectorId DetectorId() const {return kTofId;}
  const void *GetContainer(const StEvent *ev) const;
protected:
const void *GetObj(const void *cont,int lev,int idx) const; 
      int  GetSize(const void *cont,int lev        ) const; 
};
//..............................................................................
class StEtrHitCollection;
class StEtrHitIter : public StHitIter {
public:
  StEtrHitIter(const StEvent *ev){Reset(ev);}
  StEtrHitIter(const StEtrHitCollection *hc):StHitIter(hc){}
  StDetectorId DetectorId() const {return kEtrId;}
  const void *GetContainer(const StEvent *ev) const;
protected:
const void *GetObj(const void *cont,int lev,int idx) const; 
      int  GetSize(const void *cont,int lev        ) const; 
};


//________________________________________________________________________________
 StHitIter::StHitIter(const void *cont)
{
  fCont=0;Reset(cont);
} 
//________________________________________________________________________________
void StHitIter::Reset(const void *cont)
{
  fKase=kINI; fLev=0;
  if (cont) fCont = cont; 
}  
//________________________________________________________________________________
void StHitIter::Reset(const StEvent *evt)
{
  fKase=kINI; fLev=0;  
  if (!evt) return;
  fCont = (evt)?  GetContainer(evt):0;
}
//________________________________________________________________________________
int StHitIter::operator++()
{
   int n,j; const void *v;
   do  {
     switch (fKase) {

     case kINI: {
       if (!fCont) {fKase = kEND; break;}
       fLev=0; fStk[0].mN=GetSize(fCont,0); fStk[0].mJ=0; fStk[0].mV=fCont;
       fKase = kDOW; if (!fStk[0].mN) fKase = kEND; 
       break;}

     case kDOW: {
       fStk[fLev+1].mV = v = GetObj(fStk[fLev].mV,fLev,fStk[fLev].mJ);
       if (!v) {fKase=kHOR; break;}
       fStk[++fLev].mN = n = GetSize(v,fLev);
       fStk[  fLev].mJ = -1;
       if (n==-1){fKase=kHIT; break;} 
       fKase = kHOR; break;}



     case kHIT: {
      j = ++fStk[fLev-1].mJ; 
      if (j >=fStk[fLev-1].mN) {fLev++; fKase=kUPP; break;}
      fStk[fLev].mV = v = GetObj(fStk[fLev-1].mV,fLev-1,j);
      break;}

     case kHOR: {
       j = ++(fStk[fLev].mJ);
       if(j>=fStk[fLev].mN) {fKase=kUPP; break;}
       fStk[fLev+1].mV = v = GetObj(fStk[fLev].mV,fLev,j); if (!v) break;
       fStk[fLev+1].mN = n = GetSize(v           ,fLev+1); if (!n) break;
       fStk[++fLev].mJ = -1;
       if (n == -1) {fKase=kHIT; break;}
       fKase = kHOR; break;}

     case kUPP: {
       fLev--; if (fLev<0) { fKase = kEND; break;}
       fKase = kHOR; break;
     }
     case kEND: break;
     
     default: assert(0 && "Wrong fKase");
     }

   } while (fKase!=kEND && !(fKase==kHIT && fStk[fLev].mV));
   return (fKase!=kEND);      
}     
//________________________________________________________________________________
StHit *StHitIter::operator*() 
{ 
  if (fKase==kINI) ++(*this);
  return (StHit*)((fKase==kHIT)? fStk[fLev].mV:0);
}     
//________________________________________________________________________________
UInt_t StHitIter::UPath() const
{ 
  assert (fKase==kHIT);
  UInt_t u=0;
  for (int j=0;j<fLev;j++) { u = u*(fStk[j].mN+1)+fStk[j].mJ+1;}
  return u;
}     
//________________________________________________________________________________
void StHitIter::Print(const char *opt) 
{ 
  Reset((StEvent*)0);
  StHitIter &it = *this;
  StHit *hit=0;
  int nhit = 0;
  for (;(hit=*it);++it) {
    nhit++;
    printf("%4d - Det=%d(%d) Hardw=%p XYZ=%g %g %g\n",nhit
          ,(int)it.DetectorId(),(int)hit->detector()
	  ,(void*)hit->hardwarePosition()
	  ,hit->position().x()
	  ,hit->position().y()
	  ,hit->position().z());
  }
  Reset((StEvent*)0);
}
//________________________________________________________________________________
const void *StTpcHitIter::GetContainer(const StEvent *ev) const
{ return (ev)? ev->tpcHitCollection():0; }
//________________________________________________________________________________
const void *StTpcHitIter::GetObj(const void *cont,int lev,int idx) const 
{
   switch (lev) {
   
     case 0: return ((StTpcHitCollection*)cont)->sector(idx);

     case 1: return ((StTpcSectorHitCollection*)cont)->padrow(idx);

     case 2: return ((StTpcPadrowHitCollection*)cont)->hits().at(idx);

     default: return 0;
   }
}

//________________________________________________________________________________
int StTpcHitIter::GetSize(const void *cont,int lev) const 
{
   switch (lev) {
   
     case 0: return ((StTpcHitCollection*)cont)->numberOfSectors();

     case 1: return ((StTpcSectorHitCollection*)cont)->numberOfPadrows();

     case 2: return ((StTpcPadrowHitCollection*)cont)->hits().size();

     case 3: return -1;

     default: assert(0 && "Wrong level");;
   }
}
//________________________________________________________________________________
//________________________________________________________________________________
const void *StSvtHitIter::GetContainer(const StEvent *ev) const
{ return (ev)? ev->svtHitCollection():0; }
//________________________________________________________________________________
const void *StSvtHitIter::GetObj(const void *cont,int lev,int idx) const 
{
   switch (lev) {
   
     case 0: return ((StSvtHitCollection*)cont)->barrel(idx);

     case 1: return ((StSvtBarrelHitCollection*)cont)->ladder(idx);

     case 2: return ((StSvtLadderHitCollection*)cont)->wafer(idx);

     case 3: return ((StSvtWaferHitCollection*)cont)->hits().at(idx);

     default: return 0;
   }
}

//________________________________________________________________________________
int StSvtHitIter::GetSize(const void *cont,int lev) const 
{
   switch (lev) {
   
     case 0: return ((StSvtHitCollection*)cont)->numberOfBarrels();

     case 1: return ((StSvtBarrelHitCollection*)cont)->numberOfLadders();

     case 2: return ((StSvtLadderHitCollection*)cont)->numberOfWafers();

     case 3: return ((StSvtWaferHitCollection*)cont)->hits().size();

     case 4: return -1;

     default: assert(0 && "Wrong level");;
   }
}
//________________________________________________________________________________
//________________________________________________________________________________
const void *StSsdHitIter::GetContainer(const StEvent *ev) const
{ return (ev)? ev->ssdHitCollection():0; }
//________________________________________________________________________________
const void *StSsdHitIter::GetObj(const void *cont,int lev,int idx) const 
{
   switch (lev) {
   
     case 0: return ((StSsdHitCollection*)cont)->ladder(idx);

     case 1: return ((StSsdLadderHitCollection*)cont)->wafer(idx);

     case 2: return ((StSsdWaferHitCollection*)cont)->hits().at(idx);

     default: return 0;
   }
}

//________________________________________________________________________________
int StSsdHitIter::GetSize(const void *cont,int lev) const 
{
   switch (lev) {
   
     case 0: return ((StSsdHitCollection*)cont)->numberOfLadders();

     case 1: return ((StSsdLadderHitCollection*)cont)->numberOfWafers();

     case 2: return ((StSsdWaferHitCollection*)cont)->hits().size();

     case 3: return -1;

     default: assert(0 && "Wrong level");;
   }
}
//________________________________________________________________________________
//________________________________________________________________________________
//________________________________________________________________________________
const void *StFtpcHitIter::GetContainer(const StEvent *ev) const
{ return (ev)? ev->ftpcHitCollection():0; }
//________________________________________________________________________________
const void *StFtpcHitIter::GetObj(const void *cont,int lev,int idx) const 
{
   switch (lev) {
   
     case 0: return ((StFtpcHitCollection*)cont)->plane(idx);

     case 1: return ((StFtpcPlaneHitCollection*)cont)->sector(idx);

     case 2: return ((StFtpcSectorHitCollection*)cont)->hits().at(idx);

     default: return 0;
   }
}

//________________________________________________________________________________
int StFtpcHitIter::GetSize(const void *cont,int lev) const 
{
   switch (lev) {
   
     case 0: return ((StFtpcHitCollection*)cont)->numberOfPlanes();

     case 1: return ((StFtpcPlaneHitCollection*)cont)->numberOfSectors();

     case 2: return ((StFtpcSectorHitCollection*)cont)->hits().size();

     case 3: return -1;

     default: assert(0 && "Wrong level");;
   }
}
//________________________________________________________________________________
//________________________________________________________________________________
//________________________________________________________________________________
const void *StRnDHitIter::GetContainer(const StEvent *ev) const
{ return (ev)? ev->rndHitCollection():0; }
//________________________________________________________________________________
const void *StRnDHitIter::GetObj(const void *cont,int lev,int idx) const 
{
   switch (lev) {
   
     case 0: return ((StRnDHitCollection*)cont)->hits().at(idx);
     default: return 0;
   }
}

//________________________________________________________________________________
int StRnDHitIter::GetSize(const void *cont,int lev) const 
{
   switch (lev) {
   
     case 0: return ((StRnDHitCollection*)cont)->hits().size();

     case 1: return -1;

     default: assert(0 && "Wrong level");;
   }
}
//________________________________________________________________________________
//________________________________________________________________________________
const void *StTofHitIter::GetContainer(const StEvent *ev) const
{ return (ev)? ev->tofCollection():0; }
//________________________________________________________________________________
const void *StTofHitIter::GetObj(const void *cont,int lev,int idx) const 
{
   switch (lev) {
   
     case 0: return ((const StTofCollection*)cont)->tofHits().at(idx);
     default: return 0;
   }
}

//________________________________________________________________________________
int StTofHitIter::GetSize(const void *cont,int lev) const 
{
   switch (lev) {
   
     case 0: return ((const StTofCollection*)cont)->tofHits().size();

     case 1: return -1;

     default: assert(0 && "Wrong level");;
   }
}
//________________________________________________________________________________

const void *StEtrHitIter::GetContainer(const StEvent *ev) const
{ return (ev)? ev->etrHitCollection():0; }
//________________________________________________________________________________
const void *StEtrHitIter::GetObj(const void *cont,int lev,int idx) const 
{
   switch (lev) {
   
     case 0: return ((const StEtrHitCollection*)cont)->hits().at(idx);
     default: return 0;
   }
}

//________________________________________________________________________________
int StEtrHitIter::GetSize(const void *cont,int lev) const 
{
   switch (lev) {
   
     case 0: return ((const StEtrHitCollection*)cont)->hits().size();

     case 1: return -1;

     default: assert(0 && "Wrong level");;
   }
}
//________________________________________________________________________________
//________________________________________________________________________________
StEventHitIter::StEventHitIter(const StEvent *ev)
{
 fStEvent = ev;
 fJter=0; fNter=0;
 memset(fIter,0,sizeof(fIter));
}
//________________________________________________________________________________
StEventHitIter::~StEventHitIter()
{
  for (int jk=0;jk<fNter;jk++) {delete fIter[jk];fIter[jk]=0;}
}
//________________________________________________________________________________
int StEventHitIter::AddDetector(StDetectorId detId)
{
   switch ((int)detId) {
   
     case kTpcId: fIter[fNter++] = new StTpcHitIter(fStEvent);break;
     case kSvtId: fIter[fNter++] = new StSvtHitIter(fStEvent);break;
     case kSsdId: fIter[fNter++] = new StSsdHitIter(fStEvent);break;
     case kFtpcWestId:; 
     case kFtpcEastId:
                  fIter[fNter++] = new StFtpcHitIter(fStEvent);break;
     case kPxlId: case kIstId: case kFgtId: case kFmsId: 
                  fIter[fNter++] = new StRnDHitIter(fStEvent,detId);break;

     case kTofId: fIter[fNter++] = new StTofHitIter(fStEvent)   ;break;
     case kEtrId: fIter[fNter++] = new StEtrHitIter(fStEvent)   ;break;

     default: printf("StEventHitIter::AddDetector: No iterator for detectorId=%d",(int)detId);
     assert(0 && "No iterator for detectorId");
     return 1;
  }
  return 0;
}
//________________________________________________________________________________
int StEventHitIter::AddDetector(const char *name)
{
   return AddDetector(detectorIdByName(name));
}
//________________________________________________________________________________
void StEventHitIter::Reset(const StEvent *ev)
{
  fJter = 0;
  if (ev) fStEvent = ev;
  for (int jk=0;jk<fNter;jk++) {fIter[jk]->Reset(fStEvent);}
  StHitIter::Reset((void*)0);
}
//________________________________________________________________________________
int StEventHitIter::operator++()
{
  for (;fJter<fNter;fJter++) {if (++(*fIter[fJter])) return 1;}
  return 0;
}

//________________________________________________________________________________
StDetectorId StEventHitIter::DetectorId() const
{
   if (fJter >= fNter) return kUnknownId;
   return fIter[fJter]->DetectorId();
}
//________________________________________________________________________________
UInt_t StEventHitIter::UPath() const
{
   if (fJter >= fNter) return 0;
   return fIter[fJter]->UPath();
}
//________________________________________________________________________________
StHit *StEventHitIter::operator*() 
{
   if (fJter>=fNter) return 0;
   return *(*(fIter[fJter]));
}
