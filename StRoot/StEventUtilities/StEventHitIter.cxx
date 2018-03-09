#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "StEvent.h"
#include "StEvent/StEnumerations.h"
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
#include "StFgtCollection.h"
#include "StFgtHitCollection.h"
#include "StFgtPointCollection.h"
#include "StIstHitCollection.h"

ClassImp(StHitIter)
ClassImp(StHitIterGroup)
ClassImp(StEventHitIter)

//________________________________________________________________________________
StHitIter::StHitIter()
{
fCont = 0;
fDowIter=0;
fJIter=-1;
fNIter=0;
}
//________________________________________________________________________________
StHitIter::~StHitIter()
{
  delete fDowIter;fDowIter=0;
}
//________________________________________________________________________________
const TObject *StHitIter::Reset(const TObject *cont) 
{
  fCont = cont;
  if (!fCont) return 0;
  fNIter = GetSize();
  for (fJIter =0;fJIter<fNIter;fJIter++) {
    const TObject *dowCont = GetObject(fJIter);
    if (!dowCont) continue;
    if (!fDowIter)	return dowCont;
    const TObject *to = fDowIter->Reset(dowCont);
    if (to) return to;
  }
  return 0;
}
//________________________________________________________________________________
const TObject *StHitIter::Get() const
{
   if (fJIter>=fNIter) return 0;
   const TObject *obj = GetObject(fJIter);
   if (!obj) return 0;
   if (!fDowIter) return obj;
   return fDowIter->Get();
}
//________________________________________________________________________________
const TObject *StHitIter::operator++()
{
  const TObject *to=0;
  if (fJIter>=fNIter) 	return 0;
  if (!fDowIter) { 	//Lowest level. Hits are there
    while (++fJIter<fNIter) 	{
      if ((to = GetObject(fJIter))) return to;;
    }
    return 0;
  }
//		Intermediate level, containers there
  if ((to= ++(*fDowIter))) 		return to; 
  while(++fJIter<fNIter) {
    if (!(to=GetObject(fJIter)))	continue;;
    if ( (to=fDowIter->Reset(to)))	return to;
  }
  return 0;  
}
//________________________________________________________________________________
void StHitIter::UPath(ULong64_t &upath) const
{
  if (fDowIter) fDowIter->UPath(upath);
  upath*=fNIter; upath+=fJIter; 
}

//________________________________________________________________________________
//________________________________________________________________________________
StHitIterGroup::StHitIterGroup()
{
  fDetectorId = kUnknownId;
}
//________________________________________________________________________________
StHitIterGroup::~StHitIterGroup()
{
  for (int i=0;i<(int)fGroup.size();i++) { delete fGroup[i];}
}
//________________________________________________________________________________
const TObject *StHitIterGroup::Reset(const TObject *cont)
{
  if (cont) fCont=cont;
  if (!fCont) return 0;
  fDetectorId = kUnknownId;
  fNIter = fGroup.size();
  for (fJIter=0;fJIter<fNIter;fJIter++) {
    const TObject *to=fGroup[fJIter]->Reset(fCont);
    if (to) return to;
  }
  return 0;
}
//________________________________________________________________________________
void StHitIterGroup::Add(StHitIter* iter)
{
  fGroup.push_back(iter);
  fNIter = fGroup.size();
}
//________________________________________________________________________________
const TObject *StHitIterGroup::GetObject (int) const
{ assert(0 && "In StHitIterGroup::GetObject");}
//________________________________________________________________________________
const TObject *StHitIterGroup::Get () const
{
  fDetectorId = kUnknownId;
  if (fJIter>=fNIter) return 0;
  fDetectorId = fGroup[fJIter]->DetectorId();
  return fGroup[fJIter]->Get();
}
//________________________________________________________________________________
const TObject *StHitIterGroup::operator++()
{
 if (fJIter>=fNIter) return 0;
 const TObject *to;
 if ((to=++(*fGroup[fJIter]))) 	return to;

 while(++fJIter<fNIter) {
   to = fGroup[fJIter]->Reset(fCont);
   if (to) return to;
 } 
 return 0;
}
//________________________________________________________________________________
UInt_t StHitIterGroup::UPath() const
{
  ULong64_t ul=0;
  UPath(ul);
  assert(!(ul>>32));
  return ul;
}
//________________________________________________________________________________
void StHitIterGroup::UPath(ULong64_t &ul) const
{
  fGroup[fJIter]->UPath(ul);
}

//________________________________________________________________________________
//_______TPC_______TPC_______TPC_______TPC_______TPC_______TPC_______TPC_______TPC
//________________________________________________________________________________

//..............................................................................
class StTpcHitIter : public StHitIter {
public:
    		StTpcHitIter();
virtual        ~StTpcHitIter(){;}
virtual const TObject *Reset(const TObject *cont);
virtual const TObject *GetObject (int idx) const;
virtual           int  GetSize () const;
  StDetectorId DetectorId() const {return kTpcId;}
protected:
};

//..............................................................................
class StTpcSectorHitIter : public StHitIter {
public:
    		StTpcSectorHitIter(){;}
virtual        ~StTpcSectorHitIter(){;}
virtual const TObject *GetObject (int idx) const;
virtual           int  GetSize () const;
protected:
};
//..............................................................................
class StTpcPadrowHitIter : public StHitIter {
public:
    		StTpcPadrowHitIter(){;}
virtual        ~StTpcPadrowHitIter(){;}
virtual const TObject *GetObject (int idx) const;
virtual           int  GetSize () const;
protected:
};

//________________________________________________________________________________
StTpcHitIter::StTpcHitIter()
{
  StHitIter *obj=this,*hi;
  
  // Create a new iterator over sectors and register it with tpc hit iter
  hi = new StTpcSectorHitIter();
  obj->SetDowIter(hi);
  obj = hi;
		
  // Create a new iterator over padrows and register it with sector iter
  hi = new StTpcPadrowHitIter();
  obj->SetDowIter(hi);
  obj = hi;
}
//________________________________________________________________________________
const TObject *StTpcHitIter::Reset(const TObject *cont)
{
  const StTpcHitCollection *to = 0;
  if (cont) to = ((StEvent*)cont)->tpcHitCollection();
  return StHitIter::Reset(to);
}

//________________________________________________________________________________
int StTpcHitIter::GetSize()const
{ return ((StTpcHitCollection*)fCont)->numberOfSectors();}


//________________________________________________________________________________
const TObject *StTpcHitIter::GetObject (int idx) const
{
  return ((StTpcHitCollection*)fCont)->sector(idx);
}
//________________________________________________________________________________
const TObject *StTpcSectorHitIter::GetObject (int idx) const
{
  return ((StTpcSectorHitCollection*)fCont)->padrow(idx);
}
//________________________________________________________________________________
int StTpcSectorHitIter::GetSize () const
{
  return ((StTpcSectorHitCollection*)fCont)->numberOfPadrows();
}
//________________________________________________________________________________
const TObject *StTpcPadrowHitIter::GetObject (int idx) const
{
  return (const TObject*)((StTpcPadrowHitCollection*)fCont)->hits().at(idx);
}
//________________________________________________________________________________
int StTpcPadrowHitIter::GetSize () const
{
  return ((StTpcPadrowHitCollection*)fCont)->hits().size();
}

//________________________________________________________________________________
//_______SVT_______SVT_______SVT_______SVT_______SVT_______SVT_______SVT_______SVT
//________________________________________________________________________________

//..............................................................................
class StSvtHitIter : public StHitIter {
public:
    		StSvtHitIter();
virtual        ~StSvtHitIter(){;}
virtual const TObject *Reset(const TObject *cont);
virtual const TObject *GetObject (int idx) const;
virtual           int  GetSize () const;
  StDetectorId DetectorId() const {return kSvtId;}
protected:
};
//..............................................................................
class StSvtBarrelHitIter : public StHitIter {
public:
virtual        ~StSvtBarrelHitIter(){;}
virtual const TObject *GetObject (int idx) const;
virtual           int  GetSize () const;
protected:
};
//..............................................................................
class StSvtLadderHitIter : public StHitIter {
public:
virtual        ~StSvtLadderHitIter(){;}
virtual const TObject *GetObject (int idx) const;
virtual           int  GetSize () const;
protected:
};
//..............................................................................
class StSvtWaferHitIter : public StHitIter {
public:
virtual        ~StSvtWaferHitIter(){;}
virtual const TObject *GetObject (int idx) const;
virtual           int  GetSize () const;
protected:
};
//________________________________________________________________________________
StSvtHitIter::StSvtHitIter()
{
  StHitIter *iter = this,*jter=0;
  iter->SetDowIter((jter=new StSvtBarrelHitIter())); iter=jter;
  iter->SetDowIter((jter=new StSvtLadderHitIter())); iter=jter;
  iter->SetDowIter((jter=new StSvtWaferHitIter ())); iter=jter;
}
//________________________________________________________________________________
const TObject *StSvtHitIter::Reset(const TObject *cont)
{
  const StSvtHitCollection *to = 0;
  if (cont) to = ((StEvent*)cont)->svtHitCollection();
  return StHitIter::Reset(to);
}
//________________________________________________________________________________
const TObject *StSvtHitIter::GetObject (int idx) const
{
  return (const TObject*)((StSvtHitCollection*)fCont)->barrel(idx);
}
//________________________________________________________________________________
int StSvtHitIter::GetSize () const
{
  return ((StSvtHitCollection*)fCont)->numberOfBarrels();
}

//________________________________________________________________________________
const TObject *StSvtBarrelHitIter::GetObject (int idx) const
{
  return (const TObject*)((StSvtBarrelHitCollection*)fCont)->ladder(idx);
}
//________________________________________________________________________________
int StSvtBarrelHitIter::GetSize () const
{
  return ((StSvtBarrelHitCollection*)fCont)->numberOfLadders();
}
//________________________________________________________________________________
const TObject *StSvtLadderHitIter::GetObject (int idx) const
{
  return (const TObject*)((StSvtLadderHitCollection*)fCont)->wafer(idx);
}
//________________________________________________________________________________
int StSvtLadderHitIter::GetSize () const
{
  return ((StSvtLadderHitCollection*)fCont)->numberOfWafers();
}
//________________________________________________________________________________
const TObject *StSvtWaferHitIter::GetObject (int idx) const
{
  return (const TObject*)((StSvtWaferHitCollection*)fCont)->hits().at(idx);
}
//________________________________________________________________________________
int StSvtWaferHitIter::GetSize () const
{
  return ((StSvtWaferHitCollection*)fCont)->hits().size();
}

//________________________________________________________________________________
//_______ETR_______ETR_______ETR_______ETR_______ETR_______ETR_______ETR_______ETR
//________________________________________________________________________________
//..............................................................................
class StEtrHitIter : public StHitIter {
public:
    		StEtrHitIter(){;}
virtual        ~StEtrHitIter(){;}
virtual const TObject *Reset(const TObject *cont);
virtual const TObject *GetObject (int idx) const;
virtual           int  GetSize () const;
  StDetectorId DetectorId() const {return kEtrId;}
public:
protected:
};
//________________________________________________________________________________
const TObject *StEtrHitIter::Reset(const TObject *cont)
{
  const StEtrHitCollection *to = 0;
  if (cont) to = ((StEvent*)cont)->etrHitCollection();
  return StHitIter::Reset(to);
}
//________________________________________________________________________________
const TObject *StEtrHitIter::GetObject (int idx) const
{
  return (const TObject*)((StEtrHitCollection*)fCont)->hits().at(idx);
}
//________________________________________________________________________________
int StEtrHitIter::GetSize () const
{
  return ((StEtrHitCollection*)fCont)->hits().size();
}
//________________________________________________________________________________
//_______SSD_______SSD_______SSD_______SSD_______SSD_______SSD_______SSD_______SSD
//________________________________________________________________________________
//..............................................................................
class StSsdHitIter : public StHitIter {
public:
    		StSsdHitIter();
virtual        ~StSsdHitIter(){;}
virtual const TObject *Reset(const TObject *cont);
virtual const TObject *GetObject (int idx) const;
virtual           int  GetSize () const;
  StDetectorId DetectorId() const {return kSsdId;}
protected:
};
//..............................................................................
class StSsdLadderHitIter : public StHitIter {
public:
virtual        ~StSsdLadderHitIter(){;}
virtual const TObject *GetObject (int idx) const;
virtual           int  GetSize () const;
protected:
};
//..............................................................................
class StSsdWaferHitIter : public StHitIter {
public:
virtual        ~StSsdWaferHitIter(){;}
virtual const TObject *GetObject (int idx) const;
virtual           int  GetSize () const;
protected:
};
//________________________________________________________________________________
StSsdHitIter::StSsdHitIter()
{
  StHitIter *iter = this,*jter=0;
  iter->SetDowIter((jter=new StSsdLadderHitIter())); iter=jter;
  iter->SetDowIter((jter=new StSsdWaferHitIter ()));
}
//________________________________________________________________________________
const TObject *StSsdHitIter::Reset(const TObject *cont)
{
  const StSsdHitCollection *to = 0;
  if (cont) to = ((StEvent*)cont)->ssdHitCollection();
  return StHitIter::Reset(to);
}
//________________________________________________________________________________
const TObject *StSsdHitIter::GetObject (int idx) const
{
  return (const TObject*)((StSsdHitCollection*)fCont)->ladder(idx);
}
//________________________________________________________________________________
int StSsdHitIter::GetSize () const
{
  return ((StSsdHitCollection*)fCont)->numberOfLadders();
}

//________________________________________________________________________________
const TObject *StSsdLadderHitIter::GetObject (int idx) const
{
  return (const TObject*)((StSsdLadderHitCollection*)fCont)->wafer(idx);
}
//________________________________________________________________________________
int StSsdLadderHitIter::GetSize () const
{
  return ((StSsdLadderHitCollection*)fCont)->numberOfWafers();
}
//________________________________________________________________________________
const TObject *StSsdWaferHitIter::GetObject (int idx) const
{
  return (const TObject*)((StSsdWaferHitCollection*)fCont)->hits().at(idx);
}
//________________________________________________________________________________
int StSsdWaferHitIter::GetSize () const
{
  return ((StSsdWaferHitCollection*)fCont)->hits().size();
}

//_______IST_______IST_______IST_______IST_______IST_______IST_______IST_______IST
//________________________________________________________________________________
//..............................................................................
class StIstHitIter : public StHitIter {
public:
                StIstHitIter();
virtual        ~StIstHitIter(){;}
virtual const TObject *Reset(const TObject *cont);
virtual const TObject *GetObject (int idx) const;
virtual           int  GetSize () const;
  StDetectorId DetectorId() const {return kIstId;}
protected:
};
//..............................................................................
class StIstLadderHitIter : public StHitIter {
public:
                StIstLadderHitIter(){;}
virtual        ~StIstLadderHitIter(){;}
virtual const TObject *GetObject (int idx) const;
virtual           int  GetSize () const;
protected:
};
//..............................................................................
class StIstSensorHitIter : public StHitIter {
public:
                StIstSensorHitIter(){;}
virtual        ~StIstSensorHitIter(){;}
virtual const TObject *GetObject (int idx) const;
virtual           int  GetSize () const;
protected:
};
//________________________________________________________________________________
StIstHitIter::StIstHitIter()
{
  StHitIter *iter = this,*jter=0;
  iter->SetDowIter((jter=new StIstLadderHitIter())); iter=jter;
  iter->SetDowIter((jter=new StIstSensorHitIter ()));
}
//________________________________________________________________________________
const TObject *StIstHitIter::Reset(const TObject *cont)
{
  const StIstHitCollection *to = 0;
  if (cont) to = ((StEvent*)cont)->istHitCollection();
  return StHitIter::Reset(to);
}
//________________________________________________________________________________
const TObject *StIstHitIter::GetObject (int idx) const
{
  return (const TObject*)((StIstHitCollection*)fCont)->ladder(idx);
}
//________________________________________________________________________________
int StIstHitIter::GetSize () const
{
  return kIstNumLadders;
}
//________________________________________________________________________________
const TObject *StIstLadderHitIter::GetObject (int idx) const
{
  return (const TObject*)((StIstLadderHitCollection*)fCont)->sensor(idx);
}
//________________________________________________________________________________
int StIstLadderHitIter::GetSize () const
{
  return kIstNumSensorsPerLadder;
}
//________________________________________________________________________________
const TObject *StIstSensorHitIter::GetObject (int idx) const
{
  return (const TObject*)((StIstSensorHitCollection*)fCont)->hits().at(idx);
}
//________________________________________________________________________________
int StIstSensorHitIter::GetSize () const
{
  return ((StIstSensorHitCollection*)fCont)->hits().size();
}

//________________________________________________________________________________
//_______FTPC______FTPC______FTPC______FTPC______FTPC______FTPC______FTPC______TPC
//________________________________________________________________________________

//..............................................................................
class StFtpcHitIter : public StHitIter {
public:
    		StFtpcHitIter();
virtual        ~StFtpcHitIter(){;}
virtual const TObject *Reset(const TObject *cont);
virtual const TObject *GetObject (int idx) const;
virtual           int  GetSize () const;
  StDetectorId DetectorId() const {return kFtpcEastId;}
protected:
};

//..............................................................................
class StFtpcPlaneHitIter : public StHitIter {
public:
    		StFtpcPlaneHitIter(){;}
virtual        ~StFtpcPlaneHitIter(){;}
virtual const TObject *GetObject (int idx) const;
virtual           int  GetSize () const;
protected:
};
//..............................................................................
class StFtpcSectorHitIter : public StHitIter {
public:
    		StFtpcSectorHitIter(){;}
virtual        ~StFtpcSectorHitIter(){;}
virtual const TObject *GetObject (int idx) const;
virtual           int  GetSize () const;
protected:
};

//________________________________________________________________________________
StFtpcHitIter::StFtpcHitIter()
{
  StHitIter *obj=this,*hi;
  obj->SetDowIter((hi=new StFtpcPlaneHitIter())); obj = hi;
  obj->SetDowIter((hi=new StFtpcSectorHitIter())); obj = hi;
}
//________________________________________________________________________________
const TObject *StFtpcHitIter::Reset(const TObject *cont)
{
  const StFtpcHitCollection *to = 0;
  if (cont) to = ((StEvent*)cont)->ftpcHitCollection();
  return StHitIter::Reset(to);
}

//________________________________________________________________________________
int StFtpcHitIter::GetSize()const
{ return ((StFtpcHitCollection*)fCont)->numberOfPlanes();}


//________________________________________________________________________________
const TObject *StFtpcHitIter::GetObject (int idx) const
{
  return ((StFtpcHitCollection*)fCont)->plane(idx);
}
//________________________________________________________________________________
const TObject *StFtpcPlaneHitIter::GetObject (int idx) const
{
  return ((StFtpcPlaneHitCollection*)fCont)->sector(idx);
}
//________________________________________________________________________________
int StFtpcPlaneHitIter::GetSize () const
{
  return ((StFtpcPlaneHitCollection*)fCont)->numberOfSectors();
}
//________________________________________________________________________________
const TObject *StFtpcSectorHitIter::GetObject (int idx) const
{
  return (const TObject*)((StFtpcSectorHitCollection*)fCont)->hits().at(idx);
}
//________________________________________________________________________________
int StFtpcSectorHitIter::GetSize () const
{
  return ((StFtpcSectorHitCollection*)fCont)->hits().size();
}

//________________________________________________________________________________
//_______RND_______RND_______RND_______RND_______RND_______RND_______RND_______TPC
//________________________________________________________________________________
//..............................................................................
class StRnDHitIter : public StHitIter {
public:
    		StRnDHitIter(StDetectorId id){fDetectorId=id;}
virtual        ~StRnDHitIter(){;}
virtual const TObject *Reset(const TObject *cont);
virtual const TObject *GetObject (int idx) const;
virtual           int  GetSize () const;
  StDetectorId DetectorId() const {return fDetectorId;}
protected:
StDetectorId fDetectorId;
};
//________________________________________________________________________________
const TObject *StRnDHitIter::Reset(const TObject *cont)
{
  const StRnDHitCollection *to = 0;
  if (cont) to = ((StEvent*)cont)->rndHitCollection();
  return StHitIter::Reset(to);
}
//________________________________________________________________________________
const TObject *StRnDHitIter::GetObject (int idx) const
{
  return (const TObject*)((StRnDHitCollection*)fCont)->hits().at(idx);
}
//________________________________________________________________________________
int StRnDHitIter::GetSize () const
{
  return ((StRnDHitCollection*)fCont)->hits().size();
}
//________________________________________________________________________________
//_______TOF_______TOF_______TOF_______TOF_______TOF_______TOF_______TOF_______TOF
//________________________________________________________________________________
//..............................................................................
#define StTofHitCollection StTofCollection
#define tofHitCollection tofCollection

class StTofHitIter : public StHitIter {
public:
    		StTofHitIter(){;}
virtual        ~StTofHitIter(){;}
virtual const TObject *Reset(const TObject *cont);
virtual const TObject *GetObject (int idx) const;
virtual           int  GetSize () const;
  StDetectorId DetectorId() const {return kTofId;}
public:
protected:
};
//________________________________________________________________________________
const TObject *StTofHitIter::Reset(const TObject *cont)
{
  const StTofHitCollection *to = 0;
  if (cont) to = ((StEvent*)cont)->tofHitCollection();
  return StHitIter::Reset(to);
}
//________________________________________________________________________________
const TObject *StTofHitIter::GetObject (int idx) const
{
  return (const TObject*)((StTofHitCollection*)fCont)->tofHits().at(idx);
}
//________________________________________________________________________________
int StTofHitIter::GetSize () const
{
  return ((StTofHitCollection*)fCont)->tofHits().size();
}


// -------------------------------------------------------------------------------
// -------------------------------------------------------------------------------
// -------------------------------------------------------------------------------

//________________________________________________________________________________
//_______ FGT _____ FGT _____ FGT _____ FGT _____ FGT _____ FGT _____ FGT ____ FGT
//________________________________________________________________________________
class StFgtHitIter : public StHitIter 
{ public:
  StFgtHitIter(); // implementation below...
  virtual ~StFgtHitIter(){ /* nada */ };
  virtual const TObject *Reset( const TObject *container );
  virtual const TObject *GetObject( Int_t idx ) const;
  virtual       Int_t    GetSize() const;
  StDetectorId           DetectorId() const { return kFgtId; }
  protected:
  //  StDetectorId fDetectorId;
};

class StFgtPointHitIter : public StHitIter
{ public:
  StFgtPointHitIter(){ /* nada */ };
  virtual ~StFgtPointHitIter(){ /* nada */ };
  virtual const TObject *GetObject( Int_t idx ) const;
  virtual Int_t GetSize() const;
};
// -------------------------------------------------------------------------------
StFgtHitIter::StFgtHitIter()
{
  this -> SetDowIter( new StFgtPointHitIter() );
}
// -------------------------------------------------------------------------------
const TObject *StFgtHitIter::Reset( const TObject *cont )
{
  const StFgtCollection *to = 0;
  if ( cont ) to = ((StEvent *)cont)->fgtCollection();
  return StHitIter::Reset( to );
}
// -------------------------------------------------------------------------------
Int_t StFgtHitIter::GetSize() const
{
  return 1; // note mispelling in StEvent
}
// -------------------------------------------------------------------------------
const TObject *StFgtHitIter::GetObject( Int_t idx ) const
{
  return (const TObject *)((StFgtCollection *)fCont)->getPointCollection();
}
// -------------------------------------------------------------------------------
const TObject *StFgtPointHitIter::GetObject( Int_t idx ) const
{
  return (const TObject *)((StFgtPointCollection *)fCont)->getPointVec().at(idx);
}
// -------------------------------------------------------------------------------
Int_t StFgtPointHitIter::GetSize() const
{
  return (Int_t)((StFgtPointCollection *)fCont)->getPointVec().size();
} 
// -------------------------------------------------------------------------------

//_______PXL_______PXL_______PXL_______PXL_______PXL_______PXL_______PXL_______PXL
//________________________________________________________________________________
#include "StEvent/StPxlHitCollection.h"
#include "StEvent/StPxlHit.h"
#include "StEvent/StPxlLadderHitCollection.h"
#include "StEvent/StPxlSectorHitCollection.h"
#include "StEvent/StPxlSensorHitCollection.h"
//Sector/Ladder/Sensor/Hit


//..............................................................................
class StPxlHitIter : public StHitIter {
public:
                StPxlHitIter();
virtual        ~StPxlHitIter(){;}
virtual const TObject *Reset(const TObject *cont);
virtual const TObject *GetObject (int idx) const;
virtual           int  GetSize () const;
  StDetectorId DetectorId() const {return kPxlId;}
protected:
};
//..............................................................................
class StPxlSectorHitIter : public StHitIter {
public:
                StPxlSectorHitIter(){;}
virtual        ~StPxlSectorHitIter(){;}
virtual const TObject *GetObject (int idx) const;
virtual           int  GetSize () const;
protected:
};
//..............................................................................
class StPxlLadderHitIter : public StHitIter {
public:
                StPxlLadderHitIter(){;}
virtual        ~StPxlLadderHitIter(){;}
virtual const TObject *GetObject (int idx) const;
virtual           int  GetSize () const;
protected:
};
//..............................................................................
class StPxlSensorHitIter : public StHitIter {
public:
                StPxlSensorHitIter(){;}
virtual        ~StPxlSensorHitIter(){;}
virtual const TObject *GetObject (int idx) const;
virtual           int  GetSize () const;
protected:
};
//________________________________________________________________________________
StPxlHitIter::StPxlHitIter()
{
  StHitIter *iter = this,*jter=0;
  iter->SetDowIter((jter=new StPxlSectorHitIter())); iter=jter;
  iter->SetDowIter((jter=new StPxlLadderHitIter())); iter=jter;
  iter->SetDowIter((jter=new StPxlSensorHitIter ()));
}
//________________________________________________________________________________
const TObject *StPxlHitIter::Reset(const TObject *cont)
{
  const StPxlHitCollection *to = 0;
  if (cont) to = ((StEvent*)cont)->pxlHitCollection();
  return StHitIter::Reset(to);
}
//________________________________________________________________________________
const TObject *StPxlHitIter::GetObject (int idx) const
{
  return (const TObject*)((StPxlHitCollection*)fCont)->sector(idx);
}
//________________________________________________________________________________
const TObject *StPxlSectorHitIter::GetObject (int idx) const
{
  return (const TObject*)((StPxlSectorHitCollection*)fCont)->ladder(idx);
}
//________________________________________________________________________________
int StPxlHitIter::GetSize () const
{
  return ((StPxlHitCollection*)fCont)->numberOfSectors();
}
//________________________________________________________________________________
int StPxlSectorHitIter::GetSize () const
{
  return ((StPxlSectorHitCollection*)fCont)->numberOfLadders();
}
//________________________________________________________________________________
const TObject *StPxlLadderHitIter::GetObject (int idx) const
{
  return (const TObject*)((StPxlLadderHitCollection*)fCont)->sensor(idx);
}
//________________________________________________________________________________
int StPxlLadderHitIter::GetSize () const
{
  return ((StPxlLadderHitCollection*)fCont)->numberOfSensors();
}
//________________________________________________________________________________
const TObject *StPxlSensorHitIter::GetObject (int idx) const
{
  return (const TObject*)((StPxlSensorHitCollection*)fCont)->hits().at(idx);
}
//________________________________________________________________________________
int StPxlSensorHitIter::GetSize () const
{
  return ((StPxlSensorHitCollection*)fCont)->hits().size();
}


//________________________________________________________________________________
//_______EVENT_____EVENT_____EVENT_____EVENT_____EVENT_____EVENT_____EVENT_____TOF
//________________________________________________________________________________
int StEventHitIter::AddDetector(StDetectorId detId)
{
   switch ((int)detId) {
   
   case kTpcId: Add(new StTpcHitIter());break;
   case kSvtId: Add(new StSvtHitIter());break;
   case kSsdId: Add(new StSsdHitIter());break;
   case kFtpcWestId:; 
   case kFtpcEastId:
                Add(new StFtpcHitIter());break;
   case kPxlId: Add(new StPxlHitIter()) ;break;
   case kIstId: Add(new StIstHitIter()) ;break; 
     /* case kFgtId: n.b. This will be removing the RnD version of the FGT */ 
   case kFmsId: 
     Add(new StRnDHitIter(detId));break;
     
   case kTofId: Add(new StTofHitIter()) ;break;
   case kEtrId: Add(new StEtrHitIter()) ;break;
   case kFgtId: Add(new StFgtHitIter()) ;break;
       
     
   default: printf("StEventHitIter::AddDetector: No iterator for detectorId=%d",(int)detId);
     assert(0 && "No iterator for detectorId");
     return 1;
   }
  return 0;
}
