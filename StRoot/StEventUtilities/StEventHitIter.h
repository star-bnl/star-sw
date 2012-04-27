// Author: Victor Perev   08/04/01
#ifndef ROOT_StEventHitIter_HH
#define ROOT_StEventHitIter_HH


//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StEventHitIter                                                       //
//                                                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "Rtypes.h"
#include "TObject.h"
#include "StEnumerations.h"
#include "vector"
class StEvent;
class StHit;


//namespace QWERTY {
//..............................................................................
class StHitIter : public TObject
{
public: 
    		StHitIter();
virtual        ~StHitIter();
virtual const TObject *Reset(const TObject *cont);
virtual const TObject *operator++();
virtual const TObject *Get () const;
virtual const TObject *GetObject (int idx) const=0;
virtual           int  GetSize   ()        const=0;
virtual  StDetectorId  DetectorId() const {return kUnknownId;}
                 void  SetDowIter(StHitIter *it) {fDowIter=it;}
virtual          void  UPath(ULong64_t &upath) const;
protected:

const TObject *fCont;
StHitIter  *fDowIter;
int fJIter;
int fNIter;
};
//..............................................................................
class StHitIterGroup : public StHitIter {
public: 
    		StHitIterGroup();
virtual        ~StHitIterGroup();
virtual const TObject *Reset(const TObject *cont);
virtual const TObject *operator++();
virtual const TObject *Get () const;
virtual const StHit *operator*() const 		{return (StHit*)Get();}
virtual const TObject *GetObject (int idx) const;
virtual           int  GetSize   () const 	{return fGroup.size();};
virtual StDetectorId DetectorId()   const 	{return fDetectorId;}
                void UPath(ULong64_t &upa) const;
              UInt_t UPath() const;
virtual void Add(StHitIter* iter);
protected:
mutable StDetectorId fDetectorId;
std::vector<StHitIter*> fGroup;
};

//..............................................................................
class StEventHitIter : public StHitIterGroup {
public:
  StEventHitIter(const TObject *ev=0){if (ev) Reset(ev);}
  virtual ~StEventHitIter(){;}
  int   AddDetector(StDetectorId detId);
  int   AddDetector(const char *name);
private:
};

//}// QWERTY
#endif //ROOT_StEventHelper
