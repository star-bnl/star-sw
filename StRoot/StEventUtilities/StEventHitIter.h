// Author: Victor Perev   08/04/01
#ifndef ROOT_StEventHitIter_HH
#define ROOT_StEventHitIter_HH


//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StEventHelper                                                        //
//                                                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "Rtypes.h"
#include "StEnumerations.h"
#include "StHit.h"
class StEvent;
class StHit;


//..............................................................................
class StHitIter 
{
private:
  class StkCell_t {
  public:
    const void *mV;	//pointer to collection of objects
    int mN;		//size of collection
    int mJ; 		//current index of object in collection
  };
enum {kINI=0,kDOW=1,kHOR=2,kUPP=3,kHIT=4,kEND};
protected:
    StHitIter(const void *cont=0);
virtual const void *GetObj(const void *cont,int lev,int idx) const =0; 
virtual       int  GetSize(const void *cont,int lev        ) const =0; 
public:
virtual void Reset(const StEvent *evt=0);
virtual void Reset(const void *cont);
virtual StDetectorId DetectorId() const = 0;
virtual int          HitPlaneId() const{return 0;};
virtual int operator++();
virtual StHit *operator*();
virtual UInt_t UPath() const;
virtual const void *GetContainer(const StEvent *ev) const=0;
virtual void Print(const char *opt="");
protected:
const void *fCont;
int fKase;
int fLev;
StkCell_t fStk[10];
};

//..............................................................................
class StEventHitIter : public StHitIter {
  enum {kMaxIters=10};
public:
  StEventHitIter(const StEvent *ev);
  virtual ~StEventHitIter();
  void  Reset(const StEvent *evt=0);
  int   AddDetector(StDetectorId detId);
  int   AddDetector(const char *name);
  StDetectorId DetectorId() const;
  UInt_t      UPath() const;
  int    operator++();
  StHit *operator*();

protected:
  const void *GetObj (const void *cont,int lev,int idx) const{return 0;} 
        int   GetSize(const void *cont,int lev        ) const{return 0;} 
private:
  const void *GetContainer(const StEvent *ev) const{return 0;}
  void  Reset(const void *cont){assert(0);}

private:
  const StEvent *fStEvent;
  int    fJter,fNter;  
  StHitIter 	*fIter[kMaxIters];
};
#endif //ROOT_StEventHelper
