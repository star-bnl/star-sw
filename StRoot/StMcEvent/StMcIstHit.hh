/***************************************************************************
 *
 * $Id: StMcIstHit.hh,v 2.13 2015/03/13 18:44:58 perev Exp $
 * $Log: StMcIstHit.hh,v $
 * Revision 2.13  2015/03/13 18:44:58  perev
 * Roll back
 *
 * Revision 2.11  2012/12/18 21:01:18  perev
 * Ist development (Jonathan)
 *
 * Revision 2.10  2011/10/17 00:24:00  fisyak
 * Add time of flight for hits
 *
 * Revision 2.9  2009/02/06 15:38:13  fisyak
 * Jonathan: decoding for upgr15 geometry
 *
 * Revision 2.8  2006/10/23 21:13:46  calderon
 * Updates to layer(), wafer() and side() methods from Willie L.
 *
 * Revision 2.7  2005/11/22 21:44:52  fisyak
 * Add compress Print for McEvent, add Ssd collections
 *
 * Revision 2.6  2005/09/28 21:30:15  fisyak
 * Persistent StMcEvent
 *
 * Revision 2.5  2005/07/19 20:07:34  calderon
 * Addition of default constructor, including base class StMcHit constructor.
 * Bracket calls to StMemoryPool inside #ifdef.
 *
 * Revision 2.4  2005/07/06 20:05:28  calderon
 * Remove forward declaration of StThreeVectorF, use #include, and only in
 * StMcHit base class.  StThreeVectorF is not a class anymore, it is now
 * only a typedef, only template version of StThreeVector exists now.
 *
 * Revision 2.3  2005/05/12 22:38:30  potekhin
 * Willie: Added function wafer() to return wafer number (1-7,1-10,1-13 for layers 1,2,3)
 * and side() to return ladder side (1=inner,2=outer)
 *
 * Revision 2.2  2005/05/11 20:54:28  calderon
 * Added persistency: ClassImp, ClassDef and inheritance from StObject.
 *
 * Revision 2.1  2004/09/14 05:00:29  calderon
 * Added support for Ist, Ssd and changes to Pixel, from "El Kai".
 *
 * Revision 2.1  2003/08/20 18:50:21  calderon
 * Addition of Tof classes and Pixel classes.  Modified track, event, and
 * container code to reflect this.
 * Fix bug in StMcVertex and in clearing of some hit collections.
 *
 *
 **************************************************************************/
#ifndef StMcIstHit_hh
#define StMcIstHit_hh

#include "StMcHit.hh"
#include "tables/St_g2t_ist_hit_Table.h" 


class StMcIstHit : public StMcHit {
public:
  StMcIstHit() : StMcHit() {}
  StMcIstHit(const StThreeVectorF& x,const StThreeVectorF& p,
	     Float_t de = 0, Float_t ds = 0, Float_t tof = 0, Long_t k = 0, Long_t volId = 0, StMcTrack* parent=0) : 
    StMcHit(x,p,de,ds,tof,k,volId,parent) {}
  StMcIstHit(g2t_ist_hit_st* pt) : 
    StMcHit(StThreeVectorF(pt->x[0], pt->x[1], pt->x[2]),
	    StThreeVectorF(pt->p[0], pt->p[1], pt->p[2]), 
	    pt->de, pt->ds, pt->tof, pt->id, pt->volume_id, 0) {}
  ~StMcIstHit() {}
  
#ifdef POOL
  void* operator new(size_t)     { return mPool.alloc(); }
  void  operator delete(void* p) { mPool.free(p); }
#endif
  
  ULong_t layer()  const {return 1;} // 
  ULong_t ladder() const {return mVolumeId/1000000 -1;}
  ULong_t wafer()  const {return  (mVolumeId%1000000)/10000;} 
  ULong_t side()   {return (mVolumeId%10);} //1=inner; 2=outer;
  
  // Willie: Added function wafer() to return wafer number (1-12)
  // unsigned long wafer() const;
  // Willie: Added function wafer() to return wafer number (1-10,1-13 for layers 1,2)
  // and side() to return ladder side (1=inner,2=outer)
  // ULong_t wafer() {return ((mVolumeId/100)%20);}
  // ULong_t side() {return (mVolumeId%10);} //1=inner; 2=outer;

  virtual void Print(Option_t *option="") const; // *MENU* 
  
private:
  
  ClassDef(StMcIstHit,2)
};

ostream&  operator<<(ostream& os, const StMcIstHit&);
#endif
