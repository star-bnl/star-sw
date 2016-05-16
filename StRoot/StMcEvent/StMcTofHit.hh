/***************************************************************************
 *
 * $Id: StMcTofHit.hh,v 2.7 2016/05/16 23:47:09 perev Exp $
 * $Log: StMcTofHit.hh,v $
 * Revision 2.7  2016/05/16 23:47:09  perev
 * Coverity fix
 *
 * Revision 2.6  2011/10/17 00:24:01  fisyak
 * Add time of flight for hits
 *
 * Revision 2.5  2005/11/22 21:44:52  fisyak
 * Add compress Print for McEvent, add Ssd collections
 *
 * Revision 2.4  2005/09/28 21:30:15  fisyak
 * Persistent StMcEvent
 *
 * Revision 2.3  2005/07/06 20:05:28  calderon
 * Remove forward declaration of StThreeVectorF, use #include, and only in
 * StMcHit base class.  StThreeVectorF is not a class anymore, it is now
 * only a typedef, only template version of StThreeVector exists now.
 *
 * Revision 2.2  2005/01/27 23:40:48  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.1  2003/08/20 18:50:21  calderon
 * Addition of Tof classes and Pixel classes.  Modified track, event, and
 * container code to reflect this.
 * Fix bug in StMcVertex and in clearing of some hit collections.
 *
 */
#ifndef StMcTofHit_hh
#define StMcTofHit_hh

#include "StMcHit.hh"
#include "tables/St_g2t_ctf_hit_Table.h"  

class StMcTofHit : public StMcHit {
public:
  StMcTofHit() {mStrack=0;}
  StMcTofHit(const StThreeVectorF& x,const StThreeVectorF& p,
	     Float_t de = 0, Float_t ds = 0, Float_t tof = 0, Long_t k = 0, Long_t volId = 0, StMcTrack* parent=0) : 
    StMcHit(x,p,de,ds,tof,k,volId,parent) {mStrack=0;}
  StMcTofHit(g2t_ctf_hit_st* pt, Float_t cl_x=0, Float_t cl_t=0): 
    StMcHit(StThreeVectorF(pt->x[0], pt->x[1], pt->x[2]),
	    StThreeVectorF(pt->p[0], pt->p[1], pt->p[2]), 
	    pt->de, pt->ds, pt->tof, pt->id, pt->volume_id, 0) {mStrack=0;}
  ~StMcTofHit() {}
  Float_t sTrack() const {return mStrack;}
  virtual void Print(Option_t *option="") const; // *MENU* 
private:
  Float_t               mStrack; 
  ClassDef(StMcTofHit,2)
};

ostream&  operator<<(ostream& os, const StMcTofHit&);

#endif
