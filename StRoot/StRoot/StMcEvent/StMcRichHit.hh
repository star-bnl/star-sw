/***************************************************************************
 *
 * $Id: StMcRichHit.hh,v 2.9 2011/10/17 00:24:00 fisyak Exp $
 * $Log: StMcRichHit.hh,v $
 * Revision 2.9  2011/10/17 00:24:00  fisyak
 * Add time of flight for hits
 *
 * Revision 2.8  2005/09/28 21:30:15  fisyak
 * Persistent StMcEvent
 *
 * Revision 2.7  2005/07/06 20:05:28  calderon
 * Remove forward declaration of StThreeVectorF, use #include, and only in
 * StMcHit base class.  StThreeVectorF is not a class anymore, it is now
 * only a typedef, only template version of StThreeVector exists now.
 *
 * Revision 2.6  2005/01/27 23:40:47  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.5  2000/06/06 02:58:41  calderon
 * Introduction of Calorimeter classes.  Modified several classes
 * accordingly.
 *
 * Revision 2.4  2000/05/26 21:42:11  calderon
 * Added volumeId() method.
 *
 * Revision 2.3  2000/05/05 15:25:44  calderon
 * Reduced dependencies and made constructors more efficient
 *
 * Revision 2.2  2000/04/17 23:01:15  calderon
 * Added local momentum to hits as per Lee's request
 *
 * Revision 2.1  2000/03/06 18:05:21  calderon
 * 1) Modified SVT Hits storage scheme from layer-ladder-wafer to
 * barrel-ladder-wafer.
 * 2) Added Rich Hit class and collection, and links to them in other
 * classes.
 *
 *
 **************************************************************************/
#ifndef StMcRichHit_hh
#define StMcRichHit_hh

#include "StMcHit.hh"
#include "tables/St_g2t_rch_hit_Table.h"

class StMcRichHit : public StMcHit {
public:
  StMcRichHit() {}
  StMcRichHit(const StThreeVectorF& x,const StThreeVectorF& p,
	     Float_t de = 0, Float_t ds = 0, Float_t tof = 0, Long_t k = 0, Long_t volId = 0, StMcTrack* parent=0) : 
    StMcHit(x,p,de,ds,tof,k,volId,parent) {}
  StMcRichHit(g2t_rch_hit_st* pt) : 
    StMcHit(StThreeVectorF(pt->x[0], pt->x[1], pt->x[2]),
	    StThreeVectorF(pt->p[0], pt->p[1], pt->p[2]), 
	    pt->de, pt->ds, pt->tof, pt->id, pt->volume_id, 0) {}
  ~StMcRichHit() {}

  UShort_t  pad() const {return (mVolumeId & 0xff);}  // first 8 bits
  UShort_t  row() const {return ( (mVolumeId>>8) & 0xff);}  // second 8 bits
private:
    ClassDef(StMcRichHit,1)
};

ostream&  operator<<(ostream& os, const StMcRichHit&);

#endif
