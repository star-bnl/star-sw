/***************************************************************************
 *
 * $Id: StMcSvtHit.hh,v 2.14 2012/03/01 16:48:29 perev Exp $
 * $Log: StMcSvtHit.hh,v $
 * Revision 2.14  2012/03/01 16:48:29  perev
 * method Browse() added
 *
 * Revision 2.13  2011/10/17 00:24:01  fisyak
 * Add time of flight for hits
 *
 * Revision 2.12  2005/11/22 21:44:52  fisyak
 * Add compress Print for McEvent, add Ssd collections
 *
 * Revision 2.11  2005/09/28 21:30:15  fisyak
 * Persistent StMcEvent
 *
 * Revision 2.10  2005/01/27 23:40:48  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.9  2000/06/06 02:58:41  calderon
 * Introduction of Calorimeter classes.  Modified several classes
 * accordingly.
 *
 * Revision 2.8  2000/05/05 15:25:44  calderon
 * Reduced dependencies and made constructors more efficient
 *
 * Revision 2.7  2000/04/19 14:34:48  calderon
 * More corrections for the SSD, thanks Helen
 *
 * Revision 2.6  2000/04/18 23:46:12  calderon
 * Fix bug in reurning barrel number
 * Enumerations for the Max barrels, ladders & wafers modified for
 * SSD inclusion in current scheme.
 *
 * Revision 2.5  2000/04/18 22:55:28  calderon
 * Functions to access the volume Id
 * Added volume Id to output of operator<<
 *
 * Revision 2.4  2000/01/18 20:52:31  calderon
 * Works with CC5
 *
 * Revision 2.3  1999/12/14 07:04:49  calderon
 * Numbering scheme as per SVT request.
 *
 * Revision 2.2  1999/12/03 00:51:52  calderon
 * Tested with new StMcEventMaker.  Added messages for
 * diagnostics.
 *
 * Revision 2.1  1999/11/19 19:06:33  calderon
 * Recommit after redoing the files.
 *
 * Revision 2.0  1999/11/17 02:12:16  calderon
 * Completely revised for new StEvent
 *
 * Revision 1.4  1999/09/24 01:23:16  fisyak
 * Reduced Include Path
 *
 * Revision 1.3  1999/09/23 21:25:52  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *
 **************************************************************************/
#ifndef StMcSvtHit_hh
#define StMcSvtHit_hh

#include "StMcHit.hh"
#include "tables/St_g2t_svt_hit_Table.h"

class StMcSvtHit : public StMcHit {
public:
  StMcSvtHit() {}
  StMcSvtHit(const StThreeVectorF& x,const StThreeVectorF& p,
	     Float_t de = 0, Float_t ds = 0, Float_t tof = 0, Long_t k = 0, Long_t volId = 0, StMcTrack* parent=0) : 
    StMcHit(x,p,de,ds,tof,k,volId,parent) {}
  StMcSvtHit(g2t_svt_hit_st* pt) : 
    StMcHit(StThreeVectorF(pt->x[0], pt->x[1], pt->x[2]),
	    StThreeVectorF(pt->p[0], pt->p[1], pt->p[2]), 
	    pt->de, pt->ds, pt->tof, pt->id, pt->volume_id, 0) {}
  ~StMcSvtHit() {}
  ULong_t layer()  const {return  (mVolumeId%10000)/1000;}      // layer=[1,6] with SSD [1-8]
  ULong_t ladder() const {return   (mVolumeId)%100;}   // ladder=[1-8] with SSD [1-20]
  ULong_t wafer()  const {return   mVolumeId%10000 < 7101 ?  ((mVolumeId)%1000)/100 :  (((mVolumeId%10000)/100)-70);}  // wafer=[1-7] with SSD [1-16]
  ULong_t barrel() const {return  (layer()+1)/2; }    // barrel=[1-3] with SSD [1-4]
  ULong_t hybrid() const {return  0; } 
  virtual void Print(Option_t *option="") const; // *MENU* 
protected:
    ClassDef(StMcSvtHit,2)
};

ostream&  operator<<(ostream& os, const StMcSvtHit&);

#endif
