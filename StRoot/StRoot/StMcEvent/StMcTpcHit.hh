/***************************************************************************
 *
 * $Id: StMcTpcHit.hh,v 2.16 2016/09/18 22:43:02 fisyak Exp $
 * $Log: StMcTpcHit.hh,v $
 * Revision 2.16  2016/09/18 22:43:02  fisyak
 * increament version no.
 *
 * Revision 2.15  2016/09/18 22:41:39  fisyak
 * Add no. of primary electrons
 *
 * Revision 2.14  2016/07/26 15:10:13  jwebb
 * Initialize members in ctor / coverity
 *
 * Revision 2.13  2012/03/01 16:48:30  perev
 * method Browse() added
 *
 * Revision 2.12  2011/10/17 00:24:01  fisyak
 * Add time of flight for hits
 *
 * Revision 2.11  2005/11/22 21:44:52  fisyak
 * Add compress Print for McEvent, add Ssd collections
 *
 * Revision 2.10  2005/09/28 21:30:15  fisyak
 * Persistent StMcEvent
 *
 * Revision 2.9  2005/07/06 20:05:28  calderon
 * Remove forward declaration of StThreeVectorF, use #include, and only in
 * StMcHit base class.  StThreeVectorF is not a class anymore, it is now
 * only a typedef, only template version of StThreeVector exists now.
 *
 * Revision 2.8  2005/01/27 23:40:49  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.7  2000/06/06 02:58:42  calderon
 * Introduction of Calorimeter classes.  Modified several classes
 * accordingly.
 *
 * Revision 2.6  2000/05/05 15:25:44  calderon
 * Reduced dependencies and made constructors more efficient
 *
 * Revision 2.5  2000/03/06 18:05:23  calderon
 * 1) Modified SVT Hits storage scheme from layer-ladder-wafer to
 * barrel-ladder-wafer.
 * 2) Added Rich Hit class and collection, and links to them in other
 * classes.
 *
 * Revision 2.4  2000/01/18 20:52:31  calderon
 * Works with CC5
 *
 * Revision 2.3  1999/12/14 07:04:49  calderon
 * Numbering scheme as per SVT request.
 *
 * Revision 2.2  1999/12/03 00:51:53  calderon
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
 * Revision 1.3  1999/09/23 21:25:53  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *
 **************************************************************************/
#ifndef StMcTpcHit_hh
#define StMcTpcHit_hh

#include "StMcHit.hh"
#include "tables/St_g2t_tpc_hit_Table.h"  

class StMcTpcHit : public StMcHit {
public:
  StMcTpcHit() : StMcHit(),
		 mLgamma(0),
		 mAdc(0),
		 mMcl_x(0),
		 mMcl_t(0),
		 mnP(0)
  {}
//   StMcTpcHit(const StThreeVectorF& x,const StThreeVectorF& p,
// 	     Float_t de = 0, Float_t ds = 0, Float_t tof = 0, Long_t k = 0, Long_t volId = 0, StMcTrack* parent=0, 
// 	     Float_t adc = 0, Float_t cl_x = 0, Float_t cl_t = 0): 
//     StMcHit(x,p,de,ds,tof,k,volId,parent), mAdc(adc), mMcl_x(cl_x), mMcl_t(cl_t) {}
  StMcTpcHit(g2t_tpc_hit_st* pt): 
    StMcHit(StThreeVectorF(pt->x[0], pt->x[1], pt->x[2]),
	    StThreeVectorF(pt->p[0], pt->p[1], pt->p[2]), 
	    pt->de, pt->ds, pt->tof, pt->id, pt->volume_id, 0), 
    mLgamma(pt->lgam), mAdc(pt->adc), mMcl_x(pt->pad), mMcl_t(pt->timebucket), mnP(pt->np) {}
  virtual ~StMcTpcHit() {}
  ULong_t sector()     const { return (mVolumeId%10000)/100; }// 1-24
  ULong_t padrow()     const { return (mVolumeId%100); }      // 1-45

  ULong_t isDet()      const { return mVolumeId/100000; }     // pseudo pad row
  Float_t lgamma()     const { return mLgamma;}
  Float_t adc()        const { return mAdc;}
  Float_t pad()        const { return mMcl_x;}
  Float_t timeBucket() const { return mMcl_t;}
  
  virtual void Print(Option_t *option="") const; // *MENU* 
  
private:
  Float_t     mLgamma; //  ALOG10(GEKin/AMass) from g2t_tpc_hit
  Float_t     mAdc;        
  Float_t     mMcl_x;      /* average pad */
  Float_t     mMcl_t;      /* average timebucket */
  Int_t       mnP;         /* no. of primary electrons */
  ClassDef(StMcTpcHit,3)
};

ostream&  operator<<(ostream& os, const StMcTpcHit&);

#endif
