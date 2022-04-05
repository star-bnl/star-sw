/***************************************************************************
 *
 * $Id: StMcHit.hh,v 2.14 2016/05/16 23:47:09 perev Exp $
 * $Log: StMcHit.hh,v $
 * Revision 2.14  2016/05/16 23:47:09  perev
 * Coverity fix
 *
 * Revision 2.13  2012/03/01 16:48:29  perev
 * method Browse() added
 *
 * Revision 2.12  2011/10/17 00:24:00  fisyak
 * Add time of flight for hits
 *
 * Revision 2.10  2005/11/22 21:44:52  fisyak
 * Add compress Print for McEvent, add Ssd collections
 *
 * Revision 2.9  2005/09/28 21:30:14  fisyak
 * Persistent StMcEvent
 *
 * Revision 2.8  2004/01/13 21:02:51  fisyak
 * Add inheritance from StObject
 *
 * Revision 2.7  2003/10/08 20:17:55  calderon
 * -using <iostream>, std::cout, std::ostream.
 * -changes in FTPC volume Id.
 *   o Causes changes in decoding of plane().
 *   o sector() is added.
 *   o print volumeId and sector() in the operator<<.
 *
 * Revision 2.6  2003/09/02 17:58:41  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 2.5  2000/06/06 02:58:41  calderon
 * Introduction of Calorimeter classes.  Modified several classes
 * accordingly.
 *
 * Revision 2.4  2000/05/05 15:25:43  calderon
 * Reduced dependencies and made constructors more efficient
 *
 * Revision 2.3  2000/04/17 23:01:15  calderon
 * Added local momentum to hits as per Lee's request
 *
 * Revision 2.2  1999/12/14 07:04:49  calderon
 * Numbering scheme as per SVT request.
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
 * Revision 1.3  1999/09/23 21:25:51  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *
 **************************************************************************/
#ifndef StMcHit_hh
#define StMcHit_hh

#include "StObject.h"
#include "Stiostream.h"
#include "StThreeVectorF.hh"
#include "tables/St_g2t_hits_Table.h"
#include "StMcTrack.hh"

class StMcHit : public StObject {
public:
  enum EMcHitBits {
    kMatched = BIT(23) // if hit has matched with reconstructed one
  };
  StMcHit()  : mPosition(0,0,0), mLocalMomentum(0,0,0), mdE(0),mdS(0),mTof(0),mKey(0),mVolumeId(0),mParentTrack(0) {}
  StMcHit(const StThreeVectorF& x,const StThreeVectorF& p,
	  Float_t de, Float_t ds, Float_t tof, Long_t k, Long_t volId, StMcTrack* parent=0)
    : mPosition(x), mLocalMomentum(p), mdE(de), mdS(ds), mTof(tof), mKey(k), mVolumeId(volId), mParentTrack(parent) {}
  StMcHit(g2t_hits_st* pt) : mPosition(pt->x[0],pt->x[1],pt->x[2]), mLocalMomentum(pt->p[0],pt->p[1],pt->p[2]),
			     mdE(pt->de), mdS(pt->ds), mTof(pt->tof), mKey(pt->id), mVolumeId(0),
			     mParentTrack(0) {}
    // StMcHit(const StSvtHit&);                  use default
    // const StMcHit & operator=(const StMcHit&);   use default
  virtual ~StMcHit() {}
    
  Int_t operator==(const StMcHit& h) const;
  Int_t operator!=(const StMcHit& h) const {return !(*this == h); }
    

  // "Get" Methods
  virtual const StThreeVectorF&      position() const { return mPosition;}
  virtual const StThreeVectorF& localMomentum() const { return mLocalMomentum;}
  virtual Float_t                            dE() const { return mdE; }
  virtual Float_t                            dS() const { return mdS; }
  virtual Float_t                           tof() const { return mTof; }
  virtual Long_t                            key() const { return mKey; }
  virtual Long_t                       volumeId() const { return mVolumeId; }
  virtual StMcTrack*              parentTrack() const { return mParentTrack; }	
  // "Set" Methods

  virtual void setPosition(const StThreeVectorF& val) { mPosition = val; }
  virtual void setLocalMomentum(const StThreeVectorF& val) { mLocalMomentum = val; }
  virtual void setdE(Float_t val) 	{ mdE  = val;}
  virtual void setdS(Float_t  val) 	{ mdS  = val;}
  virtual void setTof(Float_t tof) 	{ mTof = tof;}
  virtual void setKey(Long_t val) 	{ mKey = val;}
  virtual void setVolumeId(Long_t val) 	{ mVolumeId = val; }
  virtual void setParentTrack(StMcTrack* val) { mParentTrack = val; }
  virtual void Print(Option_t *option="") const; // *MENU* 
    
protected:
  StThreeVectorF       mPosition;
  StThreeVectorF       mLocalMomentum;
  Float_t              mdE;
  Float_t              mdS;
  Float_t              mTof;    
  Long_t               mKey;
  Long_t               mVolumeId;
  StMcTrack*           mParentTrack;
  ClassDef(StMcHit,2)
};
ostream&  operator<<(ostream& os, const StMcHit&);
#endif

