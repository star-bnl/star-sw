/*!
 * \class StKinkMc
 * \author Gene Van Buren
 *
 *               Monte Carlo Kink micro dst class
 *
 */

#ifndef  STAR_StKinkMc
#define  STAR_StKinkMc
#include "StKinkBase.hh"

class StMcVertex;
class StMcTrack;

class StKinkMc : public StKinkBase {  // StKinkBase inherits StKinkI methods
public:
  StKinkMc();
  StKinkMc(StMcVertex*, StMcTrack*);
  virtual ~StKinkMc();

  Int_t decayMode() const;
  void SetHitInfo(Int_t commonHits);
  
  Int_t simTpcHits() const;
  Int_t commonTpcHits() const;

  Int_t parentCharge() const;
  Int_t daughterCharge() const;

protected:
  Int_t mSimTpcHits;
  Int_t mCommonTpcHits;
  
  Int_t mDecayMode;

  ClassDef(StKinkMc,5)
};

inline Int_t StKinkMc::decayMode() const
             { return mDecayMode; }
inline void  StKinkMc::SetHitInfo(Int_t commonHits) 
             { mCommonTpcHits = commonHits; }
inline Int_t StKinkMc::commonTpcHits() const
             { return mCommonTpcHits; }
inline Int_t StKinkMc::simTpcHits() const
             { return mSimTpcHits; }
inline Int_t StKinkMc::daughterCharge() const
             { return parentCharge(); }

#endif


/***********************************************************************
 * $Id: StKinkMc.hh,v 3.3 2003/06/01 04:25:19 genevb Exp $
 * $Log: StKinkMc.hh,v $
 * Revision 3.3  2003/06/01 04:25:19  genevb
 * Update ClassDef version for altered inheritance
 *
 * Revision 3.2  2003/05/30 21:20:19  genevb
 * doxygen savvy, encoding of FTPC mults, change virtual funcs
 *
 * Revision 3.1  2001/05/04 20:15:14  genevb
 * Common interfaces and reorganization of components, add MC event info
 *
 * Revision 3.0  2000/07/14 12:56:48  genevb
 * Revision 3 has event multiplicities and dedx information for vertex tracks
 *
 * Revision 2.0  2000/06/05 05:19:40  genevb
 * New version of Strangeness micro DST package
 *
 ***********************************************************************/

