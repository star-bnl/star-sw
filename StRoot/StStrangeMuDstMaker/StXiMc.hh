/***********************************************************************
 *
 * $Id: StXiMc.hh,v 3.1 2000/07/14 21:28:34 genevb Exp $
 * $Log: StXiMc.hh,v $
 * Revision 3.1  2000/07/14 21:28:34  genevb
 * Added V0Mc index for XiMc, fixed bug with entries for XiMc, cleaned up controllers
 *
 * Revision 3.0  2000/07/14 12:56:50  genevb
 * Revision 3 has event multiplicities and dedx information for vertex tracks
 *
 * Revision 2.0  2000/06/05 05:19:47  genevb
 * New version of Strangeness micro DST package
 *
 *
 ***********************************************************************
 *
 * Description: Monte Carlo Xi micro dst class
 *
 ***********************************************************************/
#ifndef  STAR_StXiMc
#define  STAR_StXiMc
#include "StODMc.hh"


class StXiMc : public StODMc {
public:
  StXiMc();
  StXiMc(StMcVertex*, StMcTrack*);
  ~StXiMc();
  Int_t V0Index();
  void SetV0Index(Int_t index);
protected:
  Int_t v0;
  ClassDef(StXiMc,3)
};

inline Int_t StXiMc::V0Index() { return v0; }
inline void  StXiMc::SetV0Index(Int_t index) { v0=index; }

#endif
