/***********************************************************************
 *
 * $Id: StXiMc.hh,v 3.0 2000/07/14 12:56:50 genevb Exp $
 * $Log: StXiMc.hh,v $
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

  ClassDef(StXiMc,3)
};

#endif
