/***********************************************************************
 *
 * $Id: StKinkMc.hh,v 2.0 2000/06/05 05:19:40 genevb Exp $
 * $Log: StKinkMc.hh,v $
 * Revision 2.0  2000/06/05 05:19:40  genevb
 * New version of Strangeness micro DST package
 *
 *
 ***********************************************************************
 *
 * Description: Monte Carlo Kink micro dst class
 *
 ***********************************************************************/
#ifndef  STAR_StKinkMc
#define  STAR_StKinkMc
#include "StODMc.hh"


class StKinkMc : public StODMc {
public:
  StKinkMc();
  StKinkMc(StMcVertex*, StMcTrack*);
  ~StKinkMc();

  ClassDef(StKinkMc,1)
};

#endif
