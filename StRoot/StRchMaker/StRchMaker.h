/***************************************************************************
 *
 * $Id: StRchMaker.h,v 1.5 1999/07/21 13:33:56 gans Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: RICH offline software
 *              StRchMaker.h - ROOT/STAR Maker for offline chain.
 ***************************************************************************
 *
 * $Log: StRchMaker.h,v $
 * Revision 1.5  1999/07/21 13:33:56  gans
 * *** empty log message ***
 *
 * Revision 1.5  1999/07/20 00:00:00 gans
 * added test histogram
 * Revision 1.4  1999/07/15 13:57:23  perev
 * cleanup
 * Revision 1.3  1999/03/20 22:00:19  perev
 * new maker schema
 *
 * Revision 1.2  1999/02/12 17:29:02  fisyak
 * Make it compiled
 *
 * Revision 1.1  1999/02/12 00:12:33  lyons
 * Trail version... untested
 *
 *
 * Fills new dst_rch_pixel;
 * debug macros;
 * used in first DAQ data
#endif
#define rCH_WITH_PAD_MONITOR 1

#ifndef StMaker_H
#ifndef ROOT_TH1

#endif

#ifndef ROOT_TH2
#include "TH2.h"
#endif

#ifndef ROOT_TH3
#include "TH3.h"
#endif
#endif
class StRichReaderInterface;

class StRichSimpleHitCollection;
   Bool_t drawinit;
   TH3S * hist;        //!



    
  StRchMaker(const char *name="rch");
  virtual       ~StRchMaker();
  virtual Int_t  Init();
  virtual Int_t  Make();
  virtual void   PrintInfo();
  virtual Int_t  Finish();
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StRchMaker.h,v 1.5 1999/07/21 13:33:56 gans Exp $ built "__DATE__" "__TIME__ ; return cvs;}
    StRchMaker(const char *name="rch", int daq=0, int matrix=0, int cf=0);

// the following is a ROOT macro  that is needed in all ROOT code
	m_Mode = mode;
};

    // the following is a ROOT macro  that is needed in all ROOT code



	};

inline void StRchMaker::setUseMatrix(int v) {mUseMatrix = v;}

#endif 
#endif /* __ROOT__ */
