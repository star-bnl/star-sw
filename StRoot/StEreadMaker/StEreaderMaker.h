/***************************************************************************
 *
 * $Id: StEreaderMaker.h,v 1.1 2001/07/17 00:05:30 perev Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: RICH offline software
 *              StRchMaker.h - ROOT/STAR Maker for offline chain.
 ***************************************************************************
 * $Log: StEreaderMaker.h,v $
 * Revision 1.1  2001/07/17 00:05:30  perev
 * new maker
 *
 * Revision 2.2  2001/02/07 16:06:39  lasiuk
 * adc decoder modified for 11bit check
 * this-> for internal calls
 * hit ntuple extended to include pads in hit
 *
 * Revision 2.1  2000/09/29 18:59:33  lasiuk
 * addition of software monitor
 * write flags in persistent hit (reservedLong)
 * Histodefintions
 *
 * Revision 2.0  2000/08/09 16:22:11  gans
 * Cosmetic Changes. Naming convention for TDrawable objects
 *
 * Revision 1.13  2000/06/13 18:26:13  dunlop
 * Modified SetMode (public)
 *
 * Revision 1.12  2000/05/25 21:35:32  fisyak
 * Make rootcint happy
 *
 * Revision 1.11  2000/05/23 16:49:55  lasiuk
 * writing to StEvent/StRichCollection
 *
 * Revision 1.10  2000/05/18 21:57:19  lasiuk
 * dev patch
 *
 * Revision 1.9  2000/05/01 20:22:50  dunlop
 * Added in SetMode
 *
 * Revision 1.8  2000/04/05 21:24:28  lasiuk
 * with cf
 *
 * Revision 1.7  2000/02/14 20:50:29  lasiuk
 * use DAQ/sim interface with a switch settable at the c'tor
 *
 * Revision 1.6  2000/01/11 21:18:04  lasiuk
 * Fills new dst_rch_pixel;
 * debug macros;
 * used in first DAQ data
 *
 **************************************************************************/
#ifdef __ROOT__
#ifndef STAR_StEreadMaker
#define STAR_StEreadMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif

//#define rCH_DEBUG 1
//#define rCH_HISTOGRAM 1

//#include "StRichDisplayActivate.h"

#ifndef __CINT__
#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif
#endif

//#ifdef RCH_HISTOGRAM
//#include "TFile.h"
//#include "TH1.h"
//#include "TNtuple.h"
//#endif
class StDAQReader;
class StEMCReader;


class StEreadMaker : public StMaker {
    
private:
    
protected:
    
public: 
    StEreadMaker(const char *name="Eread", int daq=0);
    virtual       ~StEreadMaker();
    virtual Int_t  Init();
    virtual Int_t  Make();
    virtual void   PrintInfo();
    virtual Int_t  Finish();

protected:
    
private:
    StDAQReader*           mTheDataReader;//!
    StEMCReader* mTheEmcReader;//!
    St_DataSet*            mTheEmcData;//!
    
    int mDaq;  // looking for DAQ data or not?

virtual const char *GetCVS() const	{
    static const char cvs[]=
	"Tag $Name:  $ $Id: StEreaderMaker.h,v 1.1 2001/07/17 00:05:30 perev Exp $ built "__DATE__" "__TIME__ ;
    return cvs;
}
public:
private:
    // the following is a ROOT macro  that is needed in all ROOT code
    ClassDef(StEreadMaker, 1)   
	};

#endif 
#endif /* __ROOT__ */
