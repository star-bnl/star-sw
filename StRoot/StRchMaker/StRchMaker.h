/***************************************************************************
 *
 * $Id: StRchMaker.h,v 1.7 2000/02/14 20:50:29 lasiuk Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: RICH offline software
 *              StRchMaker.h - ROOT/STAR Maker for offline chain.
 ***************************************************************************
 * $Log: StRchMaker.h,v $
 * Revision 1.7  2000/02/14 20:50:29  lasiuk
 * use DAQ/sim interface with a switch settable at the c'tor
 *
 * Revision 1.8  2000/04/05 21:24:28  lasiuk
 * with cf
 *
 * Revision 1.7  2000/02/14 20:50:29  lasiuk
 * use DAQ/sim interface with a switch settable at the c'tor
 *
 * Fills new dst_rch_pixel;
 * debug macros;
 * used in first DAQ data
#define rCH_DEBUG
 **************************************************************************/
#include "StMaker.h"
#endif
#define rCH_WITH_PAD_MONITOR 1

#ifndef StMaker_H
#include "StMaker.h"
#include <vector>

using std::vector;
#endif
#endif
#include "TFile.h"

class StDAQReader;
class StRichReaderInterface;
class StRichClusterAndHitFinder;
class StRichSimpleHitCollection;
class StRichSinglePixel;


class StRchMaker : public StMaker {
    
    StRchMaker(const char *name="rch", int daq=0);
    Bool_t drawinit;
    
    StRchMaker(const char *name="rch", int daq=0, int matrix=0, int stevent=0);
    
public: 
    StRchMaker(const char *name="rch", int daq=0, int matrix=0, int cf=0);
    virtual Int_t  Make();
    virtual void   PrintInfo();
    virtual Int_t  Finish();
protected:
    St_DataSet*            mTheRichData;
    StDAQReader*           mTheDataReader;//!
    StRichReaderInterface* mTheRichReader;//!
    St_DataSet*            mTheRichData;//!

    StRichGeometryDb*      mGeometryDb;//!

    
    StRichClusterAndHitFinder*  mClusterFinder;//!
    

    TH1F* mqpad;//!
    TH1F* mcratio;//!
	static const char cvs[]="Tag $Name:  $ $Id: StRchMaker.h,v 1.7 2000/02/14 20:50:29 lasiuk Exp $ built "__DATE__" "__TIME__ ;
    TH1F* mhc;//!
    TH1F* mhmc;//!
	static const char cvs[]="Tag $Name:  $ $Id: StRchMaker.h,v 1.7 2000/02/14 20:50:29 lasiuk Exp $ built "__DATE__" "__TIME__ ;
public:
    virtual void SetMode(Int_t mode=0) {
	m_Mode = mode;
    
    // the following is a ROOT macro  that is needed in all ROOT code
	};

inline void StRchMaker::setUseMatrix(int v) {mUseMatrix = v;}

#endif 
#endif /* __ROOT__ */
