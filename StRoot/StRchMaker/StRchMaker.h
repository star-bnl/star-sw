/***************************************************************************
 *
 * $Id: StRchMaker.h,v 1.10 2000/05/18 21:57:19 lasiuk Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: RICH offline software
 *              StRchMaker.h - ROOT/STAR Maker for offline chain.
 ***************************************************************************
 * $Log: StRchMaker.h,v $
 * Revision 1.10  2000/05/18 21:57:19  lasiuk
 * dev patch
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
#include "StMaker.h"
#endif
#define rCH_WITH_PAD_MONITOR 1

#ifndef StMaker_H
#include "StMaker.h"
#include <vector>

using std::vector;
#endif
#endif
#ifdef RCH_HISTOGRAM
#include "TFile.h"

// forward StEvent declaration
class StEvent;
class StRichCollection;

class StDAQReader;
class StRichReaderInterface;
class StRichClusterAndHitFinder;
class StRichSimpleHitCollection;
class StRichSinglePixel;


class StRchMaker : public StMaker {
    
private:
    Bool_t drawinit;
    
    StRchMaker(const char *name="rch", int daq=0, int matrix=0, int stevent=0);
    
public: 
    StRchMaker(const char *name="rch", int daq=0, int matrix=0, int cf=0);
    virtual       ~StRchMaker();
    virtual Int_t  Init();
    virtual Int_t  Make();
    virtual void   PrintInfo();
    virtual Int_t  Finish();
protected:
    void fillStEvent();
    
    St_DataSet*            mTheRichData;
    StDAQReader*           mTheDataReader;//!
    StRichReaderInterface* mTheRichReader;//!
    St_DataSet*            mTheRichData;//!

    StRichGeometryDb*      mGeometryDb;//!
    
    int mDaq;  // looking for DAQ data or not?
    int mPads;  // number of pads
    int mRows;
    int mWithDstPixels;
    
    
    StRichClusterAndHitFinder*  mClusterFinder;//!
    
    StEvent*                    mEvent;//!
    StRichCollection*           mTheRichCollection;//!

#ifdef RCH_HISTOGRAM
    TFile* mRchNTupleFile; //!
    TNtuple* mPadPlane;   //!
    TNtuple* mClusters;//!
    TNtuple* mHits;//!
    float mRawData[4];    //!
    float mCluster[5];//!
    float mHit[3];//!
    
    TH1F* mcc;//!
    TH1F* mmc;//!
    TH1F* mrms;//!
    TH1F* mpad;//!
    TH1F* mqpad;//!
    TH1F* mcratio;//!
    // hits
    TH1F* mhc;//!
    TH1F* mhmc;//!
	static const char cvs[]="Tag $Name:  $ $Id: StRchMaker.h,v 1.10 2000/05/18 21:57:19 lasiuk Exp $ built "__DATE__" "__TIME__ ;
#endif
    virtual const char *GetCVS() const	{
    
	return cvs;
    }
public:
    virtual void SetMode(Int_t mode=0) {
	m_Mode = mode;
    
	// variable in St_tpcdaq_Maker, so reverse it. 
    }
private:
    // the following is a ROOT macro  that is needed in all ROOT code
    ClassDef(StRchMaker, 1)   //StAF chain virtual base class for Makers
	};

inline void StRchMaker::setUseMatrix(int v) {mUseMatrix = v;}

#endif 
#endif /* __ROOT__ */
