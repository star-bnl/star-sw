/***************************************************************************
 *
 * $Id: StRichMixerMaker.h,v 1.2 2003/09/10 19:47:29 perev Exp $
 *
 * Author: bl 
 ***************************************************************************
 *
 * Description: RICH offline software
 *              Embedding software
 * This module is to run in the embedding chain
 * which is directed in the bfcMixer.C
 *
 ***************************************************************************
 * $Log: StRichMixerMaker.h,v $
 * Revision 1.2  2003/09/10 19:47:29  perev
 * ansi corrs
 *
 * Revision 1.1  2001/08/27 16:43:43  lasiuk
 * Initial Revision
 *
 **************************************************************************/
#ifndef StRichMixerMaker_hh
#define StRichMixerMaker_hh

#ifndef StMaker_H
#include "StMaker.h"
#endif

#define rCH_DEBUG 1
#define rCH_HISTOGRAM 1

#ifdef RCH_HISTOGRAM
#include "TFile.h"
#include "TH1.h"
#include "TNtuple.h"
#endif
#ifndef __CINT__
#include "StRrsMaker/StRichSinglePixel.h"
#include "StRrsMaker/StRichSingleMCPixel.h"
#endif
// forward StEvent declaration
class StEvent;
class StRichCollection;

class StDAQReader;
class StRichReaderInterface;
class StRichGeometryDb;
class StRichClusterAndHitFinder;
class StRichHit;
class StRichSimpleHit;
class StRichSimpleHitCollection;
class StRichSinglePixel;


class StRichMixerMaker : public StMaker {
    
public: 
    StRichMixerMaker(const char *name="RichMixer");
    virtual       ~StRichMixerMaker();
    virtual Int_t  Init();
    virtual Int_t  Make();
    virtual void   PrintInfo();
    virtual Int_t  Finish();

//     int  adcDecoder(unsigned long code, unsigned long* pad, unsigned long* row, unsigned long* adc);
    void setPedestalSubtract(int v,const char *file);

protected:
    void fillStEvent();
    
private:
    St_DataSet*            mTheRichDaqData;//!
    StDAQReader*           mTheDaqDataReader;//!
    StRichReaderInterface* mSimStreamReader;//!
    StRichReaderInterface* mDaqStreamReader;//!

    
    int mDaq;  // looking for DAQ data or not?
    int mPads;  // number of pads
    int mRows;
    int mNumberOfPads;
    int mEventNumber;
    double mSaturatedValue;

    // flags
    short mPixelCollectionPresent;
    short mClusterCollectionPresent;
    short mHitCollectionPresent;

    //pedestal
    bool  mPedestalSubtract;
    float mPedestal[160][96];
    float mSigma[160][96];
    const char* mPedestalFile;
    
    // From StEvent
    StRichCollection*           mTheRichCollection;//!
    //StRichCollection*           mTheEmbeddedEvent;//!

    Bool_t drawinit;
    
#ifdef RCH_HISTOGRAM
    TFile* mTupleFile;    //!
    TNtuple* mPadPlane;   //!
    float mRawData[4];    //!
#endif
virtual const char *GetCVS() const	{
    static const char cvs[]=
	"Tag $Name:  $ $Id: StRichMixerMaker.h,v 1.2 2003/09/10 19:47:29 perev Exp $ built "__DATE__" "__TIME__ ;
    return cvs;
}
public:
    virtual void SetMode(Int_t mode=0) {
	m_Mode = mode;
    }
private:
    // the following is a ROOT macro  that is needed in all ROOT code
    ClassDef(StRichMixerMaker,0)   //StAF chain virtual base class for Makers
	};

#endif 
