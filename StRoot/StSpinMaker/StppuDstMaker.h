/***************************************************************************
 *
 * $Id: StppuDstMaker.h,v 1.1 2002/01/16 20:22:54 akio Exp $
 * $Log: StppuDstMaker.h,v $
 * Revision 1.1  2002/01/16 20:22:54  akio
 * First version
 *
 * 
 * Author: Akio Ogawa June 2001
 ***************************************************************************
 *
 * Description:  TTree uDst for spin-pp
 *
 ***************************************************************************/
#ifndef StppuDst_h
#define StppuDst_h
#include "StMaker.h"

#define _BBC_data_
#define _FPD_data_

class TFile;
class TTree;
class StppEvent;
class StppGeant;
class StBbcTriggerDetector;
class StFpdCollection;

class StppuDstMaker : public StMaker {
public:
    StppuDstMaker(const Char_t *name="StppuDst");
    Int_t Init();
    Int_t Make();
    Int_t Finish();
	
protected:
    
private:
    size_t          mGoodCounter; //!
    size_t          mBadCounter;  //!
    TFile           *m_outfile;   //!
    TTree           *ppuDst;      //!
    StppEvent       *ppEvent;     //!
    StppGeant       *ppGeant;     //!
#ifdef _BBC_data_
    StBbcTriggerDetector *bbc;    //!
#endif
#ifdef _FPD_data_
    StFpdCollection *fpd;         //!
#endif
    Int_t           infoLevel;

    ClassDef(StppuDstMaker,0)
};
#endif
