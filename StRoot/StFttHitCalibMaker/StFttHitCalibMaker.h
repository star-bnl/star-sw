#ifndef STAR_StFttHitCalibMaker_H
#define STAR_StFttHitCalibMaker_H


/***************************************************************************                                          
 *                                                                                                                    
 * $Id: StFttHitCalibMaker.h,v 0.1 2017/02/21 17:50:32 tlusty Exp $                                                    
 * StFttHitCalibMaker - class to fille the StEvent from DAQ reader                                                        
 *--------------------------------------------------------------------------                                          
 *                                                                                                                    
 ***************************************************************************/
#include "StMaker.h"

// ROOT
#include "TH1.h"
#include "TH2.h"
#include "TH2Poly.h"
#include "TTree.h"
#include "TCanvas.h"
#include "TString.h"

// STL
#include <vector>

#include "HitCalibHelper.h"

class StEvent;
class StFttCollection;
class StFttRawHit;
class StFttDb;

class StFttHitCalibMaker: public StMaker
{
private:

public:

/// Default constructor                                                                                          
    StFttHitCalibMaker(const char *name="fttHitCalib");

    Int_t  Init();
    Int_t  InitRun(Int_t);
    Int_t  FinishRun(Int_t);
    Int_t  Finish();
    Int_t  Make();


    StEvent*             mEvent;
    StFttCollection*     mFttCollection;
    StFttDb*             mFttDb;
    HitCalibHelper*      mHelper;

    bool mDebug = false;


    ClassDef(StFttHitCalibMaker, 1)

};

#endif
