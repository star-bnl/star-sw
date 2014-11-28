/***************************************************************************
 *
 * $Id: StZdcVertexMaker.h,v 1.6 2014/08/06 11:43:52 jeromel Exp $
 *
 * Author:  Johan E. Gonzalez, August 2001
 ***************************************************************************
 *
 * Description: This is the .h file for StZdcVertexMaker.cxx
 *
 ***************************************************************************
 *
 * $Log: StZdcVertexMaker.h,v $
 * Revision 1.6  2014/08/06 11:43:52  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.5  2004/01/14 22:57:29  fisyak
 * Add declaration of InitRun
 *
 * Revision 1.4  2003/10/06 04:06:14  perev
 * cvs() overloaded
 *
 * Revision 1.3  2003/09/10 19:47:42  perev
 * ansi corrs
 *
 * Revision 1.2  2001/08/31 19:07:36  macross
 * Modified code to retrieve ADC and TDC pulses from TrgDet table
 *
 **************************************************************************/
#ifndef StZdcVertexMaker_hh
#define StZdcVertexMaker_hh

#include "StMaker.h"

class St_ZdcCalPars;

class StZdcVertexMaker : public StMaker {
public:
    StMaker* currentChain;
    StZdcVertexMaker(const char* name = "StZdcVertexMaker",
                     const char* title = "event/StZdcVertexMaker");
    ~StZdcVertexMaker();
    void  Clear(const char* opt="");
    Int_t Init();
    Int_t InitRun(int runumber);
    Int_t Make();
    Int_t Finish();
    virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StZdcVertexMaker.h,v 1.6 2014/08/06 11:43:52 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
    
private:    
    float mEAP0;
    float mEAP1;
    float mEAP2;
    float mEAP3;
    float mWAP0;
    float mWAP1;
    float mWAP2;
    float mWAP3;
    float mVPAR;
    float mOFF;
    
    // the following is a ROOT macro  that is needed in all ROOT accessible code
    ClassDef(StZdcVertexMaker,0)
};
#endif



