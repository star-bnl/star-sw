/**********************************************
 *
 * $Id: StMcEventMaker.h,v 1.2 1999/07/28 20:27:43 calderon Exp $
 * $Log: StMcEventMaker.h,v $
 * Revision 1.2  1999/07/28 20:27:43  calderon
 * Version with SL99f libraries
 *
 *
 **********************************************/

#ifndef StMcEventMaker_HH
#define StMcEventMaker_HH

#ifndef StMaker_H
#include "StMaker.h"
#endif

class StMcEvent;

class StMcEventMaker : public StMaker {
  
private:
    Bool_t drawinit;
    StMcEvent* mCurrentMcEvent; //!  This tells CINT not to parse it.
    
    

protected:

  
public:

    StMaker* currentChain;
    StMcEventMaker(const char* name = "MCEvent", const char* title = "event/McEvent");
    virtual ~StMcEventMaker();
    virtual void  Clear(const char* opt="");
    virtual void PrintInfo();
    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();
  

    // if need to make histograms or print stuff, look at St_QA_Maker

    //method to return the read event like in StEventMaker 
    StMcEvent* currentMcEvent() { return mCurrentMcEvent;}; 

    virtual const char* GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StMcEventMaker.h,v 1.2 1999/07/28 20:27:43 calderon Exp $ built "__DATE__" "__TIME__; return cvs;}	
    
    ClassDef(StMcEventMaker, 1)

};

#endif
