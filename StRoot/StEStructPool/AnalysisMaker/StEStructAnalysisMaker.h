/***************************************************************************
 *
 *$Id: StEStructAnalysisMaker.h,v 1.1 2003/10/15 18:20:32 porter Exp $
 *   
 *
 *
 ***************************************************************************
 *
 * Description:  This is a maker for general EStruct Analysis.
 *               Requires at least 1 reader and 1 analysis 
 *
 **************************************************************************/

#ifndef StEStructAnalysisMaker_HH
#define StEStructAnalysisMaker_HH

#include "Stiostream.h"

#include "StMaker.h"
//
//-> include pure virtual interface
#include "StEStructEventReader.h"
#include "StEStructAnalysis.h"

class StEStructEvent;
#include "StMuDSTMaker/COMMON/StMuTimer.h"
//class StMuTimer;


class StEStructAnalysisMaker : public StMaker {

public:
                   StEStructAnalysisMaker(const Char_t *name="EbyE 2-pt");
    virtual       ~StEStructAnalysisMaker();
    
    void          Clear(Option_t *option="");
    Int_t         Init();
    Int_t         Make();
    Int_t         Finish();

  void          SetReaderAnalysisPair(StEStructEventReader* reader, StEStructAnalysis * analysis);
    void          SetEventReader(StEStructEventReader* reader);
    void          SetAnalysis(StEStructAnalysis * analysis);
    void          toggleMemoryInfo();
    int           getNumberOfEventsLooped();
    int           getNumberOfEventsProcessed();

    StMuTimer*      getTimer();
    void          startTimer();
    void          stopTimer();
    void          logAnalysisTime(ostream& os);
    void          logInputEvents(ostream& os);
    void          logOutputEvents(ostream& os);
    void          logOutputRate(ostream& os);

    void          compiledLoop();


    virtual const char *GetCVS() const
    {static const char cvs[]="$Id: StEStructAnalysisMaker.h,v 1.1 2003/10/15 18:20:32 porter Exp $ built "__DATE__" "__TIME__ ; return cvs;}
//-------------------------------------------------


private:

    Bool_t              doPrintMemoryInfo; //!
    StMuTimer*            mtimer;
    UInt_t              mEventLoopCounter; //!        //  mNeventsLooped
    UInt_t              mEventProcessedCounter; //!   //  mNeventsProcessed
    int*                mEventProcessedPerType; //!
    int                 numReaders;
    int                 numAnalysis;
    // --> new pointers for data I/O, cuts, and analysis

    StEStructEventReader* mreader[100]; //! base class for reading an event
    StEStructAnalysis* manalysis[100];  //!

    //pointers to an event ... don't need this now but may be useful
    StEStructEvent*        pEStructEvent;               //  pointer to uEvent data
 
 public:

    ClassDef(StEStructAnalysisMaker,1)   
};


inline StMuTimer* StEStructAnalysisMaker::getTimer(){ return mtimer; };

inline void StEStructAnalysisMaker::SetReaderAnalysisPair(StEStructEventReader* reader, StEStructAnalysis* analysis){
  SetEventReader(reader);
  SetAnalysis(analysis);
}

inline void StEStructAnalysisMaker::SetEventReader(StEStructEventReader* reader){ 
  mreader[numReaders]=reader; numReaders++;
}

inline void StEStructAnalysisMaker::SetAnalysis(StEStructAnalysis* analysis){ 
  manalysis[numAnalysis]=analysis; numAnalysis++;
};

inline void StEStructAnalysisMaker::toggleMemoryInfo(){
  if(doPrintMemoryInfo){ 
      doPrintMemoryInfo=false;
      return;
  }
  doPrintMemoryInfo=true;
}


inline int StEStructAnalysisMaker::getNumberOfEventsLooped(){ return mEventLoopCounter; }
inline int StEStructAnalysisMaker::getNumberOfEventsProcessed(){ return mEventProcessedCounter; }

inline void StEStructAnalysisMaker::logAnalysisTime(ostream& os){
  if(!mtimer) return;
  os<<"<processStat \"analysisTime\">"<<mtimer->elapsedTime();
  os<<"</processStat>"<<endl;
}

inline void StEStructAnalysisMaker::logInputEvents(ostream& os){
  os<<"<processStat \"inputEvents\">"<<mEventLoopCounter;
  os<<"</processStat>"<<endl;
}

inline void StEStructAnalysisMaker::logOutputEvents(ostream& os){
  os<<"<processStat \"outputEvents\">"<<mEventProcessedCounter;
  os<<"</processStat>"<<endl;
}

inline void StEStructAnalysisMaker::logOutputRate(ostream& os){
  if(!mtimer || mtimer->elapsedTime()<0.01)return;

  os<<"<processStat \"outputRate\">"<<mEventProcessedCounter/mtimer->elapsedTime();
  os<<"</processStat>"<<endl;
}



#endif

/***********************************************************************
 *
 * $Log: StEStructAnalysisMaker.h,v $
 * Revision 1.1  2003/10/15 18:20:32  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/

