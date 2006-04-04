/***************************************************************************
 *
 *$Id: StEStructAnalysisMaker.h,v 1.6 2006/04/04 22:05:03 porter Exp $
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
#include "StEStructCuts.h"

#include "StEStructQAHists.h"
class StEStructEvent;
#include "StMuDSTMaker/COMMON/StMuTimer.h"
//class StMuTimer;



class StEStructAnalysisMaker : public StMaker {

public:
                   StEStructAnalysisMaker(const Char_t *name="ESTRUCT 2pt");

    virtual       ~StEStructAnalysisMaker();
    
    void          Clear(Option_t *option="");
    Int_t         Init();
    Int_t         Make();
    Int_t         Finish();

    void          SetReaderAnalysisPair(StEStructEventReader* reader, StEStructAnalysis * analysis);
    void          SetEventReader(StEStructEventReader* reader);
    void          SetAnalysis(StEStructAnalysis * analysis);
    void          SetAnalyses(StEStructAnalysis ** analyses, int n);
    void          SetQAHists(StEStructQAHists* qa);
    int           getAnalysisIndex();          
    void          writeQAHists(const char* fileName);

    void          writeDiagnostics(int opt=0); // a holder for diagnostic info
    void          toggleMemoryInfo();
    int           getNumberOfEventsLooped();
    int           getNumberOfEventsProcessed();

    StMuTimer*    getTimer();
    void          startTimer();
    void          stopTimer();

    void          logAllStats(ostream& os);
    void          logCutStats(StEStructCuts* cuts, ostream& os, int index);   

    void          logAnalysisTime(ostream& os);
    void          logInputEvents(ostream& os);
    void          logOutputEvents(ostream& os);
    void          logOutputRate(ostream& os);
    void          logAnalysisStats(ostream& os);

    void          compiledLoop();
    StEStructAnalysis* mCurrentAnalysis;


    virtual const char *GetCVS() const
    {static const char cvs[]="$Id: StEStructAnalysisMaker.h,v 1.6 2006/04/04 22:05:03 porter Exp $ built "__DATE__" "__TIME__ ; return cvs;}
//-------------------------------------------------


private:

    Bool_t              doPrintMemoryInfo; //!
    StMuTimer*            mtimer;
    UInt_t              mEventLoopCounter; //!        //  mNeventsLooped
    UInt_t              mEventProcessedCounter; //!   //  mNeventsProcessed
    int*                mEventProcessedPerType; //!
    int                 numAnalysis;
    // --> new pointers for data I/O, cuts, and analysis

    StEStructEventReader* mreader; //! base class for reading an event
    StEStructAnalysis* manalysis[100];  //!

    //pointers to an event ... don't need this now but may be useful
    StEStructEvent*        pEStructEvent;  //  pointer to uEvent data

    StEStructQAHists*      mQAHists;
 
 public:

    ClassDef(StEStructAnalysisMaker,1)   
};


inline StMuTimer* StEStructAnalysisMaker::getTimer(){ return mtimer; };

inline void StEStructAnalysisMaker::SetReaderAnalysisPair(StEStructEventReader* reader, StEStructAnalysis* analysis){
  SetEventReader(reader);
  SetAnalysis(analysis);
}

inline void StEStructAnalysisMaker::SetEventReader(StEStructEventReader* reader){ 
  mreader = reader;
}

inline void StEStructAnalysisMaker::SetAnalysis(StEStructAnalysis* analysis){ 
  manalysis[numAnalysis]=analysis; numAnalysis++;
};

inline void StEStructAnalysisMaker::SetAnalyses(StEStructAnalysis ** analyses, int n){
  for(int i=0;i<n;i++)manalysis[i]=analyses[i];
  numAnalysis=n;
};

inline void StEStructAnalysisMaker::SetQAHists(StEStructQAHists* qa){
  if(!qa) return;
  if(mQAHists)delete mQAHists; 
  mQAHists = qa;
}

inline void StEStructAnalysisMaker::toggleMemoryInfo(){
  if(doPrintMemoryInfo){ 
      doPrintMemoryInfo=false;
      return;
  }
  doPrintMemoryInfo=true;
}


inline int StEStructAnalysisMaker::getNumberOfEventsLooped(){ return mEventLoopCounter; }
inline int StEStructAnalysisMaker::getNumberOfEventsProcessed(){ return mEventProcessedCounter; }

inline void StEStructAnalysisMaker::logCutStats(StEStructCuts* cuts, ostream& os, int index){
  cuts->printCuts(os,index);
}


//-------------------------------------------------------------------
inline void StEStructAnalysisMaker::logAllStats(ostream& os){
   logAnalysisTime(os);
   logInputEvents(os);
   logOutputEvents(os);
   logOutputRate(os);
   logAnalysisStats(os);
}

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

inline void StEStructAnalysisMaker::logAnalysisStats(ostream& os){
     for(int i=0;i<numAnalysis;i++) manalysis[i]->logStats(os);
}

#endif

/***********************************************************************
 *
 * $Log: StEStructAnalysisMaker.h,v $
 * Revision 1.6  2006/04/04 22:05:03  porter
 * a handful of changes:
 *  - changed the StEStructAnalysisMaker to contain 1 reader not a list of readers
 *  - added StEStructQAHists object to contain histograms that did exist in macros or elsewhere
 *  - made centrality event cut taken from StEStructCentrality singleton
 *  - put in  ability to get any max,min val from the cut class - one must call setRange in class
 *
 * Revision 1.5  2005/09/29 17:40:25  msd
 * Changed empty analysis to create plots for determining centrality bins
 *
 * Revision 1.4  2005/09/14 17:08:27  msd
 * Fixed compiler warnings, a few tweaks and upgrades
 *
 * Revision 1.3  2005/09/07 20:18:39  prindle
 *   AnalysisMaker: Keep track of currentAnalysis (for use in doEStruct macro)
 *   EventCuts.h:   Added trigger cuts including cucu and year 4.
 *   MuDstReader:   Added dE/dx histograms. Re-arranged code to count tracks
 *                    before making centrality cut.
 *   TrackCuts:     Random changes. Moved some variables from private to public.o
 *
 * Revision 1.2  2004/06/25 03:10:28  porter
 * added a new common statistics output and added electron cut with momentum slices
 *
 * Revision 1.1  2003/10/15 18:20:32  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/

