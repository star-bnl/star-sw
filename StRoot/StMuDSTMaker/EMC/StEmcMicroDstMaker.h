/*!\class StEmcMicroDstMaker
\author Alexandre A. P. Suaide

This is the EMC micro DST maker.

The main goal of this software is to convert StEvent into StEmcMicroEvent format
and save it on output file for physics analysis. This software also can read
EMC micro DST's and create back the StEvent format. This feature makes possible
to have only one version of the reconstruction and analysis library.

The default option is to read StEvent object from memory and create a StEmcMicroEvent
object. If the method AddMicroEventFile(char*) is used, the software starts reading
micro DST files and convert the StEmcMicroEvent objects into StEvent objects.

The selection of events is done by using the StEmcFilter objects defined in this
maker. There are two filters defines:

  - mPFilter = this is the filter responsible for event selection and primary tracks selection.
  - mGFilter = this is the filter responsible for global tracks selection

To get the pointer to these filters, use methods GetPrimaryFilter() and GetGlobalFilter(). To a more
detailed explanation how these filters work, check StEmcFilter documentation.

*/
#ifndef StEmcMicroDstMaker_h
#define StEmcMicroDstMaker_h
#include "StMaker.h"
#include "TH1.h"
#include "TFile.h"
#include "TTree.h"
#include "TChain.h"

class StEvent;
class StEmcMicroEvent;
class StEmcFilter;
class StEmcMicroUtil;

class StEmcMicroDstMaker : public StMaker 
{
  protected:
         
    TH1F*            mAcc;           //!

    StEvent          *mStEvent;      //!
    StEmcMicroEvent  *mMicroEvent;   //!
    
    TFile            *mMicroDstFile; //!
    TTree            *mEmcTree;      //!
    TString          mEventFile;
    TString          mEventFileOld;
    TString          mEventDir;
    
    Int_t            mFileCounter; 
    Int_t            mAccEv; 

    StEmcFilter      *mPFilter;
    StEmcFilter      *mGFilter;
    
    StEmcMicroUtil   *mMicroUtil;
		StEmcMicroDstMaker *mOldMaker;
    
    TChain           *mMicroEventChain;
    Int_t            mNMicroEvents;
    Int_t            mCurMicroEvent;
    Int_t            mStart;
    
    Bool_t           mDoRead;
    Bool_t           mDoCreateStEvent;
    
    Bool_t           mDoSavePrimaries;
    Bool_t           mDoSaveGlobals;
    Bool_t           mDoSaveEmc;
    Bool_t           mDoSaveFpd;
    Bool_t           mDoSaveV0;
		    
    Int_t            initMicroEventFile();
  
  private:
  
  public:

                       StEmcMicroDstMaker(const Char_t *name="EmcMicroDst");
             virtual   ~StEmcMicroDstMaker();
    Int_t              Init();
    Int_t              Make();
    Int_t              Finish();
            
    const char*        getOutputDir()          { return mEventDir.Data(); }  ///< Return output directory where micro DST's are being saved
    const char*        getCurrentFile()        { return mEventFile.Data(); } ///< Return currect .event.root file 
     
    StEmcFilter*       getPrimaryFilter()      { return mPFilter; }          ///< Return Event and primary tracks filter
    StEmcFilter*       getGlobalFilter()       { return mGFilter; }          ///< Return Global tracks filter
    StEmcMicroEvent*   getMicroEvent()         { return mMicroEvent; }       ///< Return current StEmcMicroEvent
    Int_t              getNEvents()            { return mNMicroEvents; }     ///< Return number of events in micro DST branch
    StEvent*           getStEvent()            { return mStEvent; }          ///< Return current StEvent
    
    void               setOutputDir(char *dir)    { mEventDir = dir; }       ///< Set output directory for micro DST's
    void               setCreateStEvent(Bool_t b) { mDoCreateStEvent = b; }  ///< Enable/disable creation of StEvent object if a microDST file is being processed
    void               setSavePrimaries(Bool_t a) { mDoSavePrimaries = a; }     ///< Save or don't primary tracks. Default is kTRUE. Track selection is done by PrimaryFilter
    void               setSaveGlobals(Bool_t a)   { mDoSaveGlobals = a; }       ///< Save or don't global tracks. Default is kTRUE. Track selection is done by GlobalFilter
    void               setSaveEmc(Bool_t a)       { mDoSaveEmc = a; }           ///< Save or don't EMC data. Default is kTRUE.
    void               setSaveFpd(Bool_t a)       { mDoSaveFpd = a; }           ///< Save or don't FPD data. Default is kTRUE.
    void               setSaveV0(Bool_t a)        { mDoSaveV0 = a; }           ///< Save or don't V0. Default is kTRUE.
    void               setStart(Int_t a)          { mStart = a; }               ///< Set first event to be read.
    void               setOldMaker(StEmcMicroDstMaker* a) { mOldMaker = a; }    ///< Set Old Maker.
    
    void               addMicroEventFile(char*);                             ///< Add EMC micro DST file to read
    
    	    
    ClassDef(StEmcMicroDstMaker,1)
};

#endif
