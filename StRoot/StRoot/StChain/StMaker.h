/*!
 * \class StMaker
 *
 * StMaker virtual base class for Makers
 */

#ifndef STAR_StMaker
#define STAR_StMaker

#include <assert.h>
#include "Stypes.h"
#include "TDataSet.h"
#include "TDataSetIter.h"
#include "TObjectSet.h"
#include "TClass.h"
#include "TMath.h"
#include "TString.h"
#include "TDatime.h"
#include "TH1.h"
#include "TFile.h"
#include "StEvtHddr.h"
#ifndef ROOT_TClonesArray
#include "TClonesArray.h"
#endif
#include "TStopwatch.h"
#include "StMessMgr.h" 

#ifndef __CINT__
#if ROOT_VERSION_CODE >= ROOT_VERSION(3,05,04)
typedef TDataSet::EDataSetPass EDataSetPass;
#endif
#endif

class TList;
class TBrowser;
class TChain;
class TTree;
class TTable;
class StMemStat;
class StEvtHddr;
class TAttr;

class StTurnLogger;

class StTestMaker;
class StChainOpt;

struct DbAlias_t {
  const char *tag;
  Int_t date;
  Int_t time;
  const char *geometry;
  const char *comment;
};


class StMaker : public TDataSet{

public:
   typedef  enum {kNormal, kDebug} EDebugLevel;
   enum {kSTAFCV_BAD, kSTAFCV_OK, kSTAFCV_ERR=2, kSTAFCV_FATAL=3} EModule_return_Status;
   enum EMakerStatus {kInitBeg = 1, kInitEnd = 2,
		      kMakeBeg = 3, kCleaBeg = 4,
		      kFiniBeg = 5, kFiniEnd = 6,
		      kActive  = 7};
protected:

   TDataSet     *m_DataSet;             //!  
   TDataSet     *m_ConstSet;            //!  
   TDataSet     *m_GarbSet;             //!  
   TDataSet     *m_Inputs;              //!list of logInput:ActualInput
   TDataSet     *m_Ouputs;              //!list of logOuput:ActualOuput
   TDataSet     *m_Runco;               //!Run Control parameters
   TList          *m_Histograms;        //!list of Histograms
   static StMaker *fgTopChain;          //!pointer to top StChain
   static StMaker *fgStChain;           //!current pointer to StChain
   static StMaker *fgFailedMaker;       //!current pointer to failed maker
   static StTestMaker *fgTestMaker;     //pointer to test maker called before each one
   static Int_t fgTallyMaker[kStFatal+1];//!counters
   Int_t         fTallyMaker[kStFatal+1];//!counters
   Int_t           m_Mode;              //!Integer mode of maker
   Int_t           m_Number;            //!Serial event number
   Int_t           m_LastRun;           //!Last Run number
   Int_t           m_DebugLevel;        //!Debug level
   Int_t           m_MakeReturn;        //!Make() return flag
   TStopwatch      m_Timer;             //!Timer object
   StMemStat       *fMemStatMake;        //!StMemStat for Make
   StMemStat       *fMemStatClear;       //!StMemStat for Clear
   Int_t           fStatus;             //!Maker status
   mutable StMessMgr      *fLogger;             // This object logger instance
   mutable StTurnLogger   *fLoggerHold;         // hold the pointer to the previous StMessMgr
protected:
//   inline StMessMgr    *GetLogger(){return fLogger;}
   inline StMessMgr    *GetLogger() const {return fLogger;}
public:

   /// Constructor & Destructor

                        StMaker(const char *name="",const char *dummy=0);
   virtual              ~StMaker();
   virtual Int_t IsChain() const {return 0;}


   /// User defined functions
   virtual void         Clear(Option_t *option="");
   virtual Int_t        InitRun(Int_t runumber);
   virtual Int_t        Init();
   virtual void         StartMaker();
   virtual Int_t        Make();
   virtual Int_t        IMake(Int_t number){SetNumber(number);return Make();};
   virtual void         EndMaker  (Int_t ierr);
   virtual Int_t        Finish();
   virtual Int_t        FinishRun(Int_t oldrunumber);

   
   virtual void         FatalErr(Int_t Ierr, const char *Com);  
   virtual void         PrintInfo();
   virtual void         NotifyMe(const char */* about */,const void */* ptr */){}
   virtual void         AddMaker (StMaker *mk);
#if 0
   virtual void   MakeDoc(const TString &stardir="$(STAR)",const TString &outdir="$(STAR)/StRoot/html",Bool_t baseClasses=kTRUE); 
#else
   virtual void   MakeDoc(const TString &/* stardir ="$(STAR)" */,const TString &/* outdir="$(STAR)/StRoot/html" */,Bool_t /* baseClasses=kTRUE */) {}
#endif
   ///  User methods
   virtual void  AddData (TDataSet *data,const char *dir=".data");
   virtual TDataSet *AddObj  (TObject *obj ,const char *dir, int owner=1);
   virtual TDataSet *ToWhiteBoard(const char *name, void *dat);
   virtual TDataSet *ToWhiteBoard(const char *name, void *dat, void *del);
   virtual TDataSet *ToWhiteBoard(const char *name, TObject *dat, Int_t owner);

   virtual TDataSet *ToWhiteConst(const char *name, TObject *dat);
   virtual TDataSet *ToWhiteConst(const char *name, void *dat);

//  called WhiteBoard(name,&ptr)
   virtual TDataSet *WhiteBoard  (const char *name, void *v=0) const;
//______________________________________________________________________________
   virtual Int_t        Skip(Int_t nskip);     //Skip events

   virtual void         AddConst(TDataSet *data=0){AddData(data,".const");}
   virtual void         AddHist(TH1 *h,const char *dir=0);
   virtual void         AddGarb (TDataSet *data=0){AddData(data,".garb");};
   virtual void         AddRunco (TDataSet *data=0){AddData(data,".runco");};
   virtual void         AddRunco (Double_t par,const char *name,const char *comment);
           void         AddRunCont (TDataSet *data=0){AddRunco(data);}; //alias
   virtual TList       *GetHistList() const {return (TList*)GetDirObj(".hist");};
   virtual TH1         *GetHist(const char *histName) const {TList *l=GetHistList(); return l?(TH1*)l->FindObject(histName):(TH1*)0;};
   virtual StMaker     *cd(){StMaker *ret = fgStChain; fgStChain=this; return ret;};
   virtual StMaker     *Cd(){return cd();};
   static  StMaker     *New(const char *classname, const char *name="", void *title=0);


   /// STAR methods
   virtual Int_t        GetNumber() const ;
   virtual void         SetNumber(Int_t number) ;
   static  StMaker     *GetTopChain(){return fgTopChain;}
   static  StMaker     *GetChain(){return fgStChain;}
   static  StMaker     *GetFailedMaker(){return fgFailedMaker;}
   virtual StMaker     *GetParentChain() const;
   virtual Int_t        GetIventNumber() const ;
   virtual void         SetIventNumber(Int_t iv);
   virtual Int_t        GetEventNumber() const ;
   virtual Int_t        GetRunNumber() const ;
   virtual const TDatime &GetDateTime() const;
   virtual const TDatime &GetDBTime() const;
   virtual void         SetDateTime(Int_t idat,Int_t itim);// 
   virtual StEvtHddr   *GetEvtHddr() const; //
   virtual Int_t        GetDate()  const ;
   virtual Int_t        GetTime()  const ;
   virtual const char *GetEventType() const ;


   // Get methods
   virtual TDataSet  *GetData(const char *name, const char *dir=".data") const;
   virtual TDataSet  *GetData()  const {return m_DataSet ;}
   virtual TDataSet  *GetConst() const {return m_ConstSet;}
   virtual TDataSet  *GetDataSet (const char *logInput) const {return FindDataSet(logInput);}
   virtual TDataSet  *   DataSet (const char *logInput)   const 
                           {return GetDataSet(logInput);};
   virtual TDataSet  *GetInputDS (const char *logInput)   const 
                           {return GetDataSet(logInput);};

   virtual TDataSet  *GetDataBase(const char *logInput,const TDatime *td=0);
   virtual TDataSet  *GetInputDB (const char *logInput)
                          {return GetDataBase(logInput);};


   virtual Int_t        GetDebug() const {return m_DebugLevel;}
   virtual Int_t           Debug() const {return GetDebug();};
   virtual Int_t        GetMakeReturn() const {return m_MakeReturn;}
   virtual TList       *Histograms()  const {return GetHistList();}
   virtual TString      GetAlias (const char *log, const char *dir=".aliases") const ;
   virtual TString      GetInput (const char *log) const {return GetAlias(log);};
   virtual TString      GetOutput(const char *log) const {return GetAlias(log,".aliases");};
   virtual TList       *GetMakeList() const ;
   virtual StMaker     *GetParentMaker () const;
   virtual StMaker     *GetMaker (const char *mkname);
   virtual StMaker     *GetMakerInheritsFrom (const char *mktype) const;
   virtual Bool_t       IsActive() {return TestBIT(kActive);}
   virtual StMaker     *Maker (const char *mkname){return GetMaker (mkname);};


   /// Maker Status Bits 
   virtual void         SetBIT(EMakerStatus k)   {SETBIT(fStatus,k);}
   virtual void         ResetBIT(EMakerStatus k) {CLRBIT(fStatus,k);}
   virtual Bool_t       TestBIT(EMakerStatus k)  {return TESTBIT(fStatus,k);}
   /// Setters for flags and switches
   virtual void         SetActive(Bool_t k=kTRUE) {if(k) SetBIT(kActive); else ResetBIT(kActive);} 
   virtual void         SetDebug(Int_t l=1);          // *MENU*
   virtual void         SetDEBUG(Int_t l=1);          // *MENU*
   virtual void         SetFlavor(const char *flav,const char *tabname);  //Set DB Flavor
   virtual void         SetMakeReturn(Int_t ret){m_MakeReturn=ret;}  
   virtual void         SetAlias(const char *log,const char *act,const char *dir=".aliases");
   virtual void         AddAlias(const char *log,const char *act,const char *dir=".aliases");
   virtual void         SetInput(const char *log,const char *act){SetAlias(log,act);};
   virtual void         SetOutput(const char *log,const char *act){SetAlias(log,act,".aliases");};
   virtual void         SetOutput(const char *log,TDataSet *ds);
   virtual void         SetOutput(TDataSet *ds){SetOutput(0,ds);};
   virtual void         SetOutputAll(TDataSet *ds,Int_t level=1);
   virtual void         SetMode(Int_t mode=0)   {m_Mode=mode;}   // *MENU*
   virtual void         SetNotify(const char *about,StMaker *mk);
   virtual Int_t        GetMode() { return m_Mode;}
   virtual Int_t        GetDebug(){ return m_DebugLevel;}
   virtual const StChainOpt *GetChainOpt()    const;
   virtual TFile *GetTFile() const; 			

   virtual void         NotifyEm(const char *about,const void *ptr);

   virtual Double_t     RealTime(){ return m_Timer.RealTime();}
   virtual Double_t     CpuTime() { return m_Timer.CpuTime();}
   virtual void         StartTimer(Bool_t reset = kFALSE){m_Timer.Start(reset);}
   virtual void         StopTimer(){m_Timer.Stop();}
   virtual void         PrintTimer(Option_t *option="");
   virtual void         PrintTotalTime(){}
///  special overload
   virtual const char  *GetName() const;

   /// Static functions
   static  StMaker     *GetMaker(const TDataSet *ds)  ;
   static EDataSetPass  ClearDS (TDataSet* ds,void *user );
   static const char *RetCodeAsString(Int_t kode);
   static      Int_t    AliasDate(const char *alias);
   static      Int_t    AliasTime(const char *alias);
   static      const char  *AliasGeometry(const char *alias);
   static const DbAlias_t  *GetDbAliases();
   static      void     SetTestMaker(StTestMaker *mk)	{fgTestMaker=mk;}

TObject        *GetDirObj(const char *dir) const;
void            SetDirObj(TObject *obj,const char *dir);
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StMaker.h,v 1.101 2017/04/26 18:33:12 perev Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
protected:
   virtual TDataSet  *FindDataSet (const char *logInput,
                                    const StMaker *uppMk=0,
                                    const StMaker *dowMk=0) const ;

public:
static Int_t Cleanup(TDataSet *&ds);
static void lsMakers(const StMaker *top);

private:
  TAttr *m_Attr;		
///< SetAttr(const char *opt,const char *for) 
///< sets value of m_Option data member
public:
Int_t         SetAttr(const char *key,const char *val,const char *to=".");
Int_t         SetAttr(const char *key,Int_t         val,const char *to=".");
Int_t         SetAttr(const char *key,UInt_t        val,const char *to=".");
Int_t         SetAttr(const char *key,Double_t      val,const char *to=".");
Int_t         SetAttr(const StMaker *mk);
Int_t         RemAttr(const char *key,                const char *to=".")
            {return SetAttr(key,".remove",to);}
const TAttr *GetAttr() const 	{return m_Attr;}
Int_t         IAttr(const char *key) const;
UInt_t      UAttr(const char *key) const;
Double_t      DAttr(const char *key) const;
const char *SAttr(const char *key) const;
void        PrintAttr() const;
   ClassDef(StMaker, 0)   // base class to define  one step of the recontsruction chain
};

class StMakerIter 
{
public:
  StMakerIter(const StMaker *mk, Int_t second = 0);
 ~StMakerIter();
  StMaker *NextMaker();
  StMaker *GetMaker () const {return (StMaker *)fMaker;}
private:
  Int_t fState;                 //!
  Int_t fSecond;                //!
  const StMaker *fMaker;              //!
  StMakerIter *fMakerIter;      //!
  const TDataSet *fItWas;             //!
  TDataSetIter *fIter;          //!
};  
class StTestMaker : public StMaker {
public:
   /// Constructor & Destructor

             StTestMaker(const char *name="");
virtual     ~StTestMaker(){};
virtual void SetNext(StMaker *mk);
virtual void Print(const char *opt="") const;
protected:

   StMaker *fNext;
   StMaker *fLast;

ClassDef(StTestMaker,0)
};  
#endif


// $Id: StMaker.h,v 1.101 2017/04/26 18:33:12 perev Exp $
// $Log: StMaker.h,v $
// Revision 1.101  2017/04/26 18:33:12  perev
// Add GetData() to hide  m_DataSet
//
// Revision 1.100  2014/08/06 11:42:55  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.99  2012/06/09 22:46:52  fisyak
// Synchronize tag and geometry version for y2006h, thanks to Xianglei, bug #2374
//
// Revision 1.98  2011/06/20 15:13:51  fisyak
// Force to call Finish with SIGTERM signal obtained from condor_vacate_job after time limit reached
//
// Revision 1.97  2010/01/27 20:37:04  perev
// GetValidity removed. It is St_db_Maker::GetValidity() now
//
// Revision 1.96  2009/11/16 20:16:22  fine
// Make the TDatime const interfaces
//
// Revision 1.95  2009/11/16 19:52:46  fine
// Fix the signature of the StMaker::GetDate... methods
//
// Revision 1.94  2009/11/10 17:41:19  fine
// remove the compilation warning on SL5
//
// Revision 1.93  2009/10/13 18:56:50  perev
// WhiteBoard improve
//
// Revision 1.92  2009/04/28 22:45:43  perev
// WhiteBoard cleanup
//
// Revision 1.91  2009/03/17 20:03:36  perev
// Back to StMemSet version
//
// Revision 1.89  2009/01/26 14:33:30  fisyak
// rename TMemStat => StMemStat due clash with ROOT class
//
// Revision 1.88  2008/12/21 18:59:43  perev
// GetDBTim() added
//
// Revision 1.87  2008/06/03 22:33:15  fisyak
// Add geometries for y2005g, y2006g and y2007g; use ROOT convention for variable definitions
//
// Revision 1.86  2008/03/05 00:01:52  fisyak
// Move Skip method in base class
//
// Revision 1.85  2008/01/20 00:33:56  perev
// Copy attributes from maker to maker added
//
// Revision 1.84  2007/07/12 19:17:20  fisyak
// Add fTopChain - a pointer to TopChain (for embedding), add method GetMakerInheritsFrom
//
// Revision 1.83  2007/04/26 03:59:20  perev
// new WhiteBoard methods
//
// Revision 1.82  2007/04/17 05:07:48  perev
// GetTFile()==>StMaker. Jerome request
//
// Revision 1.81  2007/03/12 17:51:19  perev
// new signature of GetDataBase()
//
// Revision 1.80  2007/01/25 06:28:04  fine
// connect Logger and Maker debug levels
//
// Revision 1.79  2006/12/19 21:59:15  fine
// replace the class StMessMgr forward declaration with the real declaration and adjust St_TLA_Maker to show how to use logger
//
// Revision 1.78  2006/10/04 18:51:26  fisyak
// Add new geometry tags: upgr04 and upgr04, remove rference to xdf
//
// Revision 1.77  2005/12/18 23:17:43  perev
// uint attributes fix
//
// Revision 1.76  2005/12/07 18:54:12  perev
// PrintAttr() method added
//
// Revision 1.75  2005/11/22 21:37:04  fisyak
// add more Simu time stamps (reflecting new SVT), and clean up
//
// Revision 1.74  2005/10/06 18:55:45  fisyak
// Add all used simulation time stamps and geometries
//
// Revision 1.73  2005/08/29 21:42:21  fisyak
// switch from fBits to fStatus for StMaker control bits
//
// Revision 1.72  2005/01/26 23:02:48  perev
// private ==> protected
//
// Revision 1.71  2004/11/13 00:28:57  fine
// move the logger instantiation away of the ctor to be able to get the csubclass name
//
// Revision 1.70  2004/11/04 22:26:38  fine
// populate the package with save/restore the logger and edit some messages
//
// Revision 1.69  2004/11/03 01:30:29  fine
// remove the redundant logger macro. They have been moved to the abstarct messager
//
// Revision 1.68  2004/09/01 22:09:56  perev
// new methods SetAttr and IAttr,DAttr,SAttr added
//
// Revision 1.67  2004/07/23 17:06:18  perev
// AliasDate & AliasTime moved fro db maker to StMaker
//
// Revision 1.66  2004/04/26 00:07:19  perev
// RetCodeAsString(kode) added. String form of STAR return codes
//
// Revision 1.65  2004/04/15 16:05:29  fine
// Add extra data-mmeber and method for the coming STAR logger
//
// Revision 1.64  2004/04/15 00:21:17  perev
// SetDateTime(int,int) added
//
// Revision 1.63  2003/11/13 02:54:34  perev
// Safe destructor of TDataSet like object added
//
// Revision 1.62  2003/07/09 19:54:44  jeromel
// Added some Get methods
//
// Revision 1.61  2003/07/01 16:59:16  perev
// error codes for Maker added
//
// Revision 1.60  2003/05/01 16:41:05  jeromel
// Declaration works in new root but not in old. Transition solution added
//
// Revision 1.59  2003/04/30 20:36:24  perev
// Warnings cleanup. Modified lines marked VP
//
// Revision 1.58  2002/04/14 21:51:12  perev
// Obsolete StBroadcast
//
// Revision 1.57  2002/02/23 00:02:49  perev
// NotyfyMe added
//
// Revision 1.56  2002/02/02 23:31:14  jeromel
// doxygenized. Added some text for the Make() method.
//
// Revision 1.55  2001/11/18 00:58:07  perev
// Broadcast method added
//
// Revision 1.54  2001/10/13 20:23:45  perev
// SetFlavor  working before and after Init()
//
// Revision 1.53  2001/08/14 16:42:48  perev
// InitRun call improved
//
// Revision 1.52  2001/06/01 02:47:31  perev
// Memory consumption measurement added
//
// Revision 1.51  2001/05/31 02:40:30  perev
// const(ing)
//
// Revision 1.50  2001/05/04 19:15:59  perev
// Fatal() -> FatalErr()
//
// Revision 1.49  2001/04/10 21:38:49  perev
// Maki(int) --> IMake(int)
//
// Revision 1.48  2000/07/04 02:36:01  perev
// AddMaker method added & gStChain removed
//
// Revision 1.47  2000/06/21 21:12:40  perev
// StMakerIter class added
//
// Revision 1.46  2000/05/20 01:11:07  perev
// IventNumber and BfcStatus added
//
// Revision 1.45  2000/04/13 02:53:35  perev
// StMaker::GetValidity added
//
// Revision 1.44  2000/04/03 23:46:49  perev
// Increased error check
//
// Revision 1.43  2000/03/23 00:15:22  fine
// Adjusted to libSTAR for ROOT 2.24
//
// Revision 1.42  2000/01/07 22:31:43  perev
// one more argument for SetOutputAll
//
// Revision 1.41  1999/12/06 01:57:30  fine
// Time statistic fixed
//
// Revision 1.39  1999/12/01 23:51:25  perev
// Alias AddRunco - AddRunCont
//
// Revision 1.38  1999/12/01 22:56:30  perev
// .runco directory & AddRunco method introduced
//
// Revision 1.37  1999/09/23 21:24:58  perev
// recovered debug level init(lost)
//
// Revision 1.36  1999/09/21 15:05:18  perev
// InitRun & FinishRun added
//
// Revision 1.35  1999/09/08 00:13:35  fisyak
// Add static *GetChain()
//
// Revision 1.34  1999/09/03 23:11:48  perev
// Add .runcont directory
//
// Revision 1.33  1999/09/02 22:27:12  fisyak
// Add SetDEBUG
//
// Revision 1.32  1999/08/13 01:12:25  fine
// StMaker::GetHist has been introduced
//
// Revision 1.31  1999/08/06 13:01:38  fisyak
// Add Active flag
//
// Revision 1.30  1999/07/29 01:05:23  fisyak
// move bfc to StBFChain
//
// Revision 1.29  1999/07/13 02:19:34  perev
// GetCVS,StEvtHddr,etc...
//
// Revision 1.28  1999/07/12 17:36:34  perev
// Spiros request to add error flags
//
// Revision 1.27  1999/07/12 02:27:10  perev
// GetCVS only
//
// Revision 1.26  1999/07/11 02:02:05  perev
// clash inside GetCVS resolved
//
// Revision 1.25  1999/07/11 01:59:04  perev
// add GetCVSTag again
//
// Revision 1.24  1999/07/09 22:00:22  perev
// GetCVS into StMaker
//
// Revision 1.23  1999/06/27 23:09:22  fisyak
// Add __DATE__ & __TIME__ to tag
//
// Revision 1.22  1999/05/23 04:05:03  fine
// The lost since 1.35 Wed Mar 10 20:23:58 timer functions have been re-introduced
//
// Revision 1.21  1999/05/10 15:37:52  perev
// Save of hisogramm in StMaker::Init
//
// Revision 1.20  1999/05/06 21:27:11  perev
// StMaker remove his from hdirectory
//
// Revision 1.19  1999/05/06 00:19:05  fine
// StMaker::MakeDoc method has been re-introduced for the 3d time
//
// Revision 1.18  1999/04/30 14:58:41  perev
//  cd() added to StMaker class
//
// Revision 1.17  1999/03/20 20:57:35  perev
// add StEvtHddr.h and fix Get/SetNumber in maker
//
// Revision 1.16  1999/03/19 20:30:50  perev
// GetCVSTag introduced
//
// Revision 1.15  1999/03/11 01:23:59  perev
// new schema StChain
//
// Revision 1.10  1998/12/21 19:42:51  fisyak
// Move ROOT includes to non system
//
// Revision 1.9  1998/11/19 01:23:57  fine
// StChain::MakeDoc has been introduced, StChain::MakeDoc has been fixed (see macros/bfc_doc.C macro
//
// Revision 1.8  1998/11/18 22:46:10  fine
// The lost MakeDoc method has been re-introduced
//
// Revision 1.7  1998/10/06 18:00:27  perev
// cleanup
//
// Revision 1.5  1998/08/18 14:05:02  fisyak
// Add to bfc dst
//
// Revision 1.4  1998/07/20 15:08:09  fisyak
// Add tcl and tpt
//
