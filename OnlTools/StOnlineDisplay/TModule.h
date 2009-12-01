
// $Id: TModule.h,v 1.1 2009/12/01 01:33:33 fine Exp $
// $Log: TModule.h,v $
// Revision 1.1  2009/12/01 01:33:33  fine
// Move online display udner OnlTools
//
// Revision 1.6  2009/02/24 22:42:08  fine
// change the base class pointer
//
// Revision 1.5  2009/02/17 20:42:18  fine
// Change the class name to face the new ROOT API
//
// Revision 1.4  2008/03/11 13:30:23  fine
// remove the RUN CPP flag
//
// Revision 1.3  2008/03/07 22:53:10  fine
// multithreaded version
//
// Revision 1.2  2007/12/28 15:27:13  fine
// version with the offline coordinate transformation
//
// Revision 1.1  2007/10/26 14:06:31  fine
//  add the header files
//
// Revision 1.4  2003/02/04 02:21:52  fine
// various corrections to make gcc 3.2 compiler happy
//
// Revision 1.3  2002/04/15 20:23:38  fine
// NEw naming schema for RootKErnel classes and a set of classes to back geometry OO
//
// Revision 1.2  2002/04/15 17:54:29  fine
// Perl utilities have been moved to the separate Utils subdir
//
//

#ifndef STAR_TModule
#define STAR_TModule

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TModule virtual base class for Modules                                //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#if 1

#include "StMaker.h"

class  TModule : public StMaker {
  protected:
      TIter   *fNxtModule;
      StMaker *fCurrentMaker;
  public:
    TModule(const char *name="",const char *dummy=0);
    virtual ~TModule();
     
   virtual Int_t Make();
   virtual void  ResetModule(bool deleteOnly=false);
   TIter *Iter() const { return fNxtModule;}
   const StMaker *CurrentMaker() const { return fCurrentMaker;}


   virtual Int_t        GetIventNumber() const ;
   virtual void         SetIventNumber(Int_t iv);
   virtual Int_t        GetEventNumber() const ;
   virtual Int_t        GetRunNumber() const ;
   virtual UInt_t       GetTriggerMask() const;
   virtual const Char_t *GetEventType() const ;

};


#else
#include <assert.h>
#include "Ttypes.h"
#include "TDataSet.h"
#include "TDataSetIter.h"
#include "TObjectSet.h"
#include <TString.h>
#include <TDatime.h>
#include <TH1.h>
#ifndef ATLAS_TClonesArray
#include <TClonesArray.h>
#endif
#include <TStopwatch.h>

class TList;
class TBrowser;
class TChain;
class TTree;
class TTable;
class StMemStat;

class TModule : public TDataSet{
public:
   typedef  enum {kNormal, kDebug} EDebugLevel;
   enum {kSTAFCV_BAD, kSTAFCV_OK, kSTAFCV_ERR=2, kSTAFCV_FATAL=3} EModule_return_Status;
   enum {kOK, kWarn, kEOF, kErr, kFatal} TModule_return_Status;

protected:

   TDataSet     *m_DataSet;		//!  
   TDataSet     *m_ConstSet;		//!  
   TDataSet     *m_GarbSet;		//!  
   TDataSet     *m_Inputs;	 	//!list of logInput:ActualInput
   TDataSet     *m_Ouputs;	 	//!list of logOuput:ActualOuput
   TDataSet     *m_Runco;	 	//!Run Control parameters
   TList          *m_Histograms;	//!list of Histograms
   static StMaker *fgStChain;     	//current pointer to StChain
   static StMaker *fgFailedModule;     	//current pointer to failed maker
   static Int_t fgTallyModule[kFatal+1]; //counters
   Int_t	   m_Mode;		// Integer mode of maker
   Int_t           m_Number;        	//Serial event number
   Int_t           m_DebugLevel;    	//Debug level
   Int_t           m_MakeReturn;    	//Make() return flag
   TStopwatch      m_Timer;             //Timer object
  
   Bool_t          fActive;             // true if active
   StMemStat       *fMemStatMake;        //!TMemStat for Make
   StMemStat       *fMemStatClear;       //!TMemStat for Clear

public:

//		Constructor & Destructor

                  	TModule(const char *name="",const char *dummy=0);
   virtual       	~TModule();
   virtual Int_t IsChain() const {return 0;}


//		User defined functions
   virtual void   	Clear(Option_t *option="");
   virtual Int_t  	InitRun(int runumber);
   virtual Int_t  	Init();
   virtual void   	StartModule();
   virtual Int_t  	Make();
   virtual Int_t  	Make(int number){SetNumber(number);return Make();};
   virtual void   	EndModule  (int ierr);
   virtual Int_t  	Finish();
   virtual Int_t  	FinishRun(int oldrunumber);
   virtual void	   FatalErr(int Ierr, const char *Com) const;  
   virtual void   	PrintInfo() const;
   virtual void         AddModule (StMaker *mk);

//		User methods
   virtual TDataSet   *AddData (TDataSet *data=0,const char *dir=".data");
   virtual TObjectSet *AddObj  (TObject *obj,const char *dir);
   virtual void        	AddConst(TDataSet *data=0){AddData(data,".const");}
   virtual void        	AddHist(TH1 *h,const char *dir=0);
   virtual void        	AddGarb (TDataSet *data=0){AddData(data,".garb");};
   virtual void        	AddRunco (TDataSet *data=0){AddData(data,".runco");};
   virtual void        	AddRunco (double par,const char* name,const char* comment);
           void        	AddRunCont (TDataSet *data=0){AddRunco(data);};	//alias
   virtual TList       *GetHistList() const {return (TList*)GetDirObj(".hist");};
   virtual TH1         *GetHist(const Char_t *histName) const {TList *l=GetHistList(); return l?(TH1*)l->FindObject(histName):(TH1*)0;};
   virtual TModule     *cd(){StMaker *ret = fgStChain; fgStChain=this; return ret;};
   virtual TModule     *Cd(){return cd();};
   static  TModule     *New(const Char_t *classname, const Char_t *name="", void *title=0);
//		STAR methods
   virtual Int_t  	GetNumber() const ;
   virtual void   	SetNumber(Int_t number) ;
   static  TModule     *GetChain(){return fgStChain;}
   static  TModule     *GetFailedModule(){return fgFailedModule;}
   virtual TModule     *GetParentChain() const;

   virtual Int_t        GetIventNumber() const ;
   virtual void         SetIventNumber(Int_t iv);
   virtual Int_t        GetEventNumber() const ;
   virtual Int_t        GetRunNumber() const ;
   virtual UInt_t       GetTriggerMask() const;


   virtual TDatime      GetDateTime() const;
   virtual Int_t     	GetDate()  const ;
   virtual const char  *GetDateString()  const;
   virtual Int_t     	GetTime()  const ;
   virtual void         SetDateTime(int idat,int itim);

   virtual const Char_t *GetEventType() const ;



//		Get
   virtual TDataSet  *GetData(const char *name, const char* dir=".data") const;
   virtual TDataSet  *GetDataSet (const char* logInput,
                                    const StMaker *uppMk=0,
                                    const StMaker *dowMk=0) const ;
   virtual TDataSet  *   DataSet (const char* logInput)   const 
                           {return GetDataSet(logInput);};
   virtual TDataSet  *GetInputDS (const char* logInput)   const 
                           {return GetDataSet(logInput);};

   virtual TDataSet  *GetDataBase(const char* logInput);
   virtual TDataSet  *GetInputDB (const char* logInput)
                          {return GetDataBase(logInput);};
//    virtual Int_t   GetValidity(const TTable *tb, TDatime *val) const;
   virtual TDataSet*  UpdateDB(TDataSet* ds){if (ds){};return 0;};
   virtual TDataSet *UpdateDB(const char* logInput);



   virtual Int_t 	GetDebug() const {return m_DebugLevel;}
   virtual Int_t 	   Debug() const {return GetDebug();};
   virtual Int_t 	GetMakeReturn() const {return m_MakeReturn;}
   virtual TList       *Histograms()  const {return GetHistList();}
   virtual TString      GetAlias (const char* log, const char* dir=".aliases") const ;
   virtual TString      GetInput (const char* log) const {return GetAlias(log);};
   virtual TString      GetOutput(const char* log) const {return GetAlias(log,".aliases");};
   virtual TList       *GetMakeList() const ;
   virtual TModule     *GetParentModule () const;
   virtual TModule     *GetModule (const char *mkname);
   virtual Bool_t       IsActive() {return fActive;}
   virtual TModule     *Module (const char *mkname){return GetModule (mkname);};


//    Setters for flags and switches

   virtual void        	SetDebug(Int_t l=1){m_DebugLevel = l;}   // *MENU*
   virtual void        	SetDEBUG(Int_t l=1);                     // *MENU*
   virtual void         SetActive(Bool_t k=kTRUE){fActive = k;}  // *MENU*
   virtual void         SetMakeReturn(Int_t ret){m_MakeReturn=ret;}  
   virtual void       	SetAlias(const char* log,const char* act,const char* dir=".aliases");
   virtual void       	AddAlias(const char* log,const char* act,const char* dir=".aliases");
   virtual void       	SetInput(const char* log,const char* act){SetAlias(log,act);};
   virtual void       	SetOutput(const char* log,const char* act){SetAlias(log,act,".aliases");};
   virtual void       	SetOutput(const char* log,TDataSet *ds);
   virtual void       	SetOutput(TDataSet *ds){SetOutput(0,ds);};
   virtual void       	SetOutputAll(TDataSet *ds,Int_t level=1);
   virtual void   	SetMode(Int_t mode=0)   {m_Mode=mode;}   // *MENU*

   virtual Double_t     RealTime(){ return m_Timer.RealTime();}
   virtual Double_t     CpuTime() { return m_Timer.CpuTime();}
   virtual void   	StartTimer(Bool_t reset = kFALSE){m_Timer.Start(reset);}
   virtual void   	StopTimer(){m_Timer.Stop();}
   virtual void   	PrintTimer(Option_t *option="");
   virtual void         PrintTotalTime(){}
   
//  doPs interface
   void DoPs(const char*opt);
//		Static functions
   static  TModule     *GetModule(const TDataSet *ds)  ;
   static EDataSetPass  ClearDS (TDataSet* ds,void *user );


   TObject        *GetDirObj(const char *dir) const;
   void            SetDirObj(TObject *obj,const char *dir);

// ----------------------------------
// New method for Event display    //
// ----------------------------------
   virtual    Int_t       Remake();


  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: TModule.h,v 1.1 2009/12/01 01:33:33 fine Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(TModule, 0)   // base class to define the member of the chain to perform some complex action ("Root" service within Athena framework, for example)
};

//_______________________________________________________________________
inline void TModule::SetDateTime(int,int){;}

//_______________________________________________________________________
//_______________________________________________________________________
class TModuleIter 
{
public:
  TModuleIter(StMaker *mk, Int_t second = 0);
 ~TModuleIter();
  StMaker *NextModule();
  StMaker *GetModule () const {return fModule;}
private:
  Int_t fState;			//!
  Int_t fSecond;		//!
  StMaker *fModule;		//!
  TModuleIter *fModuleIter;	//!
  TDataSet *fItWas;		//!
  TDataSetIter *fIter;		//!
};  
#endif
#endif
