// $Id: StMaker.h,v 1.21 1999/05/10 15:37:52 perev Exp $
// $Log: StMaker.h,v $
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
#ifndef STAR_StMaker
#define STAR_StMaker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StMaker virtual base class for Makers                                //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <assert.h>
#include "Stypes.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"
#include "St_ObjectSet.h"
#include "TString.h"
#include "TDatime.h"
#include "TH1.h"
#include "StEvtHddr.h"
#ifndef ROOT_TClonesArray
#include "TClonesArray.h"
#endif
#include "TStopwatch.h"

class TList;
class TBrowser;
class TChain;
class TTree;

class StMaker : public St_DataSet{
public:
   typedef  enum {kNormal, kDebug} EDebugLevel;
   enum {kSTAFCV_BAD, kSTAFCV_OK} EModule_return_Status;

protected:

   St_DataSet     *m_DataSet;		//!  
   St_DataSet     *m_ConstSet;		//!  
   St_DataSet     *m_GarbSet;		//!  
   St_DataSet     *m_Inputs;	 	//!list of logInput:ActualInput
   St_DataSet     *m_Ouputs;	 	//!list of logOuput:ActualOuput
   TList          *m_Histograms;	//!list of Histograms
   static StMaker *fgStChain;     	//current pointer to StChain
   Int_t	   m_Mode;		// Integer mode of maker
   Int_t           m_Number;        	//Serial event number
   Int_t           m_DebugLevel;    	//Debug level
   TStopwatch      m_Timer;             //Timer object

   StMaker        *gStChain;  		//???? Temporary ?????
public:

//		Constructor & Destructor

                  	StMaker();
                  	StMaker(const char *name,const char *dummy=0);
   virtual       	~StMaker();

//		User defined functions
   virtual void   	Clear(Option_t *option="");
   virtual Int_t  	Init();
   virtual void   	StartMaker();
   virtual Int_t  	Make();
   virtual Int_t  	Make(int number){SetNumber(number);return Make();};
   virtual void   	EndMaker  (int ierr);
   virtual Int_t  	Finish();
   virtual void	       	Fatal(int Ierr, const char *Com);  
   virtual void   	PrintInfo() const;

   virtual void   MakeDoc(const TString &stardir="$(STAR)",const TString &outdir="$(STAR)/StRoot/html",Bool_t baseClasses=kTRUE); 

//		User methods
   virtual St_DataSet   *AddData (St_DataSet *data=0,const char *dir=".data");
   virtual St_ObjectSet *AddObj  (TObject *obj,const char *dir);
   virtual void        	AddConst(St_DataSet *data=0){AddData(data,".const");}
   virtual void        	AddHist(TH1 *h,const char *dir=0);
   virtual void        	AddGarb (St_DataSet *data=0){AddData(data,".garb");};
   virtual TList       *GetHistList() const {return (TList*)GetDirObj(".hist");};
   virtual StMaker     *cd(){StMaker *ret = fgStChain; fgStChain=this; return ret;};
   virtual StMaker     *Cd(){return cd();};

//		STAR methods
   virtual Int_t  	GetNumber() const ;
   virtual void   	SetNumber(Int_t number) ;
   virtual St_DataSet*  UpdateDB(St_DataSet* ds){if (ds){};return 0;};
   virtual Int_t        GetEventNumber() const ;
   virtual Int_t        GetRunNumber() const ;
   virtual TDatime      GetDateTime() const;
   virtual Int_t     	GetDate()  const ;
   virtual Int_t     	GetTime()  const ;
   virtual const Char_t *GetEventType() const ;
   static  const Char_t *VersionTag() {return GetCVSTag();};


//		Get
   virtual St_DataSet  *GetData(const char *name, const char* dir=".data") const;
   virtual St_DataSet  *GetDataSet (const char* logInput,
                                    const StMaker *uppMk=0,
                                    const StMaker *dowMk=0) const ;
   virtual St_DataSet  *   DataSet (const char* logInput)   const 
                           {return GetDataSet(logInput);};
   virtual St_DataSet  *GetInputDS (const char* logInput)   const 
                           {return GetDataSet(logInput);};

   virtual St_DataSet  *GetDataBase(const char* logInput);
   virtual St_DataSet  *GetInputDB (const char* logInput)
                          {return GetDataBase(logInput);};


   virtual Int_t 	GetDebug() const {return m_DebugLevel;}
   virtual Int_t 	   Debug() const {return GetDebug();};
   virtual TList       *Histograms()  const {return GetHistList();}
   virtual TString      GetAlias (const char* log, const char* dir=".aliases") const ;
   virtual TString      GetInput (const char* log) const {return GetAlias(log);};
   virtual TString      GetOutput(const char* log) const {return GetAlias(log,".aliases");};
   virtual TList       *GetMakeList() const ;
   virtual StMaker     *GetParentMaker () const;
   virtual StMaker     *GetMaker (const char *mkname);
   virtual StMaker     *Maker (const char *mkname){return GetMaker (mkname);};


//    Setters for flags and switches

   virtual void        	SetDebug(Int_t l=1){m_DebugLevel=l;}
   virtual void       	SetAlias(const char* log,const char* act,const char* dir=".aliases");
   virtual void       	AddAlias(const char* log,const char* act,const char* dir=".aliases");
   virtual void       	SetInput(const char* log,const char* act){SetAlias(log,act);};
   virtual void       	SetOutput(const char* log,const char* act){SetAlias(log,act,".aliases");};
   virtual void       	SetOutput(const char* log,St_DataSet *ds);
   virtual void       	SetOutput(St_DataSet *ds){SetOutput(0,ds);};
   virtual void       	SetOutputAll(St_DataSet *ds);
   virtual void   	SetMode(Int_t mode=0)   {m_Mode=mode;}


   virtual void   	StartTimer(Bool_t reset = kFALSE){m_Timer.Start(reset);}
   virtual void   	StopTimer(){m_Timer.Stop();}
   virtual void   	PrintTimer(Option_t *option="");

//		Static functions
   static  StMaker     *GetMaker(const St_DataSet *ds)  ;
   static EDataSetPass  ClearDS (St_DataSet* ds,void *user );

//		must be in .cxx
   static const char   *GetCVSIdC();

//		must be in here in .h
   static const char   *GetCVSIdH() 
    {static const char cvs[]="$Id: StMaker.h,v 1.21 1999/05/10 15:37:52 perev Exp $";
     return cvs;};
   static const char   *GetCVSTag()
     {static const char cvs[]="$Name:  $"; return cvs;};

protected:
TObject        *GetDirObj(const char *dir) const;
void            SetDirObj(TObject *obj,const char *dir);
   ClassDef(StMaker, 0)   //StChain virtual base class for Makers
};

#endif
