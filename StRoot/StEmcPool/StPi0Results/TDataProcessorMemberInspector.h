#ifndef StPi0Results_TDataProcessorMemberInspector_H
#define StPi0Results_TDataProcessorMemberInspector_H

#include <TObject.h>
#include <TNamed.h>
#include <TString.h>
#include <TMemberInspector.h>
#include <TSysEvtHandler.h>

#include "StPi0ResultsVersion.h"

class TFolder;

class TDataProcessor;
class TDataProcessorPool;

class TDataProcessorMemberInspector: public TMemberInspector {
	public:
		typedef TDataProcessorMemberInspector this_type;
		typedef TMemberInspector inherited;
		typedef TFolder folder_type;
		typedef TObject named_type;
		typedef TDataProcessor processor_type;
		typedef TDataProcessorPool processor_pool_type;
	
		TDataProcessorMemberInspector();
		virtual ~TDataProcessorMemberInspector();
		
		Int_t debug;
		
		virtual folder_type *Inspect(named_type &named, folder_type *folder, const TString &name, const TString &title);
		virtual folder_type *Inspect(named_type *named, folder_type *folder, const TString &name, const TString &title) {return named ? this->Inspect(*named, folder, name, title) : 0;}
		virtual folder_type *Inspect(const named_type &named, folder_type *folder, const TString &name, const TString &title) {return this->Inspect((named_type&)(const_cast<named_type&>(named)), folder, name, title);}
		virtual folder_type *Inspect(const named_type *named, folder_type *folder, const TString &name, const TString &title) {return named ? this->Inspect(*named, folder, name, title) : 0;}
		virtual folder_type *Inspect(named_type &named, folder_type *folder) {return this->Inspect(named, folder, TString(named.GetName()), TString(named.GetTitle()));}
		virtual folder_type *Inspect(named_type *named, folder_type *folder) {return named ? this->Inspect(*named, folder) : 0;}
		virtual folder_type *Inspect(const named_type &named, folder_type *folder) {return this->Inspect(named, folder, TString(named.GetName()), TString(named.GetTitle()));}
		virtual folder_type *Inspect(const named_type *named, folder_type *folder) {return named ? this->Inspect(*named, folder) : 0;}

	protected:
		folder_type *mFolder;
		virtual void Inspect(TClass *cl, const char *parent, const char *name, const void *addr);
	
		ClassDef(TDataProcessorMemberInspector, STPI0RESULTS_VERSION);
};

class TMySignalHandler : public TSignalHandler {
public:
    TMySignalHandler(ESignals sig, TString fileIn, TString fileOut, Bool_t sync = kTRUE);
    virtual void     Add() {}
    virtual void     Remove() {}
    virtual Bool_t   Notify();
    
    TString filenameIn;
    TString filenameOut;

    TTimer *timer;

    ClassDef(TMySignalHandler, STPI0RESULTS_VERSION);
};
extern TMySignalHandler *mySignalHandler;

#endif
