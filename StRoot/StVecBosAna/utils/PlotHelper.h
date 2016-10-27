#ifndef PlotHelper_h
#define PlotHelper_h

#include <map>
#include <set>
#include <iostream>

#include "TCanvas.h"
#include "TClass.h"
#include "TDirectory.h"
#include "TH1.h"
#include "TH2F.h"
#include "THStack.h"
#include "TKey.h"
#include "TObject.h"
#include "TPaveStats.h"
#include "TPRegexp.h"
#include "TSystem.h"

#include "ProtoEvent.h"


class PlotHelper;

typedef std::map<std::string, TObject*>                    ObjMap;
typedef std::map<std::string, TObject*>::iterator          ObjMapIter;
typedef std::map<std::string, TObject*>::const_iterator    ObjMapConstIter;
typedef std::map<std::string, PlotHelper*>                 PlotHelperMap;
typedef std::map<std::string, PlotHelper*>::iterator       PlotHelperMapIter;
typedef std::map<std::string, PlotHelper*>::const_iterator PlotHelperMapConstIter;
typedef std::set<PlotHelper*>                              PlotHelperSet;
typedef std::set<PlotHelper*>::iterator                    PlotHelperSetIter;
typedef std::set<PlotHelper*>::const_iterator              PlotHelperSetConstIter;


/** */
class PlotHelper : public TObject
{
protected:

	std::string    fSignature;

public:

   TDirectory    *fDir;
   ObjMap         o;
   PlotHelperMap  d;

public:

   PlotHelper();
   PlotHelper(TDirectory *dir);
   virtual ~PlotHelper();

   std::string  GetSignature() const;
   virtual void SetSignature(const std::string signature);
   TDirectory*  GetDir();
   void         SetDir(TDirectory *dir);
   void         ReadFromDir();
   void         ReadFromDir(TDirectory *dir);
   void         Add(PlotHelper* oc);
   void         Print(const Option_t* opt="") const;

   virtual void PreFillPassOne();
   virtual void FillDerivedPassOne();
   virtual void PostFillPassOne(PlotHelper *oc=0);

   virtual void PreFill();
   virtual void Fill(ProtoEvent &ev);
   virtual void FillDerived();
   virtual void FillDerived(PlotHelper &oc);         // special processing for dependant histograms
   virtual void PostFill();
   virtual void PostFill(PlotHelper &oc);            // special processing for dependant histograms
   virtual void SaveAllAs(TCanvas &c, std::string pattern="^.*$", std::string path="./", Bool_t thumbs=kFALSE);
   virtual void SaveHStackAs(TCanvas &c, THStack &hstack, std::string path="./");
   void         Draw(Option_t* option = "") { ((TObject*)this)->Draw(option); }
   void         Draw(TCanvas &c);
   Int_t        Write(const char* name = 0, Int_t option = 0, Int_t bufsize = 0);
   Int_t        Write(const char* name = 0, Int_t option = 0, Int_t bufsize = 0) const;
   void         Delete(Option_t* option="");
   virtual void UpdateLimits();

   ClassDef(PlotHelper, 1)
};

#endif
