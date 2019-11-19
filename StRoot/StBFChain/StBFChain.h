#ifndef STAR_StBFChain
#define STAR_StBFChain

//////////////////////////////////////////////////////////////////////////
/*!

 \class  StBFChain
 \author Yuri Fisyak, Jerome LAURET
 \date   1999/07/29 , 2001-2011
 @(#)StRoot/StBFChain:$Name:  $:$Id: StBFChain.h,v 1.59 2019/11/19 17:26:17 jeromel Exp $

 Class to control "BFC" chain

 This class :
   - Initialises the run default parameters
   - Provides API to Set/Get run parameters
   - Creates the support lists (TClonesArrays) for the Event structure
   - Creates the physics objects makers

*/
//////////////////////////////////////////////////////////////////////////
#include "StChain.h"
#include "TFile.h"
#include "TTable.h"
#include "Ttypes.h"
#include "Bfc.h"
//_____________________________________________________________________

class StFileI;
class TObjArray;

class StBFChain : public StChain {
 private:
  Bfc_st              *fBFC;      // Private chain
  StFileI             *fSetFiles; //
  TString             fInFile;    //
  TString             fFileOut;   //
  TFile              *fTFile;
  Int_t               FDate;      // floating timestamp date (MaxDateTime)
  Int_t               FTime;      // floating timestamp time (unused)
  Int_t               FDateS;     // floating timestamp date (DateTime)
  Int_t               FTimeS;     // floating timestamp time (DateTime)
  TString             fFiltTrg;   // trigger filtering properties (flavor, inc/exc)
  Int_t               fRunG;      // on fly simulation run no. & set for RDNM
  TString             fFmsGainCorrTag; // FMS GainCorrection Flavor Tag
  Int_t               fNoChainOptions;
  vector<TString>     Gproperty;  // a global property name
  vector<TString>     Gpattern;   // a global pattern
  vector<TString>     Gvalue;     // a global value
#ifdef USE_BFCTIMESTAMP
  StVecBFCTS          GTSOptions; // global set of detector specific timestamps
#endif /*  USE_BFCTIMESTAMP */
  St_Bfc             *fchainOpt;
  Int_t               fkChain;    // Master chain option

 public:
  StBFChain(const char *name="bfc", const Bool_t UseOwnHeader = kFALSE) :
           StChain(name,UseOwnHeader)
	     ,fBFC(0), fSetFiles(0),fInFile(""),fFileOut(""),fTFile(0)
	     ,FDate(0),FTime(0),FDateS(0),FTimeS(0),fFiltTrg(""),fRunG(0)
	     ,fNoChainOptions(0), fchainOpt(0), fkChain(-1) {}

#if 0
    StBFChain(Int_t /* mode */, const char *name="bfc",const Bool_t UseOwnHeader = kFALSE) :
            StChain(name,UseOwnHeader)
           ,fSetFiles(0),fInFile(""),fFileOut(""),fTFile(0)
	   ,fNoChainOptions(0), fchainOpt(0), fkChain(-1) {}
#endif
  void Setup(Int_t mode=1);
   virtual            ~StBFChain();
   virtual Int_t       Make(int number){ SetIventNumber(number); return StChain::Make(number);};
   virtual Int_t       Make(){return StChain::Make();};
   virtual Int_t       Load();             
   virtual Int_t       Instantiate();      
   virtual Int_t       Init();      
   virtual Int_t       AddAB (const Char_t *after="",const StMaker *maker=0,const Int_t Opt=1);
   virtual Int_t       AddAfter  (const Char_t *after, const StMaker *maker) {return AddAB (after,maker);}
   virtual Int_t       AddBefore (const Char_t *before,const StMaker *maker) {return AddAB (before,maker,-1);}
   static  Int_t       ParseString (const TString &tChain, TObjArray &Opt, Bool_t Sort=kFALSE);
   void                SetFlags(const Char_t *Chain="gstar tfs"); // *MENU*
   void                Set_IO_Files(const Char_t *infile=0, const Char_t *outfile=0); // *MENU
   void                SetInputFile(const Char_t *infile=0);                          // *MENU
   void                SetOutputFile(const Char_t *outfile=0);                        // *MENU
   void                SetTFile(TFile *tf)			{fTFile=tf;}
   TFile              *GetTFile() const			        {return fTFile;}
   virtual Int_t       kOpt(const TString *Tag, Bool_t Check = kTRUE) const;
   virtual Int_t       kOpt(const Char_t  *Tag, Bool_t Check = kTRUE) const;
   virtual void        SetDbOptions(StMaker *db=0);
   virtual void        SetGeantOptions(StMaker *geant=0);
   virtual void        SetTreeOptions();
   virtual void        SetOption(const Int_t k, const Char_t *chain="Chain");
   virtual void        SetOption(const Char_t*  Opt, const Char_t *chain="Chain") {SetOption(kOpt(Opt), chain);}
   virtual void        SetOption(const TString* Opt, const Char_t *chain="Chain") {SetOption(kOpt(Opt),chain);}
   virtual void        SetOptions(const Char_t*  Opt, const Char_t *chain="Chain");
   virtual void        SetOptionOff(const Char_t*  Opt, const Char_t *chain="Chain") {SetOption(-kOpt(Opt),chain);}
   virtual void        SetOptionOff(const TString* Opt, const Char_t *chain="Chain") {SetOption(-kOpt(Opt),chain);}
   virtual Int_t       Finish();
   virtual Option_t*   GetOption() const{return TObject::GetOption();}
   virtual Bool_t      GetOption(const Int_t k)  const;
   virtual Bool_t      GetOption(const TString *Opt, Bool_t Check = kTRUE) const {return GetOption(kOpt(Opt,Check));}
   virtual Bool_t      GetOption(const Char_t  *Opt, Bool_t Check = kTRUE) const {return GetOption(kOpt(Opt,Check));}
   virtual Char_t     *GetOptionString(const Char_t  *);
   virtual const TString &GetFileIn()  const {return *(&fInFile);}
   virtual const TString &GetFileOut() const {return *(&fFileOut);}
               TString GetGeometry() const;
   virtual Long_t      ProcessLine(const char *line);
   virtual const char *GetCVS() const {
       static const char cvs[]="Tag $Name:  $ $Id: StBFChain.h,v 1.59 2019/11/19 17:26:17 jeromel Exp $ built " __DATE__ " " __TIME__ ;
       return cvs;
   }
   /// StBFChain control class
   ClassDef(StBFChain, 3)
};
#endif
