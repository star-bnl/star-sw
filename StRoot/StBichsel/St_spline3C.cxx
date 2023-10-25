#include "Riostream.h"
#include "St_spline3C.h"
#include "TString.h"
#include "TInterpreter.h"
#include "TSystem.h"
//________________________________________________________________________________
St_spline3  *St_spline3C::Open(const Char_t *path) {
  St_spline3 *table = 0;
  TString PATH(path);
  TString Dir(gSystem->DirName(PATH));
  TString File(gSystem->BaseName(PATH));
  File += ".C";
  TString pathF(".:./StarDb/"); pathF +=  Dir + ":$STAR/StarDb/" + Dir;
  Char_t *file = gSystem->Which(pathF,File,kReadPermission);		
  if (! file) {
    std::cout << Form("Fatal::St_spline3C::Open   \tFile %s has not been found in path %s",File.Data(),pathF.Data()) << std::endl;
    return table;
  } else   {
    std::cout << Form("Warning::St_spline3C::Open \tFile %s has been found as %s",File.Data(),file) << std::endl;
  }
  TString command(".L "); command += file; TInterpreter::EErrorCode ee; 
  gInterpreter->ProcessLine(command,&ee);				
  if (ee) { //assert(!ee);							
    std::cout << Form("Fatal::St_spline3C::Open has failed to read  \tFile %s",file) << std::endl;
    delete [] file;
    return table;
  }
  table = (St_spline3 *) gInterpreter->Calc("CreateTable()",&ee); 
  if (! table) {//assert(table);	  
    std::cout << Form("Fatal::St_spline3C::Open has failed to load  \tFile %s",file) << std::endl;
    delete [] file;
    return table;
  }
  table->Print(0,1);
  command.ReplaceAll(".L ",".U ");
  gInterpreter->ProcessLine(command,&ee);	
  if (ee) { // assert(!ee);				
    std::cout << Form("Fatal::St_spline3C::Open has failed to unload  \tFile %s",file) << std::endl;
    delete [] file;
    SafeDelete(table);
    return table;
  }
  return table;
}
//________________________________________________________________________________
St_spline3C::St_spline3C(St_spline3 *table) : TChair(table), fSpline(0), fFunc(0), fValid(kTRUE) {
  if (table) {
    fSpline = new TSpline3("Spline3", Xknots(), Yknots(), nknots(), option(), ValBeg(), ValEnd());               
    fSpline->SetLineColor(2);
    fXmin = Xknots()[0] - 0.1;
    fXmax = Xknots()[nknots()-1] + 0.1;
    fFunc   = new TF1(GetName(), this, fXmin, fXmax, 0, "St_spline3C"); 
    fFunc->SetNpx(100);
    fFunc->Save(Xknots()[0], Xknots()[nknots()-1], 0., 0., 0., 0.);
  } else {
    fValid = kFALSE;
  }
}
#define MakeChairInstance3(CLASS,PATH)					\
  ClassImp(CLASS);							\
  CLASS *CLASS::fgInstance = 0;						\
  CLASS *CLASS::instance() {						\
    if (fgInstance && ! fgInstance->IsValid()) return 0;		\
    if (fgInstance) return fgInstance;					\
    St_spline3 *table = St_spline3C::Open(# PATH);			\
    fgInstance = new CLASS(table);					\
    return fgInstance;							\
  }
//________________________________________________________________________________
MakeChairInstance3(Stspline3LndNdxL10,dEdxModel/spline3LndNdxL10);
MakeChairInstance3(StElectonsDEV_dEdx,dEdxModel/ElectonsDEV_dEdx);
MakeChairInstance3(StPionDEV_dEdx,dEdxModel/PionDEV_dEdx);
MakeChairInstance3(StProtonDEV_dEdx,dEdxModel/ProtonDEV_dEdx);
