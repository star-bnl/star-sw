#ifndef __StarMuEventReader_h__
#define __StarMuEventReader_h__

#include "StarMCPrimaryGenerator.h"
#include "StarGenerator/EVENT/StarGenEvent.h"
#include "TTreeIter.h"

class StarMuEventReader : public StarMCPrimaryGenerator
{
public:
 StarMuEventReader() : StarMCPrimaryGenerator() {}
 ~StarMuEventReader(){ /* nada */ };
  void GeneratePrimaries() {Generate();}
  Int_t Init();
  Int_t Generate();
  Int_t Skip(Int_t Nskip);
  Int_t ReadEvent(Int_t N = 0);
  void  SetMuDstFile(const Char_t *muDstFile);
  static void  UseGlobals() {fgUseOnlyPrimaries = kFALSE;}
private:
protected:
  TTreeIter *fMuDstIter;
  static Bool_t fgUseOnlyPrimaries;
  ClassDef( StarMuEventReader, 1 );
};

#endif
