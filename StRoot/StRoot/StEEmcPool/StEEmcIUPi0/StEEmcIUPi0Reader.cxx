#include "StEEmcIUPi0Reader.h"

#include "TString.h"

ClassImp(StEEmcIUPi0Reader);

// ----------------------------------------------------------------------------
StEEmcIUPi0Reader::StEEmcIUPi0Reader(const Char_t *name):StMaker(name)
{
  mChain=new TChain(name,"pi0 tree");
  index=0;
}

// ----------------------------------------------------------------------------
Int_t StEEmcIUPi0Reader::Init()
{ 
  mEvent=new StEEmcIUMixEvent();
  mChain-> SetBranchAddress("MixEvent",&mEvent);
  return StMaker::Init();
}

// ----------------------------------------------------------------------------
Int_t StEEmcIUPi0Reader::Make()
{
  return getEvent(index++);
}

// ----------------------------------------------------------------------------
Int_t StEEmcIUPi0Reader::getEvent(Int_t i)
{
  Int_t stat=mChain->GetEntry(i);
  if (!stat)return kStEOF;    
  return kStOK;
}

// ----------------------------------------------------------------------------
void StEEmcIUPi0Reader::Clear(Option_t *opts)
{
}

// ----------------------------------------------------------------------------
void StEEmcIUPi0Reader::chainFile( const Char_t *file )
{
  
  TString fname=file;
  if ( !fname.Contains("root") ) return;
  std::cout << "+ " << fname << std::endl;
  mChain->Add(fname);

}
