#include "StEEmcPi0Reader.h"

#include "TString.h"

ClassImp(StEEmcPi0Reader);

// ----------------------------------------------------------------------------
StEEmcPi0Reader::StEEmcPi0Reader(const Char_t *name,const Char_t *bname):StMaker(name)
{
  mChain=new TChain(name,"pi0 tree");
  mBranchName=bname;
  index=0;
}

// ----------------------------------------------------------------------------
Int_t StEEmcPi0Reader::Init()
{ 
  mEvent=new StEEmcMixEvent();
  mChain-> SetBranchAddress(mBranchName,&mEvent);
  return StMaker::Init();
}

// ----------------------------------------------------------------------------
Int_t StEEmcPi0Reader::Make()
{
  return getEvent(index++);
}

// ----------------------------------------------------------------------------
Int_t StEEmcPi0Reader::getEvent(Int_t i)
{
  Int_t stat=mChain->GetEntry(i);
  if (!stat)return kStEOF;    
  return kStOK;
}

// ----------------------------------------------------------------------------
void StEEmcPi0Reader::Clear(Option_t *opts)
{
}

// ----------------------------------------------------------------------------
void StEEmcPi0Reader::chainFile( const Char_t *file )
{
  
  TString fname=file;
  if ( !fname.Contains("root") ) return;
  std::cout << "+ " << fname << std::endl;
  mChain->Add(fname);

}
