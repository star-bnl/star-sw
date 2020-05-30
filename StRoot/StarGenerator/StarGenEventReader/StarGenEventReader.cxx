#include "StarGenEventReader.h"
#include "TTree.h"
//________________________________________________________________________________
Int_t StarGenEventReader::Init() {
  TString filename(SAttr("InputFile"));
  if (filename != "") SetInputFile(filename);
  return StMaker::Init();
}
//________________________________________________________________________________
Int_t StarGenEventReader::Generate()
{
  if ( mInputTree ) mInputTree -> GetEntry( mEntry++ );
  if ( IAttr("debug") ) mEvent->Print();
  return kStOK;
};
