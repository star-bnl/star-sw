/* $Id: StarRndm.cxx,v 1.1 2004/07/12 20:36:39 potekhin Exp $ */

///////////////////////////////////////////////////
//   Random number generator base class          //
//   This class allows to have different         //
//   random number generator for different       //
//   elements of simulation                      //
//   It also allows saving and retrieval         //
//   of the random number seeds                  //
///////////////////////////////////////////////////

#include "TFile.h"
#include "TError.h"
#include "TRandom3.h"
#include "TSystem.h"

#include "StarRndm.h"

ClassImp(StarRndm)

//_______________________________________________________________________
StarRndm::StarRndm():
  fRandom(gRandom)
{
  // 
  // Default ctor
  //
}

//_______________________________________________________________________
StarRndm::StarRndm(const StarRndm& rn):
  fRandom(gRandom)
{
  //
  // Copy constructor
  //
  rn.Copy(*this);
}

//_______________________________________________________________________
void StarRndm::Copy(StarRndm&) const
{
  ::Fatal("Copy","Not implemented\n");
}


//_____________________________________________________________________________
void StarRndm::Rndm(Float_t* array, Int_t size) const
{
  //
  // Return an array of n random numbers uniformly distributed 
  // between 0 and 1 not included
  //
  for(Int_t i=0; i<size; i++) 
#ifdef CKNONE
    array[i]=fRandom->Rndm();
#else
    do array[i]=fRandom->Rndm(); while(0>=array[i] || array[i]>=1);
#endif
}

//_____________________________________________________________________________
void StarRndm::ReadRandom(const char *filename)
{
  //
  // Reads saved random generator status from filename
  //
  char *fntmp = gSystem->ExpandPathName(filename);
  TFile *file = new TFile(fntmp,"r");
  delete [] fntmp;
  if(!file) {
    printf("StarRndm:: Could not open file %s\n",filename);
  } else {
    if(!fRandom) fRandom = new TRandom();
    fRandom->Read("Random");
    file->Close();
    delete file;
  }
}

//_____________________________________________________________________________
void StarRndm::WriteRandom(const char *filename) const
{
  //
  // Writes random generator status to filename
  //
  char *fntmp = gSystem->ExpandPathName(filename);
  TFile *file = new TFile(fntmp,"new");
  delete [] fntmp;
  if(!file) {
    printf("StarRndm:: Could not open file %s\n",filename);
  } else {
    fRandom->Write();
    file->Close();
    delete file;
  }
}
