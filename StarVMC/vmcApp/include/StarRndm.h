#ifndef STARRNDM_H
#define STARRNDM_H

/* $Id: StarRndm.h,v 1.1 2004/07/12 20:35:59 potekhin Exp $ */

//////////////////////////////////////////////
//                                          //
//   Random Number Generator Interface      //
//                                          //
//////////////////////////////////////////////

#include <TRandom.h>

class StarRndm 
{
public:
  StarRndm();
  StarRndm(const StarRndm &rn);
  virtual ~StarRndm() {fRandom=0;}
  StarRndm & operator=(const StarRndm& rn) 
    {rn.Copy(*this); return (*this);}
  
  // Random number generator bit
  virtual void SetRandom(TRandom *ran=0)
  {if(ran) fRandom=ran;
  else fRandom=gRandom;}

  virtual TRandom* GetRandom() const {return fRandom;}
  virtual void Rndm(Float_t* array, Int_t size) const; 
#ifdef CKNONE
  virtual Float_t Rndm() const {return fRandom->Rndm();}
#else
  virtual Float_t Rndm() const {
    Float_t r;
    do r=fRandom->Rndm(); while(0>=r || r>=1); return r;}
#endif
  virtual void WriteRandom(const char *filename) const;
  virtual void ReadRandom(const char *filename);

protected:
  TRandom *fRandom;       // Pointer to the random number generator

private:
  void Copy(StarRndm &rn) const;

  ClassDef(StarRndm,1)  //Random Number generator wrapper
};

#endif 
