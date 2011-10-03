// $Id: EEmcTowerHit.h,v 1.1.1.1 2003/12/18 18:00:54 zolnie Exp $

#ifndef STAR_EEmcTowerHit
#define STAR_EEmcTowerHit

/*!
 *                                                                     
 * \class  EEmcTowerHit
 * \author Piotr A. Zolnierczuk
 * \date   2003/12/17
 * \brief  
 *
 * Not the nicest solution, but considering the other stuff
 * floating around STAR - this is at an AUDI.
 *
 */                                                                      


class TObject;


class EEmcTowerHit : public TObject {

 public:
  EEmcTowerHit(Int_t s=0, Int_t ss=0, Int_t e=0, Int_t adc=0, Float_t edep=0.0);
  virtual ~EEmcTowerHit() {};


  // "set" methods 
  void    setTower    (Int_t s  , Int_t ss       , Int_t e         ); // sets mSec,mSub and mEta
  Float_t setEnergyADC(Int_t adc, Float_t ped=0.0, Float_t gain=0.0); // sets mADC, calculates/sets/returns mEdep

  void    setEnergy(Float_t edep) { mEdep = edep; };
  void    setADC   (Int_t   adc ) { mADC=adc;     };

  // "get" methods  
  Int_t   sector()    { return mSec;  }
  Int_t   subsector() { return mSub;  }
  Int_t   etabin()    { return mEta;  }
  Int_t   adc()       { return mADC;  }
  Float_t energy()    { return mEdep; }

 private:
  
  Int_t   mSec; 
  Int_t   mSub; 
  Int_t   mEta;
  Int_t   mADC;
  Float_t mEdep;

  ClassDef(EEmcTowerHit, 1)   // 
    
};

#endif


// $Log: EEmcTowerHit.h,v $
// Revision 1.1.1.1  2003/12/18 18:00:54  zolnie
// Imported sources
//
//
