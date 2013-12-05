#ifndef __StarAgmlChecker_h__
#define __StarAgmlChecker_h__

#include "TGeoChecker.h"
#include "TObjectSet.h"
#include "TMath.h"

class StarAgmlChecker : public TGeoChecker
{
 public:
  StarAgmlChecker( TGeoManager *manager );
  ~StarAgmlChecker(){ /* nada */ };

  TObjectSet *MaterialPlot( const Char_t  *top   ="CAVE"  , 
			    const Int_t    nEta  =   100  , 
			    const Double_t mnEta = -5.00  ,
			    const Double_t mxEta = +5.00  ,
			    const Int_t    nPhi  =   360  ,
			    const Double_t mnPhi = -TMath::Pi() ,
			    const Double_t mxPhi = +TMath::Pi() ,
			    const Double_t rmin  =    0.0 ,
			    const Double_t rmax  =    400.0, // ~30cm outside of MAGP
			    const Option_t *opts = "top" );

 private:
 protected:
  void Fill( TObjectSet *set );

  ClassDef(StarAgmlChecker,1);

};

#endif
