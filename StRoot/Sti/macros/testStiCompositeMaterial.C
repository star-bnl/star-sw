/*
  Tests the StiCompositeMaterial class.

  To run:

  root4star -b

  gSystem->Load("St_base");           // needed by StEvent
  gSystem->Load("StarClassLibrary");  // needed by StEvent
  gSystem->Load("StEvent");           // needed by Sti

  gSystem->Load("StUtilities");       // needed by StSvtDbMaker, StTpcDb
  gSystem->Load("StChain");           // needed by StSvtDbMaker
  gSystem->Load("StSvtDbMaker");      // needed by Sti

  gSystem->Load("St_Tables");         // needed by StTpcDb           
  gSystem->Load("StTpcDb");           // needed by Sti

  gSystem->Load("Sti");

  .L testStiCompositeMaterial.C++
  testStiCompositeMaterial()
*/

#include "StRoot/Sti/StiCompositeMaterial.h"

void testStiCompositeMaterial(){

  // Make a composite material representing Methane (CH4).
  // The ionization potential, density, and rad length
  // will be wrong, but it will give us the right effective A & Z.

  StiMaterial *pCarbon = new StiMaterial();
  pCarbon->set("C", 2.265, 18.8, 12.011, 6, 11.2603);
  StiMaterial *pHydrogen = new StiMaterial();
  // density & rad length are a kluge (/2 and x2 that of hydrogen gas)
  pHydrogen->set("H", 0.0000419, 1462000, 1.00794, 1, 13.5984);

  StiCompositeMaterial *pMethane = new StiCompositeMaterial();
  pMethane->setName("Methane");
  pMethane->addByNumber(pCarbon, 1.);
  pMethane->addByNumber(pHydrogen, 4.);

  cout << "Calculated methane:" << endl;
  cout << *pMethane;

  // now we have to fix the other values of methane from a table:
  pMethane->setDensity(.000667);  // @STP
  pMethane->setRadLength(69647.85);  // @STP
  pMethane->setIonization(12.98);

  cout << "Fixed methane:" << endl;
  cout << *pMethane;

  // Now make a composite material for P10 gas.  We will add by number
  // again because it is a 9:1 mix by volume, which is the same as
  // by number for ideal gasses.

  StiMaterial *pArgon = new StiMaterial();
  pArgon->set("Ar", .00166, 11762.31, 39.948, 18, 15.7596);
  //                @STP    @STP

  StiCompositeMaterial *pP10 = new StiCompositeMaterial();
  pP10->setName("P10");
  pP10->addByNumber(pArgon, 9.);
  pP10->addByNumber(pMethane, 1.);

  cout << "Calculated P10:" << endl;
  cout << *pP10;
  
  // Make a material representing the published characteristics of P10
  // at operating pressure & temperature:
  // http://www.star.bnl.gov/STAR/html/tpc_l/hard/tpcrings/page2.html
  //
  // For methane & argon above we used quoted values from the PDG 
  // website (at STP?):
  // http://pdg.lbl.gov/AtomicNuclearProperties/

  StiMaterial *pP10real = new StiMaterial();
  pP10real->set("P10", 0.00156, 12820, 36.2741, 16.4, 15.4816);

  cout << "Published P10:" << endl;
  cout << *pP10real << endl;

}

