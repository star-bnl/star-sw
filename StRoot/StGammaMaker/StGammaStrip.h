////////////////////////////////////////////////////////////
//                                                        //
//    StGammaStrip                                        //
//                                                        //
//    Lightweight class to hold SMD strip information     //
//                                                        //
//    Original concept and implementation by              //
//    Jason Webb (Valpo)                                  //
//                                                        //
////////////////////////////////////////////////////////////

#ifndef STAR_StGammaStrip
#define STAR_StGammaStrip

#include <TObject.h>
#include <vector>
#include <TRefArray.h>

enum { kEEmcSmdu=0, kEEmcSmdv=1, kBEmcSmdEta=10, kBEmcSmdPhi=11 };

class StGammaStrip: public TObject
{

    public:
        StGammaStrip()  {};
        ~StGammaStrip() {};
        
        virtual const char* GetCVS() const
        {static const char cvs[] = "Tag $Name:  $ $Id: StGammaStrip.h,v 1.6 2008/12/03 15:38:15 betan Exp $ built "__DATE__" "__TIME__; return cvs; }
        
        Int_t index;      // index of strip in plane
        Int_t sector;     // or bemc module
        Int_t plane;      // 0=esmd-u 1=esmdv 10=bsmd-eta 11=bsmd-phi
        Float_t energy;   // energy deposited
        Float_t position; // Reference position for the calculation of moments
                          // bsmd-eta -> theta
                          // bsmd-phi -> phi
                          // esmd-u -> u index
                          // esmd-v -> v index
        Int_t stat;       // status bits (non fatal HW problems)
        Int_t fail;       // fail bits (fatal HW problems)
        
        TRefArray candidates; // referencing candidates
        void print();

  ClassDef(StGammaStrip, 2);
  
};

typedef std::vector<StGammaStrip> StGammaStripVec_t;

#endif
