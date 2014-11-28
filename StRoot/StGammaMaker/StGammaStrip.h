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
        StGammaStrip();
        ~StGammaStrip() {};
        
        virtual const char* GetCVS() const
        {static const char cvs[] = "Tag $Name:  $ $Id: StGammaStrip.h,v 1.9 2014/08/06 11:43:18 jeromel Exp $ built " __DATE__ " " __TIME__; return cvs; }
        
        Int_t index;      // index of strip in plane
        Int_t sector;     // or bemc module
        Int_t plane;      // 0=esmd-u 1=esmdv 10=bsmd-eta 11=bsmd-phi
        Float_t energy;   // energy deposited
        Int_t adc;        // ADC
        Int_t stat;       // status bits (non fatal HW problems)
        Int_t fail;       // fail bits (fatal HW problems)

        // Strip geometry for shower shape analyses
        // In the BEMC:
        //   Eta Strips - Location in z (cm)
        //   Phi Strips - Location in phi (radians)
        // In the EEMC:
        //   left/right not used
        //   U Strips - position -> U Index
        //   V Strips - position -> V Index
        Float_t position;
        Float_t left;
        Float_t right;

        TRefArray candidates; // referencing candidates
        void print();

  ClassDef(StGammaStrip, 3);
  
};

typedef std::vector<StGammaStrip> StGammaStripVec_t;

#endif
