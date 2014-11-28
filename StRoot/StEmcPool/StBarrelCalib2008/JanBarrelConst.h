#ifndef JAN_BARREL_CONST_HH
#define JAN_BARREL_CONST_HH
// bsmd indexes needed for Jan's calibration
enum {mxBStrips=18000, mxBSmd=2, kBSE=0, kBSP=1}; 
enum {mxBtow=4800, mxBTile=2, kBTow=0, kBPrs=1} ; // tower, preshower indexes
enum {mxBprsCrate=4};
enum {mxBTetaBin=20}; // # of eta bins for towers, preshower
enum {mxBcap=128}; // # of capacitors in SCA 
enum {mxBetaEtrMod=150}; // # of Eta strip in module
enum {mxBphiStrBand=900}; // # of Phi strip in Barrel at fixed eta
enum { mxBXcell=20, mxYcell=60}; // dimension of 2D eta-phi array of cells
enum { mxBCutH=2}; // for cut dependent histograms


/*
Use idividual bits of 'Warn' to exclude individual
channels from a particular analysis, but let other 
analysis make a different choice.
*/


namespace BsmdStatus {// make your choice 
    //..... fatal status bits (short int) 
    enum Fatal {
      Adc0=       0x0001, // raw adc often stuck on zero
      WidePed=    0x0002, // pedestal is wide
      PedOffset=  0x0004, // DB pedestal is off
      OnlyPed=    0x0008, // spectrum above ped empty
      SlopeChi2=  0x0010, // slope chi2 is bad
      SlopeErr=   0x0020, // slope error is large
      SlopeValue= 0x0040,  // slope value not typical
      FakeStrip=  0x0080  // fake, alwasy dead strip 
    };

    // .......warning status bits (short int)
    enum  Warn {
      SpecialCAP= 0x0100 // CAP id=123,124, or 125
    } ;
}


#endif 
 
 
