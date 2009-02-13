// K. Yip : Feb. 15, 2008 
//
// iswitch : 0 => for the 1st 16 bits of P2P
//         : 1 => for the 2nd 16 bits of P2P



#ifndef pp2ppHistogramGroup_h
#define pp2ppHistogramGroup_h


#include "Rtypes.h"
#include "HistogramGroup.h"

class pp2ppHistogramGroup : public HistogramGroup {

public:
  pp2ppHistogramGroup();

  pp2ppHistogramGroup(unsigned int iswitch, const char* group="P2P", const char* subGroup="pp2pp", const char* trigger="any", const char* detector="pp2pp");
  ~pp2ppHistogramGroup();

  virtual void reset();
  virtual bool fill(evpReader* evp, char* datap);
  virtual void draw(TCanvas* cc); 

private:

  enum {
    mMaxBits     = 16,
  };

  TH1D* h_P2P[mMaxBits]; 

  unsigned int mswitch ;


  ClassDef(pp2ppHistogramGroup,1) ;
};

//inline unsigned short pp2ppHistogramGroup::getNHT(int v) const {return (unsigned short)  (v & 0x000000FF); }

#endif
