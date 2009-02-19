#ifndef FMSHistogramGroup_h
#define FMSHistogramGroup_h


#include "Rtypes.h"
#include "HistogramGroup.h"

class FMSHistogramGroup : public HistogramGroup {

public:

  FMSHistogramGroup();
  FMSHistogramGroup(const char* group, const char* subGroup="fms", const char* trigger="any", const char* detector="fms");
  ~FMSHistogramGroup();

  virtual void reset();
  virtual bool fill(evpReader* evp, char* datap);
  virtual void draw(TCanvas* cc); 

private:
  void createHistos();
  void deleteHistos();

  TH1* h_fms_nqtdata;               // # of QT data lines
  TH1* h_fms_nqtheader;             // # of QT header lines
  TH1* h_fms_qtdecode_err;          // # of QT decode error
  TH1* h_fms_quad_sum[4];           // # of QT quadrant sum
  TH1* h_fms_quad_mult[4];          // # of QT quadrant multiplicity

  enum {
    mMaxLine     = 1600,
    mMaxCrate    = 4,
    mMaxAddr     = 16,
    mMaxDCard    = 4,
    mMaxChan     = 8,
    mOffsetCrate = 11,
    mOffsetAddr  = 16,
    mQTLastWord  = 0xac10
  };

  int mNumQTdata;
  int mNumHeader;
  unsigned short mADC[mMaxCrate][mMaxAddr][mMaxDCard][mMaxChan];
  unsigned short mTDC[mMaxCrate][mMaxAddr][mMaxDCard][mMaxChan];
  unsigned int mBdSum[mMaxCrate][mMaxAddr];
  unsigned int mQuadSum[mMaxCrate];
  unsigned int mQuadMult[mMaxCrate];

  void readParams();
  int sbin[mMaxCrate], mbin[mMaxCrate], mthr[mMaxCrate];
  double smin[mMaxCrate],smax[mMaxCrate];
  double mmin[mMaxCrate],mmax[mMaxCrate];

  bool decodeQT();
  unsigned short getNHT(int) const;
  unsigned short getADR(int) const;
  unsigned short getCRT(int) const;
  unsigned short getADC(int) const;
  unsigned short getTDC(int) const;
  unsigned short getQT8(int) const;
  unsigned short getCHA(int) const; 
  unsigned short getUNU(int) const; 

  ClassDef(FMSHistogramGroup,1) ;
};

inline unsigned short FMSHistogramGroup::getNHT(int v) const {return (unsigned short)  (v & 0x000000FF); }
inline unsigned short FMSHistogramGroup::getADR(int v) const {return (unsigned short) ((v & 0x001F0000) >> 16);}
inline unsigned short FMSHistogramGroup::getCRT(int v) const {return (unsigned short) ((v & 0xFF000000) >> 24);}
inline unsigned short FMSHistogramGroup::getADC(int v) const {return (unsigned short)  (v & 0x00000FFF);}
inline unsigned short FMSHistogramGroup::getTDC(int v) const {return (unsigned short) ((v & 0x001F0000) >> 16);}
inline unsigned short FMSHistogramGroup::getQT8(int v) const {return (unsigned short) ((v & 0x18000000) >> 27);}
inline unsigned short FMSHistogramGroup::getCHA(int v) const {return (unsigned short) ((v & 0xE0000000) >> 29);} 
inline unsigned short FMSHistogramGroup::getUNU(int v) const {return (unsigned short)  (v & 0x07E0F000);} 

#endif
