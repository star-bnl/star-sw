#ifndef UPCHistogramCtbGroup_h
#define UPCHistogramCtbGroup_h


#include "Rtypes.h"
#include "HistogramGroup.h"


class UPCHistogramCtbGroup : public HistogramGroup {
public:
  UPCHistogramCtbGroup();
  UPCHistogramCtbGroup(const char* group, const char* subGroup="ctb", const char* trigger="upc", const char* detector="ctb");
  ~UPCHistogramCtbGroup();

  virtual void reset();
  virtual bool fill(evpReader* evp, char* datap);
  virtual void draw(TCanvas* cc); 

private:

  void putCdbLabels(TH1* h);

  static const unsigned char mCtbMap[2][120];  // CTB byte mapping

  TH1* h_ctb_adc_sum;        // ADC sum spectrum
  TH1* h_ctb_count_vs_tray;  // number of counts in (trays, slats)
  TH2* h_ctb_cdb;            // ADC spectra of the 16 CDBs
  TH2* h_ctb_cdb_zoom;       // ADC spectra of the 16 CDBs (zoomed)

  ClassDef(UPCHistogramCtbGroup,1) ;

};


#endif
