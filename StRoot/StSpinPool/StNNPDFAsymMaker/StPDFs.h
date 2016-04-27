#ifndef ST_PDF_H
#define ST_PDF_H

#include "mstwpdf.h"
//#ifndef StMaker_H
extern "C"{
  //double ctq5pd_(int*,int*,double*,double*,int*);
  //void dssvini2009a_(int*);
  //void dssvfit2009a_(double*,double*,double*,double*,double*,double*,double*,double*);
  void dssvinig_(char *);
  void dssvgupdate_(double*,double*,double*,double*,double*,double*,double*,double*);
}
//#endif
class StPDFs{
 public:
  static void init_polPDF_DSSV2009a(char *file);
  static double get_polPDF_NLO_DSSV2009a(int flavor, double x, double Q2);

  void init_unpolPDF_NLO(char *prefix, int iset = 0);
  double get_unpolPDF_NLO(int flavor, double x, double Q2);
 private:
  c_mstwpdf *mstwpdf;
};
#endif
