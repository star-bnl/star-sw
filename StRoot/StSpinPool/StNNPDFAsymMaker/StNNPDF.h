#ifndef ST_NNPDF
#define ST_NNPDF
#include "NNPDFDriver.h"

#include "TMath.h"
//#include "TObject.h"

//class StNNPDF : public TObject
class StNNPDF
{
 public:
 StNNPDF() : pdf(0){}
  StNNPDF(const char *name){
    //    nnpdfdriver_(name);
    pdf = new NNPDFDriver(name);
  }
  StNNPDF(const char *name, int iset){
    pdf = new NNPDFDriver(name, iset);
  }
  void InitPDF(int iset){
    //    nninitpdf_(&iset);
    pdf->initPDF(iset);
  }
  ~StNNPDF(){
    if(! pdf) delete pdf;
  }
  double XPDF(int flavor, double x, double Q2){
      double Q = TMath::Sqrt(Q2);
      //      double xfx[14] = {0.};
      //      nnevolvepdf_(&x, &Q, xfx);
      int fl = flavor;
      if(fl == 21) fl = 0;//gluon flavor
      if(TMath::Abs(fl) <= 6)
	//          return xfx[flavor+6];
	return pdf->xfx(x,Q,fl);
      else
	return 1000.;
  }
 private:
  NNPDFDriver *pdf;
  //  ClassDef(StNNPDF, 0);
};

#endif
