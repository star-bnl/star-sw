#ifndef STAR_StNNPDFAsymMaker
#define STAR_StNNPDFAsymMaker

#ifndef StMaker_H
#include "StMaker.h"
extern "C"{
 void num_(double*,double*,double*,double*,double*,double*,double*,double*,double*,double*,double*);
 void denom_(double*,double*,double*,double*,double*,double*,double*,double*,double*,double*,double*);
}
#endif

class StPythiaEvent;
class StNNPDF;
class StPDFs;

class StNNPDFAsymMaker : public StMaker
{
private:    
    StNNPDF *mPolPdf;  
    StNNPDF *mUnpPdf;  
    StPythiaEvent * mEvent;
    Double_t getPartonicALL(double s, double t, double u, int pid, int flavor1, int flavor2, int flavor3, int flavor4);
    void getPythia();
    void getVertex();
    void getEvent();
    void getParticles();
    void getAsymmetries();
    
    StPDFs *mPdfs;
 public: 
 

    StNNPDFAsymMaker(const char *name="NNPDFAsym");
    ~StNNPDFAsymMaker();
    Int_t Init();
    Int_t Make();
    void Clear(const Option_t* option = "");

    StPythiaEvent* pythiaEvent() const { return mEvent; }
    const char *GetCVS() const {
      static const char cvs[]="Tag $Name:  $ $Id: StNNPDFAsymMaker.h,v 1.1.4.2 2016/05/23 18:43:25 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
      return cvs;
    }
    
    ClassDef(StNNPDFAsymMaker,0)   //StAF chain virtual base class for Makers
};

#endif
