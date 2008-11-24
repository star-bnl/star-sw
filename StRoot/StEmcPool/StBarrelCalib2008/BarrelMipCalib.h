#ifndef STAR_BarrelMipCalib_h
#define STAR_BarrelMipCalib_h


#include "JanBprsEveA.h"
class TH2F;
class  TObjArray ;
class  StMuDstMaker;
class  StJanBarrelDbMaker;
class  JanBarrelEvent;

class BarrelMipCalib {
 public:
  BarrelMipCalib( TObjArray *HList,  StJanBarrelDbMaker* , StMuDstMaker *);
  void search( JanBarrelEvent &fullEve);
  void searchEtaBin20( JanBarrelEvent &fullEve); // only for special use, to search for eta bin=20 location

  int checkFiducial(float zTr, float phiTr, int softID, float Rxy);

  //----------------------
  void  setCut(float z, float pt, float eta, float nff, float de, float zm, float rxy)
    { cut_zVertex=z; cut_primPt=pt; cut_primEta=eta; cut_nFitFrac=nff; cut_dedx=de; cut_zMargin=zm; cut_primRxy=rxy;}

  //----------------------
  void print(){
    printf("BarrelMipCalib CUT:  zVertex=<%.1f, primPt>%.1f, primEta<%.2f, nFitFrac>%.2f, dEdX<%.1fkeV,  zMargin=%.1fcm, primRxy>%.1fcm \n",
	   cut_zVertex, cut_primPt, cut_primEta, cut_nFitFrac, cut_dedx, cut_zMargin, cut_primRxy );
  }

 private:
    float  cut_zVertex, cut_primPt, cut_primEta,cut_nFitFrac, cut_dedx, cut_primRxy;
    float cut_zMargin;
    StJanBarrelDbMaker *mJanDbMaker;
    StMuDstMaker       * muMaker;
        
 public:
    enum {mxH=32};
    TH1 * hA[mxH];

};
#endif
