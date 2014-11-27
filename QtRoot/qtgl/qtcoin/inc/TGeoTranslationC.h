#ifndef STAR_TGeoTranslationF
#define STAR_TGeoTranslationF

#include "TGeoMatrix.h"
#include "TRotMatrix.h"
#include "TVolumePosition.h"
#include "TQtCoin3DDefInterface.h"

class TGeoTranslationC : public TGeoTranslation, public TQtCoin3DNode {
public:
 TGeoTranslationC() : 
      TGeoTranslation(), TQtCoin3DNode() {}
 TGeoTranslationC(const TGeoTranslationC& other) : 
      TGeoTranslation(other),TQtCoin3DNode(other) {}
 TGeoTranslationC(const TGeoTranslation& other) : 
      TGeoTranslation(other),TQtCoin3DNode() {}
 TGeoTranslationC(const TGeoMatrix& other) : 
      TGeoTranslation(other),TQtCoin3DNode() {}
 TGeoTranslationC(Double_t dx, Double_t dy, Double_t dz) : 
      TGeoTranslation(dx, dy, dz), TQtCoin3DNode(){}
 TGeoTranslationC(const char* name, Double_t dx, Double_t dy, Double_t dz): 
      TGeoTranslation(name, dx, dy, dz), TQtCoin3DNode(){}
};


class TRotMatrixC : public TRotMatrix, public TQtCoin3DNode {
public:
   TRotMatrixC(){;}
   TRotMatrixC(const char *name, const char *title, Double_t *matrix):
       TRotMatrix(name, title, matrix), TQtCoin3DNode() {}

   TRotMatrixC(const char *name, const char *title, Double_t theta, Double_t phi, Double_t psi):
      TRotMatrix(name, title, theta, phi, psi), TQtCoin3DNode(){}

   TRotMatrixC(const char *name, const char *title, Double_t theta1, Double_t phi1,
                                           Double_t theta2, Double_t phi2,
                                           Double_t theta3, Double_t phi3):
      TRotMatrix(name, title, theta1, phi1, theta2, phi2, theta3, phi3), TQtCoin3DNode(){}
   virtual ~TRotMatrixC() {}
};

class TVolumePositionC : public TVolumePosition, public TQtCoin3DNode {
public:
   TVolumePositionC(TVolume *node=0,Double_t x=0, Double_t y=0, Double_t z=0, TRotMatrix *matrix=0):
      TVolumePosition(node=0,x,y,z,matrix),TQtCoin3DNode(){}

   TVolumePositionC(TVolume *node,Double_t x, Double_t y, Double_t z, const Text_t *matrixname):
      TVolumePosition(node,x,y,z,matrixname), TQtCoin3DNode(){}

   TVolumePositionC(const TVolumePosition* oldPosition, const TVolumePosition* curPosition):
      TVolumePosition(oldPosition, curPosition), TQtCoin3DNode(){}

   TVolumePositionC(const TVolumePositionC&pos): TVolumePosition(pos), TQtCoin3DNode(pos){}
   TVolumePositionC(const TVolumePosition&pos): TVolumePosition(pos), TQtCoin3DNode(){}

   virtual ~TVolumePositionC(){};
};

#endif
