#ifndef _JLINE_H_
#define _JLINE_H_

#include <TROOT.h>
#include <TObject.h>
#include <TLine.h>

class JLine : public TLine {
  int ndcY;
  int ndcX;
 
  Double_t jx[2];
  Double_t jy[2];

 public:
  void SetNDC_x(int ndc);
  void SetNDC_y(int ndc);

  void SetX1(Double_t x);
  void SetX2(Double_t x);
  void SetY1(Double_t y);
  void SetY2(Double_t y);
  void SetXY(Double_t x1, Double_t y1, Double_t x2, Double_t y2);

  JLine();
  JLine(Double_t x1, Double_t y1, Double_t x2, Double_t y2);
  void Draw(Option_t * option = "");

  ClassDef(JLine, 1);
};

#endif
