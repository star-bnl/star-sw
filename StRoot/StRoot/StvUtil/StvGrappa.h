#ifndef STVGRAPPA_H
#define STVGRAPPA_H

#include <vector>
#include "TSystem.h"
#include "TNamed.h"

class TCanvas;
class TGraph;
class StvTrack;

class StvGrappa: public TNamed 
{
public:
enum StvGrappa_e {kNode,kHit,kHIT,kHelx,kPont,kNObj,kNPad=4,kNVal=4};

StvGrappa(const char* name="");
~StvGrappa(){ Clear();}
void Add(double x,double y,double z,int iObj);
void Show();
void Clear(const char* opt=0);
 int NObj() const;
void SetActive(int akt=1) {mActive = akt;}
void Show(const StvTrack *tk);
private:
void MakeCanvas();
void MyShow(int iPad,int iObj,int iX,int iY );
void MySize(int iPad,int iX,int iY);
void MyClear();
void MySort();


protected:
char mBeg[1];
int  mMarkerColor;
int  mMarkerStyle;
int  mMarkerSize;
float mMiMax[kNVal][2];
const char *mOpt;
TCanvas *mCanvas;
char mEnd[1];
int mActive;
std::vector<float> mPts[kNObj][kNVal];	//[iObj][x,y,z,rxy]
std::vector<TObject*> mKeep;		//
ClassDef(StvGrappa,0)
};
#endif
