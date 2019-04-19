#ifndef STVGRAPPA_H
#define STVGRAPPA_H

#include <vector>
#include "TSystem.h"
#include "TNamed.h"

class TCanvas;
class TGraph;
class StvHit;
class StvHits;
class StvTrack;

enum StvGrappa_e {kNode,kHit,kHIT,kThit,kTHIT,kHelx,kPont,kNObj,kNPad=4,kNVal=4};
class StvGrappa: public TNamed 
{
public:
// kNode: 	Node green line
// kHit: 	Unused Not TRUE hit Blue point
// kHIT: 	Used hit Not TRUE,Blue  Star
// kThit	Unused BUT TRUE hit Red point
// kTHIT	Used AND TRUE hit Red Star
// kHelx:	Yellow line
// kPont:	Seed hit, Black Mult
StvGrappa(const char* name="");
~StvGrappa(){ Clear();}
void Add(double x,double y,double z,int iObj);
void Show();
void Clear(const char* opt=0);
 int NObj() const;
void SetActive(int akt=1) {mActive = akt;}
void Show(const StvTrack *tk);
void Zhow(const StvTrack *tk);
void Show(const StvHits *tk, int objType = kPont);
void Zhow(const StvHits *tk, int objType = kPont);
private:
void MakeCanvas();
void MyShow(int iPad,int iObj,int iX,int iY );
void MySize(int iPad,int iX,int iY);
void MyClear();
void MySort();
protected:
double Dist2(const StvTrack *tk,const float *hit) const;
double Dist2(const StvHits  *tk,const float *hit) const;
void MakeLims(const StvTrack *tk,double xMiMax[2][3]) const;
void MakeLims(const StvHits  *tk,double xMiMax[2][3]) const;
void TestHit(const StvHit *hit) const;

protected:
char mBeg[1];
int  mMarkerColor;
int  mMarkerStyle;
int  mMarkerSize;
float mMiMax[kNVal][2];
const char *mOpt;
TCanvas *mCanvas;
int mNTotal;
int mTkTruth;
int mState; 	//0=Last Call Clear,1=last call Zhow, 2=last call Show
char mEnd[1];
int mActive;
std::vector<float> mPts[kNObj][kNVal];	//[iObj][x,y,z,rxy]
std::vector<TObject*> mKeep;		//
ClassDef(StvGrappa,0)
};
#endif
