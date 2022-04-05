
#include "TMath.h"
#include "StvGrappa.h"
#include "TCanvas.h"
#include "TGraph.h"

#include "Stv/StvHit.h"
#include "Stv/StvNode.h"
#include "Stv/StvTrack.h"

ClassImp(StvGrappa)
enum StvGrappa2_e {kX,kY,kZ,kR};

//______________________________________________________________________________
StvGrappa::StvGrappa(const char* name) 
{
static int nCall=0; nCall++;
  if (name && *name) { SetName(name);}
  else { TString ts("Grappa_"); ts+=nCall; SetName(ts.Data());}
  memset(mBeg,0,mEnd-mBeg+1);
  mActive = 1;
}
//______________________________________________________________________________
void StvGrappa::Add(double x,double y,double z,int iObj)
{
  if (!mActive) return;
  float r = sqrt(x*x+y*y);
  mPts[iObj][kX].push_back(x);
  mPts[iObj][kY].push_back(y);
  mPts[iObj][kZ].push_back(z);
  mPts[iObj][kR].push_back(r);
}
//______________________________________________________________________________
void StvGrappa::Clear(const char *)
{
  if (!mActive) return;
  delete mCanvas;mCanvas=0;
  for (int i=0;i<(int)mKeep.size();i++) { delete mKeep[i];}
  mKeep.clear();
  MyClear();
}

//______________________________________________________________________________
void StvGrappa::MyClear()
{
  for (int i=0;i<kNObj;i++) {
    for (int j=0;j<kNVal;j++) {
      mPts[i][j].clear();
  } } 
}
//______________________________________________________________________________
int StvGrappa::NObj() const
{
  int n = 0;
  for (int i=0;i<kNObj;i++) {
    for (int j=0;j<kNVal;j++) {
      n+= mPts[i][j].size();
  } } 
 return n;

}
//______________________________________________________________________________
void StvGrappa::MakeCanvas()
{
  if (!mCanvas) mCanvas = new TCanvas("C1","",600,800);
    mCanvas->Divide(1,kNVal);
}
//______________________________________________________________________________
void StvGrappa::MySize (int iPad, int iX,int iY)
{
   if (iPad==0) {
     for (int i=0;i<kNVal;i++) { mMiMax[i][0]=1e11; mMiMax[i][1]=-1e11;}
     for (int iObj=0;iObj<kNObj;iObj++) {
       int nSize = mPts[iObj][iX].size();
       for (int i=0;i<nSize;i++) {
	 for (int iVal=0;iVal<kNVal;iVal++) {
           float val = mPts[iObj][iVal][i];
           if (mMiMax[iVal][0]>val) mMiMax[iVal][0]=val;
           if (mMiMax[iVal][1]<val) mMiMax[iVal][1]=val;
   } } } }

   TGraph *gra = new TGraph(2,mMiMax[iX],mMiMax[iY]);
   mCanvas->cd(iPad+1);
   gra->Draw("AP");
   mKeep.push_back(gra);
}
//______________________________________________________________________________
void StvGrappa::Show()
{
static const int iXY[kNPad][2] = {{0,1},{2,0},{2,1},{2,3}};
  if (!NObj()) return;
  if (!mActive) return;
  MySort();
  MakeCanvas();
  
  for (int iPad=0;iPad<kNPad;iPad++) {
    int iX = iXY[iPad][0],iY = iXY[iPad][1];
    MySize(iPad,  iX,iY);
//		Nodes
//  mOpt = "PL";
    mOpt = "PC";
    mMarkerColor = kGreen;	//
    mMarkerStyle = kFullDotMedium;
    mMarkerSize = 1;
    MyShow(iPad,kNode,iX,iY);
//		Unused hits
    mOpt = "P";
    mMarkerColor = kBlue;	//
    mMarkerStyle = kCircle; 
    mMarkerSize = 1;
    MyShow(iPad,kHit,iX,iY);
//		Used hits

    mOpt = "P";
    mMarkerColor = kRed;	//Red
    mMarkerStyle = kStar; mMarkerSize = 1;
    MyShow(iPad,kHIT,iX,iY);
//		Helix nodes
    mOpt = "PL";
    mMarkerColor = kYellow;	//
    mMarkerStyle = kFullDotSmall;
    mMarkerSize = 1;
    MyShow(iPad,kHelx,iX,iY);
//		Seed hits
    mOpt = "P";
    mMarkerColor = kBlack;	//
    mMarkerStyle = kMultiply; 
    mMarkerSize = 1;
    MyShow(iPad,kPont,iX,iY);
  }
  MyClear();
  mCanvas->Modified();
  mCanvas->Update();
  while(!gSystem->ProcessEvents()){}; 
  MyClear();
}

//______________________________________________________________________________
void StvGrappa::MyShow(int iPad,int iObj, int iX,int iY)
{

  int n = mPts[iObj][iX].size();
  if (!n) return;
  auto *myGra = new TGraph(n,&mPts[iObj][iX][0],&mPts[iObj][iY][0]);

  myGra->SetMarkerStyle(mMarkerStyle);
  myGra->SetMarkerSize (mMarkerSize );
  myGra->SetMarkerColor(mMarkerColor);
  mCanvas->cd(iPad+1);
  myGra->Draw(mOpt);
  mKeep.push_back(myGra);
}  
//______________________________________________________________________________
void StvGrappa::MySort()
{
std::vector<int> Idx;
std::vector<float> Buf;
  for (int iObj=0;iObj<kNObj;iObj++) {
    int nSize = mPts[iObj][2].size();
    if (!nSize) continue;
    Idx.resize(nSize);
    Buf.resize(nSize);
    TMath::Sort(nSize,&mPts[iObj][2][0],&Idx[0],kTRUE);
    for (int iX=0;iX<4;iX++) {
      auto &arr = mPts[iObj][iX];
      arr.swap(Buf);
      for (int i=0;i<nSize;i++) {arr[i]=Buf[Idx[i]];}
  } }
}

//______________________________________________________________________________
void StvGrappa::Show(const StvTrack *tk)
{
  for (StvNodeConstIter it=tk->begin();it!=tk->end();++it) {
    const StvNode *node = (*it);
    const StvHit *hit = node->GetHit();
    const double *Xd  = node->GetFP().pos();
    Add(Xd[0],Xd[1],Xd[2],kNode);
    if (!hit) continue;
    const float *Xf  = hit->x();
    Add(Xf[0],Xf[1],Xf[2],kHit);
  }
  Show();
}
