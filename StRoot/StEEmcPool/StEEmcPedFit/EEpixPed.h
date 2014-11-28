// \class  EEpixPed
// \author Jan Balewski
// \edited Justin Stevens

#ifndef EEpixPed_HH
#define EEpixPed_HH

#include <TObjArray.h>
class TFile;

class EEpixPed :public TObject{ 
  int c_x1,c_x2,c_minInt,c_xFit; // constrains for Ped search 
  float qa_minInt, qa_pedLow, qa_pedHigh, qa_pedSigMin, qa_pedSigMax;
  TFile *tFd;
  TObjArray *HList;

 public:
  int y1,y2,y;
  EEpixPed(TString hFile);
  void setLimits(int x1, int x2,int a, int f) 
    {c_x1=x1;c_x2=x2;c_minInt=a; c_xFit=f;
    }  // peak search [x1,x2], peak fit [max-xFit, max+xFit]
  void setQA( float minInt,  float pedLow,  float pedHigh,  float pedSigMin,  float pedSigMax) { 
    qa_minInt=minInt; qa_pedLow=pedLow; qa_pedHigh=pedHigh; qa_pedSigMin=pedSigMin; qa_pedSigMax=pedSigMax;}
    
  void findTowerHisto();
  void findMapmtHisto();
  void fitHisto(TString fPath="outPed/");
  void dropHisto(){  HList->RemoveAll();}
  void saveHisto(TString fname="outPed/spectra");
  void savePedTable( char *mode, TString fname="outPed/ped");
  ClassDef(EEpixPed,1) 
    
    };
     
#endif



