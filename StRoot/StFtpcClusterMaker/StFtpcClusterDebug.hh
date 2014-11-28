// StFtpcClusterDebug

#ifndef STAR_StFtpcClusterDebug
#define STAR_StFtpcClusterDebug

#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include "TFile.h"
#include "TString.h"
#include "TH2.h"
#include "TTree.h"
#include "TBranch.h"
#include "TObjArray.h"
#include "TDirectory.h"

#include "StDAQMaker/StFTPCReader.h"
#include "StFtpcClustersStructures.hh"

class StFtpcClusterDebug
{

 private:

  TString histodatei;
  TFile *histofile;
  int hardsecold, hardrowold,hardsecold2, hardrowold2;
  Int_t run, nevent, neventold;
  TH2F *clusterhisto,*clusterhisto2;
  TH1F *vertex_east,*vertex_west,*vertex_both;
  bool fileopen;
  bool dir,dir2;
  TBranch *bRun;
  TBranch *bhit, *bevent, *bcluster;
  TBranch *bthit, *btcluster, *btevent;
  TBranch *btrevent, *btrack,*btrvertex;
  TBranch *bclusterraw;

  TDirectory *histdir, *histdir2, *vertexdir;
  TDirectory *topdir;

  void backup();

 public:
  
  TTree *drtree;
  TTree *dtree;
  TTree *dttree;
  TTree *dtrtree;
  TTree *dtreeraw;

  int drawclhisto;
  int drawvertexhisto;

  StFtpcClusterDebug();
  StFtpcClusterDebug(int grun, int gevent);
  void drawhisto(int hardsec, int hardrow, int iPad, TPCSequence HSequence);
  void fillraw(int hardsec, int hardrow, int iPad, TPCSequence HSequence);
  void drawgainhisto(int hardsec, int hardrow,int iPad,float gainfac,TPCSequence HSequence);
  void drawvertex(TH1F *veast,TH1F *vwest, TH1F *v);
  void fillRun(Int_t run, Int_t date, Int_t time, Float_t micropertimebin, Float_t normalizedNowPressure, Float_t standardPressure, Float_t baseTemperature, Float_t gasTemperatureWest, Float_t gasTemperatureEast, Float_t deltapW, Float_t deltapE);
  void fillclustertree(TPeak *Peak,TClusterUC *cl,Float_t charge,Int_t hsec, Int_t hrow, Float_t raderror, Float_t phierror,Int_t flag,float getpressure, int getnumpeaks);
  void fillclustertree(TPeak Peak,TClusterUC *cl,Float_t charge,Int_t hsec, Int_t hrow, Float_t raderror, Float_t phierror,Int_t flag,float getpressure, int getnumpeaks);
  void clusteranalyse();
  void filltracktree(TObjArray *foundtracks,Double_t vertexpos[3]);
  void fillgeanttree();
  ~StFtpcClusterDebug();
};

#endif
