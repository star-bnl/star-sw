// \class  BbcVertex
// \author Jan Balewski
#ifndef BbcVertex_h
#define BbcVertex_h
/******************************************************
 * $Id: BbcVertex.h,v 1.2 2004/12/04 05:07:38 balewski Exp $
 ******************************************************
 * Descripion:
 *  finds pi0 based on EEMC tower response
 ******************************************************/

class TObjArray;
class TH1F ;
class TH2F ;

class  BbcHex;

class BbcVertex {

 protected:
  enum {mxHex=16, mxEW=2};
  int  nInpEve;
  int unixTime, unixTime0;

  //..vertex info
  float onlTdiff, oflTdiff;
  float zTpc;
        
  BbcHex *hex[mxEW][mxHex];
  int findTime(BbcHex **);
  void export2NN(BbcHex **);

  //time walk calib
  TH2F  *hC[mxEW][mxHex];
  void calibWalk( BbcHex *x, BbcHex *y, float dz, TH2F **h);

  float cm2tdcCh;
  
  TH1F *hA[32];
  void doVertex();

  void clear();
  TObjArray  *HList; /// output histo access point

 public:
  
  BbcVertex();
  virtual ~BbcVertex();
  void print();
  void finish();
  void init( );
  void initRun(int runID);// must be called after DB timestamp is known
  void readCalib(char *fname);
  void setTdcCalib(float x) {  cm2tdcCh=x;}
 
  ClassDef(BbcVertex,1) 
};
#endif

 


/*****************************************************************
 * $Log: BbcVertex.h,v $
 * Revision 1.2  2004/12/04 05:07:38  balewski
 * export to NN
 *
 * Revision 1.1  2004/08/31 03:44:13  balewski
 * first
 *
 * Revision 1.5  2004/08/26 04:39:40  balewski
 *
 ********************************************************************/

