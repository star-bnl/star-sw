
#include "Cut.h"

#include <iostream>

ClassImp(Cut)

int parseNumber(int a, int b);

//
// initiatlize
//
NchCentrality   Cut::mZdcCtbCent[2] = {kTotal,kFive};
Int_t  Cut::mFlowCent[2] = {-1,-1};
Int_t  Cut::mHMinusCent[2] = {-1,-1};
Int_t  Cut::mNchCent[2] = {-1,1};
 
// set all to false as default.  
bool Cut::mDoFlowCent=false;
bool Cut::mDoZdcCtbCent=false;
bool Cut::mDoHMinusCent=false;
bool Cut::mDoNchCent=false;
bool Cut::mDoNchCentKludge=false;

bool Cut::mDoSpectraCent=false; // default is not to use the spectra centrality

Float_t Cut::mVertexZ[2] = {-200,200};
Float_t Cut::mVertexZSkip = 0;

Float_t Cut::mEta[2] = {-.5,.5};
Int_t   Cut::mFitPts[2] = {0,45};
Float_t Cut::mSDcaGl[2] = {-3,3};
Float_t Cut::mDcaPr[2] = {0,3};
Float_t Cut::mDcaXYGl[2] = {-3,3};
Int_t   Cut::mMcPts[2] = {10,99};
Float_t Cut::mEtaTight[2] = { -.2, .2 };
Float_t Cut::mHitAvoid=0;
Int_t Cut::mCut = 0;

char  Cut::mHalf = 0;
char  Cut::mHitHalf = 0;
char  Cut::mGeomHalf = 0;




void
Cut::SetHalf(char half)
{
  switch(half){
  case 'e' : mHalf = 'e'; mVertexZ[1] = 0; break;
  case 'w' : mHalf = 'w'; mVertexZ[0] = 0; break;
  case 's' : mHalf = 's'; break; // enforce same side
  default : cerr << "Wrong half " << half << endl; exit(1);
  }
}

void
Cut::SetHitHalf(char half)
{
  switch(half){
  case 'e' : mHitHalf = 'e'; break;
  case 'w' : mHitHalf = 'w'; break;
  case 's' : mHitHalf = 's'; break;
  default : cerr << "Wrong hit half " << half << endl; exit(1); 
  }
}

void
Cut::SetGeomHalf(char half)
{
  switch(half){
  case 'e' : mGeomHalf = 'e'; break;
  case 'w' : mGeomHalf = 'w'; break;
  case 's' : mGeomHalf = 's'; break;
  default : cerr << "Wrong geom half " << half << endl; exit(1); 
  }
}

/*

  Cuts defined as : 
  a. centrality
      9=minbias
      8=central
      7=peripheral
      6=central (version 2 (uses 5% hminus cut for other))
      5=peripheral kludge (use 50-80 for corrections)
      4=10-20%
      3=20-30%
      2=30-40%
      1=40-60%
  b. which centrality definition for spectra
      9=flow
      8=zdc-ctb
      7=hminus
      6=nch
  c. which centrality definition for corrections/other
      9=flow
      8=zdc-ctb
      7=hminus
      6=nch
  d. vertex
      9=-200,200
      8=-75,75
      7=-95,95
      6=-30,30
      5=0,30
  e. half?
      9=none
      8=hit avoid CM
      7=hit avoid 10 cm around CM
  f. fit pts
      9=20-45
      8=25
      7=10
      6=35 // really tight
      5=24
      4=23
      3=30
  g. dca 3d
      9=1
      8=1.2
      7=3
  h. eta
      9=.5
      8=.7  
      7= >0, <.5
*/
/*
e.g. a=15796, b=5 : return = 1
     a=5924, b=3 : return = 9, etc. 
*/
int parseNumber(int a, int b)
{
  int div1=(int)pow(10,b);
  int div2=(int)pow(10,b-1);

  int val=a%div1;
  return (val==a) ? int(a/div2) : int(val/div2);
}

void
Cut::SetCut(Int_t type)
{
  if(type<10000000){
    cout << "type " << type << " is too short" << endl;exit(1);
  }
 
  mCut=type;
 
  int centType=parseNumber(type,8);
  int centDefSpectra=parseNumber(type,7);
  int centDefOther=parseNumber(type,6);
  int vertex=parseNumber(type,5);
  int half=parseNumber(type,4);
  int fitPts=parseNumber(type,3);
  int dca=parseNumber(type,2);
  int eta=parseNumber(type,1);
  
  cout << "Set cut?" << endl
       << "\tcentType=" << centType << endl
       << "\tcentDefSpectra=" << centDefSpectra << endl
       << "\tcentDefOther=" << centDefOther << endl
       << "\tvertex=" << vertex << endl
       << "\thalf=" << half << endl
       << "\tfitPts=" << fitPts << endl
       << "\tdca=" << dca << endl
       << "\teta=" << eta << endl;
  

  // list the peripheral definitions here for convenience
  NchCentrality peripheralMin = kEighty,
    peripheralMax = kSeventy;
  NchCentrality kludgeMin = kEighty,
    kludgeMax = kSixty;
  //NchCentrality kludge2Min = kEighty,
  //  kludge2Max = kSixty;


  switch(centType){
  case 9: // minbias
    if(mDoSpectraCent){
      if(centDefSpectra==9){ // flow
	mFlowCent[0]=0; mFlowCent[1]=9; mDoFlowCent=true;
      }
      else if(centDefSpectra==8){ // zdc-ctb
	mZdcCtbCent[0]=kTotal; mZdcCtbCent[1]=kFive; mDoZdcCtbCent=true;
      }
      else if(centDefSpectra==7){ // hminus
	mHMinusCent[0]=kTotal; mHMinusCent[1]=kFive; mDoHMinusCent=true;
      }
      else if(centDefSpectra==6){ // nch
	mNchCent[0]=kTotal; mNchCent[1]=kFive; mDoNchCent=true;
      }
      else if(centDefSpectra==5){ // nch
	mNchCent[0]=kTotal; mNchCent[1]=kFive; mDoNchCentKludge=true;
      }
      else{
	cerr << "unknown centrality definition for spectra " << centDefSpectra<<endl; exit(1);
      }
    }
    else{ // not spectra centrality
      if(centDefOther==9){ // flow
	mFlowCent[0]=0; mFlowCent[1]=9; mDoFlowCent=true;
      }
      else if(centDefOther==8){ // zdc-ctb
	mZdcCtbCent[0]=kTotal; mZdcCtbCent[1]=kFive; mDoZdcCtbCent=true;
      }
      else if(centDefOther==7){ // hminus
	mHMinusCent[0]=kTotal; mHMinusCent[1]=kFive; mDoHMinusCent=true;
      }
      else if(centDefOther==6){ // nch
	mNchCent[0]=kTotal; mNchCent[1]=kFive; mDoNchCent=true;
      }
      else if(centDefOther==5){ // nch
	mNchCent[0]=kTotal; mNchCent[1]=kFive; mDoNchCentKludge=true;
      }
      else{
	cerr << "unknown centrality definition for spectra " << centDefSpectra<<endl; exit(1);
      }
    }
    break;
  case 8: // central
    if(mDoSpectraCent){
      if(centDefSpectra==9){ // flow
	mFlowCent[0]=8; mFlowCent[1]=9; mDoFlowCent=true;
      }
      else if(centDefSpectra==8){ // zdc-ctb
	mZdcCtbCent[0]=kFive; mZdcCtbCent[1]=kFive; mDoZdcCtbCent=true;
      }
      else if(centDefSpectra==7){ // hminus
	mHMinusCent[0]=kFive; mHMinusCent[1]=kFive; mDoHMinusCent=true;
      }
      else if(centDefSpectra==6){ // nch
	mNchCent[0]=kFive; mNchCent[1]=kFive; mDoNchCent=true;
      }
      
      else{
	cerr << "unknown centrality definition for spectra " << centDefSpectra<<endl; exit(1);
      }
    }
    else{    
      if(centDefOther==9){ // flow
	mFlowCent[0]=8; mFlowCent[1]=9; mDoFlowCent=true;
      }
      else if(centDefOther==8){ // zdc-ctb
	mZdcCtbCent[0]=kTen; mZdcCtbCent[1]=kFive; mDoZdcCtbCent=true;
      }
      else if(centDefOther==7){ // hminus
	mHMinusCent[0]=kTen; mHMinusCent[1]=kFive; mDoHMinusCent=true;
      }
      else if(centDefOther==6){ // nch
	mNchCent[0]=kTen; mNchCent[1]=kFive; mDoNchCent=true;
      }
      else{
	cerr << "unknown centrality definition for spectra " << centDefSpectra<<endl; exit(1);
      }
    }
    break;
  case 7: // peripheral
    if(mDoSpectraCent){
      if(centDefSpectra==9){ // flow
	mFlowCent[0]=1; mFlowCent[1]=3; mDoFlowCent=true;
      }
      else if(centDefSpectra==8){ // zdc-ctb
	mZdcCtbCent[0]=peripheralMin; 
	mZdcCtbCent[1]=peripheralMax; mDoZdcCtbCent=true;
      }
      else if(centDefSpectra==7){ // hminus
	mHMinusCent[0]=peripheralMin; 
	mHMinusCent[1]=peripheralMax; mDoHMinusCent=true;
      }
      else if(centDefSpectra==6){ // nch
	mNchCent[0]=peripheralMin; 
	mNchCent[1]=peripheralMax; mDoNchCent=true;
      }
      else if(centDefSpectra==5){ // nch
	mNchCent[0]=peripheralMin; 
	mNchCent[1]=peripheralMax; mDoNchCentKludge=true;
      }
      else{
	cerr << "unknown centrality definition for spectra " << centDefSpectra<<endl; exit(1);
      }
    }
    else{
      if(centDefOther==9){ // flow
	mFlowCent[0]=1; mFlowCent[1]=3; mDoFlowCent=true;
      }
      else if(centDefOther==8){ // zdc-ctb
	mZdcCtbCent[0]=peripheralMin; 
	mZdcCtbCent[1]=peripheralMax; mDoZdcCtbCent=true;
      }
      else if(centDefOther==7){ // hminus
	mHMinusCent[0]=peripheralMin; 
	mHMinusCent[1]=peripheralMax; mDoHMinusCent=true;
      }
      else if(centDefOther==6){ // nch
	mNchCent[0]=peripheralMin; 
	mNchCent[1]=peripheralMax; mDoNchCent=true;
      }
      else if(centDefOther==5){ // nch
	mNchCent[0]=peripheralMin; 
	mNchCent[1]=peripheralMax; mDoNchCentKludge=true;
      }
      else{
	cerr << "Unknown centrality definition for spectra: " << centDefSpectra<<endl; exit(1);
      }
    }
    break;
    case 6: // central
    if(mDoSpectraCent){
      if(centDefSpectra==9){ // flow
	mFlowCent[0]=8; mFlowCent[1]=9; mDoFlowCent=true;
      }
      else if(centDefSpectra==8){ // zdc-ctb
	mZdcCtbCent[0]=kFive; mZdcCtbCent[1]=kFive; mDoZdcCtbCent=true;
      }
      else if(centDefSpectra==7){ // hminus
	mHMinusCent[0]=kFive; mHMinusCent[1]=kFive; mDoHMinusCent=true;
      }
      else if(centDefSpectra==6){ // nch
	mNchCent[0]=kFive; mNchCent[1]=kFive; mDoNchCent=true;
      }
      else if(centDefSpectra==5){ // nch
	mNchCent[0]=kFive; mNchCent[1]=kFive; mDoNchCentKludge=true;
      }
      else{
	cerr << "unknown centrality definition for spectra " << centDefSpectra<<endl; exit(1);
      }
    }
    else{    
      if(centDefOther==9){ // flow
	mFlowCent[0]=8; mFlowCent[1]=9; mDoFlowCent=true;
      }
      else if(centDefOther==8){ // zdc-ctb
	mZdcCtbCent[0]=kFive; mZdcCtbCent[1]=kFive; mDoZdcCtbCent=true;
      }
      else if(centDefOther==7){ // hminus
	mHMinusCent[0]=kFive; mHMinusCent[1]=kFive; mDoHMinusCent=true;
      }
      else if(centDefOther==6){ // nch
	mNchCent[0]=kFive; mNchCent[1]=kFive; mDoNchCent=true;
      }
      else if(centDefOther==5){ // nch
	mNchCent[0]=kFive; mNchCent[1]=kFive; mDoNchCentKludge=true;
      }
      else{
	cerr << "unknown centrality definition for spectra " << centDefSpectra<<endl; exit(1);
      }
    }
    break;
    
    case 5: // peripheral kludge
    if(mDoSpectraCent){
      if(centDefSpectra==9){ // flow
	mFlowCent[0]=1; mFlowCent[1]=3; mDoFlowCent=true;
      }
      else if(centDefSpectra==8){ // zdc-ctb
	mZdcCtbCent[0]=peripheralMin; 
	mZdcCtbCent[1]=peripheralMax; mDoZdcCtbCent=true;
      }
      else if(centDefSpectra==7){ // hminus
	mHMinusCent[0]=peripheralMin; 
	mHMinusCent[1]=peripheralMax; mDoHMinusCent=true;
      }
      else if(centDefSpectra==6){ // nch
	mNchCent[0]=peripheralMin; 
	mNchCent[1]=peripheralMax; mDoNchCent=true;
      }
      else if(centDefSpectra==5){ // nch
	mNchCent[0]=peripheralMin; 
	mNchCent[1]=peripheralMax; mDoNchCentKludge=true;
      }
      else{
	cerr << "unknown centrality definition for spectra " << centDefSpectra<<endl; exit(1);
      }
    }
    else{
      if(centDefOther==9){ // flow
	mFlowCent[0]=1; mFlowCent[1]=3; mDoFlowCent=true;
      }
      else if(centDefOther==8){ // zdc-ctb
	mZdcCtbCent[0]=kludgeMin; 
	mZdcCtbCent[1]=kludgeMax; mDoZdcCtbCent=true;
      }
      else if(centDefOther==7){ // hminus
	mHMinusCent[0]=kludgeMin; 
	mHMinusCent[1]=kludgeMax; mDoHMinusCent=true;
      }
      else if(centDefOther==6){ // nch
	mNchCent[0]=kludgeMin; 
	mNchCent[1]=kludgeMax; mDoNchCent=true;
      }
      else if(centDefOther==5){ // nch
	mNchCent[0]=kludgeMin; 
	mNchCent[1]=kludgeMax; mDoNchCentKludge=true;
      }
      else{
	cerr << "Unknown centrality definition for spectra: " << centDefSpectra<<endl; exit(1);
      }
    }
    break;

  case 4: // 10-20%
    if(mDoSpectraCent){
      if(centDefSpectra==9){ // flow
	mFlowCent[0]=6; mFlowCent[1]=7; mDoFlowCent=true;
      }
      else if(centDefSpectra==8){ // zdc-ctb
	mZdcCtbCent[0]=kTwenty; 
	mZdcCtbCent[1]=kTwenty; mDoZdcCtbCent=true;
      }
      else if(centDefSpectra==7){ // hminus
	mHMinusCent[0]=kTwenty; 
	mHMinusCent[1]=kTwenty; mDoHMinusCent=true;
      }
      else if(centDefSpectra==6){ // nch
	mNchCent[0]=kTwenty; 
	mNchCent[1]=kTwenty; mDoNchCent=true;
      }
      else if(centDefSpectra==5){ // nch
	mNchCent[0]=kTwenty; 
	mNchCent[1]=kTwenty; mDoNchCentKludge=true;
      }
      else{
	cerr << "unknown centrality definition for spectra " << centDefSpectra<<endl; exit(1);
      }
    }
    else{
      if(centDefOther==9){ // flow
	mFlowCent[0]=6; mFlowCent[1]=7; mDoFlowCent=true;
      }
      else if(centDefOther==8){ // zdc-ctb
	mZdcCtbCent[0]=kTwenty; 
	mZdcCtbCent[1]=kTwenty; mDoZdcCtbCent=true;
      }
      else if(centDefOther==7){ // hminus
	mHMinusCent[0]=kTwenty; 
	mHMinusCent[1]=kTwenty; mDoHMinusCent=true;
      }
      else if(centDefOther==6){ // nch
	mNchCent[0]=kTwenty; 
	mNchCent[1]=kTwenty; mDoNchCent=true;
      }
      else if(centDefOther==5){ // nch
	mNchCent[0]=kTwenty; 
	mNchCent[1]=kTwenty; mDoNchCentKludge=true;
      }
      else{
	cerr << "Unknown centrality definition for spectra: " << centDefSpectra<<endl; exit(1);
      }
    }
    break;
    case 3: // 20-30
    if(mDoSpectraCent){
      if(centDefSpectra==9){ // flow
	mFlowCent[0]=4; mFlowCent[1]=5; mDoFlowCent=true;
      }
      else if(centDefSpectra==8){ // zdc-ctb
	mZdcCtbCent[0]=kThirty; 
	mZdcCtbCent[1]=kThirty; mDoZdcCtbCent=true;
      }
      else if(centDefSpectra==7){ // hminus
	mHMinusCent[0]=kThirty; 
	mHMinusCent[1]=kThirty; mDoHMinusCent=true;
      }
      else if(centDefSpectra==6){ // nch
	mNchCent[0]=kThirty; 
	mNchCent[1]=kThirty; mDoNchCent=true;
      }
      else if(centDefSpectra==5){ // nch
	mNchCent[0]=kThirty; 
	mNchCent[1]=kThirty; mDoNchCentKludge=true;
      }
      else{
	cerr << "unknown centrality definition for spectra " << centDefSpectra<<endl; exit(1);
      }
    }
    else{
      if(centDefOther==9){ // flow
	mFlowCent[0]=1; mFlowCent[1]=3; mDoFlowCent=true;
      }
      else if(centDefOther==8){ // zdc-ctb
	mZdcCtbCent[0]=kThirty; 
	mZdcCtbCent[1]=kThirty; mDoZdcCtbCent=true;
      }
      else if(centDefOther==7){ // hminus
	mHMinusCent[0]=kThirty; 
	mHMinusCent[1]=kThirty; mDoHMinusCent=true;
      }
      else if(centDefOther==6){ // nch
	mNchCent[0]=kThirty; 
	mNchCent[1]=kThirty; mDoNchCent=true;
      }
      else if(centDefOther==5){ // nch
	mNchCent[0]=kThirty; 
	mNchCent[1]=kThirty; mDoNchCentKludge=true;
      }
      else{
	cerr << "Unknown centrality definition for spectra: " << centDefSpectra<<endl; exit(1);
      }
    }
    break;

    case 2: // 30-40
    if(mDoSpectraCent){
      if(centDefSpectra==9){ // flow
	mFlowCent[0]=3; mFlowCent[1]=3; mDoFlowCent=true;
      }
      else if(centDefSpectra==8){ // zdc-ctb
	mZdcCtbCent[0]=kForty; 
	mZdcCtbCent[1]=kForty; mDoZdcCtbCent=true;
      }
      else if(centDefSpectra==7){ // hminus
	mHMinusCent[0]=kForty; 
	mHMinusCent[1]=kForty; mDoHMinusCent=true;
      }
      else if(centDefSpectra==6){ // nch
	mNchCent[0]=kForty; 
	mNchCent[1]=kForty; mDoNchCent=true;
      }
      else if(centDefSpectra==5){ // nch
	mNchCent[0]=kForty; 
	mNchCent[1]=kForty; mDoNchCentKludge=true;
      }
      else{
	cerr << "unknown centrality definition for spectra " << centDefSpectra<<endl; exit(1);
      }
    }
    else{
      if(centDefOther==9){ // flow
	mFlowCent[0]=1; mFlowCent[1]=3; mDoFlowCent=true;
      }
      else if(centDefOther==8){ // zdc-ctb
	mZdcCtbCent[0]=kForty; 
	mZdcCtbCent[1]=kForty; mDoZdcCtbCent=true;
      }
      else if(centDefOther==7){ // hminus
	mHMinusCent[0]=kForty; 
	mHMinusCent[1]=kForty; mDoHMinusCent=true;
      }
      else if(centDefOther==6){ // nch
	mNchCent[0]=kForty; 
	mNchCent[1]=kForty; mDoNchCent=true;
      }
      else if(centDefOther==5){ // nch
	mNchCent[0]=kForty; 
	mNchCent[1]=kForty; mDoNchCentKludge=true;
      }
      else{
	cerr << "Unknown centrality definition for spectra: " << centDefSpectra<<endl; exit(1);
      }
    }
    break;

    case 1: // 40-60
    if(mDoSpectraCent){
      if(centDefSpectra==9){ // flow
	mFlowCent[0]=2; mFlowCent[1]=2; mDoFlowCent=true;
      }
      else if(centDefSpectra==8){ // zdc-ctb
	mZdcCtbCent[0]=kSixty; 
	mZdcCtbCent[1]=kFifty; mDoZdcCtbCent=true;
      }
      else if(centDefSpectra==7){ // hminus
	mHMinusCent[0]=kSixty; 
	mHMinusCent[1]=kFifty; mDoHMinusCent=true;
      }
      else if(centDefSpectra==6){ // nch
	mNchCent[0]=kSixty; 
	mNchCent[1]=kFifty; mDoNchCent=true;
      }
      else if(centDefSpectra==5){ // nch
	mNchCent[0]=kSixty; 
	mNchCent[1]=kFifty; mDoNchCentKludge=true;
      }
      else{
	cerr << "unknown centrality definition for spectra " << centDefSpectra<<endl; exit(1);
      }
    }
    else{
      if(centDefOther==9){ // flow
	mFlowCent[0]=1; mFlowCent[1]=3; mDoFlowCent=true;
      }
      else if(centDefOther==8){ // zdc-ctb
	mZdcCtbCent[0]=kSixty; 
	mZdcCtbCent[1]=kFifty; mDoZdcCtbCent=true;
      }
      else if(centDefOther==7){ // hminus
	mHMinusCent[0]=kSixty; 
	mHMinusCent[1]=kFifty; mDoHMinusCent=true;
      }
      else if(centDefOther==6){ // nch
	mNchCent[0]=kSixty; 
	mNchCent[1]=kFifty; mDoNchCent=true;
      }
      else if(centDefOther==5){ // nch
	mNchCent[0]=kSixty; 
	mNchCent[1]=kFifty; mDoNchCentKludge=true;
      }
      else{
	cerr << "Unknown centrality definition for spectra: " << centDefSpectra<<endl; exit(1);
      }
    }
    break;

  default:
    cerr << "Unknown centrality type: " << centType << endl; exit(1);
  }

  switch(vertex){
  case 9:
    mVertexZ[0]=-200; mVertexZ[1]=200; break;
  case 8:
    mVertexZ[0]=-75; mVertexZ[1]=75; break;
  case 7:
    mVertexZ[0]=-95; mVertexZ[1]=95; break;
  case 6:
    mVertexZ[0]=-30; mVertexZ[1]=30; break;
  case 5:
    mVertexZ[0]=0; mVertexZ[1]=30; break;
  default:
    cerr << "Unknown vertex type: " << vertex << endl; exit(1);
  }
  
  switch(half){
  case 9:
    break;
  case 8:
    SetHitHalf('s'); break;
  case 7:
    SetHitAvoid(10); break;
  default:
    cerr << "Unknown half type : " << half << endl; exit(1);
  }
    
  switch(fitPts){
  case 9:
    mFitPts[0]=20; mFitPts[1]=99; break;
  case 8:
    mFitPts[0]=25; mFitPts[1]=99; break;
  case 7:
    mFitPts[0]=10; mFitPts[1]=99; break;
  case 6:
    mFitPts[0]=35; mFitPts[1]=99; break;  
  case 5:
    mFitPts[0]=24; mFitPts[1]=99; break;
  case 4:
    mFitPts[0]=23; mFitPts[1]=99; break;  
  case 3:
    mFitPts[0]=30; mFitPts[1]=99; break;
  default:
    cerr << "Unknown fit pts type : " << fitPts << endl; exit(1);
  }
  
  switch(dca){
  case 9:
    mSDcaGl[0]=-1; mSDcaGl[1]=1; break;
  case 8:
    mSDcaGl[0]=-1.2; mSDcaGl[1]=1.2; break;
  case 7:
    mSDcaGl[0]=-3; mSDcaGl[1]=3; break;
    
  default:
    cerr << "Unknown dca type : " << half << endl; exit(1);
  }
  
  switch(eta){
  case 9:
    mEta[0]=-.5; mEta[1]=.5; break;
  case 8:
    mEta[0]=-.7; mEta[1]=.7; break;
  case 7:
    mEta[0]=0; mEta[1]=0.5; break;
  default:
    cerr << "Unknown dca type : " << half << endl; exit(1);
  }


}


void
Cut::ShowCuts()
{

  cout << "******************************************************" << endl;
  cout << "Cut::ShowCuts()" << endl;
  cout << "Cut=" << mCut << endl;
  cout << "event cuts:" << endl;
  
  if(mDoSpectraCent)
    cout << "\tWill use spectra centrality definition" << endl;
  else
    cout << "\tWill use 'other' centrality definition" << endl;

  if(mDoFlowCent)
    cout << "\tflow cent : " << mFlowCent[0] << " -- " << mFlowCent[1] << endl;
  else if(mDoZdcCtbCent)
    cout << "\tzdc-ctb cent  : " << (int) mZdcCtbCent[0] << " -- " 
	 << (int) mZdcCtbCent[1] << endl;
  else if(mDoHMinusCent)
    cout << "\th minus cent : " << (int) mHMinusCent[0] << " -- "
	 << (int) mHMinusCent[1] << endl;
  else if(mDoNchCent)
    cout << "\th nCh cent : " << (int) mNchCent[0] << " -- "
	 << (int) mNchCent[1] << endl;
  else if(mDoNchCentKludge){
    cout << "\tDoing nCh KLUDGE" << endl;
    cout << "\th nCh cent : " << (int) mNchCent[0] << " -- "
	 << (int) mNchCent[1] << endl;
  }  
  else{
    cout << "\tUnknown centrality definition. Goodbye" << endl; exit(1);
  }
  if(mDoNchCentKludge)
    cout << "\tDoing nCh KLUDGE" << endl;

  cout << "\tvertex z  : " << mVertexZ[0] << " -- " << mVertexZ[1] << endl;
  if(mVertexZSkip){
    cout << "\tvtx z skip: "<< mVertexZSkip << endl;
  }
  if(mHalf){
    cout << "\twill process half " << "'" <<mHalf <<"'" << endl;
  }
  if(mGeomHalf){
    cout << "\twill process geom half " << "'" << mGeomHalf << "'" << endl;
  }
  
  cout << "track cuts:" << endl
       << "\teta       : " << mEta[0] << " -- " << mEta[1] << endl
       << "\tfit pts   : " << mFitPts[0] << " -- " << mFitPts[1] << endl
       << "\tsdca      : " << mSDcaGl[0] << " -- " << mSDcaGl[1] << endl
       << "\tmc pts    : " << mMcPts[0] << " -- " << mMcPts[1] << endl
       << "\teta tight : " << mEtaTight[0] << "-- " << mEtaTight[1] << endl
       << endl;
  if(mHitAvoid){
    cout << "\ttracks with hits outside of " << mHitAvoid << " cm" <<endl;
  }
  if(mHitHalf){
    cout << "\twill process tracks with hits on half " << mHitHalf << endl;
  }
  cout << "******************************************************" << endl;

  

}
