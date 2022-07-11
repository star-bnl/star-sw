#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
class daqReader;
#include <TH1F.h>
#include <TH2F.h>
#include <TRandom.h>
#include <string>
#include <math.h>

class gmtBuilder : public JevpBuilder {
public:
  int run;

  gmtBuilder(JevpServer *parent=NULL); 
  ~gmtBuilder();
  

  void initialize(int argc, char *argv[]);
  void startrun(daqReader *rdr);
  void stoprun(daqReader *rdr);
  void event(daqReader *rdr);

  int getAPV(int i_arm, int i_port,int i_apv){

    if(i_arm ==0 &&  i_port==0 && i_apv ==0 ) {return 0;}
    else if(i_arm ==0 &&  i_port==0 && i_apv ==1 ) {return 1;}
    else if(i_arm ==0 &&  i_port==0 && i_apv ==2 ) {return 2;}
    else if(i_arm ==0 &&  i_port==0 && i_apv ==3 ) {return 3;}
    else if(i_arm ==0 &&  i_port==1 && i_apv ==0 ) {return 4;}
    else if(i_arm ==0 &&  i_port==1 && i_apv ==1 ) {return 5;}
    else if(i_arm ==0 &&  i_port==1 && i_apv ==2 ) {return 6;}
    else if(i_arm ==0 &&  i_port==1 && i_apv ==3 ) {return 7;}
    else if(i_arm ==1 &&  i_port==0 && i_apv ==0 ) {return 8;}
    else if(i_arm ==1 &&  i_port==0 && i_apv ==1 ) {return 9;}
    else if(i_arm ==1 &&  i_port==0 && i_apv ==2 ) {return 10;}
    else if(i_arm ==1 &&  i_port==0 && i_apv ==3 ) {return 11;}
    else if(i_arm ==1 &&  i_port==1 && i_apv ==0 ) {return 12;}
    else if(i_arm ==1 &&  i_port==1 && i_apv ==1 ) {return 13;}
    else if(i_arm ==1 &&  i_port==1 && i_apv ==2 ) {return 14;}
    else if(i_arm ==1 &&  i_port==1 && i_apv ==3 ) {return 15;}
  }

  int getLayer(int iarm, int iport, int iapv){
    if      ( iarm == 0 && iport==0 && ( iapv >= 0 && iapv <= 1 ) )   { return 0; }
    else if ( iarm == 0 && iport==0 && ( iapv >= 2 && iapv <= 3 ) )   { return 1; }
    else if ( iarm == 0 && iport==1 && ( iapv >= 0 && iapv <= 1 ) )   { return 2; }
    else if ( iarm == 0 && iport==1 && ( iapv >= 2 && iapv <= 3 ) )   { return 3; }
    else if ( iarm == 1 && iport==0 && ( iapv >= 0 && iapv <= 1 ) )   { return 4; }
    else if ( iarm == 1 && iport==0 && ( iapv >= 2 && iapv <= 3 ) )   { return 5; }
    else if ( iarm == 1 && iport==1 && ( iapv >= 0 && iapv <= 1 ) )   { return 6; }
    else if ( iarm == 1 && iport==1 && ( iapv >= 2 && iapv <= 3 ) )   { return 7; }
    else    { 
      printf("getLayer() wrong layer#  iarm=%d iport=%d IAPV=%d\n",iarm,iport,iapv); 
      return -1; 
    }
  }
  
bool usedAPV(int i_rdo,int i_arm,int i_port,int i_apv){
  if(i_rdo==1 && i_arm==0 && i_port==0 && i_apv>=0 && i_apv<=3) return true;
  if(i_rdo==1 && i_arm==0 && i_port==1 && i_apv>=0 && i_apv<=3) return true;
  if(i_rdo==1 && i_arm==1 && i_port==0 && i_apv>=0 && i_apv<=3) return true;
  if(i_rdo==1 && i_arm==1 && i_port==1 && i_apv>=0 && i_apv<=3) return true;
  return false;
  }
 static void main(int argc, char *argv[]);

 private:


  int evtCount;
  //*** Histogram Declarations...
  //*** Use the union to be able to treat in bulk
  //*** As well as by name...
  union {

    TH2 *hPedArray[1];
    struct {

      TH2* pedestalsAPV[16];   // for 16 APPVs
   
    };
  } hPedContents;

   union {
    
    TH1 *hSignalArray[1];
    struct {
      
      TH1* Signals[8];  //For 8 Chambers (0-7)
      
    };
  } hSignalContents;
  
  union {
    
    TH1 *hTimebinArray[1];
    struct {
      
      TH1* Timebins[8];  //For 8 Chambers (0-7)
      };
  } hTimebinContents;
  
  union {
    
    TH1 *hSumArray[1];
    struct {
      TH1* h1SumAllsignals; 
                 
    };
  } hSumContents;
  union {
  TH2 *hSigtb[1];
    struct {
      TH2* h2SignalTimebins; 
    };
  } hSigtbContents;

  static const int ADCcut = 760;  //Can be changed 
  //========= Fixed Values ===================
  static const int numLayers = 8;
  static const int numRDOs = 1;
  static const int numARMs = 2;
  static const int numPORTs = 2;
  static const int numAPVs = 16;
  static const int numChannels = 128;
  static const int numTimebins = 15;
  //========================================
  int nped;
  int nsig;
  int ntb;
  int nsum;

  int channel, timebin;
  double ch_seq;
  double adc;

  JevpPlot** plots;
  daq_dta *dd;
   
 
  double  SignalPedCorrected[numARMs][numPORTs][numAPVs][numChannels][numTimebins];
  double  SumSignalPedCorrected[numTimebins][numLayers];

  double sumSignal_AllChambers[numTimebins];
  double sumSignal_AllTimebins[numLayers];

  JLatex* errorMsg;

  ClassDef(gmtBuilder, 1);
};
