// macro to upload tofr5 INL tables to database
//
// based on
//  http://www.star.bnl.gov/STAR/comp/db/StoreDbTable.cc.html
//
// Jing Liu, 02/18/2005 
//

 
// #include "StDbLib/StDbManager.hh"
// #include "StDbLib/StDbConfigNode.hh"
// #include "StDbLib/StDbTable.h"
// #include "StDbLib/StDbDefs.hh"

#include <iostream>
#include <fstream>
#include <string>
#include <iomanip>
using namespace std;


void storetofTrgWindow()
//int main(int argc, char *argv[])

{

// year8
//  const Int_t NTRAY = 5;
// year9
//  const Int_t NTRAY = 94;
// year10
  const Int_t NTRAY = 120;
  const Int_t NVPDTRAY = 2;
  const Int_t NMAX = 120;
  const Int_t NMTD = 2; //there is only one MTD, but it has ID=124

  //-- load dBase and Table definition libraries
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("StUtilities");
  gSystem->Load("St_Tables.so");

  gSystem->Load("StDbLib.so");
  gSystem->Load("libStDb_Tables.so");

  //-- get the singleton manager
  StDbManager* dbManager = StDbManager::Instance();

  //-- connect to the db & get an empty container
  StDbConfigNode* configNode = dbManager->initConfig("Calibrations_tof");

  //----------------------------------------
//  TString ZStoreTime = "2007-11-01 00:00:01";
//  TString ZStoreTime = "2009-02-01 00:00:00";  // y9 1st version very loose
//  TString ZStoreTime = "2009-02-01 00:00:01";  // y9 2st version  
//  TString ZStoreTime = "2009-03-26 00:00:00";  // y9 500GeV day 85-92
//  TString ZStoreTime = "2009-04-23 21:00:00";  // 10113066-10146091
//  TString ZStoreTime = "2009-05-27 00:00:00";  // 10147124-10149036
//  TString ZStoreTime = "2009-05-29 14:00:00";  // 10149077-10150023
//  TString ZStoreTime = "2009-05-30 15:30:00";  // 10150024-10152010
//  TString ZStoreTime = "2009-06-03 16:00:00";  // 10154045-...

  // year10
//  TString ZStoreTime = "2009-11-01 00:00:00"; // run 10 preliminary
  TString ZStoreTime = "2009-11-01 00:01:00"; // updated version, includes MTD
 TString ZStoreTime = "2009-11-01 00:01:01"; // updated version, (2nd attempt)includes MTD


  //-- add table to the container with descriptor given by Database
  StDbTable* tofTrgWindow = configNode->addDbTable("tofTrgWindow");

  //-- fill structures & store times
  tofTrgWindow_st *trg = new tofTrgWindow_st[NMAX+NVPDTRAY+NMTD];

// year 10
unsigned short cutlow[124]={
2828,2825,2826,2826,2832,2831,2835,2831,2836,2832,2832,2829,2833,2830,2833,
2831,2824,2826,2825,2824,2827,2836,2833,2830,2829,2831,2838,2833,2837,2838,
2836,2838,2850,2841,2847,2845,2845,2846,2843,2838,2837,2836,2835,2835,2829,
2824,2830,2829,2828,2827,2806,2807,2807,2814,2815,2818,2813,2821,2826,2828,
2891,2901,2891,2894,2891,2897,2894,2900,2899,2899,2898,2904,2905,2904,2898,
2904,2907,2912,2916,2916,2908,2914,2908,2914,2903,2902,2907,2901,2901,2898,
2895,2894,2897,2896,2896,2906,2903,2908,2907,2914,2911,2698,2913,2915,2915,
2916,2914,2913,2913,2912,2914,2909,2905,2908,2902,2898,2902,2895,2899,2899,
2857,2921,0,2900};
unsigned short cuthi[124]={
2903,2900,2901,2901,2907,2906,2910,2906,2911,2907,2907,2904,2908,2905,2908,
2906,2899,2901,2900,2899,2902,2911,2908,2905,2904,2906,2913,2908,2912,2913,
2911,2913,2925,2916,2922,2920,2920,2921,2918,2913,2912,2911,2910,2910,2904,
2899,2905,2904,2903,2902,2881,2882,2882,2889,2890,2893,2888,2896,2901,2903,
2966,2976,2966,2969,2966,2972,2969,2975,2974,2974,2973,2979,2980,2979,2973,
2979,2982,2987,2991,2991,2983,2989,2983,2989,2978,2977,2982,2976,2976,2973,
2970,2969,2972,2971,2971,2981,2978,2983,2982,2989,2986,2773,2988,2990,2990,
2991,2989,2988,2988,2987,2989,2984,2980,2983,2977,2973,2977,2970,2974,2974,
2932,2996,0,2975};

// year10
  for(int i=0;i<NMAX+NVPDTRAY+NMTD;i++) {
    trg[i].trgWindow_Min = cutlow[i];
    trg[i].trgWindow_Max = cuthi[i];
    cout << " tray = " << i+1 << " min = " << trg[i].trgWindow_Min << " max = " << trg[i].trgWindow_Max << endl;
  }

  //- store data in table
  tofTrgWindow->SetTable((char*)trg, NMAX+NVPDTRAY+NMTD);
  //- set store time
  dbManager->setStoreTime(ZStoreTime.Data());
  //- store table in dBase
  cout<<" here "<<endl;
  dbManager->storeDbTable(tofTrgWindow);
  cout<<"uploaded"<<endl;
      //    return 0;
}

