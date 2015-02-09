//This generates a projected channel map of TOF.

#include <iostream>
#include <fstream>
#include <string>
#include <iomanip>
using namespace std;


//update headers


void tofStatus(){
  const Int_t NTRAY = 120;
  const Int_t NCHANNELTRAY = 192;
  const Int_t NCHANNELTOTAL = 23040;
  const Int_t NCHANNELMODULE = 6;
  Short_t tofmap[NCHANNELTOTAL]; //Make 120 trays of 192 channels. They can either be usable(1) or not(0).


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
  StDbConfigNode* configNode = dbManager->initConfig("Calibrations_tof");//Not really a calibration.

//-- add table to the container with descriptor given by Database
  StDbTable* tofStatus = configNode->addDbTable("tofStatus");//

//--fill structures and store times
  tofStatus_st *Status = new tofStatus_st[1];


//----------------------------------------
// Commit log:
  // Note: start time is part of the configuration file (the following line is only to set a default)
  TString ZStoreTime = "2011-12-20 00:00:02"; //Default start time, user can enter more.


	//Initialize all the channels
	for(int channel = 0; channel<NCHANNELTOTAL; channel++){
	   tofmap[channel]=1; //set all channels as good
	}//end of nchannels loop

	TString Date,Time;
	Int_t nEntries=0;
	Int_t inputTray[100]={0},inputModule[100]={0},inputChannel[100]={0};
/*
  Now for user input
  Expect the format" Date, Time, number of entries, tray, module, channel, tray, module, channel...
  Tray 1-120, Module 1-32, Channel 1-6.  Set tray,module or channel to 0 for a whole range to be cleared.
  channel=0, sets that module bad, module=0 sets that tray bad, and tray=0 sets all of tof bad.
  Entries=0, enables all trays.
  Eg: tray=1,module=2,channel=0 all of module 2 set bad. 
  Sample format for the input file:

2012-01-01 00:00:01 8
95 0 0
102 0 0
33 23 6
76 28 1
77 1 6
39 13 0
39 14 1
39 14 2
*/
 

	//May want to add multiple date entries to make this easier.

        ifstream inData;
        inData.open("data/run13/tofStatus/badtofentry.txt");
	inData >> Date >> Time >> nEntries;

	cout<<"Date: "<<Date<<", Entries: "<<nEntries<<endl;
//	Int_t inputTray[nEntries],inputModule[nEntries],inputChannel[nEntries];


	for(int entry=0;entry<nEntries;entry++){
	inData >> inputTray[entry] >> inputModule[entry] >> inputChannel[entry];
	cout<<"tray,module,channel: "<<inputTray[entry]<<", "<<inputModule[entry]<<", "<<inputChannel[entry]<<endl;
	}//entries
       inData.close();


	Date.Append(" ");
	ZStoreTime = Date.Append(Time);
	cout <<"Date+time and ZStoreTime : "<<Date<<", "<<ZStoreTime<<endl;


	if(nEntries==0) cout<< "Enabling All Trays--Entries=0" <<endl;


	for(int entry=0;entry<nEntries;entry++){
	
   	   //Mapping whole trays, modules, or channels.
	   if(nEntries==0) continue; //Set only default values.

	   if(inputTray[entry]==0){//tray==0 Set all of tof disabled.
		cout <<"Caution: Disabling all trays"<<endl;
		for(int channel=0;channel<NCHANNELTOTAL;channel++) tofmap[channel]=0;
	   }

	   if(inputTray[entry]!=0&&inputModule[entry]==0){//module==0 Set the whole tray disabled.
	      //Tray
	      for(int channel =0;channel<NCHANNELTRAY;channel++){
	         tofmap[(inputTray[entry]-1)*NCHANNELTRAY+channel]=0;
	      }//
	   }

	   if(inputTray[entry]!=0&&inputModule[entry]!=0&&inputChannel[entry]==0){//channel==0 Set that module disabled

		//
	     for(int channel =0;channel<NCHANNELMODULE;channel++){
		tofmap[(inputTray[entry]-1)*NCHANNELTRAY+channel+(inputModule[entry]-1)*NCHANNELMODULE]=0;
	     }//module	
	   }

	   //Channel	
 	   if((inputTray[entry]!=0)&&(inputModule[entry]!=0)&&(inputChannel[entry]!=0)){
		tofmap[(inputTray[entry]-1)*NCHANNELTRAY+(inputModule[entry]-1)*NCHANNELMODULE+inputChannel[entry]-1]=0;
	   }
	}//entries



	// Move values to Status table.
	for(int tray=0;tray<NTRAY;tray++){
	  for(int channel=0;channel<NCHANNELTRAY;channel++){
	    if (0)  cout <<"Tray, channel, map: "<<tray<<", "<<channel<<", "<< tofmap[tray*NCHANNELTRAY+channel]<<endl;
	    Status[0].status[channel+tray*NCHANNELTRAY]=tofmap[tray*NCHANNELTRAY+channel];
	  }//channel
	}
	

	//Upload to Database.
	tofStatus->SetTable((char*)Status,1);
	//time
	dbManager->setStoreTime(ZStoreTime.Data());
	dbManager->storeDbTable(tofStatus);
	cout<<"Done."<<endl;

}//end of tofStatus

