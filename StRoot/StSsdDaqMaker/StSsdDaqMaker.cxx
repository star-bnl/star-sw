// $Id: StSsdDaqMaker.cxx,v 1.22 2017/04/26 20:12:06 perev Exp $
//
// $log$
//
// Id: StSsdDaqMaker.cxx,v 1.5 2005/04/22 16:12:02 lmartin Exp $
//
// Log: StSsdDaqMaker.cxx,v $
// Revision 1.5  2005/04/22 16:12:02  lmartin
// bug in the ladder index fixed
//
// Revision 1.4  2005/04/21 14:59:47  lmartin
// useless print removed
//
// Revision 1.3  2005/04/21 14:55:17  lmartin
// bug in the offset correction fixed
//
// Revision 1.2  2005/04/21 09:50:28  lmartin
// Hardware offset corrected for specific ladders
//
// Revision 1.1  2005/04/15 15:11:24  lmartin
// StSsdDaqMaker
//

//*-- Authors : Lilian Martin
//*--         : Joerg Reinnarth
//*--         : Jonathan Bouchet
            
#include "StSsdDaqMaker.h"
#include "TDataSetIter.h"
#include "StMessMgr.h"
#include "StDAQMaker/StDAQReader.h"
#include "StDAQMaker/StSSDReader.h"
#include "ssdLadderMap.h"
#include "tables/St_spa_strip_Table.h"
#include "tables/St_ssdConfiguration_Table.h"
#include "tables/St_ssdPedStrip_Table.h"
#include "StSsdUtil/StSsdConfig.hh"
#include "TH1.h"
#include "TH2.h"
#include "TFile.h"
#include "TNtuple.h"
ClassImp(StSsdDaqMaker)

//_____________________________________________________________________________
/// StSsdDaqMaker constructor
/*!
  const char *name -  the name of this constructor
  The first comment lines after the opening bracket
  ({) of a member function are considered as a member function description 
  See <A HREF="http://root.cern.ch/root/Documentation.html"> ROOT HTML documentation </A>

 */
StSsdDaqMaker::StSsdDaqMaker(const char *name):StMaker(name){
  //
}


//_____________________________________________________________________________
/// This is StSsdDaqMaker destructor
/*!
  The first comment lines after the opening bracket
  ({) of a member function are considered as a member function description 
  
  The first comment lines after the opening bracket
  ({) of a member function are considered as a member function description 
  see: <A HREF="http://root.cern.ch/root/Documentation.html"> ROOT HTML documentation </A> 

 */
StSsdDaqMaker::~StSsdDaqMaker(){
  //
}


//_____________________________________________________________________________
/// Init - is a first method the top level StChain calls to initialize all its makers 
Int_t StSsdDaqMaker::Init(){
  // Now we try to get the configuration information (which ladders are active)
  // table using the StSsdUtil and StSsdDbMaker...


  LOG_INFO << "Read now Databases " << endm;
  //	Create SsdPedestal histograms
  if (IAttr(".histos")) {
    occupancy_wafer = new TH2S("occupancy_wafer","occupancy per wafer",40,0,40,20,0,20);
    occupancy_chip  = new TH2S("occupancy_chip","occupancy per chip",40,0,40,99,0,99);
    noise_chip      = new TH2S("noise_chip","mean noise per chip",40,0,40,99,0,99);
    noise_wafer     = new TH2S("noise_wafer","mean noise per wafer",40,0,40,20,0,20);
    noise_chip_P    = new TH2S("noise_chip_P","mean noise per chip for the P Side ",20,0,20,96,0,96);
    noise_chip_N    = new TH2S("noise_chip_N","mean noise per chip for the N Side",20,0,20,96,0,96);
    pedestal_chip   = new TH2S("pedestal_chip","pedestal per chip",40,0,40,99,0,99);
    ped_zero_ladP   = new TH2S("ped_zero_ladP","map of p-side wafers where strips have ped = 0",20,0,20,15,0,15);
    ped_zero_ladN   = new TH2S("ped_zero_ladN","map of n-side wafers where strips have ped = 0",20,0,20,15,0,15);
    ped_high_ladP   = new TH2S("ped_high_ladP","map of p-side wafers where strips have ped = 255",20,0,20,15,0,15);
    ped_high_ladN   = new TH2S("ped_high_ladN","map of n-side wafers where strips have ped = 255",20,0,20,15,0,15);
    noise_zero_ladP = new TH2S("noise_zero_ladP","map of p-side wafers where strips have noise = 0",20,0,20,15,0,15);
    noise_zero_ladN = new TH2S("noise_zero_ladN","map of n-side wafers where strips have noise = 0",20,0,20,15,0,15);
    noise_high_ladP = new TH2S("noise_high_ladP","map of p-side wafers where strips have noise = 255",20,0,20,15,0,15);
    noise_high_ladN = new TH2S("noise_high_ladN","map of n-side wafers where strips have noise = 255",20,0,20,15,0,15);
    occupancy       = new TH1F("deadStrips","number of dead strips",40,0,40);
    occupancy_wafer->SetXTitle("Ladder");
    occupancy_wafer->SetYTitle("Wafer");  
    occupancy_chip->SetXTitle("Ladder");
    occupancy_chip->SetYTitle("Chip");  
    noise_chip->SetXTitle("Ladder");
    noise_chip->SetYTitle("Chip");
    noise_chip_P->SetXTitle("Ladder");
    noise_chip_N->SetXTitle("Ladder");
    noise_chip_P->SetYTitle("Chip"); 
    noise_chip_N->SetYTitle("Chip");   
    noise_wafer->SetXTitle("Ladder");
    noise_wafer->SetYTitle("Wafer"); 
    pedestal_chip->SetXTitle("Ladder");
    pedestal_chip->SetYTitle("Chip");
    occupancy->SetXTitle("Ladder");
    occupancy->SetYTitle("#");
    ped_zero_ladP->SetXTitle("Ladder");
    ped_zero_ladP->SetYTitle("Wafer"); 
    ped_zero_ladN->SetXTitle("Ladder");
    ped_zero_ladN->SetYTitle("Wafer");
    ped_high_ladP->SetXTitle("Ladder");
    ped_high_ladP->SetYTitle("Wafer"); 
    ped_high_ladN->SetXTitle("Ladder");
    ped_high_ladN->SetYTitle("Wafer");
    noise_zero_ladP->SetXTitle("Ladder");
    noise_zero_ladP->SetYTitle("Wafer");
    noise_zero_ladN->SetXTitle("Ladder");
    noise_zero_ladN->SetYTitle("Wafer");
    noise_high_ladP->SetXTitle("Ladder");
    noise_high_ladP->SetYTitle("Wafer");
    noise_high_ladN->SetXTitle("Ladder");
    noise_high_ladN->SetYTitle("Wafer");
    mPedOut = 0;//default : we do not write the Tuple
    if(mPedOut)
      {
	DeclareNTuple();
      }    
  }
  LOG_INFO << "Init() - Done " << endm;
  
  return StMaker::Init();
}

//_____________________________________________________________________________
Int_t StSsdDaqMaker::InitRun(int runumber)
{
  LOG_INFO <<"InitRun(int runumber) - Read now Databases"<<endm;
  Int_t run = (runumber/1000000)-1;
  
  St_ssdConfiguration *configuration = (St_ssdConfiguration*) GetDataBase("Geometry/ssd/ssdConfiguration");
  if (!configuration){
    LOG_ERROR << "InitRun("<<runumber<<") - ERROR - ssdConfiguration==0"<<endm;
    return 0;
  }
  ssdConfiguration_st *config  = (ssdConfiguration_st*) configuration->GetTable() ;
  if (!config){
    LOG_ERROR <<"InitRun("<<runumber<<") - ERROR - config==0"<<endm;
    return 0;
  }
  
  mConfig = new StSsdConfig();
  
  int totLadderPresent = 0;
  
  for (int ladder = 1; ladder<=config->nMaxLadders;ladder++) 
    {
      LOG_INFO<< " on sector = " << config->ladderIsPresent[ladder-1];
      if (config->ladderIsPresent[ladder-1] != 0)
	totLadderPresent++;
      mConfig->setLadderIsActive(ladder,config->ladderIsPresent[ladder-1]);
    }
  PrintConfiguration(run,config);
  mConfig->setNumberOfLadders(totLadderPresent);
  mConfig->setNumberOfWafers(config->nMaxWafers/config->nMaxLadders);
  mConfig->setNumberOfHybrids(2);
  mConfig->setTotalNumberOfHybrids(2*16*totLadderPresent);
  mConfig->setTotalNumberOfLadders(config->nMaxLadders);
  mConfig->setNumberOfStrips(768);

  mConfig->setConfiguration();

      LOG_INFO << "_____________________________" << endm;
      LOG_INFO << "       Via  Datababase......." << endm;
      LOG_INFO << ".......numberOfSectors =     " << config->nMaxSectors << endm;
      LOG_INFO << ".......numberOfLadders =     " << totLadderPresent << endm;
      LOG_INFO << " .numberOfWafersPerLadder =  " << config->nMaxWafers/config->nMaxLadders << endm;
      LOG_INFO << "_____________________________" << endm;
      LOG_INFO << "      InitRun() - Done       "<<endm;
  return kStOk;

}

//_____________________________________________________________________________

void StSsdDaqMaker::DeclareNTuple(){
  pFile = new TFile("PedestalFile.root","RECREATE");
  string varlist = "side:ladder:wafer:strip:pedestal:noise:chip:id_wafer:tot_strip";
  pTuple     = new TNtuple("PedestalNTuple","Pedestal Ntuple",varlist.c_str());
  LOG_INFO << "StSsdDaqMaker::DeclareNtuple - Done - "<<endm;
}

//_____________________________________________________________________________
// Make - this method is called in loop for each event
// 
// The real data are saved in the spa_strip table 
// The pedestal data are saved in the ssdPedStrip table
// 
// 
// 
// 
Int_t StSsdDaqMaker::Make(){
  int strip_number,id_wafer,id_side,count,my_channel=-9999;
  int ladderCountN[20]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0} ;
  int ladderCountP[20]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0} ;
  int data,pedestal,noise,channel,newchannel,ladder; char EastWest;
  int maxChannel;
  int nStrip =0;
  int iLad = 0;
  int iWaf = 0;
  int chip = 0;
  // SSD parameters independant from its configuration (half or full barrel)
  // mConfig->getTotalNumberOfLadders()=20;
  // mConfig->getNumberOfStrips()=768;
  // mConfig->getNumberOfWafers()=16;
  for (Int_t i=0;i<9;i++){
    PedestalNTuple[i] = 0;
  }
  maxChannel=mConfig->getNumberOfStrips()*mConfig->getNumberOfWafers();
  TDataSet *daq = GetDataSet("StDAQReader");                 
  if (!daq) {
    LOG_WARN << "Make : StDAQReader Dataset not found - Skipping the event" << endm;
    return kStWarn;
  } 
  StDAQReader *daqReader = (StDAQReader*) (daq->GetObject()); 
  if (!daqReader) {
    LOG_WARN << "Make : StDAQReader Object not found - Skipping the event" << endm;
    return kStWarn;
  }
  if (!(daqReader->SSDPresent())) {
    LOG_WARN << "Make : The SSD was not in the data stream  - Skipping the event" << endm;
    return kStWarn;
  } 
  StSSDReader *stssdreader  = daqReader->getSSDReader();   
  if(!stssdreader)
  {
    LOG_WARN << "Make : SSD reader not found - Skipping the event" << endm;
    return kStWarn;
  }
  // creating the spa_strip and spa_ped_strip tables
  St_spa_strip *spa_strip = (St_spa_strip *) GetData("spa_strip");  
  if(!spa_strip)
  {
    spa_strip   = new St_spa_strip("spa_strip",100000);
    AddData(spa_strip);
  }
  St_ssdPedStrip *ssdPedStrip = (St_ssdPedStrip *) GetData("ssdPedStrip");
  if(!ssdPedStrip)
  {
    ssdPedStrip = new St_ssdPedStrip("ssdPedStrip",100000);
    AddData(ssdPedStrip);
  }
  spa_strip_st   out_strip;
  ssdPedStrip_st out_ped_strip;
  count=1;
  // looping on the sides ...      
static const char EW[4]={'E','W','p','n'};
  for (id_side=0;id_side<2;id_side++)
    {
      EastWest=EW[id_side];
      LOG_DEBUG <<" process "<<EW[id_side+2]<<"-side ladders "<< endm;
      for (ladder=0;ladder<mConfig->getTotalNumberOfLadders();ladder++) 
	{
	  if (mConfig->getLadderIsActive(ladder+1)>0)
	    {
	      if ((ladder+1)== 4 || (ladder+1)== 6 || (ladder+1)==10 || 
		  (ladder+1)==11 || (ladder+1)==13 || (ladder+1)==15 ||
		  (ladder+1)==17 ) 
		maxChannel=mConfig->getNumberOfStrips()*mConfig->getNumberOfWafers()-1;
	      else maxChannel=mConfig->getNumberOfStrips()*mConfig->getNumberOfWafers();
	      for (newchannel=0;newchannel<maxChannel;newchannel++)
		{
		  if ((ladder+1)== 4 || (ladder+1)== 6 || (ladder+1)==10 || 
		      (ladder+1)==11 || (ladder+1)==13 || (ladder+1)==15 ||
		      (ladder+1)==17 ) 
		    channel= newchannel+1;
		  else channel=newchannel;
		  if(stssdreader->getSsdData(ladder+1,EastWest,newchannel,data,pedestal,noise)==0) 
		    {
		      //We are looking at a physics run
		      // filling the out_strip structure...
		      //the strip id is coded as follow:
		      // id_strip=10000*(10*strip_number+id_side)+id_wafer
		      //strip number=1-mConfig->getNumberOfStrips()
		      //id_side=0 for p side, 1 for n side
		      if (data>0) {
			if (id_side==1) my_channel=maxChannel-1-channel;
			else            my_channel=channel;                  
			// the ssd mapping tables are inverted at the moment so we have to scan 
			// them to get the correct channel.
			if (id_side==1) {
			  for (int kk=0;kk<maxChannel;kk++) {
			    if (ssd_ladder_mapN[kk]==channel) my_channel=kk;
			  }
			}
			else {
			  for (int kk=0;kk<maxChannel;kk++) {
			    if (ssd_ladder_mapP[kk]==channel) my_channel=kk;
			  }
			}
                        assert(my_channel>=0);
			strip_number=my_channel-(my_channel/mConfig->getNumberOfStrips())*mConfig->getNumberOfStrips()+1;
			if (id_side==0)
			  id_wafer=7000+100*(mConfig->getNumberOfWafers()-(my_channel/mConfig->getNumberOfStrips()))+ladder+1;
			else
			  id_wafer=7000+100*((my_channel/mConfig->getNumberOfStrips())+1)+ladder+1;
			out_strip.id          = count;
			out_strip.adc_count   = data;
			out_strip.id_strip    = 10000*(10*strip_number+id_side)+id_wafer;
			out_strip.id_mchit[0]   = 0;
			out_strip.id_mchit[1]   = 0;
			out_strip.id_mchit[2]   = 0;
			out_strip.id_mchit[3]   = 0;
			out_strip.id_mchit[4]   = 0;
			spa_strip->AddAt(&out_strip);
			if (id_side ==0) ladderCountP[ladder]++;
			else             ladderCountN[ladder]++;
			count++;
		      } // end else for if data > 0
		    }
		  else // else for if(stssdreader->getSsdData(ladder,EastWest,channel,data,pedestal,noise)==0) 
		    {
		      //We are looking at a pedestal run
		      // filling the out_strip structure...
		      //the strip id is coded as follow:
		      // id_strip=10000*(10*strip_number+id_side)+id_wafer
		      //strip number=1-mConfig->getNumberOfStrips()
		      //id_side=0 for p side, 1 for n side
		      if (pedestal>=0) {
			//			  if (id_side==1) my_channel=maxChannel-1-channel;
			//			  else my_channel=channel;
			// the ssd mapping tables are inverted at the moment so we have to scan 
			// them to get the correct channel.
			if (id_side==1) {
			  for (int kk=0;kk<maxChannel;kk++) {
			    if (ssd_ladder_mapN[kk]==channel) my_channel=kk;
			  }
			  //			    my_channel=ssd_ladder_mapN[channel];
			}
			else {
			  for (int kk=0;kk<maxChannel;kk++) {
			    if (ssd_ladder_mapP[kk]==channel) my_channel=kk;
			  }
			  //			    my_channel=ssd_ladder_mapP[channel];
			}
			strip_number=my_channel-(my_channel/mConfig->getNumberOfStrips())*mConfig->getNumberOfStrips()+1;
			if (id_side==0)
			  id_wafer=7000+100*(mConfig->getNumberOfWafers()-(my_channel/mConfig->getNumberOfStrips()))+ladder+1;
			else
			  id_wafer=7000+100*((my_channel/mConfig->getNumberOfStrips())+1)+ladder+1;
			out_ped_strip.id        = count;
			out_ped_strip.id_strip  = 10000*(10*strip_number+id_side)+id_wafer;
			out_ped_strip.noise     = noise;
			out_ped_strip.pedestal   = pedestal;
			nStrip=(int)(out_ped_strip.id_strip/100000.);
			iWaf    = (int)((id_wafer - 7*1000)/100 - 1);
			iLad    = (int)(id_wafer - 7*1000 - (iWaf+1)*100 - 1);
			nStrip=nStrip-1; // to have the good number of chip [0;95]
			chip=(int)((nStrip+(768*(iWaf)))/128.);
			ssdPedStrip->AddAt(&out_ped_strip);
			if (id_side ==0) {
			  ladderCountP[ladder]++;
			  occupancy_wafer->Fill(2*iLad,iWaf,1);
			  occupancy_chip->Fill(2*iLad,chip,1);
			  noise_chip->Fill(2*iLad,chip,(out_ped_strip.noise/(16.)));
			  noise_wafer->Fill(2*iLad,iWaf,(out_ped_strip.noise/(16.))); 
			  noise_chip_P->Fill(iLad,chip,(out_ped_strip.noise/(16.)));
			  pedestal_chip->Fill(2*iLad,chip,out_ped_strip.pedestal);
			  if(pedestal==0)ped_zero_ladP->Fill(iLad,iWaf);
			  if(pedestal==255)ped_high_ladP->Fill(iLad,iWaf);
			  if(noise==0)noise_zero_ladP->Fill(iLad,iWaf);
			  if(noise==255)noise_high_ladP->Fill(iLad,iWaf);

			  //store the pedestal and noise if mPedOut is on
			  if(mPedOut){
			    PedestalNTuple[0]=0;
			    PedestalNTuple[1]=iLad; 
			    PedestalNTuple[2]=iWaf; 
			    PedestalNTuple[3]=nStrip+1;
			    PedestalNTuple[4]=out_ped_strip.pedestal; 
			    PedestalNTuple[5]=out_ped_strip.noise; 
			    PedestalNTuple[6]=(nStrip/128)+1;
			    PedestalNTuple[7]=id_wafer;
			    PedestalNTuple[8]=nStrip+(iWaf-1)*768;
			    pTuple->Fill(PedestalNTuple);
			  }
			}
			else {
			  ladderCountN[ladder]++;
			  occupancy_wafer->Fill((2*iLad)+1,iWaf,1);
			  occupancy_chip->Fill((2*iLad)+1,chip,1);
			  noise_chip->Fill((2*iLad)+1,chip,(out_ped_strip.noise/(16.)));
			  noise_wafer->Fill((2*iLad)+1,iWaf,(out_ped_strip.noise/(16.)));
			  noise_chip_N->Fill(iLad,chip,(out_ped_strip.noise/(16.)));
			  pedestal_chip->Fill((2*iLad)+1,chip,out_ped_strip.pedestal);
			  if(pedestal==0)ped_zero_ladN->Fill(iLad,iWaf);
			  if(pedestal==255)ped_high_ladN->Fill(iLad,iWaf);
			  if(noise==0)noise_zero_ladN->Fill(iLad,iWaf);
			  if(noise==255)noise_high_ladN->Fill(iLad,iWaf);
			  if(mPedOut){
			    PedestalNTuple[0]=1;
			    PedestalNTuple[1]=iLad; 
			    PedestalNTuple[2]=iWaf;
			    PedestalNTuple[3]=nStrip-1;
			    PedestalNTuple[4]=out_ped_strip.pedestal; 
			    PedestalNTuple[5]=out_ped_strip.noise;
			    PedestalNTuple[6]=(nStrip/128)+1;
			    PedestalNTuple[7]=id_wafer;
			    PedestalNTuple[8]=nStrip+(iWaf-1)*768;
			    pTuple->Fill(PedestalNTuple);
			  }
			}   
			count++;
			for (int i=0;i<20;i++)
			  {
			    occupancy->SetFillColor(2);
			    occupancy->Fill((2*i),(12288-ladderCountP[i]));
			    occupancy->Fill((2*i)+1,(12288-ladderCountN[i]));
			  }
			//LOG_INFO << "Make()/ssdPedStrip->NRows= "<<ssdPedStrip->GetNRows()<<endm; 
		      } // end if pedestal > 0
		    } // end if(stssdreader->getSsdData(ladder,EastWest,channel,data,pedestal,noise)==0)  
		} // end for (channel=0;channel<maxChannel;channel++)
	    } // end if (mConfig->getLadderIsActive(ladder)>0)
	} // end for (ladder=0;ladder<mConfig->mConfig->getTotalNumberOfLadders();ladder++) 
    }
  // end for (id_side=0;id_side<2;id_side++)
    
  LOG_DEBUG <<"Make()/Number of raw data in the SSD ";
  LOG_DEBUG << "Make()/Active Ladders: ";
  for (int i=0;i<mConfig->getTotalNumberOfLadders();i++){ 
    if (mConfig->getLadderIsActive(i+1)>0) {
      LOG_DEBUG.width(5);
      LOG_DEBUG <<i+1<<" ";
  } }
  LOG_DEBUG <<endm;
  LOG_DEBUG << "Make()/Counts (p-side): ";
  for (int i=0;i<mConfig->getTotalNumberOfLadders();i++){ 
    if (mConfig->getLadderIsActive(i+1)>0) {
      LOG_DEBUG.width(5);
      LOG_DEBUG <<ladderCountP[i]<<" ";
  } }
  LOG_DEBUG << endm;
  LOG_DEBUG << "Make()/Counts (n-side): "; 
  for (int i=0;i<mConfig->getTotalNumberOfLadders();i++){
    if (mConfig->getLadderIsActive(i+1)>0) {
      LOG_DEBUG.width(5);
      LOG_DEBUG <<ladderCountN[i]<<" ";
  } }
  LOG_DEBUG<<endm;
  if((spa_strip->GetNRows()==0)&&(ssdPedStrip && ssdPedStrip->GetNRows()!=0)){
    LOG_INFO << "Make()/ Read Pedestal & Noise"<< endm; 
    LOG_INFO << "Make()/ssdPedStrip->NRows= "<<ssdPedStrip->GetNRows()<<endm; 
  }
  else 
    if((! ssdPedStrip || ssdPedStrip->GetNRows()==0) && (spa_strip->GetNRows()!=0)){
      LOG_INFO << "Make()/ Read Signal from Physics Run"<< endm; 
      LOG_INFO << "Make()/  spa_strip->NRows= "<<spa_strip->GetNRows()<<endm;
    }
  return kStOK;
}

//_____________________________________________________________________________
Int_t StSsdDaqMaker::Finish() 
{
  LOG_INFO << Form("Finish()") << endm;
  if(mPedOut){
    LOG_INFO << "Write Pedestal tuple"<< endm;
    pFile->Write();
    pFile->Close();
  }
  return kStOK;
}
//_____________________________________________________________________________
void StSsdDaqMaker::PrintConfiguration(Int_t runumber,ssdConfiguration_st *config)
{
  switch(runumber){
  case 4 : {
    LOG_INFO <<"Configuration of ladders for run IV" <<endm;
    break;
  }
  case 5 : {
    LOG_INFO <<"Configuration of ladders for run V" << endm;
    break;
  }
  case 6 : {
    LOG_INFO <<"Configuration of ladders  for run VI"<< endm;
    break;
  }
  case 7 : {
    LOG_INFO <<"Configuration of ladders  for run VII" << endm;
    break;
  }
  }
  Int_t i =0;
  Int_t totladderPresent =0;
  LOG_INFO << "PrintLadderSummary:ladder id                 :";
  for (i=1;i<=config->nMaxLadders;i++){ 
    LOG_INFO.width(3);
    LOG_INFO << i;
  }
  LOG_INFO <<endm;
  LOG_INFO << "PrintLadderSummary:Active Ladders on sectors: ";
  for (i=1;i<=config->nMaxLadders;i++){ 
    LOG_INFO.width(3);
    LOG_INFO <<mConfig->getLadderIsActive(i);
    if(mConfig->getLadderIsActive(i)>0)totladderPresent++;

  }
  LOG_INFO << endm;
  LOG_INFO << "totLadderActive = "<<totladderPresent<<endm; 
}
