// $Id: StSsdDaqMaker.cxx,v 1.3 2005/04/21 14:55:17 lmartin Exp $
//
// $Log: StSsdDaqMaker.cxx,v $
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
#include "StChain.h"
#include "St_DataSetIter.h"
#include "StMessMgr.h"
#include "StDAQMaker/StDAQReader.h"
#include "StDAQMaker/StSSDReader.h"
#include "ssdLadderMap.h"
#include "tables/St_spa_strip_Table.h"
#include "tables/St_ssdConfiguration_Table.h"
#include "tables/St_ssdPedStrip_Table.h"
#include "StSsdDbMaker/StSsdDbMaker.h"
#include "StSsdDbMaker/St_SsdDb_Reader.hh"
#include "StSsdUtil/StSsdConfig.hh"


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


  gMessMgr->Info() << " StSsdDaqMaker::Init() - Read now Databases " << endm;
  
  if (!gStSsdDbMaker)
    gMessMgr->Error("StSsdDaqMaker::Init() - ERROR - gStSsdDbMaker==0");
  else
  {
    St_SsdDb_Reader *pSsdDb_Reader = gStSsdDbMaker->get_SsdDb_Reader();

    if (!pSsdDb_Reader)
      gMessMgr->Error("StSsdDaqMaker::Init() - ERROR - pSsdDb_Reader==0");
    else
    {
      ssdConfiguration_st*  config = pSsdDb_Reader->getSsdConfiguration() ;

      if (!config)
        gMessMgr->Error("StSsdDaqMaker::Init() - ERROR - ssdConfiguration==0");
      else
      {
        mConfig = pSsdDb_Reader->getConfiguration(config);

        if (mConfig)
          gMessMgr->Info() <<"StSsdDaqMaker::Init() - SSD Configuration loaded..." << endm;
        else
          gMessMgr->Error("StiSsdDetectorBuilder::loadDb() - ERROR - _config==0");
      }
    } 
  }

   gMessMgr->Info() << " StSsdDaqMaker::Init() - Done " << endm;
   return StMaker::Init();
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

  int strip_number,id_wafer,id_side,count,my_channel;
  int ladderCountN[20]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0} ;
  int ladderCountP[20]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0} ;
  int data,pedestal,noise,channel,newchannel,ladder; char EastWest;
  int maxChannel;

  // SSD parameters independant from its configuration (half or full barrel)
  // mConfig->getTotalNumberOfLadders()=20;
  // mConfig->getNumberOfStrips()=768;
  // mConfig->getNumberOfWafers()=16;
  maxChannel=mConfig->getNumberOfStrips()*mConfig->getNumberOfWafers();

  cout<<" here we go... maxChannel="<<maxChannel<<endl;
  
  St_DataSet *daq = GetDataSet("StDAQReader");                 
  if (!daq) {
    gMessMgr->Warning() << "StSsdDaqMaker:Make : StDAQReader Dataset not found - Skipping the event" << endm;
    return kStWarn;
  }
  
  StDAQReader *daqReader = (StDAQReader*) (daq->GetObject()); 
  if (!daqReader) {
    gMessMgr->Warning() << "StSsdDaqMaker:Make : StDAQReader Object not found - Skipping the event" << endm;
    return kStWarn;
  }
  
  if (!(daqReader->SSDPresent())) {
    gMessMgr->Warning() << "StSsdDaqMaker:Make : The SSD was not in the data stream  - Skipping the event" << endm;
    return kStWarn;
  }

  StSSDReader *stssdreader  = daqReader->getSSDReader();   
  if(!stssdreader)
  {
    gMessMgr->Warning() << "StSsdDaqMaker:Make : SSD reader not found - Skipping the event" << endm;
    return kStWarn;
  }
  
  // creating the spa_strip and spa_ped_strip tables
  St_spa_strip *spa_strip = (St_spa_strip *) m_DataSet->Find("spa_strip");  
  if(!spa_strip)
  {
    spa_strip   = new St_spa_strip("spa_strip",100000);
    m_DataSet->Add(spa_strip);
  }

  St_ssdPedStrip *ssdPedStrip = (St_ssdPedStrip *) m_DataSet->Find("ssdPedStrip");
  if(!ssdPedStrip)
  {
    ssdPedStrip = new St_ssdPedStrip("ssdPedStrip",100000);
    m_DataSet->Add(ssdPedStrip);
  }


  spa_strip_st   out_strip;
  ssdPedStrip_st out_ped_strip;

  count=1;
  // looping on the sides ...      
  for (id_side=0;id_side<2;id_side++)
    {
      if (id_side == 0) EastWest='E'; // side = 0 = East = P-side
      if (id_side == 1) EastWest='W'; // side = 1 = West = N-side
      for (ladder=0;ladder<mConfig->getTotalNumberOfLadders();ladder++) 
	{
	  if (mConfig->getLadderIsActive(ladder)>0)
	    {
	      if (ladder== 4 || ladder== 6 || ladder==10 || 
		  ladder==11 || ladder==13 || ladder==15 ||
		  ladder==17 ) 
		maxChannel=mConfig->getNumberOfStrips()*mConfig->getNumberOfWafers()-1;
	      else maxChannel=mConfig->getNumberOfStrips()*mConfig->getNumberOfWafers();
	      for (newchannel=0;newchannel<maxChannel;newchannel++)
		{
		  if (ladder== 4 || ladder== 6 || ladder==10 || 
		      ladder==11 || ladder==13 || ladder==15 ||
		      ladder==17 ) 
		    channel= newchannel+1;
		  else channel=newchannel;
		  if(stssdreader->getSsdData(ladder,EastWest,newchannel,data,pedestal,noise)==0) 
		    {
		      //We are looking at a physics run
		      // filling the out_strip structure...
		      //the strip id is coded as follow:
		      // id_strip=10000*(10*strip_number+id_side)+id_wafer
		      //strip number=1-mConfig->getNumberOfStrips()
		      //id_side=0 for p side, 1 for n side
		      if (data>0) {
			if (id_side==1) my_channel=maxChannel-1-channel;//3/11 : decommenter
				else my_channel=channel;                  //3/11 : decommenter
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
			else ladderCountN[ladder]++;
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
		      if (pedestal>0) {
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
			ssdPedStrip->AddAt(&out_ped_strip);
			if (id_side ==0) ladderCountP[ladder]++;
			else ladderCountN[ladder]++;
			count++;
		      } // end if pedestal > 0
		    } // end if(stssdreader->getSsdData(ladder,EastWest,channel,data,pedestal,noise)==0)  
		} // end for (channel=0;channel<maxChannel;channel++) 
	    } // end if (mConfig->getLadderIsActive(ladder)>0)
	} // end for (ladder=0;ladder<mConfig->mConfig->getTotalNumberOfLadders();ladder++) 
    }  // end for (id_side=0;id_side<2;id_side++)

    gMessMgr->Info() <<"StSsdDaqMaker::Make()/Number of raw data in the SSD" << endm;
    gMessMgr->Info() << "StSsdDaqMaker::Make()/Active Ladders:  ";
    for (int i=0;i<mConfig->getTotalNumberOfLadders();i++) 
      if (mConfig->getLadderIsActive(i+1)>0) {
	gMessMgr->width(5);
	*gMessMgr<<i+1<<" ";
      }
  
    *gMessMgr<<endm;
    gMessMgr->Info() << "StSsdDaqMaker::Make()/Counts (p-side): ";
    for (int i=0;i<mConfig->getTotalNumberOfLadders();i++)
      if (mConfig->getLadderIsActive(i+1)>0) {
	gMessMgr->width(5);
	*gMessMgr <<ladderCountP[i]<<" ";
      }
    *gMessMgr<<endm;
    gMessMgr->Info() << "StSsdDaqMaker::Make()/Counts (n-side): ";
    for (int i=0;i<mConfig->getTotalNumberOfLadders();i++)
      if (mConfig->getLadderIsActive(i+1)>0) {
	gMessMgr->width(5);
	*gMessMgr <<ladderCountN[i]<<" ";
      }
    *gMessMgr<<endm;

  gMessMgr->Info() << "StSsdDaqMaker::Make()/  spa_strip->NRows= "<<spa_strip->GetNRows()<<endm;
  gMessMgr->Info() << "StSsdDaqMaker::Make()/ssdPedStrip->NRows= "<<ssdPedStrip->GetNRows()<<endm;
  return kStOK;
}










