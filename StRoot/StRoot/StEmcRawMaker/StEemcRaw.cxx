// $Id: StEemcRaw.cxx,v 1.19 2011/01/04 19:04:08 stevens4 Exp $

#include <math.h>
#include <assert.h>

#include <StMessMgr.h>

#include <StEventTypes.h>
#include <StEvent.h>

#include <StDAQMaker/StEEMCReader.h>

#include <StEEmcUtil/database/StEEmcDb.h>
#include <StEEmcUtil/database/EEmcDbItem.h>
#include <StEEmcUtil/database/EEmcDbCrate.h>

#include <StEEmcUtil/EEfeeRaw/EEfeeDataBlock.h>  // for corruption tests
#include "StMuDSTMaker/EZTREE/EztEmcRawData.h"

#include "StEemcRaw.h"
#include "StEmcRawMaker.h"
#include "DAQ_READER/daq_det.h"
#include "DAQ_ETOW/daq_etow.h"
#include "DAQ_ESMD/daq_esmd.h"
#include "DAQ_EMC/daq_emc.h"
#include "StChain/StRtsTable.h"

ClassImp(StEemcRaw)

//_____________________________________________________________________
StEemcRaw::StEemcRaw()
{
    mDb=0;
    memset(hs,0,sizeof(hs));
}

//___________________________________________________________
StEemcRaw::~StEemcRaw()
{
}

//____________________________________________________
//____________________________________________________
//____________________________________________________
Bool_t StEemcRaw::make(StEmcRawMaker* maker, StEvent* mEvent){
    if (hs[0])
        hs[0]->Fill(0);

    StEmcRawData *eemcRaw = mEvent->emcCollection()->eemcRawData();

    if(!maker || !mDb || ! eemcRaw )
    {
        gMessMgr->Message("","W") <<  GetName()<<"::makeEemc() , some pointers are ZERO, code is sick, chain should be aborted, no EEMC data processed,JB :" <<maker << mDb << eemcRaw <<endm;
        return false;
    }

    if (hs[0])
        hs[0]->Fill(1);
    //::::::::::::::::: copy raw data to StEvent :::::::::::::
    if(! copyRawData(maker,  eemcRaw) )
        return false;

    if(hs[0])
        hs[0]->Fill(2);

    //::::::::::::::: assure raw data are sane  ::::::::::::::::
    StL0Trigger* trg=mEvent->l0Trigger();
    if (! trg)
    {
        gMessMgr->Message("","W") <<  GetName()<<"::makeEemc() , l0Trigger data, EEMC not verified, abort all EEMC hits in StEvent EmcCollection, but raw data are saved" << endm;
        return false;
    }


    int token=trg->triggerToken();
    // printf("\nStL0Trigger::token=%d\n",token);
    int runId=mEvent->runId();
    int time=mEvent->time();

    if( headersAreSick(maker, eemcRaw, token,  runId, time) )
        return false;
    if (hs[0])
        hs[0]->Fill(3);

    if( towerDataAreSick( eemcRaw))
        return false;
    if (hs[0])
        hs[0]->Fill(3);

    raw2pixels(mEvent);
    return true;

}

Bool_t StEemcRaw::copyRawData(StEmcRawMaker* maker, StEmcRawData *raw)
{
    StRtsTable* etow = maker->GetDaqElement("etow/adc");
    int nb=0;
    if(etow){
      etow_t* etowdata = (etow_t*)*etow->begin();
      for(int icr = 0; icr < ETOW_MAXFEE; icr++){
	nb++;
	const EEmcDbCrate *fiber = mDb->getFiber(icr);

	raw->createBank(icr,fiber->nHead,fiber->nCh);
	for(int i = 0; i < fiber->nCh; i++){
	  raw->setData(icr,i,etowdata->adc[icr][i]);
	  //printf("agrdl: ETOW ADC %d %d %d\n",icr,i,etowdata->adc[icr][i]);
	}
	for(int i = 0; i < fiber->nHead; i++){
	  raw->setHeader(icr,i,etowdata->preamble[icr][i]);
	  //printf("agrdl: ETOW HEAD %d %d %d\n",icr,i,etowdata->preamble[icr][i]);
	}
      }
    }else{
      LOG_ERROR<<"ETOW Structure not found"<<endm;
    }
    StRtsTable* esmd = maker->GetDaqElement("esmd/adc");
    if(esmd){
      esmd_t* esmddata = (esmd_t*)*esmd->begin();
      for(int icr = 0; icr < ESMD_MAXFEE;icr++){
	nb++;
	int id = icr + ETOW_MAXFEE;
	const EEmcDbCrate *fiber = mDb->getFiber(id);

	raw->createBank(id,fiber->nHead,fiber->nCh);
	for(int i = 0; i < fiber->nCh; i++){
	  raw->setData(id,i,esmddata->adc[icr][i]);
	  //printf("agrdl: ETOW ADC %d %d %d\n",id,i,esmddata->adc[icr][i]);
	}
	for(int i = 0; i < fiber->nHead; i++){
	  raw->setHeader(id,i,esmddata->preamble[icr][i]);
	  //printf("agrdl: ETOW HEAD %d %d %d\n",id,i,esmddata->preamble[icr][i]);
	}
      }
    }else{
      LOG_ERROR<<"ESMD Structure not found"<<endm;
    }

    LOG_INFO << "StEemcRaw::copyRawData() "<<nb<<" data bloks copied" << endm;
    return true;
}

Bool_t StEemcRaw::headersAreSick(StEmcRawMaker* maker, StEmcRawData *raw, int token, int runId, int time)
{
    if (! raw)
    {
        gMessMgr->Message("","W") << "StEemcRaw::headersAreSick() no EEMC raw data" << endm;
        return true;
    }


    EEfeeDataBlock block; // use utility class as the work horse

    int icr;
    int totErrBit=0;
    int nOn=0;

    LOG_INFO << "StEemcRaw::headersAreSick() --> Nfibers=" << mDb->getNFiber() << endm;
    //LOG_INFO << "StEemcRaw::headersAreSick() --> isEemcBankIn('T')=" << etow << endm;
    //LOG_INFO << "StEemcRaw::headersAreSick() --> isEemcBankIn('S')=" << esmd << endm;

    for(icr=0;icr<mDb->getNFiber();icr++)
    {
        const EEmcDbCrate *fiber=mDb-> getFiber(icr);
        if(!fiber->useIt)
            continue; // drop masked out crates

	//if (fiber->type == 'T' && !etow) continue;
	//if (fiber->type == 'S' && !esmd) continue;

        if(raw->sizeHeader(icr)<=0) {
	 LOG_WARN <<Form("StEemcRaw::headersAreSick() sizeHeader(icr=%d)<=0,  crID=%d, skip it\n",icr,fiber->crID)<<endm;
	  continue;  //drop cartes not present in data blocks
	}
	// printf(" EEMC raw-->pix crID=%d type=%c \n",fiber->crID,fiber->type);

        int isOff=EztEmcRawData::isCrateOFF(raw->header(icr));
        // printf("AAA icr=%d isOff=%d\n",icr,isOff);
        if(isOff)
        { // kill this fiber for the rest of this job
            mDb->setFiberOff(icr);
            LOG_WARN << "StEemcRaw::headersAreSick() detected icr="<<icr<< ", name="<<fiber->name<<" is OFF,\n this fiber will ignored till the end of this job" << endm;
            continue;
        }
        nOn++;
        block.clear();
        block.setHead(raw->header(icr));

        int lenCount=fiber->nCh+fiber->nHead;
        int errFlag=0;

        if(fiber->type=='T')
            lenCount+=32; // one more board exist in harware

	if(fiber->type=='S' && time<1104537600 && runId<6000000)
            errFlag=0x28;  // bug in box firmawer prior to 2005

	//HACK^2 - disable token check, March 22, 2007
	//token=block.getToken();
	// end of hack, Jan B.

        int trigCommand=4; // physics, 9=laser/LED, 8=??
        int sanity=block.isHeadValid(token,fiber->crIDswitch,lenCount,trigCommand,errFlag);
	if(0){
	  block.print(1);
	  cout<<(short)block.getCrateID()<<" "<<fiber->crIDswitch<<" :::: "<<block.getLenCount()<<" "<<lenCount<<" :::: "<<block.getTrigComm()<<" "<<trigCommand<<" :::: "<<block.getErrFlag()<<" "<<errFlag<<endl;
	}
	//printf("Endcap crate=%d token=%d\n",fiber->crID,block.getToken());
        int i;
        for(i=0;i<8;i++)
        {// examin & histo all sanity bits
            if(!(sanity&(1<<i)))
                continue;
            totErrBit++;
            int k=icr*10+i;
            //      printf("ic=%d on bit=%d k=%d   %d %d  \n",ic,i,k,1<<i,sn&(1<<i) );
            if (hs[3])
                hs[3]->Fill(k);
        }
       LOG_DEBUG  << GetName()<<"::checkHeader("<<fiber->name<<"), errorCode="<<sanity<<endm;
       if(sanity) LOG_WARN  << GetName()<<"::checkHeader("<<fiber->name<<"), errorCode="<<sanity<<endm;
    }

    if (hs[4])
        hs[4]->Fill(totErrBit);

    LOG_INFO<< GetName()<<"::checkHeader --> totErrBit "<<totErrBit<<" in "<<nOn<<" crates"<<endm;
    return totErrBit;
}

///////////
//////////
/////////

Bool_t   StEemcRaw::make(StEEMCReader *eeReader, StEvent* mEvent){
    //  printf("JB make()  EEMC\n");

    if (hs[0])
        hs[0]->Fill(0);

    StEmcRawData *eemcRaw = mEvent->emcCollection()->eemcRawData();

    if(!eeReader || !mDb || ! eemcRaw )
    {
        gMessMgr->Message("","W") <<  GetName()<<"::makeEemc() , some pointers are ZERO, code is sick, chain should be aborted, no EEMC data processed,JB :" <<eeReader << mDb << eemcRaw <<endm;
        return false;
    }

    if (hs[0])
        hs[0]->Fill(1);
    //::::::::::::::::: copy raw data to StEvent :::::::::::::
    if(! copyRawData(eeReader,  eemcRaw) )
        return false;

    if(hs[0])
        hs[0]->Fill(2);

    //::::::::::::::: assure raw data are sane  ::::::::::::::::
    StL0Trigger* trg=mEvent->l0Trigger();
    if (! trg)
    {
        gMessMgr->Message("","W") <<  GetName()<<"::makeEemc() , l0Trigger data, EEMC not verified, abort all EEMC hits in StEvent EmcCollection, but raw data are saved" << endm;
        return false;
    }


    int token=trg->triggerToken();
    // printf("\nStL0Trigger::token=%d\n",token);
    int runId=mEvent->runId();
    int time=mEvent->time();

    if( headersAreSick(eeReader, eemcRaw, token,  runId, time) )
        return false;
    if (hs[0])
        hs[0]->Fill(3);

    if( towerDataAreSick( eemcRaw))
        return false;
    if (hs[0])
        hs[0]->Fill(3);

    raw2pixels(mEvent);
    return true;
}

//____________________________________________________
//____________________________________________________
//____________________________________________________
Bool_t   StEemcRaw::copyRawData(StEEMCReader *eeReader, StEmcRawData *raw)
{
    int nb=0;
    int icr;
    for(icr=0;icr<mDb->getNFiber();icr++)
    {
        const EEmcDbCrate *fiber=mDb-> getFiber(icr);
        if(!eeReader->isEemcBankIn(fiber->type))
            continue;
        nb++;
        //    printf("copy EEMC raw: ");fiber->print();
        raw->createBank(icr,fiber->nHead,fiber->nCh);
        raw->setHeader(icr,eeReader->getEemcHeadBlock(fiber->fiber,fiber->type));
        raw->setData(icr,eeReader->getEemcDataBlock(fiber->fiber,fiber->type));
    }
    gMessMgr->Message("","I") << "StEemcRaw::copyRawData() "<<nb<<" data bloks copied" << endm;
    return true;
}


//____________________________________________________
//____________________________________________________
//____________________________________________________
Bool_t   StEemcRaw::headersAreSick(StEEMCReader *eeReader, StEmcRawData *raw, int token, int runId, int time)
{

    if (! raw)
    {
        gMessMgr->Message("","W") << "StEemcRaw::headersAreSick() no EEMC raw data" << endm;
        return true;
    }


    EEfeeDataBlock block; // use utility class as the work horse

    int icr;
    int totErrBit=0;
    int nOn=0;

    LOG_INFO << "StEemcRaw::headersAreSick() --> Nfibers=" << mDb->getNFiber() << endm;
    LOG_INFO << "StEemcRaw::headersAreSick() --> isEemcBankIn('T')=" << eeReader->isEemcBankIn('T') << endm;
    LOG_INFO << "StEemcRaw::headersAreSick() --> isEemcBankIn('S')=" << eeReader->isEemcBankIn('S') << endm;

    for(icr=0;icr<mDb->getNFiber();icr++)
    {
        const EEmcDbCrate *fiber=mDb-> getFiber(icr);
        if(!fiber->useIt)
            continue; // drop masked out crates

	if (fiber->type == 'T' && !eeReader->isEemcBankIn('T')) continue;
	if (fiber->type == 'S' && !eeReader->isEemcBankIn('S')) continue;

        if(raw->sizeHeader(icr)<=0) {
	 gMessMgr->Message("","W") <<Form("StEemcRaw::headersAreSick() sizeHeader(icr=%d)<=0,  crID=%d, skip it\n",icr,fiber->crID)<<endm;
	  continue;  //drop cartes not present in data blocks
	}
	// printf(" EEMC raw-->pix crID=%d type=%c \n",fiber->crID,fiber->type);

        int isOff=EztEmcRawData::isCrateOFF(raw->header(icr));
        // printf("AAA icr=%d isOff=%d\n",icr,isOff);
        if(isOff)
        { // kill this fiber for the rest of this job
            mDb->setFiberOff(icr);
            gMessMgr->Message("","W") << "StEemcRaw::headersAreSick() detected icr="<<icr<< ", name="<<fiber->name<<" is OFF,\n this fiber will ignored till the end of this job" << endm;
            continue;
        }
        nOn++;
        block.clear();
        block.setHead(raw->header(icr));

        int lenCount=fiber->nCh+fiber->nHead;
        int errFlag=0;

        if(fiber->type=='T')
            lenCount+=32; // one more board exist in harware

        if(fiber->type=='S' && time<1104537600 && runId<6000000)
            errFlag=0x28;  // bug in box firmawer prior to 2005

	//HACK^2 - disable token check, March 22, 2007
	//token=block.getToken();
	// end of hack, Jan B.

        int trigCommand=4; // physics, 9=laser/LED, 8=??
        int sanity=block.isHeadValid(token,fiber->crIDswitch,lenCount,trigCommand,errFlag);
	//printf("Endcap crate=%d token=%d\n",fiber->crID,block.getToken());
        int i;
        for(i=0;i<8;i++)
        {// examin & histo all sanity bits
            if(!(sanity&(1<<i)))
                continue;
            totErrBit++;
            int k=icr*10+i;
            //      printf("ic=%d on bit=%d k=%d   %d %d  \n",ic,i,k,1<<i,sn&(1<<i) );
            if (hs[3])
                hs[3]->Fill(k);
        }
       LOG_DEBUG  << GetName()<<"::checkHeader("<<fiber->name<<"), errorCode="<<sanity<<endm;
       if(sanity) LOG_WARN  << GetName()<<"::checkHeader("<<fiber->name<<"), errorCode="<<sanity<<endm;
    }

    if (hs[4])
        hs[4]->Fill(totErrBit);

    gMessMgr->Message("","I") << GetName()<<"::checkHeader --> totErrBit "<<totErrBit<<" in "<<nOn<<" crates"<<endm;
    return totErrBit;
}


//____________________________________________________
//____________________________________________________
//____________________________________________________
Bool_t  StEemcRaw::towerDataAreSick(StEmcRawData* raw)
{
    const int mxN256one=5;
    const int mxN256tot=40; // may need to be changed for 2005 data with more MAPMT crates, JB

    int nGhostTot=0, n256Tot=0;
    int icr;
    for(icr=0;icr<mDb->getNFiber();icr++)
    {
        const EEmcDbCrate *fiber=mDb-> getFiber(icr);
        if(!fiber->useIt)
            continue; // drop masked out crates
        if(fiber->type!='T')
            continue;
        const  UShort_t* data=raw->data(icr);
        //assert(data);
        int i;
        int nGhost=0, n256=0;
        for(i=0;i<raw->sizeData(icr);i++)
        {
            if((data[i] &0xff)==0 )
                n256++;
            if(i>=121 && data[i]>40)
                nGhost++;
        }
        nGhostTot+=nGhost;
        n256Tot+=n256;
        if(nGhost>0)
        {
            int k=icr*10+5;
            if (hs[3])
                hs[3]->Fill(k);
        }
        if(n256>mxN256one)
        {
            int k=icr*10+6;
            if (hs[3])
                hs[3]->Fill(k);
        }

    }

    if(hs[1])
        hs[1]->Fill(n256Tot);
    if(hs[2])
        hs[2]->Fill(nGhostTot);

    gMessMgr->Message("","I") << GetName()<<"::checkTowerAdc-->total n256="<<n256Tot <<", nGhost="<<nGhostTot<<endm;
    if(nGhostTot>0)
        return true;
    if(n256Tot>mxN256tot)
        return true;
    return false;
}


//____________________________________________________
//____________________________________________________
//____________________________________________________
void  StEemcRaw::raw2pixels(StEvent* mEvent)
{

    StEmcCollection* emcC =(StEmcCollection*)mEvent->emcCollection();
    if(emcC==0)
    {
        gMessMgr->Message("","W") << GetName()<<"::raw2pixels() no emc collection, skip"<<endm;
        return ;
    }

    StEmcRawData* raw=emcC->eemcRawData();
    if (! raw)
    {
        gMessMgr->Message("","W") << "StEemcRaw::raw2pixels() no EEMC raw data" << endm;
        return;
    }

    // initialize tower/prePost /U/V in StEvent

    StEmcDetector* emcDet[kEndcapSmdVStripId+1]; // what a crap,JB
    StDetectorId emcId[kEndcapSmdVStripId+1];    // more crap
    memset(emcDet,0,sizeof(emcDet));

    int det;
    for(det = kEndcapEmcTowerId; det<= kEndcapSmdVStripId; det++)
    {
        emcId[det] = StDetectorId(det);
        emcDet[det] = new StEmcDetector(emcId[det],MaxSectors);
        emcC->setDetector(emcDet[det]);
    }

    gMessMgr->Message("","I") << GetName()<<"::raw2pixels() collections created"<<endm;

    // store data from raw blocks to StEvent
    int nDrop=0;
    int nMap=0;
    int nTow=0, nPre=0,nSmd=0;
    int icr;
    for(icr=0;icr<mDb->getNFiber();icr++)
    {
        const EEmcDbCrate *fiber=mDb-> getFiber(icr);
        if(!fiber->useIt)
            continue; // drop masked out crates

        const  UShort_t* data=raw->data(icr);
        //assert(data);

        for(int chan=0;chan<raw->sizeData(icr);chan++)
        {
            const  EEmcDbItem  *x=mDb->getByCrate(fiber->crID,chan);
            if(x==0)
            {
                //printf("No EEMC mapping for crate=%3d chan=%3d\n",crate,chan);
                nDrop++;
                continue;
            }

            int det = 0;
            char type=x->name[2];
            int sec=x->sec;   //range 1-12
            int sub=1+x->sub-'A'; //range 1-5
            int eta=x->eta;   //range 1-12

            int rawAdc=data[chan];
            float energy=123.456; // dumm value, calib & peds are not known

            switch(type)
            { // tw/pre/post/SMD
            case 'T': // towers
                det = kEndcapEmcTowerId;
                nTow++;
                break;
            case 'P': // pres1
            case 'Q': // pres2
            case 'R': // post
                sub+=5* (type-'P'); // pack pre1,2,post together
                det = kEndcapEmcPreShowerId;
                nPre++;
                break;
            case 'U': //SMD
            case 'V':
                sub=1; // not used for SMD
                eta=x->strip;  //range 1-288
                nSmd++;
                det = kEndcapSmdUStripId;
                if(type=='V')
                    det = kEndcapSmdVStripId;
                break;
            default:
                continue;
            }

            //assert(det);
            if (! det)
            {
                gMessMgr->Message("","W") << "StEemcRaw::raw2pixels(), logic error2, skip" << endm;
                return;
            }

            // if(type=='T') printf("EEMC crate=%3d chan=%3d  ADC: raw=%4d  energy=%+10g  -->   %2.2dT%c%2.2d\n",fiber->crID,chan,rawAdc,energy,x->sec,x->sub,x->eta);

            StEmcRawHit* h = new StEmcRawHit(emcId[det],sec,eta,sub,rawAdc,energy);
            emcDet[det]->addHit(h);
            nMap++;

        } // end of loop over channels
    }// end of loop over crates


    gMessMgr->Message("","I") << GetName()<<"::raw2pixels() finished  nDrop chan="<< nDrop<<",nMap="<< nMap<<",nTow="<<nTow <<",nPre="<<nPre <<", nSmd="<<nSmd <<endm;

}


//__________________________________________________
//__________________________________________________
//__________________________________________________
void StEemcRaw::initHisto()
{

    hs[0]= new TH1F("EndcapHealth","raw data health; X: 0=nEve, 1=raw, 2=OKhead , 3=tower(No ghost/n256)",9,-1.5,7.5);

    hs[1]= new TH1F("EndcapN256","No. of n256/eve, all header OK",100, -1.5,98.5);
    hs[2]= new TH1F("EndcapGhost","No. of tower nGhost/eve, all header OK, chan>119",100,-1.5,98.5);

    hs[3]=new TH1F("EndcapCorrBytes","sanity, crates Tw cr=0-5, Mapmt cr=6-53,  X= bits(cr)+ cr*10;bits: 0=crID, 1=token,2=len,3=trgCom,4=ErrFlg,5=Ghost,6=n256 ",540,-0.5,539.5);

    hs[4]=new TH1F("EndcapCorrTot","total # of corruption bits in Headers per eve",220,-0.5,219.5);

    return;
}


// $Log: StEemcRaw.cxx,v $
// Revision 1.19  2011/01/04 19:04:08  stevens4
// added event time to EEMC header check
//
// Revision 1.18  2009/05/06 14:25:54  mattheww
// changed a threshold in EEMC
//
// Revision 1.17  2009/03/23 21:08:32  mattheww
// Update default BPRS ZS handling and fix EEMC minor bug
//
// Revision 1.16  2009/02/04 21:05:42  kocolosk
// Refactor StEEmcDb(Maker), new location for StEmcDecoder. Fixes RT #1388.
//
// Revision 1.15  2009/01/27 19:58:36  mattheww
// Updates to StEmcRawMaker to be compatible with 2009 DAQ Format
//
// Revision 1.14  2008/01/10 20:49:58  balewski
// now more warnings if ESMD is not in the run, thanks Pibero
//
// Revision 1.13  2007/04/11 03:29:11  balewski
// undo hacks,
// Endcap code is now in default configuration
//
// Revision 1.12  2007/03/23 02:18:00  balewski
// drop token check, temporay hack
//
// Revision 1.11  2006/12/22 15:20:59  balewski
// more printouts
//
// Revision 1.10  2006/07/21 14:15:09  balewski
// less printouts
//
// Revision 1.9  2006/01/16 11:12:00  suaide
// tower map bug fixed and astyle run
//
// Revision 1.8  2005/08/17 20:59:32  balewski
// cleanup
//
// Revision 1.7  2005/08/17 20:50:44  balewski
// drop only crates which are off insetad of the whole event
//
// Revision 1.6  2005/05/04 22:02:08  balewski
// more clear printouts
//
// Revision 1.5  2005/02/03 02:35:11  balewski
// accomodate MAPMT firmware change in 2005
//
// Revision 1.4  2005/01/08 03:55:36  balewski
// logic was too conservative
//
// Revision 1.3  2004/11/14 21:55:52  jeromel
// Unit var initialized (++ used later)
//
// Revision 1.2  2004/10/21 00:01:50  suaide
// small changes in histogramming and messages for BEMC
// Complete version for EEMC done by Jan Balewski
//
// Revision 1.1  2004/10/19 23:48:49  suaide
// Initial implementation of the endcap detector done by Jan Balewski
//

//.........old pieces of code use for some tests .......................



#if 0 // test of tower data storage
{
    int det= kEndcapEmcTowerId;
    int eta,sub=1,sec=12;
    for(sec=1;sec<=12;sec++)
        for(sub=1;sub<=5;sub++)
            for(eta=1;eta<=12;eta++)
            {
                int adc=1000+ (eta-1) + (sub-1)*12 +(sec-1)*60;
                float ener=777;
                StEmcRawHit* h = new StEmcRawHit(emcId[det], sec,eta,sub,adc,ener);
                emcDet[det]->addHit(h);
            }
}
#endif



#if 0 // test of Pre/post data storage
{
    int det= kEndcapEmcPreShowerId;
    int eta,sub,sec=12, pre=1;
    for(pre=1;pre<=3;pre++)
        for(sec=1;sec<=12;sec++)
            for(sub=1;sub<=5;sub++)
                for(eta=1;eta<=12;eta++)
                {
                    int ssub=sub + 5*(pre-1);
                    int adc= eta-1 + (sub-1)*12 +(sec-1)*60 +pre*1000;
                    StEmcRawHit* h = new StEmcRawHit(emcId[det], sec,eta,ssub,adc);
                    emcDet[det]->addHit(h);
                }
}
#endif



#if 0 // test of U-smd data storage
{
    det = kEndcapSmdUStripId;
    int eta,sub=1,sec=12;
    for(sec=1;sec<=12;sec++)
        for(eta=1;eta<=288;eta++)
        {
            int adc= 1000 + eta-1  +(sec-1)*300;
            StEmcRawHit* h = new StEmcRawHit(emcId[det], sec,eta,sub,adc);
            emcDet[det]->addHit(h);
        }
}

for(det = kEndcapEmcTowerId; det<= kEndcapSmdVStripId; det++)
{
    StEmcDetector* emcDetX= emcC->detector( StDetectorId(det));
    // assert(emcDetX);
    gMessMgr->Message("","I") <<" StEmcDetectorID="<< StDetectorId(det)<<"  nHits="<<emcDetX->numberOfHits()<<endm;
}



#endif



#if 0 // test of new access method

for(icr=0;icr<mDb->getNCrate();icr++)
{
    const EEmcDbCrate *crate=mDb-> getCrate(icr);
    printf("geting data for fiber: ");
    crate->print();

    printf("---- HEAD ----\n");
    for(ch=0;ch<crate->nHead;ch++)
    {
        int val=-1;
        val=steemcreader->getEemcHead(crate->fiber,ch,crate->type);
        printf("cr=%d ch=%d val=0x%04x\n",crate->crID,ch,val);
    }

    printf("---- DATA ----\n");
    for(ch=0;ch<crate->nch;ch++)
    {
        int val=-1;
        val=steemcreader->getEemcData(crate->fiber,ch,type);
        printf("cr=%d ch=%d val=0x%04x\n",crate->crID,ch,val);
    }
    //   break;
}
#endif


