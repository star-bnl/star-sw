// page 1 goes bemcStatus (X, Y linear scales), bemcStatusCrates (X, Y linear) and bemcTotalAdc( Y in log)
// page 2 goes bemcTDC[0]  to bemcTDC[14] with y-axis in log scale
// page 3 goes bemcTDC[15] to bemcTDC[29] with y-axis in log scale
// page 4 goes bsmdCapacitor (X, Y linear scales) and bsmdTotalAdc (Y in log)
// page 5 goes bsmdTotalAdcFiber[0] to bsmdTotalAdcFiber[4] (X is linear, Y is log scales)
// page 6 goes bsmdTotalAdcFiber[5] to bsmdTotalAdcFiber[8] (X is linear, Y is log scales)
// page 7 goes HT (X is lin, Y is lin), HTDistr (X is lin, Y is log), PA (X is lin, Y is lin) and PADistr (X is lin, Y is log)

// change the labels Crates 1-15 and Crates 16-30 to TDC 0-14 and TDC 15-29

// All 2-D histograms are to be displayed as "colz"

// please remove the High tower and Patch histograms (Bemc Trigger) from the trigger page
// it has old decoding and does not work well anymore

TH1F *bemcTotalAdc;  //261 +
TH1F *bemcTDC[30];   //231-260 +
TH2F *bemcCratesStatus; //263  +
TH1F *bemcStatus;       //262  +
TH2F *bsmdCapacitor;    //264  +
TH1F *bsmdTotalAdc;     //265  +
TH1F *bsmdTotalAdcFiber[8]; //317 - 324 +
TH2F *HT; //325
TH2F *PA; //326
TH1F *HTDistr; //327
TH1F *PADistr; //328

void bookBemcHisto()
{
    float minTowerSum = 0;
    float maxTowerSum = 20000;
    float minSmdSum = 500000;
    float maxSmdSum = 2500000;

    for(int i = 0;i<30;i++)
    {
        char tmp[30],tmp1[50];
        sprintf(tmp,"BemcTDC-%02i",i+1);
        sprintf(tmp1,"BEMC total ADC histogram for TDC channel %02i",i+1);
        bemcTDC[i] = new TH1F(tmp,tmp,250,minTowerSum,maxTowerSum);
    }
    for(int i = 0;i<8;i++)
    {
        char tmp[30],tmp1[50];
        sprintf(tmp,"SMDFiber-%02i",i+1);
        sprintf(tmp1,"BSMD total ADC histogram for fiber %02i",i+1);

        bsmdTotalAdcFiber[i] = new TH1F(tmp,tmp,250,minSmdSum/6.,maxSmdSum/4.);

    }
    bemcTotalAdc = new TH1F("BemcTotalAdc","Bemc Total ADC distribution",250,15*minTowerSum,15*maxTowerSum);
    bemcStatus = new TH1F("BemcStatus","Bemc Global Status (0 = total, 1 = good, 2 = bad)", 5,-1.5,3.5);
    bemcCratesStatus = new TH2F("BemcTDCStatus","Bemc TDC Status (0 = total, 1 = good, 2 = bad)",5,-1.5,3.5,30,-0.5,29.5);
    bsmdCapacitor = new TH2F("BsmdCapacitor","Bsmd Capacitor distribution",8,-0.5,7.5,128,-0.5,127.5);
    bsmdTotalAdc = new TH1F("BsmdTotalAdc","Bsmd Total ADC distribution",250,minSmdSum,maxSmdSum);

    HT = new TH2F("HT","High Tower trigger data",300,-0.5,299.5,64,-0.5,63.5);

    PA = new TH2F("PA","Patch trigger data",300,-0.5,299.5,64,-0.5,63.5);

    HTDistr = new TH1F("HTDistr","High Tower histogram",64,-0.5,63.5);

    PADistr = new TH1F("PADistr","Patch histogram",64,-0.5,63.5);

    // histogram configurations
    for(int i = 0;i<30;i++)
    {
        bemcTDC[i]->GetXaxis()->SetTitle("ADC Sum");
        bemcTDC[i]->SetFillColor(4);
    }
    for(int i = 0;i<8;i++)
    {
        bsmdTotalAdcFiber[i]->GetXaxis()->SetTitle("ADC Sum");
        bsmdTotalAdcFiber[i]->SetFillColor(4);
    }
    bemcTotalAdc->SetFillColor(4);
    bemcTotalAdc->GetXaxis()->SetTitle("Total ADC");

    bemcStatus->SetFillColor(4);
    bemcStatus->GetXaxis()->SetTitle("EMC Event Status");

    bemcCratesStatus->GetXaxis()->SetTitle("TDC Status");
    bemcCratesStatus->GetYaxis()->SetTitle("TDC Number");

    bsmdCapacitor->GetXaxis()->SetTitle("Fiber optics number (RDO)");
    bsmdCapacitor->GetYaxis()->SetTitle("Capacitor Number");

    bsmdTotalAdc->SetFillColor(4);
    bsmdTotalAdc->GetXaxis()->SetTitle("Total ADC");

    HT->GetXaxis()->SetTitle("Trigger Tower Number");
    HT->GetYaxis()->SetTitle("ADC");
    PA->GetXaxis()->SetTitle("Trigger Tower Number");
    PA->GetYaxis()->SetTitle("ADC");
    HTDistr->GetXaxis()->SetTitle("ADC");
    PADistr->GetXaxis()->SetTitle("ADC");

}
//////////////////////////////////////////////////////////////////////////////////////////////
void fillBemcHisto(char *datap)
{
    int NTDC = 30;
    int NSMD = 8;

    int ret=emcReader(datap);

    if(ret<0)
        return; // bad Event
    if(ret<1024)
        return; // no EMC in the event

    unsigned short *header = emc.btow_raw;

    // bemc --------------------------------------------------------------------------------
    // decoding the header of the event
    int evstatus = 1;
    float totalAdc = 0;
    int adcSum[30];
    int status[30];
    for(int i=0;i<NTDC;i++)
    { adcSum[i]=0; status[i] = 1;}

    if(header) // good tower event
    {
        // decoding the data
        for(int i=0;i<4800;i++)
        {
            int tdc = i%30;
            if(tdc<NTDC)
            {
                int crate   = *(header+tdc+90) & 0x0FF;
                int counter = *(header+tdc);
                if(crate>0 && crate <31 && counter==164)
                {
                    adcSum[tdc] +=  emc.btow[i];
                    totalAdc+=(float)emc.btow[i];
                }
            }
        }
        int nbad=0;
        for(int i=0;i<NTDC;i++)
        {
            int crate   = *(header+i+90) & 0x0FF;
            //float avg = (float)adcSum[i]/160.;
            int counter = *(header+i);
            if(counter!=164 || crate<=0 || crate >30)
            {
                status[i] = 2;
                //evstatus = 2;
                nbad++;
            }
            //taken out by alexst
            if(nbad>20)
                evstatus = 2;
            bemcCratesStatus->Fill(0.0,(float)i);
            bemcCratesStatus->Fill((float)status[i],(float)i);
        }
    }
    bemcStatus->Fill(evstatus);
    bemcStatus->Fill(0);
    //if(evstatus==1)
    {
        bemcTotalAdc->Fill(totalAdc);
        for(int i=0;i<30;i++)
            if(adcSum[i]!=0 && status[i]==1)
                bemcTDC[i]->Fill(adcSum[i]);
    }

    // smd ---------------------------------------------------------------------------------

    totalAdc = 0;
    float fiberSum[8];
    int cap[8];
    for(int i=0;i<NSMD;i++)
    {cap[i]=-1; fiberSum[i] = 0;}

    for(int RDO=0;RDO<NSMD;RDO++)
    {
        cap[RDO] = emc.bsmd_cap[RDO];
        bsmdCapacitor->Fill(RDO,cap[RDO]);
        for(int index=0;index<4800;index++)
        {
            totalAdc+=(float)emc.bsmd[RDO][index];
            fiberSum[RDO]+=(float)emc.bsmd[RDO][index];
        }
    }
    bsmdTotalAdc->Fill(totalAdc);
    for(int i = 0;i<NSMD;i++)
    {
        bsmdTotalAdcFiber[i]->Fill(fiberSum[i]);
    }

    // high tower ---------------------------------------------------------------------------------

    ret=trgReader(datap);
    if(ret<0)
        return; // no trigger event

    unsigned char raw[480];
    for(int i=0;i<240;i++)
    {
        raw[i]=trg.BEMC[1][i];
        raw[i+240]=trg.BEMC[0][i];
    }

    int patch;
    int dsm_read_map[16]={7,6,5,4,3,2,1,0,15,14,13,12,11,10,9,8};
    int tower_map[10]={0,1,2,3,4,5,6,7,8,9};  // map into DSM board
    unsigned char dsmby[30][16];
    unsigned char ch[16];

    for (int i=0; i<30; i++)
        for (int j=0; j<16; j++)
        {
            int k = 16*i + j;
            dsmby[i][j] = raw[k];
        }
    for(int i=0;i<ndsms;i++)
    {
        patch = i;
        for(int j=0;j<16;j++)
        {
            int k = dsm_read_map[j];
            ch[k]= dsmby[i][j];
        }
        int nt=0;
        for(int k=0;k<5;k++)
        {
            int nby=3*k;
            int hi_tower = (ch[nby]) & 0x3f;
            int sum_tower = ((ch[nby]>>6) & 0x3) + (((ch[nby+1]) & 0xf) << 2);
            int it =  tower_map[nt] + 10*(patch);
            HT->Fill(it,hi_tower);
            PA->Fill(it,sum_tower);
            HTDistr->Fill(hi_tower);
            PADistr->Fill(sum_tower);
            nt++;

            hi_tower = ((ch[nby+1]>>4) & 0xf) + (((ch[nby+2]) & 0x3) << 4);
            sum_tower = ((ch[nby+2]>>2) & 0x3f);
            it = tower_map[nt] + 10*(patch);
            HT->Fill(it,hi_tower);
            PA->Fill(it,sum_tower);
            HTDistr->Fill(hi_tower);
            PADistr->Fill(sum_tower);
            nt++;
        }
    }

    return;
}





/***************************************************************************
 *
 * $Id: onlinehisto.h,v 1.1 2009/01/23 16:10:56 jeromel Exp $
 *
 * Author: Frank Laue, laue@bnl.gov
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: onlinehisto.h,v $
 * Revision 1.1  2009/01/23 16:10:56  jeromel
 * Import from online/RTS/src/
 *
 * Revision 1.1  2007/02/27 15:23:41  laue
 * Initial version
 *
 * Revision 1.1  2006/10/04 20:31:34  laue
 * Initial Version
 *
 *
 ***************************************************************************/

