#include <stdio.h>
#include <stdlib.h>

#include "DAQ_READER/daqReader.h"
#include "DAQ_READER/daq_dta.h"
#include <DAQ_READER/daq_dta.h>
#include "DAQ_READER/daq_det.h"
#include <DAQ_FGT/daq_fgt.h>

#include "Jevp/StJevpPlot/RunStatus.h"
#include "StEvent/StTriggerData.h"
#include <TH1I.h>
#include <TH2F.h>
#include <TPaveStats.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "fgtBuilder.h"
#include <RTS/include/rtsLog.h>

// This is the one PlotSet that is guarenteed to always exist
// It's main purpose is to provide the run information 
// To the server...
//
// It has no plots (currently)
//


ClassImp(fgtBuilder);
  

fgtBuilder::fgtBuilder(JevpServer *parent):JevpBuilder(parent),evtCt(0) {
  plotsetname = (char *)"fgt";
  // start with histograms undefined...
  memset(&contents, 0, sizeof(contents));
  memset(&tbVsAdcContents,0,sizeof(tbVsAdcContents));
  memset(&hContents,0,sizeof(hContents));
  memset(&hSumContents,0,sizeof(hSumContents));
}

fgtBuilder::~fgtBuilder() {

  // Delete any existing histograms...

  int n = sizeof(contents) / sizeof(TH2 *);
  int hNp=sizeof(hContents)/sizeof(TH1 *);
  int hSNp=sizeof(hSumContents)/sizeof(TH2 *);
  int nTbVsAdc=sizeof(tbVsAdcContents)/sizeof(TH2 *);


  for(int i=0;i<n;i++) {
    if(contents.array[i]) delete contents.array[i];
  }
  for(int i=0;i<nTbVsAdc;i++)
    {
      if(tbVsAdcContents.tbVsAdcArray[i])delete tbVsAdcContents.tbVsAdcArray[i];
    }
  for(int i=0;i<hNp;i++){
    if(hContents.hArray[i]) delete hContents.hArray[i];
  }

  for(int i=0;i<hSNp;i++){
    if(hSumContents.sumHArray[i]) delete hSumContents.sumHArray[i];
  }

}

void fgtBuilder::initialize(int argc, char *argv[]) {
  
  // Initialization of histograms.
  //could run a loop...
  // Add root histograms to Plots
  //  cout <<"init " << endl;
  errorMsg=0;
  for(int i=0;i<maxC*maxA;i++)
    {
      meanVals[i]=0;
      aVals[i]=0;
      //      rmsVals[i]=0;
      numVals[i]=0;
      numOverOneSig[i]=0;
      oldStdDevs[i]=0;
      isChannelBad[i]=false;
      runningAvg[i]=0;
      runningStdDevSq[i]=0;
    }


  //////////////////////////////////add bad channels here///////////////////////
  ///////////////////isChannelBad[numAssembly*maxC+channel]=true;




  ////////////////////////////////////
  np = sizeof(contents) / sizeof(TH2 *);
  hNp=sizeof(hContents)/sizeof(TH1 *);
  hSNp=sizeof(hSumContents)/sizeof(TH2 *);
  nTbVsAdc=sizeof(tbVsAdcContents)/sizeof(TH2 *);

  char buffer[50];
  char buffer2[50];
  for(int gid=0;gid<np;gid++)
    {
      int rdo;
      if(gid<12)
	rdo=1;
      else 
	rdo=2;

      int sec=floor(gid%12)/2;
      int group=gid%2;

      sprintf(buffer,"Assembly %s",Gid2Label[gid].c_str());
      sprintf(buffer2,"Assembly %s with RDO:%d, ARM:%d, group:%d",Gid2Label[gid].c_str(),rdo,sec,group);
      contents.array[gid]=new TH2F(buffer,buffer2,maxC+1,0,maxC,100,0,4096);
      contents.array[gid]->GetXaxis()->SetTitle("X=APV*128+chan");
      contents.array[gid]->GetYaxis()->SetTitle("ADC value");
      contents.array[gid]->GetXaxis()->SetNdivisions(10,false);
      contents.array[gid]->SetStats(false);
      //      contents.array[gid]->GetXaxis()->SetTitleOffset(0.9);
      //      contents.array[gid]->GetXaxis()->SetTitleSize(0.06);
      //      contents.array[gid]->GetXaxis()->SetLabelSize(0.06);
      contents.array[gid]->GetYaxis()->SetTitleOffset(1.2);
      //     contents.array[gid]->GetYaxis()->SetTitleSize(0.06);
      //      contents.array[gid]->GetYaxis()->SetLabelSize(0.06);
    }

  for(int iD=0;iD<nTbVsAdc;iD++)
    {
      sprintf(buffer,"ADC_Vs_Tb_Disk_%d",iD+1);
      sprintf(buffer2,"ADC vs. Timebin, Disk:%d",iD+1);
      tbVsAdcContents.tbVsAdcArray[iD]=new TH2F(buffer,buffer2,7,-0.5,6.5,100,0,4096);
      tbVsAdcContents.tbVsAdcArray[iD]->GetXaxis()->SetTitle("Timebin");
      tbVsAdcContents.tbVsAdcArray[iD]->GetYaxis()->SetTitle("ADC value");
      tbVsAdcContents.tbVsAdcArray[iD]->GetXaxis()->SetNdivisions(7,false);
      for(int iB=1;iB<=7;iB++)
	{
	  sprintf(buffer,"TB %d",iB);
	  tbVsAdcContents.tbVsAdcArray[iD]->GetXaxis()->SetBinLabel(iB,buffer);
	}
      tbVsAdcContents.tbVsAdcArray[iD]->SetStats(false);
      //      tbVsAdcContents.tbVsAdcArray[gid]->GetXaxis()->SetTitleOffset(0.9);
      //      tbVsAdcContents.tbVsAdcArray[gid]->GetXaxis()->SetTitleSize(0.06);
      //      tbVsAdcContents.tbVsAdcArray[gid]->GetXaxis()->SetLabelSize(0.06);
      tbVsAdcContents.tbVsAdcArray[iD]->GetYaxis()->SetTitleOffset(1.2);
    }


  hContents.h1=new TH1F("MeanPeds","Mean Pedestal Values",300,minPedVal,maxPedVal);
  hContents.h1->GetXaxis()->SetTitle("Mean Pedestal Value");
  hContents.h1->SetFillColor(kYellow-9);
  hContents.h1->SetStats(false);
  hContents.h2=new TH1F("MeanStdDev","Mean StdDev",100,0,maxRMSVal);
  hContents.h2->GetXaxis()->SetTitle("#sigma");
  hContents.h2->SetFillColor(kYellow-9);
  hContents.h2->SetStats(false);
  hContents.hSumBad=new TH1F("Number of good channels per APV","Number of good channels per APV",240,0,240);
  //  hContents.hSumBad->GetXaxis()->SetNdivisions(19,false);
  hContents.hSumBad->SetFillColor(kYellow-9);
  hContents.hSumBad->SetStats(false);
  hContents.hApvCorpt=new TH1F("APV Corruption","Frequency of visible APVs per event", 241,-0.5,240.5);
  hContents.hApvCorpt->GetXaxis()->SetTitle("number of visible APVs");
  hContents.hApvCorpt->SetFillColor(kYellow-9);
  //  cout <<"seting statis"<<endl;
  hContents.hApvCorpt->SetStats(true);
  hContents.hEventSize=new TH1F("FGT Event Size","FGT Event Size",100,-0.5,6000+0.5);
  hContents.hEventSize->GetXaxis()->SetTitle("Unpacked Event size [kB]");
  hContents.hEventSize->SetFillColor(kYellow-9);
  hContents.hEventSize->SetStats(true);
  //
  //  cout <<"done"  <<endl;



  hSumContents.hSumPed=new TH2F("Pedestal per APV","Pedestal per APV",240,0,240,1497,0,1496);
  hSumContents.hSumPed->SetStats(false);
  hSumContents.hSumSig=new TH2F("Pedestal StdDev per APV","Pedestal StdDev per APV",240,0,240,201,0,200);
  hSumContents.hSumSig->SetStats(false);
  hSumContents.hSumFrac=new TH2F("Fraction in #sigma per APV","Fraction in #sigma per APV",240,0,240,100,0,1);
  hSumContents.hSumFrac->SetStats(false);

  for(int i=0;i<24;i++)
    {
      //bin 0 is underflow
      hContents.hSumBad->GetXaxis()->SetBinLabel(i*10+1,Gid2Label[Indx2Gid[i]].c_str());
      hSumContents.hSumSig->GetXaxis()->SetBinLabel(i*10+1,Gid2Label[Indx2Gid[i]].c_str());
      hSumContents.hSumFrac->GetXaxis()->SetBinLabel(i*10+1,Gid2Label[Indx2Gid[i]].c_str());
      hSumContents.hSumPed->GetXaxis()->SetBinLabel(i*10+1,Gid2Label[Indx2Gid[i]].c_str());

    }


  //  JevpPlot *plots[np+hNp];
  plots=new JevpPlot*[np+hNp+nTbVsAdc+hSNp];

  for(int i=0;i<np;i++)
    {
      contents.array[i]->SetOption("colz");
      plots[i] = new JevpPlot(contents.array[Indx2Gid[i]]);
      //    plots[i] = new JevpPlot(contents.array[i]);
    }


  for(int i=0;i<nTbVsAdc;i++)
    {
      tbVsAdcContents.tbVsAdcArray[i]->SetOption("colz");
      plots[np+i]=new JevpPlot(tbVsAdcContents.tbVsAdcArray[i]);
      plots[np+i]->optlogz=true;
    }

  plots[nTbVsAdc+np]=new JevpPlot(hContents.h1);
  plots[nTbVsAdc+np+1]=new JevpPlot(hContents.h2);
  plots[nTbVsAdc+np+2]=new JevpPlot(hContents.hSumBad);
  plots[nTbVsAdc+np+3]=new JevpPlot(hContents.hApvCorpt);
  plots[nTbVsAdc+np+4]=new JevpPlot(hContents.hEventSize);
  plots[nTbVsAdc+np+3]->logy=true;
  plots[nTbVsAdc+np+3]->setOptStat(10);
  JLine* line=new JLine(0,128,240,128);
  line->SetLineColor(kRed);
  plots[nTbVsAdc+np+2]->addElement(line);

  for(int i=0;i<hSNp;i++)
    {
      hSumContents.sumHArray[i]->SetOption("colz");
      plots[np+nTbVsAdc+hNp+i]=new JevpPlot(hSumContents.sumHArray[i]);
      hSumContents.sumHArray[i]->SetStats(false);
    }
  //  cout <<"adding plots... " <<endl;  
  // Add Plots to plot set...
  for(int i=0;i<np+nTbVsAdc+hNp+hSNp;i++) {
    LOG(DBG, "Adding plot %d",i);
    addPlot(plots[i]);
  }


  // cout <<"2" <<endl;
  //red would be [2]
  errorMsg=new JLatex(.25,.12,"#color[4]{No Error Message}");
  errorMsg->SetTextSize(0.035);
  errorMsg->SetTextAlign(13);
  errorMsg->SetTextAngle(45);
  plots[nTbVsAdc+np]->addElement(errorMsg);

  //    cout <<" done " <<endl;
}

void fgtBuilder::startrun(daqReader *rdr) {
  LOG(NOTE, "fgtBuilder starting run #%d",rdr->run);
  resetAllPlots();

  for(int i=0;i<maxC*maxA;i++)
    {
      meanVals[i]=0;
      aVals[i]=0;
      //      rmsVals[i]=0;
      numVals[i]=0;
      numOverOneSig[i]=0;
      oldStdDevs[i]=0;
      isChannelBad[i]=false;
      runningAvg[i]=0;
      runningStdDevSq[i]=0;
    }
  errorMsg->SetText("No Error Message");    
  sumHistogramsFilled=0;  
  t_2min = time(NULL);
  t_10min = time(NULL);
  t_120min = time(NULL);
}

#define safelog(x) ((x > 0) ? log10(x) : 0)

void fgtBuilder::event(daqReader *rdr)
{
  //default: 7 timebins
  Int_t numTb=7;
  //Jan's request
  memset(chCntDaq,0,sizeof(chCntDaq));
  //  contents.h2_tmp->Fill(tRnd.Rndm(0));
  if(!(evtCt %1000))
    LOG(DBG, "Looking at evt %d",evtCt);
  daq_dta *dd=rdr->det("fgt")->get("adc");

  if(dd && dd->meta)
    {
      Bool_t gotTB=false;
      apv_meta_t *meta = (apv_meta_t *)dd->meta ;
      for(int r=1;r<=FGT_RDO_COU;r++) {
	if(meta->arc[r].present == 0) continue ;
	                                 
	for(int arm=0;arm<FGT_ARM_COU;arm++) {
	  if(meta->arc[r].arm[arm].present == 0) continue ;
	  for(int apv=0;apv<FGT_APV_COU;apv++) {
	    if(meta->arc[r].arm[arm].apv[apv].present == 0) continue ;
	    numTb=meta->arc[r].arm[arm].apv[apv].ntim;
	    gotTB=true;
	    break;//should all be the same, so break after we got it once...
	  }
	  if(gotTB)
	    break;
	}
	if(gotTB)
	  break;
      }

    }
  //check if we have to look for the zs data
  size_t evtSize=0;
  daq_dta *ddZS=rdr->det("fgt")->get("zs");
  if(ddZS)
    {
      while(ddZS && ddZS->iterate())
	{
	  evtSize+=ddZS->ncontent*sizeof(fgt_adc_t);
	}
    }
      //don't use zs data to fill histos...

  while(dd && dd->iterate()) {
    fgt_adc_t *f = (fgt_adc_t *) dd->Void ;
    evtSize+=dd->ncontent*sizeof(fgt_adc_t);
    //    cout<<" adding " << dd->ncontent  <<" * " << sizeof(fgt_adc_t) <<" to evt size : " << dd->ncontent*sizeof(fgt_adc_t) <<" now: " << evtSize <<endl;
    for(u_int i=0;i<dd->ncontent;i++)
      {
	//not zs data

	//corrupted data, should be fine for run 13 as well, these apvs do not exst
	if(dd->pad>21 || dd->pad<0 || dd->pad==10 || dd->pad==11)
	  continue;
	if(dd->rdo<1 || dd->rdo > 2 || dd->sec <0 || dd->sec> 5)
	  continue;
	if(f[i].ch>127 || f[i].ch<0|| f[i].tb>numTb)
	  continue;


	//	if(evtCt <1000)
	//	  printf("FGT ADC: RDO %d, ARM %d, APV %d: %d values\n",dd->rdo,dd->sec,dd->pad,dd->ncontent) ;
	//	dd->rdo;
	//the arm
	//	dd->sec;// two arms per disc
	//	int disc=dd->rdo*3+dd->sec/2;
	///For Jan's request, only use timebin 0 so there is no double counting
	//	cout <<"naj fill : " << dd->rdo<<" sec: " << dd->sec << " pad " << dd->pad <<endl;
	if(f[i].tb==0)
	  {
	    chCntDaq[dd->rdo-1][dd->sec][dd->pad]++;
	  }
	//	cout <<"down  " <<endl;
	//see ben's spreadsheet, first rdo has 10, second 9 assemblies attached
	int gid=(dd->rdo-1)*12+dd->sec*2;
	if(dd->pad>10)
	  gid+=1;
	if(gid>24)
	  cout <<"gid: " << gid <<" to high "<<endl;
	int quad=(dd->rdo-1)*12+dd->sec*2;
	if(dd->pad>10)
	  quad+=1;
	int channel;
	Int_t disk=-1;
	if(gid<25)
	  {
	    disk=Gid2Disk[gid];
	    channel=(dd->pad%12)*128+f[i].ch;
	    //	    if(gid==2 && channel==768)
	    //	      cout <<"before test, rdo: " << dd->rdo <<" pad: " << dd->sec*2 << " channel: " << channel <<  " adc: " << f[i].adc <<endl;
	    if(isChannelBad[gid*maxC+channel])
	      continue;
	    //apvs go 0-9 then 12-...
	    contents.array[gid]->Fill(channel,f[i].adc);
	    //	    rmsVals[gid*maxC+channel]+=(meanVals[gid*maxC+channel]-f[i].adc)*(meanVals[gid*maxC+channel]-f[i].adc);
	    aVals[gid*maxC+channel]+=f[i].adc;
	    numVals[gid*maxC+channel]++;
	    runningAvg[gid*maxC+channel]+=(f[i].adc-runningAvg[gid*maxC+channel])/numVals[gid*maxC+channel];
	    runningStdDevSq[gid*maxC+channel]+=((float)numVals[gid*maxC+channel]-1)/(numVals[gid*maxC+channel])*(f[i].adc-runningAvg[gid*maxC+channel])*(f[i].adc-runningAvg[gid*maxC+channel]);

	    oldStdDevs[gid*maxC+channel]=sqrt(runningStdDevSq[gid*maxC+channel]/numVals[gid*maxC+channel]);
	    //	      cout <<"channel:  " <<gid*maxC+channel <<" stddevsq: " << runningStdDevSq[gid*maxC+channel] <<" oldStddev: " << oldStdDevs[gid*maxC+channel] <<" avg: " << runningAvg[gid*maxC+channel] <<endl;
	    Bool_t isBad=false;

	  if(runningAvg[gid*maxC+channel]<250 || runningAvg[gid*maxC+channel]>maxPedVal)
	    isBad=true;
	  if(oldStdDevs[gid*maxC+channel]<5 || oldStdDevs[gid*maxC+channel]> maxRMSVal)
	    isBad=true;
	  if(!isBad)
	    tbVsAdcContents.tbVsAdcArray[disk-1]->Fill(f[i].tb,f[i].adc);

	  if(f[i].adc-runningAvg[gid*maxC+channel]>oldStdDevs[gid*maxC+channel] && oldStdDevs[gid*maxC+channel]>0)
	      {
		numOverOneSig[gid*maxC+channel]++;
	      }
	    //else
	    //	          cout <<"no" <<endl;
	  }
	else
	  cout <<"quad too large: " << quad <<endl;

	//	cout <<" filling with : " << dd->pad*128+f[i].ch <<" and " << f[i].adc <<endl;
	/*if(dd->pad <= APVEnd)
	  {
	  value=f[i].adc-pedestals[dd->pad-APVStart][f[i].ch];
	  vals[dd->pad-APVStart][f[i].ch]+=value;
	  //timbin
	  f[i].tb;
	  }*/
	//quad should be 0-23
      }
  }
  //Fill Jan's histo
  //	cout <<" fill histo" <<endl;
  //do for zs and non-zs data
  //fill with num kB
  hContents.hEventSize->Fill(evtSize/1024);
  //  cout <<"evt size: " << evtSize <<endl;
  //makes only sense for non zero suppressed data....
  if(dd)
    {

      int goodAPVs=0;
      for(int iRdo=0;iRdo<numRDO;iRdo++)
	{
	  for(int iArm=0;iArm<numARM;iArm++)
	    {
	      for(int iApv=0;iApv<numAPV;iApv++)
		{
		  if(chCntDaq[iRdo][iArm][iApv]>goodChCut)
		    goodAPVs++;
		}
	    }
	}
      hContents.hApvCorpt->Fill(goodAPVs);
      //  cout <<"done" <<endl;
    
      evtCt++;
    }
  // Fill Histograms...
  //  int tpc_size = rdr->getDetectorSize("tpx");
  //  contents.h2_tpc->Fill(safelog(tpc_size));
  
  // Reset rolling histos if necessary..
  int tm = time(NULL);
  if((tm > t_10min + 10) || (!(evtCt%50)))
    {
      t_10min = tm;
      fillSumHistos();
    }

  //  contents.h155_time_size_2min->Fill(tm-t_2min, safelog(sz));
  // End Fill Histograms...
}


//right now all time bins are summed, so what is shown in mean and std dev is the mean over all tb for a channel
void fgtBuilder::fillSumHistos()
{

  //  cout <<"fill..: "<<endl;
  char buffer[200];
  hContents.h1->Reset();
  hContents.h2->Reset();
  hContents.hSumBad->Reset();
  hSumContents.hSumSig->Reset();
  hSumContents.hSumPed->Reset();
  hSumContents.hSumFrac->Reset();

  int numBad=0;
  //    int numTB=5;
  sumHistogramsFilled++;

  //rms actually returns sigma
  for(int gid=0;gid<maxA;gid++){
    for(int iApv=0;iApv<10;iApv++){
      for(int iCh=0;iCh<128;iCh++){
	//	      int index=Gid2Indx[gid]*maxC+iApv*128+iCh;
	int gIndex=gid*maxC+iApv*128+iCh;
	//should not be a memory leak, since histogram exists and should just be refilled...
	//+1 due to underflow bin
	//	      projX=contents.array[gid]->ProjectionY("_px",iApv*128+iCh+1,iApv*128+iCh+1,"o");
	//	      cout <<"getting proj from gid: " << gid <<" channel: " << iApv*128+iCh << endl;
	//	      float mean=projX->GetMean();
	//	      float sig=projX->GetRMS();
	float mean=runningAvg[gIndex];
	float sig=oldStdDevs[gIndex];
	//	      cout <<"mean: " << mean <<" sig: " << sig <<endl;
	hContents.h1->Fill(mean);
	if(sig>0)
	  {
	    hContents.h2->Fill(sig);
	    hSumContents.hSumSig->Fill(Gid2Indx[gid]*10+iApv,sig);
	  }
	hSumContents.hSumPed->Fill(Gid2Indx[gid]*10+iApv,mean);
	//	      meanVals[gIndex]=mean;
	//	      oldStdDevs[gIndex]=sig;
      }}}

  for(int i=0;i<maxC*maxA;i++) 
    {
      int gid=floor(i/maxC);
      int index=Gid2Indx[gid];
      //maxC is 1280
      int apvNr=10*index+floor((i%maxC)/128);
      //	cout <<"apv nr is : " << apvNr << " gid: " << gid <<" i : " << i <<endl;
      //       	cout <<" 10 gid: " << 10*gid <<" i%maxC: " << (i%maxC) <<" add: " << floor((i%maxC)/128) <<" aVals: " << aVals[i] << " num: " << numVals[i] <<endl;
      bool isBad=false;
      if(numVals[i] > 0) 
	{
	  //	    hContents.h1->Fill(aVals[i]/numVals[i]);
	  //	    meanVals[i]=aVals[i]/numVals[i];
	  //	    hSumContents.hSumPed->Fill(apvNr,meanVals[i]);
	  //	    cout <<"filling with " << numOverOneSig[i] <<" / "<< numVals[i]<< "=" <<numOverOneSig[i]/(float)numVals[i]<<endl;
	  //numOverOneSig is filled according to gid
	  hSumContents.hSumFrac->Fill(apvNr,numOverOneSig[i]/(float)numVals[i]);
	  if(runningAvg[i]<250 || runningAvg[i]>maxPedVal)
	    isBad=true;
	}
      if(numVals[i]>1) 
	{
	  //cout <<"numVals: " << numVals[i] <<" rms val: " << rmsVals[i] << " filling with : " << sqrt(rmsVals[i]/(numVals[i]-1)) <<endl;
	  //	    double rms=sqrt(rmsVals[i]/(numVals[i]-1));
	  //	    hContents.h2->Fill(rms);
	  //	    oldStdDevs[i]=rms;
	  //	    cout <<" rms: " << rms <<endl;
	  //	    hSumContents.hSumSig->Fill(apvNr,rms);

	  //filled before from the histos
	  double rms=oldStdDevs[i];
	  if(rms<5 || rms> maxRMSVal)
	    isBad=true;
	  else
	    hContents.hSumBad->Fill(apvNr);
	}

      ///      aVals[i]=0;
      //      rmsVals[i]=0;
      if(isBad)
	numBad++;
    }
  sprintf(buffer,"#color[4]{You seem to have %d bad channels that are not masked}", numBad);  
  errorMsg->SetText(buffer);    
}




void fgtBuilder::stoprun(daqReader *rdr) {
  //  cout <<"stopping run " <<endl;  
  //here I should refill the histograms with the overall statistics (the summary histos)

  //if this is 0 we only have the means, 1 means we have not yet computed the stdDev, greater, equal 2 means that they should be filled
  for(int i=0;i<maxC*maxA;i++) 
    {
      numVals[i]=0;
      numOverOneSig[i]=0;
      runningAvg[i]=0;
      runningStdDevSq[i]=0;
      oldStdDevs[i]=0;
      meanVals[i]=0;
      aVals[i]=0;
      //      rmsVals[i]=0;
      isChannelBad[i]=false;
    }
  //no effect anyways, since stoprun is only called after the histos are drawn
  //  if(sumHistogramsFilled<2)
  //    {
  //      fillSumHistos();
  //    }
}

void fgtBuilder::main(int argc, char *argv[])
{
  fgtBuilder me;
  //  cout <<"starting main" << endl;
  me.Main(argc, argv);
  //  cout <<"ending main" << endl;
}

//const string fgtBuilder::Gid2Label[19]={"1AB","1BC","1CD","1DA","2AB","2BC","2DA","3AB","3BC","3DA","4AB","4BC","4DA","5AB","5BC","5DA","6AB","6BC","6DA"};
///--->old aactie const string fgtBuilder::Gid2Label[19]={"1DA","1AB","2DA","2AB","3AB","3BC","4BC","5DA","6DA","6AB","1BC","1CD","2BC","3DA","4DA","4AB","5AB","5BC","6BC"};
const string fgtBuilder::Gid2Label[24]={"3DA","6AB","2DA","5DA","1BC","3BC","4BC","6DA","3CD","6CD","5CD","5BC","2AB","1CD","3AB","1DA","4DA","2BC", "5AB","6BC","4AB","4CD","2CD","1AB"};
const int fgtBuilder::Indx2Gid[24]={23,4,13,15,12,17,22,2,14,5,8,0,20,6,21,16,18,11,10,3,1,19,9,7};
//const int fgtBuilder::Gid2Indx[24]={3,0,6,4,7,8,11,15,18,16,1,2,5,9,12,10,13,14,17};
const int fgtBuilder::Gid2Indx[24]={11,20,7,19,1,9,13,23,10,22,18,17,4,2,8,3,15,5,16,21,12,14,6,0};
const int fgtBuilder::Gid2Disk[24]={3,6,2,5,1,3,4,6,3,6,5,5,2,1,3,1,4,2,5,6,4,4,2,1};
const int fgtBuilder::maxA=24;
//const int fgtBuilder::maxC=1400;
const int fgtBuilder::maxC=1280;
const int fgtBuilder::maxPedVal=1500;
const int fgtBuilder::maxRMSVal=250;
const int fgtBuilder::minPedVal=0;
const int fgtBuilder::minRMSVal=0;
const int fgtBuilder::numRDO=2;
const int fgtBuilder::numARM=6;
const int fgtBuilder::numAPV=24;
const int fgtBuilder::goodChCut=64;
