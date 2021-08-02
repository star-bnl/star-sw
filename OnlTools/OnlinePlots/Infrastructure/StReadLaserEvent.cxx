#include <math.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <TROOT.h>
#include <TStyle.h>
//#include <TFile.h>
#include <TH1.h>
#include <TH2.h>
#include <TF1.h>
#include <TCanvas.h>
#ifndef NEW_DAQ_READER
#  include <evpReader.hh>
#else
#  include "DAQ_READER/daqReader.h"
#  include "DAQ_TPC/tpcReader.h"
#endif
#include "StReadLaserEvent.h"
#include <time.h>
#include <iostream>
using namespace std;

//TROOT root("TPC", "TPC");

double fgaus(double *x, double *par)
{
    float xx =x[0];
    double f =  par[0] * exp(-(pow((xx - par[1]),2) / (2.*pow(par[2],2))));
    return f;
}

float StReadLaserEvent::Make(int runNumber, int eventNumber, char* datap)
{
    if(nloop<=10) nloop++;
    printf("Laser Event %d\nEvent number: %d\nRun number: %d\n", nloop, eventNumber, runNumber);
    //printf("nloop %d\n", nloop);

    if (nloop < 1)
    { 
      return 0.; 
    }
    if (nloop == 1)
    {
      mVelGauss->Reset();
      SetRunNumber(runNumber);
      SetFirstEvent(eventNumber);
    }
    if (nloop > 10) {
      printf("nloop > 10 called: %d\n",nloop);
      return 0;
    }
    
    if (getRunNumber() != runNumber)
    { printf("resetAll called\n"); resetAll(); }

    int s,r,p,t ;
    u_int timebin;
    u_char val ;
    // loop over sectors, rows, pads
    for(s=0; s<24 ;s++)                 // sector
    {
        int ret = tpcReader(datap,s+1); // needs to be 1..24 for Run 9+
        if(ret < 0)
            continue;           // try another sector
        for(r=44; r<45; r++)            // row
        {
            for(p=68; p<77; p++)        // pad
            {
                for(t=0; t<tpc.counts[r][p]; t++)
                {
                    val = tpc.adc[r][p][t] ;
                    timebin = tpc.timebin[r][p][t];
                    if (8 < val && val < 1024)
                    {
                        mSector[s]->Fill(timebin,val);
                    }
                }
            }
        }
    }


    printf("nloop(laser) = %d\n",nloop);
    fflush(stdout);

    if (nloop == 10)
    {
        //SetFrequencyAndPressure();
        SetLastEvent(eventNumber);
        //get mean drift velocity
        getMeanPosition();
        float vMean = getMeanDriftVelocity();
        //getTruncatedMeanDriftVelocity(vMean);
        //vMean = getDriftVelocityFromGaussFit();
	vMean = getDriftVelocityFromHistMean();
        setVDrift(vMean);
	//if(vMean>5.8 || vMean<5.4)nloop=0;
        //fillDifference(vMean);
        //writeHistograms();

	// jml 
        //resetAll();

        //writeVDrift();
        printf("StReadLaser::vDrift = %10.3f\n   run=%d evt=%d", getVDrift(),runNumber,eventNumber);
	fflush(stdout);
        float rval = getVDrift();
	if(rval<5.4 || rval>5.8) rval = 1972.;//flag as bad value to trigger reset in HistoHandler.cxx
	return rval;				   
    }

    return 0.0;
}

StReadLaserEvent::StReadLaserEvent()
{
    //book histograms
    //printf("booking histograms...\n");
    nloop = 0;
    setVDrift(0.);

    char buf[128];

    for (int i=0; i<24; i++)
    {
        sprintf(buf, "Sector#%d", i+1);
        mSector[i] = new TH1F(buf, buf, 512, 0, 512);
        mSector[i]->SetTitle(buf);
        mSector[i]->SetXTitle("Time Bucket");
        mSector[i]->SetYTitle("Counts");
        mSector[i]->Reset();
    }

    // ADC-value vs time bucket
    mMean = new TH1F("Mean", "Mean", 84, 0, 84);
    mMean->SetTitle("Mean");
    mMean->SetXTitle("Laser#");
    mMean->SetYTitle("Time Bucket");
    mMean->Reset();

    // mean drift velocity
    mVel = new TH1F("Vel", "Vel", 252, 0, 252);
    mVel->Sumw2();
    mVel->SetStats(0);
    mVel->SetTitle("TPC Drift Velocity");
    mVel->SetXTitle("Laser#");
    mVel->SetYTitle("Drift Velocity (cm/#musec)");
    mVel->Reset();
    mVel->SetMarkerStyle(8);
    mVel->SetMarkerSize(1.0);
    mVel->SetMarkerColor(2);
    mVel->SetLineColor(2);

    // deviation from mean drift velocity
    mDeltaVel = new TH1F("DeltaVel", "DeltaVel", 252, 0, 252);
    mDeltaVel->Sumw2();
    mDeltaVel->SetStats(0);
    mDeltaVel->SetTitle("Difference to Mean Drift Velocity");
    mDeltaVel->SetXTitle("Laser#");
    mDeltaVel->SetYTitle("Difference (cm/#musec)");
    mDeltaVel->Reset();
    mDeltaVel->SetMarkerStyle(8);
    mDeltaVel->SetMarkerSize(1.0);
    mDeltaVel->SetMarkerColor(2);
    mDeltaVel->SetLineColor(2);

    // distribution of drift velocity
    mVelGauss = new TH1F("VelGauss", "VelGauss", 200, 4.0, 8.0);
    mVelGauss->Sumw2();
    mVelGauss->SetStats(0);
    mVelGauss->SetTitle("distribution of drift velocity");
    mVelGauss->SetXTitle("drift velocity (cm/#musec)");
    mVelGauss->SetYTitle("counts");
    mVelGauss->Reset();
}

void StReadLaserEvent::getMeanPosition()
{
    // boundaries to get weighted mean
    //int loLim[7] = {30,  80, 130, 180, 230, 270, 320};
    //int hiLim[7] = {60, 110 ,160, 210, 260, 310, 360};
    int loLim[7] = {40,  80, 140, 180, 240, 290, 320};
    int hiLim[7] = {60, 100 ,160, 210, 260, 310, 360};

    // loop over sectors 2,4,6,...,24
    for (int i=1; i<24; i+=2) // add Sector#22,#24 back in
    {
        if (i==9)
            i+=2; // leave out Sector#10
        if (i==15)
            i+=2; // leave out Sector#16
        for (int j=0; j<6; j++) // 7 mirrors seen by each sector, leave out central membrane
        {
            float Sum = 0.;
            float WeightedSum= 0.;
            float Mean = 0.;
            for (int k=loLim[j]; k<hiLim[j]; k++)
            {
                float cen = mSector[i]->GetBinCenter(k);
                float y = mSector[i]->GetBinContent(k);
                WeightedSum += cen*y;
                Sum += y;
                //printf(" %10.3f  %10.3f\n", cen, y);
            }
            Mean = (Sum>1000.) ? WeightedSum/Sum : 0.;
            printf("Sector #%d, Laser#%d, Mean %10.3f\n", i+1, j+1, Mean);

            // get rms

            float Sum2 = 0.;
            float WeightedSum2= 0.;
            float Mean2 = 0.;
            for (int k=loLim[j]; k<hiLim[j]; k++)
            {
                float cen = mSector[i]->GetBinCenter(k);
                float y = mSector[i]->GetBinContent(k);
                WeightedSum2 += pow((cen-Mean),2)*y;
                Sum2 += y;
                //printf(" %10.3f  %10.3f\n", cen, y);
            }
            Mean2 = (Sum2>1000.) ? WeightedSum2/Sum2 : 0.;
            //printf("Sector: %d Peak: %d Mean: %f\n", i+1, j+1, Mean2);
            if (Mean2>2.5)
            {
                Mean = 0.;
            }
            int bin = 7*int(i/1.999) + j + 1;
            mMean->SetBinContent(bin, Mean);
        }
    }
}

float StReadLaserEvent::getMeanDriftVelocity()
{
    //const  float TPC_DELTAT = 0.106574;
    //const  float TPC_DELTAT = 0.108508;
    const  float TPC_DELTAT = 0.106576; //200GeV d+Au

    // laser mirror positions
    float LaserPosition[12][7] =
        { {-179.353, -151.665, -120.698, -90.8549, -59.3999, -32.1487, 0.0},
          {-179.207, -151.674, -120.563, -90.6271, -59.6521, -32.0164, 0.0},
          {-179.089, -151.571, -120.368, -90.553,  -59.5709, -31.9249, 0.0},
          {-179.153, -151.63,  -120.537, -90.6864, -59.6839, -31.9726, 0.0},
          {-179.219, -151.566, -120.598, -90.6391, -59.6167, -32.011,  0.0},
          {-179.198, -151.572, -120.411, -90.5936, -59.4754, -31.951,  0.0},
          { 179.211,  151.567,  120.522,  90.8314,  59.7034,  32.126,  0.0},
          { 179.291,  151.72,   120.616,  90.7793,  59.73,    32.0686, 0.0},
          { 179.079,  151.591,  120.479,  90.5401,  59.5061,  31.8956, 0.0},
          { 179.264,  151.672,  120.632,  90.7614,  59.7441,  32.1511, 0.0},
          { 179.247,  151.597,  120.589,  90.6547,  59.6163,  32.0717, 0.0},
          { 179.182,  151.769,  120.625,  90.7889,  59.7514,  31.9764, 0.0} };

    // get drift velocity
    //float tpc_deltat = 1./getFrequency();
    float tpc_deltat =  TPC_DELTAT;
    if (!(tpc_deltat > 0.))
    { return 0.;}

    printf("deltaT = %10.3f\n", tpc_deltat);

    int index = 0;
    int NoOfEntries = 0;
    float Sum = 0.;
    float eSum = 0.;
    for (int i=0; i<12; i++)
    {
        for (int l=0; l<6; l++)
        {
            for (int m=1; m<7-l; m++)
            {
                index++;
                float dist = fabs(LaserPosition[i][l+m] - LaserPosition[i][l]);
                //printf("dist %f  ", dist);
                int bin = i*7 + l + 1;
                //printf("bin %d  ", bin);
                float t1 = mMean->GetBinContent(bin);
                float t2 = mMean->GetBinContent(bin+m);
                float time = (t1>0.1 && t2>0.1) ? fabs(t2 - t1)*tpc_deltat : 0.;
                //printf("t1 t2 time %f %f %f  ", t1, t2 ,time);
                float v = (time>0.1) ?  dist/time : 0.;
                if (v>0.)
                {
		  //printf("v %f\n", v);
		    printf("Sector #%d, Laser1#%d, Laser2#%d, v %0.4f\n", i+1, l, m, v);
                }
                mVel->SetBinContent(index,v);
                float dv = (time>0.1) ? 0.1/time*v : 0.;
                mVel->SetBinError(index,dv);
                //add to get mean
                if (2.0 < v && v < 10.0)
                {
                    NoOfEntries++;
                    Sum += v;
                    eSum += pow(dv,2);
                    mVelGauss->Fill(v);
                }
            }
        }
    }
    // get mean drift velocity
    float vMean = 0.;
    float eMean = 0.;
    if (NoOfEntries>0)
    {
        vMean =  Sum / NoOfEntries;
        eMean = sqrt(eSum) / NoOfEntries;
        //showVelocity(vMean, eMean);
    }
    else
    {
        vMean = 0.;
        eMean = 0.;
    }

    return vMean;
}

float StReadLaserEvent::getTruncatedMeanDriftVelocity(float vMean)
{
    // get truncated mean
    int NoOfEntries = 0;
    float Sum = 0.;
    float eSum = 0.;
    float vMeanTruncated;
    float eMeanTruncated;
    for (int i=1; i<=252; i++)
    {
        float v = mVel->GetBinContent(i);
        float e = mVel->GetBinError(i);
        if ( fabs(v-vMean) < 0.1 )
        {
            NoOfEntries++;
            Sum += v;
            eSum += pow(e,2);
        }
    }
    if (NoOfEntries>0)
    {
        vMeanTruncated = Sum / NoOfEntries;
        eMeanTruncated = sqrt(eSum) / NoOfEntries;
        //showVelocity(vMeanTruncated, eMeanTruncated);
    }
    else
    {
        vMeanTruncated = 0.;
        eMeanTruncated = 0.;
    }

    return vMeanTruncated;
}

float StReadLaserEvent::getDriftVelocityFromGaussFit()
{
    TF1* f0 = new TF1("f0", "gaus", 5.,6.);
    f0->SetParameter(0,5.);
    f0->SetParameter(1,5.55);
    f0->SetParameter(2,0.03); 

    float vMean = 0.;
    float eMean = 0.;
    if (mVelGauss->GetEntries() == 0)
    {
        printf("...all histograms are empty!\n");
        vMean = 999.;
        eMean = 0.;
    }
    else
    {
      //printf("I am getting drift velocity...\n");
      //fflush(stdout);

      mVelGauss->Print();
        mVelGauss->Fit("f0", "", "Q", 4., 7.);
	vMean = f0->GetParameter(1);
        eMean = f0->GetParError(1);
        if (eMean > 0.3*3.)    // factor 3. : FL on 5/7/2007 
        {
            printf("...could not determine drift velocity!  mean=%f error=%f entries=%d\n",vMean, eMean,mVelGauss->GetEntries() );
            vMean = 1111.;
            eMean = 0.;
        }
        else
        {
            showVelocity(vMean, eMean);
        }
    }
    return vMean;
}

float StReadLaserEvent::getDriftVelocityFromHistMean()
{
    float vMean = 5.55;
    float eMean = 0.;
    if (mVelGauss->GetEntries() == 0)
    {
        printf("...all histograms are empty!\n");
        vMean = 999.;
        eMean = 0.;
    }
    else
    {
      mVelGauss->Print();
      vMean = mVelGauss->GetMean();
      eMean = mVelGauss->GetRMS();
      if (eMean > 0.3*3.)    // factor 3. : FL on 5/7/2007 
        {
	  printf("...could not determine drift velocity!  mean=%f error=%f entries=%d\n",vMean, eMean,mVelGauss->GetEntries() );
	  vMean = 1111.;
	  eMean = 0.;
        }
      else
        {
	  showVelocity(vMean, eMean);
        }
    }
    return vMean;
}

void StReadLaserEvent::fillDifference(float vMean)
{
    for (int i=1; i<=252; i++)
    {
        float v = mVel->GetBinContent(i);
        float e = mVel->GetBinError(i);
        if (2.0 < v && v < 10.0)
        {
            mDeltaVel->SetBinContent(i,v-vMean);
            mDeltaVel->SetBinError(i,e);
        }
    }
}

void StReadLaserEvent::showVelocity(float vMean, float eMean)
{
  printf("\n");
  printf("        +--------------------------------------------------+\n");
  printf("        | Mean drift velocity = (%5.4f +- %5.4f) cm/usec from %d entries|\n", vMean, eMean, mVelGauss->GetEntries());
  printf("        +--------------------------------------------------+\n");
  printf("\n");
  return;
}

//void StReadLaserEvent::SetFrequencyAndPressure()
//{
//
//unsigned int timestamp = time(NULL);

//  StDbManager* mgr = StDbManager::Instance();
//--> mgr->setVerbose(true); //option for printing all SQL statements

//mgr->setQuiet(true);
//--> get structure
//StDbConfigNode* tpc = mgr->initConfig(dbConditions,dbTpc);
//StDbConfigNode* trg = mgr->initConfig(dbConditions,dbTrg);

//StDbTable* gasT = tpc.addDbTable("tpcGas");
//StDbTable* freqT = trg->addDbTable("trgClock");

//mgr->setRequestTime(timestamp);

//bool retVal = ( mgr->fetchDbTable(gasT) && mgr->fetchDbTable(freqT) );

//if(retVal)
//  {
//    tpcGas* gas = (tpcGas*)gasT->GetTable();
//    trgClock* clock = (trgClock*)freqT->GetTable();

//    mPressure = gas->PT_B2;
//    mFrequency = (clock->frequency)/1000000.; // Hz to MHz
//  }

//delete tpc;
//delete trg;

//printf("Frequency: %f\n",getFrequency());
//printf("Pressure: %f\n",getPressure());
//}

void StReadLaserEvent::writeVDrift()
{
    char buf[64];
    sprintf(buf, "/home/operator/online/standalone/vDrift_R%d_E%d_%d.txt", getRunNumber(), getFirstEvent(), getLastEvent());
    FILE* fp = fopen(buf, "w");
    fprintf(fp, "%5.4f\n", getVDrift());
    //fprintf(fp, "%5.4f\n", getFrequency());
    //fprintf(fp, "%5.4f\n", getPressure());
    fclose(fp);
    printf("Wrote to %s\n", buf);

    // write actual drift velicity for l3
    sprintf(buf, "/home/operator/online/standalone/vDrift_online.txt");
    FILE* fp2 = fopen(buf, "w");
    fprintf(fp2, "%5.4f\n", getVDrift());
    //fprintf(fp2, "%5.4f\n", getFrequency());
    //fprintf(fp2, "%5.4f\n", getPressure());
    fclose(fp2);
}

void StReadLaserEvent::resetAll()
{
    nloop = 0;
    setVDrift(0.);
    if (mMean) {
      for (int i=0; i<24; i++) mSector[i]->Reset();
      mMean->Reset();
      mVel->Reset();
      mDeltaVel->Reset();
      mVelGauss->Reset();
      return;
    }

    char buf[128];

    for (int i=0; i<24; i++)
    {
        sprintf(buf, "Sector#%d", i+1);
        mSector[i] = new TH1F(buf, buf, 512, 0, 512);
        mSector[i]->SetTitle(buf);
        mSector[i]->SetXTitle("Time Bucket");
        mSector[i]->SetYTitle("Counts");
        mSector[i]->Reset();
    }

    // ADC-value vs time bucket
    mMean = new TH1F("Mean", "Mean", 84, 0, 84);
    mMean->SetTitle("Mean");
    mMean->SetXTitle("Laser#");
    mMean->SetYTitle("Time Bucket");
    mMean->Reset();

    // mean drift velocity
    mVel = new TH1F("Vel", "Vel", 252, 0, 252);
    mVel->Sumw2();
    mVel->SetStats(0);
    mVel->SetTitle("TPC Drift Velocity");
    mVel->SetXTitle("Laser#");
    mVel->SetYTitle("Drift Velocity (cm/#musec)");
    mVel->Reset();
    mVel->SetMarkerStyle(8);
    mVel->SetMarkerSize(1.0);
    mVel->SetMarkerColor(2);
    mVel->SetLineColor(2);

    // deviation from mean drift velocity
    mDeltaVel = new TH1F("DeltaVel", "DeltaVel", 252, 0, 252);
    mDeltaVel->Sumw2();
    mDeltaVel->SetStats(0);
    mDeltaVel->SetTitle("Difference to Mean Drift Velocity");
    mDeltaVel->SetXTitle("Laser#");
    mDeltaVel->SetYTitle("Difference (cm/#musec)");
    mDeltaVel->Reset();
    mDeltaVel->SetMarkerStyle(8);
    mDeltaVel->SetMarkerSize(1.0);
    mDeltaVel->SetMarkerColor(2);
    mDeltaVel->SetLineColor(2);

    // distribution of drift velocity
    mVelGauss = new TH1F("VelGauss", "VelGauss", 200, 4.0, 8.0);
    mVelGauss->Sumw2();
    mVelGauss->SetStats(0);
    mVelGauss->SetTitle("distribution of drift velocity");
    mVelGauss->SetXTitle("drift velocity (cm/#musec)");
    mVelGauss->SetYTitle("counts");
    mVelGauss->Reset();
    // added by prs

    return;
}





/***************************************************************************
 *
 * $Id: StReadLaserEvent.cxx,v 1.4 2009/03/11 15:43:50 genevb Exp $
 *
 * Author: Frank Laue, laue@bnl.gov
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StReadLaserEvent.cxx,v $
 * Revision 1.4  2009/03/11 15:43:50  genevb
 * Some cleanup of laser codes (halt use of laser hist groups for now)
 *
 * Revision 1.3  2009/03/10 18:51:24  genevb
 * Small mod for new DAQ reader
 *
 * Revision 1.2  2009/02/06 16:22:32  fine
 * Add a few include files to compile OnlinePlots against of the ROOT 5.22
 *
 * Revision 1.1  2009/01/23 16:11:08  jeromel
 * Import from online/RTS/src/
 *
 * Revision 1.13  2008/12/19 17:09:15  fine
 * the first full compilation against of the new DAQ Reader
 *
 * Revision 1.12  2008/12/19 15:51:18  dkettler
 * Added new daqReader
 *
 * Revision 1.11  2008/03/17 22:54:16  fine
 * get rif of the redundant tpc paramater to make it RTS for offline compliant too. Thanks Paul
 *
 * Revision 1.10  2008/02/15 18:51:54  dkettler
 * Updates to laser and TOF reader
 *
 * Revision 1.8  2008/01/07 17:41:23  psoren
 * debug laser hassles
 *
 * Revision 1.7  2008/01/04 18:24:01  psoren
 * start calculation from first event
 *
 * Revision 1.6  2007/12/21 20:34:56  psoren
 * Laser changes from Vasily
 *
 * Revision 1.5  2007/12/13 02:47:01  psoren
 * Made laser calculation more robust and repeatable
 *
 * Revision 1.4  2007/05/25 14:53:44  jml
 * blah
 *
 * Revision 1.3  2007/05/07 18:58:22  laue
 * Added drift time distribution histograms
 *
 * Revision 1.2  2007/04/03 13:19:33  laue
 * Some minor modification on laser histograms by request from Blair
 *
 * Revision 1.1  2007/02/27 15:23:39  laue
 * Initial version
 *
 * Revision 1.1  2006/10/04 20:31:34  laue
 * Initial Version
 *
 *
 ***************************************************************************/

