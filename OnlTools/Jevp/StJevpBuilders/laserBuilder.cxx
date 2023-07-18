//
#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"
#include "DAQ_READER/daq_dta.h"
#include "DAQ_READER/daq_det.h"
#include "Jevp/StJevpPlot/RunStatus.h"
#include "StEvent/StTriggerData.h"
#include <TH1I.h>
#include <TH2F.h>

#include <math.h>
#include "laserBuilder.h"
#include <RTS/include/rtsLog.h>
#include "LaserReader.h"


// LASER Jevp builder
//
//  Author: F.Videbaek   / JML
//     Cut the LASER processing out fo the tpcBuilder
// 
// The raw row number from the TPX always has 27 added befire using in any plots
// that involves
// Compared to old plots:
//      added cluster widtsh for both TPX and iTPC
//      have seperate plots for itpc and tpx
//      Some summary plots do have the sums for itpc and tpx
//      Legends updated
//
//

#define checkcld 1
#define checklaser 1
#define fv 0
#define fvd 0

#define safelog(x) ((x > 0) ? log10(x) : 0)

ClassImp(laserBuilder);
  
//
// 
static const int NTPCpads[72] = {
    52, 54, 56, 58, 60, 62, 62, 64, 66, 68,
    70, 72, 74, 74, 76, 78, 80, 82, 84, 86,
    86, 88, 90, 92, 94, 96, 98, 98,100,102,
    104,106,108,110,110,112,114,116,118,120,
    98,100,102,104,106,106,108,110,112,112,114,116,118,120,122,122, //Outer 
    124,126,128,128,130,132,134,136,138,138,140,142,144,144,144,144
};

static const int Nrows = 72;
static const int Npads = 144;
static const int Nrows1 = Nrows+1;
static const int Npads1 = Npads+1;

laserBuilder::laserBuilder(JevpServer *parent) : JevpBuilder(parent) {
    plotsetname = (char *)"laser";

    // start with histograms undefined...
    memset(&contents, 0, sizeof(contents));

    setPhiAngleMap();
    laserReader = new LaserReader();
}

laserBuilder::~laserBuilder() {
    // Delete any existing histograms...
    int n = sizeof(contents) / sizeof(TH1 *);
    for(int i=0;i<n;i++) {
	if(contents.array[i]) delete contents.array[i];
    }

    delete laserReader;
}

void laserBuilder::initialize(int argc, char *argv[]) {
    contents.h_tpc_drift_vel = new TH1D("h_tpc_drift_vel", "TPC Drift Velocity (cm/us)",400,5.0,5.8);
 
    PCP;
    // Add root histograms to Plots
    int np = sizeof(contents) / sizeof(TH1 *);
    JevpPlot *plots[np];

    PCP;
    int n=0;
    plots[n] = new JevpPlot(contents.h_tpc_drift_vel);

 
    PCP;
    // Add Plots to plot set...
    for(int i=0;i<=n;i++) {
	addPlot(plots[i]);
    } 
    PCP;
}
  
void laserBuilder::startrun(daqReader *rdr) {
  LOG("JEFF", "laserBuilder starting run #%d",rdr->run);
  resetAllPlots();
  laserReader->resetAll();
  n_cld = 0;
  n_adc = 0;
  nlasers = 0;
  drift_vel = 0;
  event_no=0;
  run = rdr->run;

  tpcDataInThisRun=0;
}

#define safelog(x) ((x > 0) ? log10(x) : 0)

void laserBuilder::event(daqReader *rdr)
{
    //    // Get bunch crossing from trigger data..
    //StTriggerData *trgd = getStTriggerData(rdr);
    //if(trgd) {
    //	bunch7bit = trgd->bunchId7Bit();
    //	delete trgd;
    //}
    switch(rdr->trgcmd) {
      
    case 8:  // Lasers...
    case 9:
	{
      	    double vDrift = laserReader->Make(rdr);
     
	    LOG("JEFF","Laser Event Processed: run=%d evt=%d vDrift=%lf total_tpc_evts=%d",run, rdr->event_number, vDrift, numberOfEventsRun);

	    if((vDrift > 5.0) && (vDrift < 5.8)) {
		nlasers++;
		contents.h_tpc_drift_vel->Fill(vDrift);
	    }

	    drift_vel = contents.h_tpc_drift_vel->GetMean();

	    if(1) {    // inneficient!  write all of them :-)
		FILE *f = fopen("/RTS/conf/handler/.l4_drift_velocity","w");
		if(f) {
		    fprintf(f, "%lf", drift_vel);
		    fclose(f);
		}
		else {
		    LOG(OPER, "Can't access drift velocity file!");
		}
		
		f = fopen("/RTS/conf/handler/.l4_drift_velocity_run","w");
		if(f) {
		    fprintf(f, "%d", run);
		    fclose(f);
		}
		else {
		    LOG(OPER, "Can't access drift velocity run number file!");
		}
	  
		LOG("JEFF", "Wrote laser to .l4_drift_velocity file %lf %d", drift_vel, run);
	    }
	}
	break;
    }
}

void laserBuilder::setPhiAngleMap()
{
    const Int_t NP = 72; // # padrows
    const Float_t DEG = 57.296;//360/2pi

    Double_t Xpads[NP] = { 
	55.8, 57.4, 59, 60.6, 62.2, 63.8, 65.4, 67, 68.6, 70.2, 
	71.8, 73.4, 75, 76.6, 78.2, 79.8, 81.4, 83, 84.6, 86.2, 
	87.8, 89.4, 91, 92.6, 94.2, 95.8, 97.4, 99, 100.6, 102.2, 
	103.8, 105.4, 107, 108.6, 110.2, 111.8, 113.4, 115, 116.6, 118.2,
	127.195, 129.195, 131.195, 133.195, 135.195, //Outer
	137.195, 139.195, 141.195, 143.195, 145.195,
	147.195, 149.195, 151.195, 153.195, 155.195,
	157.195, 159.195, 161.195, 163.195, 165.195,
	167.195, 169.195, 171.195, 173.195, 175.195,
	177.195, 179.195, 181.195, 183.195, 185.195,
	187.195, 189.195
    };   
 

 
    Double_t pitches[2] = {0.5, 0.67};


    //Note from GENE
    //So within any supersector, I have a local X and Y.  X you can get from
    //Xpads above, below I have YMIN (the lower Y coordinate of each pad;
    //you would need to add half the "pitch" to get the Y center of each pad)

    float YMIN;
    float pitch;
    float phi0=60;
    float LPhi;//local phi
    float SPhi;//sector phi
    float GPhi;//global phi

    //loop over sectors and find SPhi= phi in middle of sector
    for (int sec = 0; sec < 24; sec++) {
	if (sec<12) {
	    SPhi = phi0 - (sec*30);
	    if (SPhi<-180) SPhi+=360;
	}
	if (sec>=12) {
	    SPhi = phi0 + ((sec-10)*30);
	    if (SPhi>180) SPhi-=360;
	} 
   
	//loop over each padrow in a sector
	for (int j=0; j<NP; j++) {
	    pitch = pitches[0];
	    if (j >= 40) pitch = pitches[1];
	    for (int k=0; k<NTPCpads[j]; k++) {//loop over # pads in each padrow
		YMIN = pitch * (k - 0.5*NTPCpads[j]);//find Y at bottom of padrow
		LPhi=atan(YMIN/Xpads[j]);//find local Phi (LPhi) within sector
		LPhi*=DEG;
		GPhi=LPhi+SPhi;//find global Phi (GPhi) 
	  
		//oth->fill( 	 h1,LPhi);
		//oth->fill(          h2,Xpads[j],YMIN);
		//oth->fill( 	 h3,Xpads[j],LPhi);
		//oth->fill( 	 h4,YMIN,LPhi);
		//oth->fill( 	 h5,GPhi);
		//oth->fill( 	 h6,sec,SPhi);
	  
		//Fill Look up table for pad phi angle
	  
		if(sec<12)
		    if(GPhi<-180) GPhi += 360;
	  
	  
		if(sec>=12)
		    if(GPhi>180)
			GPhi +=-360.;
	  
		mPhiAngleMap[sec][j][k]=GPhi;
	  
	  
	    }//pad
	}//padrow
    }// sector
}

void laserBuilder::main(int argc, char *argv[])
{
  laserBuilder me;
  me.Main(argc, argv);
}

