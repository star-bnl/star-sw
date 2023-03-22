#include <iostream>
#include "TMath.h"
#include "TRandom3.h"
#include "TCanvas.h"
#include "TF1.h"
#include "TH1D.h"
#include "TH1D.h"
#include "TGraph.h"
#include "TAxis.h"
#include "TGraph2D.h"
#include "TGraphErrors.h"
#include "TH1F.h"
#include "TFile.h"
#include "TNtuple.h"
#include <numeric>
#include <sstream>

void GetCentrality() {
    TFile *f0 = new TFile("glauberfit.root");
    TH1D *sim = (TH1D *)f0->Get("hRefMultSim");
    TH1D *data = (TH1D *)f0->Get("hRefMultCorr");
    //sim->Scale(1.0/sim->Integral(50,500));
    //data->Scale(1.0/data->Integral(50,500));
    

    int centralitybin[16][2];
    double integral = sim->Integral();
    for(int cent=0; cent<16; cent++){
	double distance = 1000.0; //Initialize the distance from 5% cut to be large
        double fraction = 0.05*((double)cent+1.0);
        //For the most central bins, integrate the data
	if(cent<4){
            for(int i=0; i<data->GetNbinsX(); i++){
                double thisfraction = (data->Integral(i,500))/(integral);
                double thisdistance = TMath::Abs(thisfraction - fraction);//This is how far the current integral
                                                                          // is from the desired fraction.
                if(thisdistance>distance){ //If the distance from the fraction increased, then the previous bin was your desired cut
                    if(cent==0){centralitybin[0][0]=500; centralitybin[0][1]=i-1;centralitybin[1][0]=i-2;}
                    else{centralitybin[cent][1]=i-1;centralitybin[cent+1][0]=i-2;}
                    break;
                }
		else{distance=thisdistance;}
            }
        }
	//For more peripheral bins, integrate the simulated distribution
        else{
            int newmaxbin = centralitybin[3][1]-1;//Integrate the glauber up to this bin
            double zeroToTwentyIntegral = data->Integral(newmaxbin+1,500);
            for(int i=0; i<newmaxbin; i++){
                double thisfraction=(sim->Integral(i,newmaxbin)+zeroToTwentyIntegral)/(integral);
                double thisdistance = TMath::Abs(thisfraction - fraction);
                if(thisdistance>distance){
                    if(cent==15){centralitybin[15][1]=i-1;}
                    else{centralitybin[cent][1]=i-1;centralitybin[cent+1][0]=i-2;}
                    break;
                }
		else{distance=thisdistance;}
            }
        }
    }

    //Print out all useful imformation
    cout<<"******* 16 Bins *******"<<endl;
    cout<<"High bins"<<endl;
    for(int i=0; i<16; i++){
        cout<<data->GetXaxis()->GetBinCenter(centralitybin[i][0])+0.5<<endl;
    }
    cout<<"Low bins"<<endl;
    for(int i=0; i<16; i++){
        cout<<data->GetXaxis()->GetBinCenter(centralitybin[i][1])-0.5<<endl;
    }
    cout<<"Integrals"<<endl;
    for(int i=0; i<16; i++){
        if(i<4)cout<<(data->Integral(centralitybin[i][1],centralitybin[i][0]))/(sim->Integral())<<endl;
    	else cout<<(sim->Integral(centralitybin[i][1],centralitybin[i][0]))/(sim->Integral())<<endl;
    }
    cout<<"Cumulative Integrals"<<endl;
    for(int i=0; i<16; i++){
        if(i<4)cout<<(data->Integral(centralitybin[i][1],500))/(sim->Integral())<<endl;
	else cout<<(sim->Integral(centralitybin[i][1],centralitybin[3][1]-1)+data->Integral(centralitybin[3][1],500))/(sim->Integral())<<endl;
    }
    cout<<"Efficiencies"<<endl;
    for(int i=0; i<16; i++){
        double thiseff = (data->Integral(centralitybin[i][1],centralitybin[i][0]))/(sim->Integral(centralitybin[i][1],centralitybin[i][0]));
        if(thiseff>1.0) thiseff=1.0;
        if(i<4)cout<<1<<endl;
        else cout<<thiseff<<endl;
    }
    /*
    cout<<"<refMultCorr>"<<endl;
    for(int i=0; i<16; i++){
        double thisavg = data->GetBinContent(centralitybin[i][1],centralitybin[i][0]))/(sim->Integral(centralitybin[i][1],centralitybin[i][0]));
        if(thiseff>1.0) thiseff=1.0;
        if(i<4)cout<<1<<endl;
        else cout<<thiseff<<endl;
    }
    */
    cout<<"******* 9 Bins *******"<<endl;
    cout<<"High bins"<<endl;
    for(int i=0; i<9; i++){
        if(i==0 || i==1){cout<<data->GetXaxis()->GetBinCenter(centralitybin[i][0])+0.5<<endl;}
        else{cout<<data->GetXaxis()->GetBinCenter(centralitybin[2*i-1][0])+0.5<<endl;}
    }
    cout<<"Low bins"<<endl;
    for(int i=0; i<9; i++){
        if(i==0 || i==1){cout<<data->GetXaxis()->GetBinCenter(centralitybin[i][1])-0.5<<endl;}
        else{cout<<data->GetXaxis()->GetBinCenter(centralitybin[2*i-1][1])-0.5<<endl;}
    }   
}
