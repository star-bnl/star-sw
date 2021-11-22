static const int mNTRG=21;
static const char* ctrg[mNTRG]={"JP2", "JPA1", "JPA0", "JPBC1", "JPBC0", "JPDE1", "JPDE0",
				"DiJP", "DiJPAsy",
				"DY","JPsi","DYNoEpd","DYAsy",
				"Had2","Had1","Had0",
				"EM2","EM1","EM0",
				"ELE2","EM3"};
static const char* cthr[mNTRG]={"8.0GeV", "6.0GeV", "4.0GeV", "6.0GeV", "4.0GeV", "6.0GeV", "4.0GeV",
				"5.0/5.0GeV", "5.0/4.0GeV",
				"1.0GeV","0.7GeV","1.0GeV","1.0/0.7GeV",
				"6.0GeV","4.0GeV","2.0GeV",
				"5.0GeV","3.5GeV","2.0GeV",
				"1.0GeV","1.0GeV"};
static const char* cthr2[mNTRG]={"7.0GeV", "7.0GeV", "5.0GeV", "7.0GeV", "5.0GeV", "7.0GeV", "5.0GeV",
				 "6.0/6.0GeV", "6.0/5.0GeV",
				 "1.2GeV","0.9GeV","1.2GeV","1.2/0.9GeV",
				 "7.0GeV","5.0GeV","3.0GeV",
				 "7.0GeV","5.0GeV","3.0GeV",
				 "1.2GeV","1.2GeV"};
static const char* cthr3[mNTRG]={"140GeV", "120GeV", "100GeV", "120GeV", "100GeV", "120GeV", "100GeV",
				 "120/120GeV", "120/100GeV",
				 "20GeV","15GeV","20GeV","20/15GeV",
				 "100GeV","80GeV","60GeV",
				 "100GeV","80GeV","60GeV",
				 "20GeV","20GeV"};
static const char* cthr4[mNTRG]={"100GeV", "80GeV", "60GeV", "80GeV", "60GeV", "80GeV", "60GeV",
				 "80/80GeV", "80/60GeV",
				 "10GeV","7GeV","10GeV","10/7GeV",
				 "50GeV","40GeV","30GeV",
				 "50GeV","40GeV","30GeV",
				 "10GeV","10GeV"};
static const char* cthr5[mNTRG]={"120GeV", "40GeV", "30GeV", "40GeV", "30GeV", "40GeV", "30GeV",
				 "40/40GeV", "40/30GeV",
				 "15GeV","12GeV","15GeV","15/12GeV",
				 "70GeV","40GeV","20GeV",
				 "70GeV","40GeV","20GeV",
				 "15GeV","15GeV"};
static const int bw[mNTRG] = {500.0*5/15,
			      500.0/15,500.0/15,
			      500.0*2/15,500.0*2/15,500.0*2/15,500.0*2/15,
			      100,100,
			      1000,66,66,66,
			      66,66,66,
			      133,133,133,
			      1,1};

void rate(){
    TFile* f = new TFile("fcsMc/pythia.jet.vz0/pythia_jet_vz0.qahist.root","old");
    TH1F* h = f->Get("FcsTrgRate");
    float tot=h->GetBinContent(mNTRG+1);
    float bbc=5.0e6;
    int daq=0;
    printf("Total %d events\n",(int)tot);
    printf("TRG      N    Rate[Hz]@5MHzBBC  DaqRate  PS\n");
    for(int i=0; i<mNTRG; i++){
	float n=h->GetBinContent(i+1);
	float rate=n/tot*bbc;
	int ps=rate/bw[i];
	if(ps<1) ps=1;
	daq+=bw[i];
	printf("%8s  %9d  %10.1f %4d  %6d\n",
	       ctrg[i],(int)n,rate,bw[i],ps);
    }
    printf("<table border=1>\n");
    printf("<tr><td>#</td><td>TRG</td><td>E  Thre</td><td>N</td><td>ScalerRate[Hz]@5MHzBBC</td><td>DaqRate[Hz]</td><td>PS</td></tr>\n");
    printf("<tr><td>- </td><td>Total</td><td>-</td><td>%7d</td><td>%7d</td><td>%7d</td><td>-</td></tr>\n",tot,bbc,daq);
    for(int i=0; i<mNTRG; i++){
	float n=h->GetBinContent(i+1);
	float rate=n/tot*bbc;
	int ps=rate/bw[i];
	printf("<tr><td>%2d</td><td>%10s</td><td>%10s</td><td>%7d</td><td>%7d</td><td>%7d</td><td>%7d</td></tr>\n",
	       i,ctrg[i],cthr[i],(int)n,(int)rate,bw[i],ps);
    }
    printf("</table>\n");
}
