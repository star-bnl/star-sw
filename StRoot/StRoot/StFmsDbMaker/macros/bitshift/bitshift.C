static const int Maxdet=4;
static const int Maxchl=578;
static const int Maxchs=288;
static const int Maxitr=20;

void bitshift(int y=2015){
    TFile *f = TFile::Open("DB.root");
    TTree *t = (TTree*)f->Get("tr");
    int year,iter,nstb,chan,bsft,offs,run,hts;
    t->SetBranchAddress("year",&year);
    t->SetBranchAddress("iter",&iter);
    t->SetBranchAddress("nstb",&nstb);
    t->SetBranchAddress("chan",&chan);
    t->SetBranchAddress("bitshift",&bsft);
    t->SetBranchAddress("offset",&offs);
    t->SetBranchAddress("firstRun",&run);
    t->SetBranchAddress("firstRunHTS",&hts);
    
    short bs[Maxitr][Maxdet][Maxchl];
    int nline[Maxitr];
    int runs[Maxitr][2];
    memset(bs,0,sizeof(bs));
    memset(runs,0,sizeof(runs));
    memset(nline,0,sizeof(nline));

    Long64_t n = t->GetEntries();    
    cout << "n="<<n<<endl;
    int maxiter=0;
    for (Long64_t i=0; i<n; i++) {
	t->GetEntry(i);
	if(year==y){
	    if(maxiter<=iter) maxiter=iter;
	    printf("%4d  %2d  %2d  %3d  %3d  %3d %4d %8d %8d\n",
		   i,year,iter,nstb,chan,bsft,offs,run,hts);
	    bs[iter][nstb-1][chan-1]=bsft;
	    runs[iter][0]=run;
	    runs[iter][1]=hts;
	    nline[iter]++;
	}
    }
    printf("found up to iter=%d for run %d \n",maxiter,y);

    char file[100];    
    for(int it=0; it<=maxiter; it++){
	if(nline[it]==0) continue;
	sprintf(file,"fmsBitShiftGain_y%d_i%d_run%d.txt",y,it,runs[it][0]);
	FILE* fp;
	int nn=0, nup=0, ndn=0, nz=0;
	if(fp=fopen(file,"w")){
	    printf("Writing %s\n",file);
	    for(int d=0; d<Maxdet; d++){
		// printf("d=%d Maxdet=%d\n",d,Maxdet);
		int max=Maxchl;
		if(d>=2) max=Maxchs;
		for(int c=0; c<max; c++){
		    //printf("%4d  %2d  %2d  %3d  %3d  %3d\n",
		    //       nn,y,it,d+8,c+1,bs[d][c]);
		    fprintf(fp,"%2d %3d %2d\n",d+8,c+1,bs[it][d][c]);
		    nn++;
		    if(bs[it][d][c]>0) nup++;
		    if(bs[it][d][c]<0) ndn++;
		    if(bs[it][d][c]==0) nz++;
		}
	    }
	    fclose(fp);
	    printf("Closing %s\n",file);
	    printf("N=%d N_zero=%d N_positive=%d N_negative=%d\n",nn,nz,nup,ndn);
	}else{
	    printf("Cannot openg %s\n",file);
	}
    }
}
