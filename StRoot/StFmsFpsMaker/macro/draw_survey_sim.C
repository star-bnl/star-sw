// draws boxes representing FMS Pb-glass cells, in units of mm

void draw_survey_sim(int opt=0){
    TFile * infile = new TFile("geotr.root","READ");
    TTree * tr = (TTree*) infile->Get("geotr");
    Int_t nstb,row,col;
    tr->SetBranchAddress("nstb",&nstb);
    tr->SetBranchAddress("row",&row);
    tr->SetBranchAddress("col",&col);
    
    const double xoff[4]={-0.3,   0.3, -0.93, 0.93};
    const double yoff[4]={98.8,  98.8, 46.5,  46.5};
    const double xwid[4]={5.812,5.812,3.822, 3.822};
    const double ywid[4]={5.812,5.812,3.875, 3.875};
    const int color[4]={2,4,6,7};

    Double_t x,y,z,x1,y1,x2,y2;
    Int_t it=0;
    TLine * cell[4][1264];
    
    for(Int_t i=0; i<tr->GetEntries(); i++){
	tr->GetEntry(i);
	//printf("nstb=%2d col=%2d row=%2d\n",nstb,col,row);
	int det=nstb-1;
	switch(nstb){
	case 1: case 3: x = xoff[det] - xwid[det]*(col+0.5); break;
	case 2: case 4: x = xoff[det] + xwid[det]*(col+0.5); break;	
	}
	y = yoff[det] - ywid[det]*(row+0.5);
	x1 = x - xwid[det]/2.0;
	x2 = x + xwid[det]/2.0;
	y1 = y - ywid[det]/2.0;
	y2 = y + ywid[det]/2.0;
	
	cell[0][it] = new TLine(x1,y1,x2,y1); //t
	cell[1][it] = new TLine(x1,y2,x2,y2); //b
	cell[2][it] = new TLine(x1,y1,x1,y2); //l
	cell[3][it] = new TLine(x2,y1,x2,y2); //r
	for(Int_t j=0; j<4; j++) cell[j][it]->SetLineColor(color[det]);
	it++;
    };
    
    if(opt==0){
	Double_t md = 120;
	Int_t factor = 1; // bins per millimeter... set this too high and you'll get a memory leak!
	factor *= 2;
	TH2D * bg = new TH2D("survey","FMS cells survey [mm]",factor*md,-1*md,md,factor*md,-1*md,md);
	TCanvas * survey = new TCanvas("survey","survey",700,700);
	gStyle->SetOptStat(0);
	bg->Draw();
    }
    for(Int_t i=0; i<1264; i++) for(Int_t j=0; j<4; j++) cell[j][i]->Draw();
};
