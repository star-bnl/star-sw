// $Id: LaserStraightTrackTable.C,v 1.2 1999/05/21 15:33:50 kathy Exp $
// $Log: LaserStraightTrackTable.C,v $
// Revision 1.2  1999/05/21 15:33:50  kathy
// made sure Log & Id are in each file and also put in standard comment line with name of owner
//
//=======================================================================
// owner: Bill Love
// what it does: a macro to collect data from a laser TTree into a
//               nice table of laser properties.
//=======================================================================
/*LaserStraightTrackTable.C - a macro to collect data from a laser TTree into a
  nice table of laser properties. */

void LaserStraightTrackTable(const Char_t * filename="stktreeR2805.root"){
Int_t  numb[6][7];
Float_t axs[6][7], azs[6][7], x0s[6][7], z0s[6][7];
Float_t eaxs[6][7], eazs[6][7], ex0s[6][7], ez0s[6][7];
Float_t zmin[6] = {28, 58, 89, 119, 150, 178};
Float_t zmax[6] = {32, 60, 91, 121, 152, 180};
Float_t axmin[7][6] = {0.75, 1.75, 0.8, 1.67, 0.8, 1.9,
                       .37, .95, .45, .89, .45, .96,
                       .31, .19, .30, .25, .32, .29,
                      -.01, -.01, -.01, -.01, -.01, 0.0,
                      -.32, -.23, -.32, -.3, -.35, -.23,
                      -.48, -.5, -.48, -.45, -.48, -.44,
                      -.85, -1.2, -.85, -1.25, -.8, -1.2 }; 
Float_t axmax[7][6] = {0.85, 2.1, 0.85, 2.0, 0.85, 2.1,
                       .46, 1.05, .5, .99, .48, .99,
                       .35, .30, .37, .31, .37, .30,
                        .015, .02,  .01, .05,  .01, .02,
                       -.29, -.21, -.30, -.22, -.25, -.22,
                       -.45, -.4, -.42, -.4, -.35, -.43,
                       -.75, -1.0, -.75, -1.0, -.7, -1.0 };
TString file("/disk1/star/laser/");
file += filename;
 cout << "Read file "<< file << endl;
 TFile f(file->Data());
 TTree *stks = (TTree*)f.Get("stks"); //Req'd by standard C++
Int_t entr = stks->GetEntries();
Int_t i,m;
char selection[200];
stks->SetEstimate(entr);
cout <<" mirror bundle  n        px      +-        x0      +-        pz      +-        z0      +-" << endl;
i=0;
for (i=0; i<6; i++){
  Float_t avgx0 = 0; Float_t avgz0 = 0;
  for (m=0;m<7;m++){
     sprintf(selection,"fTracks.fBz> %f && fTracks.fBz< %f && (fTracks.fPx/fTracks.fPy)> %f && (fTracks.fPx/fTracks.fPy)< %f", 
              zmin[i], zmax[i], axmin[m][i], axmax[m][i]);
     //          cout << selection << endl;
     stks->Draw("fTracks.fPx>>h1",selection,"goff");
     TH1F *h1 = (TH1F*)f.Get("h1");
     //cout << "h1 made" << endl;
     numb[i][m] = h1->GetEntries();
     axs[i][m] = h1->GetMean(1);
     eaxs[i][m] = h1->GetRMS(1);
     delete h1;
     //      cout << "h1 deleted" << endl; 
     stks->Draw("fTracks.fBx>>h2",selection,"goff");
     TH1F *h2 = (TH1F*)f.Get("h2");
     x0s[i][m] = h2->GetMean(1);
     ex0s[i][m] = h2->GetRMS(1);
     avgx0 += x0s[i][m];
     delete h2;
     stks->Draw("fTracks.fPz>>h3",selection,"goff");
     TH1F *h3 = (TH1F*)f.Get("h3");
     azs[i][m] = h3->GetMean(1);
     eazs[i][m] = h3->GetRMS(1);
     delete h3;
     stks->Draw("fTracks.fBz>>h4",selection,"goff");
     TH1F *h4 = (TH1F*)f.Get("h4");
     z0s[i][m] = h4->GetMean(1);
     ez0s[i][m] = h4->GetRMS(1);
     avgz0 += z0s[i][m];
     delete h4;

     printf("%5d %5d %6d %10.4f %7.4f %10.4f %7.4f %8.4f %7.4f %10.4f %7.4f \n",
            m+1, 6-i, numb[i][m], axs[i][m], eaxs[i][m],
            x0s[i][m], ex0s[i][m], azs[i][m], eazs[i][m], 
            z0s[i][m], ez0s[i][m]);  }
     avgx0 /= 7.0; avgz0 /= 7.0;
     printf("                         mean x0 %15.4f             mean z0 %15.4f \n",
     avgx0,avgz0);
cout << endl;
}
}
