// void par_anal(Char_t *xdffilename) 
{
  //  gROOT->Reset();

  gROOT->Reset();
  Bool_t NT = kFALSE;
  if (strcmp(gSystem.GetName(),"WinNT")==0){
    NT = kTRUE;
    if (gSystem.Load("St_base.dll")) printf(" Loading DLL \"St_base.dll\" failed \n");
    if (gSystem.Load("St_Tables.dll")) printf(" Loading DLL \"St_Tables.dll\" failed \n");
  }
  else{
    if (gSystem.Load("libasu.so"))      printf(" Loading DLL \"libasu.so\" failed \n");
    if (gSystem.Load("libdsl.so"))      printf(" Loading DLL \"libdsl.so\" failed \n");
    if (gSystem.Load("St_base.so"))      printf(" Loading DLL \"St_base.so\" failed \n");
    if (gSystem.Load("St_Tables.so"))    printf(" Loading DLL \"St_Tables.so\" failed \n");
  }
  //  Char_t *xdffilename="/star/mds/data/SD98/auau200/evg/central/hijing/set0001/regular/auau_ce_b0-2_1_200.xdf";
   Char_t *xdffilename="/star/sol/users/fisyak/auau_ce_b0-2_4801_5000.xdf";
//   Read XDF file
   printf(" File: \"%s\" \n", xdffilename);
   St_XDFFile  xdf;
   if (xdf.OpenXDF(xdffilename)){
     printf("Can not open file %s \n",xdffilename);
     return;
   }

 // Create "histograms"

   Bool_t drawinit=kFALSE;
   Int_t nxpt = 50;
   Int_t nyetha = 50;
   Float_t xminpt = 0.0;
   Float_t xmaxpt = 5.0;
   Float_t yminetha = -4.0;
   Float_t ymaxetha =  4.0;

   TH2F *h1 = new TH2F("h1","Particle pt",nxpt,xminpt,xmaxpt,nyetha,yminetha,ymaxetha);
   h1->SetXTitle("pt (GeV)");
   h1->SetYTitle("etha (rad) ");
   TH2F *h2 = new TH2F("h2","Particle etha",nxpt,xminpt,xmaxpt,nyetha,yminetha,ymaxetha);
   h2->SetXTitle("pt (GeV)");
   h2->SetYTitle("etha (rad)");
   TH1F *h3 = new TH1F("h3","Particle multiplicity",2,0.5,2.5);

//*-* Create a canvas to show the result (based on  root/tutorials/hsum.C )

   c1 = new TCanvas("c1","Particle pt by reading STAF table: \"particle.h\"",200,10,600,400);
   c2 = new TCanvas("c2","Particle etha by reading STAF table: \"particle.h\"",200,410,600,400);
   c3 = new TCanvas("c3","Particle multiplicity by reading STAF table: \"particle.h\"",800,100,250,250);
   c1->SetGrid();
   c2->SetGrid();
//      c1->Divide(1,2);
   TSlider *slider = 0;

   St_DataSet *event = 0;
   gBenchmark->Start("hsum");
   while (event = xdf.NextEventGet() ) 
   {  
     event = xdf.NextEventGet(); 
     St_DataSetIter root(event);
     St_DataSet *set = root.Cd("/evgen/particle");
     if (set) { 
       set->ls("*");
       printf(" Getting the tables \n");

       St_particle *pa= (St_particle *)set;
       particle_st *particle = pa->GetTable();

     
       Int_t kUPDATE = pa->GetNRows()/5;

       Int_t l = 0;
       for (l=0; l < pa->GetNRows(); l++) 
       {
         particle_st *p = particle+l;
         if (p->isthep == 1) {
            Float_t px = p->phep[0];
            Float_t py = p->phep[1];
            Float_t pz = p->phep[2];
            Double_t pt    =  TMath::Sqrt(px*px+py*py);
//            Double_t theta =  TMath::Atan2 ( pt, pz );
            Double_t theta =  atan2 ( pt, pz );
            Double_t  eta  = -TMath::Log(TMath::Tan(theta/2.));

            if (p->idhep==211) {
                h3->Fill(1);
                h1->Fill(pt,eta);
            }
            else if (TMath::Abs(p->idhep) == 2212) {
                h3->Fill(2);
                if (TMath::Abs(eta) < 20) 
                        h2->Fill(pt,eta);
            }
        }
    // Update the view of these histograms (just for fun)

        if (l && (l%kUPDATE) == 0) {
          if ( l==kUPDATE && !drawinit) {
             drawinit = kTRUE;
             c2->cd();
             h2->Draw(); //"lego");
             c1->cd();
             h1->Draw(); //"lego");
             c3->cd();
             h3->Draw("lego3");
             slider = new TSlider("slider","test",1.05,0,1.1,h1->GetMaximum()*1.3,38);
             slider->SetFillColor(46);
             c1->Update();
             c2->Update();
             c3->Update();
         }
         if (slider) slider->SetRange(0,Float_t(l)/pa->GetNRows());
         c1->Modified();
         c1->Update();
         c2->Modified();
         c2->Update();
         c3->Modified();
         c3->Update();

       }
     }
   }

     // End of Event. Now we can delet it
     if (event) delete event;
     event = 0;
 }
  c1->Modified();
  c2->Modified();
  c2->Modified();
  gBenchmark->Show("hsum");
  printf(" This is a finish \n");
}

