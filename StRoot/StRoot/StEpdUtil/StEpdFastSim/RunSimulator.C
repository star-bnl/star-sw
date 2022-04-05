/*
  So, you've downloaded the StRoot/StPicoEvent and StRoot/StEpdUtil packages onto your laptop, eh?  Great!!
  Now you've got to do a few more things, to make the code useable

  1.  one quick edit on StRoot/StPicoEvent/StPicoEpdHit.cxx, right at the bottom:
      replace LOG_INFO << "EPD hit id: " << mId << " QT data: " << mQTdata << " nMIP: " << mnMIP << endm;
      with    std::cout << "EPD hit id: " << mId << " QT data: " << mQTdata << " nMIP: " << mnMIP << std::endl;

      and at the top:
      replace #include "StPicoMessMgr.h
      with    #include <iostream>

  2. a quick edit to StRoot/StEpdUtil/StEpdFastSim/StEpdFastSim.cxx at the very top.
     You will see an //#include statement to un-comment.  Basically, you need to point directly to StPicoEpdHit.h file

  3. Make a similar edit to StRoot/StEpdUtil/StEpdEpFinder.cxx

  4. go to StRoot/StPicoEvent
     root
     > .L StPicoEpdHit.cxx++
     > .q

  5. go to StRoot/StEpdUtil
     root
     > .L StEpdGeom.cxx++
     > .L StEpdEpInfo.cxx++
     > .L StEpdEpFinder.cxx++  <-- this one will give you lots of warnings about possibly-undefined variables.  Ignore them.
     > .q

  6. go to StRoot/StEpdUtil/StEpdFastSim
     root
     > .L ../StEpdGeom_cxx.so
     > .L StEpdTrivialEventGenerator.cxx++
     > .L StEpdFastSim.cxx++

   Now, steps 4-6 above make the .so files.  These files are loaded (see below) with R__LOAD_LIBRARY commands.
   That's how Root 6 does it.  In Root 5, they use the gSystem->Load() commands, and you'll just have to figure
   it out.  (Or, get with the times and install root6.)

  And, if you want to run it on RCF rather than at home, that's fine, too.  You'll just need to remove the
  R__LOAD_LIBRARY statements and maybe screw around a bit, but it shouldn't be too hard.

   - Mike Lisa lisa.1@osu.edu - 15feb2020 / updated 20march2020
*/

R__LOAD_LIBRARY(./StEpdTrivialEventGenerator_cxx.so)
R__LOAD_LIBRARY(../StEpdGeom_cxx.so)
R__LOAD_LIBRARY(../../StPicoEvent/StPicoEpdHit_cxx.so)
R__LOAD_LIBRARY(./StEpdFastSim_cxx.so)

void RunSimulator(){

  StEpdGeom* eGeom = new StEpdGeom();    // handy later


  // ================================  Step 1  ==============================
  /* =================================================================
     first, generate the particles using the "Trivial Event Generator"
     feel free to replace this with something using UrQMD or AMPT!!!
     =================================================================  */
  //     The Trivial Event Generator takes as input some histograms that
  //     define the dNdeta, v1 and v2 which the program samples
  double etaMin(2.0),etaMax(5.2);
  int nbins(100);
  double dNdetaValue(500.0),v1Value(0.3),v2Value(0.1);
  TH1D* dNdeta = new TH1D("dNdeta","dNdeta",nbins,-etaMax,etaMax);
  TH1D* v1     = new TH1D("v1","v1",nbins,-etaMax,etaMax);
  TH1D* v2     = new TH1D("v2","v2",nbins,-etaMax,etaMax);
  for (int i=1; i<=nbins; i++){
    double eta = dNdeta->GetXaxis()->GetBinCenter(i); 
    if (fabs(eta)>etaMin){
      dNdeta->SetBinContent(i,dNdetaValue);
      double v1Here = (eta<0.0)?-v1Value:v1Value;
      v1->SetBinContent(i,v1Here);
      v2->SetBinContent(i,v2Value);
    }
  }
  // Having now defined the event characteristics, make the generator
  StEpdTrivialEventGenerator* eg = new StEpdTrivialEventGenerator(dNdeta,v1,v2);

  // ================================  Step 2  ===============================
  /* Loop over events
     In this process, the parts are:
     a.  Generate an event with an event generator (Trivial, UrQMD, whatever).  Format is a TClonesArray of momentum TVector3 objects
     b.  If you want the events randomly rotated, or whatever, do it yourself.  That way you control stuff and you have the event plane angle.
     c.  Hand this TClonesArray(TVector3) to the StEpdFastSimulator, which will produce a TClonesArray of StPicoEpdHit objects, just like the real data.
         In this step, you have to tell the simulator where is the primary vertex.  You, the user, control this.  Again, this way you know, and you can do analysis or whatever.
     d.  Now do your regular EPD analysis!  You can hand this TClonesArray(StPicoHit) to the StEpdEpFinder or whatever you want

     As usual, the code below is longer than strictly necessary, since I put in comments and fill histograms, etc.
     Right here is what the code looks like, pared down:
-----
  TRandom3* ran = new TRandom3;
  int Nevents=500;
  StEpdFastSim* efs = new StEpdFastSim(0.2);
  for (int iev=0; iev<Nevents; iev++){
    TClonesArray* momenta = eg->Momenta();
    double RPangle = ran->Uniform(2.0*TMath::Pi());
    for (int trk=0; trk<momenta->GetEntries(); trk++){
      TVector3* mom = (TVector3*)momenta->At(trk);
      mom->RotateZ(RPangle);}
    TVector3 PrimaryVertex(0.0,0.0,0.0);
    TClonesArray* picoHits = efs->GetPicoHits(momenta,PrimaryVertex);
    // now you do something with this data
  }
-----
  */


  // the following histograms are just standard for looking at stuff.  In principle they are optional
  TH2D* EtaPhi = new TH2D("EtaPhi","Eta-Phi from generator",50,-5.5,5.5,50,-4.0,4.0);
  TH1D* nmip   = new TH1D("Nmip","Nmip of all tiles",50,0.0,10.0);
  TH2D* EastWheel = new TH2D("East","East EPD",100,-100.0,100.0,100,-100.0,100.0);
  TH2D* WestWheel = new TH2D("West","West EPD",100,-100.0,100.0,100,-100.0,100.0);
  TH2D* EastWheelADC = new TH2D("EastADC","East EPD - ADC weighted",100,-100.0,100.0,100,-100.0,100.0);
  TH2D* WestWheelADC = new TH2D("WestADC","West EPD - ADC weighted",100,-100.0,100.0,100,-100.0,100.0);
  // end of optional histograms

  TRandom3* ran = new TRandom3;
  int Nevents=500;
  StEpdFastSim* efs = new StEpdFastSim(0.2);    // the argument is WID/MPV for the Landau energy loss.  Use 0.2 for the EPD
  for (int iev=0; iev<Nevents; iev++){
    // a.  Generate event
    TClonesArray* momenta = eg->Momenta();
    if (momenta==0) cout << "No event!!\n";
    for (int itrk=0; itrk<momenta->GetEntries(); itrk++){TVector3* mom = (TVector3*)momenta->At(itrk); EtaPhi->Fill(mom->Eta(),mom->Phi());} // just a histogram
    // b.  Randomly rotate (optional)
    /*
    double RPangle = ran->Uniform(2.0*TMath::Pi());
    for (int trk=0; trk<momenta->GetEntries(); trk++){
      TVector3* mom = (TVector3*)momenta->At(trk);
      mom->RotateZ(RPangle);
    }
    */
    // c.  Run EPD Fast simulator
    TVector3 PrimaryVertex(0.0,0.0,0.0);
    TClonesArray* picoHits = efs->GetPicoHits(momenta,PrimaryVertex);  // and that's it!  you've got the TClonesArray of StPicoHit objects

    /* fill some diagnostic plots */
    for (int i=0; i<picoHits->GetEntries(); i++){        // quick plots
      StPicoEpdHit* ph = (StPicoEpdHit*)picoHits->At(i);
      nmip->Fill(ph->nMIP());
      TVector3 point = eGeom->RandomPointOnTile(ph->id());
      if (ph->id()<0){ // East
	EastWheel->Fill(point.X(),point.Y());
	EastWheelADC->Fill(point.X(),point.Y(),ph->nMIP());
      }
      else{             // West
	WestWheel->Fill(point.X(),point.Y());
	WestWheelADC->Fill(point.X(),point.Y(),ph->nMIP());
      }
    }
    cout << "Event " << iev << " has " << momenta->GetEntries() << " particles,"
    	 << " and the StPicoEpdHit list has " << picoHits->GetEntries() << "entries\n";

    // d. Now do your analysis.  What, you want me to do THAT for you, too?

  }

  TCanvas* tc = new TCanvas("diagnostics","diag",700,1200);
  tc->Divide(2,3);
  tc->cd(1);  EtaPhi->Draw("colz");
  tc->cd(2); nmip->Draw();
  tc->cd(3)->SetLogz(); EastWheel->Draw("colz");
  tc->cd(4)->SetLogz(); WestWheel->Draw("colz");
  tc->cd(5)->SetLogz(); EastWheelADC->Draw("colz");
  tc->cd(6)->SetLogz(); WestWheelADC->Draw("colz");
}


