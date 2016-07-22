// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 11 Dec 2010


/**
 * Generates a text file (vertex.txt) with vertex coordinates (format: vx vy vz)
 * based on the beam line position corresponding to real data file(s) specified
 * by 'daqfile' argument. Vertex uses beam line constraint with spreads in x,
 * y and z, and offset in z, and the distribution in z is base on the
 * zVertexOneTofMatch histogram from the 'dataFileName' file in ROOT format.
 */
void GenerateVertex(char *dataFileName,
   const TVector3 vtxSpread=TVector3(0.03, 0.03, 50), const double vtxZOffset = 0,
   int nevents = 1000, int seed = 0, const char *daqfile = "@run10179086.list", const char *vertexfile = "vertex.txt", const char *flag = "Jet")
{
   // vertex spreads and offsets
   const double xsigma = 0.055;	    // cm
   const double ysigma = 0.02;       // cm
   //const double zsigma = 48.79;    // cm
   //const double zoffset = -2.107;  // cm

   /* ====W embedding=======
   const double xsigma = 0.0150; // cm
   const double ysigma = 0.0150; // cm
   const double zsigma = 42.0;   // cm
   const double zoffset = 0.0;   // cm
   */

   /* ==== VPDMB ====
   const double xsigma = 0.0372;	// cm
   const double ysigma = 0.0150;	// cm
   const double zsigma = 33.43;	// cm
   const double zoffset = -1.443; // cm
   */

   // load generic STAR libraries
   gSystem->Load("St_base");
   gSystem->Load("StChain");
   gSystem->Load("StBFChain");
   gSystem->Load("StUtilities");
   gSystem->Load("StIOMaker");
   gSystem->Load("StarClassLibrary");

   // load STAR db-related libraries
   gSystem->Load("St_Tables");
   gSystem->Load("StDbLib");
   gSystem->Load("StDbBroker");
   gSystem->Load("St_db_Maker");

   // create chain
   StChain *chain = new StChain;

   // Read DAQ file to get time stamp
   StIOMaker *io = new StIOMaker;
   io->SetFile(daqfile);
   io->SetIOMode("r");
   io->SetBranch("*", 0, "0");	// deactivate all branches

   // create db object
   St_db_Maker *db = new St_db_Maker("db", "MySQL:StarDb", "$STAR/StarDb");

   // initialize chain and read first event to get time stamp
   chain->Init();

   // some variables to remember
   double x0 = 0;
   double y0 = 0;
   double dxdz = 0;
   double dydz = 0;

   // output files
   FILE *VERTEXFILE = fopen(vertexfile, "w");
   assert(VERTEXFILE);

   TString rootfile = vertexfile;
   rootfile.ReplaceAll(".txt", ".root");

   TFile *ofile = TFile::Open(rootfile, "recreate");
   assert(ofile);

   // histograms
   TH1F *hVz  = new TH1F("hVz ", ";vz [cm]"        , 500, -250, 250);
   TH2F *hVxy = new TH2F("hVxy", ";vx [cm];vy [cm]", 150, 0.0, 0.6, 160, 0.0, 0.2);
   TH2F *hVzx = new TH2F("hVzx", ";vz [cm];vx [cm]", 500, -250, 250, 150, 0.0, 0.6);
   TH2F *hVzy = new TH2F("hVzy", ";vz [cm];vy [cm]", 500, -250, 250, 160, 0.0, 0.2);

   // Randomize
   gRandom->SetSeed(seed);

   // event loop
   for (int iEvent = 1; iEvent <= nevents; ++iEvent) {
      chain->Clear();
      int iMake = chain->Make(iEvent);

      if (iMake == kStSkip) continue;

      if (flag == "Jet" && iMake % 10 == kStEOF) io->Rewind();

      if (flag == "Jet" && iMake % 10 == kStFatal) break;

      if (flag == "W" && iMake == kStEOF) break;

      // new run number?
      if (chain->GetEvtHddr()->IsNewRun()) {
         // get beam line constraint
         TDataSet *ds = db->GetInputDB("Calibrations/rhic");

         if (ds) {
            vertexSeed_st *vs = ((St_vertexSeed *)ds->FindObject("vertexSeed"))->GetTable();

            if (vs) {
               printf("run number = %d\n", chain->GetRunNumber());
               printf("time stamp = %s\n", chain->GetDateTime().AsSQLString());
               puts("beamline constraint:");
               printf("x(z) = %f + %f * z\n", vs->x0, vs->dxdz);
               printf("y(z) = %f + %f * z\n", vs->y0, vs->dydz);

               // save beam line constraint
               x0 = vs->x0;
               y0 = vs->y0;
               dxdz = vs->dxdz;
               dydz = vs->dydz;
            }
         }
      }

      tf = new TFile(dataFileName);

      gDirectory->cd("Event");

      cout << "Random zVertex Value from Data is: " << zVertexOneTofMatch->GetRandom() << endl;

      //double vz = gRandom->Gaus(vtxZOffset, vtxSpread.Z());

      double vz = zVertexOneTofMatch->GetRandom();
      double vx = gRandom->Gaus(x0 + dxdz * vz, vtxSpread.X() );
      double vy = gRandom->Gaus(y0 + dydz * vz, vtxSpread.Y() );

      fprintf(VERTEXFILE, "%14f %14f %14f\n", vx, vy, vz);

      hVz->Fill(vz);
      hVxy->Fill(vx, vy);
      hVzx->Fill(vz, vx);
      hVzy->Fill(vz, vy);
   } // end event loop

   // close vertex file
   fclose(VERTEXFILE);

   // write histograms and close ROOT file
   ofile->Write();
   ofile->Close();
}
