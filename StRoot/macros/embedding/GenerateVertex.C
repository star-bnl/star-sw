// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 11 Dec 2010


class vertexSeed_st;
class St_db_Maker;


void UpdateBeamline(St_db_Maker* db, vertexSeed_st& beamline)
{
   // get beam line constraint
   TDataSet *ds = db->GetInputDB("Calibrations/rhic");

   vertexSeed_st *vs = ds ? ((St_vertexSeed *)ds->FindObject("vertexSeed"))->GetTable() : 0;

   if (!ds || !vs) {
      std::cout << "UpdateBeamline: Data from \"Calibrations/rhic/vertexSeed\" not found\n"
                << "Beamline parameters won't be updated" << std::endl;
      return;
   }

   std::cout << "BeamLine parameters:\n"
             << "x(z) = (" << vs->x0 << " +/- " << vs->err_x0 << ") + (" << vs->dxdz << " +/- " << vs->err_dxdz << ") * z\n"
             << "y(z) = (" << vs->y0 << " +/- " << vs->err_y0 << ") + (" << vs->dydz << " +/- " << vs->err_dydz << ") * z" << std::endl;

   beamline = *vs;
}



/**
 * Generates a text file (vertex.txt) with vertex coordinates (format: vx vy vz)
 * based on the beam line position corresponding to real data file(s) specified
 * by 'daqfile' argument. Vertex uses beam line constraint with spreads in x,
 * y and z, and offset in z, and the distribution in z is base on the
 * zVertexOneTofMatch histogram from the 'dataFileName' file in ROOT format.
 */
void GenerateVertex(const char *dataFileName = 0,
   const TVector3 vtxSpread=TVector3(0.03, 0.03, 50), const double vtxZOffset = 0,
   int nevents = 1000, int seed = 0, const char *daqfile = "@run10179086.list", const char *vertexfile = "vertex.txt", const char *flag = "Jet")
{
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

   vertexSeed_st beamline;// =0;

   // output files
   FILE *VERTEXFILE = fopen(vertexfile, "w");
   assert(VERTEXFILE);

   TString rootfile = vertexfile;
   rootfile.ReplaceAll(".txt", ".root");

   TFile *ofile = TFile::Open(rootfile, "recreate");
   assert(ofile);

   // histograms
   TH1F *hVz  = new TH1F("hVz ", "; vz [cm]",          100,  -50,  50);
   TH2F *hVxy = new TH2F("hVxy", "; vx [cm]; vy [cm]",  50, -2.5, 2.5, 50, -2.5, 2.5);
   TH2F *hVzx = new TH2F("hVzx", "; vz [cm]; vx [cm]", 100,  -50,  50, 50, -2.5, 2.5);
   TH2F *hVzy = new TH2F("hVzy", "; vz [cm]; vy [cm]", 100,  -50,  50, 50, -2.5, 2.5);

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
         printf("run number = %d\n", chain->GetRunNumber());
         printf("time stamp = %s\n", chain->GetDateTime().AsSQLString());
         UpdateBeamline(db, beamline);
      }

      double vz = 0;

      // Set z component based on actual distribution provided by the user
      if (dataFileName)
      {
         tf = new TFile(dataFileName);
         gDirectory->cd("Event");
         vz = zVertexOneTofMatch->GetRandom();
         cout << "Random zVertex Value from Data is: " << vz << endl;
      } else {
         vz = gRandom->Gaus(vtxZOffset, vtxSpread.Z());
      }

      // Smear beamline parameters extracted from DB
      double x0 = gRandom->Gaus(beamline.x0, beamline.err_x0);
      double y0 = gRandom->Gaus(beamline.y0, beamline.err_y0);

      double dxdz = gRandom->Gaus(beamline.dxdz, beamline.err_dxdz);
      double dydz = gRandom->Gaus(beamline.dydz, beamline.err_dydz);

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


/**
 * Specific vertex spreads and z offset for J/psi embedding as found in Chanaka's private area.
 */
void GenerateVertex4Jpsi(char *dataFileName = 0, int nevents = 1000, int seed = 0, const char *daqfile = "@run10179086.list",
   const char *vertexfile = "vertex.txt", const char *flag = "Jet")
{
   const TVector3 vertexSpread(0.055, 0.02, 48.79); // in cm
   const double vertexZOffset = -2.107; // in cm

   GenerateVertex(dataFileName, vertexSpread, vertexZOffset, nevents, seed, daqfile, vertexfile, flag);
}


/**
 * Specific vertex spreads and z offset for W embedding taken from Jinlong's private area.
 */
void GenerateVertex4WBoson(char *dataFileName = 0, int nevents = 1000, int seed = 0, const char *daqfile = "@run10179086.list",
   const char *vertexfile = "vertex.txt", const char *flag = "Jet")
{
   const TVector3 vertexSpread(0.01, 0.01, 5); // in cm
   const double vertexZOffset = 0; // in cm

   GenerateVertex(dataFileName, vertexSpread, vertexZOffset, nevents, seed, daqfile, vertexfile, flag);
}


/**
 * Specific vertex spreads and z offset for VPD.
 */
void GenerateVertex4VPD(char *dataFileName = 0, int nevents = 1000, int seed = 0, const char *daqfile = "@run10179086.list",
   const char *vertexfile = "vertex.txt", const char *flag = "Jet")
{
   const TVector3 vertexSpread(0.0372, 0.015, 33.43); // in cm
   const double vertexZOffset = -1.443; // in cm

   GenerateVertex(dataFileName, vertexSpread, vertexZOffset, nevents, seed, daqfile, vertexfile, flag);
}
