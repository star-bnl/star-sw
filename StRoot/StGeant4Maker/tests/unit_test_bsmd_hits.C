class StBFChain;
class StMessMgr;

#include <string>
#include <TString.h>

// Load sufficient libraries to bootstrap the StBFChain framework
#pragma cling load("libTree.so")
#pragma cling load("StarRoot")
#pragma cling load("St_base")
#pragma cling load("StChain")
#pragma cling load("StUtilities")
#pragma cling load("StBFChain")
#pragma cling load("liblog4cxx")
#pragma cling load("StStarLogger.so")
#pragma cling load("StarClassLibrary.so")
#pragma cling load("libmysqlclient.so")

#if !(defined(__CINT__) || defined(__CLING__)) || defined(__MAKECINT__)

#include "Stiostream.h"
#include "TSystem.h"
#include "TClassTable.h"
#include "TApplication.h"
#include "TInterpreter.h"
#include "StBFChain.h"
#include "StMessMgr.h"
#include "TROOT.h"
#include "TError.h" // Added for gErrorIgnoreLevel
#include "TAttr.h"
#include "Rtypes.h"
#include "TRandom.h"
#include <random>
#include <algorithm>

#endif

#include <vector>
#include <iostream>
#include <fstream>
#include <TTable.h>

#include "g2t_emc_hit.h"
#include "g2t_track.h"

struct Cell {
  int volumeId, softId, m, e, s, d;
  float eta, phi;
};
std::vector<Cell> cells;

struct TestResult {
  int id;
  int hits;
  int matches;
  bool passed;
};

void load_bsmd_cells(const char* filename="bsmd_cells.dat") {
  std::ifstream in(filename);
  if (!in.is_open()) return;
  Cell c;
  while (in >> c.volumeId >> c.softId >> c.m >> c.e >> c.s >> c.d >> c.eta >> c.phi) {
      cells.push_back(c);
  }
  in.close();
}


extern StBFChain* chain;
StBFChain* top = new StBFChain("physicssim");

void unit_test_bsmd_hits(const char* input_file = "bsmd_cells.dat", int jobIndex = 0) {
  
  std::string real_input_file = input_file;
  
  if (real_input_file.length() >= 5 && 
    real_input_file.compare(real_input_file.length() - 5, 5, ".list") == 0) {
    
    std::cout << "input list is " << input_file << std::endl;
    std::ifstream list_stream(input_file);
    
    if (list_stream.is_open()) {
      std::string line;
      if (std::getline(list_stream, line)) {
        size_t first = line.find_first_not_of(" \t\n\r");
        size_t last = line.find_last_not_of(" \t\n\r");
        if (first != std::string::npos && last != std::string::npos) {
          real_input_file = line.substr(first, (last - first + 1));
        }
        std::cout << "yayyy  .dat file: " << real_input_file << std::endl;
      }
      list_stream.close();
    } else {
      std::cerr << "error stubid c++ string :(" << std::endl;
    }
  }

  std::ofstream logFile("bsmd_test_results_job_" + std::to_string(jobIndex) + ".txt");
  if (!logFile.is_open()) return;

  logFile << std::left << std::setw(6)  << "Det"
          << std::left << std::setw(5)  << "Mod"
          << std::left << std::setw(12) << "Strip"
          << std::left << std::setw(12) << "Eta"
          << std::left << std::setw(12) << "Phi"
          << std::left << std::setw(12) << "Expected"
          << std::left << std::setw(12) << "Found"
          << "Status" << std::endl;
  logFile << std::string(100, '-') << std::endl;

  if (cells.empty()) load_bsmd_cells(real_input_file.c_str());
  if (cells.empty()) return;
  std::cout << "Loaded " << cells.size() << " target cells." << std::endl;
  
  std::string chainopts = "noinput nodefault y2021a sdt20201216 agml stargen:mk kinematics:mk g4star:mk geant4out";

  top->SetDebug(0);
  top->SetFlags(chainopts.c_str());
  top->Set_IO_Files(0, Form("bsmd_test_job_%d.geant.root", jobIndex));
  top->Load();
  top->Instantiate();

  auto* pmk  = top->Maker("PrimaryMaker");
  auto* kine = top->Maker("StarKine");
  auto* g4mk = top->Maker("geant4star");

  g4mk->SetAttr("application:engine", "G3"); 
  g4mk->SetAttr("all:engine", "G3"); 
  g4mk->SetAttr("wcal:engine", "G3"); 
  g4mk->SetAttr("hcal:engine", "G3"); 
  
  
  pmk->AddMaker( kine );
  pmk->SetAttr("verbose",111);

  top->ls(5);
  top->Init();
  gSystem->SetFPEMask( kNoneMask );

  int tested = 0;
  int passed = 0;
  int total  = 0;
  int nCells = cells.size();
  std::cout << "\n=== Starting BSMD Geometric Regression Test ===\n" << std::endl;

  double jitter = 0.0;
  TRandom* randGen = new TRandom(0);
  // jitter = randGen->Uniform(0, 0.001);
  
  // TODO: report that first event is trash??
  top->Clear();
  top->Make();

  for (int i=0; i<nCells; i++) {

    const auto& cell = cells[i];      
    total++;

    double eta_s = cell.d == 3 ? jitter : 0.0; //smear only in eta for smde
    double phi_s = cell.d == 4 ? jitter : 0.0; //smear only in phi for smdp
    double pt    = 100.0;
    kine->SetAttr("mode", "FlatPT"); 
    kine->SetAttr("ntrack",  1);
    kine->SetAttr("pid",     6);
    kine->SetAttr("ptlow",   pt);
    kine->SetAttr("pthigh",  pt + 0.00000001);
    kine->SetAttr("etalow",  (double)cell.eta);
    kine->SetAttr("etahigh", (double)cell.eta + 0.00000001 + eta_s);
    kine->SetAttr("philow",  (double)cell.phi); 
    kine->SetAttr("phihigh", (double)cell.phi + 0.00000001 + phi_s);
    pmk->SetAttr("xvertex", 0.0); pmk->SetAttr("yvertex", 0.0); pmk->SetAttr("zvertex", 0.0);
    pmk->SetAttr("xsigma",  0.0); pmk->SetAttr("ysigma",  0.0); pmk->SetAttr("zsigma",  0.0);

    top->Make();

    TDataSet *gds = g4mk->GetDataSet("g2t_smd_hit");
    TTable   *hitTable = (gds) ? (TTable*)gds : nullptr;
    bool matchFound = false;
    bool energyDeposit = false;
    std::vector<int> foundVids;

    if (hitTable) {
      int nHits = hitTable->GetNRows();
      g2t_emc_hit_st* hits = (g2t_emc_hit_st*)hitTable->GetArray();

      for (int j = 0; j < nHits; j++) {
        if (hits[j].track_p != 1) continue;
        
        int currentVid = hits[j].volume_id;
        
        if (currentVid == cell.volumeId) {
          matchFound = true;
          if ( hits[j].de > 0 ) {
            energyDeposit = true;
          }
        }

        int hitLayer = (currentVid / 100) % 10;
        
        bool isEtaHit = (hitLayer == 1 || hitLayer == 2);
        bool isPhiHit = (hitLayer == 3 || hitLayer == 4);
        bool targetIsEta = (cell.d == 3);
        bool targetIsPhi = (cell.d == 4);

        
        bool already_listed = false;
        for(int existing : foundVids) if(existing == currentVid) already_listed = true;
        if(!already_listed) foundVids.push_back(currentVid);
      }
    }
    
    top->Clear();
    kine->Clear(); 

    if (matchFound) passed++;

    char strName[16];
    if (cell.d == 3) snprintf(strName, 16, "S%d", cell.e);
    else             snprintf(strName, 16, "S%d(E%d)", cell.s, cell.e);

    std::stringstream ssFound;
    if (matchFound) {
      ssFound << "MATCH ";
      if ( energyDeposit ) ssFound << "EDEP ";
      for (size_t k = 0; k < foundVids.size(); ++k) {
        // if(k>1){ssFound << "+ others ";break;}
        ssFound << foundVids[k] << (k < foundVids.size()-1 ? " " : "");
      }
    } 
    else if (!foundVids.empty()) {
      ssFound << "MISMATCH: ";
      for (size_t k = 0; k < foundVids.size(); ++k) {
        // if(k>1){ssFound << "+ others ";break;}
        ssFound << foundVids[k] << (k < foundVids.size()-1 ? " " : "");
      }
    } 
    else {
      ssFound << "MISS";
    }

    logFile << std::left << std::setw(6)  << (cell.d==3 ? "SMDE" : "SMDP")
            << std::left << std::setw(5)  << cell.m
            << std::left << std::setw(12) << strName
            << std::left << std::setw(12) << std::fixed << std::setprecision(7) << cell.eta
            << std::left << std::setw(12) << std::fixed << std::setprecision(7) << cell.phi
            << std::left << std::setw(12) << cell.volumeId
            << std::left << std::setw(34) << ssFound.str() // Widen column for list of hits
            << (matchFound ? " PASS" : " FAIL") << std::endl;
  }
  std::cout << "\rDone!                                      " << std::endl;
  
  std::cout << "------------------------------------------------" << std::endl;
  std::cout << "Total Tested: " << total << std::endl;
  std::cout << "Passed:       " << passed << std::endl;
  std::cout << "Failed:       " << (total - passed) << std::endl;
  std::cout << "Success Rate: " << (float)passed/total * 100.0 << "%" << std::endl;
  std::cout << "------------------------------------------------" << std::endl;
  logFile << "------------------------------------------------" << std::endl;
  logFile << "Summary: " << passed << "/" << total << " Passed." << std::endl;
  logFile.close();

  top->Finish();
}