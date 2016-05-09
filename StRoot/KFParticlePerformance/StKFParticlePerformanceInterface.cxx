#include "StKFParticlePerformanceInterface.h"

#include "KFParticleTopoReconstructor.h"
#include "KFTopoPerformance.h"

#include "TFile.h"
#include "TDirectory.h"

ClassImp(StKFParticlePerformanceInterface)

StKFParticlePerformanceInterface::StKFParticlePerformanceInterface(const KFParticleTopoReconstructor* tr, TString outFileName): 
  fOutFileName(outFileName), fOutFile(0), fEfffileName("Efficiency.txt"),fKFTopoPerformance(0), fMCTracks(0), fMCIndexes(0)
{
  fKFTopoPerformance = new KFTopoPerformance();
  fKFTopoPerformance->SetTopoReconstructor(tr);
  
  TFile* curFile = gFile;
  TDirectory* curDirectory = gDirectory;
  
  fOutFileName = "StKFParticleFinderQA.root";
  
  if(!(fOutFileName == ""))
    fOutFile = new TFile(fOutFileName.Data(),"RECREATE");
  else
    fOutFile = gFile;
  fKFTopoPerformance->CreateHistos("KFTopoReconstructor",fOutFile);
  
  gFile = curFile;
  gDirectory = curDirectory;
}

StKFParticlePerformanceInterface::~StKFParticlePerformanceInterface()
{
  if(fKFTopoPerformance)
  {
    TDirectory *curr = gDirectory;
    TFile *currentFile = gFile;
    
    fOutFile->cd();
    WriteHistosCurFile(fKFTopoPerformance->GetHistosDirectory());
    
    if(!(fOutFileName == ""))
    {   
      fOutFile->Close();
      fOutFile->Delete();
    }
    gFile = currentFile;
    gDirectory = curr;
    
    std::fstream eff(fEfffileName.Data(),std::fstream::out);
    eff << fKFTopoPerformance->fParteff;
    eff.close();
  }
  
  if(fKFTopoPerformance) delete fKFTopoPerformance;
}

void StKFParticlePerformanceInterface::PerformanceAnalysis()
{
  if ( fKFTopoPerformance )
  {
    fKFTopoPerformance->SetMCTracks(fMCTracks);
    fKFTopoPerformance->SetTrackMatch(fMCIndexes);
  
    fKFTopoPerformance->CheckMCTracks();
    fKFTopoPerformance->MatchTracks();
    fKFTopoPerformance->FillHistos();
  }
}

void StKFParticlePerformanceInterface::WriteHistosCurFile( TObject *obj ){
  
  if( !obj->IsFolder() ) obj->Write();
  else{
    TDirectory *cur = gDirectory;
    TFile *currentFile = gFile;

    TDirectory *sub = cur->GetDirectory(obj->GetName());
    sub->cd();
    TList *listSub = (static_cast<TDirectory*>(obj))->GetList();
    TIter it(listSub);
    while( TObject *obj1=it() ) WriteHistosCurFile(obj1);
    cur->cd();
    gFile = currentFile;
    gDirectory = cur;
  }
}

void StKFParticlePerformanceInterface::SetPrintEffFrequency(Int_t n)
{ 
  fKFTopoPerformance->SetPrintEffFrequency(n); 
}
