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
  
  TFile* curFile = TFile::CurrentFile();
  TDirectory* curDirectory = TDirectory::CurrentDirectory();
  
  fOutFileName = "StKFParticleFinderQA.root";
  
  if( !TFile::CurrentFile())
    fOutFile = new TFile(fOutFileName.Data(),"RECREATE");
  else
    fOutFile = TFile::CurrentFile();
  TDirectory *dir = TDirectory::CurrentDirectory();
  fKFTopoPerformance->CreateHistos("",dir);
  
  TFile::CurrentFile() = curFile;
  TDirectory::CurrentDirectory() = curDirectory;
}

StKFParticlePerformanceInterface::~StKFParticlePerformanceInterface()
{
  if(fKFTopoPerformance)
  {
    TDirectory *curr = TDirectory::CurrentDirectory();
    TFile *currentFile = TFile::CurrentFile();
    if (currentFile && currentFile->IsWritable()) {
      fOutFile->cd();
      WriteHistosCurFile(fKFTopoPerformance->GetHistosDirectory());
      
      if(!(fOutFileName == ""))
	{   
	  fOutFile->Close();
	  fOutFile->Delete();
	}
    }
    TFile::CurrentFile() = currentFile;
    TDirectory::CurrentDirectory() = curr;
    
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
    TDirectory *cur = TDirectory::CurrentDirectory();
    TFile *currentFile = TFile::CurrentFile();

    TDirectory *sub = cur->GetDirectory(obj->GetName());
    sub->cd();
    TList *listSub = (static_cast<TDirectory*>(obj))->GetList();
    TIter it(listSub);
    while( TObject *obj1=it() ) WriteHistosCurFile(obj1);
    cur->cd();
    TFile::CurrentFile() = currentFile;
    TDirectory::CurrentDirectory() = cur;
  }
}

void StKFParticlePerformanceInterface::SetPrintEffFrequency(Int_t n)
{ 
  fKFTopoPerformance->SetPrintEffFrequency(n); 
}
