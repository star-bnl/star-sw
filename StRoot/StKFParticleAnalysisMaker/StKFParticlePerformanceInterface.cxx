#include "StKFParticlePerformanceInterface.h"

#include "KFParticleTopoReconstructor.h"
#include "KFTopoPerformance.h"

#include "TFile.h"
#include "TDirectory.h"

ClassImp(StKFParticlePerformanceInterface)

StKFParticlePerformanceInterface::StKFParticlePerformanceInterface(const KFParticleTopoReconstructor* tr, bool storeMCHistograms, TString outFileName): 
  fOutFileName(outFileName), fOutFile(0), fEfffileName("Efficiency.txt"),fKFTopoPerformance(0), fMCTracks(0), fMCIndexes(0)
{
  fKFTopoPerformance = new KFTopoPerformance();
  fKFTopoPerformance->SetTopoReconstructor(tr);
  
  TFile* curFile = TFile::CurrentFile();
  TDirectory* curDirectory = TDirectory::CurrentDirectory();
  if (fOutFileName == "")  fOutFileName = "StKFParticleFinderQA.root";
  if(! curFile)     fOutFile = new TFile(fOutFileName.Data(),"RECREATE");
  TDirectory *dir = TDirectory::CurrentDirectory();
  if(!storeMCHistograms)
    fKFTopoPerformance->DoNotStoreMCHistograms();
  fKFTopoPerformance->CreateHistos("",dir);
  
  TFile::CurrentFile() = curFile;
  TDirectory::CurrentDirectory() = curDirectory;
}

StKFParticlePerformanceInterface::~StKFParticlePerformanceInterface()
{
  if(fKFTopoPerformance)
  {
#if 1
    TDirectory *curr = TDirectory::CurrentDirectory();
    TFile *currentFile = TFile::CurrentFile();
    if (currentFile && currentFile->IsWritable()) {
      fOutFile->cd();
      if (fKFTopoPerformance->GetHistosDirectory()) {
	WriteHistosCurFile(fKFTopoPerformance->GetHistosDirectory());
      }
      if(!(fOutFileName == ""))
	{   
	  fOutFile->Close();
	  fOutFile->Delete();
	}
    }
    TFile::CurrentFile() = currentFile;
    TDirectory::CurrentDirectory() = curr;
#else
    if (fOutFile && fOutFile->IsWritable()) fOutFile->Write();
#endif
    
    std::fstream eff(fEfffileName.Data(),std::fstream::out);
    eff << fKFTopoPerformance->fParteff;
    eff.close();
  }
  SafeDelete(fKFTopoPerformance);
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
#if 1
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
#endif
void StKFParticlePerformanceInterface::SetPrintEffFrequency(Int_t n)
{ 
  fKFTopoPerformance->SetPrintEffFrequency(n); 
}

void StKFParticlePerformanceInterface::SetCentralityBin(const int iBin)
{ 
  fKFTopoPerformance->SetCentralityBin(iBin); 
}

void StKFParticlePerformanceInterface::SetCentralityWeight(const float weight)
{ 
  fKFTopoPerformance->SetCentralityWeight(weight); 
}

int StKFParticlePerformanceInterface::GetNReconstructedParticles()
{
  return fKFTopoPerformance->GetTopoReconstructor()->GetParticles().size();
}

bool StKFParticlePerformanceInterface::GetParticle(KFParticle& particle, const int iParticle)
{
  if(iParticle<0 || iParticle>=int(fKFTopoPerformance->GetTopoReconstructor()->GetParticles().size()))
  {
    std::cout << "Error!!!    StKFParticlePerformanceInterface::GetParticle()   iParticle " << iParticle << " size " << GetNReconstructedParticles() << std::endl;
    return 0;
  }
  particle = fKFTopoPerformance->GetTopoReconstructor()->GetParticles()[iParticle];
  bool isMatched = fKFTopoPerformance->ParticlesMatch()[iParticle].IsMatchedWithPdg();
  return isMatched;
}
