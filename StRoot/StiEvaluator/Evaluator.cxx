#include "StiEvaluator/Evaluator.h"
#include "TChain.h"
#include "TBranch.h"
#include "TCanvas.h"
#include "Sti/Html/HistoDocument.h"
#include "StiEvaluator/StiEvaluatorHistograms.h"

ClassImp(Evaluator)

Evaluator::Evaluator(const string & name, const string & description)
  : Named(name),
    Described(description)
{}

Evaluator:: ~Evaluator()
{}

void Evaluator::run(const char * inputFile)
{
  string file = inputFile;
  run(file);
}

void Evaluator::run(const string& inputFile)
{
  TChain * chain = new TChain("StMiniMcTree");
  chain->Add(inputFile.c_str());
  chain->SetBranchAddress("StMiniMcEvent",&event);
  TBranch* btree        = chain->GetBranch("StMiniMcEvent");
  int nEvents = (int) chain->GetEntries();
  cout << inputFile << " : " << nEvents << " events" << endl;
  for (int i=0; i<nEvents; ++i) 
    {
      btree->GetEntry(i);
      vector<StiEvaluatorHistograms*>::iterator iter;
      for (iter=_histograms.begin();iter!=_histograms.end();++iter)
	{
	  (*iter)->fill(event);
	}
    }
  delete btree;
  delete chain;
}

void Evaluator::save(const char * inputFile)
{
  string file = inputFile;
  save(file);
}

void Evaluator::save(const string &targetDirectory)
{
   vector<StiEvaluatorHistograms*>::iterator iter;
   for (iter=_histograms.begin();iter!=_histograms.end();++iter)
     {
       (*iter)->finish();
       (*iter)->write(targetDirectory+"/"+(*iter)->getName()+".root");
     }
}

void Evaluator::saveHtml(const char * inputFile)
{
  string file = inputFile;
  saveHtml(file);
}

void Evaluator::saveHtml(const string &targetDirectory)
{
   vector<StiEvaluatorHistograms*>::iterator iter;
   TCanvas * canvas = new TCanvas();
   for (iter=_histograms.begin();iter!=_histograms.end();++iter)
     {
       HistoDocument histoDocumentRec(targetDirectory,
				      (*iter)->getName(),
				      (*iter)->getDescription(),
				      canvas);
       histoDocumentRec.generateWebPage(*iter);  
     }
  delete canvas;
}

StiEvaluatorHistograms * Evaluator::add(StiEvaluatorHistograms *h)
{
  _histograms.push_back(h);
  return h;
}
