#include "StiEvaluator/Evaluator.h"
#include "TChain.h"
#include "TBranch.h"
#include "TCanvas.h"
#include "TStyle.h"
#include "TROOT.h"
#include "Sti/Html/HistoDocument.h"
#include "StiEvaluator/StiEvaluatorHistograms.h"

ClassImp(Evaluator)

Evaluator::Evaluator(const string & name, const string & description)
  : Named(name),
    Described(description)
{}

Evaluator:: ~Evaluator()
{}


void Evaluator::run(const char* inputFile)
{
  StMiniMcEvent * mEvent=0;

  TChain * chain = new TChain("StMiniMcTree");
  chain->Add(inputFile);
  int nEvents = (int) chain->GetEntries();
  cout << inputFile << " : " << nEvents << " events" << endl;

  chain->SetBranchAddress("StMiniMcEvent",&mEvent);
  TBranch* btree        = chain->GetBranch("StMiniMcEvent");
  

  for (int i=0; i<nEvents; ++i) 
    {
      cout <<"Event: "<<i<<endl;
      btree->GetEntry(i);
      vector<StiEvaluatorHistograms*>::iterator iter;
      for (iter=_histograms.begin();iter!=_histograms.end();++iter)
	{
	  (*iter)->fill(mEvent);
	}
    }

  chain->Reset();
  //delete btree;
  //delete chain;
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

void Evaluator::saveHtml(const char * targetDirectory)
{
  string td = targetDirectory;
  saveHtml(td);
}

void Evaluator::saveHtml(const string &targetDirectory)
{
   vector<StiEvaluatorHistograms*>::iterator iter;
   TCanvas * canvas = new TCanvas();
   gStyle->SetOptStat(0);
   //  gROOT->ForceStyle();
   gStyle->SetOptStat(000000);
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

void Evaluator::saveHtml(const char * targetDirectory,
			 const char * fileName,
			 const char * description,
			 StiEvaluatorHistograms* hg1,
			 StiEvaluatorHistograms* hg2)
{
  string td = targetDirectory;
  string fn = fileName;
  string des= description;
  saveHtml(td,fn,des,hg1,hg2);
}

void Evaluator::saveHtml(const char * targetDirectory,
			 const char * fileName,
			 const char * description,
			 StiEvaluatorHistograms* hg1,
			 StiEvaluatorHistograms* hg2,
			 StiEvaluatorHistograms* hg3)
{
  string td = targetDirectory;
  string fn = fileName;
  string des= description;
  saveHtml(td,fn,des,hg1,hg2,hg3);
}

void Evaluator::saveHtml(const string &targetDirectory,
			 const string &fileName,
			 const string &description,
			 StiEvaluatorHistograms* hg1,
			 StiEvaluatorHistograms* hg2)
{
//VP   vector<StiEvaluatorHistograms*>::iterator iter;
   TCanvas * canvas = new TCanvas();
   HistoDocument histoDocument(targetDirectory,fileName,description,canvas);
   histoDocument.generateWebPage(hg1,hg2);
   delete canvas;
}

void Evaluator::saveHtml(const string &targetDirectory,
			 const string &fileName,
			 const string &description,
			 StiEvaluatorHistograms* hg1,
			 StiEvaluatorHistograms* hg2,
			 StiEvaluatorHistograms* hg3)
{
//VP   vector<StiEvaluatorHistograms*>::iterator iter;
   TCanvas * canvas = new TCanvas();
   HistoDocument histoDocument(targetDirectory,fileName,description,canvas);
   histoDocument.generateWebPage(hg1,hg2,hg3);
   delete canvas;
}



StiEvaluatorHistograms * Evaluator::add(StiEvaluatorHistograms *h)
{
  _histograms.push_back(h);
  return h;
}
