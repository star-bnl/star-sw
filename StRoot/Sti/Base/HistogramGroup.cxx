#include "HistogramGroup.h"

HistogramGroup::HistogramGroup()
  : Named(""),
    Described("")
{}

HistogramGroup::HistogramGroup(const string & name, const string & description)
  : Named(name),
    Described(description)
{}

HistogramGroup::~HistogramGroup()
{}

void HistogramGroup::write(TFile * file)
{
  file->cd();
  write();
}

void HistogramGroup::write(const string & fileName, const string &option)
{
  TFile *file = new TFile(fileName.c_str(),option.c_str());
  write();
  file->Close();
  delete file;
}


void HistogramGroup::write()
{
  vector<TH1*>::iterator iter;
  for (iter=begin();iter!=end();iter++)
    {
      (*iter)->Write();
    }
}

void HistogramGroup::reset()
{
  vector<TH1*>::iterator iter;
  for (iter=begin();iter!=end();iter++)
    {
      (*iter)->Reset();
    }
}

TH1D * HistogramGroup::book(const string &title, 
			    const string & description, 
			    int nx, 
			    double xMin,
			    double xMax)
{
  string histoName = getName() + "_" + title;
  string histoDesc = getName() + " " + description;
  return dynamic_cast<TH1D*>(add(new TH1D(histoName.c_str(),histoDesc.c_str(),nx,xMin,xMax)));
}

TH2D * HistogramGroup::book(const string &title, 
			    const string & description, 
			    int nx, 
			    double xMin,
			    double xMax,
			    int ny, 
			    double yMin,
			    double yMax)
{
  string histoName = getName() + "_" + title;
  string histoDesc = getName() + " " + description;
  return dynamic_cast<TH2D*>(add(new TH2D(histoName.c_str(),histoDesc.c_str(),nx,xMin,xMax,ny,yMin,yMax)) );
}

TH3D * HistogramGroup::book(const string &title, 
			    const string & description, 
			    int nx, 
			    double xMin,
			    double xMax,
			    int ny, 
			    double yMin,
			    double yMax,
			    int nz, 
			    double zMin,
			    double zMax)
{
  string histoName = getName() + "_" + title;
  string histoDesc = getName() + " " + description;
  return dynamic_cast<TH3D*>(add(new TH3D(histoName.c_str(),histoDesc.c_str(),nx,xMin,xMax,ny,yMin,yMax,nz,zMin,zMax)) );
}

TProfile * HistogramGroup::bookProfile(const string &title, 
				       const string & description, 
				       int nx, 
				       double xMin,
				       double xMax)
{
  string histoName = getName() + "_" + title;
  string histoDesc = getName() + " " + description;
  return dynamic_cast<TProfile*>(add(new TProfile(histoName.c_str(),histoDesc.c_str(),nx,xMin,xMax)));
}

TProfile2D * HistogramGroup::bookProfile(const string &title, 
					 const string & description, 
					 int nx, 
					 double xMin,
					 double xMax,
					 int ny, 
					 double yMin,
					 double yMax)
{
  string histoName = getName() + "_" + title;
  string histoDesc = getName() + " " + description;
  return dynamic_cast<TProfile2D*>(add(new TProfile2D(histoName.c_str(),histoDesc.c_str(),nx,xMin,xMax,ny,yMin,yMax)) );
}
