#include "HistogramGroup.h"

HistogramGroup::HistogramGroup()
  : Named("HistogramGroup"),
    Described("HistogramGroup")
{}

HistogramGroup::HistogramGroup(const string & name, const string & description)
  : Named(name),
    Described(description)
{}

HistogramGroup::~HistogramGroup()
{}

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
