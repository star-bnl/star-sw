#include "StiHistograms.h"
#include "Sti/StiTrackContainer.h"

StiHistograms::StiHistograms()
  : HistogramGroup("noName","noDescription")
{}

StiHistograms::StiHistograms(const string & name, const string & description)
  : HistogramGroup(name,description)
{}

StiHistograms::~StiHistograms()
{}

void StiHistograms::fill(StiTrackContainer* container)
{}

void StiHistograms::initialize()
{}
