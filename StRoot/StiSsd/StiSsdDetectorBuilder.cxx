#include "StiSsd/StiSsdDetectorBuilder.h" 

StiSsdDetectorBuilder::StiSsdDetectorBuilder(bool active)
  : StiDetectorBuilder("SsdBuilder",active)
{}

StiSsdDetectorBuilder::~StiSsdDetectorBuilder()
{}

void StiSsdDetectorBuilder::buildDetectors(StMaker&source)
{}

