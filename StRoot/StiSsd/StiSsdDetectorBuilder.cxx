#include "StiSsd/StiSsdDetectorBuilder.h" 

StiSsdDetectorBuilder::StiSsdDetectorBuilder(bool active, const string & inputFile)
  : StiDetectorBuilder("Ssd",active,inputFile)
{}

StiSsdDetectorBuilder::~StiSsdDetectorBuilder()
{}

void StiSsdDetectorBuilder::buildDetectors(StMaker&source)
{}

