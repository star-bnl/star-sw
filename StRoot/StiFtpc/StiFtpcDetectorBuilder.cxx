#include "StiFtpc/StiFtpcDetectorBuilder.h" 

StiFtpcDetectorBuilder::StiFtpcDetectorBuilder(bool active)
  : StiDetectorBuilder("FtpcBuilder",active)
{}

StiFtpcDetectorBuilder::~StiFtpcDetectorBuilder()
{}

void StiFtpcDetectorBuilder::buildDetectors(StMaker &source)
{}
