#include "StiFtpc/StiFtpcDetectorBuilder.h" 

StiFtpcDetectorBuilder::StiFtpcDetectorBuilder(bool active, const string & inputFile)
  : StiDetectorBuilder("FtpcBuilder",active,inputFile)
{}

StiFtpcDetectorBuilder::~StiFtpcDetectorBuilder()
{}

void StiFtpcDetectorBuilder::buildDetectors(StMaker &source)
{}
