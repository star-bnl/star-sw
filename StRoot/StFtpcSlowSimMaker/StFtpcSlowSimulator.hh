// $Id: StFtpcSlowSimulator.hh,v 1.1 2000/11/23 10:16:44 hummler Exp $
// $Log: StFtpcSlowSimulator.hh,v $
// Revision 1.1  2000/11/23 10:16:44  hummler
// New FTPC slow simulator in pure maker form
//
//
#ifndef STAR_StFtpcSlowSimulator
#define STAR_StFtpcSlowSimulator

class StFtpcRawWriter;
class StFtpcGeantReader;
class StFtpcParamReader;

class StFtpcSlowSimulator {
protected:
StFtpcGeantReader *mGeant;
StFtpcParamReader *mParam;
StFtpcRawWriter *mWriter;
  
public:
  StFtpcSlowSimulator(StFtpcGeantReader *geantReader,
		      StFtpcParamReader *paramReader,
		      StFtpcRawWriter *rawWriter); 
  int simulate();
  virtual ~StFtpcSlowSimulator(); 
  
};
#endif


