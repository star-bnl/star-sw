// $Id: StFtpcSlowSimulator.hh,v 1.2 2001/03/06 23:36:30 jcs Exp $
// $Log: StFtpcSlowSimulator.hh,v $
// Revision 1.2  2001/03/06 23:36:30  jcs
// use database instead of params
//
// Revision 1.1  2000/11/23 10:16:44  hummler
// New FTPC slow simulator in pure maker form
//
//
#ifndef STAR_StFtpcSlowSimulator
#define STAR_StFtpcSlowSimulator

class StFtpcRawWriter;
class StFtpcGeantReader;
class StFtpcParamReader;
class StFtpcDbReader;

class StFtpcSlowSimulator {
protected:
StFtpcGeantReader *mGeant;
StFtpcParamReader *mParam;
StFtpcDbReader    *mDb;
StFtpcRawWriter *mWriter;
  
public:
  StFtpcSlowSimulator(StFtpcGeantReader *geantReader,
		      StFtpcParamReader *paramReader,
                      StFtpcDbReader    *dbReader,
		      StFtpcRawWriter *rawWriter); 
  int simulate();
  virtual ~StFtpcSlowSimulator(); 
  
};
#endif


