#ifndef MiniChain_H_INCLUDED
#define MiniChain_H_INCLUDED
#include "TObject.h"
class StChain;
class StiMaker;
class StiMakerParameters;
class StiDefaultToolkit;
class StIOMaker;

class MiniChain : public TObject
{
 public:
  MiniChain();
  virtual ~MiniChain();
	
  StiMakerParameters * getParameters();
  void run(int first, 
	   int nEvents, 
	   const char *  filePrefix,
	   const char ** fileList );
  
 protected:
  
  void setupInput(const char ** fileList);
  void setupOutput(const char * filePrefix,
		   const char * fileTemplate);
  void setupSimulation();
  void setupDatabase();
  void setupGui();
  void eventLoop(int first, int last); 
  
  StiMakerParameters  * _pars;
  StChain             * _chain;
  StiMaker            * _stiMaker;
  StIOMaker           * _ioMaker;
  StiDefaultToolkit   * _toolkit;

  ClassDef(MiniChain, 1)
};
#endif
