#ifndef MiniChain_H_INCLUDED
#define MiniChain_H_INCLUDED
#include "TObject.h"
class StChain;
class StiMaker;
class StiRootIOBroker;
class StiGuiIOBroker;
class MiniChainParameters;


class MiniChain : public TObject
{
 public:
  MiniChain();
  virtual ~MiniChain();
	
	MiniChainParameters * getParameters();
  StiRootIOBroker * getIOBroker();
  StiGuiIOBroker  * getGuiIOBroker();
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
 
  MiniChainParameters * _pars;
  StChain             * _chain;
  StiMaker            * _stiMaker;
  StiRootIOBroker     * _stiIoBroker;
  StiGuiIOBroker      * _guiIoBroker;
  StIOMaker           * _ioMaker;

  ClassDef(MiniChain, 1)
};

#endif
