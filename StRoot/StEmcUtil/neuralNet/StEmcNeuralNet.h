/*!\class StEmcNeuralNet
\author Julia Zulkarneeva and Dmitry Arkhipkin and Alex Suaide

This class is an interface compatible with STAR software for the Neural Net
package developed by J.P. Ernenwein (see http://e.home.cern.ch/e/ernen/www/NN/index.html
for details). The methodology of training and QA of the Neural Net was developed
by Julia Zulkarneeva and Dmitry Arkhipkin. This interface was written by Alex Suaide.

This basic interface allows the use of the neural net for any anlysis that depends
on decisions based on a set of parameters. The signal and background files sould be
created as root NTuples and saved as a TTree in a root file. 
*/
#ifndef StEmcNeuralNet_HH
#define StEmcNeuralNet_HH
#include "TObject.h"

#include "TFile.h"
#include "TTree.h"
#include "TString.h"
#include "TH1.h"
class TNNKernel;
class TNNTree;
class TNNUtils;
class TNNControlE;
class TTree;

class StEmcNeuralNet
{
   private:
      TH1F*       mHSignal;
      TH1F*       mHBack;
      TH1F*       mHTested;
      
      TFile*      mSignal;
      TFile*      mBack;
      TTree*      mSignalTree;
      TTree*      mBackTree;
      TTree*	    mValidTree;
      
      TString     mFileNames[2];
      TString     mFormula;

      TNNKernel*  mKernel;
      TNNTree*    mTrain;
      TNNTree*    mValid;

      TNNUtils*   mUSignal;
      TNNUtils*   mUBack;
      TNNUtils*	  mUValid;
      
      TNNControlE*mControl;
      
      int         mNPar;
      int         mNValid;
      
      TTree*      getTree(TFile*);
      void	      freeMemory(); 
      
   public: 
     
                  StEmcNeuralNet(); ///< Default constructor
   virtual        ~StEmcNeuralNet();///< Default destructor
   
      void        setSignalFile(char* f)    { mFileNames[0] = f; } ///< Set filename with the signal TTree
      void        setBackFile(char* f)      { mFileNames[1] = f; } ///< Set filename with the background TTree
      void        setFormula(char* f)       { mFormula = f; } ///< Set the formula to train the Neural Net
      void        setNParameters(int n)     { mNPar = n; } ///< Set number of parameters in the Net (maximum 10)
      bool        initNNet(int=0,int=0);   ///< Init neural net
      bool        train(int=2000,int=100);  ///< Train neural net. It display a graph with the training being updated
      void        saveKernel(char* file);   ///< Save the kernel data into file
      void        loadKernel(char*);        ///< Load the kernel data from file
      void        drawNNetHists();          ///< Draw QA histograms
      
      //here are the methods for use after training the neuralnet
      TNNKernel*  getNNKernel()             { return mKernel; }   ///< Gets NN Kernel 
      TNNTree*    getNNTree()               { return mTrain; }    ///< Gets NN Tree
      TNNUtils*   getNNUtilSignal()         { return mUSignal; }  ///< Gets NN Signal Utility
      TNNUtils*   getNNUtilBack()           { return mUBack; }    ///< Gets NN Background utility
      TNNControlE*getNNControlE()           { return mControl; }  ///< Gets NN Control
      
      TH1F*       getSignalHist()           { return mHSignal; }  ///< Gets Signal histogram 
      TH1F*       getBackHist()             { return mHBack; }    ///< Gets Background histogram 
      TH1F*       getTestedHist()           { return mHTested; }  ///< Gets histogram for tested events
      
      float       getNNGuess(float*);                             ///< Returns NN evaluation for a set of parameters
      
   ClassDef(StEmcNeuralNet,0)
};
#endif
