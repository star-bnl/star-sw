#ifndef Evaluator_H
#define Evaluator_H
#include "TObject.h"
#include <string>
#include <vector>
#include "Sti/Base/Named.h"
#include "Sti/Base/Described.h"
#include "StiEvaluator/StiEvaluatorHistograms.h"
#include "StMiniMcEvent/StMiniMcEvent.h"

class Evaluator : public Named, public Described, public TObject

{
 public:
  Evaluator(const string & name="Evaluator", const string & description="Evaluator");
  ~Evaluator();
  void run(const char * inputFile);
  void run(const string & inputFile);
  void save(const char * inputFile);
  void save(const string & targetDirectory);
  void saveHtml(const char * inputFile);
  void saveHtml(const string &targetDirectory);
  StiEvaluatorHistograms * add(StiEvaluatorHistograms *);

 protected:

  vector<StiEvaluatorHistograms*> _histograms;
  StMiniMcEvent   * event;

  ClassDef(Evaluator,1)
};

#endif
