#ifndef StiEvaluator_HH
#define StiEvaluator_HH

class StiTrackContainer;

/// Abstract interface for the Sti Evaluator 
/**
 * Abstract interface for the StiEvalutor package
 */
class StiEvaluator
{
public:
   virtual void evaluate(const StiTrackContainer*)=0;
};

#endif
