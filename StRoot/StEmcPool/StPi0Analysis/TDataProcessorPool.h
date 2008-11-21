#ifndef StPi0Analysis_TDataProcessorPool_H
#define StPi0Analysis_TDataProcessorPool_H

#include <TNamed.h>

class TFile;

#include <list>
using std::list;

#include <StEmcPool/StPi0Common/StPi0CommonUtil.h>

#include "StPi0AnalysisVersion.h"

class TDataProcessor;

class TDataProcessorPool : public TNamed {
	public:
		typedef TDataProcessorPool this_type;
		typedef TNamed inherited;
		typedef TDataProcessor processor_type;
		typedef processor_type *processor_type_pointer;
		typedef const processor_type *const_processor_type_pointer;
		typedef list<processor_type_pointer> list_type;

		TDataProcessorPool(const Char_t *name = 0, const Char_t *title = 0);
		TDataProcessorPool(const this_type &pool);
		virtual ~TDataProcessorPool();

		this_type &operator=(const this_type &pool);

		virtual void Print(Option_t* option) const;

		list_type processors;
		processor_type_pointer getProcessor(const Char_t *name);
		const_processor_type_pointer getProcessor(const Char_t *name) const;

		void deleteProcessors();

		Bool_t processFile(const TFile *inFile, TFile *outFile);
		Bool_t processFile(const Char_t *filename, const Char_t *outPath = 0);

		Bool_t writeToFile(TFile &outFile) const;
		Bool_t writeToFile(const Char_t *filename) const;

		Int_t debug;
		Int_t outputPrescale;
		Int_t writeArraySize;
		Int_t maxVirtualSize;
		Int_t basketSize;
		Int_t splitLevel;
		Int_t compression;

		TMyDataAnalysisSettings settings;

		ClassDef(TDataProcessorPool, STPI0ANALYSIS_VERSION)
};

typedef list<TDataProcessorPool*> pools_list_type;
extern pools_list_type poolsList;
void removePool(const TDataProcessorPool *pool, Bool_t deletePool = false, Bool_t deleteProcessors = false);

#endif
