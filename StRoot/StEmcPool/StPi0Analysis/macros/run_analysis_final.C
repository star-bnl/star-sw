#if !defined(__CINT__) || defined(__MAKECINT__)

#include <TSystem.h>
#include <TFolder.h>
#include <TBrowser.h>
#include <TCanvas.h>
#include <TDatime.h>
#include <TStopwatch.h>
#include <TString.h>

#include <StEmcPool/StPi0Analysis/TDataProcessorPool.h>

#endif

void run_analysis_final(const Char_t *filelist = "filelist.list", const Char_t *outFile = "/dev/null") {
	Info(__FILE__, "================== STARTED ===================");
	TStopwatch timer;
	timer.Start();
	TDatime startTime;
	Info(__FILE__, "Started: %s", startTime.AsSQLString());

	TDataProcessorPool pool("pool", "Pool of data processors");
	pool.debug = 10;

	{
	    TString filelistExact = findFile(filelist);
    	    Info(__FILE__, "Processing file %s", filelistExact);
	    pool.processFile(filelistExact);
    	    Info(__FILE__, "Finished processing file %s", filelistExact);
	}

    	Info(__FILE__, "Writing output file %s", outFile ? outFile : "");
	pool.writeToFile(outFile);
        Info(__FILE__, "Finished writing output file %s", outFile ? outFile : "");

	TDatime stopTime;
	Info(__FILE__, "Finished: %s", stopTime.AsSQLString());
	timer.Stop();
	timer.Print();
	Info(__FILE__, "================== FINISHED ==================");
}
