/*
   FMS calibration macro: perform fit on each mass, update gainCorr, and print new gainCorr set

   Chong Kim
   UC Riverside
   ckim@bnl.gov
*/

#include "calibFmsShow.C" //calibFmsShow.C includes calibFmsTools.C dlready

void calibFms(const int iIter = 0, const char* outPath = "./Iterations", bool PRINT = true)
{
    TStopwatch SW;
    SW.Start();

    //TFile* F = TFile::Open("./fmsCalib.root");
    TFile* F = TFile::Open(Form("./fmsCalib_%i.root", iIter));
    TFile* G = new TFile(Form("out_fmsCalib_%i.root", iIter), "RECREATE");

    TH2F* H2_mass[4];
    for (int a=0; a<4; a++) H2_mass[a] = (TH2F*)F->Get(Form("mass_d%i", a+8));

	//Map with exclamation mark is essentially required to proceed
    map<int, int>    iToCh[4]; //!
    map<int, int>    chToBS[4];
    map<int, int>    chToCellStat[4]; //!
    map<int, float>  chToGainCorr[4]; //! gainCorr applied for current iteration
    map<int, st_pos> chToPos[4];
    GetMapIndexToCh   ("./FmsMapBase.txt",     iToCh);
    GetMapChToBS      ("./FmsMapBitShift.txt", chToBS);
    GetMapChToCellStat("./FmsCellStat.txt",    chToCellStat);
    GetMapChToGainCorr("./FmsGainCorr.txt",    chToGainCorr);
    GetMapChToPos     ("./FmsMapBase.txt",     chToPos);

	//Perform fit, Update gainCorr
    //--------------------------------------------------------------------

	//Essential
    map<int, st_fitR> chToFitR[4];
    map<int, float> chToGainCorrNext[4]; //Results of "current" iteration: will be used in upcoming iteration

	//Watchdog + QA
	map<int, int> chToSkew[4];
    map<int, st_fitR> chToFitRPrev[4]; 
	if (iIter > 0) GetMapChToSkew(Form("%s/out_fmsCalibSkew_%i.txt", outPath, iIter-1), chToSkew);
    if (iIter > 0) GetMapChToFitR(Form("%s/out_fmsCalibFit_%i.txt", outPath, iIter-1), chToFitRPrev);

	//In-situ manual gainCorr update: enforce values in this list for next iteration
	map<int, float> manGainCorr[4];
	GetMapManualGainCorr(Form("FmsGainCorrManual_%i.txt", iIter), manGainCorr);

	//In-situ QA lists
	vector<int> markBadF[4]; //Bad fit
	vector<int> markCold[4]; //Not bad nor dead, but has too small # of entries (< 100)
	vector<int> markLoHi[4]; //Too low (<0.5) or Too high (>3.0) gainCorr
	vector<int> markSkew[4]; //Channels judged skewed

    #if 1
    for (int a=0; a<4; a++)
    {
		//Perform fit, get position of each cell's mass
        FitMass(iIter, H2_mass[a], iToCh[a], chToBS[a],	chToCellStat[a], chToGainCorr[a], chToPos[a], chToFitR[a]);

		//Update gainCorr
        for (unsigned int b=0; b<iToCh[a].size(); b++)
		{
			const int   ch       = iToCh[a][b];
			const float gainCorr = chToGainCorr[a][ch]; //gainCorr applied for this iteration
			const float massFitR = chToFitR[a][ch].mass;
			const float massBook = 0.1349766;
			int nSkewed = (iIter==0)?0:chToSkew[a][ch];

			bool doUpdate = false;
			if (chToCellStat[a][ch]==GOOD)
			{
				//Return mass will be 0 if # of entries is too small
				if (massFitR != 0.) doUpdate = true;
				else markCold[a].push_back(ch);
			}

			const char* infoQA = "";
			float gainCorrNext = gainCorr;
			if (doUpdate)
			{
				gainCorrNext = (massBook/massFitR) * gainCorr;

				//Check too low or too high gainCorr
				if (gainCorr<0.3 || gainCorr>3.0) markLoHi[a].push_back(ch);
			
				//Check quality of the fit, limit ratio of allowed gainCorr update by 20 % if quality looks bad
                if (fabs(massFitR - massBook) > 0.03 ||
                    chToFitR[a][ch].nRefit == 10 ||
                    chToFitR[a][ch].chi2 > 25. ||
					chToFitR[a][ch].chi2 < 0.5)
				{
					const float upRatio = (gainCorrNext - gainCorr) / gainCorr;
					if (fabs(upRatio)>0.2 && upRatio>0) gainCorrNext = gainCorr * 1.2;
					if (fabs(upRatio)>0.2 && upRatio<0) gainCorrNext = gainCorr * 0.8;
					infoQA = "BADFIT ";
					markBadF[a].push_back(ch);
				}

				//Check skewed mass: mass position don't move even if higher gainCorr applied, small cells
				if (chToCellStat[a][ch]!=CONVERGED && iIter>0 && a>1 && massFitR<0.12)
				{
					const float dGainCorr = gainCorrNext - gainCorr;
					const float dMassFitR = massFitR - chToFitRPrev[a][ch].mass;
					if (dGainCorr>=0 && dMassFitR<0) //gainCorr increased but mass decreased: cancel update
					{
						gainCorrNext = gainCorr;
						infoQA = Form("%sSKEWED ", infoQA);
						markSkew[a].push_back(ch);

						nSkewed++;
						if (nSkewed==3)
						{
							infoQA = Form("%sRESET! ", infoQA);
							gainCorrNext = 0.750;
							nSkewed = 0;
						}
						map<int, int>::iterator chNewSkew = chToSkew[a].find(ch);
						if (chNewSkew != chToSkew[a].end()) chNewSkew->second = nSkewed;
					}
				}
			}//doUpdate

			//In-situ manual gainCorr update by user, between iterations
			if (manGainCorr[a].size()!=0 && manGainCorr[a][ch]!=0.)
			{
				infoQA = Form("%sMANUAL", infoQA);
				gainCorrNext = manGainCorr[a][ch];
			}

			//Final updated gainCorr list for next iteration
			chToGainCorrNext[a].insert(pair<int, float>(ch, gainCorrNext));

			if (PRINT)
			{
				const char* infoUp = (gainCorrNext != gainCorr)?Form("-> %4.3f", gainCorrNext):Form("%8s", "");
				cout <<Form("%2i %3i | %4.3f %s %s", a+8, ch, gainCorr, infoUp, infoQA) <<endl;
			}
		}//b, ch
    }//a, detId
	F->Close();
	G->Close();
    #endif

	//Print QA plots for in-situ monitoring
    //-------------------------------------

	#if 1
	cout <<"Generating in-situ QA plots..." <<endl;
	DrawMap(iToCh, chToPos, Form("GainCorrL_i%i", iIter), false, false, true, chToGainCorr, chToCellStat);
	DrawMap(iToCh, chToPos, Form("GainCorrS_i%i", iIter),  true, false, true, chToGainCorr, chToCellStat);

	bool QaBadF = false;
	bool QaCold = false;
	bool QaLoHi = false;
	bool QaSkew = false;
	for (int a=0; a<4; a++) { if (markBadF[a].size() > 0) { QaBadF = true; break; } }
	for (int a=0; a<4; a++) { if (markCold[a].size() > 0) { QaCold = true; break; } }
	for (int a=0; a<4; a++) { if (markLoHi[a].size() > 0) { QaLoHi = true; break; } }
	for (int a=0; a<4; a++) { if (markSkew[a].size() > 0) { QaSkew = true; break; } }
	if (QaBadF) DrawMap(iToCh, chToPos, "QABadF", false, false, true, DumMapF, chToCellStat, markBadF);
	if (QaCold) DrawMap(iToCh, chToPos, "QACold", false, false, true, DumMapF, chToCellStat, markCold);
	if (QaLoHi) DrawMap(iToCh, chToPos, "QALoHi", false, false, true, DumMapF, chToCellStat, markLoHi);
	if (QaSkew) DrawMap(iToCh, chToPos, "QASkew", false, false, true, DumMapF, chToCellStat, markSkew);
	if (QaBadF || QaCold || QaLoHi || QaSkew)
	{
		const char* QAout = Form("QAMap_i%i", iIter);
		gSystem->Exec(Form("mkdir %s", QAout));
		gSystem->Exec(Form("mv *.png %s", QAout));
		gSystem->Exec(Form("mv %s %s", QAout, outPath));
	}
	#endif

	//Print files for next iteration + Test convergence
    //-------------------------------------------------

    #if 1
    PrintFitResults(Form("out_fmsCalibFit_%i.txt", iIter), iToCh, chToFitR);
    gSystem->Exec(Form("mv out_fmsCalibFit_%i.txt %s", iIter, outPath));

    PrintGainCorr(Form("FmsGainCorrNew_%i.txt", iIter), chToGainCorrNext);
    gSystem->Exec(Form("mv FmsGainCorr.txt %s/FmsGainCorr_%i.txt", outPath, iIter));
    gSystem->Exec(Form("mv FmsGainCorrNew_%i.txt FmsGainCorr.txt", iIter));

    PrintSkewMonitor(Form("out_fmsCalibSkew_%i.txt", iIter), iToCh, chToSkew);
    gSystem->Exec(Form("mv out_fmsCalibSkew_%i.txt %s", iIter, outPath));

    TestConvergence(iIter, iToCh, chToCellStat, outPath); //Includes PrintCellStat
    #endif

    SW.Stop();
    SW.Print();
    return;
}//Main
