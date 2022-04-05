#include "StFmsDbConfig.h"
#include <fstream>
#include <stdexcept>

ClassImp(StFmsDbConfig)

Int_t StFmsDbConfig::fillMap(const char* filename = "fmsrecpar.txt"){
	
	mRecPar.clear();
	LOG_INFO << "reading text file " << filename << endm;
	std::fstream fp(filename, std::ios::in);
	if(fp.is_open()){
		std::string field;
		std::string val;
		while(fp>>field>>val)mRecPar[field] = val;
	}
	else{
		LOG_ERROR << "ERROR opening text file " << filename << endm;
		return kStErr;
	}
	
	LOG_INFO << "map filled with " << mRecPar.size() << " entries" << endm;
	
	return kStOk;
}


Int_t StFmsDbConfig::fillFmsRec(fmsRec_st& rec){
	
	if(mRecPar.empty()){
		LOG_ERROR << "internal map is empty" << endm;
		return kStErr;
	}
	
	//manually assign table fields
	rec.ROW_LOW_LIMIT		= getParameter<unsigned short>("ROW_LOW_LIMIT");
	rec.COL_LOW_LIMIT		= getParameter<unsigned short>("COL_LOW_LIMIT");
	rec.CEN_ROW_LRG			= getParameter<float>("CEN_ROW_LRG");
	rec.CEN_ROW_WIDTH_LRG		= getParameter<unsigned short>("CEN_ROW_WIDTH_LRG");
	rec.CEN_UPPER_COL_LRG		= getParameter<unsigned short>("CEN_UPPER_COL_LRG");
	rec.CEN_ROW_SML			= getParameter<float>("CEN_ROW_SML");
	rec.CEN_ROW_WIDTH_SML		= getParameter<unsigned short>("CEN_ROW_WIDTH_SML");
	rec.CEN_UPPER_COL_SML		= getParameter<unsigned short>("CEN_UPPER_COL_SML");
	rec.CORNER_ROW			= getParameter<float>("CORNER_ROW");
	rec.CORNER_LOW_COL		= getParameter<float>("CORNER_LOW_COL");
	rec.CLUSTER_BASE		= getParameter<unsigned short>("CLUSTER_BASE");
	rec.CLUSTER_ID_FACTOR_DET	= getParameter<unsigned short>("CLUSTER_ID_FACTOR_DET");
	rec.TOTAL_TOWERS		= getParameter<unsigned short>("TOTAL_TOWERS");
	rec.PEAK_TOWER_FACTOR		= getParameter<float>("PEAK_TOWER_FACTOR");
	rec.TOWER_E_THRESHOLD		= getParameter<float>("TOWER_E_THRESHOLD");
	rec.BAD_2PH_CHI2		= getParameter<float>("BAD_2PH_CHI2");
	rec.BAD_MIN_E_LRG		= getParameter<float>("BAD_MIN_E_LRG");
	rec.BAD_MAX_TOW_LRG		= getParameter<float>("BAD_MAX_TOW_LRG");
	rec.BAD_MIN_E_SML		= getParameter<float>("BAD_MIN_E_SML");
	rec.BAD_MAX_TOW_SML		= getParameter<float>("BAD_MAX_TOW_SML");
	rec.VALID_FT			= getParameter<float>("VALID_FT");
	rec.VALID_2ND_FT		= getParameter<float>("VALID_2ND_FT");
	rec.VALID_E_OWN			= getParameter<float>("VALID_E_OWN");
	rec.SS_C			= getParameter<float>("SS_C");
	rec.SS_A1			= getParameter<float>("SS_A1");
	rec.SS_A2			= getParameter<float>("SS_A2");
	rec.SS_A3			= getParameter<float>("SS_A3");
	rec.SS_B1			= getParameter<float>("SS_B1");
	rec.SS_B2			= getParameter<float>("SS_B2");
	rec.SS_B3			= getParameter<float>("SS_B3");
	rec.CAT_NTOWERS_PH1		= getParameter<unsigned short>("CAT_NTOWERS_PH1");
	rec.CAT_EP1_PH2			= getParameter<float>("CAT_EP1_PH2");
	rec.CAT_EP0_PH2			= getParameter<float>("CAT_EP0_PH2");
	rec.CAT_SIGMAMAX_MIN_PH2	= getParameter<float>("CAT_SIGMAMAX_MIN_PH2");
	rec.CAT_EP1_PH1			= getParameter<float>("CAT_EP1_PH1");
	rec.CAT_EP0_PH1			= getParameter<float>("CAT_EP0_PH1");
	rec.CAT_SIGMAMAX_MAX_PH1	= getParameter<float>("CAT_SIGMAMAX_MAX_PH1");
	rec.PH1_START_NPH		= getParameter<float>("PH1_START_NPH");
	rec.PH1_DELTA_N			= getParameter<float>("PH1_DELTA_N");
	rec.PH1_DELTA_X			= getParameter<float>("PH1_DELTA_X");
	rec.PH1_DELTA_Y			= getParameter<float>("PH1_DELTA_Y");
	rec.PH1_DELTA_E			= getParameter<float>("PH1_DELTA_E");
	rec.PH2_START_NPH		= getParameter<unsigned short>("PH2_START_NPH");
	rec.PH2_START_FSIGMAMAX		= getParameter<float>("PH2_START_FSIGMAMAX");
	rec.PH2_RAN_LOW			= getParameter<float>("PH2_RAN_LOW");
	rec.PH2_RAN_HIGH		= getParameter<float>("PH2_RAN_HIGH");
	rec.PH2_STEP_0			= getParameter<float>("PH2_STEP_0");
	rec.PH2_STEP_1			= getParameter<float>("PH2_STEP_1");
	rec.PH2_STEP_2			= getParameter<float>("PH2_STEP_2");
	rec.PH2_STEP_3			= getParameter<float>("PH2_STEP_3");
	rec.PH2_STEP_4			= getParameter<float>("PH2_STEP_4");
	rec.PH2_STEP_5			= getParameter<float>("PH2_STEP_5");
	rec.PH2_STEP_6			= getParameter<float>("PH2_STEP_6");
	rec.PH2_MAXTHETA_F		= getParameter<float>("PH2_MAXTHETA_F");
	rec.PH2_LOWER_NPH		= getParameter<float>("PH2_LOWER_NPH");
	rec.PH2_LOWER_XF		= getParameter<float>("PH2_LOWER_XF");
	rec.PH2_LOWER_YF		= getParameter<float>("PH2_LOWER_YF");
	rec.PH2_LOWER_XMAX_F		= getParameter<float>("PH2_LOWER_XMAX_F");
	rec.PH2_LOWER_XMAX_POW		= getParameter<float>("PH2_LOWER_XMAX_POW");
	rec.PH2_LOWER_XMAX_LIMIT	= getParameter<float>("PH2_LOWER_XMAX_LIMIT");
	rec.PH2_LOWER_5_F		= getParameter<float>("PH2_LOWER_5_F");
	rec.PH2_LOWER_6_F		= getParameter<float>("PH2_LOWER_6_F");
	rec.PH2_UPPER_NPH		= getParameter<float>("PH2_UPPER_NPH");
	rec.PH2_UPPER_XF		= getParameter<float>("PH2_UPPER_XF");
	rec.PH2_UPPER_YF		= getParameter<float>("PH2_UPPER_YF");
	rec.PH2_UPPER_XMIN_F		= getParameter<float>("PH2_UPPER_XMIN_F");
	rec.PH2_UPPER_XMIN_P0		= getParameter<float>("PH2_UPPER_XMIN_P0");
	rec.PH2_UPPER_XMIN_LIMIT	= getParameter<float>("PH2_UPPER_XMIN_LIMIT");
	rec.PH2_UPPER_5_F		= getParameter<float>("PH2_UPPER_5_F");
	rec.PH2_UPPER_6_F		= getParameter<float>("PH2_UPPER_6_F");
	rec.PH2_3_LIMIT_LOWER		= getParameter<float>("PH2_3_LIMIT_LOWER");
	rec.PH2_3_LIMIT_UPPER		= getParameter<float>("PH2_3_LIMIT_UPPER");
	rec.GL_LOWER_1			= getParameter<float>("GL_LOWER_1");
	rec.GL_UPPER_DELTA_MAXN		= getParameter<float>("GL_UPPER_DELTA_MAXN");
	rec.GL_0_DLOWER			= getParameter<float>("GL_0_DLOWER");
	rec.GL_0_DUPPER			= getParameter<float>("GL_0_DUPPER");
	rec.GL_1_DLOWER			= getParameter<float>("GL_1_DLOWER");
	rec.GL_1_DUPPER			= getParameter<float>("GL_1_DUPPER");
	rec.GL_2_DLOWER			= getParameter<float>("GL_2_DLOWER");
	rec.GL_2_DUPPER			= getParameter<float>("GL_2_DUPPER");


	return kStOk;
}


Int_t StFmsDbConfig::readMap(const fmsRec_st& rec){
	/* read db tables and populate the internal map mRecPar */	

	mRecPar.clear();
	
	setParameter<unsigned short>("ROW_LOW_LIMIT",		rec.ROW_LOW_LIMIT);
	setParameter<unsigned short>("COL_LOW_LIMIT",		rec.COL_LOW_LIMIT);
	setParameter<float>("CEN_ROW_LRG",			rec.CEN_ROW_LRG);
	setParameter<unsigned short>("CEN_ROW_WIDTH_LRG",	rec.CEN_ROW_WIDTH_LRG);
	setParameter<unsigned short>("CEN_UPPER_COL_LRG",	rec.CEN_UPPER_COL_LRG);
	setParameter<float>("CEN_ROW_SML",			rec.CEN_ROW_SML);
	setParameter<unsigned short>("CEN_ROW_WIDTH_SML",	rec.CEN_ROW_WIDTH_SML);
	setParameter<unsigned short>("CEN_UPPER_COL_SML",	rec.CEN_UPPER_COL_SML);
	setParameter<float>("CORNER_ROW",			rec.CORNER_ROW);
	setParameter<float>("CORNER_LOW_COL",			rec.CORNER_LOW_COL);
	setParameter<unsigned short>("CLUSTER_BASE",		rec.CLUSTER_BASE);
	setParameter<unsigned short>("CLUSTER_ID_FACTOR_DET",	rec.CLUSTER_ID_FACTOR_DET);
	setParameter<unsigned short>("TOTAL_TOWERS",		rec.TOTAL_TOWERS);
	setParameter<float>("PEAK_TOWER_FACTOR",		rec.PEAK_TOWER_FACTOR);
	setParameter<float>("TOWER_E_THRESHOLD",		rec.TOWER_E_THRESHOLD);
	setParameter<float>("BAD_2PH_CHI2",			rec.BAD_2PH_CHI2);
	setParameter<float>("BAD_MIN_E_LRG",			rec.BAD_MIN_E_LRG);
	setParameter<float>("BAD_MAX_TOW_LRG",			rec.BAD_MAX_TOW_LRG);
	setParameter<float>("BAD_MIN_E_SML",			rec.BAD_MIN_E_SML);
	setParameter<float>("BAD_MAX_TOW_SML",			rec.BAD_MAX_TOW_SML);
	setParameter<float>("VALID_FT",				rec.VALID_FT);
	setParameter<float>("VALID_2ND_FT",			rec.VALID_2ND_FT);
	setParameter<float>("VALID_E_OWN",			rec.VALID_E_OWN);
	setParameter<float>("SS_C",				rec.SS_C);
	setParameter<float>("SS_A1",				rec.SS_A1);
	setParameter<float>("SS_A2",				rec.SS_A2);
	setParameter<float>("SS_A3",				rec.SS_A3);
	setParameter<float>("SS_B1",				rec.SS_B1);
	setParameter<float>("SS_B2",				rec.SS_B2);
	setParameter<float>("SS_B3",				rec.SS_B3);
	setParameter<unsigned short>("CAT_NTOWERS_PH1",		rec.CAT_NTOWERS_PH1);
	setParameter<float>("CAT_EP1_PH2",			rec.CAT_EP1_PH2);
	setParameter<float>("CAT_EP0_PH2",			rec.CAT_EP0_PH2);
	setParameter<float>("CAT_SIGMAMAX_MIN_PH2",		rec.CAT_SIGMAMAX_MIN_PH2);
	setParameter<float>("CAT_EP1_PH1",			rec.CAT_EP1_PH1);
	setParameter<float>("CAT_EP0_PH1",			rec.CAT_EP0_PH1);
	setParameter<float>("CAT_SIGMAMAX_MAX_PH1",		rec.CAT_SIGMAMAX_MAX_PH1);
	setParameter<float>("PH1_START_NPH",			rec.PH1_START_NPH);
	setParameter<float>("PH1_DELTA_N",			rec.PH1_DELTA_N);
	setParameter<float>("PH1_DELTA_X",			rec.PH1_DELTA_X);
	setParameter<float>("PH1_DELTA_Y",			rec.PH1_DELTA_Y);
	setParameter<float>("PH1_DELTA_E",			rec.PH1_DELTA_E);
	setParameter<unsigned short>("PH2_START_NPH",		rec.PH2_START_NPH);
	setParameter<float>("PH2_START_FSIGMAMAX",		rec.PH2_START_FSIGMAMAX);
	setParameter<float>("PH2_RAN_LOW",			rec.PH2_RAN_LOW);
	setParameter<float>("PH2_RAN_HIGH",			rec.PH2_RAN_HIGH);
	setParameter<float>("PH2_STEP_0",			rec.PH2_STEP_0);
	setParameter<float>("PH2_STEP_1",			rec.PH2_STEP_1);
	setParameter<float>("PH2_STEP_2",			rec.PH2_STEP_2);
	setParameter<float>("PH2_STEP_3",			rec.PH2_STEP_3);
	setParameter<float>("PH2_STEP_4",			rec.PH2_STEP_4);
	setParameter<float>("PH2_STEP_5",			rec.PH2_STEP_5);
	setParameter<float>("PH2_STEP_6",			rec.PH2_STEP_6);
	setParameter<float>("PH2_MAXTHETA_F",			rec.PH2_MAXTHETA_F);
	setParameter<float>("PH2_LOWER_NPH",			rec.PH2_LOWER_NPH);
	setParameter<float>("PH2_LOWER_XF",			rec.PH2_LOWER_XF);
	setParameter<float>("PH2_LOWER_YF",			rec.PH2_LOWER_YF);
	setParameter<float>("PH2_LOWER_XMAX_F",			rec.PH2_LOWER_XMAX_F);
	setParameter<float>("PH2_LOWER_XMAX_POW",		rec.PH2_LOWER_XMAX_POW);
	setParameter<float>("PH2_LOWER_XMAX_LIMIT",		rec.PH2_LOWER_XMAX_LIMIT);
	setParameter<float>("PH2_LOWER_5_F",			rec.PH2_LOWER_5_F);
	setParameter<float>("PH2_LOWER_6_F",			rec.PH2_LOWER_6_F);
	setParameter<float>("PH2_UPPER_NPH",			rec.PH2_UPPER_NPH);
	setParameter<float>("PH2_UPPER_XF",			rec.PH2_UPPER_XF);
	setParameter<float>("PH2_UPPER_YF",			rec.PH2_UPPER_YF);
	setParameter<float>("PH2_UPPER_XMIN_F",			rec.PH2_UPPER_XMIN_F);
	setParameter<float>("PH2_UPPER_XMIN_P0",		rec.PH2_UPPER_XMIN_P0);
	setParameter<float>("PH2_UPPER_XMIN_LIMIT",		rec.PH2_UPPER_XMIN_LIMIT);
	setParameter<float>("PH2_UPPER_5_F",			rec.PH2_UPPER_5_F);
	setParameter<float>("PH2_UPPER_6_F",			rec.PH2_UPPER_6_F);
	setParameter<float>("PH2_3_LIMIT_LOWER",		rec.PH2_3_LIMIT_LOWER);
	setParameter<float>("PH2_3_LIMIT_UPPER",		rec.PH2_3_LIMIT_UPPER);
	setParameter<float>("GL_LOWER_1",			rec.GL_LOWER_1);
	setParameter<float>("GL_UPPER_DELTA_MAXN",		rec.GL_UPPER_DELTA_MAXN);
	setParameter<float>("GL_0_DLOWER",			rec.GL_0_DLOWER);
	setParameter<float>("GL_0_DUPPER",			rec.GL_0_DUPPER);
	setParameter<float>("GL_1_DLOWER",			rec.GL_1_DLOWER);
	setParameter<float>("GL_1_DUPPER",			rec.GL_1_DUPPER);
	setParameter<float>("GL_2_DLOWER",			rec.GL_2_DLOWER);
	setParameter<float>("GL_2_DUPPER",			rec.GL_2_DUPPER);	

	return kStOk;
}


Int_t StFmsDbConfig::writeMap(const char* filename = "outfmsrec.txt"){
	
	if(mRecPar.empty()){
                LOG_ERROR << "internal map is empty, call fillMap() first" << endm;
                return kStErr;
        }
	std::cout << "writing map to " << filename << endl;
	std::fstream outfile(filename, std::ios::out);
	for(std::map<std::string, std::string>::iterator it = mRecPar.begin(); it != mRecPar.end(); ++it){
		outfile << (it->first) << " " << (it->second) << endl;
	}
	
	return kStOk;
}


Bool_t StFmsDbConfig::isMapEmpty(){
	
	if(mRecPar.empty()) return true;
	else return false;
}

Bool_t StFmsDbConfig::keyExist(std::string param){

	if(mRecPar.find(param) != mRecPar.end()) return true;
	else return false;
}

/* do not invoke conversion if user asked for std::string */
const std::string& StFmsDbConfig::getParameter(std::string param){
	
	return mRecPar[param]; //return string will be empty if there is no such key in mRecPar
	
}

/*do not invoke conversion if user is setting parameters with std::string */
void StFmsDbConfig::setParameter(std::string param, std::string value){
	
	mRecPar[param] = value;

}
