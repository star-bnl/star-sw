/**************************************************************************
 * $Id: StFmsDbConfig.h,v 1.1 2015/03/10 14:38:54 jeromel Exp $
 * $Author: jeromel $
 **************************************************************************
 *
 * Description: This helper class faciliates the maintenance work and access 
 * 		to FMS reconstruction-related parameters fmsRec
 *
 **************************************************************************
 *
 * $Log: StFmsDbConfig.h,v $
 * Revision 1.1  2015/03/10 14:38:54  jeromel
 * First version of FmsUtil from Yuxi Pan - reviewd 2015/02
 *
 *
 **************************************************************************/


#ifndef STFMSDBCONFIG_H
#define STFMSDBCONFIG_H
#include "StMessMgr.h"
#include "Stypes.h"
#include "TObject.h"
#include <map>
#include <cstdlib>
#include <string>
#include "tables/St_fmsRec_Table.h"
#include <iostream>
#include <sstream>
class StFmsDbConfig {

public:

	/* create singleton the first time it is used */
        inline static StFmsDbConfig& Instance() {
                static StFmsDbConfig theConfig;
                return theConfig;
        }

	/* fill internal map from text file*/
	Int_t fillMap(const char*);
	
	/* fill fmsRec_st structure with internal map*/
	Int_t fillFmsRec(fmsRec_st&);
	
	/* read database table fmsRe_st* into internal map*/
	Int_t readMap(const fmsRec_st&);
	
	/* write internal map to text file*/
	Int_t writeMap(const char*);
	
	Bool_t isMapEmpty();
	
	Bool_t keyExist(std::string);	

	/* do not invoke conversion if user asked for std::string, can be used with macros */
	const std::string& getParameter(std::string);

	/* do not invoke conversion if user is setting parameters with std::string */
	void setParameter(std::string, std::string);
 
	/* retrieve specific parameter by name, only works with compiled code 
 	   the user code need to make sure key exists in map (using keyExist()) */
	template<class T> 
	T getParameter(std::string param){
		
	        const std::string& val = getParameter(param);

        	T value;
		std::istringstream buffer(val);
		buffer >> value;

		return value;
	}
	
	template<class T> 
	void setParameter(std::string param, T value){
		std::ostringstream buffer;
		buffer << value;
		mRecPar[param] = buffer.str();

	}
	

private:

	 StFmsDbConfig(){};  				//hide constructor and destructor
        ~StFmsDbConfig(){};
	StFmsDbConfig(const StFmsDbConfig&);
        StFmsDbConfig& operator=(const StFmsDbConfig&);

	std::map<std::string, std::string> mRecPar;	//name to value mapping
	
	ClassDef(StFmsDbConfig,1)

};

#endif
