#ifndef _TPX_RDO_H_
#define _TPX_RDO_H_

struct tpx_rdo {
	uint8_t sector ;
	uint8_t rdo ;

	uint8_t remote ;

	uint8_t temp_rdo ;
	uint8_t temp_stratix ;

	uint32_t fpga_usercode[5] ;

	uint8_t compilation_date[24] ;

	uint16_t status_cpld ;
	uint16_t status_xilinx ;


	uint32_t three[3] ;

	struct {
		uint32_t base ;
		uint8_t x_s ;
		uint8_t id ;
		uint8_t fee_status ;
		uint8_t pad_id ;
		uint8_t jumpers ;
	} fee[3][12] ;

} ;


#endif

