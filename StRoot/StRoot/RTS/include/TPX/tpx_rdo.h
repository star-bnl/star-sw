#ifndef _TPX_RDO_H_
#define _TPX_RDO_H_

struct tpx_rdo {
	u_char sector ;
	u_char rdo ;

	u_char remote ;

	u_char temp_rdo ;
	u_char temp_stratix ;

	u_int fpga_usercode[5] ;

	u_char compilation_date[24] ;

	u_short status_cpld ;
	u_short status_xilinx ;


	u_int three[3] ;

	struct {
		u_int base ;
		u_char x_s ;
		u_char id ;
		u_char fee_status ;
		u_char pad_id ;
		u_char jumpers ;
	} fee[3][12] ;

} ;


#endif

