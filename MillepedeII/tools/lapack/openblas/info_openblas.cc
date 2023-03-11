//============================================================================
// CHK 220621
// 
// Get OpenBLAS information
//============================================================================

#include <iostream>
#include <openblas/cblas.h>

int main() {
        std::cout << "OpenBLAS information " << std::endl;        
        std::cout << "OpenBLAS number of threads: " << openblas_get_num_threads() << std::endl;
        std::cout << "Number of cores: " << openblas_get_num_procs() << std::endl;
        std::cout << "Parallel type: " << openblas_get_parallel() << std::endl; // 0 -- serial, 1 -- Default parallel type, 2 -- OpenMP
        std::cout << "Config string: " << openblas_get_config() << std::endl;
        
	return 0;
}
