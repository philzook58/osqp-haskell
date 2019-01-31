// gcc -losqp test_osqp.c 

#include <osqp/osqp.h>

int main(int argc, char **argv) {
    // Load problem data
    c_float P_x[4] = {4.00, 1.00, 1.00, 2.00, };
    c_int P_nnz = 4;
    c_int P_i[4] = {0, 1, 0, 1, };
    c_int P_p[3] = {0, 2, 4, };
    c_float q[2] = {1.00, 1.00, };
    c_float A_x[4] = {1.00, 1.00, 1.00, 1.00, };
    c_int A_nnz = 4;
    c_int A_i[4] = {0, 1, 0, 2, };
    c_int A_p[3] = {0, 2, 4, };
    c_float l[3] = {1.00, 0.00, 0.00, };
    c_float u[3] = {1.00, 0.70, 0.70, };
    c_int n = 2;
    c_int m = 3;

    // Problem settings
    OSQPSettings * settings = (OSQPSettings *)c_malloc(sizeof(OSQPSettings));

    // Structures
    OSQPWorkspace * work;  // Workspace
    OSQPData * data;  // OSQPData

    // Populate data
    data = (OSQPData *)c_malloc(sizeof(OSQPData));
    data->n = n;
    data->m = m;
    data->P = csc_matrix(data->n, data->n, P_nnz, P_x, P_i, P_p);
    data->q = q;
    data->A = csc_matrix(data->m, data->n, A_nnz, A_x, A_i, A_p);
    data->l = l;
    data->u = u;


    // Define Solver settings as default
    osqp_set_default_settings(settings);
    settings->alpha = 1.0; // Change alpha parameter

    // Setup workspace
    work = osqp_setup(data, settings);

    // Solve Problem
    osqp_solve(work);

    // Cleanup
    osqp_cleanup(work);
    c_free(data->A);
    c_free(data->P);
    c_free(data);
    c_free(settings);

    return 0;
};

/*
-- Install configuration: ""
-- Installing: /usr/local/lib/libosqp.a
-- Installing: /usr/local/include/osqp/auxil.h
-- Installing: /usr/local/include/osqp/constants.h
-- Installing: /usr/local/include/osqp/cs.h
-- Installing: /usr/local/include/osqp/ctrlc.h
-- Installing: /usr/local/include/osqp/glob_opts.h
-- Installing: /usr/local/include/osqp/kkt.h
-- Installing: /usr/local/include/osqp/lin_alg.h
-- Installing: /usr/local/include/osqp/lin_sys.h
-- Installing: /usr/local/include/osqp/osqp.h
-- Installing: /usr/local/include/osqp/osqp_configure.h
-- Installing: /usr/local/include/osqp/types.h
-- Installing: /usr/local/include/osqp/polish.h
-- Installing: /usr/local/include/osqp/proj.h
-- Installing: /usr/local/include/osqp/scaling.h
-- Installing: /usr/local/include/osqp/util.h
-- Installing: /usr/local/lib/libosqp.dylib
-- Installing: /usr/local/lib/cmake/osqp/osqp-targets.cmake
-- Installing: /usr/local/lib/cmake/osqp/osqp-targets-noconfig.cmake
-- Installing: /usr/local/lib/cmake/osqp/osqp-config.cmake
-- Installing: /usr/local/lib/libqdldl.a
-- Installing: /usr/local/include/qdldl/qdldl.h
-- Installing: /usr/local/include/qdldl/qdldl_types.h
-- Installing: /usr/local/lib/libqdldl.dylib
-- Installing: /usr/local/lib/cmake/qdldl/qdldl-targets.cmake
-- Installing: /usr/local/lib/cmake/qdldl/qdldl-targets-noconfig.cmake
-- Installing: /usr/local/lib/cmake/qdldl/qdldl-config.cmake
*/