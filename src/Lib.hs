{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Lib
    ( someFunc,
    	myfun,
    	demo
    ) where


import qualified Language.C.Inline as C
import Foreign.C.Types
import qualified Data.Vector.Storable as V

C.context (C.baseCtx <> C.vecCtx)

C.include "<math.h>"
C.include "<osqp/osqp.h>"




main :: IO ()
main = do
  x <- [C.exp| double{ cos(1) } |]
  print x
someFunc :: IO ()
someFunc = putStrLn "someFunc"


myfun :: CDouble -> CDouble
myfun x = [C.pure| double{ cos( $(double x)) } |]


-- exampleOSQPData = 

-- Actually CDoubles
data OSQPData = OSQPData {dn :: CInt, dm :: CInt, p :: CSC , 
     a :: CSC, q :: V.Vector CDouble, l :: V.Vector CDouble, u :: V.Vector CDouble}

data CSC = CSC  -- {nxmax :: CInt, m :: CInt, n :: CInt, col_p :: V.Vector Ptr, col  }


-- OSQPWorkspace *osqp_setup(const OSQPData *data, OSQPSettings *settings)
-- c_int osqp_solve(OSQPWorkspace *work)
-- c_int osqp_cleanup(OSQPWorkspace *work)



-- Maybe the easiest thing is to just inject default settings and marshal only what we need
-- Get a demo code in full c and start pulling pieces out.
demo :: IO CInt
demo = [C.block|
int {
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
} |]




