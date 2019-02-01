{-# LANGUAGE QuasiQuotes, MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Lib where


import qualified Language.C.Inline as C
import Foreign.C.Types
import qualified Data.Vector.Storable as V
import qualified Numeric.LinearAlgebra as LA
import qualified Numeric.LinearAlgebra.Data as D
import qualified Numeric.LinearAlgebra.Devel as D
import Control.Arrow ((***))
import Data.Tuple (swap)
C.context (C.baseCtx <> C.vecCtx)

C.include "<math.h>"
C.include "<osqp/osqp.h>"


testsp = D.mkSparse [((0,0), 3.0), ((1,1), 2.0)]
testcsr = D.mkCSR [((0,0), 3.0), ((1,1), 2.0)]

-- getCSR (D.SparseR csr ncols nrows) = 

assocVec :: (Num a, Ord a) => [a] -> [(Int,a)] 
assocVec v = filter (\(_,x) -> x /= 0) (zipWith (,) [0..] v)

-- conveniently, will assume that if your list stops, it is implied the rest of it is zero. Or maybe inconvenitly from a type safety persepctive
assocMat :: (Num a, Ord a) => [[a]] -> [((Int,Int),a)] 
assocMat m = let m' = map assocVec m in addRows m' where addRows m'' = concat $ zipWith (\i xs -> map (\(j,x) -> ((i,j),x)) xs) [0 ..] m''
-- AssocMatrix = [((Int, Int), Double)]

transposeAssoc = map (swap *** id) 

mkGCSC = D.tr . D.mkSparse . transposeAssoc 
mkGCSC' = mkGCSC . assocMat
mkCSC :: D.AssocMatrix -> CSC'
mkCSC = transpose'' . D.mkCSR . transposeAssoc 
mkCSC' :: [[Double]] -> CSC'
mkCSC' = mkCSC . assocMat
-- The CSC type is internal to hmatrix.
data CSC' = CSC'
        { cscVals  :: V.Vector Double
        , cscRows  :: V.Vector CInt
        , cscCols  :: V.Vector CInt
        , cscNRows :: Int
        , cscNCols :: Int
        } deriving Show

-- data OSQPCSC = CSC  -- {nzmax :: CInt, m :: CInt, n :: CInt, col_p :: V.Vector Ptr, col  }
{-
instance LA.Transposable D.CSR CSC'
  where
  tr (D.CSR vs cs rs n m) = CSC' vs cs rs m n
  tr' = tr

instance LA.Transposable CSC' D.CSR
  where
  tr (CSC' vs rs cs n m) = D.CSR vs rs cs m n
  tr' = tr
-}
transpose'' (D.CSR vs cs rs n m) = CSC' vs cs rs m n
transpose''' (CSC' vs rs cs n m) = D.CSR vs rs cs m n


funboy :: IO ()
funboy = do
  x <- [C.exp| double{ cos(1) } |]
  print x
someFunc :: IO ()
someFunc = putStrLn "someFunc"


myfun :: CDouble -> CDouble
myfun x = [C.pure| double{ cos( $(double x)) } |]


-- exampleOSQPData = 

-- Actually CDoubles
--data OSQPData = OSQPData {dn :: CInt, dm :: CInt, p :: CSC , 
--     a :: CSC, q :: V.Vector CDouble, l :: V.Vector CDouble, u :: V.Vector CDouble}




-- OSQPWorkspace *osqp_setup(const OSQPData *data, OSQPSettings *settings)
-- c_int osqp_solve(OSQPWorkspace *work)
-- c_int osqp_cleanup(OSQPWorkspace *work)



-- Maybe the easiest thing is to just inject default settings and marshal only what we need
-- Get a demo code in full c and start pulling pieces out.
-- l and u are the easiest

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




