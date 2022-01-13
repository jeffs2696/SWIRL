import unittest
import helpers
import sympy as sp

class TestHelpers(unittest.TestCase):
    
    def test_TanhMethod(self):
        r = sp.Symbol('r')
        
        n     = 9 # number of kinks
        B     = 1000
        r_min = 0
        r_max = 1
        
        result = helpers.TanhMethodMMS(n     = n     ,\
                                       B     = B     ,\
                                       r_min = r_min ,\
                                       r_max = r_max)
        
        # checks to see if result is a symbolic summation data type
        self.assertEqual(type(result),sp.Add)
        # we need to add more checks for TanhMethod
        
    def test_ModifiedManufacturedSolution(self):
        r     = sp.Symbol('r')
        r_min = sp.Symbol('r_min')
        r_max = sp.Symbol('r_max')
        r_hat     = (r - r_min)/(r_max - r_min)
        
        f         = sp.exp(r_hat)
        r_hat_max = r_hat.subs('r','r_max')
        r_hat_min = r_hat.subs('r','r_min')
    
        f_max = sp.exp(r_hat_max)
        f_min = sp.exp(r_hat_min)
        
        f_min_desired = 0.25
        f_max_desired = 0.25
        
        A_max         = 3*r_hat**2 - 2*r_hat**3
        A_min         =  1 - A_max 
         
        del_f_min = abs(f_min - f_min_desired)
        del_f_max = abs(f_max - f_max_desired)
            
        result = helpers.ModifiedManufacturedSolution(\
                                                      f_MS    = f     ,\
                                                      f_minBC = f_min ,\
                                                      f_maxBC = f_max ,\
                                                      f_minMS = f_min_desired ,\
                                                      f_maxMS = f_max_desired ,\
                                                      A_min   = A_min ,\
                                                      A_max   = A_max ) 
                                                      
        self.assertEqual(result.subs(r,r_min),f_min_desired)
        self.assertEqual(result.subs(r,r_max),f_max_desired)
        
    def test_diffModifiedManufacturedSolution(self):
        
        r = sp.Symbol('r')
        sigma = sp.Symbol('sigma')
        r_min = 0
        r_max = 1
        
        # defining a test function to imposed BCs on
        # the function has to be a function of beta! (NO beta plZ, it actually needs to be a function )
        f_MS     = sp.sin((r-r_max)/(r_max-r_min))
        df_MS    = sp.diff(f_MS,'r')
        df_minMS = df_MS.subs('r',r_min)
        df_maxMS = df_MS.subs('r',r_max)
        
        # the desired boundary conditions
        df_maxBC = 0.25
        df_minBC = 0.25
        
        B_min = (1 - r_min)*\
        (\
          (r - r_min)/(r_max - r_min)    - \
        2*((r - r_min)/(r_max - r_min))**2 +\
          ((r - r_min)/(r_max - r_min))**3  \
        )
        
        B_max = (r_max - r_min)* \
        (\
        -1*((r - r_min)/(r_max - r_min))**2 +\
           ((r - r_min)/(r_max - r_min))**3  \
        )
        
        result     = helpers.diffModifiedManufacturedSolution( f_MS   , \
                                                  df_minBC, \
                                                  df_maxBC, \
                                                  df_minMS, \
                                                  df_maxMS, \
                                                  B_min  , \
                                                  B_max) 
        diffresult = sp.diff(result,r)
    
                                                      
        self.assertEqual(diffresult.subs(r,r_min),df_minBC)
        self.assertEqual(diffresult.subs(r,r_max),df_maxBC)
        
        
        
if __name__ == '__main__':
    unittest.main()