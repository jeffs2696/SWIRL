# SourceSubstitution will substitute expressions for the following variables
def SourceSubstitution(S, \
                       A_analytic         , \
                       M_t_analytic       , \
                       M_x_analytical     , \
                       v_r_analytical     , \
                       v_t_analytical     , \
                       v_x_analytical     , \
                       p_analytical       , \
                       dp_dr_analytical   , \
                       dv_r_dr_analytical , \
                       dM_x_dr_analytical , \
                       dM_t_dr_analytical , \
                      ): 
    
    print('Input Source: ', S)
    S = S.subs({ \
                A:A_analytic       , \
                M_t:M_t_analytic   , \
                M_x:M_x_analytical , \
                v_r:v_r_analytical , \
                v_t:v_t_analytical , \
                v_x:v_x_analytical , \
                p:p_analytical     , \
                dp_dr:dp_dr_analytical, \
                dv_r_dr:dv_r_dr_analytical, \
                dM_x_dr:dM_x_dr_analytical, \
                dM_t_dr:dM_t_dr_analytical, \
               })
    print('Output Source', S)
    return S