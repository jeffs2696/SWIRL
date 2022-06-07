# ========= Packages ========
import sympy as sp 

def ModifiedManufacturedSolution(f_MS, \
                    f_minBC,\
                    f_maxBC,\
                    f_minMS,\
                    f_maxMS,\
                    A_min, \
                    A_max):
    
    
    f_BCsImposed  = f_MS + \
    A_min*(f_minBC - f_minMS) + \
    A_max*(f_maxBC - f_maxMS)
    #print(f_BCsImposed)
    # Substitute the symbols for what ever they are currently defined
    f_BCsImposed  = f_BCsImposed.subs(\
                                     [(sp.Symbol('f_MS')    ,f_MS), \
                                      (sp.Symbol('f_minBC'),f_minBC),\
                                      (sp.Symbol('f_maxBC'),f_maxBC), \
                                      (sp.Symbol('f_minMS'),f_minMS), \
                                      (sp.Symbol('f_maxMS'),f_maxMS), \
                                      (sp.Symbol('A_min'),A_min), \
                                      (sp.Symbol('A_max'),A_max) ])
    
    return f_BCsImposed

def diffModifiedManufacturedSolution(f_MS, \
                    del_f_minBC,\
                    del_f_maxBC,\
                    B_min, \
                    B_max):
    print(del_f_minBC,del_f_maxBC,B_min,B_max)
    
    f_BCsImposed  = f_MS + \
    B_min*(del_f_minBC ) + \
    B_max*(del_f_maxBC )
    
    # Substitute the symbols for what ever they are currently defined
    f_BCsImposed  = f_BCsImposed.subs(\
                                     [(sp.Symbol('f_MS')    ,f_MS), \
                                      (sp.Symbol('del_f_minBC'),del_f_minBC),\
                                      (sp.Symbol('del_f_maxBC'),del_f_maxBC), \
                                      (sp.Symbol('B_min'),B_min), \
                                      (sp.Symbol('B_max'),B_max) ])
    return f_BCsImposed


#def diffModifiedManufacturedSolution(f_MS, \
#                    df_minBC,\
#                    df_maxBC,\
#                    df_minMS,\
#                    df_maxMS,\
#                    B_min, \
#                    B_max):
#    
#    
#    f_BCsImposed  = f_MS + \
#    B_min*(df_minBC - df_minMS) + \
#    B_max*(df_maxBC - df_maxMS)
#    
#    # Substitute the symbols for what ever they are currently defined
#    f_BCsImposed  = f_BCsImposed.subs(\
#                                     [(sp.Symbol('f_MS')    ,f_MS), \
#                                      (sp.Symbol('df_minBC'),df_minBC),\
#                                      (sp.Symbol('df_maxBC'),df_maxBC), \
#                                      (sp.Symbol('df_minMS'),df_minMS), \
#                                      (sp.Symbol('df_maxMS'),df_maxMS), \
#                                      (sp.Symbol('B_min'),B_min), \
#                                      (sp.Symbol('B_max'),B_max) ])
#    return f_BCsImposed

